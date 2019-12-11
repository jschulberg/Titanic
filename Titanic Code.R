# install.packages("tidyverse")
# install.packages("randomForest")
# install.packages("ResourceSelection")
# install.packages("Amelia")
# install.packages("extrafont")
library(randomForest)
library(tidyverse)
library(ggplot2)
library(stats)
library(modelr)
library(ResourceSelection)
library(Amelia)
library(extrafont)
font_import()
loadfonts(device = "win")



import <- read.csv("train.csv", na.strings = c(""))
test <- read.csv("test.csv", na.strings = c(""))



############################
###### Data Wrangling ######
############################

# how many nulls do we have?
sapply(import, function(x) sum(is.na(x)))

# how many values are in each column?
sapply(import, function(x) length(unique(x)))

# map our missing values
missmap(import, main = "Missing values vs observed")


# make sure training and testing data have the same number of columns
test <- test %>%
  mutate(Survived = NA) %>%
  mutate(set = "test")

import <- import %>%
  mutate(set = "train")

# bring them both together so they have the same structure
data <- rbind(import,test)


# convert Sex to categorical numbers
data$ind_sex[data$Sex == 'female'] <- 0
data$ind_sex[data$Sex == 'male'] <- 1
data$ind_sex <- as.factor(data$ind_sex)

# convert age to a standardized value
mean_age <- mean(data$Age, na.rm = T)
sd_age <- sd(data$Age, na.rm = T)

# because there are a lot of null ages, let's replace these with a random sample within
# one standard deviation of the mean
data$Age[is.na(data$Age)] <- sample(mean_age-sd_age:mean_age+sd_age, nrow(data[is.na(data$Age),]), replace = T)

# standardize all the ages
data$ind_age <- (data$Age - mean_age)/sd_age

# take the ceiling of all the ages so we get rid of the half ages
data$Age <- ceiling(data$Age)

# convert Pclass to a factor
data$Pclass <- as.factor(data$Pclass)

# for cabin we only care about the floor they were on, which is the first letter
data$ind_cabin <- substr(data$Cabin, 1, 1)
data$ind_cabin[data$ind_cabin == ""] <- "Unknown"
plot(prop.table(table(data$ind_cabin)))

data$ind_cabin <- as.factor(data$ind_cabin)

# create family size column
data <- data %>%
  mutate(family_size = 1 + SibSp + Parch)
data$family_size <- as.numeric(data$family_size)

data$Survived <- as.factor(data$Survived)


# now split our training and testing apart
import <- data %>%
  filter(set == "train")

test <- data %>%
  filter(set == "test")

############################
#######     EDA     ########
############################

str(import)

# let's create data frames out of training data for those who survived and those who died
survived <- import %>% 
  filter(Survived == 1)

dead <- import %>%
  filter(Survived == 0)

# Sex is a pretty good indicator, it appears
table(survived$Sex)
prop.table(table(survived$ind_sex)) # 69% of survivors are women
hist(survived$ind_sex, probability = F)

table(dead$Sex)
prop.table(table(dead$ind_sex)) # 85% of the dead are men


# age is pretty normally distributed for dead and survived, probably not a good indicator
# this is surprising because they opt to save kids
table(survived$Age)
hist(survived$Age)

table(dead$Age)
hist(dead$Age)

mean(survived$Age, na.rm = T) # mean age of survivors is 28.5
mean(dead$Age, na.rm = T) # mean age of dead is 31.3

# draw boxplots for dead and survived
boxplot(survived$Age, dead$Age, 
        xlab = "Age", ylab = "Proportion of Ages", main = "Distribution of Ages",
        col = c("Gray", "Orange"))

legend(1.75, 15, c("Survived", "Dead"),
       fill = c("Gray", "orange"))

# SibSp is not a good indicator by itself because 93% of people 
# generally have less than 2 siblings
prop.table(table(import$SibSp)) # 93% of all passengers have less than 2 siblings

table(survived$SibSp)
prop.table(table(survived$SibSp)) # 85% of survivors have less than 2 siblings
hist(survived$SibSp)

table(dead$SibSp)
prop.table(table(dead$SibSp)) # 90% of dead have less than 2 siblings
hist(dead$SibSp)


# Number of parents / children aboard the Titanic is not a good indicator by 
# itself because 90% of people generally have less than 2 parents/children aboard
prop.table(table(import$Parch)) # 90% of all passengers have less than 2 siblings

table(survived$Parch)
prop.table(table(survived$Parch)) # 87% of survivors have less than 2 siblings
hist(survived$Parch)

table(dead$Parch)
prop.table(table(dead$Parch)) # 91% of dead have less than 2 siblings
hist(dead$Parch)

# let's see if this changes at all for kids, which I suspect is the case
survived %>%
  filter(Age < 28.5) %>% # filter less than the mean
  {prop.table(table(.$family_size))} # 80% have less than 2

survived %>%
  filter(Age > 28.5) %>% # filter less than the mean
  {prop.table(table(.$family_size))} # 92% have less than 2

# Let's check this out
plot_model2 <- ggplot(data = import, aes(x = family_size, y = Survived)) +
  geom_point()

plot_model2


# Class of passenger aboard the Titanic is not a good indicator by 
# itself because 90% of people generally have less than 2 parents/children aboard
prop.table(table(import$Pclass)) # 55% are class 3 (lowest class)
hist(import$Pclass)

table(survived$Pclass)
prop.table(table(survived$Pclass)) # only 37% of survivors are class 3
hist(survived$Pclass)

table(dead$Pclass)
prop.table(table(dead$Pclass)) # 67% of dead are class 3
hist(dead$Pclass)

# Family size aboard the Titanic is not a good indicator by 
# itself because 90% of people generally have less than 2 parents/children aboard
prop.table(table(import$family_size)) # 55% are class 3 (lowest class)
hist(import$family_size)

table(survived$family_size) # the largest family size for survivors is 7 (4 people)
prop.table(table(survived$family_size)) # 76% of survivors have family size <3
hist(survived$family_size) 

table(dead$family_size) # the largest family size for dead is 11 (8 people have greater than 8)
prop.table(table(dead$family_size)) # 82% of dead have family size <3
hist(dead$family_size) 


# It appears that more of the people who survived embarked from C
prop.table(table(import$Embarked)) 
prop.table(table(survived$Embarked)) 
prop.table(table(dead$Embarked))
ggplot(data = import, aes(x = Embarked)) + 
  geom_bar(fill = "slateblue2") +
  labs(title = "Breakout of Embarcation Points", x = "Embarcation Point", 
       y = "Number of Passengers") +
  theme(plot.title = element_text(face = "bold", family = "Palatino", size = 12)) +
  coord_flip()
  

############################
######     Model     #######
############################

# set the seed
set.seed(123)

# split our import data into training (80%) and validation (20%) data
train_id <- sample(seq_len(nrow(import)), floor(nrow(import)*.8), replace = F)
train <- import[import$PassengerId %in% train_id,]

# validation data should have all the passenger IDs not in train data
val <- import[!(import$PassengerId %in% train$PassengerId),]

# only bring in columns that we can model on
str(train)
train <- train %>%
  # select the columns we think are helpful
  select(Survived, Pclass, ind_age, ind_sex, family_size, Embarked)

# train$Survived <- as.numeric(train$Survived)
# train$ind_age <- as.numeric(train$ind_age)
fit_1 <- lm(train$Survived ~ train$ind_age)
summary(fit_1) # gives a high p value of .1841 -- probably not a good variable to use

fit_2 <- lm(train$Survived ~ train$Fare)
summary(fit_2) # gives a low p-value (8.32e-13) but the r^2 value is veryyy low

# Let's try out our first randomForest. This gives an OOB estimate of error rate 19.5%.
# The class errors seem to be pretty large as well. I'm not sure this worked as well as
# I would have liked.
(model1 <- randomForest(Survived ~ ., data = train, importance = TRUE, proximity = TRUE))
summary(model1)

# let's check how this worked on our train set
# Predicting on train set
pred_train <- predict(model1, train, type = "class")
# Checking classification accuracy
table(pred_train, train$Survived)  

# find the overall accuracy by using the mean
mean(pred_train == train$Survived) # accuracy comes out to 88.9%

# Predicting on validation set
pred_val <- predict(model1, val, type = "class")
# Checking classification accuracy
table(pred_val, val$Survived)  

# find the overall accuracy by using the mean
mean(pred_val == val$Survived, na.rm = T) # accuracy comes out to 81.9%


# Predicting on testing set
pred_test <- predict(model1, test, type = "class")

# bring the results in as our "Survived" column
test$Survived <- pred_test

# Checking classification accuracy
prop.table(table(test$Survived))

# run a logistic regression on our training data
model2 <- glm(Survived ~ ., data = train, family = "binomial")
# let's see how we did
summary(model2)

pred_model2 <- predict(model2, val)
val_model2 <- val %>%
  add_predictions(model2)

# let's see how we did by using the Hosmer and Lemeshow goodness of fit (GOF) test
hoslem.test(val_model2$survived, fitted(model2))


#### Final Output ####
results <- test %>%
  arrange(PassengerId)

# we only need the id and survived column predictor in the end
submission <- results %>%
  select(PassengerId, Survived)

# write our final output
write.csv(submission, "Submission3.csv")

