# install.packages("tidyverse")
# install.packages("randomForest")
library(randomForest)
library(tidyverse)
library(ggplot2)
library(stats)

import <- read.csv("train.csv")
test <- read.csv("test.csv")




############################
###### Data Wrangling ######
############################
# convert Sex to categorical numbers
import$ind_sex[import$Sex == 'female'] <- 0
import$ind_sex[import$Sex == 'male'] <- 1
import$ind_sex <- as.factor(import$ind_sex)

# take the ceiling of all the ages so we get rid of the half ages
import$Age <- ceiling(import$Age)

# convert age to a standardized value
mean_age <- mean(import$Age, na.rm = T)
sd_age <- sd(import$Age, na.rm = T)
import$ind_age <- (import$Age - mean_age)/sd_age

# convert Pclass to a factor
import$Pclass <- as.factor(import$Pclass)

# for cabin we only care about the floor they were on, which is the first letter
import$ind_cabin <- substr(import$Cabin, 1, 1)
import$ind_cabin[import$ind_cabin == ""] <- "Unknown"
plot(prop.table(table(import$ind_cabin)))

import$ind_cabin <- as.factor(import$ind_cabin)

# create family size column
import <- import %>%
  mutate(family_size = 1 + SibSp + Parch)
import$family_size <- as.factor(import$family_size)

import$Survived <- as.factor(import$Survived)



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
  {prop.table(table(.$Parch))} # 80% have less than 2

survived %>%
  filter(Age > 28.5) %>% # filter less than the mean
  {prop.table(table(.$Parch))} # 92% have less than 2


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
  select(Survived, Pclass, ind_sex, ind_cabin, family_size)


# ensure that we pulled enough data
if (nrow(val) + nrow(train) == nrow(import) ) {
  "Looks good"
} else {
  "Hold up wait a minute"
}


fit_1 <- lm(train$Survived ~ train$Age)
summary(fit_1) # gives a high p value of .1841 -- probably not a good variable to use

fit_2 <- lm(train$Survived ~ train$Fare)
summary(fit_2) # gives a low p-value (8.32e-13) but the r^2 value is veryyy low

# Let's try out our first randomForest. This gives an OOB estimate of error rate 19.5%.
# The class errors seem to be pretty large as well. I'm not sure this worked as well as
# I would have liked.
(model1 <- randomForest(Survived ~ ., data = train, importance = TRUE, proximity = TRUE))

# Let's try out our second randomForest where we have tried 4 variables at each split.
# This gives an OOB estimate of error rate 22%. The class errors seem to be even
# larger than the previous model. I'm not sure this worked as well as I would have liked.
(model2 <- randomForest(Survived ~ ., data = train, importance = TRUE, mtry = 4))


# let's check how this worked on our train set
# Predicting on train set
pred_train <- predict(model1, train, type = "class")
# Checking classification accuracy
table(pred_train, train$Survived)  

# Predicting on validation set
pred_val <- predict(model1, val, type = "class")
# Checking classification accuracy
table(pred_val, val$Survived)  

# find the overall accuracy by using the mean
mean(pred_val == val$Survived) # accuracy comes out to 79.9%
