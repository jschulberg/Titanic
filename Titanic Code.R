# install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(stats)

import <- read.csv("train.csv")
test <- read.csv("test.csv")

# sample line of code to see hsitory
x <- 1

# set the seed
set.seed(123)

# split our import data into training and validation data
train_id <- sample(seq_len(nrow(import)), floor(nrow(import)*.8), replace = F)
train <- import[import$PassengerId %in% train_id,]

# validation data should have all the passenger IDs not in train data
val <- import[!(import$PassengerId %in% train$PassengerId),]

# ensure that we pulled enough data
if (nrow(val) + nrow(train) == nrow(import) ) {
  "Looks good"
} else {
  "Hold up wait a minute"
}




############################
###### Data Wrangling ######
############################
# convert Sex to categorical numbers
train$ind_sex[train$Sex == 'female'] <- 0
train$ind_sex[train$Sex == 'male'] <- 1

# take the ceiling of all the ages so we get rid of the half ages
train$Age <- ceiling(train$Age)

# convert age to a standardized value
mean_age <- mean(train$Age, na.rm = T)
sd_age <- sd(train$Age, na.rm = T)
train$ind_age <- (train$Age - mean_age)/sd_age

# convert Pclass to a factor
train$Pclass <- as.factor(train$Pclass)

# for cabin we only care about the floor they were on, which is the first letter
train$ind_cabin <- substr(train$Cabin, 1, 1)
train$ind_cabin[train$ind_cabin == ""] <- "Unknown"
plot(prop.table(table(train$ind_cabin)))

train$ind_cabin <- as.factor(train$ind_cabin)

# create family size column
train <- train %>%
  mutate(family_size = 1 + SibSp + Parch)




############################
#######     EDA     ########
############################

str(train)

train$Survived <- as.factor(train$Survived)

# let's create data frames out of training data for those who survived and those who died
survived <- train %>% 
  filter(Survived == 1)

dead <- train %>%
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
prop.table(table(train$SibSp)) # 93% of all passengers have less than 2 siblings

table(survived$SibSp)
prop.table(table(survived$SibSp)) # 85% of survivors have less than 2 siblings
hist(survived$SibSp)

table(dead$SibSp)
prop.table(table(dead$SibSp)) # 90% of dead have less than 2 siblings
hist(dead$SibSp)


# Number of parents / children aboard the Titanic is not a good indicator by 
# itself because 90% of people generally have less than 2 parents/children aboard
prop.table(table(train$Parch)) # 90% of all passengers have less than 2 siblings

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
prop.table(table(train$Pclass)) # 55% are class 3 (lowest class)
hist(train$Pclass)

table(survived$Pclass)
prop.table(table(survived$Pclass)) # only 37% of survivors are class 3
hist(survived$Pclass)

table(dead$Pclass)
prop.table(table(dead$Pclass)) # 67% of dead are class 3
hist(dead$Pclass)

# Family size aboard the Titanic is not a good indicator by 
# itself because 90% of people generally have less than 2 parents/children aboard
prop.table(table(train$family_size)) # 55% are class 3 (lowest class)
hist(train$family_size)

table(survived$family_size)
prop.table(table(survived$family_size)) # 76% of survivors have family size <3
hist(survived$family_size) # the largest family size

table(dead$family_size)
prop.table(table(dead$family_size)) # 82% of dead have family size <3
hist(dead$family_size)



############################
######     Model     #######
############################

fit_1 <- lm(train$Survived ~ train$Age)
summary(fit_1) # gives a high p value of .1841 -- probably not a good variable to use

fit_2 <- lm(train$Survived ~ train$Fare)
summary(fit_2) # gives a low p-value (8.32e-13) but the r^2 value is veryyy low
