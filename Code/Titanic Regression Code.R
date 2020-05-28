################################################################## 
##################################################################

#####          Linear/Logistic Regression Analysis           #####

################################################################## 
################################################################## 
# In this script, I will employ a few facets of linear and logistic
# regression analysis to analyze the following dataset:

# Titanic -- This dataset contains various factors of the 
# individuals aboard the Titanic. I will use those factors to
# predict whether someone survived


################################################################## 
# Set Up ---------------------------------------------------------
################################################################## 

# Bring in packages
suppressMessages(library("tidyverse")) # Used for data cleaning and visualization
suppressMessages(library("here")) # Used for locating files in project folder
suppressMessages(library("readxl")) # Used for reading excel files
suppressMessages(library("reshape2")) # Used for reshaping data to tidy formats
suppressMessages(library("olsrr")) # Used for detecting outliers/high leverage points
suppressMessages(library("outliers")) # Used for detecting outliers
suppressMessages(library("patchwork")) # Used for plotting multiple vizzes side-by-side


################################################################## 
# Titanic  -------------------------------------------------------
################################################################## 
# In the last third of this script, I will analyze the titanic 
# dataset, looking at a variety of indicator factors related to
# individuals who were aboard the Titanic to predict whether or
# not they surved (1 = Survived, 0 = Did not Survive)
# In particular, I will:

#   - Convert the survived variable to a 0, 1 scale
#   - Perform a logistic regression model on the dataset, using
#     'Survived' as the response
#   - Analyze and interpret the model results
#   - Determine the probability of survival based on gender

# First, let's take a look at our data using read_tsv, which works
# on tab-separated values, like the text file I got off this site:
# http://math.ucdenver.edu/RTutorial/
titanic_data <- readr::read_tsv(here::here("Data/titanic.txt"))

# convert to a tibble and display
(titanic_data <- as_tibble(titanic_data))

# Here is a description of all the variables:
# Name - name of the individual
# pclass	Ticket class	1 = 1st, 2 = 2nd, 3 = 3rd
# Age	Age in years	
# sex	gender	
# survival	Survival	0 = No, 1 = Yes

# What percentage of each column is null?
sapply(titanic_data, function(x) paste(100*round(sum(is.na(x))/2629, 3), "%", sep = ""))

# The fact that almost half of our age data is null is concerning. We'll either have to take
# these out or impute the values.

# I noticed that one of the rows has a weird value in it, where PClass = PClass
# and the other values are NA, so let's remove it. We'll also do some other data
# prep, like converting to factors and rounding out the ages
titanic_prepped <- titanic_data %>% 
  filter(!(PClass == "PClass")) %>%
  # Make Sex, PClass and Survived factors
  mutate(Sex = as.factor(Sex),
         PClass = as.factor(PClass),
         Survived = as.factor(Survived)) %>%
  # Round our age values
  mutate(Age = round(Age)) %>%
  print()

# Now we'll look at the relationship between gender and survival rate. First, let's
# take a look at the data.
ggplot(aes(x = Sex, y = Survived), data = titanic_prepped) + 
  geom_jitter(colour = "slateblue") +
  theme_classic() +
  # Let's change the names of the axes and title
  labs(title = paste("Survival Rate of", nrow(titanic_prepped), "passengers", sep = " "),
       subtitle = "Broken out by Survival Status and Gender",
       caption = "*1 = Survived, 0 = Died") +
  ylab("Survived*") +
  # Center the title and format the subtitle/caption
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(color = "slateblue2", size = 10),
        plot.caption = element_text(hjust = 1, face = "italic", color = "dark gray")) 

# From this, we can clearly see that more females tended to survive than die, and more
# males tended to die than survive. Additionally, of those who survived, more were females
# and of those who died, more were male

100*round(prop.table(table(titanic_prepped$Sex)), 3)
# Before we jump to conclusions, just note that 65% of our passengers are male, but
# even with genders, it's clear to see a pattern in survival rate.


### Logistic Regression
# Start by setting the seed to ensure randomization and reproducibility
set.seed(123)

# Let's run a logistic regression on our dataset, with Survived as the response 
# variable and Sex as the explanatory variable
titanic_logmod <- glm(Survived ~ Sex, data = titanic_prepped, family = "binomial")
# let's see how we did
summary(titanic_logmod)
titanic_logmod$coefficients

# What do these coefficients even mean? Well, if we exponentiate the coefficients,
# we get something a bit more interpretable.
exp(titanic_logmod$coefficients[1])
exp(titanic_logmod$coefficients[2])

# From this model, we can see that the intercept is .69. This represents the log-odds
# of a female surviving on the Titanic. The results of logistic
# regression can be tough to understand, but here's my take:
# Female survival rate is the value when log(p/(1-p)) = B0. To find p, the probability
# that a female survives, we solve for p and assume a log-base of e. Thus p = exp(B0)/(1+exp(B0))
(female_survival <- exp(titanic_logmod$coefficients[[1]])/(1 + exp(titanic_logmod$coefficients[[1]])))
# We can apply the same logic for males, this time adding the two coefficients together
# before exponentiating. The math looks a little messy, but we get the right results. This will,
# in effect, be p = exp(B0 + B1)/(1+exp(B0 + B1))
(male_survival <- exp(titanic_logmod$coefficients[[1]] + titanic_logmod$coefficients[[2]])/(1 + exp(titanic_logmod$coefficients[[1]] + titanic_logmod$coefficients[[2]])))

# Thus, the probability that a female survives is 66.7% and the probability that a 
# male survives is 16.7%. This makes sense because, at least in the movie, they tended
# to put women and children on life boats. If we brought in age too, I'd expect that most
# of the males who survived were young (children).
