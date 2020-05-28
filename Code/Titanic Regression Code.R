################################################################## 
##################################################################

#####          Linear/Logistic Regression Analysis           #####

################################################################## 
################################################################## 
# In this script, I will employ a few facets of linear and logistic
# regression analysis to analyze the following three datasets:

#   1. AirBnB -- This dataset contains a variety of indicators 
#      related to housing factors to predict on the price of a house
#   2. Direct Marketing -- This dataset contains a variety of 
#      variables used to determine the Amount Spent on advertising
#   3. Titanic -- This dataset contains various factors of the 
#      individuals aboard the Titanic. I will use those factors to
#      predict whether someone survived


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
# AirBnB  --------------------------------------------------------
################################################################## 
# In the first third of this script, I will analyze the AirBnB dataset,
# looking at a variety of factors to predict the price of a housing
# unit. In particular, I will:

#   - Fit a multiple linear regression model using price as the response
#     variable and all others as predictor variables
#   - Analyze the results, interpret the coefficients
#   - Predict the price for a "fake" dataset
#   - Identify outliers using Cook's distance approach
#   - Perform Logarithimic Transformation to better fit the regression
#     models to the dataset and determine which of the four possible
#     log transformations (linear-linear, linear-log, log-linear, and
#     log-log regression) is best

# First, let's take a look at our data.
airbnb_data <- readxl::read_excel(here::here("Data/Airbnb Data.xlsx"))

# convert to a tibble and view the data
(airbnb_data <- as_tibble(airbnb_data))

# What do the different variables mean? From a website I found online
# (tomslee.net/category/airbnb-data), I found the following definitions
# for our data:

# room_id: A unique number identifying an Airbnb listing. The listing has a 
#          URL on the Airbnb web site of http://airbnb.com/rooms/room_id
# host_id: Unique number identifying an Airbnb host. The host’s page has a 
#          URL on the Airbnb web site of http://airbnb.com/users/show/host_id
# room_type: One of “Entire home/apt”, “Private room”, or “Shared room”
# city: The city or search area for which the survey is carried out. 
# reviews: The number of reviews that a listing has received. Airbnb has said 
#          that 70% of visits end up with a review, so the number of reviews can be 
#          used to estimate the number of visits. Note that such an estimate will not 
#          be reliable for an individual listing (especially as reviews occasionally 
#          vanish from the site), but over a city as a whole it should be a useful 
#          metric of traffic.
# overall_satisfaction: The average rating (out of five) that the listing has received 
#                       from those visitors who left a review.
# accommodates: The number of guests a listing can accommodate.
# bedrooms: The number of bedrooms a listing offers.
# price: The price (in $US) for a night stay. In early surveys, there may be some 
#        values that were recorded by month.

# Let's change some of the data types and clean up our dataset
airbnb_cleaned <- airbnb_data %>%
  # ID columns should be strings, not numbers
  mutate(room_id = as.character(room_id)) %>%
  mutate(host_id = as.character(host_id)) %>%
  mutate(survey_id = as.character(survey_id)) %>%
  # It looks like room type should be a factor
  mutate(room_type = as.factor(room_type))

# Before we fit a regression model, we actually won't need any
# of the ID columns or the city, which only has one value (Asheville),
# so let's remove those
airbnb_vars <- airbnb_cleaned %>%
  select(-contains("id"), -city)

# Before running a linear regression model, let's set the seed to ensure
# randomization and reproducibility
set.seed(123)

# Regardless, let's trek on with a linear regression model
airbnb_linreg1 <- lm(price ~ ., data = airbnb_vars)
summary(airbnb_linreg1)

# From this summary, we can see that the R-squared value is .3228, which
# frankly isn't too great. We also notice that our room type and reviews
# variables aren't too accurate either (low p-value). On the other hand,
# overall satisfaction, accomodates, and bedrooms are all really good
# predictors according to the model. We have to be careful though because
# we identified earlier that accomodates and bedrooms are highly correlated.

# Looking at the coefficients in our summary, we also notice that there
# are two coefficients for Room type (one for private room and one for
# shared room). But there are three levels to room type and we're missing
# "Entire home/apt". Where'd it go? Well, it's implicitly included in 
# the analysis. R automatically created indicator variables for room type,
# and if private room and shared room are both equal to '0', thus indicating
# that the type of a room is not one of those, we have entire home/apt. The
# coefficient for room_type (Shared Room) is -76.7, which can be interpreted as
# such:
# If a room is a shared room, with all else held equal, the price per night is
# actually -23.36 - 76.67 = -$100.03 cheaper per night.
# The coefficient for bedrooms, 85.65, indicates that for every extra bedroom
# in the house and with all else held equal, the price increases by $85.65 per night.


# Now let's try to predict the price (nearest dollar) per night for a listing with
# the following factors: 
# bedrooms = 1, accommodates = 2, reviews = 70, overall_satisfaction = 4, and room_type= 'Private room'
airbnb_new <- tibble(
  bedrooms = 1, 
  accommodates = 2, 
  reviews = 70, 
  overall_satisfaction = 4, 
  room_type = "Private room"
)

(airbnb_pred <- predict(airbnb_linreg1, airbnb_new))

# Thus, we predict that the private room would cost $66.20 per night. 
# How does this line up to the rest of our data, where the mean is
mean(airbnb_vars$price)


### Outliers
# Before looking at outliers, let's take a look at the boxplot of
# the price variable
ggplot(airbnb_vars, aes(y = airbnb_vars$price)) +
  geom_boxplot(outlier.colour="slateblue3",
               outlier.size=2,
               color = "slateblue") +
  theme_classic() +
  # Let's change the names of the axes and title
  labs(title = paste("Price for", nrow(airbnb_data), "houses", sep = " "),
       subtitle = "Outliers listed as dots in the visualization",
       caption = "*Price of houses per night") +
  ylab("Price (per night)*") +
  # Center the title and format the subtitle/caption
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(color = "slateblue", size = 10),
        plot.caption = element_text(hjust = 1, face = "italic", color = "dark gray"),
        # remove the x axis labels because they don't mean much for us
        axis.text.x = element_blank()) +
  # I thought the boxplot was too thick, so let's make it a little skinnier
  scale_x_discrete()

# We immediately notice that there are a number of outliers. I have a hunch
# that it has to do with room type, so let's break this out further.
ggplot(airbnb_vars, aes(y = airbnb_vars$price)) +
  geom_boxplot(outlier.colour="slateblue3",
               outlier.size=2,
               color = "slateblue") +
  # Use facet_wrap to get three boxplots based on the room type
  # We'll also free up our y axis so everything is easier to see
  facet_wrap(~ room_type, scales = "free_y") +
  theme_classic() +
  # Let's change the names of the axes and title
  labs(title = paste("Price for", nrow(airbnb_data), "houses", sep = " "),
       subtitle = "Outliers listed as dots in the visualization",
       caption = "*Price of houses per night") +
  ylab("Price ($)*") +
  # Center the title and format the subtitle/caption
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(color = "slateblue", size = 10),
        plot.caption = element_text(hjust = 1, face = "italic", color = "dark gray"),
        # remove the x axis labels because they don't mean much for us
        axis.text.x = element_blank()) +
  # I thought the boxplot was too thick, so let's make it a little skinnier
  scale_x_discrete()

# This seems to suggest that our outliers are in the entire home/apt and private room
# room types, which makes sense since these constitute 98% of our data. However,
# it looks like the outliers in entire home/apt are REALLY dragging the mean price per
# night out, whereas the private room outliers seem to be closer to the mean. Let's use
# two methodologies: Cook's distance and Grubbs Test.

### Cook's Distance
# Cook's distance is a method used to detect outliers that have a lot of influence (leverage)
# over a model. It does so using the following technique:
#   1. Delete one observation, i, from the dataset at a time
#   2. Refit our linear regression model on the remaining observations (n-1)
#   3. Examine the degree to which the fitted values change when our ith observation
#      is deleted from the model

# Use the olsrr package to plot our Cook's distance
olsrr::ols_plot_cooksd_bar(airbnb_linreg1)

# We can immediately see that a few points are really pulling the model outwards. Let's remove
# these from our dataset and re-run the model on this low leverage dataset
airbnb_lowlev <- airbnb_linreg1 %>%
  # Calculate the Cook's Distance
  cooks.distance() %>%
  # Save it as a tibble
  as_tibble() %>%
  # Rename it something meaningful
  rename(cooks_distance = value) %>%
  # Bring it back into our dataset
  bind_cols(airbnb_vars) %>%
  # Rearrange our dataset by cook's distance
  arrange(desc(cooks_distance)) %>%
  # Remove the two points with a Cook's Distance over 1
  filter(cooks_distance < 1) %>%
  # Get rid of cooks distance since we don't need it anymore
  select(-cooks_distance)

# How has this changed our box plot?
# Start by creating a new variable in our two datasets that we can
# eventually use to pivot on.
airbnb_combined <- airbnb_lowlev %>%
  mutate(outliers = "Outliers Removed")

airbnb_combined <- airbnb_vars %>%
  mutate(outliers = "Outliers Included") %>%
  bind_rows(airbnb_combined)

# Another boxplot viz
ggplot(airbnb_combined, aes(y = price)) +
  geom_boxplot(outlier.colour="slateblue3",
               outlier.size=2,
               color = "slateblue") +
  # Create separate boxplots for our dataset with and without outliers
  facet_wrap(~ outliers, scales = "free_y") +
  theme_classic() +
  # Let's change the names of the axes and title
  labs(title = paste("Price for", nrow(airbnb_data), "houses", sep = " "),
       subtitle = "Outliers with a Cook's Distance greater than\n1 removed from the second visualization",
       caption = "*Price of houses per night") +
  ylab("Price ($)*") +
  # Center the title and format the subtitle/caption
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(color = "slateblue", size = 10),
        plot.caption = element_text(hjust = 1, face = "italic", color = "dark gray"),
        # remove the x axis labels because they don't mean much for us
        axis.text.x = element_blank()) +
  # I thought the boxplot was too thick, so let's make it a little skinnier
  scale_x_discrete()

# Now let's re-run the model with these two points missing.
airbnb_linreg2 <- lm(price ~ ., data = airbnb_lowlev)
summary(airbnb_linreg2)

# Our r-squared value jumped to .42! That's higher than before (.32), 
# which is good. Let's use Grubbs Test to see if there are any other
# outliers in our dataset that may be ruining the show.
(grubbs1 <- grubbs.test(airbnb_lowlev$price))
grubbs1$alternative 
(p_value <- grubbs1$p.value) 
# Since the p-value is below .05, we can say with confidence that there
# is an outlier in the set. This indicates that the highest value 1250 
# is an outlier in our dataset. Let's try removing it and see what happens.

airbnb_nooutliers <- airbnb_lowlev %>%
  # Take out our maximum price
  filter(price != max(price)) %>%
  # Arrange our dataset on price
  arrange(desc(price)) %>%
  print()

# Let's check our Grubbs Test again and remove any remaining outliers
# using a recursive function that runs Grubbs Test, checks if the p-value
# is less than .05, removes the maximum value, and then re-runs Grubbs Test
# on the remaining values. Once the p-value goes over .05, it'll save our
# data to a new data frame and stop running
remove_outliers <- function(dataframe) {
  # Save our input dataframe and column of interest as something standard 
  data_grubbs <- dataframe
  # Run Grubbs Test
  grubbs <- grubbs.test(data_grubbs$price)
  # Check to see if the p-value is less than .05. If it is, take the highest
  # value out of our dataset and re-run Grubbs Test. 
  if (grubbs$p.value < .05) {
    # Re-save our dataset
    data_grubbs <- data_grubbs %>%
      # Take out our maximum price
      filter(price != max(price)) %>%
      # Arrange our dataset on price
      arrange(desc(price))
    
    # Re-run this function so it acts recursively
    return(remove_outliers(dataframe = data_grubbs))
    # If Grubbs Test p-value is greater than or equal to .05, save our final data
    # frame and exit.
  } else {
    airbnb_nooutliers <<- data_grubbs
    cat("Done running Grubbs Test! We successfully removed", 
        nrow(airbnb_lowlev) - nrow(airbnb_nooutliers),
        "outliers.")
  }
}

# Now that we have written our function to remove outliers using Grubbs.Test,
# let's run it on our dataset.
remove_outliers(dataframe = airbnb_nooutliers)

# Let's set ourselves up to visualize all of our results. Start by creating a
# new variable in our two datasets that we can eventually use to pivot on.
airbnb_combined2 <- airbnb_lowlev %>%
  mutate(outliers = "Outliers Removed (Cook's)")

airbnb_combined3 <- airbnb_nooutliers %>%
  mutate(outliers = "Outliers Removed (Grubbs)") %>%
  bind_rows(airbnb_combined2)

airbnb_combined3 <- airbnb_vars %>%
  mutate(outliers = "Outliers Included") %>%
  bind_rows(airbnb_combined3)

# Another boxplot viz
ggplot(airbnb_combined3, aes(y = price)) +
  geom_boxplot(outlier.colour="slateblue3",
               outlier.size=2,
               color = "slateblue") +
  # Create separate boxplots for our dataset with and without outliers
  facet_wrap(~ outliers, scales = "free_y") +
  theme_classic() +
  # Let's change the names of the axes and title
  labs(title = paste("Price for", nrow(airbnb_data), "houses", sep = " "),
       subtitle = "Outliers with a Cook's Distance greater than 1 are removed from the second visualization.\nOutliers are removed from the third visualization using Grubbs' Test.",
       caption = "*Price of houses per night") +
  ylab("Price ($)*") +
  # Center the title and format the subtitle/caption
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(color = "slateblue", size = 10),
        plot.caption = element_text(hjust = 1, face = "italic", color = "dark gray"),
        # remove the x axis labels because they don't mean much for us
        axis.text.x = element_blank()) +
  # I thought the boxplot was too thick, so let's make it a little skinnier
  scale_x_discrete()


# Now let's re-run the model with the dataset using Grubbs Test
airbnb_linreg3 <- lm(price ~ ., data = airbnb_nooutliers)
summary(airbnb_linreg3)

# Interesting. The R-squared value dropped to .38. One interesting thing to note,
# is that the reviews explanatory variable has become significant by removing
# outliers using Grubbs Test. Overall, though, I'd stick to just using Cook's 
# Distance to remove high leverage points.


### Log Transformation
# Next, we'll revert back to our dataset pre-Grubbs and use a variety of logarithmic
# transformations, which should help normalize the dataset. We'll target our energy
# on price and overall_satisfaction

## Linear-linear Model
airbnb_linlin <- lm(price ~ overall_satisfaction, data = airbnb_lowlev)
summary(airbnb_linlin)

# Let's store our results so we can visualize them later.
log_results <- tibble(transformation = "Linear-Linear", 
                      r_squared = summary(airbnb_linlin)$r.squared,
                      p_value = summary(airbnb_linlin)$coefficients[2, 4])

## Linear-Log Model
# We'll first test to see how transforming the explanatory variable, overall
# satisfaction affects the result. Note that we'll have to add 1 to the variable
# before transformation since overall_satisfaction has a range of 0-5 and log(0)
# equates to negative infinity, raising quite a number of problems.
airbnb_linlog <- lm(price ~ log(overall_satisfaction + 1), data = airbnb_lowlev)

# Bind the new results in so we have them for later.
log_results <- bind_rows(log_results,
                         tibble(transformation = "Linear-Log",
                                r_squared = summary(airbnb_linlog)$r.squared,
                                p_value = summary(airbnb_linlog)$coefficients[2, 4]))

## Log-Linear Model
# Next we'll test to see how transforming the response variable, price,
# affects the result.
airbnb_loglin <- lm(log(price) ~ overall_satisfaction, data = airbnb_lowlev)

# Bind the new results in so we have them for later.
log_results <- bind_rows(log_results,
                         tibble(transformation = "Log-Linear",
                                r_squared = summary(airbnb_loglin)$r.squared,
                                p_value = summary(airbnb_loglin)$coefficients[2, 4]))

## Log-Log Model
# Lastly, we'll test to see how transforming both variables, price and
# overall_satisfaction, affects the result. Note that we'll have to add 1 to the variable
# before transformation since overall_satisfaction has a range of 0-5 and log(0)
# equates to negative infinity, raising quite a number of problems.
airbnb_loglog <- lm(log(price) ~ log(overall_satisfaction + 1), data = airbnb_lowlev)

# Bind the new results in so we have them for later.
log_results <- bind_rows(log_results,
                         tibble(transformation = "Log-Log",
                                r_squared = summary(airbnb_loglog)$r.squared,
                                p_value = summary(airbnb_loglog)$coefficients[2, 4]))

# Viz Time
viz_rsquared <- ggplot(log_results,
                       # order by importance
                       aes(x = reorder(transformation, r_squared), y = round(100*r_squared, 4), group = 1), label = log_results$r_squared) +
  # Let's make it a column graph and change the color
  geom_col(fill = "slateblue2") +
  # Add the rounded text labels in for r-squared so it's easier to read
  geom_label(label = paste(100*round(log_results$r_squared, 4), "%", sep = "")) +
  # Change the theme to classic
  theme_classic() +
  # Let's change the names of the axes and title
  xlab("Transformation Type") +
  ylab("Percent Deviation Explained") +
  labs(title = "R-Squared for Different Logarithmic Transformations",
       subtitle = "All transformation were performed using the log() function.") +
  # format our title and subtitle
  theme(plot.title = element_text(hjust = 0, color = "black"),
        plot.subtitle = element_text(color = "dark gray", size = 10)) +
  # flip the axes and fix the axis
  coord_flip()

viz_pvalue <- ggplot(log_results,
                     # order by importance
                     aes(x = reorder(transformation, p_value), y = round(p_value, 4), group = 1), label = log_results$p_value) +
  # Let's make it a column graph and change the color
  geom_col(fill = "slateblue2") +
  # Add the rounded text labels in for r-squared so it's easier to read
  geom_label(label = round(log_results$p_value, 5)) +
  # Change the theme to classic
  theme_classic() +
  # Let's change the names of the axes and title
  xlab("Transformation Type") +
  ylab("P-value") +
  labs(title = "P-value for Different Logarithmic Transformations",
       subtitle = "All transformation were performed using the log() function.") +
  # format our title and subtitle
  theme(plot.title = element_text(hjust = 0, color = "black"),
        plot.subtitle = element_text(color = "dark gray", size = 10)) +
  # flip the axes and fix the axis
  coord_flip()

# Plot each using the patchwork library
viz_rsquared + viz_pvalue

# Based on these visualizations, it's evidently clear that the none of the models
# are effective. The best transformation, the Linear-Log Model, only has an R-squared
# of 3.5, which means that 96.5% of the variation in the relationship between price
# and overall_satisfaction is unexplained by the model. This makes sense, since I
# would not expect a large correlation between the price of an Airbnb rental and
# the ultimate satisfaction with the experience, since experience is usually independent
# of price.


### Correlation
# Let's now look at a correlation matrix, so we have a better
# grasp of how the variables in our dataset are interacting
airbnb_cor <- airbnb_vars %>%
  # First remove room type, which is our only non numeric field left
  select(-room_type) %>%
  # compute the correlation table
  cor() %>%
  # Round the results
  round(1) %>%
  print()

# Now let's look at a heat map
airbnb_cor %>%
  # Start by pivoting the correlation table to a tidy format
  reshape2::melt() %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  # Visualize as tiles
  geom_tile(color = "white") +
  # Change our scale to match the slateblue theme and extend from -1 to 1
  scale_fill_gradient2(low = "white", # color of lowest point
                       high = "slateblue4", # color of highest point
                       mid = "slateblue1", # color of midpoint
                       midpoint = 0, # definition of midpoint
                       limit = c(-1, 1), # definition of range
                       name = "Pearson\nCorrelation" # name of legend
  ) +
  # Change the theme
  theme_minimal() +
  labs(title = "Correlation Matrix for Airbnb Dataset",
       subtitle = "This analysis uses the Pearson Correlation") +
  ylab("") +
  xlab("") +
  # Center the title and format the subtitle/caption
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(color = "slateblue", size = 10),
        # Edit the axis text
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# From this we can see that there's actually quite a bit of correlation
# between our data points. For example, there exists really high 
# correlation between # of bedrooms and accomodates, which makes sense.
# Let's remove 


################################################################## 
# Direct Marketing  ----------------------------------------------
################################################################## 
# In the second third of this script, I will analyze the marketing 
# dataset, looking at a variety of indicator factors to predict 
# the amount spent on advertising. In particular, I will:

#   - Fit a multiple linear regression model using AmountSpent as the 
#     response variable and all other indicator variables as predictors
#   - Analyze the results, interpret the coefficients
#   - Predict the price for a "fake" dataset
#   - Identify outliers using Cook's distance approach

# First, let's take a look at our data.
marketing_data <- read.csv(here::here("Data/direct_marketing.csv"), header = T, sep = ",")

# convert to a tibble and get a glimpse of what we're working with
(marketing_data <- as_tibble(marketing_data))




################################################################## 
# Titanic  -------------------------------------------------------
################################################################## 
# In the second third of this script, I will analyze the marketing 
# dataset, looking at a variety of indicator factors to predict 
# the amount spent on advertising. In particular, I will:

#   - Convert the survived variable to a 0, 1 scale
#   - Perform a logistic regression model on the dataset, using
#     'Survived' as the response
#   - Analyze and interpret the model results
#   - Determine the probability of survival based on gender

# First, let's take a look at our data.
titanic_data <- read.delim(here::here("Data/titanic.txt"))

# convert to a tibble and display
(titanic_data <- as_tibble(titanic_data))

