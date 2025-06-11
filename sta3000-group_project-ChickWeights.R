# Quickly check out ChickWeight dataset 
?ChickWeight

##########################################
## PART 1 - LINEAR REGRESSION -----------#--------------------------------------
##########################################

# See what the ChickWeight looks like (First 5 rows)
head(ChickWeight)

# We will now train linear regression models. But first, we must take out identifier coloumn "Chick"
# Condition to remove unique identifier "Chick". We need to do this before making a linear regression model.
cond = c(-3) # removes 3rd column, which is "Chick"
cwt = ChickWeight[,cond] # Copy the data without this new column to a variable.

# The diet column should be factorized, we check if it is. If not, we factorize it ourselves.
# Here we see diet is already factorized. So now we're good to create our model.
is.factor(cwt$Diet)

# Make 2 linear regression models to predict weight based off time and diet
# - mod.1 has an interaction variable between Time and Diets.
# - mod.2 does not have an interaction variable.
mod.1 = lm(weight ~ Time * Diet,data=cwt)
mod.2 = lm(weight ~ .,data=cwt)

# Present summary of Model 1 -  W/ INTERACTION
# - All predictors significant besides Diet2/3/4. Likely because we implemented it into interaction
# - F test shows model is significant
# - R2 = 0.773 R2A = 0.7702 - Model has good R2 values
summary(mod.1)

# Present Summary of Model 2 - NO INTERACTION
# - All predictors are significant
# - F test shows model is significant
# - R2 = 0.7453 R2A = 0.7435 - Model has good R2 values
summary(mod.2)

# Overall, Model 1 with the interaction terms seems slightly more reliable.
# This could mean that there is a relationship between Time and all the diets. 
# It could be that diets tend to get better over time, instead of having a linear relationship.



#################################################
## PART 2 - T-TEST AND CONFIDENCE INTERAVALS ---#-------------------------------
#################################################

## Confidence intervals
## - These will give a 95% confidence interval for the mean weight at the specified diet and day. (Day is always 21 here)

# DIET 1 - Day 21
# - We are 95% confident that the mean weight of chicks on Diet 1, Day 21 is between 146.4699 and 209.0301.
subset_data <- subset(ChickWeight, Time == 21 & Diet == 1)
t.test(subset_data$weight, conf.level = 0.95)$conf.int

# DIET 2 - Day 21
# - We are 95% confident that the mean weight of chicks on Diet 2, Day 21 is between 158.8034 and 270.5966.
subset_data <- subset(ChickWeight, Time == 21 & Diet == 2)
t.test(subset_data$weight, conf.level = 0.95)$conf.int

# DIET 3 - Day 21
# - We are 95% confident that the mean weight of chicks on Diet 3, Day 21 is between 219.0643 and 321.5357.
subset_data <- subset(ChickWeight, Time == 21 & Diet == 3)
t.test(subset_data$weight, conf.level = 0.95)$conf.int

# DIET 4 - Day 21
# - We are 95% confident that the mean weight of chicks on Diet 4, Day 21 is between 205.2355 and 271.8756.
subset_data <- subset(ChickWeight, Time == 21 & Diet == 4)
t.test(subset_data$weight, conf.level = 0.95)$conf.int

# From these confidence intervals, Diet 3 seems to have the largest interval at 219.0643 and 321.5357 compared to the rest.


# This shows the confidence interval for each variable in the mod.1 Linear Regression Model
# Remember that this model includes interaction terms
confint(mod.1, level = 0.98)


## T-Tests
## - Compare the mean weight of chicks under two different diets (1-2, 3-4, and 1-4) at a specific time (day 21).

# No significant difference between the means
subset_data <- subset(ChickWeight, Time == 21 & Diet %in% c(1, 2))
t.test(weight ~ Diet, data = subset_data)

# No significant difference between the means
subset_data <- subset(ChickWeight, Time == 21 & Diet %in% c(3, 4))
t.test(weight ~ Diet, data = subset_data)

# Significant different between the means
subset_data <- subset(ChickWeight, Time == 21 & Diet %in% c(1, 4))
t.test(weight ~ Diet, data = subset_data)


# Despite Diet 3 having the highest confidence interval when tested earlier,
# there is no significant difference between the means of Diet 3 and Diet 4.
# Could this mean that both diets have chicks that obtain the same weight on average
# after 21 days on the diet?



##########################################
# PART 3 - VISUALS (LINEAR REGRESSION) --#----------------------------------------
##########################################

## We will do graphics of the Linear Regression Model that was created in Part 1.
## Specifically, mod.1 which includes the interaction terms.

# Load in library for visuals and manipulating data.
library(ggplot2)
library(dplyr)

# Copy of the ChickWeight data but adding a column for predicted values and residuals from mod.1.
# We will create visuals with this.
cwt = ChickWeight
cwt$predicted <- predict(mod.1)
cwt$residuals <- residuals(mod.1)

# Fitted Lines by Diet (with actual points)
ggplot(cwt, aes(x = Time, y = weight, color = Diet)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = predicted), size = 1.2) +
  labs(title = "Linear Regression with Interaction: Weight ~ Time * Diet")
## Analysis
# From the graph, we see that the slopes created for all 3 diets are different.
# From steepest slope to most gradual, we have...
# Steepest -
# 1. Diet 3
# 2. Diet 4
# 3. Diet 2
# 4. Diet 1
# Gradual -
# So over time, more weight is added to chicks on Diet 3 than any other diet.

# Fitted Regression Lines by Diet
ggplot(cwt, aes(x = Time, y = weight)) +
  geom_point(alpha = 0.3, aes(color = Diet)) +
  geom_line(aes(y = predicted), color = "black", size = 1) +
  facet_wrap(~ Diet) +
  labs(title = "Fitted Regression Lines per Diet")
# Similar visual as to one before but it seperates the linear regressions lines
# and separates them by the diet. For the most part, there is no outliers in the data.
# However, for Diet 2, there are quite a few points below the fitted line towards the later half.

# Residual Plot - (Model Diagnostics)
# - Unfortunately, this model violates the equal variance test. But it does pass the others.
# - Unbiased and Heteroscendastic
ggplot(cwt, aes(x = predicted, y = residuals, color = Diet)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Check Model Fit)")

# Shapiro Test
# - W = 0.92507
# - p-value is less than 0.05
# With p-value less than 0.05, we reject null hypothesis, which means Residuals are NOT normal.
# The data differs significantly from a normal distribution (W=0.925, p=2.252e-16)
# We can also check normal residuals with a Q-Q plot
shapiro.test(residuals(mod.1))


# Residual Plot with Q-Q plot (Model Diagnostics)
resid_df <- data.frame(resid = residuals(mod.1))
ggplot(resid_df, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot of Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
# In this plot, we essentially get confirmation that residuals are not normally distributed.
# Here, we have more data at extreme values

# We already have seen teh Residuals vs Fitted and Q-Q Residuals
# This shows us the Scale-Location and Residuals vs Leverage plots.
par(mfrow= c(2,2))
plot(mod.1)


##########################################
# PART 4 - VISUALS (ELSE) ---------------#---------------------------------------
##########################################

# Weight over Time by Diet
ggplot(cwt, aes(x = Time, y = weight, color = Diet)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Average Chick Weight Over Time by Diet")
# Here we can see weight over time by diet. Diet 4 leads in weight until around day 13,
# where diet 3 starts to lead, then gains a large advantage in weight by day 21.
# By the end, from the largest average chick weight to lowest, its Diet 3, 4, 2, then finally 1.

# Individual Growth Curves
ggplot(cwt, aes(x = Time, y = weight, group = Chick, color = Diet)) +
  geom_line(alpha = 0.6) +
  labs(title = "Individual Chick Growth Curves by Diet")
# This color codes each individual chick to their specific diet. Admittedly, this graph
# is hard to interpret since theres many lines. However, we can generally see Diet 3 tends to lead
# towards the later half, just as the previous graph showed. However, we can see that there a couple
# chicks that fall in extreme sides. For example, there is a chick on Diet 2 that gains a massive amount
# of weight as time went on, yet there is another chick with the same diet that completely stagnates in weight
# after day 6 or so. These extremes could affect predictions and statistics.

# Boxplots of Weight by Diet at a Specific Day
subset_day <- subset(cwt, Time == 21)
ggplot(subset_day, aes(x = Diet, y = weight, fill = Diet)) +
  geom_boxplot() +
  labs(title = "Weight Distribution by Diet on Day 21")
# These plots show similar inferences we've had before. The median follows the mean
# in terms of rankings by weight on Day 21. Again, from largest median to smallest we see
# Diet 3, 4, 2, then 1. 
# We also see in teh plot that Diet 2 have a very long bottom whisker, which shows 
# 25% of the data for diet 2 has wide variation and the lowest minimum of the 4 diets
# without necessarily having any outliers.
# On the other hand, Diet 4 has a very small bottom whisker. This could mean that although
# the average and median weights of the chicks are lower than that of Diet 3, they also do
# not reach anywhere close to the minimums of Diet 3. This could be seen as a positive for Diet 4.


# Mean ± Confidence Interval by Diet and Time
summary_data <- cwt %>%
  group_by(Time, Diet) %>%
  summarise(
    mean_weight = mean(weight),
    se = sd(weight) / sqrt(n()),
    .groups = "drop"
  )
ggplot(summary_data, aes(x = Time, y = mean_weight, color = Diet)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_weight - 1.96 * se, ymax = mean_weight + 1.96 * se, fill = Diet), alpha = 0.2) +
  labs(title = "Mean Weight with 95% CI Over Time by Diet")
# This data shows the 95% confidence interval of mean weight over time by diet. Its a little difficult to see initially, as
# the colors tend to bleed into one another, however Diet 3 has a confidence interval that reaches a much higher end.
