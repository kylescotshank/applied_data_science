# # ---------------------------------------------------------------
# ┏━━━┓╋╋┏┓┏┓╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┓╋┏┓┏┓╋╋╋╋╋┏━━━┓┏┓┏┓╋╋╋╋╋╋┏┓
# ┃┏━┓┃╋╋┃┃┃┃╋╋╋╋╋╋╋╋╋╋╋╋╋╋┃┏┛┏┛┗┫┃╋╋╋╋╋┃┏━┓┣┛┗┫┃╋╋╋╋╋┏┛┗┓
# ┃┃╋┗╋━━┫┃┃┃┏━━┳━━┳━━┓┏━━┳┛┗┓┗┓┏┫┗━┳━━┓┃┃╋┃┣┓┏┫┃┏━━┳━╋┓┏╋┳━━┓
# ┃┃╋┏┫┏┓┃┃┃┃┃┃━┫┏┓┃┃━┫┃┏┓┣┓┏┛╋┃┃┃┏┓┃┃━┫┃┗━┛┃┃┃┃┃┃┏┓┃┏┓┫┃┣┫┏━┛
# ┃┗━┛┃┗┛┃┗┫┗┫┃━┫┗┛┃┃━┫┃┗┛┃┃┃╋╋┃┗┫┃┃┃┃━┫┃┏━┓┃┃┗┫┗┫┏┓┃┃┃┃┗┫┃┗━┓
# ┗━━━┻━━┻━┻━┻━━┻━┓┣━━┛┗━━┛┗┛╋╋┗━┻┛┗┻━━┛┗┛╋┗┛┗━┻━┻┛┗┻┛┗┻━┻┻━━┛
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┛┃
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┗━━┛       Applied Data Science I - Week 5, Class 1
# # ---------------------------------------------------------------

# Today we are going to talk about correlations, correlation tests, and simple linear regressions
#
#
#
library(tidyverse)
#
#
#
data("diamonds")
# This is a pretty popular  dataset containing the prices and other attributes of almost 54,000 diamonds. 
# The variables are as follows:
#
# price (in US dollars)
#
# carat (weight of the diamond)
#
# cut (quality of the cut)
#
# color (diamond color - d is best, j is worst)
#
# clarity (l1 is worst, IF is best)
# 
# x - length
# y - width 
# z - depth 
# depth = total depth percentage 
#
# table - width fo the top of the diamond relative to widest point 
# 
# # ---------------------------------------------------------------
#
# Correlations and correlation test
#
# # ---------------------------------------------------------------

# Do we think the price of a diamond and the carat (weight) are correlated? 

cor(diamonds$carat, diamonds$price)

# Let's visualize this (don't worry - we'll cover more of this next week!)

diamonds %>% 
  ggplot(aes(x = carat, y = price, color = price)) + 
  geom_point() + 
  theme_bw()

# Pretty clear that there is a relationship here, at least visually and based on this 
# point estimate! Let's test it! Let's test if the correlation is different from zero (i.e., two-sided test)
# with a 95% confidence threshold

cor.test(diamonds$carat, diamonds$price, alternative = "two.sided",conf.level = 0.95)


# # ---------------------------------------------------------------
#
# Your turn! 
#
# # ---------------------------------------------------------------

# Test if there is a statistically significant correlation between price and x - the diamond's length.
# Use a 90% confidence threshold. 


# Test if there is a statistically significant correlation between the natural log of price
# and the natural log of carat. Use a confidence level of 0.95. Run the same visualization 
# code above to generate a plot of these transformed variables and discuss how these plots are different. 

diamonds %>% 
  ggplot(aes(x = log(carat), y = log(price), color = log(price))) + 
  geom_point() + 
  theme_bw()


# # ---------------------------------------------------------------
#
# Simple Linear Regression 
#
# # ---------------------------------------------------------------

# Let's build a simple linear model where we see how much of the variation in price
# is determined by the variation in carat! 

model_1 <- lm(price ~ carat, data = diamonds)
model_1
summary(model_1)

# Let's see if our linearized (log!) relationship is a better fit! 

model_2 <-lm(log(price) ~ log(carat), data = diamonds)
summary(model_2)

plot(model_2)



# # ---------------------------------------------------------------
#
# Your turn!
#
# # ---------------------------------------------------------------


# Build a simple linear model between x and price. Explore whether or not you should take the
# log values of either variable and explain your reasoning.
