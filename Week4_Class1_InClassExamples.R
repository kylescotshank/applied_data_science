# # ---------------------------------------------------------------
# ┏━━━┓╋╋┏┓┏┓╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┓╋┏┓┏┓╋╋╋╋╋┏━━━┓┏┓┏┓╋╋╋╋╋╋┏┓
# ┃┏━┓┃╋╋┃┃┃┃╋╋╋╋╋╋╋╋╋╋╋╋╋╋┃┏┛┏┛┗┫┃╋╋╋╋╋┃┏━┓┣┛┗┫┃╋╋╋╋╋┏┛┗┓
# ┃┃╋┗╋━━┫┃┃┃┏━━┳━━┳━━┓┏━━┳┛┗┓┗┓┏┫┗━┳━━┓┃┃╋┃┣┓┏┫┃┏━━┳━╋┓┏╋┳━━┓
# ┃┃╋┏┫┏┓┃┃┃┃┃┃━┫┏┓┃┃━┫┃┏┓┣┓┏┛╋┃┃┃┏┓┃┃━┫┃┗━┛┃┃┃┃┃┃┏┓┃┏┓┫┃┣┫┏━┛
# ┃┗━┛┃┗┛┃┗┫┗┫┃━┫┗┛┃┃━┫┃┗┛┃┃┃╋╋┃┗┫┃┃┃┃━┫┃┏━┓┃┃┗┫┗┫┏┓┃┃┃┃┗┫┃┗━┓
# ┗━━━┻━━┻━┻━┻━━┻━┓┣━━┛┗━━┛┗┛╋╋┗━┻┛┗┻━━┛┗┛╋┗┛┗━┻━┻┛┗┻┛┗┻━┻┻━━┛
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┛┃
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┗━━┛       Applied Data Science I - Week 4, Class 1
# # ---------------------------------------------------------------

# Today we are going to learn about measures of central tendency!
#
#
#
library(tidyverse)
#
# Today's dataset is all about TROPICAL STOOORRMMMSS
data("storms")


# # ---------------------------------------------------------------
#
# Moment #1: The Mean! 
#
# # ---------------------------------------------------------------


# Remember that the mean is the 1st statistical moment of the distribution of your data.
# ...that is a fancy way to say that the mean describes the *location* of your distribution - it's center of gravity. 

# The function to calculate the mean is - Surprise! - mean() 

mean(storms$wind)
# This is the average windspeed across the entire dataset (i.e. - across all measures!)
# As you know, mean also works with pipes and summarize()! 

# Let's use %>% and summarize to look at the average windspeed by status for storms in 1986 
storms %>% 
  filter(year == 1986) %>%
  group_by(status) %>% 
  summarize(avg_wind_speed = mean(wind)) %>% 
  arrange(desc(avg_wind_speed))

# Let's maybee see if hurricanes are getting "stronger" and group by year with the mean 

storms %>%
  filter(status == 'hurricane') %>% 
  group_by(year) %>% 
  summarize(avg_wind_speed = mean(wind)) %>%
  arrange(year) %>% 
  View()


# # ---------------------------------------------------------------
#
# Moment #2: Variance and Standard Deviations! 
#
# # ---------------------------------------------------------------

# Variance is the 2nd statistical moment of your distribution - it describes the *spread* of your data. 
# Often times, we want to have this spread described in the same units as mean. To do that, we take the square root. 
# The square root of the variance is the standard deviation (which you can think of as the average spread). 
# High values are more spread out than smaller values.

# Let's maybe see if hurricanes have a wider "spread" amongst their averages when we group by year

storms %>%
  filter(status == 'hurricane') %>% 
  group_by(year) %>% 
  summarize(avg_wind_speed = mean(wind),
            sd_wind_speed = sd(wind)) %>%
  arrange(year) %>% 
  View()

# Let's try visualizing this one - might help! 

storms %>%
  filter(status == 'hurricane') %>% 
  group_by(year) %>% 
  summarize(avg_wind_speed = mean(wind),
            sd_wind_speed = sd(wind)) %>%
  ggplot(aes(x = year, y = avg_wind_speed)) + 
  geom_line(alpha = 0.3) + geom_point() + 
  geom_errorbar(aes(ymin=avg_wind_speed-sd_wind_speed, ymax=avg_wind_speed+sd_wind_speed), width=.2,
                position=position_dodge(0.05), color = 'red') + 
  theme_bw()


# # ---------------------------------------------------------------
#
# Moment #3: Skewness! 
#
# # ---------------------------------------------------------------

# Skewness is the 3rd statistical moment of your distribution - it describes the *lean* of your distribution. 
# A positive skew means you have a left lean and a long right tail. 
# This means that the mean (center of gravity) is to the right of the bulk of your data.
# Skewness will matter a lot when we start talking about linear models, as they give us an idea of 
# how far our distribution is "deviating" from normal - as well perhaps which direction contains the most outliers. 

# First, let's calculate the skewness! There is no base R function that does this, so we need to install and
# use a package. Let's use PerformanceAnalytics. 

library(PerformanceAnalytics)

storms %>%
  filter(status == 'hurricane') %>%
  select(wind) %>%
  skewness()

# The rule of thumb seems to be: If the skewness is between -0.5 and 0.5, the data are fairly symmetrical. 
# If the skewness is between -1 and – 0.5 or between 0.5 and 1, the data are moderately skewed. 
# If the skewness is less than -1 or greater than 1, the data are highly skewed.

# Let's look at the probability density plot to get an idea of how "skewed" our data may be on hurricane wind speeds. 

storms %>%
  filter(status == 'hurricane') %>% 
  group_by(year) %>% 
  ggplot(aes(x = wind)) + 
  stat_density(geom = "line", alpha = 1, colour = "cornflowerblue") + 
  theme_bw()
  
# Let's add some flavor to visualize this a little better ...

storms_data <- storms %>%
  filter(status == 'hurricane') %>% 
  select(wind)

median <- median(storms_data$wind)
mean <- mean(storms_data$wind)

base_plot <- storms_data %>% 
  ggplot(aes(x = wind)) + 
  stat_density(geom = "line", alpha = 1, colour = "cornflowerblue") + 
  theme_bw()

shaded_area_data <- 
  ggplot_build(base_plot)$data[[1]] %>% 
  filter(x < mean(storms_data$wind))

median_line_data <- 
  ggplot_build(base_plot)$data[[1]] %>% 
  filter(x <= median)

mean_line_data <- 
  ggplot_build(base_plot)$data[[1]] %>% 
  filter(x <= mean)

base_plot +
  geom_segment(data = shaded_area_data, aes(x = mean, y = 0, xend = mean, yend = density), 
               color = "red", linetype = "dotted") +
  annotate(geom = "text", x = median, y = 0.01, label = "median", color = "red", 
           fontface = "plain", angle = 90, alpha = .8, vjust =  -1.75) +
  geom_segment(data = median_line_data, aes(x = median, y = 0, xend = median, yend = density), 
               color = "red", linetype = "dotted") +
  annotate(geom = "text", x = mean, y = 0.02, label = "mean", 
           fontface = "plain", angle = 90, alpha = .8, vjust =  1.75) +
  geom_segment(data = mean_line_data, aes(x = mean, y = 0, xend = mean, yend = density), 
               color = "black", linetype = "dotted") +
  ggtitle("Density Plot Illustrating Skewness")


# # ---------------------------------------------------------------
#
# Moment #4: Kurtosis! 
#
# # ---------------------------------------------------------------

# The 4th statistical moment is the kurtosis which describes how "fat" the distribution's tails are. 
# It tells you how likely it is to find extreme values in your data.
# You'll also see peopel talk about this as a measure of "peakedness" - which is weird and kind of wrong. 

# First, let's calculate the kurtosis value! There is no base R function that does this, we again turn to a package.


storms %>%
  filter(status == 'hurricane') %>%
  select(wind) %>%
  kurtosis()

# The rule of thumb for kurtosis: If the kurtosis is between -3 and 3, the data are fine (also called "mesokurtic" lol)
# If the kurtosis is greater than 3 then this is *leptokurtic*: Distribution is longer, tails are fatter. Peak is higher and sharper than Mesokurtic, which means that data are heavy-tailed or profusion of outliers.
# If the kurtosis is less than 3 then this is *platykurtic*: Distribution is shorter, tails are thinner than the normal distribution.
# Note that the kurtosis() function above SUBTRACTS 3 - so its the "excess kurtosis" - so this value is slightly leptokurtic. 

storms %>%
  filter(status == 'hurricane') %>% 
  group_by(year) %>% 
  ggplot(aes(x = wind)) + 
  stat_density(geom = "line", alpha = 1, colour = "cornflowerblue") + 
  theme_bw()



# # ---------------------------------------------------------------
#
# "Robust" measures: Median and Median Absolute Deviation! 
#
# # ---------------------------------------------------------------


# Remember how we saw that the mean and median were different? The median is more "robust" in that it is much less
# sensitive to outliers. Median absolute deviation (MAD) is the same - it's a "robust" measure of variance! 

storms %>%
  filter(status == 'hurricane') %>%
  select(wind) %>%
  summarize(mean = mean(wind),
            sd = sd(wind),
            median = median(wind),
            mad = mad(wind))
