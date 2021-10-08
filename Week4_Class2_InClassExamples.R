# # ---------------------------------------------------------------
# ┏━━━┓╋╋┏┓┏┓╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┓╋┏┓┏┓╋╋╋╋╋┏━━━┓┏┓┏┓╋╋╋╋╋╋┏┓
# ┃┏━┓┃╋╋┃┃┃┃╋╋╋╋╋╋╋╋╋╋╋╋╋╋┃┏┛┏┛┗┫┃╋╋╋╋╋┃┏━┓┣┛┗┫┃╋╋╋╋╋┏┛┗┓
# ┃┃╋┗╋━━┫┃┃┃┏━━┳━━┳━━┓┏━━┳┛┗┓┗┓┏┫┗━┳━━┓┃┃╋┃┣┓┏┫┃┏━━┳━╋┓┏╋┳━━┓
# ┃┃╋┏┫┏┓┃┃┃┃┃┃━┫┏┓┃┃━┫┃┏┓┣┓┏┛╋┃┃┃┏┓┃┃━┫┃┗━┛┃┃┃┃┃┃┏┓┃┏┓┫┃┣┫┏━┛
# ┃┗━┛┃┗┛┃┗┫┗┫┃━┫┗┛┃┃━┫┃┗┛┃┃┃╋╋┃┗┫┃┃┃┃━┫┃┏━┓┃┃┗┫┗┫┏┓┃┃┃┃┗┫┃┗━┓
# ┗━━━┻━━┻━┻━┻━━┻━┓┣━━┛┗━━┛┗┛╋╋┗━┻┛┗┻━━┛┗┛╋┗┛┗━┻━┻┛┗┻┛┗┻━┻┻━━┛
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┛┃
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┗━━┛       Applied Data Science I - Week 4, Class 2
# # ---------------------------------------------------------------

# Today we are going to learn about statistical testing! 
#
#
#
library(tidyverse)
library(broom)
#
#
# Some fun data sets today
metal <- read_csv("./data/metal_bands_2017.csv")


# # ---------------------------------------------------------------
#
#  Introduction to a t-test!
#
# A t-test is a statistic method used to determine if there is a significant difference between the 
# means of two groups based on a sample of data. Historically, this was used for data where you had a small n. 
#
# The test relies on a set of assumptions for it to be interpreted properly and with validity.
# Among these assumptions, the data must be randomly sampled from the population of interest 
# and the data variables must follow a normal distribution.
#
# A very, *very* important note about that last part - the normal distribution requirement. 
# 
# The t-test assumes that the means of the different samples are normally distributed; 
# it does *not* assume that the population is normally distributed.
#
# By the central limit theorem, means of samples from a population with finite variance approach a normal 
# distribution regardless of the distribution of the population. 
# Rules of thumb say that the sample means are basically normally distributed as 
# long as the sample size is at least 20 or 30. For a t-test to be valid on a sample of smaller size, 
# the population distribution would have to be approximately normal.

# The t-test is invalid for small samples from non-normal distributions, 
# but it is valid for large samples from non-normal distributions.
# # ---------------------------------------------------------------


# First, let's clean up. 

metal_cleaned <- metal %>%
  select(-split) %>%
  drop_na()

# Let's check some of those assumptions

# Do we have enough data? 

metal_cleaned %>% nrow()

# What are the average number of fans for bands that formed in the Denmark vs. Sweden?

metal_cleaned %>%
  filter(origin == c('Denmark', 'Sweden')) %>% 
  group_by(origin) %>%
  summarize(avg_fans = mean(fans))

# Is this difference in averages statistically significant? 

# Two ways to do this with a t-test! 

?t.test

metal_test_example <- metal_cleaned %>%
  filter(origin == c('Denmark', 'Sweden')) %>% 
  select(origin, fans) 

# The classical way! 
t.test(fans ~ origin, data = metal_test_example)

# The "tidy" way! 

metal_test_example %>% 
  do(tidy(t.test(fans ~ origin, data = .)))


# # ---------------------------------------------------------------
#
#  Now - you try! 
#
# # ---------------------------------------------------------------

# Are the average number of fans for bands that formed before the 90s DIFFERENT than fans that formed during or after the 90s? Use a 95% confidence threshold. 







# Do metal bands from Scandinavian countries (Sweden, Denmark, Norway, Finland, The Faroe Islands) have more fans
# on average than those from North America? Use a 90% confidence threshold. 




# # ---------------------------------------------------------------
#
#  Introduction to a tests of proportions - or the Z-test
#
# Two sample Z test of proportions is the test to determine whether the two populations differ 
# significantly on specific characteristics. In other words, compare the proportion of two 
# different populations that have some single characteristic. 
# 
# More formally the Z-test for proportions is a special case of a generic Z-test.
# A standard Z-test is a statistical test to determine whether two population means 
# are different when the variances are known and the sample size is large.
#
# Difference between Z-test and t-test: 
# Z-test is used when sample size is large (n>50), or the population variance is known.
# t-test is used when sample size is small (n<50) and population variance is unknown. 

# # ---------------------------------------------------------------

# Let's get that NYC party data again...

party <- read_csv("./data/party_in_nyc.csv")


# Let's try to answer a question - is the proportion of calls in manhattan against store/commercial location types
# the same as those against residential buildings? Let's use a 90% confidence threshold. 

# Let's clean the data up first: 

cleaned_party <- party %>%
  filter(Borough == 'MANHATTAN') %>% 
  mutate(location_type = `Location Type`) %>%
  filter(location_type == c("Residential Building/House",'Store/Commercial')) %>% 
  group_by(location_type) %>% 
  summarize(count = n()) %>% 
  mutate(total = sum(count))

prop.test(x = cleaned_party$count, n = cleaned_party$total, correct = FALSE, conf.level = .9)



# # ---------------------------------------------------------------
#
#  Now - you try! 
#
# # ---------------------------------------------------------------

# Of all the police calls that happened during November of 2016, was there a  statistically significant difference in
# the proportion of parties that were busted in the boroughts of Brooklyn vs. Manhattan? Use a 99% confidence threshold



# Was there a statistically significant difference in the proportion of police calls that were against 
# outdoor venues - streets, sidealks, parks, and playgrounds - than indoor venues for non-Manhattan boroughs? 
# Use a 90% confidence threhsold. 



