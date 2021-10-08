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
# # ---------------------------------------------------------------

# First, let's clean up. 

metal_cleaned <- metal %>%
  select(-split) %>%
  drop_na()

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
#  Introduction to a tests of proportions! 
#
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

# Of all the polices calls that happened during November of 2016, was there a  statistically significant difference in
# the proportion of parties that were busted in the boroughts of Brooklyn vs. Manhattan? Use a 99% confidence threshold



