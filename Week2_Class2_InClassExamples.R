# # ---------------------------------------------------------------
# ┏━━━┓╋╋┏┓┏┓╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┓╋┏┓┏┓╋╋╋╋╋┏━━━┓┏┓┏┓╋╋╋╋╋╋┏┓
# ┃┏━┓┃╋╋┃┃┃┃╋╋╋╋╋╋╋╋╋╋╋╋╋╋┃┏┛┏┛┗┫┃╋╋╋╋╋┃┏━┓┣┛┗┫┃╋╋╋╋╋┏┛┗┓
# ┃┃╋┗╋━━┫┃┃┃┏━━┳━━┳━━┓┏━━┳┛┗┓┗┓┏┫┗━┳━━┓┃┃╋┃┣┓┏┫┃┏━━┳━╋┓┏╋┳━━┓
# ┃┃╋┏┫┏┓┃┃┃┃┃┃━┫┏┓┃┃━┫┃┏┓┣┓┏┛╋┃┃┃┏┓┃┃━┫┃┗━┛┃┃┃┃┃┃┏┓┃┏┓┫┃┣┫┏━┛
# ┃┗━┛┃┗┛┃┗┫┗┫┃━┫┗┛┃┃━┫┃┗┛┃┃┃╋╋┃┗┫┃┃┃┃━┫┃┏━┓┃┃┗┫┗┫┏┓┃┃┃┃┗┫┃┗━┓
# ┗━━━┻━━┻━┻━┻━━┻━┓┣━━┛┗━━┛┗┛╋╋┗━┻┛┗┻━━┛┗┛╋┗┛┗━┻━┻┛┗┻┛┗┻━┻┻━━┛
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┛┃
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┗━━┛       Applied Data Science I - Week 2, Class 2
# # ---------------------------------------------------------------

# # ---------------------------------------------------------------
# Let's Check out some DATA
# # ---------------------------------------------------------------

# Let's load in some data that comes with R 
?data

# This command shows you allllllllll the data sets available for your use
data()

# This command loads the iris dataset into your global environment
data(iris)
?iris

# This lets you visually peek!
View(iris)
# This doesn't do anything :(
view(iris)

# This will return the dimensions of your data set
dim(iris)

# This will return your column names
names(iris)

str(iris)

summary(iris)

sum(is.na(iris))

plot(iris)

# cool, okay, we'll leave iris alone for now and come back to it! 

data(uspop)

View(uspop)

str(uspop)

plot(uspop)

# # ---------------------------------------------------------------
# Let's import some data!
# # ---------------------------------------------------------------

# If this is the first time you do this, you'll want to run this command! 
install.packages("tidyverse")

library(tidyverse)

?tidyverse

# Let's read in some Bob Ross Data! 

bob_ross_data <- read_csv(url("https://raw.githubusercontent.com/fivethirtyeight/data/master/bob-ross/elements-by-episode.csv"))
?url
?read_csv

str(bob_ross_data)
?tibble

View(bob_ross_data)

sum(is.na(bob_ross_data))

# --------------------------------------------------
# Let's answer a few questions about this data set!
# --------------------------------------------------

# How many episodes are there? 




# How many episodes contained at least one tree? 




# What percentage of episodes contained exactly one tree? 




sum(bob_ross_data$TREE) / length(bob_ross_data$EPISODE)

# What percentage of episodes contained at least two trees? 



sum(bob_ross_data$TREES) / length(bob_ross_data$EPISODE)

# What is the most common element in Joy of Painting? 




sort(colSums(bob_ross_data[,-c(1,2)]), decreasing = TRUE)


# Given that Bob Ross painted a single tree, what's the likelihood that he also painted clouds? 
#
#
#
# Remember (or learn!) the rule for conditional probability: 
# 
# P(A and B) = P(A)*P(B|A) 
#
# P(A) = Probability that A happened
# P(A and B) = Probability that A and B happened
# P(B|A) = Probability that B happened GIVEN that A happened
#
# rewrite it ....
#
# P(B|A) = P(A and B) / P(A) 


percentage_trees = sum(bob_ross_data$TREE) / length(bob_ross_data$EPISODE)

percentage_clouds = sum(bob_ross_data$CLOUDS) / length(bob_ross_data$EPISODE)

just_trees_and_clouds <- bob_ross_data %>% 
  filter(bob_ross_data$TREE == 1 & bob_ross_data$CLOUDS == 1)

# %>% is the "pipe" operator and let's you "chain" steps together
# filter() is a function that says "keep only the rows where these things are true 
# & is a logical operator for "AND". | is the logical operator for "OR" 

dim(just_trees_and_clouds)
dim(bob_ross_data)

percentage_trees_and_clouds = length(just_trees_and_clouds$EPISODE) / length(bob_ross_data$EPISODE)

(final_answer <- percentage_trees_and_clouds / percentage_trees)
# If you wrap a variable in (), it will display the variable when you execute it! 



# Given that Bob Ross painted a clouds, what's the likelihood that he also painted a conifer? 



# # ---------------------------------------------------------------
# Let's import some other data! 
# # ---------------------------------------------------------------
library(readxl)
# install.packages("readxl")

climate_change_data <- readxl::read_xls("Desktop/applied_data_science/data/climate_change_download_0.xls")

climate_change_data

climate_change_data %>% 
  # This selects the data sets and says "prepare to do more with it" by using %>% 
  select(`Series name`) %>%
  # This command says "Select ust this column" and "prepare to do more" by using the %>% 
  distinct() %>% 
  # This says "please only show me the unique values in here and "prepare to do more" by using the %>% 
  View()
  # This opens the View port so you can take a peek 


# Can we you select and filter for JUST The data in the United States that contains CO2 emissions per capita (metric tons)? 

climate_change_data %>% 
  filter(`Series name` == 'CO2 emissions per capita (metric tons)',
         `Country name` == 'United States')


# Let's save this 

us_emissions <- climate_change_data %>% 
  filter(`Series name` == 'CO2 emissions per capita (metric tons)',
         `Country name` == 'United States')

View(us_emissions)

# Let's do some cleaning! To make our lives easier, let's use the janitor package! 
library(janitor)
# install.packages("janitor")

us_emissions_cleaned <- us_emissions %>%
  clean_names() %>%
  select(-c(country_code, country_name, series_code, scale, decimals, series_name)) %>% 
  gather(year, value) %>%
  mutate(year = as.integer(str_replace(year, "[^\\d]", ""))) %>%
  filter(year < 2009)


plot(us_emissions_cleaned)
