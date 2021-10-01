# # ---------------------------------------------------------------
# ┏━━━┓╋╋┏┓┏┓╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┓╋┏┓┏┓╋╋╋╋╋┏━━━┓┏┓┏┓╋╋╋╋╋╋┏┓
# ┃┏━┓┃╋╋┃┃┃┃╋╋╋╋╋╋╋╋╋╋╋╋╋╋┃┏┛┏┛┗┫┃╋╋╋╋╋┃┏━┓┣┛┗┫┃╋╋╋╋╋┏┛┗┓
# ┃┃╋┗╋━━┫┃┃┃┏━━┳━━┳━━┓┏━━┳┛┗┓┗┓┏┫┗━┳━━┓┃┃╋┃┣┓┏┫┃┏━━┳━╋┓┏╋┳━━┓
# ┃┃╋┏┫┏┓┃┃┃┃┃┃━┫┏┓┃┃━┫┃┏┓┣┓┏┛╋┃┃┃┏┓┃┃━┫┃┗━┛┃┃┃┃┃┃┏┓┃┏┓┫┃┣┫┏━┛
# ┃┗━┛┃┗┛┃┗┫┗┫┃━┫┗┛┃┃━┫┃┗┛┃┃┃╋╋┃┗┫┃┃┃┃━┫┃┏━┓┃┃┗┫┗┫┏┓┃┃┃┃┗┫┃┗━┓
# ┗━━━┻━━┻━┻━┻━━┻━┓┣━━┛┗━━┛┗┛╋╋┗━┻┛┗┻━━┛┗┛╋┗┛┗━┻━┻┛┗┻┛┗┻━┻┻━━┛
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┛┃
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┗━━┛       Applied Data Science I - Week 3, Class 2
# # ---------------------------------------------------------------


# Today we are going to learn about just TWO verbs!
#
# "pivot_wider" and "pivot_longer"
# 
# We're also going to reinforce the tidy data principles - namely, a dataset is tidy IF and ONLY IF: 
# 
# Every column is a VARIABLE
# Every row is an OBSERVATION
# Every cell is a SINGLE VALUE 

# ---------------------------------------------------------------
# Verb #1: pivot_longer()
# ---------------------------------------------------------------

library(tidyverse)
data("relig_income")
relig_income

# What makes this dataset untidy?
# ...
# ...
# The column names are are VALUES, not VARIABLES 

# What if I wanted to know how many people identified as agnostic in this dataset? 

sum(relig_income[1,2:11])

# What if I wanted to know how many people identified as agnostic OR atheist but NOT catholic and made at least 75k a year? 
# ...it would be annoying!
# So let's tidy this data up! 

relig_income %>% 
  pivot_longer(!religion, names_to = "income", values_to = "count")


# Let's actually answer the question above 

relig_income %>% 
  pivot_longer(!religion, names_to = 'income', values_to = 'count') %>% 
  filter(religion != 'Catholic' & religion %in% c('Agnostic','Atheist'),
         income %in% c('$75-100k','$100-150k')) %>%
  summarize(count = sum(count))



# ---------------------------------------------------------------
# Verb #2: pivot_wider
# ---------------------------------------------------------------

data(us_rent_income)
us_rent_income

# hmmmm, this is not tidy! Let's tidy it up. 

us_rent_income %>%
  pivot_wider(names_from = variable, values_from = c(estimate, moe))

# now we can answer questions about it! 





# ---------------------------------------------------------------
# General Tidy Data Practice! 
# ---------------------------------------------------------------

data(billboard)
billboard

# Let's clean this up! 

billboard %>% 
  pivot_longer(
    # this identifies which rows we want to pivog!
    wk1:wk76, 
    # this tells us the new name of the column
    names_to = "week", 
    # this tells us which values to focus on!
    values_to = "rank"
  )

# That's much better! Let's go even a little farther ...

billboard %>% 
  pivot_longer(
    wk1:wk76, 
    names_to = "week", 
    values_to = "rank", 
    values_drop_na = TRUE
  ) %>% 
  mutate(
    # this cleans up the "week" column and turns it into an integer - helpful for ordering! 
    week = as.integer(gsub("wk", "", week)),
    # this makes sure that the "date.entered" column becomes a date! 
    date = as.Date(date.entered) + 7 * (week - 1),
  ) %>% 
  # this drops the now unnecessary date column! 
  select(-date.entered)




# ---------------------------------------------------------------
# Let's go find a dataset together! 
# ---------------------------------------------------------------