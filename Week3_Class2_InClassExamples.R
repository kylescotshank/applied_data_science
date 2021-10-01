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
View(relig_income)

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

relig_income %>%
  pivot_longer(`<$10k`:`Don't know/refused`, names_to = "income", values_to = "count")


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
View(us_rent_income)

# hmmmm, this is not tidy! Let's tidy it up. 

us_rent_income %>%
  pivot_wider(names_from = variable, values_from = c(estimate, moe))

# now we can answer questions about it! 

us_rent_income_cleaned <- us_rent_income %>%
  pivot_wider(names_from = variable, values_from = c(estimate, moe))



# ---------------------------------------------------------------
# General Tidy Data Practice! 
# ---------------------------------------------------------------

data(billboard)
billboard
View(billboard)
# Let's clean this up! 



billboard %>%
  pivot_longer(wk1:wk76, names_to = 'week', values_to = 'count')



billboard %>% 
  pivot_longer(
    # this identifies which rows we want to pivot!
    wk1:wk76, 
    # this tells us the new name of the column
    names_to = "week", 
    # this tells us which values to focus on!
    values_to = "rank"
  )

# That's much better! Let's go even a little farther ...

billboard_cleaned <- billboard %>% 
  pivot_longer(
    wk1:wk76, 
    names_to = "week", 
    values_to = "rank", 
    values_drop_na = TRUE
  ) %>% 
  mutate(
    # this cleans up the "week" column and turns it into an integer - helpful for ordering! 
    week = as.integer(gsub("wk", "", week)),
  ) %>% 
  # this drops the now unnecessary date column! 
  select(-date.entered)


billboard_cleaned %>% 
  group_by(artist) %>% 
  summarize(avg_by_artist = mean(rank)) %>%
  arrange(avg_by_artist)


billboard_cleaned

billboard_cleaned %>% 
  group_by(artist) %>% 
  summarize(count = n()) %>%
  arrange(desc(count))

billboard_cleaned %>%
  group_by(artist, track) %>%
  summarize(count = n()) %>%
  arrange(artist) %>% 
  group_by(artist) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

billboard_cleaned %>%
  filter(artist == 'Jay-Z') %>%
  select(track) %>%
  distinct()


# ---------------------------------------------------------------
# Let's practice some more! 
# ---------------------------------------------------------------

devtools::install_github("uc-cfss/rcfss")

library(rcfss)


data(grades)

# what's wrong with this dataset? 

# one way to fix it! 

grades %>% 
  pivot_longer(
    Fall:Winter,
    names_to = "Season",
    values_to = "Score"
  )

# ahhh, but we still have an issue - we have multiple rows per observation (see row 1 and row 7). One more step! 

grades %>% 
  pivot_longer(
    Fall:Winter,
    names_to = "Season",
    values_to = "Score"
  ) %>% 
  pivot_wider(
    names_from = Test,
    values_from = Score
  )
