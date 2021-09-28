# # ---------------------------------------------------------------
# ┏━━━┓╋╋┏┓┏┓╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┓╋┏┓┏┓╋╋╋╋╋┏━━━┓┏┓┏┓╋╋╋╋╋╋┏┓
# ┃┏━┓┃╋╋┃┃┃┃╋╋╋╋╋╋╋╋╋╋╋╋╋╋┃┏┛┏┛┗┫┃╋╋╋╋╋┃┏━┓┣┛┗┫┃╋╋╋╋╋┏┛┗┓
# ┃┃╋┗╋━━┫┃┃┃┏━━┳━━┳━━┓┏━━┳┛┗┓┗┓┏┫┗━┳━━┓┃┃╋┃┣┓┏┫┃┏━━┳━╋┓┏╋┳━━┓
# ┃┃╋┏┫┏┓┃┃┃┃┃┃━┫┏┓┃┃━┫┃┏┓┣┓┏┛╋┃┃┃┏┓┃┃━┫┃┗━┛┃┃┃┃┃┃┏┓┃┏┓┫┃┣┫┏━┛
# ┃┗━┛┃┗┛┃┗┫┗┫┃━┫┗┛┃┃━┫┃┗┛┃┃┃╋╋┃┗┫┃┃┃┃━┫┃┏━┓┃┃┗┫┗┫┏┓┃┃┃┃┗┫┃┗━┓
# ┗━━━┻━━┻━┻━┻━━┻━┓┣━━┛┗━━┛┗┛╋╋┗━┻┛┗┻━━┛┗┛╋┗┛┗━┻━┻┛┗┻┛┗┻━┻┻━━┛
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┛┃
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┗━━┛       Applied Data Science I - Week 3, Class 1
# # ---------------------------------------------------------------

# Today we are going to learn about verbs!
#
#
# ... not really, we're going to learn a bunch of great Tidyverse functions
# but they're slick because they look like verbs. 


# ---------------------------------------------------------------
# Getting Setup 
# ---------------------------------------------------------------

library(tidyverse)
library(janitor)
# If you didn't install this the last class because we ran out of time - use install.packages("janitor")

# For today, we're going to get a little noisy!
#
# Today's example data is from a Kaggle: https://www.kaggle.com/somesnm/partynyc?select=party_in_nyc.csv 
#
# This dataset contains all noise complaints calls that were received by the city police with complaint type "Loud music/Party" in 2016. 
# The data contains the time of the call, time of the police response, coordinates, and part of the city.

nyc_noise_complaints <- clean_names(read_csv(url("https://raw.githubusercontent.com/kylescotshank/applied_data_science/master/data/party_in_nyc.csv")))

View(nyc_noise_complaints)
dim(nyc_noise_complaints)
str(nyc_noise_complaints)

# ---------------------------------------------------------------
# Verb #1: Arrange
# ---------------------------------------------------------------

# Arrange does exactly what you think it does - it orders the rows of a data frame by the values of selected columns (?dplyr::arrange)

nyc_noise_complaints %>% 
  arrange(created_date)

# This will arrange from smallest to biggest - or earliest to latest. 
# If you want to FLIP the order, use arrange(desc())

nyc_noise_complaints %>%
  arrange(desc(created_date))

# You can also order by multiple columns! 
# Let's order by created_date and incident zip! 

nyc_noise_complaints %>% 
  arrange(created_date,incident_zip)


# ---------------------------------------------------------------
# Verb #2: Select
# ---------------------------------------------------------------

# Select variables in a data frame - similar to using $ or [,] - but much easier on the eyes

# Select one column...
nyc_noise_complaints %>%
  select(city)

# Select a few columns ... 
nyc_noise_complaints %>%
  select(city,borough)

# Maybe select everything but the weird lat/long stuff we're not going to use today
nyc_noise_complaints %>%
  select(-latitude, -longitude)

# Maybe just reorder the columns because we're feeling ~feisty~ 
nyc_noise_complaints %>% 
  select(location_type, borough, city, incident_zip, closed_date, created_date)

# Maybe we just want columns that start with the letter c? 

nyc_noise_complaints %>%
  select(starts_with('c'))

# There's all sorts of helper functions you may never use - check them out with ?dplyr::select

# ---------------------------------------------------------------
# Verb #3: Filter
# ---------------------------------------------------------------

# Filter is just the row-wise version of Select: it gives you only the rows you want! 

# Let's just look at stuff in New York 

nyc_noise_complaints %>% 
  filter(city == 'NEW YORK')
  # Note the == 

# Maybe let's look at stuff in New York that are JUST in Residential Buildings 

nyc_noise_complaints %>% 
  filter(city == 'NEW YORK' & location_type == 'Residential Building/House')

# You can write the above in an easier way if you like because you don't like using & 

nyc_noise_complaints %>%
  filter(city == 'NEW YORK',
         location_type == 'Residential Building/House')

# Maybe let's look at stuff in New York OR in Residential Buildings 

nyc_noise_complaints %>% 
  filter(city == 'NEW YORK' | location_type == 'Residential Building/House')

# Because the dates were so helpfull stored as time, we can filter on them like numbers! 
# Let's look for everything in Manhattan that happened BEFORE the fourth of july in 2016 

nyc_noise_complaints %>%
  filter(borough == 'MANHATTAN',
         created_date < as.Date('2016-07-04'))


# ---------------------------------------------------------------
# Verb #4: Mutate 
# ---------------------------------------------------------------

# So far we've sorted stuff, selected stuff, and changed stuff. Now let's change stuff! 

# What if we perhaps we wanted to know the time interval between the closed and open dates
# We could use mutate! 

nyc_noise_complaints %>%
  mutate(interval = closed_date - created_date)

## What if we wanted to combine the City and Zip Code for whatever reason? 

nyc_noise_complaints %>% 
  mutate(city_zip = paste(city, incident_zip, sep = ' - '))

# We'll come back to mutate more later...

# ---------------------------------------------------------------
# Verb #5: Summarize
# ---------------------------------------------------------------

# Mutate kept the same number of rows in the data frame and added a column.
# Sometimes we just want to get an answer, you know? So for that, we use summarize

# For example - remember when we mutated above to get the interval of time between the
# close and open times of each policy call? What if we wanted to know the AVERAGE time of the whole dataset? 

nyc_noise_complaints %>%
  mutate(interval = closed_date - created_date) %>%
  summarize(avg_duration = mean(interval))

# HUH. We got an NA! I wonder why that is...
?mean

nyc_noise_complaints %>%
  mutate(interval = closed_date - created_date) %>%
  summarize(avg_duration = mean(interval, na.rm = T))

# HUH. STILL WEIRD. LET'S SEE WHERE THESE NEGATIVES ARE STROLLING IN FROM. 

nyc_noise_complaints %>%
  mutate(interval = closed_date - created_date) %>%
  arrange(interval)

# got it, okay, let's filter out those rows! 

nyc_noise_complaints %>%
  mutate(interval = closed_date - created_date) %>%
  filter(interval > 0) %>%
  summarize(avg_duration = mean(interval, na.rm = T))


# We could compute multiple things this way...

nyc_noise_complaints %>%
  mutate(interval = closed_date - created_date) %>%
  filter(interval > 0) %>%
  summarize(avg_interval = mean(interval, na.rm = T),
            median_interval = median(interval, na.rm = T))

# ---------------------------------------------------------------
# Verb #6: Group By
# ---------------------------------------------------------------

# This is the sneaky little verb that's going to make your life incredibly easy - but is almost useless by itself. 
# This lets you GROUP things together to then perform further actions on it! 

# For example - what if we wanted a count of cases by borough? Adding them up individually could be quite annoying. 
#...so let's group them! 

nyc_noise_complaints %>%
  group_by(borough) %>% 
  summarize(count = n())

# n() is just a nice easy function that counts for you! 

# What if we wanted the breakdown of location types ONLY in the city of Brooklyn? 

nyc_noise_complaints %>% 
  filter(city == 'BROOKLYN') %>%
  group_by(location_type) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
