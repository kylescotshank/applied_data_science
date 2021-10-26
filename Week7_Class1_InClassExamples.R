# # ---------------------------------------------------------------
# ┏━━━┓╋╋┏┓┏┓╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┓╋┏┓┏┓╋╋╋╋╋┏━━━┓┏┓┏┓╋╋╋╋╋╋┏┓
# ┃┏━┓┃╋╋┃┃┃┃╋╋╋╋╋╋╋╋╋╋╋╋╋╋┃┏┛┏┛┗┫┃╋╋╋╋╋┃┏━┓┣┛┗┫┃╋╋╋╋╋┏┛┗┓
# ┃┃╋┗╋━━┫┃┃┃┏━━┳━━┳━━┓┏━━┳┛┗┓┗┓┏┫┗━┳━━┓┃┃╋┃┣┓┏┫┃┏━━┳━╋┓┏╋┳━━┓
# ┃┃╋┏┫┏┓┃┃┃┃┃┃━┫┏┓┃┃━┫┃┏┓┣┓┏┛╋┃┃┃┏┓┃┃━┫┃┗━┛┃┃┃┃┃┃┏┓┃┏┓┫┃┣┫┏━┛
# ┃┗━┛┃┗┛┃┗┫┗┫┃━┫┗┛┃┃━┫┃┗┛┃┃┃╋╋┃┗┫┃┃┃┃━┫┃┏━┓┃┃┗┫┗┫┏┓┃┃┃┃┗┫┃┗━┓
# ┗━━━┻━━┻━┻━┻━━┻━┓┣━━┛┗━━┛┗┛╋╋┗━┻┛┗┻━━┛┗┛╋┗┛┗━┻━┻┛┗┻┛┗┻━┻┻━━┛
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┛┃
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┗━━┛       Applied Data Science I - Week 7, Class ``
# # ---------------------------------------------------------------


# # ---------------------------------------------------------------
# # 
# # Required Packages / Settings
# # 
# # ---------------------------------------------------------------
#
#
library(tidyverse)
library(tidyquant)
library(zoo)
options(scipen=999)  # turn off scientific notation like 1e+06
#
#
#
#
# # ---------------------------------------------------------------
# # 
# # Let's get some stock market data and plot it with lines! 
# # 
# # ---------------------------------------------------------------

# Set your start and end dates

start = as.Date("2021-01-01") 
end = as.Date("2021-10-25")

# Let's grab some data about a stock via the tidyquant package. Let's check out Tesla (TSLA).

TSLA <- tq_get("TSLA", get = "stock.prices", from = start, to = end)

View(TSLA)


# Let's make a simple line plot! 

TSLA %>% 
  ggplot(aes(x = date, y = close)) + 
  geom_line() + 
  theme_bw()

# Let's maybe add some points...

TSLA %>% 
  ggplot(aes(x = date, y = close)) + 
  geom_line() + 
  geom_point() + 
  theme_bw()

# Hmmm...what about adding a trend line? 

TSLA %>% 
  ggplot(aes(x = date, y = close)) + 
  geom_line() + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_bw()

# That's, uh, not capturing reality very effectively is it? 
# Let's use the rollmean() function from the zoo library to calculate a 14 day moving average

TSLA %>%
  mutate(avg_14_day = rollmean(close, 14 ,align='right', fill=NA)) %>% 
  ggplot() + 
  geom_line(aes(x = date, y = close), color = "black") + 
  geom_point(aes(x = date, y =close), color = "black") + 
  geom_line(aes(x = date, y = avg_14_day), color = "blue") + 
  geom_point(aes(x = date, y = avg_14_day), color = "blue") + 
  theme_bw()

# Let's see what the loess smoothing does 
# Loess (or LOWESS) is "Locally Weighted Scatter Plot Smoothing" - or just "locally weighted smoothing" 

TSLA %>% 
  ggplot(aes(x = date, y = close)) + 
  geom_line() + 
  geom_point() + 
  geom_smooth() + 
  theme_bw()


# # # # # # # #
# Your turn!  #
# # # # # # # #

# Download the stock data for Moderna ("MRNA") from between 2019-01-01 and 2021-10-25. Visualize with a line chart that shows 
# the date on the x-axis and the closing value on the y-axis. Make this line black. 
# Add both a LOESS smooth that is BLUE and a 7 day rolling average that is RED to your chart. 


# Recreate the above chart - but have a legend that shows what each color means. 
# HINT: think about this first and remember how we showed legends in the past and what is DIFFERENT about the ways we plotted
# these things today. You'll probably want to think about how you can manipulate the dataset first. 


# # ---------------------------------------------------------------
# # 
# # Let's make some box plots! 
# # 
# # ---------------------------------------------------------------

# Let's go back to the TSLA stock for a second and visualize a boxplot of closing data! 

TSLA %>%
  ggplot(aes(x = symbol, y = close)) + 
  geom_boxplot() + 
  theme_bw()

# Let's add some points to help visualize this a little better...

TSLA %>% 
  ggplot(aes(x = symbol, y = close)) + 
  geom_boxplot() + 
  geom_jitter() + 
  theme_bw()

# Let's look at this across a bunch of stocks! 

data("FANG")
FANG

# Who is in here ... 

FANG %>% 
  ggplot(aes(x = symbol, y = close)) + 
  geom_jitter(alpha = 0.2) + 
  geom_boxplot(fill = NA, color = "red") + 
  theme_bw()


# # # # # # # #
# Your turn!  #
# # # # # # # #

# Use Google to go and find 4 random public companies. 
# Create box-plots of their closing data, similar to the above, 
# that shows a summary of their closing data from 2021-01-01 to 2021-10-25. 
# If you can't access the internet, use the "four_companies.csv" file. 



# Repeat the above - but this time ALSO visualize the open, high, and close. 
# Make each of these boxplots a different color and have
# inclusion on the legend. Do not add the jitter points. 

# # ---------------------------------------------------------------
# # 
# # Putting it all together and ordering it around! 
# # 
# # ---------------------------------------------------------------

# Let's combine that FANG and TSLA data ... 

tech <- FANG %>% 
  rbind(TSLA)


tech %>% 
  ggplot(aes(x = symbol, y = close)) + 
  geom_jitter(alpha = 0.2) + 
  geom_boxplot(fill = NA, color = "red") + 
  theme_bw()

# What if we wanted to reorder them? 

tech %>%
  ggplot(aes(x = reorder(symbol, close), y = close)) + 
  geom_jitter(alpha = 0.2) + 
  geom_boxplot(fill = NA, color = "red") + 
  theme_bw()

# What if we wanted to flip the axis? 

tech %>%
  ggplot(aes(x = reorder(symbol, close), y = close)) + 
  geom_jitter(alpha = 0.2) + 
  geom_boxplot(fill = NA, color = "red") + 
  coord_flip() + 
  theme_bw() 

