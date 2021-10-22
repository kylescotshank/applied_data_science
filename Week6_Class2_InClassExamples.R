# # ---------------------------------------------------------------
# ┏━━━┓╋╋┏┓┏┓╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┓╋┏┓┏┓╋╋╋╋╋┏━━━┓┏┓┏┓╋╋╋╋╋╋┏┓
# ┃┏━┓┃╋╋┃┃┃┃╋╋╋╋╋╋╋╋╋╋╋╋╋╋┃┏┛┏┛┗┫┃╋╋╋╋╋┃┏━┓┣┛┗┫┃╋╋╋╋╋┏┛┗┓
# ┃┃╋┗╋━━┫┃┃┃┏━━┳━━┳━━┓┏━━┳┛┗┓┗┓┏┫┗━┳━━┓┃┃╋┃┣┓┏┫┃┏━━┳━╋┓┏╋┳━━┓
# ┃┃╋┏┫┏┓┃┃┃┃┃┃━┫┏┓┃┃━┫┃┏┓┣┓┏┛╋┃┃┃┏┓┃┃━┫┃┗━┛┃┃┃┃┃┃┏┓┃┏┓┫┃┣┫┏━┛
# ┃┗━┛┃┗┛┃┗┫┗┫┃━┫┗┛┃┃━┫┃┗┛┃┃┃╋╋┃┗┫┃┃┃┃━┫┃┏━┓┃┃┗┫┗┫┏┓┃┃┃┃┗┫┃┗━┓
# ┗━━━┻━━┻━┻━┻━━┻━┓┣━━┛┗━━┛┗┛╋╋┗━┻┛┗┻━━┛┗┛╋┗┛┗━┻━┻┛┗┻┛┗┻━┻┻━━┛
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┛┃
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┗━━┛       Applied Data Science I - Week 6, Class 2
# # ---------------------------------------------------------------

# Let's talk about CHHHAAARRRRTTTSSS and GGRRAPPHHHSS
#
#
#
library(tidyverse)
#
#
options(scipen=999)  # turn off scientific notation like 1e+06
#
#
data("midwest", package = "ggplot2") 
midwest
#
#
# # ---------------------------------------------------------------
# # 
# # Let's make a scatterplot! 
# # 
# # ---------------------------------------------------------------

# What happens if we just initialize a plot of the area and population totals of midwesternc ounties

ggplot(midwest, aes(x = area, y = poptotal))

# This is the same as 
midwest %>% 
  ggplot(aes(x = area, y = poptotal))

# What happened? 

midwest %>% 
  ggplot(aes(x = area, y = poptotal)) + 
  geom_point()


# Let's try adding in a nice linear model! 

midwest %>% 
  ggplot(aes(x = area, y = poptotal)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# Hmmm....this is maybe not helpful because of all of the outliers. 
# One thing we could do would be to filter out these data points. 
# ANOTHER thing we could do is just change the size of the plot! 

g <- midwest %>% 
  ggplot(aes(x=area, y=poptotal)) + 
  geom_point() + 
  geom_smooth(method="lm")

# Neat thing here - you can save a plot to a variable! 

# Now, let's use xlim() and ylim() 
g + xlim(c(0, 0.1)) + ylim(c(0, 1000000)) 


# Zoom in without deleting the points outside the limits. 
# As a result, the line of best fit is the same as the original plot.
g + coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000))

# Let's annotate the graph and clean it up! 

g + coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")


# Let's add a little color

midwest %>% 
  ggplot(aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state), size=3) +  # Set color to vary based on state categories.
  geom_smooth(method="lm", col="firebrick", size=2) + 
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

# What if we weant to use some special "themes" on this plot? 

midwest %>% 
  ggplot(aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state), size=3) +  # Set color to vary based on state categories.
  geom_smooth(method="lm", col="firebrick", size=2) + 
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics") + 
  theme_bw() + labs(subtitle="BW Theme")

midwest %>% 
  ggplot(aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state), size=3) +  # Set color to vary based on state categories.
  geom_smooth(method="lm", col="firebrick", size=2) + 
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics") + 
  theme_classic() + labs(subtitle="Classic Theme")

# ------------
# YOUR TURN 
# -------------

# Make a chart that shows the county-level percentage of adult poverty on the x-axis 
# and the percentage of adults with college degrees on the y-axis. Color each point by the state. 



# Make a chart that shows the county-level percentage of adult poverty on the x-axis 
# and the percentage of adults with college degrees on the y-axis. This time, color each point by
# whether or not it's a metro-area county. Before you do this, transform the inmetro variable into
# a factor



# # ---------------------------------------------------------------
# # 
# # Let's try a bar chart!
# # 
# # ---------------------------------------------------------------


data(diamonds)
diamonds

# Let's plot a bar chart! 
diamonds %>% 
  ggplot() + 
  geom_bar(mapping = aes(x = cut)) + 
  theme_bw()

# Let's plot a bar chart with DIFFERENT COLORS for different parts of the overall count! 
diamonds %>% 
  ggplot() +
  geom_bar(aes(x = cut, fill = clarity)) + 
  theme_bw()

# Let's make those pieces be next to one another! 
diamonds %>%
  ggplot() + 
  geom_bar(aes(x = cut, fill = clarity), position = "dodge") + 
  theme_bw()

# Sometimes, we might not want to automatically "count" something in a bar chart - 
# in this case, you need to pass a new parameter, stat = "identity")

# Let's look at the sum of all prices 

diamonds %>% 
  group_by(cut) %>%
  summarize(pricesum = sum(price)) %>% 
  ggplot() + 
  geom_bar(aes(x = cut, y = pricesum), stat = "identity") + 
  theme_bw()

# ------------
# YOUR TURN 
# -------------

# Make a bar chart that shows the MEDIAN carat by cut.



# Make a facet wrap bar chart that shows the median carat by clarity, where each facet is the cut. 
# If you forget how to do that from the reading, check out ?facet_wrap 




# Make a facet GRID scatter plot chart that shows carat on the x-axis and the 
# price on the y-axis. Have the facet grid be by both color AND cut. Then add a linear model overlaid on top of it. Make the points 
# all have the same color based upon their color.   


