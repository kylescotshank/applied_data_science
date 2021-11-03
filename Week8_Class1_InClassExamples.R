# # ---------------------------------------------------------------
# ┏━━━┓╋╋┏┓┏┓╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┓╋┏┓┏┓╋╋╋╋╋┏━━━┓┏┓┏┓╋╋╋╋╋╋┏┓
# ┃┏━┓┃╋╋┃┃┃┃╋╋╋╋╋╋╋╋╋╋╋╋╋╋┃┏┛┏┛┗┫┃╋╋╋╋╋┃┏━┓┣┛┗┫┃╋╋╋╋╋┏┛┗┓
# ┃┃╋┗╋━━┫┃┃┃┏━━┳━━┳━━┓┏━━┳┛┗┓┗┓┏┫┗━┳━━┓┃┃╋┃┣┓┏┫┃┏━━┳━╋┓┏╋┳━━┓
# ┃┃╋┏┫┏┓┃┃┃┃┃┃━┫┏┓┃┃━┫┃┏┓┣┓┏┛╋┃┃┃┏┓┃┃━┫┃┗━┛┃┃┃┃┃┃┏┓┃┏┓┫┃┣┫┏━┛
# ┃┗━┛┃┗┛┃┗┫┗┫┃━┫┗┛┃┃━┫┃┗┛┃┃┃╋╋┃┗┫┃┃┃┃━┫┃┏━┓┃┃┗┫┗┫┏┓┃┃┃┃┗┫┃┗━┓
# ┗━━━┻━━┻━┻━┻━━┻━┓┣━━┛┗━━┛┗┛╋╋┗━┻┛┗┻━━┛┗┛╋┗┛┗━┻━┻┛┗┻┛┗┻━┻┻━━┛
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┛┃
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┗━━┛       Applied Data Science I - Week 8, Class 1
# # ---------------------------------------------------------------


# # ---------------------------------------------------------------
# # 
# # Required Packages / Settings
# # 
# # ---------------------------------------------------------------
#
#
library(tidyverse)
# 
# You'll need to download this one!
# install.packages("tidymodels")
library(tidymodels)
#
#
#
#
# # ---------------------------------------------------------------
# # 
# # How does k-means work!
# # 
# # Adapted from https://www.tidymodels.org/learn/statistics/k-means/ 
# # 
# # ---------------------------------------------------------------

# First, let's set a seed so we can make this reproducible! 

set.seed(20211102)

# Now, Let’s start by generating some random two-dimensional data with four clusters. 
# Data in each cluster will come from a multivariate Gaussian (normal) distribution, with different means for each cluster:

# This generates - deterministicaly - the actual center coordinates of our clusters
centers <- tibble(
  cluster = factor(1:4), 
  num_points = c(100, 150, 50, 200),  # number points in each cluster
  x1 = c(5, 0, -3, 2),              # x1 coordinate of cluster center
  x2 = c(-1, 2, 0, 4)
)

# This generates the random multivariate Gaussian distributions 
labelled_points <- 
  centers %>%
  mutate(
    x1 = map2(num_points, x1, rnorm),
    x2 = map2(num_points, x2, rnorm)
  ) %>% 
  select(-num_points) %>% 
  unnest(cols = c(x1, x2))

# Plot to prove what we did :) 

ggplot(labelled_points, aes(x1, x2, color = cluster)) +
  geom_point(alpha = 0.5) + 
  theme_bw()


# Let's how well k-means clustering works! 

points <- 
  labelled_points %>% 
  select(-cluster)

kclust <- kmeans(points, centers = 4)
kclust

# The output is a list of vectors, where each component has a different length. 
# There’s one of length 500, the same as our original data set. 
# There are two elements of length 4 (withinss and tot.withinss) and centers is a matrix with 4 rows. 
# And then there are the elements of length 1: totss, tot.withinss, betweenss, and iter. (The value ifault indicates possible algorithm problems.)

# These differing lengths have important meaning when we want to tidy our data set; 
# they signify that each type of component communicates a different kind of information.

summary(kclust)

# cluster (500 values) contains information about each point
# centers, withinss, and size (4 values) contain information about each cluster
# totss, tot.withinss, betweenss, and iter (1 value) contain information about the full clustering

# Which of these do we want to extract? There is no right answer; each of them may be interesting to an analyst. 
# Because they communicate entirely different information (not to mention there’s no straightforward way to combine them), 
# they are extracted by separate functions: augment, tidy, and glance! 

# The augment() adds the point classifications to the original data set:

augment(kclust, labelled_points)

# The tidy() function summarizes on a per-cluster level:

tidy(kclust)

# The glance() function extracts a single-row summary:

glance(kclust)

# Let's see how good our clustering was! 

original <- ggplot(labelled_points, aes(x1, x2, color = cluster)) +
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  ggtitle("Original")

kmeans_clustered <- augment(kclust, points) %>% 
  ggplot(aes(x1,x2, color = .cluster)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  ggtitle("K-means Clustering")

original
kmeans_clustered

# This is pretty good! 
# ... but what if we didn't know how many clusters we wanted to find first? 
# We could iterately search! 

# Let’s say we want to explore the effect of different choices of k, from 1 to 9, on this clustering.
# First cluster the data 12 times, each using a different value of k, then create columns containing the tidied, glanced and augmented data:


k_search <- 
  tibble(k = 1:12) %>%
  mutate(
    kclust = map(k, ~kmeans(labelled_points, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, labelled_points)
  )

# you might be saying "woah, what is this map" stuff. Let's look! 
?purrr::map

# Now let's extract those breakdowns we saw before: tidied, augmented, and glanced 

clusters <- 
  k_search %>%
  unnest(cols = c(tidied))

assignments <- 
  k_search %>% 
  unnest(cols = c(augmented))

clusterings <- 
  k_search %>%
  unnest(cols = c(glanced))

# We can use the augmented bits to visualize the "search" 
# through all values of k 
assignments %>% 
  ggplot(aes(x = x1, y = x2)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k) + 
  theme_bw()

# We could then add the estimated centroids
# by using the tidied summaries

assignments %>% 
  ggplot(aes(x = x1, y = x2)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k) + 
  geom_point(data = clusters, size = 10, shape = "x")+ 
  theme_bw()

# What in the world would we do with the glanced data?
# The data from glance() fills a different but equally important purpose; 
# it lets us view trends of some summary statistics across values of k. 
# Of particular interest is the total within sum of squares, saved in the tot.withinss column.
# This let's us visualize an "elbow plot" 

ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point() + 
  theme_bw()


#############################
#
# Your turn! 
#
#############################


# Use the iris dataset (use the command data(iris)). Draw a scatterplot showing sepal length on the x-axis and petal length on the y-axis. 
# Color the dots by species. Then perform a k-means clustering with 3 clusters. Plot your clusters in the same fashion. 

data(iris)
iris

iris %>% 
  ggplot(aes(x = Sepal.Length, y = Petal.Length, color = Species)) + 
  geom_point() + 
  theme_bw()

iris %>% 
  select(Sepal.Length, Petal.Length) %>% 
  kmeans(.,centers = 3) %>%
  augment(.,iris) %>%
  ggplot(aes(x = Sepal.Length, y = Petal.Length, color = .cluster)) + 
  geom_point() + 
  theme_bw()
 
  

# Use the iris dataset again. Perform a k-means clustering search for all values of k between 1 and 9 when comparing sepal width and petal width. 
# Create an elbow plot and decide which is the best value of k to choose. 

iris_trimmed <- iris %>% 
  select(Petal.Width, Sepal.Width)

iris_search <- 
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(iris_trimmed, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, iris_trimmed)
  )

iris_clusterings <- 
  iris_search %>%
  unnest(cols = c(glanced))

iris_clusterings %>%
  ggplot(aes(k, tot.withinss)) + 
  geom_line() + 
  geom_point() + 
  theme_bw()

iris_assignments <- 
  iris_search %>% 
  unnest(cols = c(augmented))


iris_assignments %>% 
  ggplot(aes(x = Petal.Width, y = Sepal.Width)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k) + 
  theme_bw()
