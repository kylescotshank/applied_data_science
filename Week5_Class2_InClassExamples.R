# # ---------------------------------------------------------------
# ┏━━━┓╋╋┏┓┏┓╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┓╋┏┓┏┓╋╋╋╋╋┏━━━┓┏┓┏┓╋╋╋╋╋╋┏┓
# ┃┏━┓┃╋╋┃┃┃┃╋╋╋╋╋╋╋╋╋╋╋╋╋╋┃┏┛┏┛┗┫┃╋╋╋╋╋┃┏━┓┣┛┗┫┃╋╋╋╋╋┏┛┗┓
# ┃┃╋┗╋━━┫┃┃┃┏━━┳━━┳━━┓┏━━┳┛┗┓┗┓┏┫┗━┳━━┓┃┃╋┃┣┓┏┫┃┏━━┳━╋┓┏╋┳━━┓
# ┃┃╋┏┫┏┓┃┃┃┃┃┃━┫┏┓┃┃━┫┃┏┓┣┓┏┛╋┃┃┃┏┓┃┃━┫┃┗━┛┃┃┃┃┃┃┏┓┃┏┓┫┃┣┫┏━┛
# ┃┗━┛┃┗┛┃┗┫┗┫┃━┫┗┛┃┃━┫┃┗┛┃┃┃╋╋┃┗┫┃┃┃┃━┫┃┏━┓┃┃┗┫┗┫┏┓┃┃┃┃┗┫┃┗━┓
# ┗━━━┻━━┻━┻━┻━━┻━┓┣━━┛┗━━┛┗┛╋╋┗━┻┛┗┻━━┛┗┛╋┗┛┗━┻━┻┛┗┻┛┗┻━┻┻━━┛
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┛┃
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┗━━┛       Applied Data Science I - Week 5, Class 2
# # ---------------------------------------------------------------

# Today we are going a little deeper on regressions: multivariate and logistic!
#
#
#
library(tidyverse)
#
#
#
# # ---------------------------------------------------------------
#
# Multivariate regressions 
#
# # ---------------------------------------------------------------

# Let's use the diamonds data again! 
data("diamonds")
diamonds

# Let's remember our lovely older model 
simple_model <-lm(log(price) ~ log(carat), data = diamonds)
summary(simple_model)

# Let's see if we can improve the fit by adding more variables! 

medium_model <-lm(log(price) ~ log(carat) + cut, data = diamonds)
summary(medium_model)


complex_model <- lm(log(price) ~ log(carat) + ., data = diamonds)
summary(complex_model)


anova(simple_model, medium_model)

anova(simple_model, complex_model)

# Your turn! 
#
# Let's throw the kitchen sink into it! 
# Add *every* variable to the model and tell me what you see 



# # ---------------------------------------------------------------
#
# Logistic regressions 
#
# # ---------------------------------------------------------------

# We're going to get a little macabre - let' see if we can predict 
# which passengers lived (or died) on the titanic with a logistic regression! 

titanic <- read_csv(url("https://www.openml.org/data/get_csv/16826755/phpMYEkMl"))

# Let's clean this up for our modeling purposes 

titanic_cleaned <- titanic %>%
  select(-name) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(age = as.numeric(age)) 

# Let's split this into a test and training dataset! 

## 75% of the sample size
smp_size <- floor(0.75 * nrow(titanic_cleaned))

## set the seed to make your partition reproducible
set.seed(123)

# This builds an index of your data set, titanic_cleaned, where it randomly 
# pulls row numbers equal in length to the sample size you calculated above 
train_ind <- sample(seq_len(nrow(titanic_cleaned)), size = smp_size)


titanic_train <- titanic_cleaned[train_ind, ]
titanic_test <- titanic_cleaned[-train_ind, ]

# Let's build a toy model that tries to model the probability of survival
# based SOLELY upon sex (as classified by the norms of the time) 

titan_glm_simple <- glm(survived ~ sex + age, data = titanic_train, family = 'binomial')

summary(titan_glm_simple)

# What does this say? This says that the probability of surviving the sinking of the Titanic
# given you were male was log(p / (1-p)) = 0.9474 - 2.2970*(Male) 
#
# therefore, p = 1 / (1 + exp(0.9243-2.4109(1))) = 0.79 (or 79%)
# 
# which means the probability of surviving given you were FEMALE is 
# p = 1 / (1 + exp(1.0566 - 2.5137(0))) = 0.248 (or 28%) 

# Let's test it for accuracy! 

predict_sex_survived <- predict(titan_glm_simple, newdata = titanic_test, type = 'response') 

# Since Survived can only be either 1 or 0, write if statement to round up of down the response
predict_sex_survived <- ifelse(predict_sex_survived>0.5, 1, 0)

error_1 <- mean(predict_sex_survived != titanic_test$survived)
accuracy_1 <- 1-error_1
accuracy_1

#78% accuracy, not bad


# If you want more fancy information without having to do it manually like the above: 

install.packages("caret")
library(caret)

fitted.results.cat<-as.factor(predict_sex_survived)
titanic_test <- titanic_test %>% 
  mutate(survived = as.factor(survived))

confusionMatrix(data=fitted.results.cat, 
                    reference=titanic_test$survived)

# Sensitivity = "Recall" = True Positive Rate
# Specificity = "Selectivity" = True Negative Rate 
# Positive Predictive Value = "Precision" 
# Negative Predictive Value 
# Prevalence = Proportion of the population affected 
# Detection Rate 

# Your turn! 

# Let's try to improve by including a few more variables - age and pclass - and see what happens! 



