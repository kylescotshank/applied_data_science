# # ---------------------------------------------------------------
# ┏━━━┓╋╋┏┓┏┓╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┓╋┏┓┏┓╋╋╋╋╋┏━━━┓┏┓┏┓╋╋╋╋╋╋┏┓
# ┃┏━┓┃╋╋┃┃┃┃╋╋╋╋╋╋╋╋╋╋╋╋╋╋┃┏┛┏┛┗┫┃╋╋╋╋╋┃┏━┓┣┛┗┫┃╋╋╋╋╋┏┛┗┓
# ┃┃╋┗╋━━┫┃┃┃┏━━┳━━┳━━┓┏━━┳┛┗┓┗┓┏┫┗━┳━━┓┃┃╋┃┣┓┏┫┃┏━━┳━╋┓┏╋┳━━┓
# ┃┃╋┏┫┏┓┃┃┃┃┃┃━┫┏┓┃┃━┫┃┏┓┣┓┏┛╋┃┃┃┏┓┃┃━┫┃┗━┛┃┃┃┃┃┃┏┓┃┏┓┫┃┣┫┏━┛
# ┃┗━┛┃┗┛┃┗┫┗┫┃━┫┗┛┃┃━┫┃┗┛┃┃┃╋╋┃┗┫┃┃┃┃━┫┃┏━┓┃┃┗┫┗┫┏┓┃┃┃┃┗┫┃┗━┓
# ┗━━━┻━━┻━┻━┻━━┻━┓┣━━┛┗━━┛┗┛╋╋┗━┻┛┗┻━━┛┗┛╋┗┛┗━┻━┻┛┗┻┛┗┻━┻┻━━┛
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┏━┛┃
# ╋╋╋╋╋╋╋╋╋╋╋╋╋╋┗━━┛       Applied Data Science I - Week 2, Class 1
# # ---------------------------------------------------------------

# # ---------------------------------------------------------------
# Let's Practice Arithmetic 
# # ---------------------------------------------------------------

# Addition = + 
10 + 10 
# Subtraction
2 - 1 
# Multiply
3 * 6 
# Divide
2 / 7
8 / 2  
# YOU HAVE THE POWER! 
2 ^ 5 
2 ** 5 
# 
5 %% 2 
2 %/% 7 
7 %/% 2

1 / 1
1 ^ 0
1 / 0
0 / 0



# All of the basic arithmetic operators 
# +	addition
# -	subtraction
# *	multiplication
# /	division
# ^ or **	exponentiation
# x %% y	modulus (x mod y) 5%%2 is 1
# x %/% y	integer division 5%/%2 is 2

# R uses a specific order of operations - BEDMAS 

2 * 8 
3 * (2 * 8)
3 / (3 / (3 / 8))

()
# # ---------------------------------------------------------------
# Let's learn a couple of useful mathematical functions! 
# # ---------------------------------------------------------------

# Finds the sum of numbers or a series of numbers!
sum(2,3)
sum(1,2,3,4,5)
sum(1:10)

# Finds the absolute value of a number or series of numbers
abs(-15)

# Take the log!
log(10)

# ...make sure you know which base you're working on!
log(x = 10, base = 10)

?log

# Same as above
log(10,10)

# Finds the exponential of a function
exp(2.3)

?exp

# Find the square root of a number!
sqrt(9)

# Generate a sequence of numbers with : 

1:30

# Mix and match!

log(1:30, base = 2)


sqrt(
  sum(
  log(1:30), base = 2))

# # ---------------------------------------------------------------
# Let's Talk about Logic! 
# # ---------------------------------------------------------------

3 < 4 

5 <= 6

4 > 12 

3 >= 3

2 == 1 

2 == 2 

'Kyle' == 'Kyle'

2 = 2

2 != 4

2+1i - 3+0i

# All of the basic logic operators 
# <	less than
# <=	less than or equal to
# >	greater than
# >=	greater than or equal to
# ==	exactly equal to
# !=	not equal to
# !x	Not x
#
# ... we'll talk more about these ones later
# x | y	x OR y
# x & y	x AND y
# isTRUE(x)	test if X is TRUE


# # ---------------------------------------------------------------
# Let's Make Some ~v a r i a b l e s~
# # ---------------------------------------------------------------

my_age <- 35
# This is a variable - which is also a vector of length one! 

gray = 68

my_age 

my_kids_ages <- c(4,7) 
# This is a vector - of length two!
my_kids_ages
?c

c("Willa","Gideon") -> my_kids_names
my_kids_names

str(my_kids_names)

my_age * my_kids_ages 
my_age + my_kids_ages

my_age^2

my_kids_ages * my_kids_ages

my_kids_names + my_kids_ages

# # ---------------------------------------------------------------
# Let's Talk about basic data types 
# # ---------------------------------------------------------------

# Numeric: Numbers that have a decimal value or are a fraction in nature have a data type as numeric.
class(my_age)

class(my_kids_ages)

# Character: As the name suggests, it can be a letter or a combination of letters enclosed by quotes is considered as a character data type by R. It can be alphabets or numbers. 
class(my_kids_names)

# Integer: Integer: Numbers that do not contain decimal values have a data type as an integer. However, to create an integer data type, you explicitly use as.integer() and pass the variable as an argument.
class(as.integer(my_age))
class(35L)
# Why L? Because it's 32 bits in length (so it's "long"). Also because i and l are too similar looking! 

# Logical: A variable that can have a value of True and False like a boolean is called a logical variable. 

my_age == 35
class(my_age == 35)
class(my_age == 25)

# Factor: They are a data type that is used to refer to a qualitative relationship like colors, good & bad, course or movie ratings, etc. They are useful in statistical modeling.

example <- factor(c("good", "bad", "ugly","good", "bad", "ugly"))
print(example)
class(example)
levels(example)
nlevels(example)
class(levels(example))

# Lists: These are a data type that is a collection of ... variables that basically don't have to be related to one another. Lists can be confusing, but powerful. 

kyle <- list(age = 35,
             nerd = TRUE,
             children = c("Willa","Gideon") 
)
kyle
class(kyle)
kyle$age
kyle$children
class(kyle$age)
class(kyle$nerd)

# Data Frames: The real workhorses of R!

age <- c(17, 19, 21, 37, 18, 19, 47, 18, 19)
score <- c(12, 10, 11, 15, 16, 14, 25, 21, 29)
class_results <- data.frame(age,score) 
class_results
str(class_results)

class_results$age

class_results$age[1]

names(class_results)


# # ---------------------------------------------------------------
# Let's Talk about coercion 
# # ---------------------------------------------------------------

my_kids_names + my_kids_ages

my_kids <- c(my_kids_names,my_kids_ages)
my_kids
# This is *implicit* coercion! 
str(my_kids)

my_kids <- data.frame(names = my_kids_names, ages = my_kids_ages)
my_kids
str(my_kids)

# What is *explicit* coercion? 

number_list <- 0:6
number_list
(as.logical(number_list))
as.character(number_list)



# # ---------------------------------------------------------------
# Let's go back to data frames and such ... 
# # ---------------------------------------------------------------

class_results

new_kid <- c(NA,22)

another_new_kid <- c(68, "NA")

class_results <- rbind(class_results, new_kid)

class_results <- rbind(class_results, another_new_kid)

# rbind is BINDING ROWS 
# cbind is BINDING COLUMNS 

class_results

is.na(class_results)

sum(is.na(class_results))

complete.cases(class_results)


is.na(class_results$age)

