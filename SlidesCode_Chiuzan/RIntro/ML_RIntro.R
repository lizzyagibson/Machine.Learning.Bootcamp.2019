
##################################################################################
#                             Machine Learning Bootcamp                          #
#                                Introduction to R                               #
#                   Author: Cody Chiuzan, Last Updated: 06.06.2019               #
##################################################################################       


# Clean all objects from the current workspace (R memory) 
rm(list=ls())

# Install and load packages used in this code
install.packages("mlbench", "dplyr", "caret")
library(mlbench)
library(dplyr)
library(caret)

# Create an R project that keeps all files organized together: 
# input data, R scripts, analytical results, figures, etc.
# Here we called it: ML_RIntro

#########################################################################
#                          Import Data                                  #
#########################################################################


# What is the default working directory?
getwd()

# If you want to set the working directory
setwd("/Users/cc3780/Desktop")

# Notice that all datasets are saved in the project directory
# What if you didn't save them in a designated R project/directory?
# You need to specify the path.

# Read CSV files: "Pokemon.csv"

pokemon<-read.csv("O:\\Projects\\BERD_EDU\\MiniCourses\\MLWorkshop\\Slides_Cody\\Pokemon.csv")

# Read R built-in data sets

# Check the list of existing R datasets from package 'datasets
data()

# Load R data 'PimaIndiansDiabetes'
# This data is stored in package 'mlbench'; make sure you download the package first

data(PimaIndiansDiabetes, package="mlbench")

# Obtain some information about the data
??PimaIndiansDiabetes

#########################################################################
#                 Examine Data Attributes                               #
#########################################################################

# Examine classes
str(PimaIndiansDiabetes)

# Variable names
names(PimaIndiansDiabetes)    

# Data dimension: rows x columns; here: 768 rows and 9 columns
dim(PimaIndiansDiabetes)

# Number of rows and columns
nrow(PimaIndiansDiabetes)
ncol(PimaIndiansDiabetes)

# Head and Tail observations
head(PimaIndiansDiabetes)
tail(PimaIndiansDiabetes)

# Check for missing values
anyNA(PimaIndiansDiabetes)

# Tabulate variable 'diabetes'
table(PimaIndiansDiabetes$diabetes)


#########################################################################
#               Data Manipulation: library 'dplyr'                      #
#########################################################################

# Select only column/variable age
select(PimaIndiansDiabetes, age)

# Select only rows 1:5 of the data
slice(PimaIndiansDiabetes, 1:5)

# Keep only rows where 'age' is less than 25 
filter(PimaIndiansDiabetes, age < 25)

# Select rows that contain missing data
filter(PimaIndiansDiabetes, is.na(age))
# In this data there are no NAs

# Remove column age 
dplyr::select(PimaIndiansDiabetes, -age)

# Filter rows: select all < 25 yrs old with diabetes
filter(PimaIndiansDiabetes, age < 25 & diabetes=="pos") 

# Ordering data by variable/column 'pregnant'
arrange(PimaIndiansDiabetes, pregnant)

# Arrange in descending order
arrange(PimaIndiansDiabetes, desc(pregnant))

# Order by multiple columns/variables
dplyr::arrange(PimaIndiansDiabetes, pregnant, desc(age))


#Rename variable 'mass' to 'BMI' and 
# save this in a new data frame: new_diab

new_diab <- rename(PimaIndiansDiabetes, BMI = mass)
head(new_diab)

# Create a new variable by taking the log of 'age'
PimaIndiansDiabetes <- mutate(PimaIndiansDiabetes, log_age = log(age))
head(PimaIndiansDiabetes)


# Use IF-ELSE function to create new age categories
# Cat 1: Age < 29; Cat 2: 30 <= Age <=45 30. Cat 3: Age > 45
PimaIndiansDiabetes$age_cat <- ifelse(PimaIndiansDiabetes$age < 29 , 1,
                               ifelse((PimaIndiansDiabetes$age >=30 & PimaIndiansDiabetes$age <= 45), 2, 3))

head(dplyr::select(PimaIndiansDiabetes, -log_age))                                                  

#########################################################################
#               Combine Data Sets                                       #
#########################################################################      


# Let's create two variables: one containing the ID numbers and
# another variable smoking status: 0-never smoked, 1-smoking history

ID <-seq(1:768)
Smoking <-rbinom(768,1,0.4)

# Append these two new variables to the old dataset
combo_col <- cbind(ID, PimaIndiansDiabetes, Smoking)

head(combo_col)

dim(combo_col)

# Export new data to a CSV file
# The exported data should be in the same directory that you created: ML_RIntro

write.csv(combo_col, file="Diabetes_Smoke.csv")

