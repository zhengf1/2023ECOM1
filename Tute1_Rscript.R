############### ECOM20001 Econometrics 1 Tutorial 1
# Zheng Fan; fan.z@unimelb.edu.au
# Contact me if you have any questions.
# Everything after "#" is a comment, which won't be executed 

rm(list=ls()) # remove everything in the environment

############### Set working directories ############### 
# It is very very important to set the working directories.
# Make sure your data set is in the folder.
# You can also manually find the folder and set working directories.
setwd("~/Dropbox/01 UoM-Teaching/2023-S1-Ecom1/Tute1") # your path should be different 

############### Load the data ############### 
# Only when you set the working directories properly, you can read the data
# dt is the name of the data set I assigned, you may choose whatever you like
dt = read.csv("tute1_tutors.csv") 

############### Print ############### 
print(20001)
print("Econometrics")
test_number = 20001

(test_number = 20001) # bracket will produce the output
print(test_number)

test_vector = c(1.3, 2.1, 3.4, 4.6, 5.2)
print(test_vector)

# index: how to get access to the 3rd component in test_vector?
test_vector[3]
sum(test_vector)
summary(test_vector) # some summary statistics can be obtained with one command

############### Replace the variable name ############### 
# find the name of data set
names(dt)
# find the one that is "instructor"
names(dt) == "instructor"
# replace it with tutors
names(dt)[names(dt) == "instructor"] = "tutors"
# create a new variable
dt[,5] = 1 
names(dt) # by default the variable name is "V5"
names(dt)[5] = "new_var"

############### Calculate descriptive statistics ############### 
# Check the class of each variable first. 
# You cannot perform calculation if it is not numerical. 
sapply(dt, class)

############### If it shows factor, do the following ############### 
# It might because of some default function settings in different computers. 
dt = read.csv("tute1_tutors.csv", stringsAsFactors = FALSE) 
# Check the class, it should now be character
sapply(dt, class)
# then proceed the rest

############### If it shows character, do the following. ############### 

# If fav_number shows to be a "character", first to replace the "pi" with number
which(dt$fav_number == "3.14555323 (pi)") # find the index that is "pi"
dt$fav_number[11] = 3.14 # be cautious about single and double "="

# check again to see whether fav_number becomes numerical.
# If it is numerical, then do the calculation directly.
sapply(dt, class)
mean(dt$fav_number) # If this does not work on your side, try the following 

# convert the fav_number into numerical number with the following command 
fav_number = as.numeric(dt$fav_number) # and save to another name
mean(fav_number)

## try the *Pre-tutorial Work* for our next tutorial
# Make sure your R is functional. 

## Make sure you have your attendance marked. 
# To get full attendance mark 5%, you have to participate at least 5 tutorials
# (1% for each) that you official enrolled.
# Don't send email regarding missing or replacing tutorials, because we have
# already took those situations into considerations. 


