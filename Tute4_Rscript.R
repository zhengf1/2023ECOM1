# load the Stargazer package
library(stargazer)  

# If you are running the current version of R there will be a problem using stargazer.
# For PC users, follow the instructions given in the Tutorial 4 questions file before running
# the R commands below.
# For Mac users, this 'fix' will not work so additional code has been inserted below whee required. 
# If you installed an earlier version of R there should not be any problem running the stargazer commands.

setwd("~/Library/CloudStorage/Dropbox/01 UoM-Teaching/2023-S1-Ecom1/Tute4")
# Read CSV file into dataframe
data=read.csv("tute4_cps.csv")

# ------------------------------
# Pre-tute Question 1
# ------------------------------
n1 = 5000
# This is a two-sided t test H0: mu=28 HA: mu != 28
mu1 = 28                            # mu (population mean)
alpha = 0.05                        # set desired significance
mean1 = 28.25                       # sample mean
sd1 = 10.66                         # sample std
se1 = sd1/sqrt(n1)                  # std. err {sd1\sqrt(n1)}
t1 = (mean1-mu1)/se1                # calculate the t statistic
df1 = n1-1                          # degrees of freedom
tcr_1 = qt(1-alpha/2, df1)          # calculate critical value
pval2 = 2*pt(-abs(t1),df=n1-1)      # calculate p-value for 2 sided test

# confidence interval
lcl1 = mean1-se1*tcr_1
ucl1 = mean1+se1*tcr_1
ci1 = rbind(lcl1,ucl1)
ci1_width = ucl1-lcl1
print(ci1)

# ------------------------------
# Question 2
# ------------------------------
## Mean and standard deviation of earnings for females
mean(data$ahe[data$female==1])
sd(data$ahe[data$female==1])

## Mean and standard deviation of earnings for males
mean(data$ahe[data$female==0])
sd(data$ahe[data$female==0])

# ------------------------------
# Question 5 (Regression)
# ------------------------------

## create a dataframe called data1
data1 = read.csv(file="consumption.csv")

## a) obtain the population mean of consumption
ymean = mean(data1$Consumption)
print(ymean)

## b) obtain the conditional mean of consumption
ycondmean = mean(data1[data1$Income <= 100, "Consumption"], na.rm = TRUE)
print(ycondmean)

# c) run the population model
reg1 = lm(Consumption~Income, data = data1)
summary(reg1)
stargazer(reg1, type="text",
          dep.var.labels=c("Consumption"))

# d) construct a random sample of 13 families for the population. Call it Sample A.
# Run the following regression and also create a scatter plot.
## set the seed for the random number generator
set.seed(2904)
##  construct the sample for the random sample of 13 families
data1a = data1[sample(nrow(data1),13,replace=TRUE),]
##  run the model for the random sample of 13 families
reg1a = lm(Consumption~Income, data=data1a)
## print the results in the console
summary(reg1a)
## obtain a scatter plot
plot(data1a$Income,data1a$Consumption,
     main="Consumption vs Income. \n (SRL sample A)",
     xlab="Income",
     ylab="Consumption",
     col="black",
     pch=16)
### include the fitted line in the scatter plot
abline(reg1a, col="blue")

# e) construct the following variables:
## `pred` predicted consumption
## `resid` residual
## resi` squared residual

data1a$pred = predict(reg1a, data = data1a)
data1a$resid = data1a$Consumption-data1a$pred
data1a$resid2 = data1a$resid^2
sumresid = sum(data1a$resid)
sumresid2 = sum(data1a$resid2)

print(sumresid)
print(sumresid2)

# f) construct another random sample of 13 families for the population. Call it Sample B.
# Then construct a scatter plot using  the *population*  with the PRL, the Sample Regression Line (SRL)
# of *sample* A, and SRL of sample B included.

## construct the sample
data1b = data1[sample(nrow(data1),13,replace=TRUE),]
## ##  run the model for the secondrandom sample of 13 families
reg1b = lm(Consumption~Income,data = data1b)

## put both regression results for Models A and B in one table
stargazer(reg1a,reg1b, type="text",
          dep.var.labels=c("Consumption"))











