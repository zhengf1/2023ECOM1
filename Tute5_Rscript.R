# -------------------------------------------#
# ----- ECOM20001 Econometrics 1 Tute 5 -----#
# --------- fan.z@unimelb.edu.au ------------# 
# -------------------------------------------#
# The code can be downloaded via:
# https://github.com/zhengf1/2023ECOM1
# -------------------------------------------#

# set the working directory on the path of the Rscript!
# very handy, 
# but you have to save the Rscript to the correct folder!
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

########## Pre-tute Q1 and In-tute Q1
data1=read.csv(file="tute5_height.csv")

## Using the Rcode provided, look at the results of the 
## t-test you conducted to determine whether there is a 
## difference between people’s earnings who are under 
## 170 cm tall and those who are 170 cm or taller.
## Write out the hypotheses. Using the results of your 
## test what is your decision and conclusion?

# E(earnings|height >= 170)
mean(data1$earnings[data1$height>=170])
# alternatively, we may use the following to get the same thing
mean(data1[data1$height>=170,'earnings'])

# E(earnings|height < 170)
mean(data1$earnings[data1$height<170])

Dbar = mean(data1$earnings[data1$height>=170])
        -mean(data1$earnings[data1$height<170])
print(Dbar)
# The difference in means is $4605/year.
# Always be aware of the unit when you interprate


## 2-sample t-test of difference in means for 
# people taller and shorter than 170cm 
t.test(data1$earnings[data1$height>=170],
       data1$earnings[data1$height<170])

# alternatively, calculate manually
s1 = sd(data1$earnings[data1$height>=170])
n1 = length(data1$earnings[data1$height>=170])
s2 = sd(data1$earnings[data1$height<170])
n2 = length(data1$earnings[data1$height<170])
(t_stat = Dbar/sqrt(s1^2/n1+s2^2/n2))

# reject the null and claim a difference
# but keep in mind, this does not suggest any causal relation

########## Pre-tute Q2 and In-tute Q2
## Compare the value of the t-statistic and p-value 
## from this hypothesis test to the t-statistic and 
## p-value you obtained for regressing earnings on 
## the dummy variable you created. What do you notice?

# create a dummy variable taking the values 
# 1 = height greater than or equal to 170cm 
# 0 = height less than 170 cm
data1$height.dv <- 1*(data1$height >= 170)

reg.dv <- lm(earnings~height.dv, data=data1)
# alternatively
reg.dv <- lm(data1$earnings~data1$height.dv)
summary(reg.dv)

# What is the slope coefficient?
# Does it remind you something?

# -------------------------------------------
# E(Y|height.dv) = beta0 + beta1 * height.dv

# Since height.dv takes value either 1 or 0.

# E(Y|height < 170) = beta0    , earning for people with height <170
# E(Y|height >=170) = beta0 + beta1  , earning for people with hight >=170
# E(Y|>=170) - E(Y|<170) = Dbar = beta1

# The fitted line changes when the dummy variables 
# is “turned on” (height.dv=1) and when it is “turned off” (height.dv=0).
# Significant positive coefficient estimation

plot(x=NA, type="n", ylim=c(4, 5.5), xlim=c(120, 300),
     main="Earnings vs Height d.v.",
     xlab="Height in Centimeters",
     ylab="Annual Earnings in $10,000's")
abline(a=4.90932,b=0, col="blue", lwd=2)
abline(a=4.44879,b=0, col="red", lwd=2)
text(170, 4.6, "earnings = 4.449   height.dv=0",
     cex = .8)
text(170, 5.1, "earnings = 4.449+0.460   height.dv=1",
     cex = .8)
# You will also notice both are horizontal lines because 
# we are only regressions earnings against the dummy variable.
# which makes the slope coefficient become a add up on the intercept.

########## Pre-tute Q3 and In-tute Q3

# We will now look at the relationship between "tradeshare" 
# and "economic growth" using the second data set tute 5_growth.csv .

library(stargazer)
# If there is any trouble on loading stargazer package
# Go back and check materials in week 4 tutorial
# But anyway, it is just a format package.

data2 = read.csv(file="tute5_growth.csv")
stargazer(data2, type = "text", nobs = FALSE, mean.sd = TRUE, median = TRUE,
          iqr = TRUE, digits=2, omit = "earnings",
          title = "Summary statistics for tute5 growth.csv")

# From running the above, we can see that a typical country has 
# an average annual growth rate of 1.94%, real (1960=100) GDP 
# of $3,104 per person, and a trade share of 56.47%.
# For the latter, this means the average country gross exports 
# and imports together more than half of its annual GDP.

plot(data2$tradeshare,data2$growth,
     main="Trade Share of GDP and Economic Growth",
     xlab="Average share of trade in the economy from 1960 to 1995 (X+M)/GDP",
     ylab="Average Annual Percentage Growth of Real GDP from 1960 to 1995",
     col="red")
# there does appear visually to be a positive relationship between growth and trade.
identify(data2$tradeshare,data2$growth, n=1,labels=data2$country, cex=0.8)

# check the correlation
cor(data2$tradeshare,data2$growth)

# try a regression
reg2.1 <- lm(growth~tradeshare,data=data2)
summary(reg2.1)
# beta1_hat = 2.3064

# let's try to remove the outlier
# create a new data frame excluding the Republic of Korea 
exK2data <- subset(data2, country !="Korea, Republic of")
# then run the following regression only excluding Korea 
reg2.2<-lm(growth~tradeshare,data=exK2data)
summary(reg2.2)
# beta1_hat = 2.3839
# does not change too much

# create a new data frame only excluding Malta
exM2data  <- subset(data2, country !="Malta")
# run the following regression only  excluding Malta 
reg2.3 <-lm(growth~tradeshare,data=exM2data)
summary(reg2.3)
# beta1_hat = 1.6809
# changed a lot!


########## In-tute Q4
# Look at the scatter plots which included the fitted regression
# lines for the three models you estimated, and the summary of 
# the regression results for those models in the stargazer output.
# What do you observe?

plot(data2$tradeshare,data2$growth,
     main="Trade Share of GDP and Economic Growth",
     xlab="Average share of trade in the economy from 1960 to 1995 (X+M)/GDP",
     ylab="Average Annual Percentage Growth of Real GDP from 1960 to 1995",
     col="red",
     pch=16)
abline(reg2.1, col="black", lwd=2)
abline(reg2.2, col="forestgreen", lwd=2)
abline(reg2.3, col="blue", lwd=2)
legend("bottomright", legend=c("Overall","ex Korea", "ex Malta"),
       col=c("black","forestgreen",  "blue"), lty=1, cex=1,
       box.lty=0)

# print the regress results side by side to compare
stargazer(reg2.1, reg2.2, reg2.3,
          type = "text",
          intercept.bottom = FALSE)
