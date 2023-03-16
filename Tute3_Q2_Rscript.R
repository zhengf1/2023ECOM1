# --------------------------------------------------
# ECOM20001 Econometrics 1 Tutorial 3
# Zheng Fan; fan.z@unimelb.edu.au
# --------------------------------------------------

# As we don't need to load the data
# we don't have to set the working directory

# clear the environment
rm(list = ls())

# --------------------------------------------------
# In-tute Question 2
# GENERATING DISTRIBUTIONS OF SAMPLE MEANS

## Generate a nobs=1000 sample of x's from a Chi-Square distribution with df=3, 
## and compute the mean
nobs=1000
x = rchisq(nobs, df=3)
plot(density(x), main="Chi-Square df=3 - First, nobs=1000")
x_mean=mean(x)
print("First mean")
print(x_mean)

## Do it again: generate a nobs=1000 sample of x's from a Chi-Square distribution 
## with df=3, and compute the mean
x=rchisq(nobs, df=3)
plot(density(x), main="Chi-Square df=3 - Second, nobs=1000")
x_mean=mean(x)
print("Second mean")
print(x_mean)

## Do one more time: generate a nobs=100 sample of x's from a Chi-Square distribution 
## with df=3, and compute the mean
x=rchisq(nobs, df=3)
plot(density(x), main="Chi-Square df=3 - Third, nobs=1000")
x_mean=mean(x)
print("Third mean")
print(x_mean)

# ***** OPTIONAL (start) ******

# Now suppose we were to repeat this K=500 times. 
# Each time we randomly draw samples of x's with nobs=1000 from a Chi-Square distribution 
# with df=3, and then compute the mean for each sample. 
# So in the end, we will have compute K=500 sample means when we are done.

nobs = 1000   # sample size number of observations
df = 3        # degree of freedom
K = 500       # number of repetitions

## Creates a variable called "means" with K=500 rows for saving the 500 means
means = matrix(0, K, 1)

## Draw K=500 random samples each with nobs=1000 observations from a Chi-Square distribution 
## with df=3, and save the mean from each sample

for (k in 1:K){
  x=rchisq(nobs, df=3) # draw the sample
  means[k]=mean(x)    # save the mean
}

# ***** OPTIONAL (end) ******

## Compute variance of the K=500 sample averages with var()
varmeans=var(means)

## The Chi-Square distribution has an E[X]=df. Compute true value
truevalue=df

## Compute absolute value of the error between sample averages and true value
err=abs(means-truevalue)

## Compute percentage of sample averages within 0.3 of the true value
pct=100*sum(err<0.3)/K


## Print LLN results
print("Number of observations in each random sample")
print(nobs)
print("Percentage of Sample Averages within 0.3 of the True Value")
print(pct)
print("Variance of the K=500 Sample Means")
print(varmeans)

sum(means>=3.3) # count the number of draws that greater than 3.3
sum(means<=2.7)

plot(density(means))
