####### ECOM20001 Econometrics 1
## Tutorial 2
## Zheng Fan ; fan.z@unimelb.edu.au

# set the working directory; yours will be different
setwd("~/Library/CloudStorage/Dropbox/01 UoM-Teaching/2023-S1-Ecom1/Tute2")

# load the data
dt = read.csv("tute2_crime.csv")

# check the data type
sapply(dt, class)

# as.numeric() if your data type is not numeric or integer

# histogram
hist(dt$vio)

vio = dt$vio
hist(vio)

# density plot
plot(density(vio))

# scatter plot
plot(dt$vio, dt$rob,
     xlab = "Violence",
     ylab = "Robbery",
     main = "scatter plot b/w ...",
     col = "blue",
     pch = 17)

# summary statistics
mean(dt$vio)

sapply(dt, mean)
sapply(dt, sd)
sapply(dt, median)

summary(dt)

# skewness
plot(density(dt$vio))
plot(density(dt$dens))
plot(density(dt$rob))
plot(density(dt$avginc), main = "density plot")

# relation among variables
plot(dt$vio, dt$rob)
cor(dt$vio, dt$rob)

plot(dt$avginc, dt$rob)
cor(dt$avginc, dt$rob)

plot(dt$dens,dt$rob)
cor(dt$dens,dt$rob)

