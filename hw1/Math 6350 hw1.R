# *Preliminary treatment of the data set
start_time <- Sys.time()
# Omit last three columns; Initially 397 rows
auto <- Auto[,c(1:6)]
# Horsepower designated as a factor variable
# Some rows contain non-numeric value (i.e. "?")
# Omit non-numeric rows; Number of N cases kept = 392
auto <- auto[!auto$horsepower == "?"]
# Convert horsepower to numeric variable
auto$horsepower <- as.numeric(as.character(auto$horsepower))
end_time <- Sys.time()
end_time - start_time # Computation time: 0.03091598 secs
# Rename the variable names of the dataset for convenience
install.packages(data.table)
library(data.table)
setnames(auto, old=c("cylinders", "displacement", "horsepower", "weight", "acceleration"),
         new=c("cyl", "dis", "hor", "wei", "acc"))
# 1) Compute the mean and standard deviation for features
attach(auto)
start_time <- Sys.time()
mean(cyl)
sd(cyl)
mean(dis)
sd(dis)
mean(hor)
sd(hor)
mean(wei)
sd(wei)
mean(acc)
sd(acc)
end_time <- Sys.time()
end_time - start_time # Computation time: 0.02801418 secs
# 2) Generate histograms for features and mpg
start_time <- Sys.time()
hist(cyl,
     main="Histogram of Cylinders",
     xlab="Number of cylinders",
     ylab="Number of Vehicles",
     col="orangered")
hist(dis,
     main="Histogram of Displacement",
     xlab="Engine Displacement in cubic inches",
     ylab="Number of Vehicles",
     col="orange")
hist(hor,
     main="Histogram of Horsepower",
     xlab="Engine Horsepower",
     ylab="Number of Vehicles",
     col="yellow1")
hist(wei,
     main="Histogram of Weight",
     xlab="Vehicle Weight in pounds",
     ylab="Number of Vehicles",
     col="greenyellow")
hist(acc,
     main="Histogram of Acceleration",
     xlab="Time to accelerate from 0 to 60 mph in secs",
     ylab="Number of Vehicles",
     col="deepskyblue")
hist(mpg,
     main="Histogram of MPG",
     xlab="Miles per Gallon",
     ylab="Number of Vehicles",
     col="darkorchid1")
end_time <- Sys.time()
end_time - start_time # Computation time: 0.2850239 secs
# 3,4) Generate scatterplots for features vs mpg
start_time <- Sys.time()
plot(cyl, mpg, main="Scatterplot of Cylinders vs MPG",
     xlab="Number of Cylinders", ylab="Miles Per Gallon ", col="violetred2")
plot(dis, mpg, main="Scatterplot of Displacement vs MPG",
     xlab="Engine Displacement in cubic inches", ylab="Miles Per Gallon ", col="slateblue2")
plot(hor, mpg, main="Scatterplot of Horsepower vs MPG",
     xlab="Engine Horsepower", ylab="Miles Per Gallon ", col="turquoise3")
plot(wei, mpg, main="Scatterplot of Weight vs MPG",
     xlab="Vehicle Weight in pounds", ylab="Miles Per Gallon ", col="seagreen3")
plot(acc, mpg, main="Scatterplot of Acceleration vs MPG",
     xlab="Time to accelerate from 0 to 60 mph in secs", ylab="Miles Per Gallon ",
     col="orange1")
end_time <- Sys.time()
end_time - start_time # Computation time: 0.242872 secs
# 5) Compute the correlations between features and mpg
start_time <- Sys.time()
cor(cyl, mpg)
cor(dis, mpg)
cor(hor, mpg)
cor(wei, mpg)
cor(acc, mpg)
end_time <- Sys.time()
end_time - start_time # Computation time: 0.02312088 secs
# 6) Compute the covariance and correlation matrices for the features
auto2 <- auto[,c(2:6)] # Created new data set that excludes mpg
start_time <- Sys.time()
cov(auto2)
cor(auto2)
end_time <- Sys.time()
end_time - start_time Computation time: 0.02203393 secs
# 7) Prove eigenvalues L1>L2>L3>L4>L5 of correlation matrix
eigen(cor(auto2))
L1 <- 4.07185982
L2 <- 0.69386125
L3 <- 0.13349305
L4 <- 0.06426839
L5 <- 0.03651750
start_time <- Sys.time()
L1>(L2>(L3>(L4>L5))) # TRUE
end_time <- Sys.time()
end_time - start_time # Computation time: 0.01670694 secs
# 8) Prove L1 + L2 + L3 + L4 + L5 = 5
start_time <- Sys.time()
eigen_sum <- L1 + L2 + L3 + L4 + L5
eigen_sum # Eigenvalue sum is equal to 5
end_time <- Sys.time()
end_time - start_time # Computation time: 0.02139306 secs
# 9,10) For i = 1, 2, 3, 4, 5; compute Ratios Ri = (L1 + L2 + ... + Li)/5
start_time <- Sys.time()
R1 <- L1/5
R1
R2 <- (L1 + L2)/5
R2
R3 <- (L1 + L2 + L3)/5
R3
R4 <- (L1 + L2 + L3 + L4)/5
R4
R5 <- (L1 + L2 + L3 + L4 + L5)/5
R5
end_time <- Sys.time()
end_time - start_time # Computation time: 0.0459311 secs
# 11) Reorder the rows of the dataset in ascending order
start_time <- Sys.time()
newAuto <- auto[order(mpg),]
median_mpg <- median(newAuto$mpg) # Calculate the median of mpg
# Create LOWmpg and HIGHmpg table
attach(newAuto)
LOWmpg <- newAuto[mpg < median_mpg,] # Table where mpg is less than mpg_median
HIGHmpg <- newAuto[mpg > median_mpg,] # Table where mpg is greater than mpg_median
end_time <- Sys.time()
end_time - start_time # Computation time: 0.03594804 secs
# 12,13) Generate side by side histograms for LOWmpg and HIGHmpg features
start_time <- Sys.time()
hist(LOWmpg$cyl,
     main="Histogram of LOWmpg Cylinder",
     xlab="Number of Cylinders",
     ylab="Number of Vehicles",
     col="firebrick3")
hist(HIGHmpg$cyl,
     main="Histogram of HIGHmpg Cylinder",
     xlab="Number of Cylinders",
     ylab="Number of Vehicles",
     col="firebrick3")
hist(LOWmpg$dis,
     main="Histogram of LOWmpg Displacement",
     xlab="Engine Displacement in cubic inches",
     ylab="Number of Vehicles",
     col="darkorange2")
hist(HIGHmpg$dis,
     main="Histogram of HIGHmpg Displacement",
     xlab="Engine Displacement in cubic inches",
     ylab="Number of Vehicles",
     col="darkorange2")
hist(LOWmpg$hor,
     main="Histogram of LOWmpg Horsepower",
     xlab="Engine Horsepower",
     ylab="Number of Vehicles",
     col="darkgreen")
hist(HIGHmpg$hor,
     main="Histogram of HIGHmpg Horsepower",
     xlab="Engine Horsepower",
     ylab="Number of Vehicles",
     col="darkgreen")
hist(LOWmpg$wei,
     main="Histogram of LOWmpg Weight",
     xlab="Vehicle Weight in pounds",
     ylab="Number of Vehicles",
     col="navy")
hist(HIGHmpg$wei,
     main="Histogram of HIGHmpg Weight",
     xlab="Vehicle Weight in pounds",
     ylab="Number of Vehicles",
     col="navy")
hist(LOWmpg$acc,
     main="Histogram of LOWmpg Acceleration",
     xlab="Time to accelerate from 0 to 60 mph in secs",
     ylab="Number of Vehicles",
     col="darkmagenta")
hist(HIGHmpg$acc,
     main="Histogram of HIGHmpg Acceleration",
     xlab="Time to accelerate from 0 to 60 mph in secs",
     ylab="Number of Vehicles",
     col="darkmagenta")
end_time <- Sys.time()
end_time - start_time # Computation time: 0.377131 secs
# 14) Compute the mean and standard deviation for LOWmpg and HIGHmpg features
start_time <- Sys.time()
mlow_cyl <- mean(LOWmpg$cyl)
mlow_cyl
stdlow_cyl <- sd(LOWmpg$cyl)
stdlow_cyl
mlow_dis <- mean(LOWmpg$dis)
mlow_dis
stdlow_dis <- sd(LOWmpg$dis)
stdlow_dis
mlow_hor <- mean(LOWmpg$hor)
mlow_hor
stdlow_hor <- sd(LOWmpg$hor)
stdlow_hor
mlow_wei <- mean(LOWmpg$wei)
mlow_wei
stdlow_wei <- sd(LOWmpg$wei)
stdlow_wei
mlow_acc <- mean(LOWmpg$acc)
mlow_acc
stdlow_acc <- sd(LOWmpg$acc)
stdlow_acc
mhigh_cyl <- mean(HIGHmpg$cyl)
mhigh_cyl
stdhigh_cyl <- sd(HIGHmpg$cyl)
stdhigh_cyl
mhigh_dis <- mean(HIGHmpg$dis)
mhigh_dis
stdhigh_dis <- sd(HIGHmpg$dis)
stdhigh_dis
mhigh_hor <- mean(HIGHmpg$hor)
mhigh_hor
stdhigh_hor <- sd(HIGHmpg$hor)
stdhigh_hor
mhigh_wei <- mean(HIGHmpg$wei)
mhigh_wei
stdhigh_wei <- sd(HIGHmpg$wei)
stdhigh_wei
mhigh_acc <- mean(HIGHmpg$acc)
mhigh_acc
stdhigh_acc <- sd(HIGHmpg$acc)
stdhigh_acc
end_time <- Sys.time()
end_time - start_time # Computation time: 0.07506585 secs
# 15) Compute discr(F) = |mhigh(F) - mlow(F)| / s(F)
# where s(F) = (stdlow(f) + stdhigh(F)) / #sqrt(N)
# Function that computes the discriminatory values of the features
disValue <- function(mhigh, mlow, stdhigh, stdlow){
    value <- abs(mhigh-mlow)/((stdlow+stdhigh)/sqrt(392))
    return (value)}
start_time <- Sys.time()
discr_cyl <- disValue(mhigh_cyl, mlow_cyl, stdhigh_cyl, stdlow_cyl)
discr_cyl
discr_dis <- disValue(mhigh_dis, mlow_dis, stdhigh_dis, stdlow_dis)
discr_dis
discr_hor <- disValue(mhigh_hor, mlow_hor, stdhigh_hor, stdlow_hor)
discr_hor
discr_wei <- disValue(mhigh_wei, mlow_wei, stdhigh_wei, stdlow_wei)
discr_wei
discr_acc <- disValue(mhigh_acc, mlow_acc, stdhigh_acc, stdlow_acc)
discr_acc
end_time <- Sys.time()
end_time - start_time # Computation time: 0.03590107 secs
# 15) Alternative approach to computing discriminatory values of feature F using t-tests
# Vectors that list F values corresponding to the cases belonging to LOWmpg
start_time <- Sys.time()
x_lowcyl = LOWmpg$cyl
x_lowdis = LOWmpg$dis
x_lowhor = LOWmpg$hor
x_lowwei = LOWmpg$wei
x_lowacc = LOWmpg$acc
# Vectors that list F values corresponding to the cases belonging to HIGHmpg
x_highcyl = HIGHmpg$cyl
x_highdis = HIGHmpg$dis
x_highhor = HIGHmpg$hor
x_highwei = HIGHmpg$wei
x_highacc = HIGHmpg$acc
# Perform t-tests on F values
t.test(x_lowcyl, y_highcyl)
t.test(x_lowdis, y_highdis)
t.test(x_lowhor, y_highhor)
t.test(x_lowwei, y_highwei)
t.test(x_lowacc, y_highacc)
end_time <- Sys.time()
end_time - start_time # Computation time: 0.0977459 secs