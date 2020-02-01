CALIBRI <- read.csv("C:/Users/yingy/Desktop/11 data mining/hw2/CALIBRI.csv")
COURIER <- read.csv("C:/Users/yingy/Desktop/11 data mining/hw2/COURIER.csv")
TIMES <- read.csv("C:/Users/yingy/Desktop/11 data mining/hw2/TIMES.csv")
#KEEP the 3 columns {font, strength, italic}
#KEEP the 400 columns named r0c0, r0c1,r0c2, ... , r19c18, r19c19
courier <- COURIER[,c(1,4,5,13:412)]
calibri <- CALIBRI[,c(1,4,5,13:412)]
times <- TIMES[,c(1,4,5,13:412)]

#CL1 = all rows of COURIER.csv file for which {row # >1 and strength = 0.4 and italic=0}
#CL2 = all rows of CALIBRI.csv file for which {row # >1 and strength = 0.4 and italic=0}
#CL3 = all rows of TIME.csv file for which {row # >1 and strength = 0.4 and italic=0}
CL1 <- courier[(courier$strength==0.4) & (courier$italic==0),] 
CL2 <- calibri[(calibri$strength==0.4) & (calibri$italic==0),] 
CL3 <- times[(times$strength==0.4) & (times$italic==0),]

# Sizes of CL1, CL2, CL3
n1 <- nrow(CL1)
n1
n2 <- nrow(CL2)
n2
n3 <- nrow(CL3)
n3
# Full dataset (union of CL1, CL2, and CL3) and its size
union1 <- rbind(CL1, CL2)
DATA <- rbind(union1, CL3)
N <- nrow(DATA)
N

# 1.1) Correlation Matrix COR
start_time <- Sys.time()
COR <- cor(SDATA[,4:403])
# Output COR into Excel file
write.csv(round(COR,2), file = "correlation matrix.csv")
end_time <- Sys.time()
end_time - start_time # Computation time: 2.50 secs

# 1.2) Eigenvalues and eigenvectors for matrix COR
start_time <- Sys.time()
eigen <- eigen(COR)
eigenvalues <- eigen$values
sum(eigenvalues) # Sum of eigenvalues is equal to 400 
eigenvectors <- eigen$vectors
# Output eigenvalues and eigenvectors into Excel files
# Each eigenvalue is a nonzero value greater than 0
write.csv(round(eigenvalues,2), file = "eigenvalues.csv")
write.csv(round(eigenvectors,2), file = "eigenvectors.csv")
end_time <- Sys.time()
end_time - start_time # Computation time: 0.65 secs

# 1.3) Plot the decreasing curve £fj versus j for j=1 , 2, ..., 400
start_time <- Sys.time()
plot(eigenvalues, type="o", col="springgreen3", ann="FALSE")
title(main="Decreasing curve £fj versus j", xlab="j", ylab="Eigenvalues (£fj)")
end_time <- Sys.time()
end_time - start_time # Computation time: 0.056 secs

# 1.4) The successive percentages Rj given by Rj = (£f1 + £f2 + ... + £fj)/400 for  j =1, 2,  ... , 400
start_time <- Sys.time()
Rj <- 1:400
sum <- 0
for(i in 1:400)
{
  sum <- sum + eigenvalues[i]
  percentage <- sum/400
  Rj[i] = percentage
}
# Output percentages into Excel file
write.csv(round(Rj,2), file = "percentage.csv")
end_time <- Sys.time()
end_time - start_time # Computation time: 0.40 secs

# 1.5) The increasing curve Rj versus j for j=1 , 2, ..., 400 
start_time <- Sys.time()
plot(Rj, type="o", col="slateblue3", ann="FALSE")
title(main="Increasing curve Rj versus j", xlab="j", ylab="Percentages (Rj)")
# The smallest integer "r" such that Rr >90%
smallest <- 0
for(i in 1:400)
{
  if(Rj[i] > 0.90)
  {
    smallest <- i
    break
  }
}
smallest # Smallest integer of "r" is 77
Rj[smallest] # 0.901 (90.1%)
end_time <- Sys.time()
end_time - start_time # Computation time: 0.088 secs

# 1.6) The relationship between these computations and the PCA analysis of the set DATA
# Perform PCA on DATA
start_time <- Sys.time()
pDATA=DATA[,4:403]
pr.out=prcomp(pDATA , scale=FALSE)
pr.var=pr.out$sdev^2
summary(pr.out)
pve =pr.out$sdev ^2/sum(pr.out$sdev^2) 
par(mfrow=c(1,2)) 
plot(pve , type="o", ylab="PVE", xlab="Principal Component ", col="blue") 
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab=" Principal Component ", col="brown3")
end_time <- Sys.time()
end_time - start_time # Computation time: 11.69 secs

# 1.7) Implement PCA Analysis
start_time <- Sys.time()
pca <- prcomp(SDATA[,c(4:403)], scale=FALSE, center=FALSE)
# Install library ggfortify to plot PCA
devtools::install_github("sinhrks/ggfortify") # Select option 1
library(ggfortify)
ggplot2::autoplot(pca, label = FALSE, loadings.label = TRUE)
pca.sdev <- pca$sdev
pca.var <- pca.sdev^2
pca.pve <- pca.var/sum(pca.var)
# Plots of pve and cumulative pve
plot(pca.pve, xlab=" Principal Component ", ylab=" Proportion of
Variance Explained ", ylim=c(0,0.15), type="b", col="dodgerblue4")
plot(cumsum(pca.pve), xlab=" Principal Component ", ylab ="
Cumulative Proportion of Variance Explained ", ylim=c(0,1),
     type="b", col="firebrick3")
end_time <- Sys.time()
end_time - start_time # 19.0 secs

# 1.8) The first three "scores" of example "i" from the transpose of SDATA
# Transpose the SDATA matrix and create v1, v2, and v3 from the eigenvectors
start_time <- Sys.time()
TSDATA <- t(SDATA[,4:403])
v1 <- eigenvectors[,1]
v2 <- eigenvectors[,2]
v3 <- eigenvectors[,3]
# Calculate the first three scores
scor1 <- numeric(0)
scor2 <- numeric(0)
scor3 <- numeric(0)
for (i in 1:ncol(TSDATA))
{
  scor1[i] = TSDATA[,i] %*% v1
  scor2[i] = TSDATA[,i] %*% v2
  scor3[i] = TSDATA[,i] %*% v3
}
end_time <- Sys.time()
end_time - start_time # Computation time: 1.06 secs

# 1.9) 2 dimensional scatterplot of all the Wi , i= 1,2,...,N
start_time <- Sys.time()
SDATA.font <- SDATA$font
colors <- c("red1", "mediumblue", "forestgreen")[SDATA.font]
plot(pca$x[,1:2], col=colors, pch=19, xlab ="scor1", ylab="scor2")
end_time <- Sys.time()
end_time - start_time # Computation time: 1.24 secs

# 1.10) 3 dimensional scatterplot of all the Ui , i= 1,2,...,N
# Function that hides desired color (used to exclude classes on plot)
# Credit: http://www.dataanalytics.org.uk
t_col <- function(color, percent = 100) {
  rgb.val <- col2rgb(color)
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100-percent)*255/100)
  invisible(t.col)
}
# Install and load package to create 3D scatterplot
start_time <- Sys.time()
install.packages("scatterplot3d") 
library("scatterplot3d")
colors <- c("red1", "mediumblue", t_col("forestgreen"))[SDATA.font]
scatterplot3d(scor1, scor2, scor3, pch=16, color=colors, grid=TRUE, box=TRUE, 
              xlab="scor1", zlab="scor2", ylab="scor3")
colors <- c("red1", t_col("mediumblue"), "forestgreen")[SDATA.font]
scatterplot3d(scor1, scor2, scor3, pch=16, color=colors, grid=TRUE, box=TRUE,
              xlab="scor1", zlab="scor2", ylab="scor3")
colors <- c(t_col("red1"), "mediumblue", "forestgreen")[SDATA.font]
scatterplot3d(scor1, scor2, scor3, pch=16, color=colors, grid=TRUE, box=TRUE,
              xlab="scor1", zlab="scor2", ylab="scor3")
end_time <- Sys.time()
end_time - start_time # Computation time: 9.51 secs

# Code for Part 2

# 2.1) kNN algorithm on SDATA for  k=15
# Randomly reorder rows of SDATA
start_time <- Sys.time()
set.seed(1)
rows <- sample(nrow(SDATA)) 
SDATA.subset <- SDATA[rows,]
# Convert font to factor variable (response variable to predict classes)
SDATA.subset$font <- factor(SDATA.subset$font)
# Split training and test set using an 80:20 ratio
split <- floor(nrow(SDATA)*.20)
test <- 1:split
train.font <- SDATA.subset$font[-test]
test.font <- SDATA.subset$font[test]
# Choose the 400 features to predict the classes
SDATA.subset <- SDATA.subset[,4:403]
train.SDATA <- SDATA.subset[-test,]
test.SDATA <- SDATA.subset[test,]
library(class)
# k=15 and per(15)
knn.15 <- knn(train.SDATA, test.SDATA, train.font, k=15)
table(knn.15, test.font)
per.15 <- round(((594+740+775)/split),4)
per.15
end_time <- Sys.time()
end_time - start_time # Computation time: 40.71 secs

# 2.2) kNN algorithm on SDATA for k =5, 10, 15, 20, 30, 40, 50, 100, 200
start_time <- Sys.time()
# k=5 and per(5)
knn.5 <- knn(train.SDATA, test.SDATA, train.font, k=5)
table(knn.5, test.font)
per.5 <- round(((638+787+779)/split),4)
per.5
# k=10 and per(10)
knn.10 <- knn(train.SDATA, test.SDATA, train.font, k=10)
table(knn.10, test.font)
per.10 <- round(((614+760+774)/split),4)
per.10
# Refer to 2.1 for k=15 and per(15)
# k=20 and per(20)
knn.20 <- knn(train.SDATA, test.SDATA, train.font, k=20)
table(knn.20, test.font)
per.20 <- round(((572+718+776)/split),4)
per.20
# k=30 and per(30)
knn.30 <- knn(train.SDATA, test.SDATA, train.font, k=30)
table(knn.30, test.font)
per.30 <- round(((558+689+768)/split),4)
per.30
# k=40 and per(40)
knn.40 <- knn(train.SDATA, test.SDATA, train.font, k=40)
table(knn.40, test.font)
per.40 <- round(((535+655+772)/split),4)
per.40
# k=50 and per(50)
knn.50 <- knn(train.SDATA, test.SDATA, train.font, k=50)
table(knn.50, test.font)
per.50 <- round(((511+637+757)/split),4)
per.50
# k=100 and per(100)
knn.100 <- knn(train.SDATA, test.SDATA, train.font, k=100)
table(knn.100, test.font)
per.100 <- round(((505+540+767)/split),4)
per.100
# k=200 and per(100)
knn.200 <- knn(train.SDATA, test.SDATA, train.font, k=200)
table(knn.200, test.font)
per.200 <- round(((504+425+737)/split),4)
per.200
# Curve per(k) versus k 
pers <- c(per.5, per.10, per.15, per.20, per.30, per.40, per.50,
          per.100, per.200)
k <- c(5,10,15,20,30,40,50,100,200)
plot(k, pers, type="b", col="chocolate1", ann=FALSE)
title(main="Curve per(k) versus k", xlab="k", ylab="Percentage of correct classifications per(k)")
end_time <- Sys.time()
end_time - start_time # Computation time: 6.38 mins

# 2.3) Selecting a "best" value k* for the integer k
start_time <- Sys.time()
# k=1 and per(1)
knn.1 <- knn(train.SDATA, test.SDATA, train.font, k=1)
table(knn.1, test.font)
per.1 <- round(((696+841+844)/split),4)
per.1
# k=2 and per(2)
knn.2 <- knn(train.SDATA, test.SDATA, train.font, k=2)
table(knn.2, test.font)
per.2 <- round(((655+809+810)/split),4)
per.2
# k=3 and per(3)
knn.3 <- knn(train.SDATA, test.SDATA, train.font, k=3)
table(knn.3, test.font)
per.3 <- round(((655+806+802)/split),4)
per.3
# k=4 and per(4)
knn.4 <- knn(train.SDATA, test.SDATA, train.font, k=4)
table(knn.4, test.font)
per.4 <- round(((633+802+797)/split),4)
per.4
end_time <- Sys.time()
end_time - start_time # Computation time: 3.30 mins

# 2.4) 3x3 confusion matrix for kNN classification using the "best" k= k*
start_time <- Sys.time()
table(knn.1, test.font)
end_time <- Sys.time()
end_time - start_time # Computation time: 0.018 secs


