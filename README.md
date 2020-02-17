# MSDS-6350-Stat.-Learning-and-Data-Mining
Learn different method of Machine Learning

#Final project(kernel ridge regression)
This data is to predict Electrical Grid Stability
and make sure that the ability of the electric grid to deliver electricity to customers without degradation or failure.
I use the features to predict the stability of the Electrical Grid. 
If it is negative, then the electrical grid is unstable. If it’s positive, then the electrical grid is stable.
I opitimize the parameter, the use the kernel ridge regression to see the performance. 

#HW1
preliminary treatment of the  data set
see the correlation between each features
plot histograms to see which feature has a good capacity to discriminate between high msg and low msg

#HW2 (k nearest neighbor (kNN))
preliminary treatment of the data set
Standardize the features matrix
See correlation between 400 random variables
compute its 400 eigenvalues and its 400 eigenvectors 
Implement the PCA analysis
Display graphically the 2 and 3 dimensional scatterplot
Apply the k nearest neighbor (kNN) algorithm for the automatic classification
optimize the k
use the best k to see confusion matrix for kNN classification

#HW3
use the same data as hw2
split the data into training set and test set
do matrix transposition
Fix k = 5 and apply kNN in the Euclidean space Ra to implement the automatic classification
see the performance on TEST and on TRAIN
Compare to the results already obtaine in HW2 for kNN classification with k=5
Repeat the preceding automatic classification by kNN with k=5, but based on the vectors Gi ε Rb-a
Compare these results to the preceding results
apply the unsupervized Kmean algorithm in Ra to implement automatic clustering
optimize the cost

#HW4 (SVM classification)
The data is a Vicon motion capture camera system that used to record users performing 5 hand postures with markers attached to a left-handed glove.
Center and Rescale the data
Split each class into a training set and a test set
SVM classification by radial kernel, optimize the parameters, Re-evaluation of tuning
see the confusion matrices
do again with polynomial kernel
compare them
