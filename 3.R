##Setting up the subset_data
library(Ecdat)
data("BudgetItaly")
subset_data <- subset(BudgetItaly, year == 73, select = -pfood)


#Introducing 5 outliers
outlier_data <- subset_data
outlier_data[1:5, ] <- outlier_data[1:5, ] * 0.01

#Classical PCA 
pca_classical <- prcomp(outlier_data, scale = FALSE)
summary(pca_classical)
pca_classical
plot(pca_classical, type = "lines")

#Robust PCA based on the MCD estimate
library(rrcov)
pca_robust <- PcaHubert(outlier_data)
summary(pca_robust)
pca_robust$loadings
pca_robust$eigenvalues
pca_robust$scores

