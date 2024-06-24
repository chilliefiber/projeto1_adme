library(Ecdat)
data("BudgetItaly")
seven3_no_pfood_set <- subset(BudgetItaly, year == 73, select = -pfood)
#since the year is always 73, to aid visualization I removed it from the set
final_subset_data <- subset(seven3_no_pfood_set, select = -year)

# gives Min, 1st and 3rd quartile, Median, Mean, and Max in a nice table
summary(final_subset_data)

library(psych)
psych::describe(final_subset_data)

boxplot(final_subset_data[, c(3, 4, 6, 8)], col=c("green","red", "blue", "yellow"))

# Detecting outliers with mahalanobis distance
library(MASS)

# Calculate Mahalanobis distance for each observation
mahalanobis_distances <- mahalanobis(final_subset_data, colMeans(final_subset_data), cov(final_subset_data))

print(qchisq(0.975, df = 8))
# Determine the threshold. 8 degrees of freedom because we are analyzing 8 variables. 0.975 comes from the professor's pdf
threshold <- sqrt(qchisq(0.975, df = 8))

# Plotting
plot(mahalanobis_distances, type = 'p', pch = 20, col = ifelse(mahalanobis_distances > threshold, "red", "black"),
     xlab = "Observation Index", ylab = "Mahalanobis Distance")
abline(h = threshold, col = "blue", lty = 2)

# Adding a legend
legend("topright", legend = c("Outlier", "Non-Outlier", "Threshold"),
       col = c("red", "black", "blue"), lty = c(1, 1, 2), pch = c(20, 20, NA))

# Detecting outliers with robust Mahalanobis distance

# Install and load the robustbase package
if (!require("robustbase")) install.packages("robustbase")
library(robustbase)

# Compute MCD
mcd_result <- covMcd(final_subset_data)

# Calculate Mahalanobis distance using robust mean and covariance
robust_mahalanobis <- mahalanobis(final_subset_data, center = mcd_result$center, cov = mcd_result$cov)

# Plotting
plot(robust_mahalanobis, type = 'p', pch = 20, col = ifelse(robust_mahalanobis > threshold, "red", "black"),
     xlab = "Observation Index", ylab = "Robust Mahalanobis Distance")
abline(h = threshold, col = "blue", lty = 2)

# Adding a legend
legend("topright", legend = c("Outlier", "Non-Outlier", "Threshold"),
       col = c("red", "black", "blue"), lty = c(1, 1, 2), pch = c(20, 20, NA))

# Calculate the correlation matrix
round(cor(final_subset_data), digits=3)