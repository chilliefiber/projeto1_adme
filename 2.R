##Setting up the subset_data
library(Ecdat)
data("BudgetItaly")
subset_data <- subset(BudgetItaly, year == 73, select = -pfood)



##PCA considering the variables in original scale and the classical sample covariance estimate

pca_original <- prcomp(subset_data, scale = FALSE)
summary(pca_original)
pca_original
plot(pca_original, type = "lines")


#Scores Graph
scores_1.new<-round(pca_original$x,3)
o<-order(scores_1.new[,1]) # Scores sorted by PC1 
scores.new.pc1<-scores_1.new[o,]
scores.new.pc1
plot(scores_1.new[,1], scores_1.new[,2], xlab="PC1", ylab="PC2",type="n", xlim=c(min(scores_1.new[,1]),                                                                                 max(scores_1.new[,2])))
text(scores_1.new[,1],scores_1.new[,2], rownames(scores_1.new), col="blue",cex=1.0)
abline(h=mean(scores_1.new[,2]),col="green")
abline(v=mean(scores_1.new[,1]),col="green")




##PCA considering the standardized variables

#Constant Columns (year) removed to allow standardization
constant_columns <- sapply(subset_data, function(x) var(x, na.rm = TRUE) == 0)
constant_column_names <- names(subset_data)[constant_columns]
subset_data_clean <- subset_data[, !constant_columns]
pca_standardized <- prcomp(subset_data_clean, scale = TRUE)
summary(pca_standardized)

plot(pca_standardized, type = "lines")

