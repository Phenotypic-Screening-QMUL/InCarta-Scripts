library("gplots")
library("RColorBrewer")
library("dplyr")
library("matrixStats")
library("plyr")

#This is getting data in right format if it isnt
numdata <- #Your Data Here
numdatadf <- data.frame(numdata)
numdatadf <- sapply(numdatadf,as.numeric)

#This is for corrplot
library("corrplot")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

#This Generates Correlation Matrix
df_cor <- cor(numdatadf, use=c("pairwise.complete.obs"))
#makes labels fit in margins
par(xpd=TRUE)
corrplot(df_cor, method="color", col=col(20),  
         type="upper", order="hclust", tl.cex = 0.01, # Add coefficient of correlation
         tl.col="black", tl.srt=90, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=T,mar = c(2, 0, 1, 0))


library("caret")
High_corr <- findCorrelation(df_cor, cutoff = .9, verbose = TRUE, names = TRUE)


hc = findCorrelation(df_cor, cutoff=0.9) # putt any value as a "cutoff"
hc = sort(hc)
reduced_Data = df_cor[,-c(hc)]
reduced_Data <- as.data.frame(reduced_Data)

#Full Matrix - Including Highly Correlated
write.csv(df_cor, "Correlation Matrix.csv")
#Remaining Measures Matrix
write.csv(reduced_Data, "Reduced Data.csv")
#Removed Measures
write.csv(High_corr, "Correlated Measures.csv")