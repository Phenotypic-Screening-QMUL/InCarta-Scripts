library("gplots")
library("RColorBrewer")

# Upload your file 
all <- read.csv("Models Z-Score Vs Control.csv", stringsAsFactors = F) # upload your Z-Score csv


### Can adjust model selection here ### 
samp2 <- all[,-1] # Saves a df that is missing your first column - assumes this is sample labels
rownames(samp2) <- all[,1] # Set your row-names of the new df to be the column you just removed
all<-samp2 # overwrite the original df

#Convert to data matrix 
all_matrix <- t((as.matrix(all))) # data must be a numeric matrix - transpose for heat-map direction 

# Create heat map
breaks <- unique(c(seq(-10,-1.96,length=100),seq(-1.96,1.96,length=100), seq(1.96,10,length=100)))
my_palette <- colorRampPalette(c("Blue","white","white","red"))(length(breaks)-1)
heatmap.2(all_matrix,
          Rowv = T,
          Colv = F,
          col=my_palette,
          breaks=breaks,
          density.info="none",
          trace="none",
          #main = "199 compounds; Length or Prev",
          dendrogram=c("both"), 
          symm=F,symkey=F,symbreaks=T,
          labRow= F,
          labCol= colnames(all_matrix),
          cexCol = 0.7,
          cexRow = 0.8,
          margins = c(5,5),
          key.title = "1" , key.xlab="Z Score", 
          #rowsep =c(0,4,12,19,31,37,43),
          #colsep =c(0,233),
          sepcolor = c("black"),sepwidth = c(0.05, 0.05),
          #ColSideColors=condition_colors, scale="none",
          distfun = function(x) dist(x, method = "euclidean"),
          hclust=function(x) hclust(x,method="ward.D2"))