library("gplots")
library("RColorBrewer")
library("matrixStats")
library("plyr")
library("dplyr")
library("data.table")
library("stringr")

# Remove Single Target Only and Non-Numeric 
drop_cols <- c(1:4,6:10,11:14,29:31,34,51:54,75:77,80,89:92) # removes pre-determined columns - need to adjust for your csv file


# Import Single Target Data
Ind1_ST <- fread("OIS_Ind_1_ST.csv", drop = drop_cols,stringsAsFactors = F)
Ind2_ST <- fread("OIS_Ind_2_ST.csv", drop = drop_cols,stringsAsFactors = F)
Ind3_ST <- fread("OIS_Ind_3_ST.csv", drop = drop_cols,stringsAsFactors = F)

# Filter by Sample Type
Vector_Ind_ST1 <- Ind1_ST %>% 
  filter(`WELL LABEL` %in% c("B - 2","B - 3","B - 4"))
Vector_Ind_ST2 <- Ind2_ST %>% 
  filter(`WELL LABEL` %in% c("B - 2","B - 3","B - 4"))
Vector_Ind_ST3 <- Ind3_ST %>% 
  filter(`WELL LABEL` %in% c("B - 2","B - 3","B - 4"))


OIS_Ind_ST1 <- Ind1_ST %>% 
  filter(`WELL LABEL` %in% c("B - 7","B - 8","B - 9"))
OIS_Ind_ST2 <- Ind2_ST %>% 
  filter(`WELL LABEL` %in% c("B - 7","B - 8","B - 9"))
OIS_Ind_ST3 <- Ind3_ST %>% 
  filter(`WELL LABEL` %in% c("B - 7","B - 8","B - 9"))

# Set Vectors with Sample Labels - could have used mutate dplyr function
ec_Ind <- c("Vec_Ind")
OIS_Ind <- c("OIS_Ind")

#Add the label column
Vector_Ind_ST1 <- cbind(Vec_Ind, Vector_Ind_ST1)
Vector_Ind_ST2 <- cbind(Vec_Ind, Vector_Ind_ST2)
Vector_Ind_ST3 <- cbind(Vec_Ind, Vector_Ind_ST3)

#Add the label column
OIS_Ind_ST1 <- cbind(OIS_Ind, OIS_Ind_ST1)
OIS_Ind_ST2 <- cbind(OIS_Ind, OIS_Ind_ST2)
OIS_Ind_ST3 <- cbind(OIS_Ind, OIS_Ind_ST3)

#Ensure all Data frames have the same column names (or wont be able to join later)
colnames <- c("colnames",colnames(Vector_Ind_ST1))
colnames <- colnames[-c(2)]

colnames(Vector_Ind_ST1) <- colnames
colnames(Vector_Ind_ST2) <- colnames
colnames(Vector_Ind_ST3) <- colnames
colnames(OIS_Ind_ST1) <- colnames
colnames(OIS_Ind_ST2) <- colnames
colnames(OIS_Ind_ST3) <- colnames

#Join each replicate (N number)
Ind_1 <- rbind(Vector_Ind_ST1,OIS_Ind_ST1)
Ind_2 <- rbind(Vector_Ind_ST2,OIS_Ind_ST2)
Ind_3 <- rbind(Vector_Ind_ST3,OIS_Ind_ST3)


#Scale (standardise) **WITHIN** Replicates
Ind_1_Scale <- Ind_1 %>%
  mutate_if (is.numeric, scale)

Ind_2_Scale<- Ind_2 %>%
  mutate_if (is.numeric, scale)

Ind_3_Scale <- Ind_3 %>%
  mutate_if (is.numeric, scale)

#Join Replicates
Ind_Scale <- rbind(Ind_1_Scale,Ind_2_Scale,Ind_3_Scale)

#remove unnecessary column
Ind_Scale <- Ind_Scale[,-2]

#Filter For each condition
Ind_OIS <- Ind_Scale %>%
  filter(`colnames` %in% "OIS_Ind")
Ind_Vec <- Ind_Scale %>%
  filter(`colnames` %in% "Vec_Ind")

#Get median values for each sample type
library("matrixStats")
Ind_OIS_Med <- colMedians(as.matrix(Ind_OIS[,-1]))
Ind_Vec_Med <- colMedians(as.matrix(Ind_Vec[,-1]))

#Put it back together
Meds <- rbind(Ind_Vec_Med,Ind_OIS_Med)

#Prepare dataframe for heatmap
rownames(Meds) <- c("Vec_Ind", "OIS_Ind")
colnames(Meds) <-colnames[-c(1,2)]

#Load extra packages
library("gplots")
library("RColorBrewer")

# Transpose as required
all_matrix <- t(Meds)

# Create heat map
breaks <- unique(c(seq(-1,-0.01,length=100),seq(-0.01,0.01,length=100), seq(0.01,1,length=100)))
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
          cexCol = 1.5,
          cexRow = 0.8,
          margins = c(7,15),
          key.title = "1" , key.xlab="Standard Score", 
          #rowsep =c(0,4,12,19,31,37,43),
          #colsep =c(0,233),
          sepcolor = c("black"),sepwidth = c(0.05, 0.05),
          #ColSideColors=condition_colors, scale="none",
          distfun = function(x) dist(x, method = "euclidean"),
          hclust=function(x) hclust(x,method="ward.D2"))  