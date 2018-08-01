#########################################################
### A) Installing and loading required packages
#########################################################

if (!require("gplots")) {my_pa
   install.packages("gplots", dependencies = TRUE)
   library(gplots)
   }
if (!require("RColorBrewer")) {
   install.packages("RColorBrewer", dependencies = TRUE)
   library(RColorBrewer)
   }


#########################################################
### B) Reading in data and transform it into matrix format
#########################################################

data <- read.csv("~/Documents/PH_SOMA_SERUM/NEW/FC_top_ttest_2.csv", comment.char="#")
rnames <- data[,1]                            # assign labels in column 1 to "rnames"
mat_data <- data.matrix(data[,2:ncol(data)])  # transform column 2-5 into a matrix
rownames(mat_data) <- rnames                  # assign row names


#########################################################
### C) Customizing and plotting the heat map
#########################################################

# creates a own color palette from red to green
my_palette <- colorpanel(n=299, low ="blue", mid = "green4", high= "red1")

# (optional) defines the color breaks manually for a skewed color transition
col_breaks = c(seq(0,0.85,length.out=100),seq(0.8501,1.15,length.out=100), seq(1.15101,2,length.out=100))

# creates a 5 x 5 inch image
tiff("~/Documents/PH_SOMA_SERUM/NEW/FC_SRM_tt_qn_1.tiff",    # create PNG for the heat map        
width = 8*300,        # 5 x 300 pixels
height = 10*300,
res = 300,            # 300 pixels per inchd
pointsize =10)        # smaller font size

# For gene expression (fold changes), canberra distance appears to be giving best results

row_distance = dist(mat_data, method = "canberra")
row_cluster = hclust(row_distance, method = "complete")
col_distance = dist(t(mat_data), method = "canberra")
col_cluster = hclust(col_distance, method = "complete")

heatmap.2(mat_data,
  #cellnote = mat_data,#  # same data set for cell labels
  main = "Fold protein changes", # heat map title
  notecol="black",      # change font color of cell labels to black
  density.info="none",  # turns off density plot inside color legend
  trace="none",         # turns off trace lines inside the heat map
  margins =c(12,9),     # widens margins around plot
  col=my_palette,       # use on color palette defined earlier
  breaks=col_breaks,    # enable color transition at specified limits
  Rowv = as.dendrogram(row_cluster), # apply default clustering method
  Colv = as.dendrogram(col_cluster), # apply default clustering method
  symkey=F)
dev.off()               # close the TIFF device
