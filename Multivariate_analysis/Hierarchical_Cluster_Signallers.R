##PVClusters
##au is the p-value, values over 95 are significnat
##bp is the bootstrap method
#pvclust is an R package for assessing the uncertainty in hierarchical cluster analysis. For each cluster in hierarchical clustering, quantities called p-values are calculated via #multiscale bootstrap resampling. P-value of a cluster is a value between 0 and 1, which indicates how strong the cluster is supported by data.
#pvclust provides two types of p-values: AU (Approximately Unbiased) p-value and BP (Bootstrap Probability) value. AU p-value, which is computed by multiscale bootstrap #resampling, is a better approximation to unbiased p-value than BP value computed by normal bootstrap resampling.
#pvclust performs hierarchical cluster analysis via function hclust and automatically computes p-values for all clusters contained in the clustering of original data. It also #provides graphical tools such as plot function or useful pvrect function which highlights clusters with relatively high/low p-values. Furthermore, parallel computation is #available via snow package.
#An example of analysis on Boston data (in library MASS) is shown in the right figure. 14 attributes of houses are examined and hierarchical clustering has been done. Values on #the edges of the clustering are p-values (%). Red values are AU p-values, and green values are BP values. Clusters with AU larger than 95% are highlighted by rectangles, #which are strongly supported by data.
library(pvclust)

allsgca=read.table("All SGCA.txt",h=T)
attach(allsgca)

##create matrices for clustering

asmatrix=matrix(table(Action, Signaller),16,17)


snames=sort(unique(Signaller))
anames=sort(unique(Action))


row.names(asmatrix)=anames
colnames(asmatrix)=snames

## plot dendrogram with signallers displayed and grouped by either context or action

##sigs plotted by action
aspvc <- pvclust(asmatrix, method.hclust= "ward", method.dist= "euclidean")
#Bootstrap (r = 0.5)... Done.
#Bootstrap (r = 0.56)... Done.
#Bootstrap (r = 0.69)... Done.
#Bootstrap (r = 0.75)... Done.
#Bootstrap (r = 0.88)... Done.
#Bootstrap (r = 1.0)... Done.
#Bootstrap (r = 1.06)... Done.
#Bootstrap (r = 1.19)... Done.
#Bootstrap (r = 1.25)... Done.
#Bootstrap (r = 1.38)... Done.
plot(aspvc)
pvrect(aspvc, alpha=0.95)
print(aspvc)
##saved as "Rplot_Dendrogram.jpg"

#Cluster method: average
#Distance      : euclidean
#
#Estimates on edges:
#
#      au    bp se.au se.bp      v     c  pchi
#1  0.985 0.646 0.003 0.005 -1.267 0.893 0.088
#2  0.993 0.249 0.002 0.005 -0.894 1.571 0.328
#3  0.966 0.332 0.007 0.005 -0.694 1.127 0.675
#4  0.999 0.353 0.000 0.005 -1.398 1.775 0.321
#5  0.999 0.382 0.000 0.005 -1.372 1.673 0.572
#6  0.987 0.601 0.003 0.005 -1.236 0.981 0.021
#7  0.807 0.485 0.021 0.005 -0.414 0.452 0.323
#8  0.908 0.206 0.016 0.004 -0.253 1.075 0.360
#9  0.922 0.137 0.016 0.004 -0.160 1.256 0.572
#10 0.981 0.196 0.005 0.004 -0.605 1.463 0.535
#11 0.972 0.720 0.005 0.005 -1.248 0.665 0.797
#12 0.979 0.405 0.004 0.005 -0.896 1.135 0.833
#13 0.957 0.219 0.009 0.004 -0.472 1.249 0.447
#14 0.834 0.392 0.020 0.005 -0.348 0.622 0.305
#15 0.668 0.390 0.028 0.005 -0.078 0.356 0.051
#16 1.000 1.000 0.000 0.000  0.000 0.000 0.000

##red boxes indicate 4 clusters, 6,9,11,12
##check diagnostics of these clusters
msplot(aspvc, edges=c(9))
msplot(aspvc, edges=c(11))
msplot(aspvc, edges=c(12))
msplot(aspvc, edges=c(6))

