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



##sigs plotted by context
cspvc <- pvclust(csmatrix, method.hclust= "ward", method.dist= "euclidean")
#Bootstrap (r = 0.44)... Done.
#Bootstrap (r = 0.56)... Done.
#Bootstrap (r = 0.67)... Done.
#Bootstrap (r = 0.78)... Done.
#Bootstrap (r = 0.89)... Done.
#Bootstrap (r = 1.0)... Done.
#Bootstrap (r = 1.0)... Done.
#Bootstrap (r = 1.11)... Done.
#Bootstrap (r = 1.22)... Done.
#Bootstrap (r = 1.33)... Done.
#Warning message:
#In a$p[] <- c(1, bp[r == 1]) :
#  number of items to replace is not a multiple of replacement length

plot(cspvc)
pvrect(cspvc, alpha=0.95)   
print(cspvc)

#Cluster method: ward
#Distance      : euclidean
#
#Estimates on edges:
#
#      au    bp se.au se.bp      v     c  pchi
#1  0.823 0.414 0.020 0.005 -0.355 0.572 0.059
#2  0.830 0.304 0.021 0.005 -0.220 0.733 0.140
#3  0.866 0.371 0.017 0.005 -0.389 0.718 0.544
#4  0.995 0.389 0.001 0.005 -1.132 1.415 0.038
#5  0.919 0.596 0.011 0.005 -0.820 0.578 0.696
#6  0.995 0.446 0.001 0.006 -1.235 1.370 0.121
#7  0.821 0.346 0.021 0.005 -0.261 0.658 0.246
#8  0.986 0.444 0.003 0.005 -1.026 1.167 0.227
#9  0.863 0.372 0.017 0.005 -0.384 0.711 0.011
#10 0.762 0.554 0.023 0.005 -0.425 0.289 0.229
#11 0.988 0.356 0.003 0.005 -0.945 1.314 0.010
#12 0.762 0.192 0.029 0.004  0.078 0.792 0.162
#13 0.975 0.278 0.005 0.005 -0.685 1.275 0.240
#14 0.924 0.079 0.019 0.003 -0.011 1.422 0.726
#15 0.972 0.271 0.006 0.005 -0.648 1.257 0.200
#16 1.000 1.000 0.000 0.000  0.000 0.000 0.000

##red boxes indicate 2 clusters, 13,15
##check diagnostics of these clusters
msplot(cspvc, edges=c(13))
msplot(cspvc, edges=c(15))
  
