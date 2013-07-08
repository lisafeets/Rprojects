##PVClusters
##au is the p-value, values over 95 are significnat
##from the package description:
#pvclust is an R package for assessing the uncertainty in hierarchical cluster analysis. For each cluster in hierarchical clustering, quantities called p-values are calculated via #multiscale bootstrap resampling. P-value of a cluster is a value between 0 and 1, which indicates how strong the cluster is supported by data.
#pvclust provides two types of p-values: AU (Approximately Unbiased) p-value and BP (Bootstrap Probability) value. AU p-value, which is computed by multiscale bootstrap #resampling, is a better approximation to unbiased p-value than BP value computed by normal bootstrap resampling.
#pvclust performs hierarchical cluster analysis via function hclust and automatically computes p-values for all clusters contained in the clustering of original data. It also #provides graphical tools such as plot function or useful pvrect function which highlights clusters with relatively high/low p-values. Furthermore, parallel computation is #available via snow package.
#An example of analysis on Boston data (in library MASS) is shown in the right figure. 14 attributes of houses are examined and hierarchical clustering has been done. Values on #the edges of the clustering are p-values (%). Red values are AU p-values, and green values are BP values. Clusters with AU larger than 95% are highlighted by rectangles, #which are strongly supported by data.

library(pvclust)

##saved as ""Rplot Dendrogram Signaller Action.pdf"
 PVClust <- pvclust(samatrix, method.hclust= "ward", method.dist= "euclidean")
Bootstrap (r = 0.5)... Done.
Bootstrap (r = 0.57)... Done.
Bootstrap (r = 0.64)... Done.
Bootstrap (r = 0.79)... Done.
Bootstrap (r = 0.86)... Done.
Bootstrap (r = 1.0)... Done.
Bootstrap (r = 1.07)... Done.
Bootstrap (r = 1.14)... Done.
Bootstrap (r = 1.29)... Done.
Bootstrap (r = 1.36)... Done.
> plot(PVClust, frame= F)

