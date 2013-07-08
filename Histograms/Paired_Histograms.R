##Making paired histogram for TotalTypeGestures between males and females

##get data
repertoire=read.table("Individual Gesture Repertoire April 22.txt", h=T)
attach(repertoire)

##get paired histogram for with default hist()
females=subset(repertoire, Sex == "f", select=TotalTypeGestures)
males=subset(repertoire, Sex == "m", select=TotalTypeGestures)

##get ggplot paired histograms, saved as "Rplot paired histogram totaltypegestures.pdf"
library(ggplot2)
ggplot() + 
   geom_histogram(data=females, aes(TotalTypeGestures, fill="Female", y= ..count..), binwidth=5) +
   geom_histogram(data=males, aes(TotalTypeGestures, fill="Male", y= -..count..),binwidth=5) +
   scale_fill_hue("Sex")