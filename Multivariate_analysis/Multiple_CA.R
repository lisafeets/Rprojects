##Multiple Correspondence Analysis with categories Signaller Recipient Gesture Action
##data on just the top 6 females

srga=read.table("SRGAfemales.txt",h=T)

## do multiple corresp on data, set up obejcts for plotting
##series of code from Gaston Sanchez http://gastonsanchez.wordpress.com/2012/10/13/5-functions-to-do-multiple-correspondence-analysis-in-r/
require(FactoMineR)
mca2 = MCA(srga[,2:5], graph=FALSE)
cats = apply(srga[,2:5], 2, function(x) nlevels(as.factor(x)))
mca1_vars_df = data.frame(mca2$var$coord, Variable = rep(names(cats), cats))
mca1_obs_df = data.frame(mca2$ind$coord)

##plot the Mutliple Correspondence analysis with all variables showing
library(ggplot2)
ggplot(data=mca1_vars_df, aes(x=Dim.1, y=Dim.2, label=rownames(mca1_vars_df))) +
       geom_hline(yintercept=0, colour="gray70") +
       geom_vline(xintercept=0, colour="gray70") +
       geom_text(aes(colour=Variable)) +
       ggtitle("MCA plot of communication variables for the top 6 females")


##plot MCA without the gestures
##take out gestures from middle of mca1_vars_df
mcr=rbind(mca1_vars_df[1:12,],mca1_vars_df[60:77,])
str(mcr)
#'data.frame':  32 obs. of  6 variables:
#  $ Dim.1   : num  0.476 -0.315 -0.868 0.167 -0.206 ...
#$ Dim.2   : num  0.102 -0.597 0.295 1.371 -0.489 ...
#$ Dim.3   : num  -0.411 -0.953 0.201 0.489 0.357 ...

##plot data, saved as "Rplot_MCA.jpg"
library(ggplot2)
ggplot(data=mcr, aes(x=Dim.1, y=Dim.2, label=rownames(mcr))) +
       geom_hline(yintercept=0, colour="gray70") +
       geom_vline(xintercept=0, colour="gray70") +
       geom_text(aes(colour=Variable)) +
       ggtitle("MCA plot of communication variables for the top 6 females")