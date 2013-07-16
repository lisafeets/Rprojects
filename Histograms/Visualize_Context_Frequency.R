# create a bar plot of gesture frequency across contexts


bonobo2=read.table("Bonobo Gestures April 15 2.txt", h=T)
attach(bonobo2)

ContextDataFrame=as.data.frame(bonobo2$Context)

# plot data with colors and labels, saved under /images/Rplot_Gestures_per_Context
ggplot(data=ContextDataFrame, aes(x=Var1, y=Freq)) + 
     geom_bar(colour="black", fill="#DD8888", width=.7, stat="identity") + 
     guides(fill=FALSE) +
     xlab("Context") + ylab("Frequency of gestures") +
     ggtitle("Number of gestures per context")