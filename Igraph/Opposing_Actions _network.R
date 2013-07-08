##from the correspondence analysis of Signallers x Actions looking at differences in high-contributing Actions to dimension 1
##Here I present the social networks described by those two opposing Actions


##plot social networkfor each action (move away, follow ahead, climb on me, play start contact, move into position)
## climb on me
com=read.table("Source Target Climb On Me.txt")
attach(com)

##plot saved as "Rplot_COM_network.pdf"
library(ggplot2)
library(igraph)
g2 = graph.data.frame(com)
plot(g2,layout=layout.kamada.kawai,vertex.size=3,
      vertex.label.dist=0.5, vertex.label.cex=0.8, vertex.label.font=2,
      edge.arrow.size=0.3)

##move into position
mip=read.table("Source Target Move Into Position.txt")
attach(mip)

##plot saved as "Rplot_MIP_network.pdf"
g2 = graph.data.frame(mip)
plot(g2,layout=layout.kamada.kawai,vertex.size=3,
      vertex.label.dist=0.5, vertex.label.cex=0.8, vertex.label.font=2,
      edge.arrow.size=0.3)

       

