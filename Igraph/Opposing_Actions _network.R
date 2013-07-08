##from the correspondence analysis of Signallers x Actions looking at differences in high-contributing Actions to dimension 1
##Here I present the social networks described by those two opposing Actions


##plot social networkfor each action (move away, follow ahead, climb on me, play start contact, move into position)
## climb on me
com=read.table("Source Target Climb On Me.txt")
attach(com)

##plot saved as "Rplot Directed Climb On Me network.pdf"
library(ggplot2)
library(igraph)
g2 = graph.data.frame(com)
plot(g2,layout=layout.kamada.kawai,vertex.size=3,
      vertex.label.dist=0.5, vertex.label.cex=0.8, vertex.label.font=2,
      edge.arrow.size=0.3)

##move into position
mip=read.table("Source Target Move Into Position.txt")
attach(mip)

##plot saved as "Rplot Directed Move Into Position network.pdf"
g2 = graph.data.frame(mip)
plot(g2,layout=layout.kamada.kawai,vertex.size=3,
      vertex.label.dist=0.5, vertex.label.cex=0.8, vertex.label.font=2,
      edge.arrow.size=0.3)


##both climb on me and move into position on the same circle graph
##from https://gist.github.com/kjhealy/834774 kjhealy
commip=rbind(mip, com)
g2=graph.data.frame(commip)


##function created by kjhealy to move vertex labels outside of the circle graph
##original code had ggplot2::rescale but the current version of ggplot2 no longer has rescale()
##use rescale() from the scales package
library(scales)
radian.rescale <- function(x, start=0, direction=1) {
   c.rotate <- function(x) (x + start) %% (2 * pi) * direction
   c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
 }
 
##number of signallers included in graph
n=16
lab.locs <- radian.rescale(x=1:n, direction=-1, start=0)


##plot graph with COM with green edges and MIP with blue edges, saved as "Rplot Directed Circle MIP and COM.pdf"
E(g2)[13:39]$color <- "blue"
E(g2)[1:12]$color <- "green"
plot(g2, layout=layout.circle, vertex.size=5, vertex.label.dist=1,
      vertex.label.degree=lab.locs,
       vertex.label.dist=0.5, vertex.label.cex=0.8, vertex.label.font=2,
       edge.arrow.size=0.5,vertex.color=1)
       

