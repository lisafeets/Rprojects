
##circle graph for Move into position (mip) with vertex positions set by social group, colored by social group

mip=read.table("ordered verts MIP.txt")
attach(mip)

library(igraph)
g2=graph.data.frame(mip)

##set vetices outside of circle
library(scales)
radian.rescale <- function(x, start=0, direction=1) {
   c.rotate <- function(x) (x + start) %% (2 * pi) * direction
   c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
   }
   
##set n to 16 for the number of vertices
n=17
lab.locs <- radian.rescale(x=1:n, direction=-1, start=0)
          
##.txt file has first 16 rows dedicated to ordering of the signallers (igraph sets order of circle based on which vertex it reads first in edge list)          
V(g2)[1:3]$color="purple"
V(g2)[4:5]$color="blue"
V(g2)[6:12]$color="red"
V(g2)[13:15]$color="green"
V(g2)[16]$color="orange"
V(g2)[17]$color="black"

##only plot the actual data, after the dummy label data from above
E(g2)[18:44]$color="black"

##plot, saved as "Rplot MIP ordered colored verts.pdf"
library(ggplot2)
plot(g2, layout=layout.circle, vertex.size=5, vertex.label.dist=1,
         vertex.label.degree=lab.locs,
          vertex.label.dist=0.5, vertex.label.cex=0.8, vertex.label.font=2,
          edge.arrow.size=0.5, vertex.label.color=1)
title("Move into position")


##circle graph for COM
com=read.table("ordered verts COM.txt")
g2=graph.data.frame(com)
##.txt file has first 16 rows dedicated to ordering of the signallers (igraph sets order of circle based on which vertex it reads first in edge list)          
V(g2)[1:3]$color="purple"
V(g2)[4:5]$color="blue"
V(g2)[6:12]$color="red"
V(g2)[13:15]$color="green"
V(g2)[16]$color="orange"
V(g2)[17]$color="black"
##only plot the actual data, after the dummy label data from above
E(g2)[18:28]$color="black"
##plot, saved as "Rplot COM ordered colored verts.pdf"
plot(g2, layout=layout.circle, vertex.size=5, vertex.label.dist=1,
           vertex.label.degree=lab.locs,
            vertex.label.dist=0.5, vertex.label.cex=0.8, vertex.label.font=2,
            edge.arrow.size=0.5, vertex.label.color=1)
title("Climb on me")