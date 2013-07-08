##phylogenetic trees for signaller x gesture/action/context

library(ape)


##signaller x action saved as "Rplot_Phylo_tree.pdf"
data.dist = dist(samatrix)
PhyloClust = nj(data.dist)
plot(PhyloClust, type = "u", frame= T, cex= .9)

