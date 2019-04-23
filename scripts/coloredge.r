library("RColorBrewer")
colpal <- colorRampPalette(brewer.pal(11,"BrBG"))(87)
## script for making cute social networks

setwd("C:/Users/Anne/Desktop")
edge.all <- read.csv("comediehumaine_edge.all.csv")
filename <- edge.all$filename[1]
edgecol <- colpal[1]
j=2
for (i in 2:nrow(edge.all)){
  if (edge.all$filename[i] == filename){
    edgecol <- rbind(edgecol, edgecol[i-1])
  } else {
    edgecol <- rbind(edgecol, colpal[j])
    filename <- edge.all$filename[i]
    j<-j+1
  }
}

ch.g <- graph.data.frame(edge.all, directed = FALSE, vertices = NULL)

E(ch.g)$color <- edgecol

plot.igraph(ch.g, vertex.label.color = "white", vertex.frame.color=NA)u

plot.igraph(ch.g,layout=layout_nicely, vertex.color=adjustcolor("gray",alpha.f=.2),
            vertex.label.family="Helvetica", vertex.frame.color="black",
            vertex.label.color="black")

#plot.igraph(hp_g, layout=layout_with_fr, edge.width=E(hp_g)$weight, vertex.size=degree(hp_g)/20,
#            vertex.color=adjustcolor("bisque",alpha.f=1), vertex.label = NA,vertex.frame.color="white")


plot.igraph(ch.g, vertex.color=adjustcolor("bisque",alpha.f=1), vertex.size=degree(ch.g)/9.7,
            vertex.frame.color="white", vertex.label=NA)

