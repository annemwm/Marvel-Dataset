library("igraph")
library("splitstackshape")
library("stringr")

setwd("C:/Users/Anne/Desktop/all_edges_nodes/hp_edges")
hp_edges <- data.frame(stringsAsFactors = FALSE)
tmp = list.files(pattern="*.csv")
hp_dfe = lapply(tmp, read.csv)
for (df in hp_dfe){
  hp_edges<-rbind(hp_edges, df)
}

setwd("C:/Users/Anne/Desktop/all_edges_nodes/hp")
hp_chars <- data.frame()
tmp = list.files(pattern="*.csv")
hp_df = lapply(tmp, read.csv)
for (df in hp_df){
  hp_chars<-rbind(hp_chars, df)
}


for (i in (1:length(hp_dfe))){
  df <- hp_dfe[[i]]
  df2 <- hp_df[[i]]
  df <- cbind(df, "movie"=as.character(df2$TITLE[1]))
  hp_edges<-rbind(hp_edges, df)
}

colpal <- colorRampPalette(brewer.pal(11,"BrBG"))(8)

hp_edges$color[hp_edges$movie =="HarryPotterandtheChamberofSecrets"] <- colpal[1]
hp_edges$color[hp_edges$movie =="HarryPotterandtheDeathlyHallows:Part1"] <- colpal[2]
hp_edges$color[hp_edges$movie =="HarryPotterandtheDeathlyHallows:Part2"] <- colpal[3]
hp_edges$color[hp_edges$movie =="HarryPotterandtheGobletofFire"] <- colpal[4]
hp_edges$color[hp_edges$movie =="HarryPotterandtheHalf-BloodPrince"] <- colpal[5]
hp_edges$color[hp_edges$movie =="HarryPotterandtheOrderofthePhoenix"] <- colpal[6]
hp_edges$color[hp_edges$movie =="HarryPotterandthePrisonerofAzkaban"] <- colpal[7]
hp_edges$color[hp_edges$movie =="HarryPotterandtheSorcerer'sStone"] <- colpal[8]

#marvel_nodes <- data.frame(mega_data$CHARACTER)

hp_g <- graph.data.frame(hp_edges, directed = FALSE, vertices = NULL)
g2 <- graph.adjacency(get.adjacency(g), mode=c("undirected"), weighted=TRUE)

all_hp <- data.frame()
for (i in (1: length(hp_df))){
  all_hp <- rbind(all_hp, hp_df[[i]])
}
dups <- all_hp[duplicated(all_hp$CHARACTER),]
all_hp<-all_hp[!duplicated(all_hp$CHARACTER),]

V(g)$label <- as.character(all_hp$CHARACTER)



length(V(g))
length(V(g2))

length(E(g))
length(E(g2))

sort(degree(g), decreasing = T)[1:58]

sort(degree(g2), decreasing = T)[1:36]

evc <- evcent(g, directed=FALSE)
sort(evc$vector, TRUE)[1:20]
sort(evcent(g2)$vector, TRUE)[1:36]


sort(closeness(g, vids=V(g), mode = c("all"), weights= NULL, normalized = FALSE), TRUE)[1:10]

#percentage of length 2 paths: how many closed loops there are 
transitivity(g, type=c("undirected"), vids=NULL, weights=NULL, isolates=c("NaN"))

# how interconnected graph is out of all possible connects
edge_density(g)

#assortativity
#this measures how often nodes from the same group are connected to each other

#first give attributes to your nodes and edges
char.list<-sort(as_ids(V(g)), decreasing = F)
nodes.all<-mega_data[order(mega_data$CHARACTER),]
V(g)$gender<-nodes.all$GENDER
V(g2)$gender<-nodes.all$GENDER

#calculate assortativity
assortativity.nominal(g, as.factor(V(g)$gender), directed=FALSE)

#what if we condition on the top 20 characters
deg<-names(sort(degree(g),TRUE))
deg.top20<-deg[1:20]
deg.top36 <- deg[1:36]
deg.remove<-deg[21:length(deg)]
g.reduced<-delete.vertices(g, v=deg.remove)
char.list<-sort(as_ids(V(g.reduced)), decreasing = F)
nodes.red<-nodes.all[nodes.all$CHARACTER %in% char.list,]
nodes.red<-nodes.red[order(nodes.red$CHARACTER),]
V(g.reduced)$gender<-nodes.red$GENDER
assortativity.nominal(g.reduced, as.factor(V(g.reduced)$gender), directed=FALSE)


#Community Detection
#Here are two ways of identifying communities in your network

#Louvain Clusters (maximizes modularity)
comm1<-cluster_louvain(g, weights = E(g)$weight)
#find modularity score
max(comm1$modularity)
#find who is in what community
comm1.df<-data.frame(comm1$names, comm1$membership)
c1 <- comm1.df[comm1.df$comm1.membership==1,] 
nrow(c1) # 49
c2 <- comm1.df[comm1.df$comm1.membership==2,] # 
nrow(c2) #65
c3 <- comm1.df[comm1.df$comm1.membership==3,] # 
nrow(c3) #72

V(g)$membership <- comm1.df$comm1.membership

#leading eigenvector method
comm2<-cluster_leading_eigen(g)
max(comm2$modularity)
comm2.df<-data.frame(comm2$names, comm2$membership)


robustness_vector_hp<-vector()
for (j in 1:1000){
  random.nodes<-sample(V(g), length(V(g)), replace = FALSE, prob= NULL)
  g.robust<-as.undirected(g)
  for (i in 1:length(V(g))) {
    g.robust<-delete.vertices(g.robust, v=as_ids(random.nodes[[i]]))
    if (is.connected(g.robust)==FALSE)
      break
  }
  robustness_vector_hp<-append(robustness_vector_hp, i/length(V(g)))
}
summary(robustness_vector_hp)

# vulnerability
deg<-names(sort(degree(g), TRUE))
g.vulnerable<-as.undirected(g)
for (i in 1:length(V(g))){
  g.vulnerable<-delete.vertices(g.vulnerable, v=deg[[i]])
  if (is.connected(g.vulnerable)==FALSE)
    break
}
vuln <- i
print(vuln)
vuln/length(V(g))

plot.igraph(g, layout=layout_with_fr, edge.width=E(g)$weight, vertex.size=degree(g)/10,
            vertex.color="white",vertex.label.color="black", vertex.frame.color="white")

plot.igraph(g, layout=layout_with_fr, edge.width=E(g)$weight, vertex.size=degree(g)/10,
            vertex.color=V(g)$membership, vertex.label = NA,vertex.frame.color=NA)

plot.igraph(g, layout=layout_with_fr, edge.width=E(g)$weight, vertex.size=degree(g)/10,
            vertex.color="white",vertex.label.color="black", #vertex.label.family="Arial",
            vertex.label = ifelse(degree(g) > 430, V(g)$label, NA),
            vertex.frame.color="white")
# skyblue3, firebrick2, bisque
V(g)$membership <- gsub("1", "bisque", V(g)$membership)
V(g)$membership <- gsub("2", "firebrick2", V(g)$membership)
V(g)$membership <- gsub("3", "skyblue3", V(g)$membership)

plot.igraph(hp_g, layout=layout_with_fr, edge.width=E(hp_g)$weight, vertex.size=degree(hp_g)/32.1,
            vertex.color=adjustcolor("bisque",alpha.f=1), vertex.label = NA,vertex.frame.color="white")

edge1 <- read.csv("edge1.csv")
edge1<- edge1[,2:3]
g3 <- graph.data.frame(edge1, directed = FALSE, vertices = NULL)
plot.igraph(g3, layout=lay, edge.width=E(g)$weight, vertex.size=degree(g)/50,
            vertex.color=adjustcolor("bisque",alpha.f=1),vertex.label.color="black", vertex.frame.color="white")

hp_g <- graph.data.frame(edge1, directed = FALSE, vertices = NULL)
plot.igraph(g3, layout=lay, edge.width=E(g)$weight, vertex.size=degree(g)/50,
            vertex.color="white",vertex.label.color="black", vertex.frame.color="white")


dates <- levels(hp_chars$RELEASE_DATE)
dates <- as.integer(str_sub(dates, -2,-1))
hist(dates, breaks=20)
