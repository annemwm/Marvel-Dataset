#### La comédie humaine Social Network Analysis ####
setwd("~/Desktop/txtLAB/comediehumaine")

#### Arrange csv ####

# loop through each row of masterlist, find the corresponding distinct csv, pull out those
# characters and assign them all of the values that are found in that row of the masterlist

# read in masterlist 
masterlist <- read.csv("balzac_comediehumaine.csv", header=T)

# list files (char files)
setwd("/Users/Vicky/Desktop/txtLAB/comediehumaine/")
files <- list.files('chars_clean')
# loop through files
char_masterlist <- NULL
for (i in 1:length(files)){
  char_list <- NULL
  # pull out corresponding row in masterlist
  metadata <- masterlist[(which(masterlist$filename == paste(strsplit(files[i], '.csv'), '.txt', sep=""))),]
  # go into file, add row to each char
  setwd("/Users/Vicky/Desktop/txtLAB/comediehumaine/chars_clean")
  chars <- read.csv(files[i], header=T)
  char_list <- cbind(chars[,1:3], metadata)
  char_masterlist <- rbind(char_masterlist, char_list)
}

setwd("/Users/Vicky/Desktop/txtLAB/comediehumaine/")
write.csv(char_masterlist, "char_masterlist_comhum.csv")

#### Social Network analysis ####

# iGraph, takes as input an edge list and node list (can also use Gephi)
library("igraph")
library("splitstackshape")

# ingest your edge and node lists
nodes.all<-char_masterlist
# create edge list
edge.all <- NULL
#for (i in 1:nrow(char_masterlist)){
firstchar <- 1
while (firstchar < nrow(char_masterlist)){
  work <- char_masterlist$filename[firstchar]
  #j <- i
  while (j+1 < nrow(char_masterlist)){
    char <- char_masterlist$entity[i]
    while (char_masterlist$filename[j+1] == work){
      edge.all <- rbind(edge.all, data.frame(char, char_masterlist$entity[j+1]))
      j <- j+1
    }
    i <- i+1
  }
  i <- i+1
}


firstchar <- 1


# change colnames to match

#subset only by characters (source and target)
edge1<-data.frame(source=edge.all$NAME.1, target=edge.all$NAME.2)

#make a "graph object"
g<-graph.data.frame(edge1, directed=FALSE, vertices=NULL)
#make a second that collapses all interactions into *unique* interactions w edge weights
#edge weight = # times 2 characters interact
g2 <- graph.adjacency(get.adjacency(g),mode=c("undirected"), weighted=TRUE)

#first, confirm how many nodes and edges you have
length(V(g)) #V(g) = nodes for your graph object g
length(E(g)) #E(g) = edges for your graph object g
length(V(g2)) #V(g) = nodes for your graph object g
length(E(g2)) #E(g) = edges for your graph object g

#check out your degree centrality
degree(g)
degree(g2)
#now sort it
sort(degree(g), decreasing = T)
sort(degree(g2), decreasing = T)
#keep only the top 10
sort(degree(g), decreasing = T)[1:10]
sort(degree(g2), decreasing = T)[1:10]
#now plot it
plot(sort(degree(g), decreasing = T), main="Pride and Prejudice", xlab="Characters", ylab="Connection Weights")
plot(sort(degree(g2), decreasing = T), main="Pride and Prejudice", xlab="Characters", ylab="Number of Connections")
#this is useful to get an overview of your distribution
summary(degree(g))
summary(degree(g2))

# Eigenvector centrality
# your centrality is based on the degree of your neighbors (how connected your neighbors are)
evcent(g, directed=FALSE) # if a directed graph, set to TRUE
sort(evcent(g)$vector, TRUE)
sort(evcent(g)$vector, TRUE)[1:10]
sort(evcent(g2)$vector, TRUE)[1:10]

# Closeness
sort(closeness(g, vids=V(g), mode = c("all"), weights = NULL, normalized = FALSE), TRUE)
sort(closeness(g, vids=V(g), mode = c("all"), weights = NULL, normalized = FALSE), TRUE)[1:10]

# Strength
# This incorporates edge weights into the centrality calculation
# The stronger the edge weight the "stronger" the connection. So it's degree plus weight.
#first you need to create edge weights which are the sums of all the times 2 characters interact
graph.strength(g2, vids=V(g2), mode = c("all"))
sort(graph.strength(g2, mode = c("all")), TRUE)[1:10]

# Betweenness Centrality
sort(betweenness(g), TRUE)
sort(betweenness(g), TRUE)[1:10]

# Diameter
diameter(g2, directed = FALSE, weights=E(g2)$weight)
# tells you which are the two furthest nodes from each other
farthest.nodes(g2, weights=E(g2)$weight)

#transitivity
#this tells you how many closed loops there are as a percentage of length 2 paths
transitivity(g2, type=c("undirected"), vids=NULL, weights=NULL, isolates=c("NaN"))

#graph density
#this examines how interconnected your graph is, i.e. how many people are connected out of all possible connects
#requires graph w edge weights (i.e. no duplicate edges)
edge_density(g2)

#Community Detection
#Here are two ways of identifying communities in your network

#Louvain Clusters (maximizes modularity)
comm1<-cluster_louvain(g2, weights = E(g2)$weight)
#find modularity score
max(comm1$modularity)
#find who is in what community
comm1.df<-data.frame(comm1$names, comm1$membership)

#leading eigenvector method
comm2<-cluster_leading_eigen(g)
max(comm2$modularity)
comm2.df<-data.frame(comm2$names, comm2$membership)

#assortativity
#this measures how often nodes from the same group are connected to each other

#first give attributes to your nodes and edges
char.list<-sort(as_ids(V(g)), decreasing = F)
nodes.all<-nodes.all[order(nodes.all$NAME),]
V(g)$gender<-nodes.all$GENDER
V(g2)$gender<-nodes.all$GENDER

#calculate assortativity
assortativity.nominal(g, as.factor(V(g)$gender), directed=FALSE)

#what if we condition on the top 20 characters
deg<-names(sort(degree(g2),TRUE))
deg.top20<-deg[1:20]
deg.remove<-deg[21:length(deg)]
g.reduced<-delete.vertices(g2, v=deg.remove)
char.list<-sort(as_ids(V(g.reduced)), decreasing = F)
nodes.red<-nodes.all[nodes.all$NAME %in% char.list,]
nodes.red<-nodes.red[order(nodes.red$NAME),]
V(g.reduced)$gender<-nodes.red$GENDER
assortativity.nominal(g.reduced, as.factor(V(g.reduced)$gender), directed=FALSE)

#Robustness
#this tells you how many random nodes need to be deleted before your graph breaks in 2
# we run it 1000 times and take the avg. (so it may take a few minutes)
robustness_vector<-vector()
for (j in 1:100) {
  random.nodes<-sample(V(g2), length(V(g2)), replace = FALSE, prob = NULL)
  g.robust<-as.undirected(g2)
  for (i in 1:length(V(g2))) {
    g.robust<-delete.vertices(g.robust, v=as_ids(random.nodes[[i]]))
    if (is.connected(g.robust)==FALSE)
      break
  }
  robustness_vector<-append(robustness_vector, i/length(V(g2)))
}
# it then prints the mean and standard deviation values
# the mean is the percentage of nodes that on average needed to be removed to split it in 2
# this is a measure of how densely connected it is
mean(robustness_vector) 
sd(robustness_vector)

# Vulnerability
# this is a similar process, but instead of removing random nodes
# we take out the strongest nodes in order of importance by degree
deg<-names(sort(degree(g2),TRUE))
g.vulnerable<-as.undirected(g2)
for (i in 1:length(V(g2))) {
  g.vulnerable<-delete.vertices(g.vulnerable, v=deg[[i]])
  if (is.connected(g.vulnerable)==FALSE)
    break
}
#it prints the number of nodes needed to be removed
print(i)  
#and the percentage of the network
print(vulnerability<-i/length(V(g2)))

#visualize your network
plot(g2)

#or prepare for Gephi

#version 1
#this gives you a weighted graph
gephi.df<-cbind(as_ids(E(g2)), E(g2)$weight)
gephi.df<-cSplit(gephi.df, 'V1', sep="|", type.convert=FALSE)
colnames(gephi.df)<-c("weight", "source", "target")
gephi.df$type<-c("undirected")
write.csv(gephi.df, file="Austen_PP_Edges_Gephi.csv")

#version 2
#this gives you an unweighted graph w edges labeled by positivity v. negativity
valence<-sapply(edge.all$POS.NEG, function(x) if (x > 1){x<-c("pos")} else {x<-c("neg")})
edge.all$valence<-valence
colnames(edge.all)[1:2]<-c("source", "target")
edge.all$type<-c("undirected")
write.csv(edge.all, file="Austen_PP_Edges_Gephi_Valence.csv")

### Exercise ###
setwd("~/Desktop/Year 3/LLCU 255")
filenames.edges<-list.files("Edges_ROM", pattern="*.csv", full.names=FALSE)
filenames.nodes<-list.files("Nodes_ROM", pattern="*.csv", full.names=FALSE)
inter.df<-NULL
genre<-c("ROM")
for (i in 1:length(filenames.edges)){
  #ingest your edge and node lists
  setwd("~/Desktop/Year 3/LLCU 255/Edges_ROM")
  edge.all<-read.csv(filenames.edges[i])
  setwd("~/Desktop/Year 3/LLCU 255/Nodes_ROM")
  nodes.all<-read.csv(filenames.nodes[i])
  # get the name of the main character
  main.char<-as.character(nodes.all[1,1])
  # attach gender to main character
  main.gender<-as.character(nodes.all[1,3])
  # get total number of interactions for marin character
  length1<-length(which(edge.all$NAME.1 == main.char))
  length2<-length(which(edge.all$NAME.2 == main.char))
  length.total<-length1+length2
  # turn into percentage
  length.per<-length.total/nrow(edge.all)
  # put in temporary dataframe
  temp.df<-data.frame(main.char, main.gender, length.total, length.per, genre)
  # store in master dataframe
  inter.df<-rbind(inter.df, temp.df) # attach temp.df by rows to the following thing (inter.df), cbind does columns, rows and columns must be identical
}
# subset data by a value in the column (male, gender)
men.df<-inter.df[inter.df$main.gender == "M",]
women.df<-inter.df[inter.df$main.gender == "F",]
# check for normality
hist(women.df$length.per)
shapiro.test(women.df$length.per)
shapiro.test(men.df$length.per)
# what percentage of main characters are female
nrow(women.df)/nrow(inter.df)
#
t.test(men.df$length.per, women.df$length.per)


### Assignment 3 ###

### for a directory of edge lists ###
setwd("~/Documents/6. Teaching/255 Intro to Text Mining/255 - Lesson Plans/255 - Week 8 (SNA)")
filenames.edges<-list.files("Edges", pattern="*.csv", full.names=FALSE)
filenames.nodes<-list.files("Nodes", pattern="*.csv", full.names=FALSE)
for (i in 1:length(filenames)){
  
  #ingest your edge and node lists
  setwd("~/Documents/6. Teaching/255 Intro to Text Mining/255 - Lesson Plans/255 - Week 8 (SNA)/Edges")
  edge.all<-read.csv(filenames.edges[i])
  setwd("~/Documents/6. Teaching/255 Intro to Text Mining/255 - Lesson Plans/255 - Week 8 (SNA)/Nodes")
  nodes.all<-read.csv(filenames.nodes[i])
  
  #subset only by characters (source and target)
  edge1<-data.frame(source=edge.all$NAME.1, target=edge.all$NAME.2)
  
  #make a "graph object"
  g<-graph.data.frame(edge1, directed=FALSE, vertices=NULL)
  #make a second that collapses all interactions into *unique* interactions w edge weights
  #edge weight = # times 2 characters interact
  g2 <- graph.adjacency(get.adjacency(g),mode=c("undirected"), weighted=TRUE)
  
  #first, confirm how many nodes and edges you have
  no.nodes<-length(V(g2)) #V(g2) = nodes for your graph object g
  no.edges<-length(E(g2)) #E(g2) = edges for your graph object g
  no.interactions<-length(E(g)) 
  
  #avg degree centrality
  avg.degree<-mean(degree(g2))
  
  #diameter
  diam.score<-diameter(g2, directed = FALSE, weights=E(g2)$weight)
  
  #transitivity
  #this tells you how many closed loops there are as a percentage of length 2 paths
  trans.score<-transitivity(g2, type=c("undirected"), vids=NULL, weights=NULL, isolates=c("NaN"))
  
  #graph density
  #this examines how interconnected your graph is, i.e. how many people are connected out of all possible connects
  #requires graph w edge weights (i.e. no duplicate edges)
  density.score<-edge_density(g2)
  
  #Community Detection
  #Here are two ways of identifying communities in your network
  
  #Louvain Clusters (maximizes modularity)
  comm1<-cluster_louvain(g2, weights = E(g2)$weight)
  #find modularity score
  comm1.mod<-max(comm1$modularity)
  
  #leading eigenvector method
  comm2<-cluster_leading_eigen(g)
  comm2.mod<-max(comm2$modularity)
  
  #assortativity
  #this measures how often nodes from the same group are connected to each other
  
  #first give attributes to your nodes and edges
  char.list<-sort(as_ids(V(g)), decreasing = F)
  nodes.all<-nodes.all[order(nodes.all$NAME),]
  V(g)$gender<-nodes.all$GENDER
  V(g2)$gender<-nodes.all$GENDER
  
  #calculate assortativity
  gender.assort.all<-assortativity.nominal(g2, as.factor(V(g2)$gender), directed=FALSE)
  
  #what if we condition on the top 20 characters
  deg<-names(sort(degree(g2),TRUE))
  deg.top20<-deg[1:20]
  deg.remove<-deg[21:length(deg)]
  g.reduced<-delete.vertices(g2, v=deg.remove)
  char.list<-sort(as_ids(V(g.reduced)), decreasing = F)
  nodes.red<-nodes.all[nodes.all$NAME %in% char.list,]
  nodes.red<-nodes.red[order(nodes.red$NAME),]
  V(g.reduced)$gender<-nodes.red$GENDER
  gender.assort.top20<-assortativity.nominal(g.reduced, as.factor(V(g.reduced)$gender), directed=FALSE)
  
  #Robustness
  #this tells you how many random nodes need to be deleted before your graph breaks in 2
  # we run it 1000 times and take the avg. (so it may take a few minutes)
  robustness_vector<-vector()
  for (j in 1:100) {
    random.nodes<-sample(V(g2), length(V(g2)), replace = FALSE, prob = NULL)
    g.robust<-as.undirected(g2)
    for (i in 1:length(V(g2))) {
      g.robust<-delete.vertices(g.robust, v=as_ids(random.nodes[[i]]))
      if (is.connected(g.robust)==FALSE)
        break
    }
    robustness_vector<-append(robustness_vector, i/length(V(g2)))
  }
  # it then prints the mean and standard deviation values
  # the mean is the percentage of nodes that on average needed to be removed to split it in 2
  # this is a measure of how densely connected it is
  robust.mean<-mean(robustness_vector) 
  robust.sd<-sd(robustness_vector)
  
  # Vulnerability
  # this is a similar process, but instead of removing random nodes
  # we take out the strongest nodes in order of importance by degree
  deg<-names(sort(degree(g2),TRUE))
  g.vulnerable<-as.undirected(g2)
  for (i in 1:length(V(g2))) {
    g.vulnerable<-delete.vertices(g.vulnerable, v=deg[[i]])
    if (is.connected(g.vulnerable)==FALSE)
      break
  }
  #and the percentage of the network
  vuln.score<-vulnerability<-i/length(V(g2))
  work<-as.character(filenames.nodes[i])
  male.female.ratio.all<-which(nodes.all$GENDER == "M")/which(nodes.all$GENDER =="F")
  male.female.ratio.top20<-which(nodes.red$GENDER == "M")/which(nodes.red$GENDER =="F")
  temp.df<-data.frame(work, no.nodes, no.edges, no.interactions, male.female.ratio.all, male.female.ratio.top20, avg.degree, diam.score, trans.score, density.score, comm1.mod, comm2.mod, gender.assort.all, gender.assort.top20, robust.mean, robust.sd, vuln.score)
  