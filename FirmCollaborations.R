################################################################################

library(Matrix)
library(statnet)
library(igraph)
library(network)
library(gridExtra)
library(grid)
library(ggplot2)
library(foreach)
library(blockmodeling)
library(doParallel)
library(doRNG)
library(ergm)
library(statnet)
library(RColorBrewer)
library(reshape2)

load("data/Firms_collaboration.RData")

#ATECO codes are the italian equivalent of EU NACE codes
nace <- list('A'='AGRICULTURE, FORESTRY AND FISHING',
             'B'='MINING AND QUARRYING',
             'C'='MANUFACTURING', 
             'D'='ELECTRICITY, GAS, STEAM AND AIR CONDITIONING SUPPLY', 
             'E'='WATER SUPPLY; SEWERAGE, WASTE MANAGEMENT', 
             'F'='CONSTRUCTION',
             'G'='WHOLESALE AND RETAIL TRADE', 'H'='TRANSPORTATION AND STORAGE',
             'I'='ACCOMMODATION AND FOOD SERVICE ACTIVITIES', 
             'J'='INFORMATION AND COMMUNICATION', 
             'K'='FINANCIAL AND INSURANCE ACTIVITIES', 
             'L'='REAL ESTATE ACTIVITIES', 
             'M'='PROFESSIONAL, SCIENTIFIC AND TECHNICAL ACTIVITIES', 
             'N'='ADMINISTRATIVE AND SUPPORT SERVICE ACTIVITIES', 
             'O'='PUBLIC ADMINISTRATION AND DEFENCE', 
             'P'='EDUCATION', 
             'Q'='HUMAN HEALTH AND SOCIAL WORK ACTIVITIES', 
             'R'='ARTS, ENTERTAINMENT AND RECREATION', 
             'S'='OTHER SERVICE ACTIVITIES', 
             'T'='ACTIVITIES OF HOUSEHOLDS AS EMPLOYERS', 
             'U'='EXTRATERRITORIAL ORGANISATIONS AND BODIES')

###convert dummy df into categorical vectors 
Ate<-apply(Ateco, 1, 
           function(x) names(x)[as.logical(as.numeric(as.character(x)))])
Prov<-apply(Province, 1, 
            function(x) names(x)[as.logical(as.numeric(as.character(x)))])
Hub<-apply(HUB, 1, 
           function(x) names(x)[as.logical(as.numeric(as.character(x)))])
Sll<-apply(SLL, 1, 
           function(x) names(x)[as.logical(as.numeric(as.character(x)))])

# there are "." instead of Ateco codes in some elements
points <- which(Ate == ".")

# more than one Ateco code is useless for our purpose
not_one <- which(sapply(Ate, function(x) length(x)>1))

# all elements to remove
wrongs <- c(not_one, points)

#clean categorical vectors
Ate <- Ate[-wrongs]
Prov <- Prov[-wrongs]
Hub <- Hub[-wrongs]
Sll <- Sll[-wrongs]

### descriptive analysis on "Provincia" ###
PROV_names <- c("Gorizia","Pordenone","Trieste","Udine")

p_go <- length(grep("Gorizia",Prov))
p_pn <- length(grep("Pordenone",Prov))
p_ts <- length(grep("Trieste",Prov))
p_ud <- length(grep("Udine",Prov))
PROV_vals <- c(p_go, p_pn, p_ts, p_ud)

PROV_df <- data.frame(PROV_names, PROV_vals)

plot_prov<-ggplot(data=PROV_df, aes(fill=PROV_names, x=PROV_vals, 
                                    y=PROV_names)) + 
  scale_fill_brewer(palette = "Dark2") + guides(fill=FALSE) +
  geom_bar(stat="identity", width=0.5) + 
  labs(x = "Firms number", 
       y=NULL,
       title="Firms distribution among province in Friuli-Venezia Giulia ")+ 
  theme(plot.title = element_text(size=18,face="bold"), 
        axis.text = element_text(size=14), 
        axis.title = element_text(size=14,face="bold")) +
  scale_y_discrete(limits = rev(levels(as.factor(PROV_df$PROV_names))))
plot_prov 

### descriptive analysis on "HUB" ###
HUB_names=c("Extra FVG", "HUB Giuliano", "HUB Isontino", 
            "HUB Medio e alto Friuli", "HUB Pordenonese", 
            "HUB Udine e bassa friulana")

hub_extra_fvg <- length(grep("Extra FVG",Hub))
hub_giuliano <- length(grep("HUB Giuliano",Hub))
hub_isontino <- length(grep("HUB Isontino",Hub))
hub_medio_alto <- length(grep("HUB Medio e alto Friuli",Hub))
hub_pn <- length(grep("HUB Pordenonese",Hub))
hub_ud_bassa <- length(grep("HUB Udine e bassa friulana",Hub))
HUB_vals <- c(hub_extra_fvg,hub_giuliano,hub_isontino,hub_medio_alto,
              hub_pn,hub_ud_bassa)

HUB_df=data.frame(HUB_names,HUB_vals)

plot_hub<-ggplot(data=HUB_df, aes(fill=HUB_names, x=HUB_vals, y=HUB_names)) +
  scale_fill_brewer(palette = "Dark2") + guides(fill=FALSE) +
  geom_bar(stat="identity", width=0.5) + 
  labs(x = "Firms number", 
       y=NULL,
       title="Firms distribution among hubs in Friuli-Venezia Giulia ")+ 
  theme(plot.title = element_text(size=18,face="bold"), 
        axis.text = element_text(size=14), 
        axis.title = element_text(size=14,face="bold"))+
  scale_y_discrete(limits = rev(levels(as.factor(HUB_df$HUB_names)))) +
  scale_color_brewer(palette = "Dark2")

plot_hub


### descriptive analysis on "Ateco" ###
ATE_names = LETTERS[1:21]

ATE_vals=c()
for (i in (1:21) ) {
  ATE_vals[i]<-(length(which(Ate == LETTERS[i])))
}

ATE_names <- unlist(nace)
ATE_df<-data.frame(ATE_names, ATE_vals)

plot_ateco<-ggplot(data=ATE_df, aes(fill=ATE_names, x=ATE_vals,y=ATE_names)) +
  guides(fill=FALSE) +
  geom_bar(stat="identity")+
  labs(x = "Firms number", 
       y=NULL,
       title="Firms distribution among ATECO codes in Friuli-Venezia Giulia ")+ 
  theme(plot.title = element_text(size=20,face="bold"), 
        axis.text = element_text(size=8), 
        axis.title = element_text(size=14,face="bold")) +
  scale_y_discrete(limits = rev(levels(as.factor(ATE_df$ATE_names)))) 

plot_ateco


### descriptive analysis on "Sll" ###
SLL_names = as.character(601:611)

SLL_vals = c()
for (i in (1:11) ) {
  SLL_vals[i]<-(length(grep(as.character(600+i),Sll)))
}

SLL_df<-data.frame(SLL_names, SLL_vals)

plot_sll<-ggplot(data=SLL_df, aes(fill=SLL_names, x=SLL_vals, y=SLL_names)) +
  scale_fill_brewer(palette = "Paired") + guides(fill=FALSE) +
  geom_bar(stat="identity", width=0.5)+
  labs(x = "Firms number", 
       y=NULL,
       title="Firms distribution among SLL codes in Friuli-Venezia Giulia ")+
  theme(plot.title = element_text(size=20,face="bold"), 
        axis.text = element_text(size=14), 
        axis.title = element_text(size=14,face="bold")) +
  scale_y_discrete(limits = rev(levels(as.factor(SLL_df$SLL_names))))

plot_sll

################################################################################

#init graph
g<-graph_from_adjacency_matrix(adjacencyWeightedSM,
                                weighted = T)
gorder(g)
gsize(g)

###convert dummy df into categorical vectors 
Ate<-apply(Ateco, 1, 
           function(x) names(x)[as.logical(as.numeric(as.character(x)))])
Prov<-apply(Province, 1, 
            function(x) names(x)[as.logical(as.numeric(as.character(x)))])
Hub<-apply(HUB, 1, 
           function(x) names(x)[as.logical(as.numeric(as.character(x)))])
Sll<-apply(SLL, 1, 
           function(x) names(x)[as.logical(as.numeric(as.character(x)))])

#set inital node attributes
V(g)$Provincia <- Prov
V(g)$Ateco<- Ate
V(g)$Hub<- Hub
V(g)$Sll<- Sll
adj_g <- as_adjacency_matrix(g, attr="weight")

#add time attributes to nodes
dg<-(diag(adj_g))
tot<-colSums(adj_g)
ext<-tot-dg
V(g)$int<-dg
V(g)$ext<-ext
V(g)$tot<-tot

# removing loops
g <- simplify(g, remove.multiple = FALSE)
gorder(g)
gsize(g)

#remove isolates that were already present in the network
g <- delete_vertices(g, which(degree(g)==0))
gorder(g)
gsize(g)

# there are "." instead of Ateco codes in some elements
points <- which(V(g)$Ateco == ".")

# more than one Ateco code is useless for our purpose
not_one <- which(sapply(V(g)$Ateco, function(x) length(x)>1))

# remove wrong ATECO 
wrongs <- unique(c(not_one, points))
g<-delete_vertices(g, wrongs)
gorder(g)
gsize(g)

#remove nodes with total hours less than 1y of work for a full time employee
g <- delete_vertices(g, which(V(g)$tot<1760))
gorder(g)
gsize(g)

#remove isolates generated from previous operations
g <- delete_vertices(g, which(degree(g)==0))
gorder(g)
gsize(g)

# should be 2
min(degree(g))


### descriptive analysis on "hours" ############################################
ATE_codes <- sort(rep(LETTERS[1:21],2))
NACE_names<- rep(as.vector(unlist(nace)), each=2)

HOURS_vals=c()
for (i in (1:42) ) {
  if (i %% 2 == 0){
    
    HOURS_vals[i]<-sum(V(g)[which(V(g)$Ateco == ATE_codes[i])]$ext) 
  }
  else{
    HOURS_vals[i]<- sum(V(g)[which(V(g)$Ateco == ATE_codes[i])]$int)
  }
}

HOURS_names <- rep(c("Internal Hours", "Collaboration Hours"),21)
HOURS_df<-data.frame(NACE_names, HOURS_names, HOURS_vals)


plot_hours<-ggplot(data=HOURS_df, aes(fill=HOURS_names, x=HOURS_vals,
                                      y=NACE_names)) +
  geom_bar(position="stack", stat="identity")+
  labs(x = "Firms hours", 
       y=NULL,
       fill=NULL,
       title="Firms distribution among hours in Friuli-Venezia Giulia ")+ 
  theme(plot.title = element_text(size=20,face="bold"), 
        axis.text = element_text(size=8), 
        axis.title = element_text(size=14,face="bold")) +
  scale_y_discrete(limits = rev(levels(as.factor(HOURS_df$NACE_names))))

plot_hours

########################## graph exploration ###################################

gorder(g) #number vertex

gsize(g) #number edges

#Simple graphs are graphs which do not contain loop and multiple edges.
is.simple(g) 

#Calculate the maximal (weakly or strongly) connected components of a graph
no.clusters(g) 

#Returns TRUE iff the specified graphs are connected.
is.connected(g) 

#The relationship between each pair of vertices is measured
dyad_census(g) 

#density of a graph is the ratio of the number of edges and the number of 
#possible edges.
graph.density(g)

#Transitivity measures the probability that the adjacent vertices of a vertex 
#are connected. 
transitivity(g) 

#Calculate the maximal (weakly or strongly) connected components of a graph
table(clusters(g)$csize) 

#The degree of a vertex is its most basic structural property, the number of its
#adjacent edges. 
plot(degree.distribution(g, mode="all"), log="xy")

# in this network out and in degree are identical
identical(degree.distribution(g, mode="in"),degree.distribution(g, mode="out"))

## Largest in-degree and out-degree are equal, so total degree is 2x
max(degree(g, mode="in")) 
max(degree(g, mode="all")) 

# degree distribution of graph
deg<-degree(g)
deg.dist <- degree_distribution(g, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=0.8, col="orange",
      xlab="Degree", ylab="Cumulative Frequency", log="y")


#centrality measures, closeness not computed as the graph is disconnected

#degree
summary(deg)

#betwenneess
bet <- betweenness(g, normalized = T) 
summary(bet)

# prov indices
prov_u<-grep("Udine", V(g)$Provincia)
prov_t<-grep("Trieste",V(g)$Provincia)
prov_p<-grep("Pordenone",V(g)$Provincia)
prov_g<-grep("Gorizia",V(g)$Provincia)

# prov subgraph
sub_u<-induced_subgraph(g,prov_u)
sub_t<-induced_subgraph(g,prov_t)
sub_g<-induced_subgraph(g,prov_g)
sub_p<-induced_subgraph(g,prov_p)

# hub indices
prov_g_hub<-grep("HUB Isontino", V(g)$Hub)
prov_u1_hub<-grep("HUB Medio e alto Friuli", V(g)$Hub)
prov_u2_hub<-grep("HUB Udine e bassa friulana", V(g)$Hub)
prov_t_hub<-grep("HUB Giuliano", V(g)$Hub)
prov_p_hub<-grep("HUB Pordenonese", V(g)$Hub)

# hub subgraph
sub_u1_h <- induced_subgraph(g, prov_u1_hub)
sub_u2_h <- induced_subgraph(g, prov_u2_hub)

############################ COLLABORATIONS MATRIX #############################

# create matrix of collaborations aggregated by ATECO for all ATECO groups
AtecoMatrix <- function(gr) {
  ID_ateco <- LETTERS[1:21]
  ate_ids <- c() 
  for (i in (1:21)) {
    a <- (ID_ateco[i])
    ids <- which(V(gr)$Ateco == a)
    ate_ids[[i]] <- ids
  }
  names(ate_ids) <- ID_ateco
  links_ids_ate = c()
  for (i in (1:21)) {
    links_ids_ate[[i]] <-(table(c(ID_ateco,as.vector(unlist(V(gr)[
      as.numeric(unlist(adjacent_vertices(gr, as.numeric(unlist(ate_ids[i])),
                                          mode="total")))]$Ateco)))))
  }
  mat_ate <- log(matrix(as.vector(unlist(lapply(links_ids_ate, 
                                                function(x) as.numeric(x)-1))),
                        byrow=T, ncol=21, nrow=21))
  mat_ate[mat_ate==-Inf] <- 0
  mat_ate <- mat_ate / sum(mat_ate)
  colnames(mat_ate) <- ID_ateco
  rownames(mat_ate) <- ID_ateco
  return(mat_ate)
} 

# create matrix of collaborations aggregated by ATECO for two ATECO groups
AtecoMatrixBI <- function(gr,codes) {
  ID_ateco <- codes
  ate_ids <- c() 
  for (i in (1:2)) {
    a <- (ID_ateco[i])
    ids <- which(V(gr)$Ateco == a)
    ate_ids[[i]] <- ids
  }
  names(ate_ids) <- ID_ateco
  links_ids_ate = c()
  for (i in (1:2)) {
    links_ids_ate[[i]] <-(table(c(ID_ateco,as.vector(unlist(V(gr)[
      as.numeric(unlist(adjacent_vertices(gr, as.numeric(unlist(ate_ids[i])), 
                                          mode="total")))]$Ateco)))))
  }
  mat_ate <- matrix(as.vector(unlist(lapply(links_ids_ate, 
                                            function(x) as.numeric(x)-1))), 
                    byrow=T, ncol=2, nrow=2)
  mat_ate <- mat_ate/sum(mat_ate)
  colnames(mat_ate) <- ID_ateco
  rownames(mat_ate) <- ID_ateco
  return(mat_ate)
}

# plot heatmap of collaboration matrix
PlotMatrix <- function(M, city){
  longData<-melt(M)
  longData<-longData[longData$value!=0,]
  ggplot(longData, aes(x = Var2, y = Var1)) + 
    geom_raster(aes(fill=value)) + 
    scale_fill_gradient(low="grey90", high="red") +
    labs(x="ATECO", y="ATECO", title=paste("Collaborations in ",
                                           city," by ATECO codes ")) +
    theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                       axis.text.y=element_text(size=9),
                       plot.title=element_text(size=11))
  
}

# collaborations matrix for every province
mat_go <- AtecoMatrix(sub_g)
mat_pn <- AtecoMatrix(sub_p)
mat_ts <- AtecoMatrix(sub_t)
mat_ud <- AtecoMatrix(sub_u)

# collaborations matrix plots
PlotMatrix(mat_go, "Gorizia")
PlotMatrix(mat_pn, "Pordenone")
PlotMatrix(mat_ts, "Trieste")
PlotMatrix(mat_ud, "Udine")

########################### CORENESS HELP FUNCTIONS ############################

# Create the coreness layout
CorenessLayout <- function(g) {
  coreness <- graph.coreness(g);
  xy <- array(NA, dim=c(length(coreness), 2));
  
  shells <- sort(unique(coreness));
  for(shell in shells) {
    v <- 1 - ((shell-1) / max(shells));
    nodes_in_shell <- sum(coreness==shell);
    angles <- seq(0,360,(360/nodes_in_shell));
    angles <- angles[-length(angles)]; # remove last element
    xy[coreness==shell, 1] <- sin(angles) * v;
    xy[coreness==shell, 2] <- cos(angles) * v;
  }
  return(xy);
}

# plot coreness
PlotCoreness <- function(g){
  gcore <-CorenessLayout(g)
  V(g)$core <- coreness(g, "all")
  col <- as.color(V(g)$core, opacity=1)
  plot(g, layout=gcore, 
       vertex.label=V(g)$Ateco, 
       vertex.label.cex=0.1, vertex.color=col, edge.width=1, vertex.size=8, 
       edge.arrow.size=0.1)
}

######################################## UDINE CG ##############################

# init graph of vertex with ATECO codes C and G
ids_CG <- which(V(sub_u)$Ateco == "C" | V(sub_u)$Ateco == "G")
sub_u_CG <- induced_subgraph(sub_u, ids_CG)
length(which(V(sub_u_CG)$Ateco == "C"))
length(which(V(sub_u_CG)$Ateco == "G"))
gorder(sub_u_CG)
gsize(sub_u_CG)

#isolated nodes (connected with other ATECO codes in parent graph) are deleted
table(unlist(V(sub_u_CG)[which(degree(sub_u_CG)==min(degree(sub_u_CG)))]$Ateco))
sub_u_CG <- delete.vertices(sub_u_CG, which(degree(sub_u_CG)== 0))

# subgraph dimensions
length(which(V(sub_u_CG)$Ateco == "C"))
length(which(V(sub_u_CG)$Ateco == "G"))
gorder(sub_u_CG)
gsize(sub_u_CG)

# plot coreness
PlotCoreness(sub_u_CG)

# min degree distribution among ATECO codes
table(unlist(V(sub_u_CG)[which(degree(sub_u_CG)==min(degree(sub_u_CG)))]$Ateco))

#removing less connected nodes and nodes that are now isolated
sub_u_CG_core <- delete.vertices(sub_u_CG, which(degree(sub_u_CG)<5))
length(which(V(sub_u_CG_core)$Ateco == "C"))
length(which(V(sub_u_CG_core)$Ateco == "G"))
gorder(sub_u_CG_core)
gsize(sub_u_CG_core)
sub_u_CG_core <- delete.vertices(sub_u_CG_core, which(degree(sub_u_CG_core)==0))
length(which(V(sub_u_CG_core)$Ateco == "C"))
length(which(V(sub_u_CG_core)$Ateco == "G"))
gorder(sub_u_CG_core)
gsize(sub_u_CG_core)

# plot coreness
PlotCoreness(sub_u_CG_core)

#removing once again dyads to show the core of the subgraph
sub_u_CG_core <- delete.vertices(sub_u_CG_core, which(degree(sub_u_CG_core)==2))
length(which(V(sub_u_CG_core)$Ateco == "C"))
length(which(V(sub_u_CG_core)$Ateco == "G"))
gorder(sub_u_CG_core)
gsize(sub_u_CG_core)
sub_u_CG_core <- delete.vertices(sub_u_CG_core, which(degree(sub_u_CG_core)==0))
length(which(V(sub_u_CG_core)$Ateco == "C"))
length(which(V(sub_u_CG_core)$Ateco == "G"))
gorder(sub_u_CG_core)
gsize(sub_u_CG_core)
is.connected(sub_u_CG_core) 
components(sub_u_CG_core)

# plot coreness
PlotCoreness(sub_u_CG_core)

# degree distribution of graph
deg <- degree(sub_u_CG_core, mode="all")
deg.dist <- degree_distribution(sub_u_CG_core, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.5, col="orange",
      xlab="Degree", ylab="Cumulative Frequency")

#centrality measures, closeness not computed as the graph is disconnected
summary(degree(sub_u_CG_core))
summary(closeness(sub_u_CG_core, normalized = T))
summary(betweenness(sub_u_CG_core, normalized=T)) 

# edge density
edge_density(sub_u_CG_core)

# comunity detection of core subgraph using walktrap algorithm
cl<-cluster_walktrap(sub_u_CG_core)
colors <- rainbow(max(membership(cl)))
modularity(cl)
length(sizes(cl))

# plot with auto layout from layout_nicely()
plot(sub_u_CG_core, edge.arrow.size= 0.5, 
     layout=layout_nicely(sub_u_CG_core), vertex.label=NA, 
     vertex.color=membership(cl), vertex.size=5, edge.width=1)

#plot with layout from layout_with_fr() and ATECO labels
plot(cl, sub_u_CG_core, layout=layout_with_fr(sub_u_CG_core),
     vertex.label=V(sub_u_CG_core)$Ateco, vertex.label.cex=0.8, 
     vertex.size=8, edge.width = 1)

# comunity detection of core subgraph using louvain algorithm, transformed in 
# undirected graph
sub_u_CG_core_nd <- as.undirected(sub_u_CG_core)
cl_nd<-cluster_louvain(sub_u_CG_core_nd)
colors <- rainbow(max(membership(cl_nd)))
modularity(cl_nd)
length(sizes(cl_nd))

# plot with auto layout from layout_nicely()
plot(sub_u_CG_core_nd, edge.arrow.size= 0.5, 
     layout=layout_nicely(sub_u_CG_core_nd), vertex.label=NA, 
     vertex.color=membership(cl_nd), vertex.size=5, edge.width=1)

#plot with layout from layout_with_fr() and ATECO labels
plot(cl_nd, sub_u_CG_core_nd, layout=layout_with_fr(sub_u_CG_core_nd),
     vertex.label=V(sub_u_CG_core_nd)$Ateco, vertex.label.cex=0.8, 
     vertex.size=8, edge.width = 1)

# ATECO codes of max degree nodes and their value
unlist(V(sub_u_CG_core_nd)[
  which(degree(sub_u_CG_core_nd)==max(degree(sub_u_CG_core_nd)))]$Ateco)
max(degree(sub_u_CG_core_nd))

#distribution of min degree nodes between ATECO codes and their value
table(unlist(V(sub_u_CG_core_nd)[
  which(degree(sub_u_CG_core_nd)==min(degree(sub_u_CG_core_nd)))]$Ateco))
min(degree(sub_u_CG_core_nd))

# assortativity for Ateco codes
assortativity(sub_u_CG_core, types1=(ifelse(V(sub_u_CG_core)$Ateco=="C",1,0)))

#distribution of firm collaboration between C and G ATECO codes in core subgraph
mat_CG <-AtecoMatrixBI(sub_u_CG_core_nd, c("C","G"))
print(round(mat_CG,2))

#################### UDINE CG ERG MODEL ########################################

# cutoff for binary weight conversion: median
n <- summary(E(sub_u_CG_core)$weight)[3]

# binary conversion and export of adj matrix 
E(sub_u_CG_core)$weight <- ifelse(E(sub_u_CG_core)$weight>n,1,0)
adj_c <- as_adjacency_matrix(sub_u_CG_core, attr = "weight")

#network graph creation from adj matrix, removing isolated nodes as before
nu <- network(adj_c, loops=FALSE)
nu<-network::delete.vertices(nu,isolates(nu))
summary(nu)

# mutuality is 1, so we can use only dyad-indipendent parameters
grecip(nu)

# Plot of subgraph with colors from ATECO codes
gplot(nu, vertex.col=ifelse(unlist(V(sub_u_CG_core)$Ateco)== "C","red","blue"),
      vertex.cex=as.numeric(sqrt(degree(sub_u_CG_core) + 1)))

# load network attributes
network::set.vertex.attribute(nu, "Ateco", unlist(V(sub_u_CG_core)$Ateco))
network::set.vertex.attribute(nu, "int", unlist(V(sub_u_CG_core)$int))
network::set.vertex.attribute(nu, "Sll", unlist(V(sub_u_CG_core)$Sll))
network::set.vertex.attribute(nu, "deg", as.vector(degree(sub_u_CG_core)))
network::set.vertex.attribute(nu, "mem", as.vector(cluster_louvain(
  sub_u_CG_core_nd)$membership))

#Simple simulation with edges as only param
nu.01 <- ergm(nu~edges)
summary(nu.01)
nu.sim.01 <-simulate(nu.01)

#plot original graph and simulated
par(mfrow=c(1,2))
gplot(nu)
gplot(nu.sim.01)

# evaluating results
gof_nu <- gof(nu.01)
plot(gof_nu)

#simulation with edges and homophily on ATECO codes
nu.02 <- ergm(nu~edges+nodematch("Ateco", diff=T))
summary(nu.02)
nu.sim.02 <-simulate(nu.02)

#plot original graph and simulated
par(mfrow=c(1,2))
gplot(nu)
gplot(nu.sim.02)

# evaluating results
gof_nu <- gof(nu.02)
plot(gof_nu)

#simulation with edges, homophily on ATECO codes and louvain membership
nu.03 <- ergm(nu~edges+nodematch("Ateco", diff=T)+nodecov("mem"))
summary(nu.03)
nu.sim.03 <-simulate(nu.03)

#plot original graph and simulated
par(mfrow=c(1,2))
gplot(nu)
gplot(nu.sim.03)

# evaluating results
gof_nu <- gof(nu.03)
plot(gof_nu)

#simulation with edges, homophily on ATECO codes, louvain membership and mutual
# we expect perfect fit 
nu.04 <- ergm(nu~edges+nodecov("mem")+mutual)
summary(nu.04)
nu.sim.04 <-simulate(nu.04)

#plot original graph and simulated
par(mfrow=c(1,2))
gplot(nu)
gplot(nu.sim.04)

# the graph is fitted perfectly ( as above, mutuality is 1)
mcmc.diagnostics(nu.04)
gof_nu <- gof(nu.04)
plot(gof_nu)

# reset plot
par(mfrow=c(1,1))

#################### TRIESTE FC ################################################

# init graph of vertex with ATECO codes C and F
ids_CF <- which(V(sub_t)$Ateco == "C" | V(sub_t)$Ateco == "F")
sub_t_CF <- induced_subgraph(sub_t, ids_CF)
length(which(V(sub_t_CF)$Ateco == "C"))
length(which(V(sub_t_CF)$Ateco == "F"))
gorder(sub_t_CF)
gsize(sub_t_CF)

#isolated nodes (connected with other ATECO codes in parent graph) are deleted
table(unlist(V(sub_t_CF)[which(degree(sub_t_CF)==min(degree(sub_t_CF)))]$Ateco))
sub_t_CF <- delete.vertices(sub_t_CF, which(degree(sub_t_CF)== 0))

# subgraph dimensions
length(which(V(sub_t_CF)$Ateco == "C"))
length(which(V(sub_t_CF)$Ateco == "F"))
gorder(sub_t_CF)
gsize(sub_t_CF)

# plot coreness
PlotCoreness(sub_t_CF)

#min degree distribution among ATECO code
table(unlist(V(sub_t_CF)[which(degree(sub_t_CF)==min(degree(sub_t_CF)))]$Ateco))

#removing less connected nodes and nodes that are now isolated 
sub_t_CF_core <- delete.vertices(sub_t_CF, which(degree(sub_t_CF)<5))
length(which(V(sub_t_CF_core)$Ateco == "C"))
length(which(V(sub_t_CF_core)$Ateco == "F"))
gorder(sub_t_CF_core)
gsize(sub_t_CF_core)
sub_t_CF_core <- delete.vertices(sub_t_CF_core, 
                                 which(degree(sub_t_CF_core)== 0))
length(which(V(sub_t_CF_core)$Ateco == "C"))
length(which(V(sub_t_CF_core)$Ateco == "F"))
gorder(sub_t_CF_core)
gsize(sub_t_CF_core)
is.connected(sub_t_CF_core)
components(sub_t_CF_core) 

# plot coreness
PlotCoreness(sub_t_CF_core)

#centrality measures, closeness not computed as the graph is disconnected
summary(degree(sub_t_CF_core))
summary(closeness(sub_t_CF_core, normalized = T))
summary(betweenness(sub_t_CF_core, normalized = T)) 

# degree distribution of graph
deg <- degree(sub_t_CF_core, mode="all")
deg.dist <- degree_distribution(sub_t_CF_core, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.5, col="orange",
      xlab="Degree", ylab="Cumulative Frequency")

# edge density
edge_density(sub_t_CF_core)

# comunity detection of core subgraph using walktrap algorithm
cl<-cluster_walktrap(sub_t_CF_core)
colors <- rainbow(max(membership(cl)))
modularity(cl)
length(sizes(cl))

# plot with auto layout from layout_nicely()
plot(sub_t_CF_core, edge.arrow.size= 0.5, 
     layout=layout_nicely(sub_t_CF_core), vertex.label=NA, 
     vertex.color=membership(cl), vertex.size=5, edge.width=1)

#plot with layout from layout_with_fr() and ATECO labels
plot(cl, sub_t_CF_core, layout=layout_with_fr(sub_t_CF_core),
     vertex.label=V(sub_t_CF_core)$Ateco, vertex.label.cex=0.8, 
     vertex.size=8, edge.width = 1)

# comunity detection of core subgraph using louvain algorithm, transformed in 
# undirected graph
sub_t_CF_core_nd <- as.undirected(sub_t_CF_core)
cl<-cluster_louvain(sub_t_CF_core_nd)
colors <- rainbow(max(membership(cl)))
modularity(cl)
length(sizes(cl))

# plot with auto layout from layout_nicely()
plot(sub_t_CF_core_nd, edge.arrow.size= 0.5, 
     layout=layout_nicely(sub_t_CF_core_nd),  vertex.label=NA, 
     vertex.color=membership(cl), vertex.size=5, edge.width = 1)

#plot with layout from layout_with_fr() and ATECO labels
plot(cl, sub_t_CF_core_nd, layout=layout_with_fr(sub_t_CF_core_nd),
     vertex.label=V(sub_t_CF_core_nd)$Ateco, vertex.label.cex=0.8, 
     vertex.size=8, edge.width = 1)

#Plotting coreness of the subgraph
core_sub_t_CF_core_nd <-CorenessLayout(sub_t_CF_core_nd)
V(sub_t_CF_core_nd)$core <- coreness(sub_t_CF_core_nd, "all")
col <- as.color(V(sub_t_CF_core_nd)$core, opacity=1)
plot(sub_t_CF_core_nd, layout=core_sub_t_CF_core_nd, 
     vertex.label=V(sub_t_CF_core_nd)$Ateco, 
     vertex.label.cex=0.1, vertex.color=col, edge.width=1, vertex.size=8, 
     edge.arrow.size=0.1)



PlotCoreness(sub_t_CF_core_nd)

# ATECO codes of max degree nodes and their value
unlist(V(sub_t_CF_core_nd)[
  which(degree(sub_t_CF_core_nd)==max(degree(sub_t_CF_core_nd)))]$Ateco)
max(degree(sub_t_CF_core_nd))

#distribution of min degree nodes between ATECO codes and their value
table(unlist(V(sub_t_CF_core_nd)[
  which(degree(sub_t_CF_core_nd)==min(degree(sub_t_CF_core_nd)))]$Ateco))
min(degree(sub_t_CF_core_nd))

# assortativity for Ateco and degree
assortativity(sub_t_CF_core, types1=(ifelse(V(sub_t_CF_core)$Ateco=="C",1,0)))

#distribution of firm collaboration between C and G ATECO codes in core subgraph
mat_CF <-AtecoMatrixBI(sub_t_CF_core_nd, c("C","F"))
print(round(mat_CF,2))

#################### TRIESTE CF ERG MODEL ######################################

# cutoff for binary weight conversion: median
n <- summary(E(sub_t_CF_core)$weight)[3]

# binary conversion and export of adj matrix 
E(sub_t_CF_core)$weight <- ifelse(E(sub_t_CF_core)$weight>n,1,0)
adj_d <- as_adjacency_matrix(sub_t_CF_core, attr = "weight")

#network graph creation from adj matrix, removing isolated nodes as before
nu <- network(adj_d, loops=FALSE)
nu<-network::delete.vertices(nu,isolates(nu))
summary(nu)

# mutuality is 1, so we can use only dyad-indipendent parameters
grecip(nu)

# Plot of subgraph with colors from ATECO codes
gplot(nu, vertex.col=ifelse(unlist(V(sub_t_CF_core)$Ateco)== "C","red","blue"),
      vertex.cex=as.numeric(sqrt(degree(sub_t_CF_core) + 1)))

# load network attributes
network::set.vertex.attribute(nu, "Ateco", unlist(V(sub_t_CF_core)$Ateco))
network::set.vertex.attribute(nu, "int", unlist(V(sub_t_CF_core)$int))
network::set.vertex.attribute(nu, "Sll", unlist(V(sub_t_CF_core)$Sll))
network::set.vertex.attribute(nu, "deg", as.vector(degree(sub_t_CF_core)))
network::set.vertex.attribute(nu, "mem", as.vector(cluster_louvain(
  sub_t_CF_core_nd)$membership))

#Simple simulation with edges as only param
nu.01 <- ergm(nu~edges)
summary(nu.01)
nu.sim.01 <-simulate(nu.01)

#plot original graph and simulated
par(mfrow=c(1,2))
gplot(nu)
gplot(nu.sim.01)

# evaluating results
gof_nu <- gof(nu.01)
plot(gof_nu)

#simulation with edges and homophily on ATECO codes
nu.02 <- ergm(nu~edges+nodematch("Ateco", diff=T))
summary(nu.02)
nu.sim.02 <-simulate(nu.02)

#plot original graph and simulated
par(mfrow=c(1,2))
gplot(nu)
gplot(nu.sim.02)

# evaluating results
gof_nu <- gof(nu.02)
plot(gof_nu)

#simulation with edges and louvain membership
nu.03 <- ergm(nu~edges+nodecov("mem"))
summary(nu.03)
nu.sim.03 <-simulate(nu.03)

#plot original graph and simulated
par(mfrow=c(1,2))
gplot(nu)
gplot(nu.sim.03)

# evaluating results
gof_nu <- gof(nu.03)
plot(gof_nu) 

#simulation with edges, louvain membership and mutual
# we expect perfect fit 
nu.04 <- ergm(nu~edges+nodecov("mem")+mutual)
summary(nu.04)
nu.sim.04 <-simulate(nu.04)

#plot original graph and simulated
par(mfrow=c(1,2))
gplot(nu)
gplot(nu.sim.04)

# the graph is fitted perfectly ( as above, mutuality is 1)
mcmc.diagnostics(nu.04)
gof_nu <- gof(nu.04)
plot(gof_nu)

# reset plot
par(mfrow=c(1,1))

#appendix plot
par(mfrow=c(1,4))
gplot(nu)
gplot(nu.sim.01)
gplot(nu.sim.03)
gplot(nu.sim.04)

# reset plot
par(mfrow=c(1,1))




################################################################################