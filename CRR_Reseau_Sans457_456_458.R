setwd("~/doc/Cours_Master/Reseau_CRR")

library(readr)
# Données sous d et d_meta les chapitres qui correspondent aux articles
d <- read.csv("CRR_Source_Target_Sans457_456_458.csv",sep = ";")
d_meta <- read.csv("CRR_Parties.csv",sep=";")

library(igraph)
#Transmation en graphe de d avec la spécifité des chapitres
res <- graph_from_data_frame(d,vertices = d_meta,directed = F)
# la flèche c'est la direction du lien
# nombres de liens
gsize(res)
# nombres de noeuds
gorder(res)
# liste des noeuds
V(res)
# liste des liens
E(res)
# Attribu de chaque noeud, ici partie qui est le "chapitre"
V(res)$Partie
# Matrice d'adjacence, i.e, si 1 c'est qu'il y a un lien entre les 2 articles
# En ligne c'est l'article où on se situe
# en colonne c'est l'article qui est cité
res[c(1:10),c(1:10)]
#Degré de centralité
res_deg <- degree(res,mode=c("all"))
V(res)$degree <- res_deg
# C'est le nombre de liens entrants et sortants de chaque article
which.max(res_deg)
# L'article 457 est celui qui a le plus de liens
res_deg[457]
# Il a 255 liens
# Centralité des vecteurs propres
res_eig <- evcent(res)$vector
V(res)$Eigen <- res_eig
which.max(res_eig)
# L'article 457 est celui qui a la grande valeur de vecteur propre
# Betweenness centralité
res_bw <- betweenness(res,directed = F) # préciser ici si non dirigé
V(res)$betweenness <- res_bw
which.max(res_bw)
# L'article 456 est le plus central avec cet indicateur
df <- as_long_data_frame(res)

####♠ Network

# Network densité
edge_density(res)
# densité globale
A1 <- induced_subgraph(res,V(res)[Partie=="1"],impl=c("auto"))
edge_density(A1)
# densité du chapitre 1

# Assortivité
valeurs <- as.numeric(factor(V(res)$Partie))
assortativity_nominal(res,types = valeurs)

# Assortivité observée
assort_obs <- assortativity_nominal(res,types=valeurs)
results <- vector('list',1000)
for(i in 1:1000){results[[i]] <- assortativity_nominal(res,sample(valeurs))}

# Plot l'assort et l'assort obs
hist(unlist(results),xlim = c(-0.07,0.3))
abline(v=assort_obs , col="red",lty=3,lwd=2)
# leux deux sont complètement différents

###### network visualisation
library(RColorBrewer)
# Plot avec degrée de centralité
set.seed(1001)
pal <- brewer.pal(length(unique(V(res)$Partie)),"Set3")
plot(res,edge.color='black',vertex.label.cex=0.2, 
     vertex.frame.color=pal[as.numeric(as.factor(vertex_attr(res, "Class")))],
     vertex.color=pal[as.numeric(as.factor(vertex_attr(res,"Partie")))],
     vertex.size = sqrt(res_deg)/50, edge.width = sqrt(E(res))/800,edge.arrow.size=0.02,
     layout = layout.fruchterman.reingold)

# Plot avec la centralité des vecteurs propres
set.seed(1001)
plot(res,edge.color='black',vertex.label.cex=0.2, 
     vertex.frame.color=pal[as.numeric(as.factor(vertex_attr(res, "Class")))],
     vertex.color=pal[as.numeric(as.factor(vertex_attr(res,"Partie")))],
     vertex.size = sqrt(res_eig)/50, edge.width = sqrt(E(res))/800,edge.arrow.size=0.02,
     layout = layout.fruchterman.reingold)

# Plot avec la betweenness centralité
set.seed(1001)
plot(res,edge.color='black',vertex.label.cex=0.2, 
     vertex.frame.color=pal[as.numeric(as.factor(vertex_attr(res, "Class")))],
     vertex.color=pal[as.numeric(as.factor(vertex_attr(res,"Partie")))],
     vertex.size = sqrt(res_bw)/50, edge.width = sqrt(E(res))/800,edge.arrow.size=0.02,
     layout = layout.fruchterman.reingold)

# Corrélations

# Corrélation entre degré et betweenness 
plot(V(res)$degree,V(res)$betweenness)

# Corrélation entre degré et vecteur propre
plot(V(res)$degree,V(res)$Eigen)

######## Détection de communauté

# Clustering de Louvain
lc <- cluster_louvain(res)
communities(lc)

# Plot betweenness avec les communautés

set.seed(1001)
plot(lc,res, edge.color = 'black',vertex.label.cex =0.2, 
     vertex.frame.color=pal[as.numeric(as.factor(vertex_attr(res, "Class")))],
     vertex.color=pal[as.numeric(as.factor(vertex_attr(res, "Class")))],
     vertex.size = sqrt(res_bw)/100, edge.width=sqrt(E(res)/10000),edge.arrow.size=0.02,
     layout = layout.fruchterman.reingold)
