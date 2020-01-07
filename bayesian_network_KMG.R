library(bnlearn)

#create the names for the network
names_network = c("W", "R", "SR", "WG", "S")

#attach nodes to graph
g = empty.graph(names_network)

#create connection between nodes
arc.set = matrix(c("W", "W", "S", "R", "R", "S", "R", "WG", "WG", "SR"),
ncol = 2, dimnames = list (NULL, c("from", "to")))
arc.set
arcs(g) = arc.set

#plot connected nodes
plot(g)

#create conditional probability tables for each node
cptW = matrix(c(0.6, 0.4), ncol = 2, dimnames = list(NULL, "W" = c("true", "false")))
cptS = c(0.2, 0.8, .75, .25)
dim(cptS) = c(2,2)
dimnames(cptS) = list("S" = c("true", "false"), "W" = c("true", "false"))
cptR = c(0.8, 0.2, .1, .9)
dim(cptR) = c(2,2)
dimnames(cptR) = list("R" = c("true", "false"), "W" = c("true", "false"))
cptWG = c(.8, .2, 0, 1, .95, .05, .9, .1)
dim(cptWG) = c(2,2,2) #2 x 2 x 2 = 8 numbers in cptwg
dimnames(cptWG) = list("WG" = c("true", "false"), "R" = c("true", "false"), "S" = c("false", "true"))
cptSR = c(0.7, 0.3, 0, 1)
dim(cptSR) = c(2,2)
dimnames(cptSR) = list("SR" = c("true", "false"), "R" = c("true", "false"))

#attach cpts to graph
dfit = custom.fit(g, dist = list(W = cptW, S = cptS, R = cptR, WG = cptWG, SR= cptSR))
dfit

#calculate probabilities
cpquery(debug = TRUE, dfit, event=(SR == "false"), evidence=(W == "true"))
cpquery(dfit, event=(WG == "true"), evidence=(S == "true") & (W == "true"))
cpquery(dfit, event=(SR == "true"), evidence=(WG == "true") & (S == "false"))
cpquery(dfit, event=(WG == "false"), evidence=(SR == "false") & (W == "true"))

