library(igraph)

champ_finals <- read.csv(("champions_league_finals.csv"), sep = ";", row.names = 1, header = TRUE)

# Create data frames
champ_finals <- as.data.frame(champ_finals)

# Create matrix
champ_finals_m <- as.matrix(champ_finals)

# Replace 'NA' values with '0'
champ_finals_m[is.na(champ_finals_m)] <- 0

# Create graph object
champ_finals_g <- graph_from_adjacency_matrix(champ_finals_m, mode = "directed", weighted = TRUE)

# Brief analysis
champ_finals_g[]
is.directed(champ_finals_g) # Is the object directed?
is.weighted(champ_finals_g) # Is the object weighted?
E(champ_finals_g)$weight # Examining the weight of each edge 
E(champ_finals_g) # Examining the edges of the object (victories)
V(champ_finals_g) # Examining vertices of the object
gsize(champ_finals_g) # Counting number of edges 
gorder(champ_finals_g) # Counting vertices
vertex_attr(champ_finals_g) # Examining vertices' attributes

# Out-degree Centralization
odegree_cent <- centr_degree(champ_finals_g, mode = "out")
g_ind_c <- odegree_cent$centralization # Network centralization 
g_ind_c <- round(g_ind_c, digits = 2)

# Out-degree of a vertex: A -> B (number of teams to which a vertex has an outgoing edge directed to)
g_outd <- degree(champ_finals_g, mode = c("out"))
g_outd # Number of edges of each team
index <- which.max(g_outd)
g_outd[index] # Team with more outgoing edges (victories)

# Plot
par(mar = c(0, 0, 0, 0))
plot(champ_finals_g) # How ugly is this?

V(champ_finals_g)$size = g_outd + 5
V(champ_finals_g)$color = ifelse(g_outd > 0, "green", "darkred")
V(champ_finals_g)$label.color = "black"
V(champ_finals_g)$label.cex = ifelse(g_outd > 0, .75, .5)

E(champ_finals_g)$arrow.width = .5
E(champ_finals_g)$arrow.size = .5
E(champ_finals_g)$width = (E(champ_finals_g)$weight) + 1
E(champ_finals_g)$color = "gray"

plot(champ_finals_g, layout = layout_with_dh(champ_finals_g))
