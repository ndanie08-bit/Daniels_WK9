###Natalie Daniels
###WK 9 social network

install.packages("igraph")
library(igraph)

edges <- data.frame(
  from = c("Ross","Ross","Ross","Ross","Ross","Ross","Ross","Ross",
           "Rachel","Rachel","Rachel","Rachel","Rachel",
           "Monica","Monica","Monica","Monica","Monica",
           "Chandler","Chandler","Chandler",
           "Joey","Joey","Joey",
           "Phoebe","Phoebe","Phoebe",
           "Carol","Barry"),
  to   = c("Rachel","Carol","Emily","Julie","Mona","Elizabeth","Charlie","Janice",
           "Joey","Paolo","Mark","Joshua","Barry",
           "Chandler","Richard","Pete","Alan","Julio",
           "Janice","Kathy","Mindy",
           "Kathy","Janine","Charlie",
           "Mike","David","Gary",
           "Susan","Mindy"),
  stringsAsFactors = FALSE
)

# Save to disk so the "read into R" step is real
write.csv(edges, "friends_edges.csv", row.names = FALSE)

# Optional
main_cast <- c("Ross","Rachel","Monica","Chandler","Joey","Phoebe")

#READ IN 
edge_list <- read.csv("friends_edges.csv", stringsAsFactors = FALSE)
g <- graph_from_data_frame(edge_list, directed = FALSE)

# Add node attributes
V(g)$main <- V(g)$name %in% main_cast
V(g)$degree <- degree(g)

###Quick checks
print(g)
summary(g)

###plot 
plot(g)

###make it pretty
set.seed(42)

# Color
V(g)$color <- ifelse(V(g)$main, "pink", "green")

# Size
V(g)$size <- 8 + V(g)$degree * 4

# Labels
V(g)$label.cex   <- ifelse(V(g)$main, 1.0, 0.7)
V(g)$label.font  <- ifelse(V(g)$main, 2, 1)
V(g)$label.color <- "black"
V(g)$frame.color <- "white"

# Edges
ends_mat <- ends(g, E(g))
both_main <- ends_mat[,1] %in% main_cast & ends_mat[,2] %in% main_cast
E(g)$color <- ifelse(both_main, "pink", "grey70")
E(g)$width <- ifelse(both_main, 3.5, 1.5)

# Layout
lay <- layout_with_fr(g)

png("friends_network.png", width = 1400, height = 1100, res = 150)
plot(g,
     layout = lay,
     vertex.label.dist = 0,
     main = "Who Dated Whom in Friends",
     sub  = "Node size = number of romantic ties | Pink = main cast")
legend("bottomleft",
       legend = c("Main cast", "Other love interests"),
       pt.bg  = c("pink", "green"),
       pch = 21, pt.cex = 2, bty = "n")
dev.off()

# Also show in the RStudio plot pane
plot(g, layout = lay, main = "Who Dated Whom in Friends")

