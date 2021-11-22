### LOAD LIBRARIES 
library(tidyverse)
library(ggraph)
library(tidygraph)
library(babynames)


### EXPLORE BABYNAMES STRUCTURE 
head(babynames)


### CREATE NODES TABLE 
get_random_names <- function(n) { 
  unique_babynames <- distinct(babynames, name, .keep_all = TRUE) 
  index <- sample(1:nrow(unique_babynames), n, replace = FALSE) 
  names <- unique_babynames[index, ] 
  names 
}

nodes <- get_random_names(20)


### CREATE LINKS TABLE 
# Create source and target vectors 
src <- sample(1:nrow(nodes), nrow(nodes)*2, replace = TRUE)  
target <- sample(1:nrow(nodes), nrow(nodes)*2, replace = TRUE)

# Merge vectors to form a single table 
links <- data.frame(src, target)

# Clean up 
links <- data.frame(src, target) %>%  
  filter(!(src == target)) 
links <- unique(links[c("src", "target")])


### PLOT NETWORK 
# Type cast to tbl_graph object
social_net_tbls <- tbl_graph(nodes = nodes, 
                             edges = links, 
                             directed = FALSE)

# Create the network 
social_net <- ggraph(social_net_tbls, layout = "stress") +                                                                                                         
  geom_node_point(size = 2) +                                         
  geom_node_text(aes(label = name), nudge_y = 0.05, nudge_x = 0.2)+ 
  geom_edge_link() +
  theme_void()

# Render the network 
show(social_net)
