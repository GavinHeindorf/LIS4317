library(ggnet)
library(network)
library(sna)
library(ggplot2)

# Create random, non-directional net with 10 nodes
net = rgraph(10, mode = "graph", tprob = 0.5)
net = network(net, directed = FALSE)

# Set vertex names
network.vertex.names(net) <- c("Lion", "Tiger", "Leopard", 
                              "Jaguar", "House", "Snow Leopard", 
                              "Ocelot", "Panther", "Serval", 
                              "River")

# Add hemisphere in which cat lives to set 
net %v% "Hemisphere" <- c("East", "East", "East", 
                          "West", "West", "East", 
                          "West", "West", "East", "West")

# Add cat sizes to set node size
net %v% "CatSize" <- c("Big", "Big", "Big", 
                       "Big", "Little", "Big", 
                       "Little", "Big", "Little", 
                       "Little")

##Plot network
ggnet2(net, color = "Hemisphere", palette = c("East" = "gold2", "West" = "cyan3"), 
       size = "CatSize",
       size.palette = c("Big" = 10, "Little" = 3),
       label = T, label.size = 3)+
  ggtitle("Cat Species Network")+
  theme(title = element_text(face = "bold", 
                             size = 24))

