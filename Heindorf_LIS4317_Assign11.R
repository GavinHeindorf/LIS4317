library(ggplot2)
library(ggExtra)
library(ggthemes)


data = faithful

scatter <- ggplot(data, aes(x = waiting, y = eruptions))+
  geom_point()+
  theme_bw()

ggMarginal(scatter, type = "density", fill = "grey72", size = 4)