data <- mtcars


library(ggplot2)

data$vs <- factor(data$vs, levels = c(0, 1), 
                  labels = c("V-Shaped", "Straight"))

ggplot(data, aes(x = factor(vs), y = wt))+
  geom_boxplot()+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color = "red", 
               size = 7)+
  theme_bw()+
  labs(x = "Engine", 
       y = "Weight (1000 lbs)",
       title = "Distribution of Car Weights by Engine Type")+
  theme(title = element_text(face = "bold"))

ggplot(data, aes(x = qsec, fill = vs))+
  stat_bin(bins = 10, color = "white")+
  scale_fill_manual(values = c("dodgerblue3", "darkolivegreen3"))+
  theme_bw()+
  labs(x = "1/4 Mile Time (Seconds)", y = "Count", fill = "Engine Type", 
       title = "Distribution of 1/4 Mile Times by Engine Type")+
  scale_x_continuous(expand = c(0, .5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7.2))+
  theme(title = element_text(face = "bold"))

data$am <- factor(data$am, levels = c(0, 1), 
                  labels = c("Automatic", "Manual"))
data$cyl <- factor(data$cyl, levels = c(4, 6, 8),
                   labels = c("4", 
                              "6", 
                              "8"))

ggplot(data, aes(x = mpg, color = cyl))+
  geom_freqpoly(bins = 12)+
  theme_bw()+
  facet_wrap(~vs*am, axes = "all_x")+
  labs(y = "Count", x = "Miles per Gallon", 
       color = "Cylinders", 
       title = "Distribution of Miles per Gallon")+
  theme(title = element_text(face = "bold"))

