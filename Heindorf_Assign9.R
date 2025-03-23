library(ggcorrplot)

data <- mtcars

ggcorrplot(corr = cor(data), 
           hc.order = T, 
           colors = c("brown1",
                      "white", 
                      "green2"), 
           type = "lower",
           legend.title = "Magnitude\nof Correlation")+
  theme_classic()+
  theme(axis.title = element_blank(), 
        text = element_text(family = "Times New Roman"),
        axis.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))


data$am <- factor(data$am, levels = c(0,1),
                    labels = c("Automatic", "Manual"))

ggplot(data, aes(x = factor(cyl), y = mpg, fill = am))+
    stat_summary(fun = "mean", geom = "bar", position = "dodge", 
               width = .7)+
  stat_summary(fun.data = mean_se, geom="errorbar", 
               width = .3, position = position_dodge(.7),
               linewidth = 1.5)+
  labs(fill = "Transmission", x = "Number of Cylinders",
       y = "Miles per Gallon")+
  ggtitle("Miles per Gallon by Cylinders and Transmission")+
  theme_classic()+
  scale_y_continuous(expand = c(0,.5))+
  scale_fill_manual(values = c("lightskyblue", 
                               "lightcoral"))+
  theme(text = element_text(family = "Times New Roman"), 
        title = element_text(face = "bold"))
