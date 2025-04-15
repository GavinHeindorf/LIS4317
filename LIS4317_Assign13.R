library(animation)
library(ggplot2)
library(gganimate)
library(dplyr)
library(gifski)


x <- c()
z <- c()
xvals <- seq(0, 10, .01)
iters <- seq(0, 39, 2)

      for (i in 1:length(xvals)){
  
        x = c(x, sinpi(xvals[i]))
        z = c(z, cospi(xvals[i]))
      }
all <- data.frame(xvals = rep(xvals, 2),
                      waves = c(x, z),
                      trig = c(
                        rep("sine", 1001),
                        rep("cosine", 1001)))

for (j in 1:20){
  
  xvals <- xvals + 1
  
  print(xvals[1])
  
  for (i in 1:length(xvals)){
  
    x = c(x, sinpi(xvals[i]))
    z = c(z, cospi(xvals[i]))
  }
  
  if (j == 1){
  
    all <- data.frame(xvals = rep(xvals, 2),
                      waves = c(x, z),
                      trig = c(
                        rep("sine", 201),
                        rep("cosine", 201)), 
                      xstart = rep(xvals[1], 402))
  } else{
    all <- all %>%
      add_row(data.frame(xvals = rep(xvals, 2),
                      waves = c(x, z),
                      trig = c(
                        rep("sine", 201),
                        rep("cosine", 201)), 
                      xstart = rep(xvals[1], 402)))
  }
}

anim_plot <- ggplot(all, aes(x = xvals,
                y = waves,
                color = factor(trig)))+
  geom_line(linewidth = 2)+
  labs(x = bquote(pi), y = "Trigonometric Value", color = "Function")+
  scale_color_manual(values = c("darkolivegreen2",
                                "lightpink1"))+
  theme_classic()+
  ggtitle("Visual Representation of Sine and Cosine Functions")+
  theme(text = element_text(size = 12),
        title = element_text(face = "bold", size = 14))+
  transition_reveal(xvals)+
  view_follow(fixed_y = TRUE, aspect_ratio = .4)

animate(anim_plot, fps = 70, duration = 3, 
                       renderer = gifski_renderer(file = "LIS4317_animation.gif",
                                                  width = 1500, 
                                                  height = 1200))

