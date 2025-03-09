data(airquality)

library(ggplot2)

ggplot(airquality, aes(x = Wind, y = Temp, color = factor(Month)))+
  geom_point()+
  facet_wrap(~Month)+
  stat_smooth(method = "lm", se = F, color = "black")+
  theme_classic()+
  labs(color = "Month", y = "Temperature")+
  scale_color_manual(values = c(
    "chocolate1",
    "aquamarine3", 
    "mediumpurple2", 
    "darkolivegreen3", 
    "indianred3"))+
  ggtitle("Relationship Between Wind and Temperature by Month")+
  theme(title = element_text(face = "bold"), 
        text = element_text(family = "Times New Roman"))
        
airtempmod <- lm(Temp ~ Wind, data = airquality)

airquality$resids <- airtempmod$residuals

ggplot(airquality, aes(x = Wind, 
                       y = resids, 
                       color = factor(Month)))+
  geom_hline(yintercept = 0, color = "black")+
  geom_point(size = 2)+
  theme_classic()+
  labs(color = "Month", 
       y = "Residuals")+
  scale_color_manual(values = c(
    "chocolate1",
    "aquamarine3", 
    "mediumpurple2", 
    "darkolivegreen3", 
    "indianred3"))+
  theme(title = element_text(face = "bold"),
        text = element_text(family = "Times New Roman"))+
  ggtitle("Residual Scores Across Levels of Wind by Month")


ggplot(airquality, aes(x = Wind, y = Temp))+
  geom_point(size = 2, color = "grey68")+
  geom_abline(intercept = 
                coefficients(airtempmod)[1], 
              slope = coefficients(airtempmod)[2],
              size = 1.3)+
  theme_classic()+
  ggtitle("Relationship Between Wind and Temperature")+
  labs(y = "Temperature")+
  theme(title = element_text(face = "bold"), 
        text = element_text(family = "Times New Roman"))

