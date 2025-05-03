# Load in libraries
library(ggplot2)
library(maps)
library(usmap)
library(dplyr)
library(animation)
library(gganimate)
library(gifski)

setwd("/Users/gheindorf/Downloads/")

# Read in dataset and remove all rows that do not include data for the 50 states
physicalHealth <- read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")
physicalHealth <- physicalHealth[which(physicalHealth$LocationDesc %in%
                                         state.name),]

# Get percentages by state and question for first visual
grp_ObByState <- physicalHealth %>%
  group_by(LocationDesc, Question) %>%
  summarise(Resp = mean(Data_Value, na.rm = T)) %>%
  ungroup()

# Extract only the percentages that pertain to the adult obesity question
grp_ObByState <- grp_ObByState[which(grp_ObByState$Question == 
                                       grp_ObByState$Question[2]),]

# Create first visual: map showing obesity rates by state
colnames(grp_ObByState)[1] <- "state"
plot_usmap(regions = "states", data = grp_ObByState, values = "Resp")+
  scale_fill_gradient(transform = "identity",
                      low = 'lightcyan2', 
                      high = 'royalblue3')+
  ggtitle(~bold(underline("Obesity Rates in USA by State")))+
  labs(fill = "Obesity\nRate (%)")+
  theme(legend.position = "bottom", 
        title = element_text(face = "bold", 
                             family = "Times New Roman",
                             size = 13),
         plot.title = element_text(hjust = .5, size = 17))

# Extract percentages by year, state, and question
grp_physicalHealth <- physicalHealth %>%
  group_by(YearEnd, LocationDesc, Question) %>%
  summarise(ObPerc = mean(Data_Value, na.rm = T)) %>%
  ungroup()

# Check to see whether we have 50 observations per year
grp_physicalHealth %>%
  group_by(YearEnd) %>%
  summarise(length(unique(LocationDesc))) %>%
  ungroup()

# Change order of the states variable for plotting alphabetically (top -> bottom)
grp_physicalHealth$LocationDesc <- factor(grp_physicalHealth$LocationDesc,
                                          levels = sort(unique(grp_physicalHealth$LocationDesc), 
                                                        decreasing = T))

# Create second visual: Animation showing changes in obesity rates by state acrossing time
ObPerc_byyear <- ggplot(grp_physicalHealth[which(grp_physicalHealth$Question ==
                                                   grp_physicalHealth$Question[2]),],
       aes(x = ObPerc, y = LocationDesc))+
  geom_bar(stat = "identity", fill = "lightsteelblue2", 
           width = .8, color = "navy", linewidth = .2)+
  labs(title = ~bold(underline("Obesity Percentage of States by Year")), 
       y = "State", x = "Obesity Percentage (%)", 
       subtitle = paste0("Year: ", "{frame_time}"))+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman"),
        title = element_text(face = "bold", size = 14),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        axis.text.y = element_text(size = 5))+
  scale_x_continuous(expand = expansion(mult = c(0.01, .05)))+
  transition_time(YearEnd)

animate(ObPerc_byyear, fps = 60, duration = 10, res = 300, width = 2000,
        height = 1800,
        renderer = gifski_renderer(file = "LIS4317_final_animate.gif"))

# Extract percentages by year and question
grp_ObRelPhys <- physicalHealth %>%
  group_by(YearEnd, Question) %>%
  summarise(Resp = mean(Data_Value, na.rm = T)) %>%
  ungroup()

# Keep only percentages for obesity, aerobic activity, and no leisure-team activity
grp_ObRelPhys <- grp_ObRelPhys[which(grp_ObRelPhys$Question %in%
                                       grp_ObRelPhys$Question[c(2, 3, 7)]),]

# Set up question factor to reflect the captured constructs
grp_ObRelPhys$Question <- factor(grp_ObRelPhys$Question, 
                                 levels = c(unique(grp_ObRelPhys$Question)[1], 
                                            unique(grp_ObRelPhys$Question)[2], 
                                            unique(grp_ObRelPhys$Question)[3]),
                                 labels = c("Have Obesity", "Weekly Intense\nAerobic Activity", "No Leisure-Time\nPhysical Activity"))

# Create visual 3: line graph showing time-series of all three variables
ggplot(grp_ObRelPhys, aes(x = YearEnd, y = Resp, color = Question))+
  geom_line(linewidth = 1.4)+
  theme_bw()+
  scale_x_continuous(breaks = unique(grp_ObRelPhys$YearEnd),
                     expand = expansion(mult = c(.03, .03)))+
  labs(x = "Year", y = "Percent of Population", 
       title = ~bold(underline("Obesity and Activity Among US Adult Population From 2011 to 2023")))+
  scale_y_continuous(n.breaks = 8, limits = c(0, 60))+
  theme(text = element_text(family = "Times New Roman",
                            size = 12, color = "white"),
        plot.title = element_text(hjust = .5),
        axis.title = element_text(face = "bold", size = 17), 
        legend.title = element_blank(),
        legend.position = "bottom",  
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "grey42",
                                       color = "grey42"), 
        panel.grid.major = element_line(linewidth = .1),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "white"),
        axis.text = element_text(color = "white"),
        legend.background = element_rect(fill = "grey42"))+
  scale_color_manual(values = c("orchid1",
                                "turquoise1", 
                                "greenyellow"))
