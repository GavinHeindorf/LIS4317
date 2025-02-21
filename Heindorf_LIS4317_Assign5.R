library(tidyr)
library(plyr)

#systematically create three groups with data
grpA <- sample(1:8, size = 30, replace = T)
grpB <- sample(5:13, size = 30, replace = T)
grpC <- sample(10:18, size = 30, replace = T)

#convert data to usable format
comb <- data.frame(A = grpA, B = grpB, C = grpC)
comb_long <- data.frame(pivot_longer(comb, c(A, B, C)))

#create pie chart showing observations per group
ns <- count(comb_long$name)$freq
names(ns) <- c("A", "B", "C")
pie(ns, col = c("chocolate1", "tan",
               "darkgreen"),
    main = "Observations per Group")

#create histogram showing distribution of data within each group
hist(comb_long$value[which(comb_long$name == "A")], 
     main = "Distribution of Group A Scores", 
     xlab = "Value",
     ylab = "Frequency", 
     col = "chocolate1")
hist(comb_long$value[which(comb_long$name == "B")],
     main = "Distribution of Group B Scores",
     xlab = "Value", 
     ylab = "Frequency",
     col = "tan")
hist(comb_long$value[which(comb_long$name == "C")],
     main = "Distribution of Group C Scores",
     xlab = "Value", 
     ylab = "Frequency",
     col = "darkgreen")



#create bar chart showing means across each group
means <- aggregate(comb_long$value, by = 
                    list(comb_long$name), 
                   mean)$x
names(means) <- c("A", "B", "C")

barplot(means, main = "Mean Score by Group", 
        xlab = "Group", ylab = "Score", 
        col = "skyblue", ylim = c(0,15))
abline(h = mean(comb_long$value), col = "red", 
       lwd = 4, lty = 4)

#create box plot showing scores by group
boxplot(comb_long$value ~ comb_long$name, 
        ylab = "Score", 
        xlab = "Group", 
        main = "Score Summaries by Group",
        col = c("chocolate1", "tan", 
                "darkgreen"))
abline(h = mean(comb_long$value), col = "red", 
       lwd = 4, lty = 4)

