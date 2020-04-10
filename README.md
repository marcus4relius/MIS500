# MIS500 Module 4 Critical Thinking
Log of work performed in MIS500 Foundations of Data Analytics

#R Hypothesis Test
install.packages("dplyr")

#Installing dplyr library
library(dplyr)

#Creating data frames
tScore_before <- c(40, 62, 74, 22, 64, 65, 49, 49, 49)
tScore_after <- c(68, 61, 64, 76, 90, 75, 66, 60, 63)

#Create a data frame
my_data <- data.frame(
  group = rep(c("Score Before", "Score After"), each = 9),
  scores = c(tScore_before, tScore_after)
)

#Print all Data
print(my_data)

#Calculate summary statistics
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(scores, na.rm = TRUE),
    sd = sd(scores, na.rm = TRUE)
)

#Installing ggpubr package
install.packages("ggpubr")

#Plotting weight by group and color by group
library("ggpubr")
ggboxplot(my_data, x = "group", y = "scores", 
  order = c("Score Before", "Score After"), 
  xlab = "Groups", ylab = "Scores")
  
#Install pairedData package
install.packages("PairedData")

#Load PairedData library
library(PairedData)

#Create data frame for test scores before
before <- subset(my_data, group =="Score Before", scores,
  drop = TRUE)
  
#Create data fram for test scores after
after <- subset(my_data, group == "Score After", scores,
  drop = TRUE)

#Plot paired data
pd <- paired(before, after)
plot(pd, type = "profile")  + theme_bw()

#Compute Unpaired Two Sample t-test
res <- t.test(tScore_before, tScore_after, var.equal = TRUE)
res

#Compute independent t-test
t.test(scores ~ group, data = my_data, var.equal = TRUE)
res

#test whether the average score before score is less than the average after score
t.test(scores ~ group, data = my_data,
  var.equal = TRUE, alternative = "less")
