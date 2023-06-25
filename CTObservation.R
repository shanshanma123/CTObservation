
install.packages('readr')
install.packages('tidyverse')
intall.packages('ggplot2')
install.packages('likert')
install.packages('xtable')
library(dplyr)
library(readr)
library(ggplot2)
library(xtable)
library(likert)

#import data set Academic_Performance
df1_Academic <- read_csv("Desktop/CTObservation_R/Academic_Performance.csv")
#View(df1_Academic)
#head(df1_Academic, n=5)
df1_Academic <- na.omit(df1_Academic)
str(df1_Academic)


#read igsaw_Performance data
df2_Jigsaw <- read_csv("Desktop/CTObservation_R/Jigsaw_Performance.csv")
df2_Jigsaw <- na.omit(df2_Jigsaw)

#histogram for Jigsaw time duration 
hist(df2_Jigsaw$Duration)

#histogram by group
par(mfrow = c(3,1))

#histograms for A/B/C group students' time duration 
hist(df2_Jigsaw$Duration[df2_Jigsaw$Group == "1"],
    main = "A Group Jigsaw Puzzle Solving Time", 
    xlab ="Time Duration", 
    col = "cornsilk")

hist(df2_Jigsaw$Duration[df2_Jigsaw$Group == "2"],
     main = "B Group Jigsaw Puzzle Solving Time", 
     xlab ="Time Duration", 
     col = "darkseagreen1")

hist(df2_Jigsaw$Duration[df2_Jigsaw$Group == "3"],
     main = "C Group Jigsaw Puzzle Solving Time", 
     xlab ="Time Duration", 
     col = "cadetblue1")

#Restore graphic parameter 
par(mfrow = c(1,1))


#Explore the significant difference on Jigsaw puzzle solving time between groups
oneway.test(df2_Jigsaw$Duration ~ Group, data = df2_Jigsaw, var.equal = TRUE)


#Correlations between variables Time duration, strategicOptions, ObservationMode, Complete, Correct and Tipscheck
cor(df2_Jigsaw[, c('Duration','StrategicOptions','ObservationMode','Complete','Correct','Tipscheck')])

#Read CT peroformance data
df3_CT <- read_csv("Desktop/CTObservation_R/CT_Performance.csv")
#View(df3_CT)
df3_CT <- na.omit(df3_CT)
str(df3_CT)

#mean and sd value of students' total CT score to locate the most like group in CT test manual
mean(df3_CT$All_Sum)
sd(df3_CT$All_Sum)

#histogram by CT section
par(mfrow = c(2,2))

#histograms for each section 
hist(df3_CT$Part1_Sum, 
     xlim=c(0,25),
     main = "Section I:Hypothesis Testing", 
     xlab ="Score", 
     col = "yellow")

hist(df3_CT$Part2_Sum, 
     xlim=c(0,25),
     main = "Section II:Credibility of Sources & Observations", 
     xlab ="Score", 
     col = "purple")

hist(df3_CT$Part3_Sum, 
     xlim=c(0,25),
     main = "Section III:Deduction", 
     xlab ="Score", 
     col = "blue")

hist(df3_CT$Part4_Sum, 
     xlim=c(0,25),
     main = "Section IV:Assumption Identification", 
     xlab ="Score", 
     col = "green")

#Restore graphic parameter 
par(mfrow = c(1,1))

#Histogram for CT total score
hist(df3_CT$All_Sum, 
     xlim=c(0,71),
     main = "CT total Score", 
     xlab ="Score", 
     col = "red")

length(na.omit(df2_Jigsaw$Duration))
length(na.omit(df3_CT$All_Sum))

# Correlation between puzzle solving time and students' total CT score
res <- cor.test(df2_Jigsaw$Duration, df3_CT$All_Sum, 
              method = "pearson")
res

#Correlations between variables Time duration, strategicOptions, ObservationMode, and Tipscheck
cor(df2_Jigsaw[, c('Duration', 'StrategicOptions', 'ObservationMode', 'Complete', 'Correct','Tipscheck')])


# Clear console
cat("\014")