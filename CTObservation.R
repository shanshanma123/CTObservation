#Install necessary packages
install.packages('readr')
install.packages('tidyverse')
install.packages('ggplot2')
install.packages('corrplot')
install.packages('pastecs')

#Loading the packages 
library(dplyr)
library(readr)
library(ggplot2)
library(corrplot)
library(pastecs)

#import data set Academic_Performance
df1_Academic <- read_csv("Desktop/CTObservation_R/Academic_Performance.csv")
#View(df1_Academic)
#head(df1_Academic, n=5)
df1_Academic <- na.omit(df1_Academic)
str(df1_Academic)
attach(df1_Academic)
stat.desc(df1_Academic)


#read jigsaw_Performance data
df2_Jigsaw <- read_csv("Desktop/CTObservation_R/Jigsaw_Performance.csv")
df2_Jigsaw <- na.omit(df2_Jigsaw)
attach(df2_Jigsaw)

#histogram for Jigsaw time duration 
hist(Duration)

#histogram by group
par(mfrow = c(3,1))

#histograms for A/B/C group students' time duration 
hist(Duration[Group == "1"],
    main = "A Group Jigsaw Puzzle Solving Time", 
    xlab ="Time Duration", 
    col = "cornsilk")

hist(Duration[Group == "2"],
     main = "B Group Jigsaw Puzzle Solving Time", 
     xlab ="Time Duration", 
     col = "darkseagreen1")

hist(Duration[Group == "3"],
     main = "C Group Jigsaw Puzzle Solving Time", 
     xlab ="Time Duration", 
     col = "cadetblue1")

#Restore graphic parameter 
par(mfrow = c(1,1))

#Explore the significant difference on Jigsaw puzzle solving time between groups
oneway.test(Duration ~ Group, var.equal = TRUE)
oneway.test(Duration ~ Gender, var.equal = TRUE)

#Correlations between variables Time duration, strategicOptions, ObservationMode, Complete, and Tipscheck
cor(df2_Jigsaw[, c('Duration','StrategicOptions','ObservationMode','Complete','Tipscheck')])

#Read CT performance data
df3_CT <- read_csv("Desktop/CTObservation_R/CT_Performance.csv")
#View(df3_CT)
df3_CT <- na.omit(df3_CT)
str(df3_CT)
attach(df3_CT)

#mean and sd value of students' total CT score to locate the most like group in CT test manual
mean(All_Sum)
sd(All_Sum)

#histogram by CT section
par(mfrow = c(2,2))

#histograms for each section 
hist(Part1_Sum, 
     xlim=c(-25,25),
     main = "Section I:Hypothesis Testing", 
     xlab ="Score", 
     col = "darkgoldenrod1")

hist(Part2_Sum, 
     xlim=c(-25,25),
     main = "Section II:Credibility of Sources & Observations", 
     xlab ="Score", 
     col = "darkorchid1")

hist(Part3_Sum, 
     xlim=c(-25,25),
     main = "Section III:Deduction", 
     xlab ="Score", 
     col = "chocolate1")

hist(Part4_Sum, 
     xlim=c(-25,25),
     main = "Section IV:Assumption Identification", 
     xlab ="Score", 
     col = "darkolivegreen1")

#Restore graphic parameter 
par(mfrow = c(1,1))

#Histogram for CT total score
hist(df3_CT$All_Sum, 
     xlim=c(-71,71),
     main = "CT total Score", 
     xlab ="Score", 
     col = "cyan1")


# Check the correlation between variables across data sets
##merge data sets df2 and df3 into a single data frame 
merged_CTJigsaw <- merge(df2_Jigsaw, df3_CT, by = "Code")
str(merged_CTJigsaw)
#check the correlations between CT performance and jigsaw performance
correlation_CTJS <- cor(merged_CTJigsaw[, c('All_Sum', 'Part1_Sum', 'Part2_Sum', 'Part3_Sum', 'Part4_Sum', 'Duration', 'StrategicOptions', 'ObservationMode', 'Complete', 'Tipscheck')])
correlation_CTJS 
#visualize the correlation
corrplot(correlation_CTJS, method = "circle")


#merge data sets df1 and df3 into a single data frame 
merged_CTAca <- merge(df1_Academic, df3_CT, by = "Code")
str(merged_CTAca)
#check the correlations between CT performance and academic performance
correlation_CTACA <- cor(merged_CTAca[, c('All_Sum', 'Part1_Sum', 'Part2_Sum', 'Part3_Sum','Part4_Sum', 'CN9', 'MTH9', 'Eng9','CN10', 'MTH10', 'Eng10')])
correlation_CTACA 
#visualize the correlation
corrplot(correlation_CTACA, method = "circle")


#merge data sets df1 and df2 into a single data frame 
merged_CTJigsaw <- merge(df2_Jigsaw, df1_Academic, by = "Code")
str(merged_JigsawAca)
#check the correlations between jigsaw performance and academic performance
correlation_JSACA <- cor(merged_JigsawAca[, c('CN9', 'MTH9', 'Eng9', 'CN10', 'MTH10', 'Eng10', 'Duration', 'StrategicOptions', 'ObservationMode', 'Complete', 'Tipscheck')])
correlation_JSACA 
#visualize the correlation
corrplot(correlation_JSACA, method = "circle")

#see if students' CT scores can be predicted by Jigsaw performance
reg1 <- lm(All_Sum ~ Duration + StrategicOptions + ObservationMode + Complete + Tipscheck)

#results
reg1
summary(reg1)

#see if CT score can predict students' academic performance
reg2 <- lm(All_Sum ~ CN9 + MTH9 + Eng9)
reg3 <- lm(All_Sum ~ CN10 + MTH10 + Eng10)

#results
reg2
summary(reg2)
reg3
summary(reg3)



# Clear console
cat("\014")
