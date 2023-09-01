
#retrieving that data
setwd("~/Desktop/R math 167r")
project.data <- read.csv("healthcare-dataset-stroke-data.csv", header = TRUE, na.strings = "N/A")
project.data
#calling ggplot 
library(ggplot2)
library(lmt)
#data set summary
summary(project.data)
#checking the correlations between bmi and average glucose level
cor(project.data[,9], project.data[,10], use = "complete.obs")
project.data[,9]




#checking basic bar graphs to see the proportions of the whole data set
hist(project.data[,3], main = paste("Histogram of Age"), xlab = 'age')
ggplot(project.data, aes(smoking.status)) + geom_bar()
ggplot(project.data, aes(gender)) +geom_bar()
ggplot(project.data, aes(work.type)) +geom_bar()
ggplot(project.data, aes(residence.type))+geom_bar()
ggplot(project.data, aes(as.factor(stroke)))+geom_bar()

#looking at the age people tend to have strokes
ggplot(project.data, aes(y =age, as.factor(stroke))) + geom_point()
#seeing if gender and age impact stroke risk
ggplot(project.data, aes(y =age, as.factor(stroke), color = gender)) + geom_boxplot()
#looking at what bmi people tend to have strokes
ggplot(project.data, aes(y = na.omit(bmi), x = as.factor(stroke)))+geom_point()
#looking at what average glucose level people have strokes at
ggplot(project.data, aes(y = avg_glucose_level, x = as.factor(stroke)))+geom_point()
#looking at what glucose and bmi level people have and separating by whether or not they had a stroke
ggplot(project.data, aes(y = avg_glucose_level, x = bmi))+geom_point(aes(color = as.factor(stroke)))

#looking at heart disease factor
ggplot(project.data, aes(as.factor(heart_disease)))+geom_bar(aes(fill = as.factor(stroke)))
#looking at what bi people have when they have heart disease
ggplot(project.data, aes(x = as.factor(heart_disease), y = bmi))+geom_point()
#looking at hypertension factor
ggplot(project.data, aes(x = as.factor(hypertension)))+geom_bar(aes(fill = as.factor(stroke)))


library(dplyr)
#filtering the data set by who had strokes
df2 <- filter(project.data, stroke == 1)
df2$avg_glucose_level
df2
#looking at the proportion of glucose levels in patients that had a stroke
ggplot(df2, aes(y = avg_glucose_level, x = as.factor(stroke)))+geom_boxplot(aes(color = gender))
#looking at bmi vs glucose color coded by whether or not they had heart disease
ggplot(df2, aes(y = avg_glucose_level, x = bmi))+geom_point(aes(color = as.factor(heart_disease)))
#bmi vs glucose color coded by whether or not they had hyper tension
ggplot(df2, aes(y = avg_glucose_level, x = bmi))+geom_point(aes(color = as.factor(hypertension)))
#looking at what proportions of patients that had a stroke had both heart disease and hypertension
ggplot(df2, aes(x = as.factor(hypertension)))+geom_bar(aes(fill = as.factor(heart_disease)))
#seeing the proportions of people that had a stroke and their smoking status, if they were ever married, their work type, and their gender
ggplot(df2, aes(x = as.factor(smoking_status)))+geom_bar()
ggplot(df2, aes(x = as.factor(ever_married)))+geom_bar()
ggplot(df2, aes(x = as.factor(work_type)))+geom_bar()
ggplot(df2, aes(x = as.factor(gender)))+geom_bar()
#overall summary of the data with patients that had a stroke
summary(df2)

#doing a t test on whether or not the bmi of stroke patients are different to the normal bmi range
t.test(df2$bmi, mu = 24.9, alternative = "greater", conf.level = 0.95)
#t test on whether or not the average age of a person who had a stroke is different to the average of stroke patients in general
t.test(df2$age, mu = 65, alternative = "less", conf.level = 0.95)


#filtering the the data of people who didn't have a stroke
df3 <- filter(project.data, stroke ==0)
df3
#filtering those who had a stroke and are male
dfmale <- filter(df2, gender == 'Male')
dfmale
#filtering those that had a stroke and are female
dffemale <- filter(df2, gender == 'Female')
dffemale
#t test on those that had a stroke and if their gender produces different average ages when having a stroke
t.test(dfmale$age, dffemale$age, alternative = "less", conf.level = 0.95)
#t test on whether or not the average glucose level between those that had a stroke
#and those who did not are similar
t.test(df2$avg_glucose_level, df3$avg_glucose_level, alternative = "greater", conf.level = 0.95)

# Boxplots on average glucose level for patients based on work_type #
p<-ggplot(data=project.data, aes(x=as.factor(stroke), y= avg_glucose_level, color= work_type))
p+geom_boxplot()

# One sample t-test on BMI #

df2<- filter(project.data, stroke == 1)
t.test(df2$bmi, alternative= "greater", mu= 24.9, conf.level= 0.95)

# Two sample t-test on average glucose level between groups #

df3<- filter(project, stroke== 0)
t.test(df2$avg_glucose_level, df3$avg_glucose_level, alternative= "greater", conf.level=0.95)
