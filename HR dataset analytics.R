---
title: "HR Data analyti"
output: html_document
date: "2023-05-14"
---
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lmtest)
#import dataset
#df <- read_csv("flat_file.csv")
df <- read.csv(file.choose(), header=T)
df <- df[, -c(1:2)]
head(df)
summary(df)

#Exploratory data analysis
df$ln_salary <- log(df$Salary) #because of high volatility of salary, then i compress the salary variable using log(salary) named ln_salary
#Check for null values and print out variables that contain null
null_values <- colSums(is.na(df))
print(names(df)[null_values > 0])
#Create a new column to know whether the employee is still with the company or not
df$Terminated <- 0
df$Terminated[!is.na(df$DateofTermination)] <- 1
summary(df$Terminated)
#Change the performance score in to number 
# Define custom levels for the performance scores
levels <- c("PIP", "Needs Improvement", "Fully Meets", "Exceeds")
# Convert PerformanceScore column to a factor with custom levels
df$PerformanceScore <- factor(df$PerformanceScore, levels = levels)
# Convert the factor to numeric
df$PerformanceScore_numeric <- as.numeric(df$PerformanceScore)

#QUESTION 1: What, if anything, increases the number of sick days that have been taken over the past year?
prop_incharge <- mean(is.na(df$DateofTermination)) #percantage of remaining employees. 
#About two third of all employees are in charge. So the data set will be separate into 
#2 data sets, 1 for terminated staff and 1 for employees who are still with the company(is null value in "DateofTermination" variables).
hr_terminated <- df[!is.na(df$DateofTermination), ]
head(hr_terminated, 10)
hr_incharge <- df[is.na(df$DateofTermination), ]
head(hr_incharge, 10)

summary(hr_incharge$Absences)
hist(hr_incharge$Absences) 
boxplot(hr_incharge$Absences)
#Looking at the histogram, the majority of absences day fall into the range 5-10 and the histogram seems to right skewed.
is_numeric <- sapply(hr_incharge, is.numeric)
incharge_numeric <- hr_incharge[, is_numeric]
incharge_numeric <- incharge_numeric[, -c(1:2)]
correlation_matrix <- cor(incharge_numeric)
print(correlation_matrix) 
#the correlation matrix showing that special project count has the strongest relationship with absences day of employee. 
#check average absences 
ggplot(hr_incharge, aes(x = hr_incharge$Absences, fill = hr_incharge$Sex)) + 
  geom_histogram(position = "stack",color="black", alpha = 0.3, bins = 20) + 
  scale_fill_manual(values = c("blue", "pink")) + 
  labs(title = "Absence Days by Gender")

#OLS regression with independent variable = absences, predictors = log(Salary) + EmpSatisfaction + SpecialProjectsCount + Department(dummy)
hr_incharge$Department <- factor(hr_incharge$Department)

sick_days <- lm(Absences ~ ln_salary + EmpSatisfaction + SpecialProjectsCount + EngagementSurvey + Years_in_post, data = hr_incharge)
summary(sick_days)
#As it shown in above model, Special project count overall holds the highest impact on number of absences days in the company. 
#It is also the only one predictor statistically significant in the model. Therefore, employee who carrying out higher number of special projects have the tendency to absences more than normal staff. As it the slope coefficient of this variable is 0.7 

#To illustrate above model, a scatter plot and regression line are conducted.
ggplot(hr_incharge, aes(x=SpecialProjectsCount, y = Absences)) +
  geom_point() + # add the scatter plot points
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Special project count ~ absences", x = "Special projects", y = "Absences")

model <- aov(Absences ~ Department, data = hr_incharge)
summary(model)
#the model shows that there is a differences in absences day among departments as Pr(>F) = 6.11e-15 > 0.001
mean_absences <- aggregate(Absences ~ Department, data = df, FUN = mean)
ggplot(mean_absences, aes(x = reorder(Department, Absences, FUN = mean), y = Absences, group = 1)) +
  geom_line(color = "skyblue", size = 1) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Mean Absences by Department",
       x = "Department",
       y = "Mean Absences") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#QUESTION 2: What, if anything, increases the likelihood of someone leaving the organisation? 
#Anova test for difference in means in employee satisfaction 
model2 <- aov(Years_in_post ~ Terminated, data = df)
summary(model2)

# Lets visualize the difference in mean of year in post between employees
ggplot(df, aes(x = Terminated, y = Years_in_post, fill = Terminated)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  geom_text(stat = "summary", aes(label = round(..y.., digits = 2)), vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(x = "Terminated", y = "Mean of Working year") +
  ggtitle("Difference in Mean Performance Score by Termination Status")

t.test(SpecialProjectsCount ~ Terminated, data=df)
ggplot(df, aes(x = Terminated, y = SpecialProjectsCount, fill = Terminated)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  geom_text(stat = "summary", aes(label = round(..y.., digits = 2)), vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(x = "Terminated", y = "Special Projects Count") +
  ggtitle("Difference in average special projects taken by Termination Status")

t.test(PerformanceScore_numeric ~ Terminated, data=df)
ggplot(df, aes(x = Terminated, y = PerformanceScore_numeric, fill = Terminated)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  geom_text(stat = "summary", aes(label = round(..y.., digits = 2)), vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(x = "Terminated", y = "Mean of Performance Score") +
  ggtitle("Difference in Mean Performance Score by Termination Status")
