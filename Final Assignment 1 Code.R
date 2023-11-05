rm(list=ls())

setwd("~/Fall_2023/dataScience/Assignment_1")
load("titanic_train.RDATA")
data = titanic.train

if(!require("ggplot2"))
  install.packages("ggplot2") 
if(!require("vcd"))
  install.packages("vcd") 
if(!require("scales"))
  install.packages("scales") 
if(!require("tidyverse"))
  install.packages("tidyverse") 
if(!require("dplyr"))
  install.packages("dplyr") 

library(vcd)
library(ggplot2)
library(scales)
library(tidyverse)
library(dplyr)

#change categorical variables to factors
survival = as.factor(data$Survived)
pclass = as.factor(data$Pclass)
sex = as.factor(data$Sex)
embarked = as.factor(data$Embarked)

#totals and ratios of the categorical variable factors
#0 signifies no survival
#1 signifies survival
tab_survival = table(survival) 
ratio_sur = prop.table(tab_survival)

#1st, 2nd, and 3rd class tickets
tab_pclass = table(pclass)
ratio_pclass = prop.table(tab_pclass)

#female or male
tab_sex = table(sex)
ratio_sex = prop.table(tab_sex)

#Port of Embarkation (C = Cherbourg, Q = Queenstown, S = Southhampton)
tab_embarked = table(embarked)
ratio_embarked = prop.table(tab_embarked)

#identify summary information for numerical variables
age = summary(data$Age)
sibsp = summary(data$SibSp)
parch = summary(data$Parch)
ticket = summary(data$Ticket)
fare = summary(data$Fare)

#indices of those who survived and did not survive
auxS = which(data$Survived == 1)
auxD = which(data$Survived == 0)

#most of the data does not have information on cabins, so it will not be that useful
cabin = summary(data$Cabin)

#Q1 Did passengers from a particular ticket class have a higher likelihood of 
#survival compared to others? How did survival rates vary by ticket class?
#Observation: the more money money spent on fare, the more likely there was a 
#cabin assigned and that cabin would be within the A-D sections and the ticket 
#would like have "PC" in front of it

#out of the passengers that survived, what were their different classes
#create a bar graph demonstrating the data of each of the 3 classes
df_class = data.frame(Class = names(ratio_pclass*100), Percentage = as.numeric(ratio_pclass*100))
ggplot(data = df_class, aes(x = Class, y = Percentage, fill = Class)) +
  geom_bar(stat = "identity", position = "dodge")+
  labs(
    title = "Percentage of Survivors by Ticket Class",
    x = "Ticket Class",
    y = "Percentage (%)"
  )+
  theme(plot.title = element_text(face = "bold" ,size = 16, hjust = 0.5)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 4)

# plot 시각화
table2 <- table(ifelse(data$Survived == 1, "survived", "dead"), data$Pclass)
table2_percent <- prop.table(table2, margin = 1) * 100  # 행 기준으로 정규화 

# 테이블 출력
print(table2_percent)

# 테이블을 데이터프레임으로 변환
table_df <- as.data.frame(table2_percent)

# Correlation between Pclass and Survival
ggplot(table_df, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), position = position_stack(vjust = 0.5)) +
  labs(title = "Correlation between Pclass and Survival", x = "Pclass", y = "Percentage") +
  scale_fill_manual(values = c("dead" = "#f6bbbd", "survived" = "#00b9ef")) +
  theme_minimal()

#When the Titanic sank, it was said that women, the elderly, and children were
#prioritized, but the high survival rate of women proves that it makes some sense.

#Survival by gender_
ggplot(data, aes(x = factor(Survived), fill = Sex)) +
  geom_bar(position = "dodge", width = 0.6, color = "white") +
  geom_text(aes(label = stat(count)), 
            stat = "count", 
            position = position_dodge(width = 0.6), 
            vjust = -0.5, 
            size = 3, 
            color = "black") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(title = "Survival by Gender", x = "Survived", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("female" = "#f6bbbd", "male" = "#00b9ef"))

#Q3a Did age play a role in survival? Were children more more likely to survive, 
#or did age not significantly impact survival rates? 

#- i want to create a graph that should the 
#different age groups. This will be 2 different data sets. If they survived it 
#will be dark blue, but if they did not it will be dark green.

table(titanic.train$Age, titanic.train$Survived)

#takes the rows of data of the people who survived
#from the people that survived, shows the age across the x axis
ggplot(data=data[auxS,])+ 
  aes(x=Age, fill = survival)+ 
  geom_histogram(bins=floor(sqrt(nrow(data))),fill="darkblue",
                 color="white",
                 aes(y=after_stat(density))) +
  geom_histogram(data=data[auxD,], 
                 bins=floor(sqrt(nrow(data))),
                 fill="darkgreen",
                 color="black",
                 aes(y=-after_stat(density))) + labs(x="Age",y="did NOT survive / did survive")+
  ggtitle("Age vs. Survival")+
  theme(plot.title = element_text(face = "bold" ,size = 16, hjust = 0.5))

#3b boxplot
ggplot(data, aes(x = Survived, y = Age, fill = Survived)) +
  geom_boxplot() +
  labs(title = "Age Distribution by Survival", x = "Survived", y = "Age") +
  scale_fill_manual(values = c("red", "green")) +
  theme_minimal()

#3c jitterplot
ggplot(data, aes(x = factor(Survived), y = Age)) +
  geom_jitter(width = 0.3, height = 0, color = "black", size = 3) +
  labs(title = "Jitter Plot of Age by Survival", x = "Survived", y = "Age") +
  theme_minimal()

#4 scatter plot
ggplot(data, aes(x = Age, y = Fare, color = Survived, shape = Sex)) +
  geom_point() +
  geom_jitter() +
  labs(title = "Scatter Plot of Age and Fare", x = "Age", y = "Fare") +
  scale_color_manual(values = c("0" = "#ec6d71", "1" = "#69bfcd" )) +
  scale_shape_manual(values = c("female" = 3, "male" = 16))

#5 Did the survival rate vary for passengers of different ages within each 
#ticket class?
ggplot(data, aes(y = pclass, x = Age)) +
  geom_boxplot(aes(fill = pclass)) +
  facet_grid(Survived~.) +
  labs(
    title = "Age Based on Passengers' Class, Separated by Survival",
    x = "Age",
    fill = "Class"
  ) +
  theme_minimal() + theme(axis.title.y = element_blank())+
  theme(plot.title = element_text(face = "bold" ,size = 16, hjust = 0.5))

#6 Survival by Family Size on the Titanic
data['Family'] =data['SibSp'] + data['Parch'] + 1
ggplot(data, aes(x = Family, fill = factor(Survived))) +
  geom_bar(position = 'dodge') +
  labs(title = 'Survival by Family Size on the Titanic',
       x = 'Family Size',
       y = 'Count') +
  scale_fill_discrete(name = 'Survived', labels = c('No', 'Yes'))+
  scale_x_continuous(breaks = seq(min(data$Family), max(data$Family), by = 1))

#7
# Were there any interesting interactions or correlations between the 
#variables that were not evident from individual analysis, such as the combined
#effect of ticket class, gender, and fare on survival?

ggplot(data, aes(x = pclass, y = Fare, fill = Sex)) +
  geom_boxplot() +
  facet_grid(. ~ Survived) +
  labs(
    title = "Fare Based on Passengers' Class and Sex, Separated by Survival",
    x = "Class",
    y = "Fare, USD ($)"
  ) +
  theme_minimal()+
  theme(plot.title = element_text(face = "bold" ,size = 16, hjust = 0.5))

#8
#Survival Count by Embarked
titanic.train <- titanic.train %>%
  mutate(
    Embarked = recode(Embarked, 'C' = 'Cherbourg'),
    Embarked = recode(Embarked, 'Q' = 'Queenstown'),
    Embarked = recode(Embarked, 'S' = 'Southampton'),
  )

ggplot(titanic.train, aes(x = Embarked, fill = factor(Survived))) +
  geom_bar(position = 'dodge') +
  geom_text(
    aes(label = ..count..),
    stat = 'count',
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3,
    color = 'black'
  ) +
  labs(title = 'Survival Count by Embarked') +
  scale_fill_manual(values = c('0' = '#f6bbbd', '1' = '#00b9ef'), name = 'Survived') +
  xlab('Embarked') +
  ylab('Count')

ggplot(titanic.train, aes(x = Embarked, fill = factor(Survived))) +
  geom_bar(position = 'dodge') +
  geom_text(
    aes(label = ..count..),
    stat = 'count',
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 1.9,
    color = 'black'
  ) +
  labs(title = 'Survival Count by Embarked location and Gender') +
  scale_fill_manual(values = c('0' = '#f6bbbd', '1' = '#00b9ef'), name = 'Survived') +
  xlab('Embarked') +
  ylab('Count') +
  facet_grid(Sex ~ .,margins=TRUE)

#######################################################################################################
#요금제 밀도 그래프 FARE GRAPH
ggplot(data, aes(x=Fare)) + geom_density(fill="blue")
#There were many passengers with low fares

#age 
ggplot(data, aes(x=Age)) + geom_density(fill="blue")
#There were many passengers in 20~40 aged.It can be seen that a passenger of a young age,
#believed to be their children, was on board. 

#ggplot_fare(data, aes(x = Fare, y=..density..), )

#"correlation sex and survived"
#table1 <- table(ifelse(data$Survived==1,"survived","dead"),data$Sex)
#mosaicplot(table1, color=TRUE, main= "correlation sex and survived")
#women have a high survival rate


#"correlation Pclass and survived"
#table2 <- table(ifelse(data$Survived==1,"survived","dead"),data$Pclass)
#mosaicplot(table2, color=TRUE, main= "correlation Pclass and survived")
#Among the surviving passengers, the proportion of passengers on first-class tickets is high.
#On the other hand, most passengers who did not survive are passengers on third-class tickets.

boxplot(Age ~ Survived,data , xlab="survived")

plot(Age ~ jitter(Survived),data, cex=1.2, xlab="survived")

#(data, aes(Age, log(Fare) , color = factor(Survived), shape = factor(Sex))) + geom_point() + geom_jitter()