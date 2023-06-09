#Read data
howell <- read.csv("G4_howell.csv" , na.strings = c(""))
howell
# Get and print current working directory.
print(getwd())
#Check 
print(is.data.frame(howell))
#View data on R environment.
View(howell)
#Number of rows
nrow(howell)
#Number of col
ncol(howell)
#Column names.
colnames(howell)
#Row names.
rownames(howell)
#Data structure.
str(howell)
#Data summary.
#Get first 6 rows
head(howell)
#Get last 6 rows
tail(howell)
#Get first N rows
head(howell , 20)
#Get last N rows
tail(howell , 20)
#Remove some columns
howell[ ,- c(1,2)]
#Select specific data
spec <- howell[2, "age"] #---> using name of col
spec
spec <- howell[2,2] #---> using num of col 
spec
#Get a specific row
spec <- howell[1,]
spec
#Get a specific col
spec <- head(howell[ , 2]) #first 6 rows for col 2 --> int
spec
spec <- howell[,2] #int
spec
spec <- howell[["age"]] #int
spec
spec <- howell[,2] #int
spec
spec <- howell[ "age" ] #as data frame
spec
#Get specific rows and column
Spec <- head(howell[ c(1:10), c("age", "sex" ,"height")])
Spec
#Filter data 
sex <- howell[howell$sex=="M",]
sex
age <- howell[howell$age <90 & howell$age > 45 ,]
age
mix <- howell[howell$sex == "M"& howell$age > 50 ,c(1, 2)]
mix
#Sorting data according to specific column
sorting <- howell[order(-howell$age) ,] #increasing
sorting
sorting <- howell[order(howell$age) ,]  #decreasing
sorting
#Add new column
howell$color <- c("A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" ,"D","A","B","C" )
howell
#Delete a column
howell$color <- NULL
howell
#Re-coding
howell$new_sex[howell$sex =="M"] = 1
howell$new_sex[howell$sex =="F"]= 2
howell
howell$Overweight[howell$weight > 50] = T
howell$Overweight[howell$weight < 50] = F
#Re-code with multicondation
howell$new_height[howell$height <  120] = "Low"
howell$new_height[howell$height >= 120 & howell$height <  135] = "Medium"
howell$new_height[howell$height >= 135] = "High"
howell
#Max
max_age <- max(howell$age)
max_age 
max_height <- max(howell$height)
max_height
#Min
min_age <- min(howell$age)
min_age 
min_height <- min(howell$height)
min_height
#Mean
mean_age <- mean(howell$age)
mean_age 
mean_height <- mean(howell$height)
mean_height
#Sum
sum_age <- sum(howell$age)
sum_age 
sum_height <- sum(howell$height)
sum_height
#Removing and Convert text in numeric values
howell$weight<-as.numeric(gsub("kg","", howell$weight))
howell$sex<-as.factor((howell$sex))
howell$Overweight <- NULL
summary(howell)
#Location ofNA
complete.cases(howell)
#Using Mice
library(mice)
imputation <- mice(howell, m = 10, meth = c('', '', 'pmm', ''), maxit = 20)
new_howell <- complete(imputation, 10)
summary(new_howell[c("age", "weight", "height")])
table(new_howell$sex)
aggregate(cbind(weight, height) ~ sex, data = new_howell, mean)
cor(new_df$weight, new_howell$height)
aggregate(age ~ sex, data = new_howell, mean)
summary(new_howell[c("age", "weight", "height")])
#Data Visualization
library(ggplot2)
library(tidyverse)
#Histogram
draw_hist <- ggplot(howell, aes(age))
draw_hist +
  geom_histogram(binwidth = 3 , color="green" ,fill = "darkslategray",alpha = .6) +
  ggtitle("The Age ")+
  labs(y = "Probability" , x = "Age" )
#Barchart
draw_bar<- ggplot(howell, aes(x = sex, fill=age))
draw_bar +
  geom_bar() + theme_light() +
  labs(y = "Count", title = "Statistic")
#Scatter plot
draw_sc <- ggplot(howell, aes(x = height, y = age))
draw_sc + 
  geom_point() + 
  theme_light()+
  stat_smooth(se = FALSE) +
  labs(x = " Lenght", y = "weight", title = " Length and Height Scatter")









