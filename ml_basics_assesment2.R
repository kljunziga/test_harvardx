library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type



dat %>% group_by(type) %>% summarize(prop_female = mean(sex=="Female"))


y_hat <- ifelse(x == "online", "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)


specificity(y_hat,y)

mean(y == "Female")

#assesment 2 start
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species


# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]


cutoff <- seq(6, 7, by = 0.1)
accuracy_l <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "versicolor", "virginica") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
max(accuracy_l)

cutoff <- seq(2, 3.5, by = 0.1)
accuracy_w <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "versicolor", "virginica") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
max(accuracy_w)

cutoff <- seq(0, 9, by = 0.1)
accuracy_pl <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
max(accuracy_pl)

cutoff <- seq(0, 10, by = 0.1)
accuracy_pw <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
max(accuracy_pw)

#calculating train accuracy
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)	



#testing accuracy on test data
best_cutoff <- cutoff[which.max(accuracy_pl)]
best_cutoff
y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
y_hat <- factor(y_hat)
mean(factor(test$Species) == factor(y_hat))

predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)


foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,3:4],2,foo)
sapply(predictions,max)	

rangedValues_l <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
predictions <- foo(train[,3])
cutoffs_l <- rangedValues_l[which(predictions==max(predictions))]
rangedValues_w <- seq(range(train[,4])[1],range(train[,4])[2],by=0.1)
predictions <- foo(train[,4])
cutoffs_w <- rangedValues_w[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs_l[1] | test[,4]>cutoffs_w[1],'virginica','versicolor')
mean(y_hat==test$Species)











