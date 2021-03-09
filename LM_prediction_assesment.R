library(tidyverse)
library(caret)

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))


######  Q1

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later

models_rmse <- replicate(n, {
  #split data into 2 parts
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  
  test_size <- nrow(test_set)
  
  #train the model
  fit <- lm(y ~ x, data = train_set)
  #predict outcomes
  y_hat <- predict(fit, test_set)
  
  #calculate RMSE
  rmse <- sqrt(sum((test_set$y - y_hat)^2/test_size))
})

mean(models_rmse)
sd(models_rmse)

######  Q2

different_size_dataset <- function(n_value){
  n <- n_value
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n_value, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  
  models_rmse <- replicate(100, {
    #split data into 2 parts
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    
    test_size <- nrow(test_set)
    
    #train the model
    fit <- lm(y ~ x, data = train_set)
    #predict outcomes
    y_hat <- predict(fit, test_set)
    
    #calculate RMSE
    rmse <- sqrt(sum((test_set$y - y_hat)^2/test_size))
  })
  
  c(mean(models_rmse), sd(models_rmse))
}

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n_values <- c(100, 500, 1000, 5000, 10000)
results <- sapply(n_values, different_size_dataset)
results


######  Q4

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
models_rmse <- replicate(n, {
  #split data into 2 parts
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  
  test_size <- nrow(test_set)
  
  #train the model
  fit <- lm(y ~ x, data = train_set)
  #predict outcomes
  y_hat <- predict(fit, test_set)
  
  #calculate RMSE
  rmse <- sqrt(sum((test_set$y - y_hat)^2/test_size))
})

mean(models_rmse)
sd(models_rmse)


######  Q6 & Q8 (now Q8)

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

test_size <- nrow(test_set)

#train the model
fit <- lm(y ~ x_1, data = train_set)
#predict outcomes
y_hat <- predict(fit, test_set)

#calculate RMSE
rmse <- sqrt(sum((test_set$y - y_hat)^2/test_size))
rmse