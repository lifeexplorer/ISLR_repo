# Environment setting up --------------------------
library(tidyverse)
library(ISLR) # for obtain the data set called Weekly
library(corrplot) # for visualising the correlation plot
library(magrittr)
library(MASS) # for LDA and QDA function
library(class) # for the KNN function
library(GGally) # beautiful generalised scatter plot (extension to ggplot2)

# Exercise 10 -----------------------

# EDA --------------------------

# Note:
# the business meaning of the predictors are not clear, 
# but I would accept them from now on and focus on the technical modelling side

# Data Introduction:
# 6 predictors, 2 response
# Lag1 is the percentage return of previous trading week, Lag2 is for the week before last week.
# Predictor Volume is the number of shares traded on the previous week;
# Response 1 Today is the percentage return on the date of the week in question.
# Response 2 Direction is the market up or down.

names(Weekly) 
dim(Weekly)
sum(is.na(Weekly))

# Method:
# 1. Based on the assumption of two model, performing the EDA to comfirm those assumptions; 
# 2. Search online material on EDA for classification;
# 3. Read EDA material from John Hopskin Unviersity.


plot(Weekly, col = Weekly$Direction)
# Assumption of logistic model: the last row of the scatter plot does not indicate strong linear relation
# Assumption of LDA/QDA model: the Lag series predictors do look like Gaussian.
summary(Weekly)

select(Weekly, Year, starts_with('Lag'), Volume) %>% cor() %>% corrplot(., method = 'circle')
# the relatively strong correlation apprears between Volume and Year, so plot the Volume
Weekly %$% plot(Volume)

# the volume keeps increasing exponentially in the first few years, then dropped down in. 
# It is probably because of the financial crisis between 2008-2009
ts(Weekly$Volume, start = c(1990, 4), frequency = 52) %>% plot


select(Weekly, Year, starts_with("Lag"), Volumn, Today)
# boxplot for each predictor (except year) with direction as the separation class ()
gather(Weekly, Lag1, Lag2, Lag3, Lag4, Lag5, Volume, key = 'type', value = 'value') %>% 
  ggplot(., aes(x = Direction, y = value)) + geom_boxplot() + 
  facet_wrap(~type, scales = 'free')
# time series on the direction (inc. Year predictor)
ts(Weekly$Direction, start = c(1990, 4), frequency = 52) %>% plot # optimisation needed
# 




# Logistic regression ---------------------

# fit the model 
# (N.B. the first response level is denoted as the failure)
weekly.lrm <- glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
                  family = binomial, data = Weekly)
summary(weekly.lrm)

# produce the predicted probability on the training data-set
weekly.lrm.prob <- predict.glm(object = weekly.lrm, newdata = Weekly, type = 'response')
# the type argument: 
# 1. selecting "link" gives the value of linking function
# 2. selecting 'response' gives the value of probability, in the context of logistic regression
# 3. selecting 'terms' (don't understand this part)

# decision boundary
weekly.lrm.pred <- if_else(weekly.lrm.prob > 0.5, "Up", "Down")

table(weekly.lrm.pred, Weekly$Direction)
# Since the data set relatively balanced, the accuracy could be chosen as the performance metrics
accuracy.lrm <- sum(weekly.lrm.pred == Weekly$Direction)/nrow(Weekly)
accuracy.lrm # 56.10%
# When the lrm indicates the market is going down, 52.9% of chances the market laterally going down
54/(54+48)
# When the lrm indicates the market going up, 43.56% of chances the market laterally going up.
430/(430+557)


# training the model with hold-out set 
weekly.train <- filter(Weekly, Year <= 2008)
weekly.test <- filter(Weekly, Year > 2008)

weekly.lrm.train <- glm(formula = Direction ~ Lag2, family = binomial, data = weekly.train)
summary(weekly.lrm.train)

weekly.lrm.test.prob <- predict.glm(object = weekly.lrm.train, newdata = weekly.test, type = 'response') # type = response returns the probability
weekly.lrm.test.pred <- if_else(weekly.lrm.test.prob > 0.5, 'Up', 'Down')

# Confusion matrix
table(weekly.lrm.test.pred, weekly.test$Direction)
# Accuracy = 62.5%
mean(weekly.lrm.test.pred == weekly.test$Direction)





# Discrimiant analysis --------------

# fitting LDA with hold-out set
weekly.lda.train <- lda(formula = Direction ~ Lag2, data = weekly.train)
weekly.lda.test <- predict(weekly.lda.train, weekly.test)
# confusion matrix
table(weekly.lda.test$class, weekly.test$Direction)
# accuracy = 62.50%
mean(weekly.lda.test$class == weekly.test$Direction)


# fitting QDA with hold-out set
weekly.qda.train <- qda(formula = Direction ~ Lag2, data = weekly.train)
weekly.qda.test <- predict(weekly.qda.train, weekly.test)
# confusion matrix
table(weekly.qda.test$class, weekly.test$Direction)
# accuracy 58.65%
mean(weekly.qda.test$class == weekly.test$Direction)

# KNN method -------------------

weekly.knn.train <- dplyr::select(weekly.train, Lag2)
weekly.knn.train.cl <- weekly.train$Direction
weekly.knn.test <- dplyr::select(weekly.test, Lag2)
weekly.knn.pred <- class::knn(train = weekly.knn.train, test = weekly.knn.test, 
                              cl = weekly.knn.train.cl, k = 1)

# confusion matrix
table(weekly.knn.pred, weekly.test$Direction)
# accuracy 50.96%
mean(weekly.knn.pred == weekly.test$Direction)


# Experiments with transformations and interactions of predictors ---------------
# This part is suspended as it required 

plot(Weekly, col = Weekly$Direction)








# To-do list for Exercise 10 ---------------------------

# First priority To-Do:
# 1. revise the LDA&QDA estimator 
# 2. Q: understand the details of KNN mechanism (especially the existance of training and test)
#    A: There is only one phase, which combining the training and test in one go.


# Secondary To-Do
# ToDO: In order to bespoke the binary classifier assessment method, 
#       for instance, the trading strategy requires different set of metrics as those in the fraud detection
#       check the performance review on classifier website. (In the context of trading strategy)
# 



# Exercise 11 -----------------------
# response gas mileage

# 11 (a) -------
# binarification of the response into higher than the median and lower than the median, 
# where the high is indicated by number 1.
Auto.dat <- mutate(Auto, mpg01 = if_else(mpg > median(mpg), 1, 0) %>% as.factor())

# 11 (b) -------

### Data Overview ###
## Predictors:
#     Ordinal: cylinders, year
#      nomial: origin, name
#     numeric: displacement, horsepower, weight, accelerate

## Response: mpg (numeric), mpg01 (binarifiction)

# Visual EDA 
dplyr::select(Auto.dat, -name) %>% plot(., col = Auto.dat$mpg01)

# Name: (ignored currently, due to lack of NLP)

###### Ordinal predictors 
# year: exhibits a positive linear relation
# Generally speaking, mpg changes along with the time, predictor year.
# Since the mpg01 is generated by comparing the median, 
# the higher percentage of high mpg car is expected in later years.
plot(jitter(Auto.dat$year), Auto.dat$mpg, col = Auto.dat$mpg01)
abline(h = median(Auto.dat$mpg), col = 'orange')
boxplot(year ~ mpg01, data = Auto.dat)

# cylinders: exhibits a general negative trend with small perturbation
# the percentage of high mpg car concentrated for 4 clinders car.
Auto.dat %$% plot(jitter(cylinders), mpg, col = mpg01) # jitter process on x-axis
abline(h = median(Auto.dat$mpg), col = 'orange')
boxplot(cylinders ~ mpg01, data = Auto.dat)


###### numeric predictors 
# 
dplyr::select(Auto.dat, displacement, horsepower, weight, acceleration, mpg01) %>% 
  ggpairs()



# Keep going on the EDA for other types of predictors






# 11 (c) -------------

# split the training and test set by random splitting into 70% and 30%
set.seed(2)
Auto.index <- sample(2, nrow(Auto.dat), replace = TRUE, prob = c(0.7, 0.3))
Auto.train <- Auto.dat[Auto.index == 1, ]
Auto.test <- Auto.dat[Auto.index == 2, ]

# 11 (d) ----------------

# LDA requires numeric predictors 
Auto.lda <- lda(formula = mpg01 ~ displacement + horsepower + weight + acceleration, 
                data = Auto.train)

# 









# 12 (a) ----------------



# 12 (b) ----------------


