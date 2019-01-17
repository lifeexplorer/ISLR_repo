
# Practice one --------------
# load the stock market data
Smarket <- ISLR::Smarket %>% as.tibble()

# fit the data into logistic regression
glm.fits <- glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial) 
summary(glm.fits)

# predict probability on the training set
glm.probs <- predict(glm.fits, type = 'response')
# first 10 probability
glm.probs[1:10]
# print out the dummie variable for the response in R
contrasts(Smarket$Direction)
# convert the probability into categorical response (up or down) with threshold at 0.5
glm.pred <- if_else(glm.probs > .5, "Up", "Down")

# Confusion matrix
table(glm.pred, Smarket$Direction)


# fit the model on train and test (forecasting style hold-out set) ----------------
train <- Smarket$Year < 2005
glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial, subset = train)
# predict the probability on the test set
glm.probs <- predict(glm.fits, Smarket[!train, ], type = 'response')
# 
glm.pred <- if_else(glm.probs > .5, "Up", "Down")
# Confusion matrix
table(glm.pred, Smarket[!train, ]$Direction)
# accuracy
mean(glm.pred == Smarket[!train, ]$Direction)

# Remove insignificant variable to reduce the variance without bias decresing, in order to achieve high performance on test set
glm.fits <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fits, Smarket[!train, ], type = 'response')
glm.pred <- if_else(glm.probs > .5, "Up", "Down")
# Confusion matrix
table(glm.pred, Smarket[!train, ]$Direction)
# accuracy
mean(glm.pred == Smarket[!train, ]$Direction)


# predict on the newdata
predict(glm.fits, newdata = tibble(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)), type = 'response')


# LDA model on the stock market data ----------------------
library(MASS) # the LDA function is in the MASS package
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.pred <- predict(lda.fit, Smarket[!train, ])
table(lda.pred$class, Smarket[!train, ]$Direction)
# accuracy
mean(lda.pred$class == Smarket[!train, ]$Direction)

# QDA model on the stock market data ----------------------
library(MASS)
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.pred <- predict(qda.fit, Smarket[!train, ])
table(qda.pred$class, Smarket[!train, ]$Direction)
# accuracy
mean(qda.pred$class == Smarket[!train, ]$Direction)

# KNN model on the stock market data ----------------------
library(class) # it involves the knn 
train.x <- dplyr::select(Smarket, Lag1, Lag2)[train, ]
test.x <- dplyr::select(Smarket, Lag1, Lag2)[!train, ]
train.Direction <- Smarket$Direction[train]
# set 
set.seed(1)
knn.pred <- knn(train = train.x, test = test.x, cl = train.Direction, k = 3)
table(knn.pred, Smarket[!train, ]$Direction)
# accuracy
mean(knn.pred == Smarket[!train, ]$Direction)

# search for the optimal number of neighbour points for test accuracy
accuracy <- c()
for (i in 1:20) {
  knn.pred <- knn(train = train.x, test = test.x, cl = train.Direction, k = i)
  accuracy[i] <- mean(knn.pred == Smarket[!train, ]$Direction)
}
plot(x = 1:20, y = accuracy, type = 'l', xlab = 'Number of neighbour', ylab = 'Accuracy on test set')


# KNN model on the Caravan insurance data ----------------
Caravan <- ISLR::Caravan
# since the KNN is based on the distance measure, it is essential to standardisation the 
standardised.x = scale(Caravan[, -86])
# split the data into training and testing
train.x <- tail(standardised.x, 4822)
test.x <- head(standardised.x, 1000)
train.y <- tail(Caravan$Purchase, 4822)
test.y <- head(Caravan$Purchase, 1000)
set.seed(1)
knn.pred <- knn(train.x, test.x, train.y, k = 1)
# confusion matrix
table(knn.pred, test.y)
# error rate
mean(test.y != knn.pred)
# accuracy
mean(test.y == knn.pred)
# precision
sum((test.y == knn.pred)&(knn.pred == "Yes"))/sum(knn.pred == 'Yes')

# logistic regression on the Caravan insurance data -----------
test <- 1:1000
train <- tail(1:5822, -1000)
glm.fits <- glm(Purchase ~ ., data = Caravan, family = binomial, subset = train)
glm.probs <- predict(glm.fits, newdata = Caravan[test, ], type = 'response')
glm.pred <- if_else(glm.probs > .25, "Yes", "No")
table(glm.pred, test.y)

precision

