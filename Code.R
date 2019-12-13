library(randomForest)
library(caret)
house.df <- read.csv("HousingData.csv")

# Assign column names to housing.df
#names(housing.df) <- names2

summary(house.df)

housing.df <- house.df

#Replace the missing values (NA's) from the variables with their respective median values
housing.df$CRIM[which(is.na(housing.df$CRIM))] <- median(housing.df$CRIM, na.rm= T)
housing.df$ZN[which(is.na(housing.df$ZN))] <- median(housing.df$ZN, na.rm= T)
housing.df$INDUS[which(is.na(housing.df$INDUS))] <- median(housing.df$INDUS, na.rm= T)
housing.df$CHAS[which(is.na(housing.df$CHAS))] <- median(housing.df$CHAS, na.rm= T)
housing.df$AGE[which(is.na(housing.df$AGE))] <- median(housing.df$AGE, na.rm= T)
housing.df$LSTAT[which(is.na(housing.df$LSTAT))] <- median(housing.df$LSTAT, na.rm= T)
summary(housing.df)

suppressMessages(library(caret))
cor(housing.df,housing.df$MEDV)

#correlation analysis
#boston_cor <- cor(boston_new) #All 14 variables
#corrplot(boston_cor, method = "circle") 

# Centering/scaling of input features
house.scale <- cbind(scale(housing.df[1:13]), housing.df[14])

set.seed(12345)
#Do data partitioning
inTrain <- createDataPartition(y = house.scale$MEDV, p = 0.70, list = FALSE)
training <- house.scale[inTrain,]
testing <- house.scale[-inTrain,]

#print(nrow(training))
#--------------------------MODEL 1------------------------------#
set.seed(12345)
#Try linear model using all features
fit.lm <- lm(MEDV~.,data = training)
#check cooeffs
data.frame(coef = round(fit.lm$coefficients,2))

set.seed(12345)
#predict on test set
pred.lm <- predict(fit.lm, newdata = testing)

# Root-mean squared error
rmse.lm <- sqrt(sum((pred.lm - testing$MEDV)^2)/length(testing$MEDV))
c(RMSE = rmse.lm, R2 = summary(fit.lm)$r.squared)

#png(file = "price1.png")
#plot(pred.lm,testing$MEDV,main = "Model 1", xlab = "Predicted Price", ylab = "Actual Price", pch = 3)
#dev.off()

#--------------------------MODEL 2----------------------------#
set.seed(12345)
#Try linear model using all features
fit.lm1 <- lm(log(MEDV)~.,data = training)

set.seed(12345)
#predict on test set
pred.lm1 <- predict(fit.lm1, newdata = testing)

# Root-mean squared error
rmse.lm1 <- sqrt(sum((exp(pred.lm1) - testing$MEDV)^2)/
                   length(testing$MEDV))

c(RMSE = rmse.lm1, R2 = summary(fit.lm1)$r.squared)

#png(file = "price2.png")
#plot(pred.lm1,testing$MEDV, main = "Model 2", xlab = "Predicted Price", ylab = "Actual Price", pch = 3)
#dev.off()

#----------------------MODEL 3--------------------------#
suppressMessages(library(randomForest))
set.seed(12345)
fit.rf <- randomForest(formula = MEDV ~ ., data = training)

set.seed(12345)
pred.rf <- predict(fit.rf, testing)

rmse.rf <- sqrt(sum(((pred.rf) - testing$MEDV)^2)/
                  length(testing$MEDV))
c(RMSE = rmse.rf, pseudoR2 = mean(fit.rf$rsq))

#png(file = "price3.png")
#plot(pred.rf,testing$MEDV,main = "Random Forest", xlab = "Predicted Price", ylab = "Actual Price", pch = 3)
#dev.off()