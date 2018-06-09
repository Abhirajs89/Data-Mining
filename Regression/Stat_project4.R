library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(corrgram) # Correlograms http://www.datavis.ca/papers/corrgram.pdf
library(car) #required for nearest neighbors
library(FNN) # nearest neighbors techniques
library(pROC) # to make ROC curve

wine_data <- read.csv("C:/Users/Ranjit/Desktop/Stats/winequality-red.csv", sep=";")
head(winequality.red)

summary(wine_data$quality)
table(wine_data$quality)
linear_quality = lm(quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=wine_data)
summary(linear_quality)

corrgram(wine_data, lower.panel=panel.shade, upper.panel=panel.ellipse)

linear_quality_1 = lm(quality ~ alcohol, data = wine_data)
summary(linear_quality_1)


linear_quality_4 = lm(quality ~ alcohol + volatile.acidity + citric.acid + sulphates, data = wine_data)
summary(linear_quality_4)

linear_quality.res = resid(linear_quality) # gets residuals
linear_quality_1.res = resid(linear_quality_1)
linear_quality_4.res = resid(linear_quality_4)

plot(wine_data$alcohol, linear_quality.res) # plot residuals against alcohol variable
points(wine_data$alcohol, linear_quality_1.res, col="red") # add the residuals for 1-dimension
points(wine_data$alcohol, linear_quality_4.res, col="blue") # add residuals for 4 dimension

plot(linear_quality_4)
vif(linear_quality_4)

anova(linear_quality_4)

linear_quality_5 = lm(quality ~ alcohol + volatile.acidity + sulphates, data = wine_data)
summary(linear_quality_5)
anova(linear_quality_5)
vif(linear_quality_5)

# Split data into training and validation samples
# We will use (train.size)% for training and (100-train.size)% for validation
set.seed(2018)
train.size <- 0.8 
train.index <- sample.int(length(wine_data$quality), round(length(wine_data$quality) * train.size))
train.sample <- wine_data[train.index,]
valid.sample <- wine_data[-train.index,]

### VIF, F-ratio and p-values say it is good, so no need to do anything else

##### Now evaluate the final linear model
#     Find all predicted values for both a training set and a validation set
train.sample$Pred.quality <- predict(linear_quality_5, 
                                     newdata = subset(train.sample, select=c(quality, alcohol, volatile.acidity, citric.acid, sulphates)))
valid.sample$Pred.quality <- predict(linear_quality_5, 
                                     newdata = subset(valid.sample, select=c(quality, alcohol, volatile.acidity, citric.acid, sulphates)))

# The theoretical model performance is defined here as R-Squared
summary(linear_quality_5)

# Check how good is the model on the training set - correlation^2, RME and MAE
train.corr <- round(cor(train.sample$Pred.quality, train.sample$quality), 2)
train.RMSE <- round(sqrt(mean((train.sample$Pred.quality - train.sample$quality)^2)))
train.MAE <- round(mean(abs(train.sample$Pred.quality - train.sample$quality)))
c(train.corr^2, train.RMSE, train.MAE)

# Check how good is the model on the validation set - correlation^2, RME and MAE
valid.corr <- round(cor(valid.sample$Pred.quality, valid.sample$quality), 2)
valid.RMSE <- round(sqrt(mean((valid.sample$Pred.quality - valid.sample$quality)^2)))
valid.MAE <- round(mean(abs(valid.sample$Pred.quality - valid.sample$quality)))
c(valid.corr^2, valid.RMSE, valid.MAE)
