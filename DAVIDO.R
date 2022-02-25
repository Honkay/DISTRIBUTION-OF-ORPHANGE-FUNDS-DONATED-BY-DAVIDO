library(readxl)

DAVIDOR <- read_excel("Downloads/DAVIDOR.xlsx")
Funds  <- DAVIDOR$funds
Numbers  <- DAVIDOR$number
LINEAR  <-data.frame(Numbers, Funds)
linearMod <- lm(Funds ~ Numbers, data=LINEAR)
print(linearMod)
summary(linearMod)
layout(matrix(c(1,1,2,3),2,2,byrow=T))
#Day x Residuals Plot
plot(linearMod$resid~LINEAR$Numbers[order(LINEAR$Numbers)],
     main="Numbers x Residuals\nfor Simple Regression",
     xlab="Numbers", ylab="Residuals")
abline(h=0,lty=2)
#Histogram of Residuals
hist(linearMod$resid, main="Histogram of Residuals",
     ylab="Residuals")
#Q-Q Plot
qqnorm(linearMod$resid)
qqline(linearMod$resid)

library(fBasics)
jarqueberaTest(linearMod$resid) #Test residuals for normality
#Null Hypothesis: Skewness and Kurtosis are equal to zero
#Residuals X-squared: 4.1228 p Value: 0.1273
#With a p value of 0.1273, we fail to reject the null hypothesis that the skewness and kurtosis of residuals are statistically equal to zero.
library(lmtest) #dwtest
dwtest(linearMod) #Test for independence of residuals
#Null Hypothesis: Errors are serially UNcorrelated
# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(LINEAR), 0.8*nrow(LINEAR))  # row indices for training data
trainingData <- LINEAR[trainingRowIndex, ]  # model training data
testData  <- LINEAR[-trainingRowIndex, ]   # test data
# Build the model on training data
lmMod <- lm(Funds ~ Numbers, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance
pred1a.test <- predict(linearMod, newdata=testData )
data.frame(testData, pred1a.test, distPred)
summary (lmMod)
summary(pred1a.test)
AIC (lmMod)
actuals_preds <- data.frame(cbind(actuals=testData, predicteds=distPred))  # make actuals_predicteds dataframe.
Second.test <- testData$Funds
par(mfrow=c(1,2))
plot(distPred, Second.test)
abline(a=0, b=1, lty=2)
plot(distPred, pred1a.test)
abline(a=0, b=1, lty=2)
cor.test <- cor(pred1a.test, Second.test)
R2.test <- cor.test^2
R2.test
alpha <- 0.05
conf.Second <- predict(linearMod, data = LINEAR, interval="confidence", level=1-alpha) 
head(conf.Second)
pred.Second <- predict(linearMod, data = LINEAR, interval="prediction", level=1-alpha) 
head(pred.Second)
library(ggplot2)
theme_set(theme_bw())
pl <- ggplot(LINEAR) + geom_point(aes(x=Numbers, y=Funds), size=2, colour="#993399") + 
  xlab("Numbers") + ylab("Funds")  
print(pl)
LINEAR[c("fit","lwr.conf", "upr.conf")] <- conf.Second
LINEAR[c("lwr.pred", "upr.pred")] <- pred.Second[,2:3]
pl +
  geom_ribbon(data=LINEAR, aes(x=Numbers, ymin=lwr.pred, ymax=upr.pred), alpha=0.1, inherit.aes=F, fill="blue") + 
  geom_ribbon(data=LINEAR, aes(x=Numbers, ymin=lwr.conf, ymax=upr.conf), alpha=0.2, inherit.aes=F, fill="#339900") +  
  geom_line(data=LINEAR, aes(x=Numbers, y=fit), colour="#339900", size=1)
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
# Min-Max Accuracy Calculation
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# MAPE Calculation
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
library(DAAG)
cvResults <- suppressWarnings(CVlm(data=LINEAR, form.lm=Funds ~ Numbers, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=TRUE));  # performs the CV
attr(cvResults, 'ms')   
scatter.smooth(x=LINEAR$Numbers, y=LINEAR$Funds, main="Funds ~ Numbers")  # scatterplot 
plot(linearMod$residuals, pch = 16, col = "red")
ggplot(LINEAR, aes(x = Numbers, y = Funds)) +
  geom_point() +
  stat_smooth()
cor(LINEAR$Numbers, LINEAR$Funds)
ggplot(LINEAR, aes(Numbers, Funds)) +
  geom_point() +
  stat_smooth(method = lm)
confint(linearMod)
# RESIDUAL STANDARD ERROR
sigma(linearMod)*100/mean(LINEAR$Funds)
par(mfrow=c(2,2))
plot(linearMod)
par(mfrow=c(1,1))

library(e1071)
modelsvm = svm(Funds ~ Numbers, data=LINEAR)
summary(modelsvm)
jarqueberaTest(modelsvm$resid)
predYsvm = predict(modelsvm, data=LINEAR)
plot(LINEAR)
points(LINEAR$Numbers, predYsvm, col = "red", pch=16)
##Calculate parameters of the SVR model
library(hydroGOF)
#Find value of W
W = t(modelsvm$coefs) %*% modelsvm$SV

#Find value of b
b = modelsvm$rho
summary(predYsvm)
## RMSE for SVR Model

#Calculate RMSE 
RMSEsvm=rmse(predYsvm,LINEAR$Funds)
## Tuning SVR model by varying values of maximum allowable error and cost parameter

#Tune the SVM model
OptModelsvm=tune(svm, Funds ~ Numbers, data=LINEAR,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
# YOU CAN ALSO TRY seq(0,0.2,0.01)
#Print optimum value of parameters
print(OptModelsvm)

#Plot the perfrormance of SVM Regression model
plot(OptModelsvm)
## Select the best model out of 1100 trained models and compute RMSE

#Find out the best model
BstModel=OptModelsvm$best.model

#Predict Y using best model
PredYBst=predict(BstModel,LINEAR)

#Calculate RMSE of the best model 
RMSEBst=rmse(PredYBst,LINEAR$Funds)
##Calculate parameters of the Best SVR model

#Find value of W

#Find value of b
b = BstModel$rho
## Plotting SVR Model and Tuned Model in same plot
#Actual data (black), SVR model (blue), tuned SVR model (red).
plot(LINEAR, pch=16)
points(LINEAR$Numbers, predYsvm, col = "blue", pch=3)
points(LINEAR$Numbers, PredYBst, col = "red", pch=4)
points(LINEAR$Numbers, predYsvm, col = "blue", pch=3, type="l")
points(LINEAR$Numbers, PredYBst, col = "red", pch=4, type="l")

library(caret)
trainingRowIndex <- sample(1:nrow(LINEAR), 0.8*nrow(LINEAR))  # row indices for training data
train <- LINEAR[trainingRowIndex, ]  # model training data
test  <- LINEAR[-trainingRowIndex, ]   # test data
model_reg = svm(Funds ~ Numbers, data=train)
jarqueberaTest(model_reg$resid)
print(model_reg)
pred = predict(model_reg, test)

x = 1:length(test$Funds)
plot(x, test$Funds, pch=18, col="red")
lines(x, pred, lwd="1", col="blue")

mae = MAE(test$Funds, pred)
rmse = RMSE(test$Funds, pred)
r2 = R2(test$Funds, pred, form = "traditional")

cat(" MAE:", mae, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", r2)
cm = data.frame(test , pred)


#Kuwait <-
#Here I can remove the ,bs="cr", I can remove k = 12, default K is 10
# FOR method, any of the following c("GCV.Cp", "GACV.Cp", "REML", "P-REML", "ML", "P-ML")
#family=poisson OR negative.binomial(1) family=Gamma(link="log"),, family="gaussian"
GAM <-gam(LINEAR$Funds ~ s(LINEAR$Numbers,bs="cr", k = 12),
            family="gaussian",method="ML") 
gam.check(GAM)
summary(GAM)
#WE CAN APPRAISE WITHOUT method = "simulate"
appraise(GAM, method = "simulate")
plot(GAM,residuals=TRUE)
plot(GAM,too.far=0.15)
draw(GAM)
predict(GAM)
pv <- predict(GAM,se=TRUE)
pv$fit
predict(GAM,type="response")
pv$se
pv1 <- predict(GAM,type="response",se=TRUE)
pv1$se
#MIXED MODEL
GAM3<-gamm(LINEAR$Funds ~ s(LINEAR$Numbers,bs="cr", k = 12),
              correlation = NULL,method="REML") 
m.same<-gam(LINEAR$Funds ~ s(LINEAR$Numbers,bs="cr", k = 12),
            family=Gamma(link=log),method="ML")
gam.check(m.same)
summary(m.same)
appraise(m.same)
draw(m.same)
predict.gam(m.same)
plot.gam(m.same)


