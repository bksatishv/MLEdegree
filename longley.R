#Open the Longley data set from the datasets package

require(stats); 
require(tidyverse);
longley.x <- data.matrix(longley[, 1:6])
longley.y <- longley[, "Employed"]

# Plot Employed against each variable 
# and select the 3 variables most correlated to Employed.

## Plot GNP.deflator and Employed
plot(longley.x[,"GNP.deflator"],longley.y)

## Plot GNP and Employed
plot(longley.x[,"GNP"],longley.y)

## Plot Unemployed and Employed
plot(longley.x[,"Unemployed"],longley.y)

## Plot Armed.Forces and Employed
plot(longley.x[,"Armed.Forces"],longley.y)

## Plot Population and Employed
plot(longley.x[,"Population"],longley.y)

## Plot Year and Employed
plot(longley.x[,"Year"],longley.y)


## GNP.deflator, GNP and Population are most correlated

## For each variable selected in number 2, 
## create a regression model for Employed. Select the best model

lm.mod<- lm(longley.y~longley.x[,"GNP.deflator"])
summary(lm.mod)

lm1.mod<- lm(longley.y~longley.x[,"GNP"])
summary(lm1.mod)

lm2.mod<- lm(longley.y~longley.x[,"Population"])
summary(lm2.mod)

## Select GNP as best model based on Least Residual Standard Error

# For the champion model from number 3, create the model matrices.

y<-as.matrix(longley.y)
x<-longley.x[,"GNP"]

# Recalculate the regression parameters 
# and predicted values from the model matrices.
X<-cbind(1,matrix(x))
B<-solve(t(X)%*%X)%*%t(X)%*%y

# 1	51.84358978
# 2	0.03475229

data.00<-as.data.frame(X)
pred<-data.00%>%
  mutate(Predicted=51.84+V2*0.034)
View(pred)
