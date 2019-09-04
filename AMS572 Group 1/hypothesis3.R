#setting working directory
setwd("C:/Users/onkar/Downloads/AMS 572/project group1/credit card defaulters/")

library(car)
library(MASS)


#reading dataset
ams_572_data = read.csv('dataset.csv', header = TRUE)


#pre processing
#sum of bill payments, converting the negatives to postive
ams_572_data$sum_pay <- ams_572_data$X6 + ams_572_data$X7 + ams_572_data$X8 
                        + ams_572_data$X9 + ams_572_data$X10 + ams_572_data$X11

ams_572_data$sum_pay <- ams_572_data$sum_pay - min(ams_572_data$sum_pay)+1

colnames(ams_572_data)[26] <- 'X24'


#removing irrelavent data from education and maritial status
ams_572_data <- subset(ams_572_data, !(X3 %in% c(0,4,5,6)))
ams_572_data <- subset(ams_572_data, !(X4 %in% c(0,3)))

Data_col_name = colnames(ams_572_data)

attach(ams_572_data)

#PCA Analysis to get the vector that can denote all columns in our data
cor(ams_572_data[13:18]) #bill
cor(ams_572_data[19:23]) #payment
cor(ams_572_data[7:12])  #due date
pc = princomp(ams_572_data[,13:18],cor = T,scores = T)
#pc2 = princomp(ams_572_data[,13:18],cor = F,scores = T)
summary(pc)
pc$loadings
statement = as.matrix(ams_572_data[,13:18])
pca1 <- as.matrix(pc$loadings[,1])
score = statement %*% pca1
ams_572_data = cbind(ams_572_data,score)
rm(score)
detach(ams_572_data)
attach(ams_572_data)


# linear regression model selection
fit1 <- lm(X1~X2+X3+X4+X5+X24+score+X18+X19+X20+X21+X22+X23+X24)
summary(fit1)
plot(fit1,which=1:6)


# standardize 
ams_572_data_standardized=ams_572_data
for (j in 13:24) {
  ams_572_data_standardized[,j] = (ams_572_data[,j]-mean(ams_572_data[,j]))/sd(ams_572_data[,j]) 
}
fit2 <- lm(X1~X2+X3+X4+X5+X24+score+X18+X19+X20+X21+X22+X23+X24, data = ams_572_data_standardized)
summary(fit2)



#Box-Cox Transformation
bc = boxcox(fit1)
bc$x[which.max(bc$y)]


fit1 <- lm( X1^0.3 ~ X2+X3+X4+X5+X24+score+X18+X19+X20+X21+X22+X23+X24+Y)
summary(fit1)
plot(fit1,which=1:2)



#we need consider the influence of OutLier
outlier = outlierTest(fit1)
while(outlier$p<0.01){
  row = as.numeric(names(outlier$p))
  ams_572_data = ams_572_data[c(-row),]
  detach(ams_572_data)
  attach(ams_572_data)
  fit1 <- lm( log(X1) ~ X2+X3+X4+X5+X24+score+X18+X19+X20+X21+X22+X23+X24)
  outlier = outlierTest(fit1)
}
summary(fit1)

#cook's distance
result<-cooks.distance(fit1)
cook=4/(fit1$df)
plot(fit1,which=4,cook.levels=cook)
abline(h=0.004,lty=2,col=2) 
sum(result>0.004)
Data_new = ams_572_data[!result>0.004,]
detach(ams_572_data)
attach(Data_new)
fit3 <- lm( X1^0.3 ~ X2+X3+X4+X5+X24+score+X18+X19+X20+X21+X22+X23+X24,data = Data_new)
summary(fit3)
plot(fit3,which=1:6)
smoothScatter(fit3$fitted.values,fit3$residuals)


