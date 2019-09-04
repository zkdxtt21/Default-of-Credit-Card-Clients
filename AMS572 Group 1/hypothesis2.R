
#setting working directory
setwd("C:/Users/onkar/Downloads/AMS 572/project group1/credit card defaulters/")

#reading dataset
data = read.csv('dataset.csv', header = TRUE)

#pre processing
#sum of bill payments, converting the negatives to postive
data$sum_pay <- data$X6+data$X7+data$X8+data$X9+data$X10+data$X11
data$sum_pay <- data$sum_pay - min(data$sum_pay)+1

#considering subset of data and log transforming the values;
#negative values taken to be zero ~1K rows negative
credit <- data[,c(1,26,13:25)]
for (i in 2:14){
  credit[,i][credit[,i] <= 0] <- 1
  credit[,i] <- log10(credit[,i])
}
colnames(credit)[2] <- 'X24'

#hypothesis
attach(credit)
ID1 = credit[Y==1,1]
credit1 = credit[ID1,]
 k1 = dim(credit1)[1]
 k2 = 5000
 credit1_training_id = sample(1:k1,k2)
 credit1_training = credit1[credit1_training_id,]
 testing_data_1 = credit1[-credit1_training_id,]
 credit0 = credit[-ID1,]
 k3 = dim(credit0)[1]
 training_number = seq(10000,28000,by=2000)
 n=30000
 l = length(training_number)
 testing_number = rep(1,times=l)
 correct_rate = rep(1,times=l)
 zero_correct = rep(1,times=l)
 zero_false = rep(1,times=l)
 zero_correct_rate = rep(1,times=l)
 one_correct = rep(1,times=l)
 one_false = rep(1,times=l)
 one_correct_rate = rep(1,times=l)
 for (i in 1:l){
 m = training_number[i]
 
 if((m/2) < 2*k2){                                 ###banlanced sampling method
 	training_id1 = sample(1:k2,m/2-k2)
 	training_data_1 = rbind(credit1_training,credit1_training[training_id1,])
 	}
 else if((m/2) >= 2*k2){
 	training_id1 = sample(1:k2,m/2-2*k2)	
 	training_data_1 =  rbind(credit1_training,credit1_training,credit1_training[training_id1,])
 	}
 training_data_id0 = sample(1:k3,m/2)
 training_data_0 = credit0[training_data_id0,]
 testing_data_0 = credit0[-training_data_id0,]
 training_data = rbind(training_data_1,training_data_0)
 testing_data = rbind(testing_data_1,testing_data_0)
 Y_testing = testing_data[,15]
 testing_number[i] = length(Y_testing)
 library(ISLR)
 
 credit_model = glm(Y~ X12 + X13 + X14 + X15 + X17+X18+X19+X20+X21+X22+X23+X24, data = training_data, family = binomial(link = "logit"))
 credit_pred_probs = predict(credit_model, testing_data, type = "response")    ###logistic regression
 summary(credit_model)
 credit_pred_Y = rep("0",testing_number[i])
 credit_pred_Y[credit_pred_probs>0.5] = "1"
 table(credit_pred_Y, Y_testing)                 #to produce the confusion matrix----the classification results

 correct_rate[i] = mean(credit_pred_Y == Y_testing)
 zero_correct[i] = table(credit_pred_Y, Y_testing)[1,1]
 zero_false[i] = table(credit_pred_Y, Y_testing)[2,1]
 zero_correct_rate[i] = zero_correct[i]/(zero_correct[i]+zero_false[i])
 one_correct[i] = table(credit_pred_Y, Y_testing)[2,2]
 one_false[i] = table(credit_pred_Y, Y_testing)[1,2]
 one_correct_rate[i] = one_correct[i]/(one_correct[i]+one_false[i])

 }
 correct_rate
 zero_correct_rate
 one_correct_rate

###plot the total corract_rate of classification
 par(mfcol=c(1,2))
 plot(training_number,correct_rate,ylim=c(0,1),xlab="number of training samples")
 abline(h=0.73,col="red")
 title(main = "correct_rate vs. the number of training samples")
 plot(training_number/(testing_number),correct_rate,ylim=c(0,1),xlab="training samples vs. test samples")
 abline(h=0.73,col="blue")
 title(main = "correct_rate vs the rate of training samples to test \nsamples")

### plot the correct_rate of o and 1
 par(mfrow=c(2,2))
 plot(training_number,one_correct_rate,ylim=c(0,1),xlab="number of training samples")
 abline(h=0.6,col="red")
 title(main = "1_correct_rate vs. the number of training samples")
 plot(training_number/(testing_number),one_correct_rate,ylim=c(0,1),xlab="training samples vs. test samples")
 abline(h=0.6,col="blue")
 title(main = "1_correct_rate vs the rate of training samples to test \nsamples")
 plot(training_number,zero_correct_rate,ylim=c(0,1),xlab="number of training samples")
 abline(h=0.74,col="red")
 title(main = "0_correct_rate vs. the number of training samples")
 plot(training_number/(testing_number),zero_correct_rate,ylim=c(0,1),xlab="training samples vs. test samples")
 abline(h=0.74,col="blue")
 title(main = "0_correct_rate vs the rate of training samples to test \nsamples")

 
 
 
 
