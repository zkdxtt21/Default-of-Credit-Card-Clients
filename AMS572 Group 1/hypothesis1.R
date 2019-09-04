
#setting working directory
setwd("C:/Users/onkar/Downloads/AMS 572/project group1/credit card defaulters/")

#reading dataset
data = read.csv('dataset.csv', header = TRUE)


#considering subset of data
data_bill_amt = data[,c(13:18)]

#preprocessing
#subtracting april month data from rest other cols
diff.names <- colnames(data_bill_amt)[1:5]
diff <- vector('list',length(diff.names))
names(diff) <- diff.names


diff <- matrix(nrow = dim(data)[1],ncol=5)
dimnames(diff) <- list(rownames(diff),colnames(data_bill_amt)[1:5])

for (col_name in colnames(diff)){
  diff[,col_name] <- (data_bill_amt[,col_name] - data_bill_amt$X17)
}

#checking summary
summary(diff)

## Plotting for boxplot, to check visually if variance across the five columns is constant
boxplot(c(log(diff)-min(diff))~rep(c(1:dim(diff)[2]),each=dim(diff)[1]))

boxplot(c(diff)~rep(c(1:dim(diff)[2]),each=dim(diff)[1]))


##f - test to check for equal variance
for (i in 1:4){
  for (j in (i+1):5){
    print(i)
    print(j)
    print(var.test(diff[,i],diff[,j]))
  }
}

# since variance is not constant, we can't use anova method

#hence use equality on means of two bill amount with unequal var [t-test]


for (i in 1:4){
  print(t.test(diff[,i],diff[,i+1],var.equal = FALSE, conf.level = (1-0.1/4)))
}



##plotting mean data
mean_amt <- c(mean(diff[,5]),mean(diff[,4]),mean(diff[,3]),mean(diff[,2]),mean(diff[,1]))
month_name <- c('May','June', 'July','August', 'September')

plot(mean_amt, xlab = "Month", ylab = "Mean amount",
     xaxt='n', ann=FALSE)
axis(1, at= 1:5, labels=month_name)


