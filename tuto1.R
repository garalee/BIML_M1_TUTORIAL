
#############################
######   naive bayes   ######
#############################

library(made4)
data(khan)
summary(khan)
khan$train
#...

library(plsgenomics)
data(SRBCT)
dim(SRBCT$X)
sum(SRBCT$Y==1)
sum(SRBCT$Y==2)
sum(SRBCT$Y==3)
sum(SRBCT$Y==4)

library(e1071)

#khan..

train_data <- khan$train
train_class <- khan$train.classes
test_data <- khan$test
test_class <- khan$test.classes

train_data <- t(train_data)
test_data <- t(test_data)

naive_bayes_model <- naiveBayes(x=train_data, y=train_class)

test_result_nb <- predict(naive_bayes_model, test_data)

table(test_result_nb, test_class) 


### SRBCT..

train_row <- sample(1:83,60)
train_data <- SRBCT$X[train_row,]
test_data <- SRBCT$X[-train_row,]
train_class <- factor(SRBCT$Y[train_row],levels = c("1","2","3","4"))
test_class <- factor(SRBCT$Y[-train_row],levels = c("1","2","3","4"))

naive_bayes_model <- naiveBayes(x=train_data, y=train_class)
test_result_nb <- predict(naive_bayes_model, test_data)
table(test_result_nb, test_class)


## cross validation

k=5
index <- sample(1:k,nrow(SRBCT$X),replace=TRUE)
folds <- 1:k
myRes=data.frame()

for (i in 1:k) {
  # create training set
  training <- subset(SRBCT$X, index %in% folds[-i]) 
  # create training set label
  training_class <- factor(subset(SRBCT$Y, index %in% folds[-i]),levels = c("1","2","3","4")) 
  # create test set
  test <- subset(SRBCT$X, index %in% c(i))
  # create test set label
  test_class <- factor(subset(SRBCT$Y, index %in% c(i)),levels = c("1","2","3","4")) 
  # train model
  naive_bayes_model <- naiveBayes(x=training, y=training_class) 
  # run model on test set
  temp <- data.frame(predict(naive_bayes_model, test)) 
  colnames(temp)="Predicted"
  # create data.frame for results
  results <- data.frame(Predicted=temp, Actual=test_class) 
  # append results for each iteration
  myRes <- rbind(myRes, results)
}

table(myRes)



##############Evaluation
library(caret)
library(pROC)

### Confusion matrix
confusionMatrix(myRes$Predicted, myRes$Actual)

### ROC curve
roc_nb <- plot.roc(as.numeric(myRes$Predicted), as.numeric(myRes$Actual))
lines(roc_nb, col="black")
legend("bottomright", c("Naive Bayes"), fill = c("black"))

multiclass.roc(as.numeric(myRes$Predicted), as.numeric(myRes$Actual), percent=TRUE)



#knn
train_data <- SRBCT$X[train_row,]
test_data <- SRBCT$X[-train_row,]
train_class <- factor(SRBCT$Y[train_row],levels = c("1","2","3","4"))
test_class <- factor(SRBCT$Y[-train_row],levels = c("1","2","3","4"))
library(class)
test_result_knn <- knn(train_data, test_data ,train_class, k = 3)
table(test_result_knn, test_class)


####
maxval=0
maxk=1
for(i in 1:20){
  ###########################
  test_result_knn <- knn(train_data, test_data ,train_class, k = i)
  tempacc <- confusionMatrix(test_result_knn,test_class)$overall[1]
  if(maxval<tempacc){
    maxval=tempacc
    maxk=k
  }
}
maxk
maxval

#############################
######   Regression   ######
#############################

#####simple
library("MASS")
data(cats)
cats
str(cats)
summary(cats)
hist(cats$Bwt)
hist(cats$Bwt, breaks=5)
plot(Hwt ~ Bwt, data=cats)

Reg = lm(formula = Hwt ~ Bwt, data = cats)
summary(Reg)
abline(Reg, col="red")


#####multivariate
library(glmnet)

library(hierGWAS)
x <- data.matrix(simGWAS[,1:1000])
y <- simGWAS$y

# linear regression
fit <- glmnet(x,y, family="gaussian", lambda = 0)
pred <- predict(fit,x)

# mean square error
mean((y-pred)**2)


### ridge regression
fit <- glmnet(x,y, family="gaussian", alpha = 0, lambda = 10)
pred <- predict(fit,x)
mean((y-pred)**2)

# lamda tuning (cross validation)
cv.fit <- cv.glmnet(x,y, family="gaussian", alpha = 0, nlambda = 10)
cv.fit$lambda.min
fit2 <- glmnet(x,y, family="gaussian", alpha = 0, lambda = cv.fit$lambda.min)
pred2 <- predict(fit2,x)
mean((y-pred2)**2)



### lasso
fit <- glmnet(x,y, family="gaussian", alpha = 1, lambda = 10)
pred <- predict(fit,x)
mean((y-pred)**2)

# lamda tuning (cross validation)
cv.fit <- cv.glmnet(x,y, family="gaussian", alpha = 1, nlambda = 10)
cv.fit$lambda.min
fit2 <- glmnet(x,y, family="gaussian", alpha = 1, lambda = cv.fit$lambda.min)
pred2 <- predict(fit2,x)
mean((y-pred2)**2)

nonzeroCoef(fit2$beta)
fit2$beta[nonzeroCoef(fit2$beta)]



### logistic regression
fit <- glmnet(x, as.factor(y), alpha=1, family="binomial")
pred <- predict(fit,x)

a=c()
for(i in y){
  if(i>median(y)){
    a=c(a,1)
  }
  else{
    a=c(a,0)
  }
}

confusionMatrix(a, y)




#############################
######   clustering    ######
#############################
library(kernlab)

data(spirals)
plot(spirals)

km = kkmeans(spirals, kernel = 'rbfdot', alg='kkmeans', centers=2)
plot(spirals, col=km)
             
plot(spirals)
sc = specc(spirals, kernel = "rbfdot", centers = 2)
plot(spirals, col=sc)






