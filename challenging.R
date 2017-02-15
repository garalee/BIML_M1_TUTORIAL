
library(e1071)
library(autoencoder)

## Real data
attach(iris)
x = subset(iris,select=-Species)
y = Species
n = dim(iris)[1]

ntrain = round(n*0.8)
tindex = sample(n,ntrain)
xtrain = x[tindex,]
xtest = x[-tindex,]

ytrain = y[tindex]
ytest = y[-tindex]
s

## dimension reduction for training data
autoencoder.object = autoencode(X.train=as.matrix(xtrain),nl=3,N.hidden=2,unit.type='logistic',lambda=0.01,beta=6,rho=0.01,epsilon=0.1)
w = autoencoder.object$W[[1]]
reduced.data = as.matrix(xtrain) %*% t(w)
plot(reduced.data,col=ytrain)

svp = svm(reduced.data,ytrain,scale=c(),type='nu-classification')

## dimension reduction for test data
reduced.test.data = as.matrix(xtest) %*% t(w)
ypred = predict(svp,reduced.test.data)

table(ytest,ypred)
