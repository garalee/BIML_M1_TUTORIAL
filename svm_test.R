## Support Vector Machine Tutorial
## Jan 23rd 2016

library(e1071)


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

svp = svm(xtrain,ytrain,scale=c())
ypred = predict(svp,xtest)

table(ytest,ypred)
