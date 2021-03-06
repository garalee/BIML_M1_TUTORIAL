## Support Vector Machine Tutorial
## Jan 23rd 2016


## Generate Data
n <- 150 # number of data points
p <- 2 # dimension
sigma <- 1 # variance of the distribution
meanpos <- 0 # centre of the distribution of positive examples
meanneg <- 3 # centre of the distribution of negative examples
npos <- round(n/2) # number of positive examples
nneg <- n-npos # number of negative examples
# Generate the positive and negative examples
xpos <- matrix(rnorm(npos*p,mean=meanpos,sd=sigma),npos,p)
xneg <- matrix(rnorm(nneg*p,mean=meanneg,sd=sigma),npos,p)
x <- rbind(xpos,xneg)
# Generate the labels
y <- matrix(c(rep(1,npos),rep(-1,nneg)))
# Visualize the data
plot(x,col=ifelse(y>0,1,2))
legend("topleft",c('Positive','Negative'),col=seq(2),pch=1,text.col=seq(2))


## Prepare a training and a test set ##
ntrain <- round(n*0.8) # number of training examples
tindex <- sample(n,ntrain) # indices of training samples
xtrain <- x[tindex,]
xtest <- x[-tindex,]
ytrain <- y[tindex]

ytest <- y[-tindex]
istrain=rep(0,n)
istrain[tindex]=1
# Visualize
plot(x,col=ifelse(y>0,1,2),pch=ifelse(istrain==1,1,2))
legend("topleft",c('Positive Train','Positive Test','Negative Train','Negative Test'),
       col=c(1,1,2,2),pch=c(1,2,1,2),text.col=c(1,1,2,2))

## Train a SVM
# load the kernlab package
library(e1071)
# train the SVM
svp = svm(xtrain,ytrain,kernel='linear',scale=c())

w = colSums(as.numeric(svp$coefs) * xtrain[svp$index,])
b = svp$rho

# Draw the lines
abline(b/w[2],-w[1]/w[2])
abline((b+svp$epsilon)/w[2],-w[1]/w[2],lty=2)
abline((b-svp$epsilon)/w[2],-w[1]/w[2],lty=2)

## Real data

attach(iris)
x = subset(iris,select=-Species)
y = Species

https://github.com/garalee/BIML_M1_TUTORIAL
