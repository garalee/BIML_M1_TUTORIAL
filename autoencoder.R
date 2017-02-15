## Written by Garam, Ajou University Feb. 12th
## Autoencoder tutorial

library(autoencoder)

wine = read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data',sep=',')
colnames(wine) <- c("type","Alcohol","Malic acid","Ash","Alcalinity of ash", "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols", "Proanthocyanins", "Color intensity", "Hue", "OD280/OD315 of diluted wines", "Proline")


wine.type = wine[,1]
wine.data = wine[,-1]
wine.data = scale(wine.data)

autoencoder.object = autoencode(X.train=wine.data,nl=3,N.hidden=2,unit.type='tanh',lambda=0.01,beta=6,rho=0.01,epsilon=0.1)

w = autoencoder.object$W[[1]]

reduced.data = wine.data %*% t(w)
plot(reduced.data,col=wine.type)

