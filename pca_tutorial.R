
library(pcaMethods)


## wine data set
wine = read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data',sep=',')
colnames(wine) <- c("type","Alcohol","Malic acid","Ash","Alcalinity of ash", "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols", "Proanthocyanins", "Color intensity", "Hue", "OD280/OD315 of diluted wines", "Proline")

wine.type = wine[,1]
wine.data = scale(wine[,-1])
result = pca(wine.data,method='svd')
plot(scores(result),col=wine.type,main='Dimension reduction(PCA)')