data = iris
head(data,20)
sapply(data,class)

data$Species=as.numeric(data$Species)

pearson=function(x,y)
{
  xmean=mean(x)
  ymean=mean(y)
  num=sum((x-xmean)*(y-ymean))
  dino=sqrt(sum((x-xmean)^2))*sqrt(sum((y-ymean)^2))
  return (num/dino)
}



for (i in 1:ncol(data)) {
  a <- pearson(data[, i], data[, 5])
  print(paste("Correlation of", colnames(data)[i], "and", colnames(data)[5], "is:", a))
}



