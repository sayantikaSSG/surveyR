##################
#Jackknife scratch 1
##################

library(survey)
data(api)
head(apiclus1)
jkknife<-function(variable, clus ,func,dataset){
statistic = func(variable)
nh = length(unique(dataset[,clus]))
a <- unique(dataset[,clus])
b = c(1:nh)
for(i in 1:length(a)){
  b[i] = func(variable[which(dataset[,clus] != a[i])])
}
c=0
for(i in 1: nh){
  c=c+(b[i]-statistic)^2
}
variance= c*(nh-1)/(nh)
return(data.frame(statistic,sqrt(variance)))
}

dclus1<-svydesign(id=~dnum, weights=~pw, data=apiclus1)
jkclus1<-as.svrepdesign(dclus1)
svytotal(~api00, jkclus1)
