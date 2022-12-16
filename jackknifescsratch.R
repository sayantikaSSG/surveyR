library(survey)
data(api)
jkknife<- function(variable, clus, func, dataset)
{
  statistic = func(dataset[,variable])
  nh = length(unique(dataset[,clus]))
  a<-unique(dataset[[,clus]])
  b = c(1:nh)
  for(i in 1:nh){
    c = c+ (b[i]-statistic)^2
  }
  variance = c*(nh-1)/nh
  return(data.frame(statistic, sqrt(variance)))
}
jkknife(apiclus1$api00, "dnum", mean, apiclus1)
