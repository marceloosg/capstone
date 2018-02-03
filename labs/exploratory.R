library(tm)
library(ggplot2)
lang="english"
source('getTDM.R')
path='final/en_US/'
size=1000
# get 5 different size samples from source
samples=sapply(1:5,function(count) getSamples(path=path,size=size,count=count))

fulls=lapply(1:10,function(i) getFreq(path,size*i,sapply(1:5,function(count) getSamples(path=path,size=size*i,count=count))
))
getFulls=function(i,f=fulls)        f[i][[1]]$ret[seq(1,dim(f[[i]]$ret)[1],dim(f[[i]]$ret)[1]/1000)]

allf=rbindlist(lapply(1:10,getFulls))

colnames(allf)[1:2]=c('media','dev')
ggplot(allf,aes(x=media,y=dev))+geom_line()+facet_wrap(~size)


plotall=all[,.(i=1,media,dev,size)][,.(i=cumsum(i),media,dev),size]
ggplot(plotall,aes(color=size,x=i,y=media))+geom_errorbar(aes(ymin=media-dev,ymax=media+dev))

fulls=lapply(1:5,function(i) getFreq(path,size*i,sapply(1:5,function(count) getSamples(path=path,size=size*i,count=count))
))






ml=min(sapply(1:length(freq),function(i) length(freq[[i]])))

ret=data.table(f=freq1gram)[,.(f=.N,N=sum(freq1gram)),.(q=f)]
ret[,.(q,f,fc=cumsum(f*q)/N,N)]
et$source=sampledir
ret$ordem=1:dim(ret)[1]
ret
mf=melt(freq,id.vars = c("source",'ordem'))
mf=mf[,.(v=mean(value),sd(value)),.(ordem,variable)]
