library(tm)
lang="english"
source('getTDM.R')
path='final/en_US/'
size=1000
full=lapply(1:10,function(i) getFreq(path,size*i))
all=rbindlist(lapply(1:10,function(i) 
        full[i][[1]][seq(1,length(full[i][[1]][[1]]),(length(full[i][[1]][[1]])/1000))]))
all$dev=unlist(all$sd)
all$media=unlist(all$media)
plotall=all[,.(i=1,media,dev,size)][,.(i=cumsum(i),media,dev),size]
ggplot(plotall,aes(color=size,x=i,y=media))+geom_errorbar(aes(ymin=media-dev,ymax=media+dev))
