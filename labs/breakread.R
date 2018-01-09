library(xml2)
library(SnowballC)  
library(dplyr)
library(data.table)
options(mc.cores=1)

base="final/en_US/"
freqs=list()
for(path in dir(base)){
        fn=paste(base,path,collapse = "")
        fn=gsub(" +","",fn)
        freqs[[path]]=getAllFreq(fn,100000)
}
save(freqs,file="freqs.rdata")
