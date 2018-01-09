
getSamples=function(path,type="twitter",size=1000,count=1){
        source=paste(path,'en_US.',type,'.txt',sep='')
        samplepath=paste('sample.',size,'.',type,'.',count,sep='')
        print(samplepath)
        dir.create(samplepath,showWarnings = F)
        target=paste(samplepath,'/',type,'.',count,'.txt',sep='')
        system(paste('cat',source,'|shuf -n ',size,' -o ',target))
        samplepath
}
getTDM=function(sampledir,type='twitter'){
        sp <- Corpus(DirSource(sampledir,pattern = paste(type,'.*',sep='')),
                     readerControl=list(readPlain,
                                        language=lang,
                                        load=F)
        )
        sp=tm_map(sp, content_transformer(tolower))
        sp=tm_map(sp, removeWords,stopwords(kind="en"))
        sp=tm_map(sp, removePunctuation)
        sp=tm_map(sp, removeNumbers)
        tdmsp=TermDocumentMatrix(sp,control = list(stripWhitespace=TRUE,wordLengths=c(5,50)))        
}
getFreq=function(path,size){
        samples=sapply(1:5,function(count) getSamples(path=path,size=size,count=count))
        freq=sapply(samples,function(sampledir){
                tdmsp=getTDM(sampledir)
                freq1gram <- sort(rowSums(as.matrix(tdmsp)), decreasing=TRUE)
        })
        library(data.table)
        ml=min(sapply(1:length(freq),function(i) length(freq[[i]])))
        normalFreq=lapply(1:length(freq),function(i) as.numeric(cumsum(freq[[i]][1:ml])/sum(freq[[i]][1:ml])))
        freqs=data.table(matrix(unlist(lapply(1:length(freq),function(i) normalFreq[[i]][1:ml])),ncol = ml))
        freqsd=freqs[,.(size=size,media=unlist(lapply(.SD,mean)),sd=unlist(lapply(.SD,sd)))]
}