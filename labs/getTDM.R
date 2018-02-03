
getSamples=function(path,type="twitter",size=1000,count=1){
        source=paste(path,'en_US.',type,'.txt',sep='')
        samplepath=paste('sample.',size,'.',type,'.',count,sep='')
       # print(samplepath)
        dir.create(samplepath,showWarnings = F)
        target=paste(samplepath,'/',type,'.',count,'.txt',sep='')
        if(!file.exists(target))
                system(paste('cat',source,'|shuf -n ',size,' -o ',target))
        samplepath
}
getTDM=function(sampledir,type='twitter'){
        sp <- Corpus(DirSource(sampledir,pattern = paste(type,'.*',sep='')),
                     readerControl=list(readPlain,
                                        language=lang,
                                        load=F)
        )
        detectNonAsciiChar = function(x) iconv(x, from="UTF-8", to="ASCII",sub="X")
        transformNonAsciiWord = function(x) gsub("[a-z]*X+[a-z]*", "NonAsciiWord", x)
        removeHttp = function(x) gsub("http((?! ).)*","",x,perl=TRUE)
        removeTripleChars = function(x) gsub("([[:alpha:]])\\1{2,}","\\1",x,perl=TRUE)
        sp=tm_map(sp, content_transformer(tolower))
        sp = tm_map(sp, content_transformer(detectNonAsciiChar))
        sp = tm_map(sp, content_transformer(transformNonAsciiWord))
        sp=tm_map(sp, content_transformer(removeHttp))
        sp=tm_map(sp, removeWords,stopwords(kind="en"))
        sp=tm_map(sp, removePunctuation)
        sp=tm_map(sp, removeNumbers)
        tdmsp=TermDocumentMatrix(sp,control = list(stripWhitespace=TRUE,wordLengths=c(3,50)))        
}
getFrequencies=function(path,size,maxcount,maxsamples){
        freq=rbindlist(
                lapply(1:maxcount,function(i){
                        rbindlist(lapply(sapply(1:maxsamples,function(count) 
                                getSamples(path=path,size=size*i,count=count)),
                                function(sampledir){
                                        tdmsp=getTDM(sampledir)
                                        freq1gram <- sort(rowSums(as.matrix(tdmsp)), decreasing=TRUE)
                                        its=strsplit(sampledir,'\\.')[[1]]
                                        data.table(names=names(freq1gram),f=as.integer(freq1gram),
                                                   source=sampledir,
                                                   lines=its[2],
                                                   sample=its[4])
                                }
                        ))
                }))
}
getPercentages=function(freq,percentages=1:10*0.1){
        st=rbindlist(lapply(percentages,function(Thresh) freq[,.(f=sum(f)),.(names,lines)][order(-f),.(T=sum(f),f,cf=cumsum(f)/sum(f),names),.(lines)][cf<Thresh][,.(TargetWords=length(unique(names)),TotalWords=unique(T),Percentage=Thresh),lines]))
}
getFreq=function(path,size,samples){
        # Process each sample and get its frequency word
        freq=sapply(samples,function(sampledir){
                tdmsp=getTDM(sampledir)
                freq1gram <- sort(rowSums(as.matrix(tdmsp)), decreasing=TRUE)
                freq1gram[freq1gram>1]
        })
        library(data.table)
        # Get the minimum word count from all samples (compare samples of the same size)
        ml=min(sapply(1:length(freq),function(i) length(freq[[i]])))
        # Calculate the cummulated frequency for the cutted samples
        normalFreq=lapply(1:length(freq),function(i) as.numeric(cumsum(freq[[i]][1:ml])/sum(freq[[i]][1:ml])))
        freqs=data.table(matrix(unlist(lapply(1:length(freq),function(i) normalFreq[[i]][1:ml])),ncol = ml))
        freqsd=data.table(t(rbind(freqs[,lapply(.SD,mean)],freqs[,lapply(.SD,sd)])))
        freqsd$size=size
        list(ret=freqsd,raw=freqs)
}