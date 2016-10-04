library(tm)
library(RWeka)
library(xml2)
library(SnowballC)  
library(dplyr)
library(data.table)
options(mc.cores=1)
lang="english"
dirlist=dir("./teste")
tdmlist=list()
dt=data.table()
for( d in dirlist){
        start.time <- Sys.time()
        ovid <- Corpus(DirSource(paste0("./teste/",d)),
               readerControl=list(readPlain,
                                  language=lang,
                                  load=F)
        )
        ovid <- tm_map(ovid, removeWords, stopwords("english"))
        #ovid <- tm_map(ovid, stripWhitespace) hangs
        end.time <- Sys.time()
        time.taken.to.read <- end.time - start.time
        print(c(d,"took ",time.taken.to.read,"to read"))
        
        TrigramTokenizer <- function(x) NGramTokenizer(x, 
                                               Weka_control(min = 1, max = 3))
        start.time <- Sys.time()
        tdm=TermDocumentMatrix(ovid,control = 
                               list(   tokenizer=TrigramTokenizer, 
                                       removePunctuation = TRUE,
                                   removeNumbers = TRUE,
                                    wordLengths=c(5,50)))

        end.time <- Sys.time()
        time.taken.to.process <- end.time - start.time
        print(c("took ",time.taken.to.process,"to proccess"))
        tdmlist=c(tdmlist,list(tdm))
        dTime=data.table(scale=d,read=time.taken.to.read, process=time.taken.to.process)
        if(length(dt)==0){
                dt=dTime
        }
        else
        {
                dt=rbind(dt,dTime)      
        }
}
#class(tdmlist)=class(tdm)
print("saving...")
saveRDS(dt,"dtime.rds")
saveRDS(tdmlist,"tdmlist.rds")
dtt=dt
dtt$read=(as.numeric(dt$read))
dtt$scale=3*6319*2^(as.numeric(dt$scale)-1)
dtt$process=as.numeric(dt$process)
library(ggplot2)
summary(lm(data=dtt,formula= read ~I(scale+process)))
g=ggplot(data=dtt)+geom_point(aes(y=(as.numeric(read)),x=as.numeric(scale)))+
        geom_smooth(aes(y=(as.numeric(read)),x=as.numeric(scale)),method="lm")+
        geom_point(aes(y=(as.numeric(process)),x=as.numeric(scale)),pch=2)+
        geom_smooth(aes(y=(as.numeric(process)),x=as.numeric(scale)),method="lm")+
        geom_point(aes(y=(as.numeric(process+read)),x=as.numeric(scale)),pch=3)+
        geom_smooth(aes(y=(as.numeric(process+read)),x=as.numeric(scale)),method="lm")
plot(g)
id=1;st=c();for(d in tdmlist){s=data.frame(value=sort(-slam::row_sums(d))[1:20]);s$variable=rownames(s);s$order=1:20;s$value=-s$value;s$id=id;id=id+1;st=rbind(st,s)};st=data.table(st)
stf=st[variable %in% st[id==8 & order>5]$variable & order < 5,]
ggplot(data=stf,aes(y=order,x=id,group=as.factor(variable),colour=as.factor(variable)))+geom_point()+geom_line()

#tdm[figgplot2Terms(tdm,2,10)[1:num],] %>%
 #       as.matrix() %>%
  #      rowSums()

#sort(-slam::row_sums(tdm))[1:10]
#sort(-slam::row_sums(tdm[grep("^just",tdm$dimnames$Terms),]))[1:10]
