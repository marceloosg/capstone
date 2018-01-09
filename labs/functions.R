library(tm)
library(RWeka)

library(data.table)
TrigramTokenizer <- function(x) NGramTokenizer(x, 
                                               Weka_control(min = 3, max = 3))
BigramTokenizer <- function(x) NGramTokenizer(x, 
                                               Weka_control(min = 2, max = 2))
MonogramTokenizer <- function(x) NGramTokenizer(x, 
                                               Weka_control(min = 1, max = 1))
filename="final/en_US/en_US.blogs.txt"
nskip=0
#output=unlist(lapply(output[[1]], function(x) strsplit(split=" +[0-9] +",x)))

getCorpus=function(filename,nskip=0,lines=10000){
        output=scan(file=filename,what=list(""),sep="\n",skip = nskip,nmax=lines,
            quote=NULL)
        if(length(output[[1]])==0) return(NULL)
        
        output=lapply(output,tolower)
        output=lapply(output, function(x) gsub("(’|')","",x))
        output=lapply(output, function(x) gsub("(‘|”|“|\\.|,|\\(|\\)|-|/|!+|\\$+|\\*|\\+|_|\\{|
                                               \\}|\\[|\\]|\\:|;|\\?|\\||\\@|–)","",x))
        output=lapply(output, function(x) gsub("\\","",x,fixed=T))
        output=lapply(output, function(x) gsub("[0-9]+"," (number) ",x))
        output=lapply(output, function(x) gsub(" +"," ",x))
        output=lapply(output, function(x) gsub("^ ","",x))
        tm_map(Corpus(VectorSource(output)), removeWords, stopwords("english"))
}

getFreq=function(ovid,token=MonogramTokenizer,top=30000){
        tdm=TermDocumentMatrix(ovid,control = 
                                       list(   tokenizer=token, 
                                               wordLengths=c(3,20)))
        values=slam::row_sums(tdm)
        variables=names(values)
        s=data.table(variables=variables,values=values)
        setorder(s,-values)
        top=min(dim(s)[1],top)
        s=s[1:top,]
#        s$variable=rownames(s)
#        s$value=-s$value
        s
}
getAllFreq=function(filename,lines=100000){
        nskip=0
        mono=data.table(variables=NA,values=NA)
        mono=mono[-1]
        bi=data.table(variables=NA,values=NA)
        bi=bi[-1]
        con=file(filename,"r")
        repeat{
                times=list()
                a=Sys.time()
                ovid=getCorpus(con,nskip,lines )
                b=Sys.time()
                times$corpus=b-a
                if(is.null(ovid)) {
                        setorder(mono,-values)
                        setorder(bi,-values)
                        return(list(mono,bi))
                        break
                }
                a=Sys.time()
                mono=rbind(mono,getFreq(ovid,token = MonogramTokenizer))
                mono=mono[,.(values=sum(values)),by=variables]
                b=Sys.time()
                times$mono=b-a
                a=Sys.time()
                bi=rbind(bi,getFreq(ovid,token = BigramTokenizer))
                bi=bi[,.(values=sum(values)),by=variables]
                b=Sys.time()
                times$bi=b-a
#                nskip=nskip+lines
                sizes=data.table(corpus=object.size(ovid),
                                 mono=object.size(mono),
                                 bi=object.size(bi))
                print(sizes)
                print(as.data.table(times))
        }
        close(con)
setorder(mono,-values)
setorder(bi,-values)
list(mono,bi)
}