library(tm)
library(ggplot2)
library(data.table)
library(RWeka)
library(xml2)
library(SnowballC)  
lang="english"
source('getTDM.R')
path='final/en_US/'
size=1000
freq=getFrequencies(path,size,maxcount = 10,maxsamples = 5)
st=getPercentages(freq)
ggplot(st,aes(x=TotalWords,y=TargetWords,color=factor(Percentage)))+geom_point()
ggplot(st[TotalWords>60000],aes(x=1/TotalWords,y=TargetWords,color=factor(Percentage)))+geom_point()+
        scale_x_continuous(limits=c(0,1/60000))


coefs=lapply(unique(st$Percentage),function(P) 
        lm(TargetWords ~ 1/TotalWords,data= st[lines>4000][Percentage==P]))

z=st[TotalWords>60000 ]

t=rbindlist(lapply(
unique(as.character(z$Percentage)),function(P){
c=coefficients(summary(lm(log(TargetWords) ~ log(1+1/TotalWords),data=z[Percentage==P])))
data.table(P=P,v=c[1],err=c[3],p=c[7])        
}))


frequent_word_distribuition=t[,.(Percentage=P,Words=exp(v),error=err*exp(v),p)]
ggplot(frequent_word_distribuition,aes(x=(as.numeric(Percentage)),y=(Words),ymin=(Words-error),ymax=(Words+error)))+geom_point()

ggplot(frequent_word_distribuition[Percentage>0.1],aes(x=log(as.numeric(Percentage)),y=log(Words),ymin=log(Words-error),ymax=log(Words+error)))+geom_point()+
        geom_errorbar()+geom_smooth(method="lm")

summary(lm(y~x,
        data=frequent_word_distribuition[Percentage>0.1,
                                         .(x=log(as.numeric(Percentage)),y=log(Words))]
        ))
                                      
