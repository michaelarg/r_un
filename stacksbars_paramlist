
library(sp)
library(RColorBrewer)
library(jpeg)
library(plyr)
library(ggplot2)
library(plotrix)


xseries<-"H:\Statistical Information\01 SDAnnual\R_graphs\Stacked bars"


folderpath<-"H:/Statistical Information/01 SDAnnual/R_graphs/"
param_source<-read.csv(paste(folderpath, "stacked bars/stacked_bars_params.csv", sep=""), header=T, sep=",", stringsAsFactors = FALSE)
param<-subset(param_source, Graph_id==xseries)
outputfile<-paste("_Outputs/SYB2015/", param$Graph_id[1],".png", sep="")

png(filename = paste(folderpath,outputfile,sep=""),
    width = 2400, height = 2600, units = "px", pointsize = 12, 
    res=300, bg = "white")

xval<-as.numeric(param$xValue)
xlab<-param$xLabel


count = 0

  for(i in 1:15) {
    f<-paste0("year_",i)
    k<-paste0("param$",f)
    j<-eval(parse(text=k))

    if(is.na(j[1]) == TRUE || j[1] == '') {
      count = count + 1
    }
  }

activeyears<-15-count
activecountries<-length(param$Country)-1

DATA_VALUE<-as.numeric(c
(
param$year_1[1:activecountries+1],
param$year_2[1:activecountries+1],
param$year_3[1:activecountries+1],
param$year_4[1:activecountries+1],
param$year_5[1:activecountries+1],
param$year_6[1:activecountries+1],
param$year_7[1:activecountries+1],
param$year_8[1:activecountries+1],
param$year_9[1:activecountries+1],
param$year_10[1:activecountries+1],
param$year_11[1:activecountries+1],
param$year_12[1:activecountries+1],
param$year_13[1:activecountries+1],
param$year_14[1:activecountries+1],
param$year_15[1:activecountries+1]
))


aone=rep(0,6)
gh<-0

countit<-function(end) {
  
  for(j in 4:6) {  
    
    a<-seq(from = 0 , to = end ,length.out= j)
    
    for(i in 2:length(a)) {
      
      if(a[i]%%floor(a[i]) == 0 ) {
        
        aone[j] = aone[j]+1
        
      }
      
    }
  }
  
 print(aone)
}

pq<-match(max(countit(30)),countit(30))


DATA_VALUE<-DATA_VALUE[1:sum(!sapply(DATA_VALUE, is.na))]

cdf<-c(param$year_1[1],param$year_2[1],param$year_3[1],param$year_4[1],param$year_5[1],param$year_6[1],param$year_7[1],param$year_8[1],param$year_9[1],param$year_10[1],param$year_11[1],param$year_12[1],param$year_13[1],param$year_14[1],param$year_15[1])
cdf<-cdf[1:activeyears]

Year<-sort(rep(cdf,activecountries))

Country<-rep(param$Country[1:activecountries+1],activeyears)

x.df<-data.frame(Year,Country,DATA_VALUE)

x1<-subset(x.df,x.df$DATA_VALUE>=0)
x1$Country=factor(x1$Country, as.character(unique(x1$Country)))

x2<-subset(x.df,x.df$DATA_VALUE<0)

round<-function(x,years) {
  
  len<-length(years)
  
  fmax<-0
  
  for(i in 1:len) {
    mtemp<-sum(x$DATA_VALUE[which(x$Year==unique(years[i]))])
    
    
    if(mtemp>fmax){
      (fmax=mtemp)
    }
    
  }
  
  return(fmax)
}

o<-round(x1,x.df$Year)

rndval<-function(x) {
  
  indic=0
  
  if(x<0) {
    
    indic=1
    x=abs(x)
  }
  
  
  xor=x
  
  if(x/100 > 1 & x/100< 10) {
    
    x=100
    
  }
  
  else if(x/1000 > 1 & x/1000< 10) {
    
    x=1000
  }
  
  else if(x/10000 > 1 & x/10000< 10) {
    
    x=1000
  }
  
  else if(x/100000 > 1 & x/100000< 10) {
    
    x=1000
  }
  
  
  else if(x/10 > 1 & x/1000< 10) {
    
    x=10
  }
  
  val = round_any(xor, x, f = ceiling)
  
  
  if(indic==1){
    
    val=-val
    
  }
  
  return(val)
}

end<-rndval(o)

if(min(x.df$DATA_VALUE >=0)) {
  
  start=0
}

if(min(x.df$DATA_VALUE) <0) {
  
  start= rndval(min(x.df$DATA_VALUE ))
  
}

ggplot() + 
  
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),

    panel.background = element_blank(),
    axis.text.x = element_text(colour="black",size=12), axis.text.y = element_text(colour="black",size=12),
    
    axis.line = element_line(color = 'black'),
    axis.ticks =element_line(colour = "black"),
    axis.title.y=element_text(vjust=0.99)
  )+

  geom_bar(data = x1, aes(x=x1$Year, y=x1$DATA_VALUE, fill=x1$Country,order=rev(Country)),stat = "identity") +
  #geom_bar(data = x2, aes(x=x2$Year, y=x2$DATA_VALUE, fill=x2$Country,order=rev(x.df$Country)), stat = "identity") +
  xlab("")+
  ylab(param$y_label)+
  scale_x_discrete(expand = c(0,0))+
  
  coord_cartesian(ylim = c(start, end)) +
  #scale_y_continuous(limits=c(start,end),breaks=seq(from = start , to = end ,length.out= pq)) + #if you would like custom y axis tick points edit this part
  
  scale_y_continuous(limits=c(start,end),breaks=c(1000,3000,5000,7000)) + #if you would like custom y axis tick points edit this part
  
  
  guides(fill=guide_legend(title=NULL))+

  scale_fill_discrete(labels=x.df$Country)


dev.off()
