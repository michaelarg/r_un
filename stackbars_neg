#stacked bars
library(ggplot2)
library(png)

library(RODBC)
stw<-odbcConnect("STW")


#folderpath<-"H:/Statistical Information/01 SDAnnual/R_graphs/" 
#outputfile<-paste("_Outputs/SYB2015/", as.character(param$Graph_id),".png", sep="")
#png(filename = paste(folderpath,outputfile,sep=""), width = 2400, height = 1550, units = "px", pointsize = 12, res=300, bg = "white")


png(filename="stacktestneg.png", width = 2400, height = 2600, units = "px", pointsize = 12, res=300, bg = "white")

# 1) Here user must enter the Indicator ID and Unit ID, for the appropriate Quantile.
# Entered on the last line of each of the following 5 sql Queries. 

construction <- sqlQuery(stw, paste("SELECT dbo.cvt_CC_Numeric_to_Alphabetic (2, Control_code) as CodeName,
                                    *  from localVW_final_report_MAIN_forR 
                                    WHERE Indicator_id =3907 AND Unit_Id = 2720"))

fossil <- sqlQuery(stw, paste("SELECT dbo.cvt_CC_Numeric_to_Alphabetic (2, Control_code) as CodeName,
                              *  from localVW_final_report_MAIN_forR 
                              WHERE Indicator_id =3908 AND Unit_Id = 2720"))

biomass <- sqlQuery(stw, paste("SELECT dbo.cvt_CC_Numeric_to_Alphabetic (2, Control_code) as CodeName,
                               *  from localVW_final_report_MAIN_forR 
                               WHERE Indicator_id =3906 AND Unit_Id = 2720"))

metalores <- sqlQuery(stw, paste("SELECT dbo.cvt_CC_Numeric_to_Alphabetic (2, Control_code) as CodeName,
                                 *  from localVW_final_report_MAIN_forR 
                                 WHERE Indicator_id =3909 AND Unit_Id = 2720"))

country<-"ESCAP"
years<-c("1990","1995","2000","2005","2010")


x.df<-data.frame(rbind(construction,fossil,biomass,metalores))
master<-x.df
x.df<- x.df[x.df$Year %in% years,]     
x.df = x.df[x.df$Area %in% country,] 
x.df$DATA_VALUE = x.df$DATA_VALUE/1000000

x1<-subset(x.df,DATA_VALUE>=0)
x2<-subset(x.df,DATA_VALUE<0)

ggplot() + 
  
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    #  panel.border = element_rect(fill=NA,color="black", size=0.5, 
    #  linetype="solid"),
    
    #panel.grid.major.y = element_line( size=.1, color="black" ) ,
    
    # panel.background = element_rect(colour = "black"),
    panel.background = element_blank(),
    axis.text.x = element_text(colour="black",size=12), axis.text.y = element_text(colour="black",size=12),
    
    axis.line = element_line(color = 'black'),
    axis.ticks =element_line(colour = "black")
  )+
  geom_hline(yintercept=0, size=.5)+
  geom_hline(yintercept=300, size=.5)+
  geom_hline(yintercept=600, size=.5)+
  geom_hline(yintercept=900, size=.5)+
  geom_hline(yintercept=1200, size=.5)+
  
  geom_bar(data = x1, aes(x=x1$Year, y=x1$DATA_VALUE, fill=x1$Indicator_name),stat = "identity") +
  geom_bar(data = x2, aes(x=x2$Year, y=x2$DATA_VALUE, fill=x2$Indicator_name),stat = "identity") +
  xlab("")+
  ylab("Million Tons")+
  scale_x_continuous( expand = c(0,0))+
  #(-100,1300)+
  
  #geom_hline(yintercept=0, size=.5)
  
  coord_cartesian(ylim = c(-100, 1500)) +
  scale_y_continuous(limits=c(-100,1250),breaks=c(-100,0,300,600,900,1200,1500)) +
  
  guides(fill=guide_legend(title=NULL))+ #add legend title here if you want
  scale_fill_brewer(palette = "Set2")+
  scale_fill_discrete(labels=c("Construction", "Fossil fuels", "Biomass","Metal ores"))
#scale_y_continuous(breaks=seq(round(min(x2$DATA_VALUE)),round(max(x1$DATA_VALUE)) , 0.25))


dev.off()
