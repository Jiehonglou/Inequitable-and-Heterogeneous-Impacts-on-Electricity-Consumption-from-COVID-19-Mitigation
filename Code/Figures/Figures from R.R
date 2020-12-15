######################################################################
## Creating figures for the paper of Covid project 
## AUTHOR:            Jiehong Lou
## DATE of CREATION:  Dec 14, 2020
######################################################################

require(readstata13)
require(grid)
require(ggplot2)
require(gridExtra)
require(plyr)
require(xlsx)
setwd("Project/Covid")

### figure 2A
jpeg("f2A.jpg",width = 4.3, height=5, units="in",res=300) 
data<-read.xlsx("income_sep.xlsx", "pew")
pd = position_dodge(.1) 
data$key<-factor(data$key, 
                 levels=c("Minority","White","High income","Middle income","Low income","Overall"), order=T) 

ggplot(data,            
       aes(x     = beta,
           y     = key,
           color = state))+
  
  geom_point(shape = 16,
             size  = 3.5,
             position = pd) +  
  scale_color_manual(values = c("#FF7F00", "#0c0cae"))+
  
  geom_errorbar(aes(xmin  = beta - 1.96*se,
                    xmax  = beta + 1.96*se),
                width = 0.0,
                size  = 1.2,
                position = pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("")+
  xlab("% Changes in hourly electricity consumption")+
  geom_vline(xintercept=0, linetype="dashed", size=0.5)
dev.off()

### figure 2B
jpeg("f2B.jpg",width = 4.9, height=5, units="in",res=300) 
data<-read.xlsx("income_sep.xlsx", "Sheet3")
pd = position_dodge(.3) 
data$key<-factor(data$key, 
                 levels=c("Hispanic_high income", "Hispanic_low income", "Other_high income", "Other_low income",  "White_high income", "White_low income")) 

ggplot(data,
       aes(x     = beta,
           y     = key,
           color = policy)) +
  
  geom_point(shape = 16,
             size  = 3.5,
             position = pd) +  
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  
  geom_errorbar(aes(xmin  = beta - 1.96*se,
                    xmax  = beta + 1.96*se),
                width = 0.0,
                size  = 1.2,
                position = pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("")+
  xlab("% Changes in hourly electricity consumption") +
  geom_vline(xintercept=0, linetype="dashed", size=0.5)

dev.off()

### figure 5A
jpeg("f5A.jpg",width = 4.7, height=6, units="in",res=300) 
data3<-read.xlsx("income_sep.xlsx", "Sheet6")
pd = position_dodge(.2) 
data$key<-factor(data3$key, 
                 levels=c("Overall business", "Non-impacted business", "Impacted business"), order=T) 

ggplot(data3,
       aes(x     = beta,
           y     = key,
           color = policy)) +  
  
  geom_point(shape = 16,
             size  = 3.5,
             position = pd) +  
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  
  geom_errorbar(aes(xmin  = beta - 1.96*se,
                    xmax  = beta + 1.96*se),
                width = 0.0,
                size  = 1.2,
                position = pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("")+
  xlab("% Changes in hourly electricity consumption") +
  geom_vline(xintercept=0, linetype="dashed", size=0.5)

dev.off()

### figure 5B
jpeg("f5B.jpg",width = 5.9, height=6, units="in",res=300) 
data<-read.xlsx("income_sep.xlsx", "Sheet5")
pd = position_dodge(.3) 
data$key<-factor(data$key, 
                 levels=c("Wholesale Trade", "Utilities", "Transportation and Warehousing", "Retail Trade",  "Real Estate Rental and Leasing", 
                          "Public Administration","Professional, Scientific, and Tech…","Other Services","Mining","Manufacturing",
                          "Information","Health Care and Social Assistance","Finance and Insurance","Educational Services","Construction",
                          "Arts, Entertainment, and Recreation","Agriculture, Forestry, Fishing…",
                          "Administrative and Support and Waste…","Accommodation and Food Services","Overall"), order=T) 

ggplot(data,
       aes(x     = beta,
           y     = key,
           color = policy)) +
  
  geom_point(shape = 16,
             size  = 3.5,
             position = pd) +  
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  
  geom_errorbar(aes(xmin  = beta - 1.96*se,
                    xmax  = beta + 1.96*se),
                width = 0.0,
                size  = 1.2,
                position = pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("")+
  xlab("% Changes in hourly electricity consumption") +
  geom_vline(xintercept=0, linetype="dashed", size=0.5)

dev.off()


### figure 6
jpeg("f6.jpg",width = 5.9, height=6, units="in",res=300) 
data<-read.xlsx("income_sep.xlsx", "size_two_com")
pd = position_dodge(.3) 
data$key<-factor(data$key, 
                 levels=c("Wholesale Trade", "Utilities", "Transportation and Warehousing", "Retail Trade",  "Real Estate Rental and Leasing", 
                          "Public Administration","Professional, Scientific, and Tech…","Other Services","Manufacturing",
                          "Information","Health Care and Social Assistance","Finance and Insurance","Educational Services","Construction",
                          "Arts, Entertainment, and Recreation","Agriculture, Forestry, Fishing…",
                          "Administrative and Support and Waste…","Accommodation and Food Services","Overall"), order=T) 

ggplot(data,
       aes(x     = beta,
           y     = key,
           color = size)) +
  
  geom_point(shape = 16,
             size  = 3.5,
             position = pd) +  
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  
  geom_errorbar(aes(xmin  = beta - 1.96*se,
                    xmax  = beta + 1.96*se),
                width = 0.0,
                size  = 1.2,
                position = pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("")+
  xlab("% Changes in hourly electricity consumption") +
  geom_vline(xintercept=0, linetype="dashed", size=0.5)
dev.off()

### Supplementary Figure 12
jpeg("SF12.jpg",width = 4.3, height=5, units="in",res=300) 
data<-read.xlsx("income_sep.xlsx", "IL_incomecolor")
pd = position_dodge(.3) 
data$key<-factor(data$key, 
                 levels=c("White_high income", "White_low income","Minority_high income", "Minority_low income")) 

ggplot(data,
       aes(x     = beta,
           y     = key,
           color = policy)) +
  
  geom_point(shape = 16,
             size  = 3.5,
             position = pd) +  
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  
  geom_errorbar(aes(xmin  = beta - 1.96*se,
                    xmax  = beta + 1.96*se),
                width = 0.0,
                size  = 1.2,
                position = pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("")+
  xlab("% Changes in hourly electricity consumption") +
  geom_vline(xintercept=0, linetype="dashed", size=0.5)

dev.off()

###  Supplementary Figure 2A
jpeg("SF2A.jpg",width = 6.5, height=5, units="in",res=300) 
data<-read.xlsx("Datascript.xlsx", "A")

data$key<-factor(data$Feature, 
                 levels=c("Household members"), order=T) 

ggplot(data,            
       aes(x     = key,
           y     = Value,
           color = Class))+
  
  geom_point(shape = 16,
             size  = 3.5) +  
  scale_color_manual(values = c("#7105fa", "#C077FD","#FA7105", "#EBC547","#050505", "#8f8d8b"))+
  
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text=element_text(size=12),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Mean")+
  xlab("Features of Household")
dev.off()

###  Supplementary Figure 2B
jpeg("SF2B.jpg",width = 6.5, height=5, units="in",res=300) 
data<-read.xlsx("Datascript.xlsx", "B")

data$key<-factor(data$Feature, 
                 levels=c("House age"), order=T) 

ggplot(data,            
       aes(x     = key,
           y     = Value,
           color = Class))+
  
  geom_point(shape = 16,
             size  = 3.5) +  
  scale_color_manual(values = c("#7105fa", "#C077FD","#FA7105", "#EBC547","#050505", "#8f8d8b"))+
  
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text=element_text(size=12),
        legend.text = element_text(size = 12),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Mean")+
  xlab("Features of Household")
dev.off()

###  Supplementary Figure 2C
jpeg("SF2C.jpg",width = 12, height=5, units="in",res=300) 
data<-read.xlsx("Datascript.xlsx", "C")

data$key<-factor(data$Feature, 
                 levels=c("Thermostats","Programmable thermostats","Programmable auto thermostats"), order=T) 

ggplot(data,            
       aes(x     = key,
           y     = Value,
           color = Class))+
  
  geom_point(shape = 16,
             size  = 3.5) +  
  scale_color_manual(values = c("#7105fa", "#C077FD","#FA7105", "#EBC547","#050505", "#8f8d8b"))+
  
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text=element_text(size=12),
        legend.text = element_text(size = 12),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Mean")+
  xlab("Features of Household")
dev.off()
###  Supplementary Figure 2D

jpeg("SF2D.jpg",width = 12, height=5, units="in",res=300) 
data<-read.xlsx("Datascript.xlsx", "D")

data$key<-factor(data$Feature, 
                 levels=c("Dryer","Frige","TV","Computers","Internet",
                          "Desktop","Laptop","Tablet","Cellphone","Smartphone","Story"), order=T) 

ggplot(data,            
       aes(x     = key,
           y     = Value,
           color = Class))+
  
  geom_point(shape = 16,
             size  = 3.5) +  
  scale_color_manual(values = c("#7105fa", "#C077FD","#FA7105", "#EBC547","#050505", "#8f8d8b"))+
  
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=12),
        legend.text = element_text(size = 12),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Mean")+
  xlab("Features of Household")
dev.off()

###  Supplementary Figure 11A
residential_statsby<-read.dta13("AZschool_residential.dta")

residential_statsby_analysis_school<-ggplot()
residential_statsby_analysis_school<-residential_statsby_analysis_school+geom_line(data=residential_statsby, 
                                                                                   aes(x=rank_school, y=b_order), color="red", size=2)
residential_statsby_analysis_school<-residential_statsby_analysis_school+geom_line(data =residential_statsby, aes(x=rank_school, y=CI_l_order), color="grey", linetype="dotted", size=1.5)
residential_statsby_analysis_school<-residential_statsby_analysis_school+geom_line(data =residential_statsby, aes(x=rank_school, y=CI_u_order), color="grey", linetype="dotted", size=1.5)
residential_statsby_analysis_school<-residential_statsby_analysis_school+theme_bw()+theme(axis.text.x = element_text(angle = 90, lineheight=0.5, hjust = 1, vjust=0.5),
                                                                                          text = element_text(size=25),
                                                                                          legend.text=element_text(size=25),
                                                                                          panel.grid.major = element_blank(),
                                                                                          panel.grid.minor = element_blank(),
                                                                                          strip.background = element_blank(),
                                                                                          strip.placement = "outside",
                                                                                          legend.position="bottom")
residential_statsby_analysis_school<-residential_statsby_analysis_school+labs(x="Households",y ="Houly electricity consumption log(kWh), School")
residential_statsby_analysis_school<-residential_statsby_analysis_school+ geom_hline(data=residential_statsby, aes(yintercept=0),  col="black", linetype="dashed")
residential_statsby_analysis_school


residential_statsby<-read.dta13("AZorder_residential.dta")
residential_statsby_analysis_order<-ggplot()
residential_statsby_analysis_order<-residential_statsby_analysis_order+geom_line(data=residential_statsby, 
                                                                                 aes(x=rank_order, y=b_order), color="red", size=2)
residential_statsby_analysis_order<-residential_statsby_analysis_order+geom_line(data =residential_statsby, aes(x=rank_order, y=CI_l_order), color="grey", linetype="dotted", size=1.5)
residential_statsby_analysis_order<-residential_statsby_analysis_order+geom_line(data =residential_statsby, aes(x=rank_order, y=CI_u_order), color="grey", linetype="dotted", size=1.5)
residential_statsby_analysis_order<-residential_statsby_analysis_order+theme_bw()+theme(axis.text.x = element_text(angle = 90, lineheight=0.5, hjust = 1, vjust=0.5),
                                                                                        text = element_text(size=25),
                                                                                        legend.text=element_text(size=25),
                                                                                        panel.grid.major = element_blank(),
                                                                                        panel.grid.minor = element_blank(),
                                                                                        strip.background = element_blank(),
                                                                                        strip.placement = "outside",
                                                                                        legend.position="bottom")
residential_statsby_analysis_order<-residential_statsby_analysis_order+labs(x="Households",y ="Houly electricity consumption log(kWh), Order")
residential_statsby_analysis_order<-residential_statsby_analysis_order+ geom_hline(data=residential_statsby, aes(yintercept=0),  col="black", linetype="dashed")
residential_statsby_analysis_order

residential_statsby_analysis <- grid.arrange(residential_statsby_analysis_school,
                                             residential_statsby_analysis_order,
                                             nrow=1)

ggsave("SF11A.jpg", residential_statsby_analysis, width = 16.2, height=10, units="in")

jpeg(filename = "SF11A.jpg",width = 16.2, height=10, units="in", res=300)
residential_statsby_analysis
dev.off()

###  Supplementary Figure 11B
ommercial_statsby<-read.dta13("AZschool_commmercial.dta")

commercial_statsby_analysis_school<-ggplot()
commercial_statsby_analysis_school<-commercial_statsby_analysis_school+geom_line(data=commercial_statsby, 
                                                                                 aes(x=rank_school, y=b_order), color="red", size=2)
commercial_statsby_analysis_school<-commercial_statsby_analysis_school+geom_line(data =commercial_statsby, aes(x=rank_school, y=CI_l_order), color="grey", linetype="dotted", size=1.5)
commercial_statsby_analysis_school<-commercial_statsby_analysis_school+geom_line(data =commercial_statsby, aes(x=rank_school, y=CI_u_order), color="grey", linetype="dotted", size=1.5)
commercial_statsby_analysis_school<-commercial_statsby_analysis_school+theme_bw()+theme(axis.text.x = element_text(angle = 90, lineheight=0.5, hjust = 1, vjust=0.5),
                                                                                        text = element_text(size=25),
                                                                                        legend.text=element_text(size=25),
                                                                                        panel.grid.major = element_blank(),
                                                                                        panel.grid.minor = element_blank(),
                                                                                        strip.background = element_blank(),
                                                                                        strip.placement = "outside",
                                                                                        legend.position="bottom")
commercial_statsby_analysis_school<-commercial_statsby_analysis_school+labs(x="Commercials",y ="Houly electricity consumption log(kWh), School")
commercial_statsby_analysis_school<-commercial_statsby_analysis_school+ geom_hline(data=commercial_statsby, aes(yintercept=0),  col="black", linetype="dashed")
commercial_statsby_analysis_school


commercial_statsby<-read.dta13("AZorder_commercial.dta")
commercial_statsby_analysis_order<-ggplot()
commercial_statsby_analysis_order<-commercial_statsby_analysis_order+geom_line(data=commercial_statsby, 
                                                                               aes(x=rank_order, y=b_order), color="red", size=2)
commercial_statsby_analysis_order<-commercial_statsby_analysis_order+geom_line(data =commercial_statsby, aes(x=rank_order, y=CI_l_order), color="grey", linetype="dotted", size=1.5)
commercial_statsby_analysis_order<-commercial_statsby_analysis_order+geom_line(data =commercial_statsby, aes(x=rank_order, y=CI_u_order), color="grey", linetype="dotted", size=1.5)
commercial_statsby_analysis_order<-commercial_statsby_analysis_order+theme_bw()+theme(axis.text.x = element_text(angle = 90, lineheight=0.5, hjust = 1, vjust=0.5),
                                                                                      text = element_text(size=25),
                                                                                      legend.text=element_text(size=25),
                                                                                      panel.grid.major = element_blank(),
                                                                                      panel.grid.minor = element_blank(),
                                                                                      strip.background = element_blank(),
                                                                                      strip.placement = "outside",
                                                                                      legend.position="bottom")
commercial_statsby_analysis_order<-commercial_statsby_analysis_order+labs(x="Commercials",y ="Houly electricity consumption log(kWh), Order")
commercial_statsby_analysis_order<-commercial_statsby_analysis_order+ geom_hline(data=commercial_statsby, aes(yintercept=0),  col="black", linetype="dashed")
commercial_statsby_analysis_order

commercial_statsby_analysis <- grid.arrange(commercial_statsby_analysis_school,
                                            commercial_statsby_analysis_order,
                                            nrow=1)

ggsave("SF11B.jpg", commercial_statsby_analysis, width = 16.2, height=10, units="in")

jpeg(filename = "SF11B.jpg",width = 16.2, height=10, units="in", res=300)
commercial_statsby_analysis 
dev.off()

###  Supplementary Figure 11C
residential_statsby<-read.dta13("IL_school_residential.dta")

residential_statsby_analysis_school<-ggplot()
residential_statsby_analysis_school<-residential_statsby_analysis_school+geom_line(data=residential_statsby, 
                                                                                   aes(x=rank_school, y=b_order), color="red", size=2)
residential_statsby_analysis_school<-residential_statsby_analysis_school+geom_line(data =residential_statsby, aes(x=rank_school, y=CI_l_order), color="grey", linetype="dotted", size=1.5)
residential_statsby_analysis_school<-residential_statsby_analysis_school+geom_line(data =residential_statsby, aes(x=rank_school, y=CI_u_order), color="grey", linetype="dotted", size=1.5)
residential_statsby_analysis_school<-residential_statsby_analysis_school+theme_bw()+theme(axis.text.x = element_text(angle = 90, lineheight=0.5, hjust = 1, vjust=0.5),
                                                                                          text = element_text(size=25),
                                                                                          legend.text=element_text(size=25),
                                                                                          panel.grid.major = element_blank(),
                                                                                          panel.grid.minor = element_blank(),
                                                                                          strip.background = element_blank(),
                                                                                          strip.placement = "outside",
                                                                                          legend.position="bottom")
residential_statsby_analysis_school<-residential_statsby_analysis_school+labs(x="Households",y ="Houly electricity consumption log(kWh), School")
residential_statsby_analysis_school<-residential_statsby_analysis_school+ geom_hline(data=residential_statsby, aes(yintercept=0),  col="black", linetype="dashed")
residential_statsby_analysis_school


residential_statsby<-read.dta13("IL_order_residential.dta")
residential_statsby_analysis_order<-ggplot()
residential_statsby_analysis_order<-residential_statsby_analysis_order+geom_line(data=residential_statsby, 
                                                                                 aes(x=rank_order, y=b_order), color="red", size=2)
residential_statsby_analysis_order<-residential_statsby_analysis_order+geom_line(data =residential_statsby, aes(x=rank_order, y=CI_l_order), color="grey", linetype="dotted", size=1.5)
residential_statsby_analysis_order<-residential_statsby_analysis_order+geom_line(data =residential_statsby, aes(x=rank_order, y=CI_u_order), color="grey", linetype="dotted", size=1.5)
residential_statsby_analysis_order<-residential_statsby_analysis_order+theme_bw()+theme(axis.text.x = element_text(angle = 90, lineheight=0.5, hjust = 1, vjust=0.5),
                                                                                        text = element_text(size=25),
                                                                                        legend.text=element_text(size=25),
                                                                                        panel.grid.major = element_blank(),
                                                                                        panel.grid.minor = element_blank(),
                                                                                        strip.background = element_blank(),
                                                                                        strip.placement = "outside",
                                                                                        legend.position="bottom")
residential_statsby_analysis_order<-residential_statsby_analysis_order+labs(x="Households",y ="Houly electricity consumption log(kWh), Order")
residential_statsby_analysis_order<-residential_statsby_analysis_order+ geom_hline(data=residential_statsby, aes(yintercept=0),  col="black", linetype="dashed")
residential_statsby_analysis_order

residential_statsby_analysis <- grid.arrange(residential_statsby_analysis_school,
                                             residential_statsby_analysis_order,
                                             nrow=1)

ggsave("SF11C.jpg", residential_statsby_analysis, width = 16.2, height=10, units="in")

jpeg(filename = "SF11C.jpg",width = 16.2, height=10, units="in", res=300)
residential_statsby_analysis
dev.off()

###  Supplementary Figure 11D
residential_statsby<-read.dta13("IL_school_commercial.dta")

residential_statsby_analysis_school<-ggplot()
residential_statsby_analysis_school<-residential_statsby_analysis_school+geom_line(data =residential_statsby, aes(x=rank_school, y=CI_l_order), color="grey", linetype="dotted", size=1.5)
residential_statsby_analysis_school<-residential_statsby_analysis_school+geom_line(data =residential_statsby, aes(x=rank_school, y=CI_u_order), color="grey", linetype="dotted", size=1.5)
residential_statsby_analysis_school<-residential_statsby_analysis_school+geom_line(data=residential_statsby, 
                                                                                   aes(x=rank_school, y=b_order), color="red", size=2)
residential_statsby_analysis_school<-residential_statsby_analysis_school+theme_bw()+theme(axis.text.x = element_text(angle = 90, lineheight=0.5, hjust = 1, vjust=0.5),
                                                                                          text = element_text(size=25),
                                                                                          legend.text=element_text(size=25),
                                                                                          panel.grid.major = element_blank(),
                                                                                          panel.grid.minor = element_blank(),
                                                                                          strip.background = element_blank(),
                                                                                          strip.placement = "outside",
                                                                                          legend.position="bottom")
residential_statsby_analysis_school<-residential_statsby_analysis_school+labs(x="Commercials",y ="Houly electricity consumption log(kWh), School")
residential_statsby_analysis_school<-residential_statsby_analysis_school+ geom_hline(data=residential_statsby, aes(yintercept=0),  col="black", linetype="dashed")
residential_statsby_analysis_school


residential_statsby<-read.dta13("IL_school_commercial.dta")
residential_statsby_analysis_order<-ggplot()

residential_statsby_analysis_order<-residential_statsby_analysis_order+geom_line(data =residential_statsby, aes(x=rank_order, y=CI_l_order), color="grey", linetype="dotted", size=1.5)
residential_statsby_analysis_order<-residential_statsby_analysis_order+geom_line(data =residential_statsby, aes(x=rank_order, y=CI_u_order), color="grey", linetype="dotted", size=1.5)
residential_statsby_analysis_order<-residential_statsby_analysis_order+geom_line(data=residential_statsby, 
                                                                                 aes(x=rank_order, y=b_order), color="red", size=2)
residential_statsby_analysis_order<-residential_statsby_analysis_order+theme_bw()+theme(axis.text.x = element_text(angle = 90, lineheight=0.5, hjust = 1, vjust=0.5),
                                                                                        text = element_text(size=25),
                                                                                        legend.text=element_text(size=25),
                                                                                        panel.grid.major = element_blank(),
                                                                                        panel.grid.minor = element_blank(),
                                                                                        strip.background = element_blank(),
                                                                                        strip.placement = "outside",
                                                                                        legend.position="bottom")
residential_statsby_analysis_order<-residential_statsby_analysis_order+labs(x="Commercials",y ="Houly electricity consumption log(kWh), Order")
residential_statsby_analysis_order<-residential_statsby_analysis_order+ geom_hline(data=residential_statsby, aes(yintercept=0),  col="black", linetype="dashed")
residential_statsby_analysis_order

residential_statsby_analysis <- grid.arrange(residential_statsby_analysis_school,
                                             residential_statsby_analysis_order,
                                             nrow=1)

ggsave("SF11D.jpg", residential_statsby_analysis, width = 16.2, height=10, units="in")

jpeg(filename = "SF11D.jpg",width = 16.2, height=10, units="in", res=300)
residential_statsby_analysis
dev.off()


### Supplementary Figure 13 Panel A
jpeg("SF13A.jpg",width = 4.3, height=5, units="in",res=300) 
data<-read.xlsx("income_sep.xlsx", "poverty")
pd = position_dodge(.1) 
data$key<-factor(data$key, 
                 levels=c("Minority","White","High income","Middle income","Low income","Poverty","Overall"), order=T) 

ggplot(data,            
       aes(x     = beta,
           y     = key,
           color = state))+
  
  geom_point(shape = 16,
             size  = 3.5,
             position = pd) +  
  scale_color_manual(values = c("#FF7F00", "#0c0cae"))+
  
  geom_errorbar(aes(xmin  = beta - 1.96*se,
                    xmax  = beta + 1.96*se),
                width = 0.0,
                size  = 1.2,
                position = pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("")+
  xlab("% Changes in hourly electricity consumption")+
  geom_vline(xintercept=0, linetype="dashed", size=0.5)
dev.off()

### Supplementary Figure 13 Panel B
jpeg("SF13B.jpg",width = 4.9, height=5, units="in",res=300) 
data<-read.xlsx("income_sep.xlsx", "poverty_color")
pd = position_dodge(.3) 
data$key<-factor(data$key, 
                 levels=c("Hispanic_high income", "Other_high income","White_high income", "Hispanic_low income",  "Other_low income","White_low income","Hispanic_poverty","Others_poverty","White_poverty")) 

ggplot(data,
       aes(x     = beta,
           y     = key,
           color = policy)) +
  
  geom_point(shape = 16,
             size  = 3.5,
             position = pd) +  
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  
  geom_errorbar(aes(xmin  = beta - 1.96*se,
                    xmax  = beta + 1.96*se),
                width = 0.0,
                size  = 1.2,
                position = pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("")+
  xlab("% Changes in hourly electricity consumption") +
  geom_vline(xintercept=0, linetype="dashed", size=0.5)

dev.off()
