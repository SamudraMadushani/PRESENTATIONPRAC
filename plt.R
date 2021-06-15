library(remotes)
library(fabletools)
library(dplyr)
library(tsibble)
library(xts)
library(forecast)
library(rlang)
library(tidyr)
library(magrittr)
library(dplyr)
library(stringr)
library(hts)
library(zoo)
library(moment)
library(tsibble)
library(lubridate)
library (data.table)
library(readr)
library(feasts)
library(thief)
library(ggplot2)
library(GGally)
library(plotly)
library(patchwork)
library(RColorBrewer)
library(pdftools)
library(here)
library(tibble)
library(data.table)
library(tsibble)
library(fabletools)
library(thief)
library(sf)
library(maps)
library(oz)
library(ozmaps)
library(readxl)
library(icons)
library(remedy)
library(gganimate)
library(magick)
library(tsfeatures)

load("F:/NEW/ABC/data/FinalData.Rda")
D<-Data_Counts
Week<-tsibble::yearweek(D$EndDate)
Date<-D$EndDate
Province<-rep(c(rep("Western",3),rep("Central",3),rep("Southern",3),rep("Nothern",5),
                rep("Eastern",3),rep("NorthWestern",2),rep("NorthCentral",2),
                rep("Uva",2),rep("Sabaragamuwa",2),rep("Eastern",1)),730)
Districts<-D$Division
Counts<-as.integer(D$Dengue)
DengueTEST<-tibble(Week,Date,Counts,Districts,Province)
DataDengueTEST<- DengueTEST%>% 
  as_tsibble(key=c(Province,Districts),index=Week)
DataDengueTEST
#Training Set
DengueTRAIN<-DengueTEST[1:17654,]
DataDengueTRAIN<-DengueTRAIN%>% 
  as_tsibble(key=c(Province,Districts),index=Week)
DataDengueTRAIN


DIS_DataDengueHR<-DataDengueTRAIN %>%aggregate_key(Province/Districts,Counts=sum(Counts))
DIS_DataDengueHR
#View(DIS_DataDengueHR)
DIS_DataDengueTEST<-DataDengueTEST %>%aggregate_key(Province/Districts,Counts=sum(Counts)) #ARRANGE TEST SET
DIS_DataDengueTEST

SLAGG<-DIS_DataDengueTEST%>%
  filter(is_aggregated(Province) & is_aggregated(Districts)) %>%
  autoplot(Counts,col="#e7298a") + labs(y = "Counts",x="Week",title=" ")

DIS_AGG_SRS<-DIS_DataDengueTEST %>%
  filter(is_aggregated(Districts)& !is_aggregated(Province)) %>%
  autoplot(Counts) +
  labs(y = "Counts",x="Week",title=" ") +
  facet_wrap(vars(Province), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")

DIS_SRS<-DIS_DataDengueTEST %>%
  filter(!is_aggregated(Districts)& !is_aggregated(Province)) %>%
  autoplot(Counts) +
  labs(y = "Counts",x="Week",title=" ") +
  facet_wrap(vars(Province), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")

EAS<-DIS_DataDengueTEST %>%
  filter(!is_aggregated(Districts)& !is_aggregated(Province)) %>%
  filter(Province=="Eastern") %>%
  autoplot(Counts) +
  labs(y = "Counts",x="Week",title=" ") +
  facet_wrap(vars(Province), scales = "free_y", ncol = 3) +
  theme(legend.position = "bottom")

Dengue_Districts <- DataDengueTEST%>%
  group_by(Districts) %>%
  summarise(Counts = sum(Counts))

cp<-Dengue_Districts%>%autoplot(Counts)+labs(y = "Counts",x="Week")
cp<-cp+theme(legend.position="bottom")

D<-Data_Counts
D<-D%>%filter(Week!=53)

####################################################################
P1<-Dengue_Districts%>% filter(Districts=="Colombo")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Colombo")+ theme(text = element_text(size=6))
P2<-Dengue_Districts%>% filter(Districts=="Gampaha")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Gampaha")+ theme(text = element_text(size=6))
P3<-Dengue_Districts%>% filter(Districts=="Kalutara")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Kalutara")+ theme(text = element_text(size=6))
P4<-Dengue_Districts%>% filter(Districts=="Kandy")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Kandy")+ theme(text = element_text(size=6))
P5<-Dengue_Districts%>% filter(Districts=="Matale")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Matale")+ theme(text = element_text(size=6))
P6<-Dengue_Districts%>% filter(Districts=="NuwaraEliya")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="NuwaraEliya")+ theme(text = element_text(size=6))
P7<-Dengue_Districts%>% filter(Districts=="Galle")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Galle")+ theme(text = element_text(size=6))
P8<-Dengue_Districts%>% filter(Districts=="Hambanthota")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Hambanthota")+ theme(text = element_text(size=6))
P9<-Dengue_Districts%>% filter(Districts=="Matara")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Matara")+ theme(text = element_text(size=6))
P10<-Dengue_Districts%>% filter(Districts=="Jaffna")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Jaffna")+ theme(text = element_text(size=6))
P11<-Dengue_Districts%>% filter(Districts=="Kilinochchi")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Kilinochchi")+ theme(text = element_text(size=6))
P12<-Dengue_Districts%>% filter(Districts=="Mannar")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Mannar")+ theme(text = element_text(size=6))
P13<-Dengue_Districts%>% filter(Districts=="Vavuniya")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Vavuniya")+ theme(text = element_text(size=6))
P14<-Dengue_Districts%>% filter(Districts=="Mullaitivu")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Mullaitivu")+ theme(text = element_text(size=6))
P15<-Dengue_Districts%>% filter(Districts=="Batticaloa")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Batticaloa")+ theme(text = element_text(size=6))
P16<-Dengue_Districts%>% filter(Districts=="Ampara")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Ampara")+ theme(text = element_text(size=6))
P17<-Dengue_Districts%>% filter(Districts=="Trincomalee")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Trincomalee")+ theme(text = element_text(size=6))
P18<-Dengue_Districts%>% filter(Districts=="Kurunagala")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Kurunagala")+ theme(text = element_text(size=6))
P19<-Dengue_Districts%>% filter(Districts=="Puttalam")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Puttalam")+ theme(text = element_text(size=6))
P20<-Dengue_Districts%>% filter(Districts=="Anuradhapura")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Anuradhapura")+ theme(text = element_text(size=6))
P21<-Dengue_Districts%>% filter(Districts=="Polonnaruwa")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Polonnaruwa")+ theme(text = element_text(size=6))
P22<-Dengue_Districts%>% filter(Districts=="Badulla")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Badulla")+ theme(text = element_text(size=6))
P23<-Dengue_Districts%>% filter(Districts=="Monaragala")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Monaragala")+ theme(text = element_text(size=6))
P24<-Dengue_Districts%>% filter(Districts=="Ratnapura")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Ratnapura")+ theme(text = element_text(size=6))
P25<-Dengue_Districts%>% filter(Districts=="Kegalle")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Kegalle")+ theme(text = element_text(size=6))
P26<-Dengue_Districts%>% filter(Districts=="Kalmunei")%>% autoplot(Counts) +
  labs(y = "Counts",x="Week",title="Kalmunei")+ theme(text = element_text(size=6))
aq1<-(P1/P2/P3/P4/P5)
aq2<-(P6/P7/P8/P9/P10)
aq3<-(P11/P12/P13/P14/P15)
aq4<-(P16/P17/P18/P19/P20)
aq5<-(P21/P22/P23/P24/P25)


####################################################################
Dengue_Disease<-D[,c(5,6,7)]
Dengue_Disease_tsbl<-Dengue_Disease%>%as_tsibble(key=Division,index=EndDate)
Weekly_Dengue<-Dengue_Disease_tsbl%>%
  index_by(WD=~yearweek(.))%>%
  summarise(Dengue=sum(Dengue))
Dengue_total_data<-c(Weekly_Dengue$Dengue)

Dengue_total_data<- ts(Dengue_total_data, freq=52, start=c(2006,52))

aggts_total = tsaggregates(Dengue_total_data)
W<-autoplot(aggts_total[[1]],col="#e7298a") + labs(y = " ",x=" ",title="Weekly")
BW<-autoplot(aggts_total[[2]],col="#1b9e77") + labs(y = " ",x=" ",title="Bi-weekly")
M<-autoplot(aggts_total[[3]],col="#d95f02") + labs(y = " ",x=" ",title="Monthly")
Q<-autoplot(aggts_total[[4]],col="#7570b3") + labs(y = " ",x=" ",title="Quarterly")
Y<-autoplot(aggts_total[[5]],col="#66a61e") + labs(y = " ",x=" ",title="Yearly")
TEM<-(W/BW/M/Q/Y)

TEM1<-(W|BW)/(M|Q)/(Y)

TEM31<-(W|M|Q|Y)
TEM3<-(W|M)
TEM4<-(Q|Y)

ss<-window(aggts_total[[1]],start=c(2010,1),end=c(2010,52))

WS<-autoplot(ss,col="#e7298a") + labs(y = " ",x=" ",title="weekly series-2010")

A<-DIS_DataDengueTEST %>%
  filter(is_aggregated(Districts)& !is_aggregated(Province)) %>%
  autoplot(Counts) +
  labs(y = "Counts",x="Week",title=" ")+
  theme(legend.position = "none")

B<-DIS_DataDengueTEST %>%
  filter(!is_aggregated(Districts)& !is_aggregated(Province)) %>%
  autoplot(Counts) +
  labs(y = "Counts",x="Week",title=" ")+
  theme(legend.position = "none")

A<-(SLAGG/A/B)

#############################################################################


DENVSCOVID <- read_excel("data/DENVSCOVID.xlsx")
DENVSCOVID$Date<-as.Date(DENVSCOVID$Date)
DENVSCOVID<- DENVSCOVID%>% as_tsibble(index=Date)
DENVSCOVID<-DENVSCOVID%>% index_by(WEEK=~yearweek(.))
DENVSCOVID_DATA<-data.frame(DENVSCOVID)


CVsD<-ggplot(data = DENVSCOVID_DATA, aes(x=Date, y=value, color=Disease )  )             +
  ylab('Counts')                                 +
  geom_line(aes(y=Covid.19 , col='Covid'), size=1, alpha=.5)   +
  geom_line(aes(y=Dengue, col='Dengue'),  size=1, alpha=.5)   +
  theme(legend.position=c(.1,.85))

###############################################################################
Province <- read_excel("F:/NEW/ABC/Province.xlsx")
sf_sl <- read_sf("F:/NEW/ABC/mapdata/lka_admbnda_adm2_slsd_20200305.shp") %>%
  select(ADM2_EN, geometry)
sf_sl <- sf_sl %>% st_transform(5235)
sf_sl<-sf_sl%>%mutate(ADM2_EN  = case_when(ADM2_EN  ==  "[unknown]" ~" ",TRUE ~ ADM2_EN))
sf_sl <- sf_sl %>%
  mutate(DISTRICT = str_to_upper(ADM2_EN)) %>%
  select(-ADM2_EN)
districts_sl <- left_join(sf_sl, Province,by = c("DISTRICT"))

DisMap1<-ggplot(districts_sl) +
  geom_sf(aes(fill = PROVINCE))+
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+theme(legend.position = "none")

DisMap1<-DisMap1+ scale_fill_manual(breaks = c("Central", "Eastern", "North Central","Northern","North Western","Sabaragamuwa","Southern","Uva","Western"),
                                    values=c("#377eb8","#377eb8","#377eb8","#377eb8","#377eb8","#377eb8","#377eb8","#377eb8","#377eb8"))

###############################################################################
Province <- read_excel("F:/NEW/ABC/Province.xlsx")
sf_sl <- read_sf("F:/NEW/ABC/mapdata/lka_admbnda_adm2_slsd_20200305.shp") %>%
  select(ADM2_EN, geometry)
sf_sl <- sf_sl %>% st_transform(5235)
sf_sl<-sf_sl%>%mutate(ADM2_EN  = case_when(ADM2_EN  ==  "[unknown]" ~" ",TRUE ~ ADM2_EN))
sf_sl <- sf_sl %>%
  mutate(DISTRICT = str_to_upper(ADM2_EN)) %>%
  select(-ADM2_EN)
districts_sl <- left_join(sf_sl, Province,by = c("DISTRICT"))

DisMap2<-ggplot(districts_sl) +
  geom_sf(aes(fill = PROVINCE))+
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

DisMap2<-DisMap2+ scale_fill_manual(breaks = c("Central", "Eastern", "North Central","Northern","North Western","Sabaragamuwa","Southern","Uva","Western"),
                                    values=c("#984ea3","#377eb8","#4daf4a","#ff7f00","#ffff33","#f781bf","#e7298a","#d95f02","#b2df8a"))
###############################################################################
Province <- read_excel("F:/NEW/ABC/Province.xlsx")
sf_sl <- read_sf("F:/NEW/ABC/mapdata/lka_admbnda_adm2_slsd_20200305.shp") %>%
  select(ADM2_EN, geometry)
sf_sl <- sf_sl %>% st_transform(5235)
sf_sl<-sf_sl%>%mutate(ADM2_EN  = case_when(ADM2_EN  ==  "[unknown]" ~" ",TRUE ~ ADM2_EN))
sf_sl <- sf_sl %>%
  mutate(DISTRICT = str_to_upper(ADM2_EN)) %>%
  select(-ADM2_EN)
districts_sl <- left_join(sf_sl, Province,by = c("DISTRICT"))

DisMap4<-ggplot(districts_sl) +
  geom_sf(aes(fill = DISTRICT))+
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

#DisMap4<-DisMap4+ scale_fill_manual(breaks = c("AMPARA","ANURADHAPURA","BADULLA","BATTICALOA","COLOMBO","GALLE","GAMPAHA","HAMBANTOTA","JAFFNA"      
#                                              ,"KALUTARA","KANDY","KEGALLE","KILINOCHCHI","KURUNEGALA","MANNAR","MATALE","MATARA","MONARAGALA","MULLAITIVU"  
#                                             , "NUWARA ELIYA","POLONNARUWA","PUTTALAM","RATNAPURA","TRINCOMALEE", "VAVUNIYA") ,
#                                 values=c("#88419d","#377eb8","#4daf4a","#66c2a4","#ffff33","#f781bf","#e7298a","#d95f02","#b2df8a",
#                                         "#984ea3","#377eb8","#4daf4a","#ff7f00","#ffff33","#b2e2e2","#e7298a","#d95f02","#b2df8a",
#                                        "#984ea3","#377eb8","#4daf4a","#ff7f00","#ffff33","#f781bf","#e7298a","#d95f02","#b2df8a","#b2df8a"))


######################################################

y <- c(56.73,65.29,68.43,74.32,79.36,80.6,82.4,88.02,88.9,91,92.02,94,95.03,96,98, 100,102,104,104.2,107,109,112,115,119,121,121.01,124)
y<-ts(y,start=c(2012,1),frequency = 12)

trs<-autoplot(y,col="#238b45") + labs(y = " ",x=" ")

stft<-tsfeatures(y)

trsF<-stft%>%  ggplot(aes(x = trend, y = seasonal_strength)) +
  geom_point() +
  labs(y = "Seasonality strength", x="Trend strength")+ 
  theme(aspect.ratio=1)

#########################################################################

srs<-autoplot(AirPassengers,col="#238b45") + labs(y = " ",x=" ")

sstf<-tsfeatures(AirPassengers)

srsF<-sstf%>%  ggplot(aes(x = trend, y = seasonal_strength)) +
  geom_point() +
  labs(y = "Seasonality strength", x="Trend strength")+ 
  theme(aspect.ratio=1)

############################################################################

library(ceylondata)
library(sp)
library(viridis)
data(sf_sl_0)
SLPLOT<-ggplot(sf_sl_0) + geom_sf(mapping = aes(fill = COUNTRY), show.legend = FALSE)+
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

PWPLOT<-ggplot(province) + geom_sf(mapping = aes(fill = PROVINCE), show.legend = TRUE)+
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

DisMap4M<-ggplot(districts_sl) +
  geom_sf(aes(fill = DISTRICT), show.legend = FALSE)+
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

PWPLOTM<-ggplot(province) + geom_sf(mapping = aes(fill = PROVINCE), show.legend = FALSE)+
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

SL_season<-DIS_DataDengueTEST%>%
  filter(is_aggregated(Province) & is_aggregated(Districts))%>%
  gg_season(Counts) +
  labs(y = "Counts", x="Week")+
  theme(legend.position = "right") 

#####################################################################


library(stats)
library(MARSS)
library(forecast)
library(datasets)

data("Seatbelts")
data("AirPassengers")

wk<-DIS_DataDengueTEST$Week[1:144]
wk<-as.Date(wk)

A<-c(AirPassengers)
B<-c(10*Seatbelts[,7])[1:144]
D<-c(Seatbelts[,5]/25)[1:144]
C<-c(seq(10,582,by=4))
pp<-data.frame(wk,A,B,C)

data_long <- melt(pp, id.vars = "wk")
colnames(data_long)<-c("Year","series","value")
TSFEAT<-ggplot(data_long,                            
               aes(x = Year,
                   y = value,
                   col = series)) +
  geom_line()

A.ts<-ts(c(AirPassengers),start=c(2007,1),frequency=52)
A.FT<-tsfeatures(A.ts)
B.ts<-ts(c(10*Seatbelts[,7])[1:144],start=c(2007,1),frequency=52)
B.FT<-tsfeatures(B.ts)
C.ts<-ts(c(seq(10,582,by=4)),start=c(2007,1),frequency=52)
C.FT<-tsfeatures(C.ts)
series<-c("A","B","C")
seasonality<-c(A.FT$seasonal_strength,B.FT$seasonal_strength,C.FT$seasonal_strength)
trend<-c(A.FT$trend,B.FT$trend,C.FT$trend)
sstf1<-data.frame(series,seasonality,trend)
sstf1<-as_tibble(sstf1)

srsF1<-sstf1%>%  ggplot(aes(x = trend, y = seasonality,col = series), show.legend = FALSE) +
  geom_point() +
  labs(y = "seasonality", x="trend")+ 
  theme(aspect.ratio=1)
