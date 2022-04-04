library(tidyverse)
library(car)
library(MASS)
library(mlogit)
library(nnet)
library(foreign)
library(lubridate)
library(zoo)
select <- dplyr::select

##DATA SETS ### did not use 
getwd()

animal.df <- read_csv("reg_meas_export_cafo_2022-02-01.csv")

## Cleaning Animal DF
sum(is.na(animal.df))
animal.df%>% count(is.na(animal.df[]))
length(colnames(animal.df))
drop(animal.df['npdes_number_ca_number'])

sapply(animal.df, function(x) sum(is.na(x)))

## drop columns with too many missing values 
getwd()
for (i in 1:length(animal.df)){
  columns = (colnames(
    animal.df)[i])
  print(columns)
  column_NA = (sum(is.na(
    animal.df[,i])))
  print(column_NA)}




for (i in 1:length(animal.df)){
  columns = (colnames(animal.df)[i])
  print(columns)
  column_NA = (sum(is.na(animal.df[,i])))
  print(column_NA)
  if (column_NA > 800){animal.df[i]=NULL}}



animal.df = animal.df[!is.na(animal.df$latitude_decimal_degrees)&!is.na(animal.df$longitude_decimal_degrees),]

animal.df%>% write.csv('animal.waste.df1.csv')


### safe swim sites 

Safe_swim_sites = read_csv("sites_for_safetoswim_2022-01-31.csv")

Safe_swim_sites= Safe_swim_sites[!is.na(Safe_swim_sites$Latitude)&!is.na(Safe_swim_sites$Longitude),]
sum(is.na(Safe_swim_sites))
Safe_swim_sites%>% write.csv('Safe_to_Swim_sites.csv')


#### beaches / Water associated recreational areas  

Econ_sites = read_csv("Economically_Significant_Sites_-_OSPR_[ds356].csv")

beaches = Econ_sites[grep(pattern ='pier|beach|surfing|Coastal Access' ,x=Econ_sites$USE_,ignore.case = TRUE),]



for (i in 1:length(beaches)){
  columns = (colnames(
    beaches)[i])
  print(columns)
  column_NA = (sum(is.na(
    beaches[,i])))
  print(column_NA)}

beaches =beaches%>%filter(LATDD > 32 & LATDD <35 ) 
beach_map = beaches%>% select(LOCATION,LATDD,LONDD,USE_)
  
sum(is.na(beaches)) 

beaches = write.csv('CA_beaches.csv')



##### fecal contamination data set ########



n_distinct(fecal_m$StationName)
n_distinct(fecal_m$StationCode)
fecal_df<- read_csv("safetoswim_after2010_2022-01-31.csv")


fecal_m = fecal_df %>% filter(fecal_df$Unit=="MPN/100 mL")

fecal_m$SampleDate= as.character(fecal_m$SampleDate)

class(fecal_m$SampleDate)

21+36+16+5+4
fecal_m$SampleDate[fecal_m$SampleDate=="2202-01-14 00:00:00"]= '2022-01-14 00:00:00'
fecal_m$SampleDate=as.yearmon(fecal_m$SampleDate)

fecal_m%>% group_by(Analyte,StationName)%>% 
  summarise(total= sum(Result))%>% 
  arrange(desc(total))%>%head(10)%>% 
  mutate(ratio = (total/sum(total)) *100)%>% 
  ggplot(aes(x= StationName,y =ratio, fill = Analyte))+geom_col()



ratio_Ent = fecal_m%>%select(StationName,Analyte,SampleDate,Result)%>% 
  filter(Analyte =='Enterococcus' & SampleDate =='Jan 2022')%>% 
  mutate(b=sum(Result))%>% 
  group_by(StationName)%>% 
  mutate(n= sum(Result))%>%
  mutate(ratio = (n/b) *100)%>% arrange(desc(ratio))


ratio_coli = fecal_m%>%select(StationName,Analyte,SampleDate,Result)%>% 
  filter(Analyte =='Coliform, Total' & SampleDate =='Jan 2022')%>% 
  mutate(b=sum(Result))%>% 
  group_by(StationName)%>% 
  mutate(n= sum(Result))%>%
  mutate(ratio = (n/b) *100)%>% arrange(desc(ratio))





ratio_fec = fecal_m%>%select(StationName,Analyte,SampleDate,Result)%>% 
  filter(Analyte =='E. coli' & SampleDate =='Jan 2022')%>% 
  mutate(b=sum(Result))%>% 
  group_by(StationName)%>% 
  mutate(n= sum(Result))%>%
  mutate(ratio = (n/b) *100)%>% arrange(desc(ratio))
ratio_fec %>%select(ratio, unique('StationName'))#%>% distinct('StationName')

fecal_m = fecal_m%>%filter(TargetLatitude > 32 & TargetLatitude <35 )

total_fecal =fecal_m %>% select(StationName,Result)%>% group_by(StationName)%>%drop_na() %>%summarise(total = sum(Result))%>%arrange(desc(total))%>%head()
total_max = total_fecal %>% head(10)
total_min = total_fecal%>% tail(10)
top_fec = filter(fecal_m, fecal_m$StationName %in% unique(total_max$StationName))
top_fec %>% select(StationName,Result,SampleDate) %>% group_by(StationName)%>% ggplot(aes(Result,SampleDate, color= StationName))+geom_line()



  select(StationCode,Result,SampleDate,Analyte,Unit) %>%  
  group_by(StationCode,SampleDate)%>% 
  mutate(Resultsums= sum(Result))

top_fec %>% 
  select(StationName,Result,SampleDate) %>% 
  group_by(StationName,SampleDate)%>% 
  mutate(Resultsums= sum(Result)) %>% 
  ggplot(aes(SampleDate, log10(Resultsums), color = StationName)) +
  geom_line()+facet_wrap(~StationName)

n_distinct(fecal_m$Station)
f.line.df %>% ggplot(aes(x=SampleDate,y=Resultsums))+geom_line(aes(color = StationCode))

fecal_m =fecal_m %>% mutate(yer= format(SampleDate, '%Y'))

f_map = fecal_m %>% select(Analyte,Unit,Result,TargetLatitude,TargetLongitude,SampleDate)%>% distinct(TargetLatitude,.keep_all = TRUE)%>%drop_na()%>% filter(SampleDate > 'Jan 2020')
fecal_m %>% group_by(StationCode,StationName,SampleDate)%>%arrange(SampleDate)%>% ggplot(aes(x=SampleDate,y =Result))+ geom_line()
f_map= f_map%>%filter(TargetLatitude > 32 & TargetLatitude <35 )
  
min(f_map$TargetLatitude)
min(f_map$TargetLongitude)
max(f_map$TargetLatitude)
max(f_map$TargetLongitude)
coliform = sum(str_count(fecal_m$Analyte,'Coliform, Total'))
ecoli = sum(str_count(fecal_m$Analyte,'E. coli'))
Entero = sum(str_count(fecal_m$Analyte,'Enterococcus'))
Col_fec = sum(str_count(fecal_m$Analyte,'Coliform, Fecal'))

total.fec = length(fecal_m$Analyte)

fecal_m%>% transmute(col.total = coliform/length(Analyte),eco.total=ecoli/length(Analyte),ent.total= Entero/length(Analyte),col_fec.total= Col_fec/length(Analyte))

fecal_m %>%
  select(unique('StationName'),Analyte,Unit,Result,SampleDate)%>% 
  filter(Analyte == 'Coliform, Total'& Result > 126)%>% mutate(n= sum(Result))%>%
  group_by(StationName,SampleDate)%>% 
  mutate(b = Result/n)%>% arrange(desc(b))%>% head(15)


fecal_m %>%
  select(unique('StationName'),Analyte,Unit,Result,SampleDate)%>% 
  filter(Analyte == 'E. coli'& Result > 10)%>%filter(SampleDate=="Jan 2022")%>% mutate(n= sum(Result))%>% 
  group_by(StationName)%>% 
  mutate(b = Result/n)%>% arrange(desc(b))

fecal_m %>%
  select(unique('StationName'),Analyte,Unit,Result,SampleDate)%>% 
  filter(Analyte == 'Enterococcus'& Result > 250000)%>% mutate(n= sum(Result))%>%
  group_by(StationName,SampleDate)%>% 
  mutate(b = Result/n)%>% arrange(desc(b))%>% head(15)








fecal_m%>% select(distinct('StationName'))  
  
  
fecal_m %>% filter(StationName == 'Tijuana River MLS') %>% ggplot(aes(SampleDate,log10(Result)))+geom_line()


unique(fecal_m$Analyte)

#### stream quality data set 
spec(fecal_df)
getwd()
#stream_quality <- read_csv("California-Water-Quality-/fig.-20.-stream-quality-index-map.csv")
#stream_quality = stream_quality[grep(pattern ='impacted|stress' ,x=stream_quality$SQI ,ignore.case = TRUE),]  

stream_quality%>% ggplot(aes(x=H_AqHab),color = H_AqHab)+geom_histogram(bins = 10)
sum(is.na(stream_quality))

###################################################################

stream_quality<- read_csv("California-Water-Quality-/fig.-20.-stream-quality-index-map.csv")

###################### Visualizations ###################### 



SQI.df = stream_quality[,1:31]


SQI.df%>% group_by(Regional_board)%>% 
  count(SQI) %>%
  ggplot(aes(x=Regional_board,y= n))+ geom_col(aes(fill=SQI),position='dodge')+labs(y = '# of Streams', x= 'Region')

SQI.df%>% group_by(Regional_board)%>% filter(yr=='2016')%>% 
 count(SQI) %>%
  ggplot(aes(x=Regional_board,y= n))+ geom_col(aes(fill=SQI),position='dodge')+labs(y = '# of Streams', x= 'Region')

SQI.df%>% group_by(County)%>% ggplot(aes(x=County,y =pHab))+geom_col(aes(fill = County))

SQI.df%>% group_by(County)%>% ggplot(aes(x=County,y =IPI))+geom_col(aes(fill = County))

SQI.df%>% group_by(County)%>% ggplot(aes(x=County,y =CRAM))+geom_col(aes(fill = County))

SQI.df%>% group_by(Regional_board,yr)%>% summarise(n = mean(pOverall))%>% ggplot(aes(y=n,x=yr,color=Regional_board))+geom_line()+labs(y='likelihood of biological alteration',x ='year')


SQI.df%>% group_by(Regional_board,yr)%>% summarise(n = mean(IPI))%>% ggplot(aes(y=n,x=yr,color=Regional_board))+geom_line()+labs(y=' average IPI',x ='year')

SQI.df%>% group_by(yr,Regional_board)%>%
  summarise(avg.IPI = mean(IPI))%>%ggplot(aes(x=yr,y=avg.IPI))+geom_line(aes(color = Regional_board ))

SQI.df%>% group_by(yr)%>%
  summarise(avg.CRAM = mean(CRAM))%>%ggplot(aes(x=yr,y=avg.CRAM))+geom_line()


region.SQI.bai = SQI.df%>% group_by(yr,Regional_board,SQI)%>% count() 

write.csv(region.SQI.bai,'region.sqi.bai.csv')


max(SQImap.df$lat)
max(SQImap.df$lon)
min(SQImap.df$lat)
min(SQImap.df$lon)

region.SQI.bai %>%
  ggplot(aes(x=yr, y = n))+
  geom_col(aes(fill=SQI),position='dodge')+ 
  facet_grid(rows=vars(Regional_board))+
  scale_fill_manual("legend", values = c("Healthy and unstressed" = "green", "Healthy and resilient" = 'blue', "Impacted and stressed" = "red",'Impacted by unknown stress'='orange'))+
  ggtitle('CA stream Conditions by year')+ labs(y = "Count of Stream Conditions", x = "Year")


sqi.mean.df = SQI.df%>% filter(SQI =='Impacted and stressed')%>% group_by(Regional_board)%>% 
  summarise(mean(CRAM),mean(ASCI),mean(CSCI),mean(IPI),mean(pOverall),mean(pHab),mean(pChem))
write.csv(sqi.mean.df,'sqimean.csv')


sqi.bai=SQI.df%>%select(yr,CRAM,IPI,CSCI,ASCI,pHab,pChem,pOverall,Regional_board)%>% group_by(yr)%>%
  mutate(avg.CRAM = mean(CRAM),avg.IPI = mean(IPI),avg.CSCI = mean(CSCI),avg.ASCI = mean(ASCI),avg.phab=mean(pHab),avg.chem=mean(pChem))%>% distinct(yr,.keep_all = TRUE)

write.csv(sqi.bai,'sqi-beautiful-ai-data.csv',row.names = T)
write.csv(sqi.bai,'sqi-beautiful-ai-data2.csv')
SQI.df%>% ggplot(aes(x=H_AqHab),color = H_AqHab)+geom_histogram(bins = 10)






#####  IPI metrics 






SQI.df%>% ggplot(aes(x=Ev_FlowHab))+geom_histogram(bins = 10)

SQI.df%>% ggplot(aes(x=XCMG))+geom_histogram(bins = 10)

SQI.df%>% ggplot(aes(x=PCT_SAFN))+geom_histogram(bins = 10)

SQI.df %>% ggplot(aes(x= H_SubNat))+ geom_histogram(bins=10)

SQI.df%>% ggplot(aes(x=IPI))+geom_histogram()



#### CRAM Attributes 





SQI.df%>% ggplot(aes(y=bs,x=blc))+geom_point()

SQI.df %>% ggplot(aes(x=blc ))+ geom_histogram(bins=10)

SQI.df %>% ggplot(aes(x=hy))+ geom_histogram(bins=10)

SQI.df %>% ggplot(aes(x=ps ))+ geom_histogram(bins=10)

SQI.df%>% ggplot(aes(x=CSCI,y=pChem))+geom_point()

SQI.df=SQI.df%>% select(-Constraint_class)

for (i in 1:length(SQI.df)){
  columns = (colnames(
    SQI.df)[i])
  print(columns)
  column_NA = (sum(is.na(
    SQI.df[,i])))
  print(column_NA)}


cor(SQI.df%>% select(Ev_FlowHab,H_AqHab,H_SubNat,hy,IPI,))

plot(SQI.df$H_AqHab,SQI.df$H_SubNat)

sapply(SQI.df, function(x) sum(is.infinite(x)))

#Impacted and stressed,	
#Healthy and unstressed
#Healthy and resilient
#Impacted by unknown stress










################### LOGISTIC REGRESSION ################# 

SQI.df$SQI =as.factor(SQI.df$SQI)
class(SQI.df$SQI)

SQI.df$SQI <- relevel(SQI.df$SQI, ref = "Healthy and unstressed")

#### IPI MODEL 


IPI.model<-glm(IPI~H_AqHab+H_SubNat+PCT_SAFN+Ev_FlowHab+XCMG,data=SQI.df)
plot(IPI.model)
summary(IPI.model)

scatter.smooth(IPI.model$fit,
               residuals(IPI.model, type = "deviance"),
               lpars = list(col = "red"),
                xlab = "Fitted Probabilities",
               ylab = "Deviance Residual Values",
               main = "Residual Plot for CSCI LOGISTIC REGRESSION")


class(SQI.df$CSCI)

IPI.zscore <- summary(IPI.model)$coefficients/summary(IPI.model)$standard.errors
IPI.zscore
IPI.p <- (1 - pnorm(abs(SQI.zscore), 0, 1))*2
IPI.p
summary(SQI.df)


sd(SQI.df$H_AqHab)
sapply(SQI.df[c(H_AqHab,H_SubNat,PCT_SAFN,Ev_FlowHab,XCMG)],sd)
sapply(SQI.df[,19:21],mean)
exp(coef(IPI.model))


H_aq = seq(0,1,mu = .65,sd = .28,length.out=1000)

PCT = seq(0,1,mu = .65,sd = .28,length.out=1000)

XCG = seq(0,1,mu = .65,sd = .28,length.out=1000)

EV_F = seq(0,1,mu = .65,sd = .28,length.out=1000)

H_sub = seq(0,1,mu = .65,sd = .28,length.out=1000)

samples = sample(XSAMPLE,size = 267) 

predicted=predict(SQI.model,samples,type="probs")
##### CRAM MODEL ##### 

CRAM.model<-glm(CSCI~bs+blc+hy+ps,data=SQI.df)

summary(CRAM.model)








####################### Leaflet map





library(leaflet)
library(riskyr)
library(pals)

# ADDING COLUMN COLOR BASED ON SQI 

SQImap.df=SQI.df %>% 
  mutate(color = case_when(str_detect(SQI.df$SQI, "Healthy and unstressed") ~ "blue",
                           str_detect(SQI.df$SQI, "Healthy and resilient") ~ "green",
                           str_detect(SQI.df$SQI,'Impacted and stressed')~"red",
                           str_detect(SQI.df$SQI,'Impacted by unknown stress')~'pink',
                           TRUE ~ "a default"))



?addPolygons()
?awesomeIcons()
?addAwesomeMarkers

# Creating Leaflet MAP of SQI 
icons <- awesomeIcons(icon = 'glyphicon-tint',markerColor = SQImap.df$SQI,iconColor = 'white')

F.icons <- awesomeIcons(icon = 'warning-sign', markerColor = 'orange',iconColor="white") 

B.icons <- awesomeIcons(icon = 'glyphicon-globe',markerColor= "lightred",iconColor= 'gray')


map = leaflet(data=SQImap.df) %>% 
  setView(lat = 33.811148, lng = -117.521611, zoom = 6.2) %>%
  addTiles() %>%
  addAwesomeMarkers(lng = SQImap.df$lon,lat = SQImap.df$lat,popup=paste("SQI:", SQImap.df$SQI,"<br>", "CSCI:", SQImap.df$CSCI, "<br>" ,"ASCI:", SQImap.df$ASCI,"<br>","IPI:",SQImap.df$IPI,"<br>",'CRAM:',SQImap.df$CRAM),icon=icons ,clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F))






### adding Fecal Contamination points 

map = addAwesomeMarkers(map,lng=f_map$TargetLongitude,lat=f_map$TargetLatitude,popup = paste("Analyte:", f_map$Analyte, "<br>" ,"Result MPN/100 mL:", f_map$Result),clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),icon=F.icons)



### Adding Popular Beaches 


map = addAwesomeMarkers(map,lng=beach_map$LONDD,lat=beach_map$LATDD,popup = paste("Location:", beach_map$LOCATION, "<br>" ,"USE", beach_map$USE_),icon=B.icons,clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F))

map





