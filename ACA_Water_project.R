library(tidyverse)
library(ggplot2)
library(dplyr)
##DATA SETS 
getwd()
#animal.df = reg_meas_export_cafo_2022_02_01
animal.df <- read_csv("reg_meas_export_cafo_2022-02-01.csv")

## Cleaning Animal DF
sum(is.na(animal.df))
animal.df%>% count(is.na(animal.df[]))
length(colnames(animal.df))
drop(animal.df['npdes_number_ca_number'])

sapply(animal.df, function(x) sum(is.na(x)))

## drop columns with too many missing values 

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


#### famous surf break beaches

Econ_sites = read_csv("Economically_Significant_Sites_-_OSPR_[ds356].csv")

beaches = Econ_sites[grep(pattern ='pier|beach|surfing|Coastal Access' ,x=Econ_sites$USE_,ignore.case = TRUE),]

for (i in 1:length(beaches)){
  columns = (colnames(
    beaches)[i])
  print(columns)
  column_NA = (sum(is.na(
    beaches[,i])))
  print(column_NA)}

beaches = beaches%>% subset(data = beaches, select = -c(ALIAS,RESP_CAT,ADDRESS))
   
sum(is.na(beaches)) 
beaches%>% filter(is.na(DESCRIPTIO))%>% summarise(DESCRIPTIO=LOCATION)



beaches = write.csv('CA_beaches.csv')
### fecal contamination data set 
fecal_df<- read_csv("safetoswim_after2010_2022-01-31.csv")
sum(is.na(fecal_df))

#### stream quality data set 
spec(fecal_df)
stream_quality<- read_csv("fig.-20.-stream-quality-index-map.csv")
#stream_quality = stream_quality[grep(pattern ='impacted|stress' ,x=stream_quality$SQI ,ignore.case = TRUE),]  

fecal_m = fecal_df %>% filter(fecal_df$Unit=="MPN/100 mL")
fecal_m
#fecal_df%>%merge(stream_quality)
stream_quality%>% ggplot(aes(x=H_AqHab),color = H_AqHab)+geom_histogram(bins = 10)
sum(is.na(stream_quality))

###################################################################

stream_quality<- read_csv("fig.-20.-stream-quality-index-map.csv")

#####################################################################

stream_qual = stream_quality[,1:31]

stream_qual

stream_qual%>% ggplot(aes(x=H_AqHab),color = H_AqHab)+geom_histogram(bins = 10)

#####  IPI metrics 

stream_qual%>% ggplot(aes(x=Ev_FlowHab))+geom_histogram(bins = 10)

stream_qual%>% ggplot(aes(x=XCMG))+geom_histogram(bins = 10)

stream_qual%>% ggplot(aes(x=PCT_SAFN))+geom_histogram(bins = 10)

stream_qual %>% ggplot(aes(x= H_SubNat))+ geom_histogram(bins=10)

stream_qual%>% ggplot(aes(x=IPI))+geom_histogram()



#### CRAM Attributes 

stream_qual%>% ggplot(aes(x=bs))+geom_histogram()

stream_qual %>% ggplot(aes(x=blc ))+ geom_histogram(bins=10)

stream_qual %>% ggplot(aes(x=hy))+ geom_histogram(bins=10)

stream_qual %>% ggplot(aes(x=ps ))+ geom_histogram(bins=10)

stream_qual=stream_qual%>% select(-Constraint_class)

for (i in 1:length(stream_qual)){
  columns = (colnames(
    stream_qual)[i])
  print(columns)
  column_NA = (sum(is.na(
    stream_qual[,i])))
  print(column_NA)}


length(unique(stream_qual$COMID))



