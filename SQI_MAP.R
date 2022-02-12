
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
  addAwesomeMarkers(lng = SQImap.df$lon,lat = SQImap.df$lat,popup=paste( "CSCI:", SQImap.df$CSCI, "<br>" ,"ASCI:", SQImap.df$ASCI,"<br>","IPI:",SQImap.df$IPI,"<br>",'CRAM:',SQImap.df$CRAM),icon=icons ,clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F))
 
#map




### adding Fecal Contamination points 

map = addAwesomeMarkers(map,lng=f_map$TargetLongitude,lat=f_map$TargetLatitude,popup = paste("Analyte:", f_map$Analyte, "<br>" ,"Result MPN/100 mL:", f_map$Result),clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),icon=F.icons)



### Adding Popular Beaches 


map = addAwesomeMarkers(map,lng=beach_map$LONDD,lat=beach_map$LATDD,popup = paste("Location:", beach_map$LOCATION, "<br>" ,"USE", beach_map$USE_),icon=B.icons,clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F))

map



addLayersControl(
  overlayGroups = names(),
  options = layersControlOptions(collapsed = FALSE)

  


