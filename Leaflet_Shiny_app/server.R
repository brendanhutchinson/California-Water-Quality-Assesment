#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(leaflet)
library(riskyr)
library(pals)
library(shiny)

icons <- awesomeIcons(icon = 'glyphicon-tint',markerColor = SQImap.df$SQI,iconColor = 'white')

F.icons <- awesomeIcons(icon = 'warning-sign', markerColor = 'orange',iconColor="white") 

B.icons <- awesomeIcons(icon = 'glyphicon-globe',markerColor= "lightred",iconColor= 'gray')


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$mymap <- renderLeaflet({
    m <- leaflet(data=SQImap.df) %>% 
      setView(lat = 31.510134,  lng =-117.323194, zoom = 7.4) %>%
      addTiles() %>%
      addAwesomeMarkers(lng = SQImap.df$lon,
                        lat = SQImap.df$lat,
                        popup=paste("CSCI:", SQImap.df$CSCI, 
                                    "<br>" ,"ASCI:", SQImap.df$ASCI,
                                    "<br>","IPI:",SQImap.df$IPI,"<br>",'CRAM:',
                                    SQImap.df$CRAM),icon=icons,
                        clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F))

})})

# Creating Leaflet MAP of SQI 



