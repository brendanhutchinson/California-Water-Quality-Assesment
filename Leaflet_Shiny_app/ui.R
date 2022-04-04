#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(leaflet)
library(riskyr)
library(pals)
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
 

    # App title
    titlePanel("Streams effecting California Economically and Ecologically important sites "),
    # Outputs leaflet map 
    leafletOutput("mymap",height = 1000)
        
      
    )
)
