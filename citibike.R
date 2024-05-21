#--------------DANE-----------------------------------------
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(leaflet)
library(stringr)
options(stringsAsFactors=FALSE)
styczen <- read.csv("201501-citibike-tripdata.csv")
luty <- read.csv("201502-citibike-tripdata.csv")
marzec <- read.csv("201503-citibike-tripdata.csv")
kwiecien <- read.csv("201504-citibike-tripdata.csv")
maj <- read.csv("201505-citibike-tripdata.csv")
czerwiec <- read.csv("201506-citibike-tripdata.csv")
lipiec <- read.csv("201507-citibike-tripdata.csv")
sierpien <- read.csv("201508-citibike-tripdata.csv")
wrzesien <- read.csv("201509-citibike-tripdata.csv")
pazdziernik <- read.csv("201510-citibike-tripdata.csv")
listopad <- read.csv("201511-citibike-tripdata.csv")
grudzien <- read.csv("201512-citibike-tripdata.csv")
calosc <- rbind(styczen, luty, marzec, kwiecien, maj, czerwiec, lipiec, sierpien, wrzesien, pazdziernik, listopad, grudzien)

#------------FUNKCJA----------------------------------------
funkcja <- function(dane){
  zmienna <- dane
  zmienna$day <- weekdays(as.Date(zmienna$stoptime, format="%m/%d/%Y %H:%M"))
  zmienna$hour <- format(as.POSIXct(zmienna$stoptime, format="%m/%d/%Y %H:%M"), "%H")
  zmienna$hour <- str_remove(zmienna$hour, "^0")
  zmienna$dzien <- as.Date(zmienna$stoptime, format="%m/%d/%Y %H:%M")
  zmienna <- dplyr::count(zmienna, hour, end.station.name, dzien, end.station.latitude, end.station.longitude, name = "Freq")
  return(zmienna)
}
doshiny <- funkcja(calosc)

#--------------SHINY-------------------------------------------
ui1 <- fluidPage(titlePanel("5 NAJPOPULARNIJESZYCH CELÓW PODRÓŻY W NOWYM YORKU W 2015"),
                 sidebarLayout(
                   sidebarPanel(
                     dateInput(
                       inputId = "days",
                       label = "Wybierz datę: ",
                       value = "2015-01-01",
                       min = "2015-01-01",
                       max = "2015-12-31",
                       format = "yyyy-mm-dd",
                       startview = "month",
                       weekstart = 0,
                       language = "pl"
                     ),
                     
                     sliderInput(
                       inputId = "godzina",
                       label = "Wybierz godzinę: ",
                       min = 00,
                       max = 23,
                       value = 12
                     )
                     
                   ),
                   mainPanel(
                     plotOutput(outputId = "wyjscieHistogram"),
                     leafletOutput("mymap")
                   )
                 ))
             

server1 <- function(input, output){
  output$wyjscieHistogram <- renderPlot({
    date <- as.character(input$days)
    dane <- filter(doshiny, dzien == date)
    dane <- filter(dane, hour == input$godzina)
    
    dane <- arrange(dane, desc(Freq))
    dane <- head(dane, n=5)
    
    print("render")
  
    output$mymap <- renderLeaflet({
      dane <- dane %>% mutate(popup_info = paste(end.station.name, "<br/>", Freq, "<br/>"))
      leaflet() %>% addTiles() %>% addCircleMarkers(data = dane , lat = ~end.station.latitude, lng = ~end.station.longitude, radius = ~3, popup = ~popup_info)
    })
    
    ggplot(dane, aes(Freq, end.station.name))+               
      geom_bar(stat = "identity", fill = "gray", 
               color = "black", width = 0.7)
      #geom_text(show.legend = FALSE)
  })
  
}

shinyApp(ui1, server1)

