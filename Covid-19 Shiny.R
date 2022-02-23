library(tidyverse)
library(dplyr)
library(ggplot2)
library(shiny)
library(here)
here("covid-variants.csv")

Data <- read_csv("covid-variants.csv")

Data$Date <- as.Date(Data$date)


ui <- fluidPage(
  titlePanel("Covid-19 cases"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "Country",label = h3("Select Countries to Compare"),
              choices = c(unique(Data$location)),
              multiple = TRUE),
  
  selectInput(inputId = "Variant", label = h3("Select a Variant"),
              choices = c(unique(Data$variant)),
              multiple = FALSE)),
  
  mainPanel(
    plotOutput("line_graph")
  )
  )
)
  



Server <- function(input, output){
  
  Country <- c(Data$location) 
  Date <- c(Data$Date)
  Cases <- c(Data$num_sequences)
  Variant <- c(Data$variant)
  df <- data.frame(Country, Date, Cases, Variant)
  
 
  ## This section allows for reactive subsetting 
  df_subset <- reactive({
    a <- subset(df, Country %in% input$Country
                & Variant %in% input$Variant, 
                )
    return(a)
  })
  
  output$line_graph <- renderPlot({
    ggplot(df_subset(), aes(x = Date, y = Cases,
                         color = Country)) +
      geom_line(size = 1)
      
})
}

shinyApp(ui = ui, server = Server)

