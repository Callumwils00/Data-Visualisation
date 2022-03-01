library(tidyverse)
library(ggplot2)
library(shiny)


Data <- read_csv('#paste data path')
head(Data)
Data$male <- Data$`Male Height in Cm`
Data$female <- Data$`Female Height in Cm`
Data$country <- Data$`Country Name`
Data_long <- gather(Data, Gender, Height, male, female, factor_key = TRUE) 


ui <- fluidPage(
  titlePanel("Height difference between males and females by country"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "Country",label = h3("Select a Country"),
                  choices = c(unique(Data_long$country)),
                  multiple = TRUE)),
      mainPanel(
        plotOutput("violinpl")
      )
    ))


server <- function(input, output){
  
  Country <- c(Data_long$country) 
  Height <- c(Data_long$Height)
  Gender <- c(Data_long$Gender)
  
  df <- data.frame(Country, Height, Gender)
  
  df_subset <- reactive({
    a <- subset(df, Country %in% input$Country
    )
    return(a)})
  
    output$violinpl <- renderPlot({
      ggplot(df_subset(), aes(x = Gender, y = Height, color = Country)) + geom_point(size = 5)})
  }

shinyApp(ui = ui, server = server)
