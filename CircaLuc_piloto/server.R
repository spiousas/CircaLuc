#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Set workign directory
  setwd("~/Dropbox/Estadistica/CircaLuc")
  
  # Get and reshape the data
  data.df <- read_csv("./data/data.csv") %>%
    gather(key = "Subject", 
           value = "Lumin", 
           -ZTTime , 
           factor_key=TRUE) %>%
  group_by(Subject) %>%
    mutate(Z_Lumin = (Lumin-mean(Lumin, na.rm=TRUE))/sd(Lumin, na.rm=TRUE)) 
  
  output$distPlot <- renderPlot({
    
    data.df %>%
      filter(Subject == input$subject) %>%
      ggplot(aes(x = ZTTime, y = Z_Lumin)) + 
      geom_line()
    
  })
  
})
