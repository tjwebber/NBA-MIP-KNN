#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(formattable)

data <- read.csv("https://www.dropbox.com/s/km8kfq154x39nb6/final_knn_data.csv?dl=1")

data$PER_Percent_Change <- percent(data$PER_Percent_Change)
data$Usage_Percent_Change <- percent(data$Usage_Percent_Change)


final_test_data <- filter(data,
                          subset=="Test")
final_train_data <- filter(data,
                           subset=="Train")
final_nominee_data <- filter(data,
                             subset=="Nominee")


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Percent Change of PER and Usage Rating of NBA Players"),
  sidebarLayout(
    sidebarPanel(selectInput("Name", "Please Select a Player:", choices = as.character(final_test_data$Name))),
    mainPanel(plotOutput("scatterPlot",brush = "plot_brush"),
              verbatimTextOutput("info"))
  )
)

server <- function(input, output, session) {
  
  
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  
  
  Name_List <- final_test_data$Name
  
  Name <- reactive({input$Name_list})
  
  knn_plot <- ggplot(mapping = aes(Usage_Percent_Change,PER_Percent_Change)) +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    geom_point(data=final_nominee_data, color = "red") +
    geom_point(data=final_train_data,color = "blue") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    ylab("Percent Change in Player Efficiency Rating") + xlab("Percent Change in Usage Rating") +
    ggtitle("Most Improved Player Scatterplot")
  
  output$scatterPlot <- renderPlot({
    knn_plot + 
      geom_point(data=final_test_data %>% 
                   filter(Name==req(input$Name)),
                 color = "orange",size=4)
  })
  
  output$info <- renderPrint({
    brushedPoints(data, input$plot_brush, xvar = "Usage_Percent_Change", yvar = "PER_Percent_Change")
  })
}


shinyApp(ui=ui,server=server)

