#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(fec16)
library(tidyverse)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            selectInput(inputId = "state_tool", 
                        "Please choose a state.",
                        choices = c("CT", "MA", "VT", "NC")),
            radioButtons(inputId = "incumbent", 
                         label = "Incumbents?",
                         choices = c("TRUE", "FALSE"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(type = "tabs", 
                       tabPanel("histogram", plotOutput("distPlot")),
                       tabPanel("scatterplot", plotOutput("primaryvgen")))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$primaryvgen <- renderPlot({
        results_house %>%
            filter(state == input$state_tool) %>%
            filter(incumbent == input$incumbent) %>%
            ggplot(aes(x = primary_percent, y = general_percent, color = won)) +
            geom_point() +
            theme_classic() +
            labs(title = paste("Relationship between Primary and General results in", input$state_tool), 
                 x = "Primary Percentage", 
                 y = "General Election Percentage")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
