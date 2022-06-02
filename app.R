#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(scales)
library(shiny)
library(tidyverse)

colony <- read_csv("data/colony.csv")
stressor <- read_csv("data/stressor.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bee Colonies and their Stressors Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          h3("Data on Bee Colonies"),
          p("The data displayed here was retrieved from:"),
          HTML('<a href="https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-11/readme.md">Tidy Tuesday</a>'),
          p("Navigate through the different visualizations using the tabs.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Histogram", 
                     sliderInput("bins",
                                 "Number of bins:",
                                 min = 1,
                                 max = 50,
                                 value = 30),
                     plotOutput("distPlot")),
            tabPanel("Time Line",
                     selectInput("state",
                                 "Select state:",
                                 choices = colony %>%  distinct(state) %>% pull(state)
                                ),
                     sliderInput("year",
                                 "Select year:",
                                 min = 2015,
                                 max = 2021,
                                 value = 2015,
                                 sep = ""),
                     plotOutput("timelinePlot")),
            tabPanel("Stressor Graph",
                     sliderInput("years",
                                 "Choose your year:",
                                 min = 2015,
                                 max = 2021,
                                 value = 2016,
                                 sep = ""),
                     selectInput("Stressor",
                                 "Choose your stressor:",
                                 choices = stressor %>% distinct(stressor) %>% pull(stressor)
                                 ),
                     selectInput("states",
                                 "Choose your state:",
                                 choices = stressor %>% distinct(state) %>% pull(state)
                                 ),
                     plotOutput("newplot"))
            
          )
    
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- colony %>% pull(colony_lost_pct)
        bins <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$timelinePlot <- renderPlot({
      colony %>%
        filter(state == input$state & year == input$year) %>%
        mutate(months = factor(months,
                               levels = c("January-March",
                                          "April-June",
                                          "July-September",
                                          "October-December"))) %>%
        ggplot(aes(x = months,
                   y = colony_lost_pct,
                   group = 1,
                   label = percent(colony_lost_pct, scale = 1))) +
        geom_line() +
        geom_label() +
        facet_wrap(~year) +
        theme_linedraw()  + 
        labs(y = "colony lost percentage",
             title = paste("Percentage of Bee Colonies lost in",
                           input$state)) +
        scale_y_continuous(labels = percent_format(scale = 1),
                           limits = c(0, 40))
    })
# I wanted to make something simlar to the line graph that the instructor made but I wanted to add more 
    # dimensions so we could view how all the stress types effect which state colonies in different time periods
    # this is can be placed in the explore category of interactive visualizations
    output$newplot <- renderPlot({
      stressor %>%
        filter(stressor == input$Stressor & year == input$years & state == input$states) %>%
        ggplot(aes(x = months,
                   y = stress_pct,
                   group = 1,
                   label = percent(stress_pct, scale = 1),
                   ))+
        geom_line(aes(x = months,
                  y = stress_pct))+
        geom_point()+
        theme_linedraw()+
        scale_y_continuous(labels = percent_format(scale = 1),
                           limits = c(0,102))+
        labs(y = "Stress Percentage", x = "Quarter of the Year",
             title = "Bees Have Several Stress Types", 
             subtitle = paste("Percentage of",input$states, "Colonies Affected by", input$Stressor, "in", input$years))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
