library(shiny)

# Define UI for application that plots random distributions 
shinyUI(fluidPage(
  tabsetPanel(
    tabPanel("Kershaw SR and SD by RS, PS, WS", fluid = TRUE,
             mainPanel(
               plotOutput('plot1')
             )
    ),
    tabPanel("Kershaw SR by SL v Non-SL x RS, PS, WS", fluid = TRUE, 
             mainPanel(
               plotOutput('plot2')
             )
    ),
    tabPanel("Darvish SR and SD by RS, PS, WS", fluid = TRUE,
             mainPanel(
               plotOutput('plot3')
             )
    ),
    tabPanel("Darvish SR by SL v Non-SL x RS, PS, WS", fluid = TRUE, 
             mainPanel(
               plotOutput('plot4')
             )
    ),
    tabPanel("Keuchel SR and SD by RS, PS, WS", fluid = TRUE, 
             mainPanel(
               plotOutput('plot5')
             )
    ),
    tabPanel("Keuchel SR by SL v Non-SL x RS, PS, WS", fluid = TRUE, 
             mainPanel(
               plotOutput('plot6')
             )
    )
  )
))