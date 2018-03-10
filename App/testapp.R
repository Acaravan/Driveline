library(shiny)


shinyApp(
  ui = fluidPage(
    tabsetPanel(
      tabPanel("Kershaw SR and SD by RS, PS, WS", fluid = TRUE,
               mainPanel(
                 plotOutput('plot1')
               )

  )
  )), 
  server = function(input, output) {
    output$plot1 <- renderPlot({
      hist(rnorm(100,0,1))
})}
)

deployApp()