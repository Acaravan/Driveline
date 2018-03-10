library(shiny)


shinyApp(
  ui = fluidPage(
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
  ), 
  server = function(input, output) {
    output$plot1 <- renderPlot({
      par(mfrow = c(2,3))
      hist(AllFX17_cc$spin_rate[AllFX17_cc$PS_All == "other" & AllFX17_cc$pitcher == 477132], xlab = 'Spin Rate RPM', main = "CK Reg Season SR")
      hist(AllFX17_cc$spin_rate[AllFX17_cc$PS_All == "PS_All" & AllFX17_cc$pitcher == 477132], xlab = 'Spin Rate RPM', main = "CK PostSeason SR")
      hist(AllFX17_cc$spin_rate[AllFX17_cc$WS == "WS" & AllFX17_cc$pitcher == 477132], xlab = 'Spin Rate RPM', main = "CK World Series SR")
      
      hist(AllFX17_cc$spin_dir[AllFX17_cc$PS_All == "other" & AllFX17_cc$pitcher == 477132], xlab = 'Spin Direction', main = "CK Reg Season SD")
      hist(AllFX17_cc$spin_dir[AllFX17_cc$PS_All == "PS_All" & AllFX17_cc$pitcher == 477132], xlab = 'Spin Direction', main = "CK PostSeason SD")
      hist(AllFX17_cc$spin_dir[AllFX17_cc$WS == "WS" & AllFX17_cc$pitcher == 477132], xlab = 'Spin Direction', main = "CK World Series SD")
      
    })
    output$plot2 <- renderPlot({
      par(mfrow = c(2,3))
      
      hist(AllFX17_cc$spin_rate[AllFX17_cc$PS_All == "other" & AllFX17_cc$pitch_type == 'SL' & AllFX17_cc$pitcher == 477132], xlab = 'Spin Rate RPM', main = "CK Reg Season Slider SR")
      hist(AllFX17_cc$spin_rate[AllFX17_cc$PS_All == "PS_All" & AllFX17_cc$pitch_type == 'SL' & AllFX17_cc$pitcher == 477132], xlab = 'Spin Rate RPM', main = "CK PostSeason Slider SR")
      hist(AllFX17_cc$spin_rate[AllFX17_cc$WS == "WS" & AllFX17_cc$pitch_type == 'SL' & AllFX17_cc$pitcher == 477132], xlab = 'Spin Rate RPM', main = "CK WS Slider SR")
      
      hist(AllFX17_cc$spin_rate[AllFX17_cc$PS_All == "other" & AllFX17_cc$pitch_type != 'SL' & AllFX17_cc$pitcher == 477132], xlab = 'Spin Rate RPM', main = "CK Reg Season Non-Slider SR")
      hist(AllFX17_cc$spin_rate[AllFX17_cc$PS_All == "PS_All" & AllFX17_cc$pitch_type != 'SL' & AllFX17_cc$pitcher == 477132], xlab = 'Spin Rate RPM', main = "CK PostSeason Non-Slider SR")
      hist(AllFX17_cc$spin_rate[AllFX17_cc$WS == "WS" & AllFX17_cc$pitch_type != 'SL' & AllFX17_cc$pitcher == 477132], xlab = 'Spin Rate RPM', main = "CK World Series Non-Slider SR")
    })
    output$plot3 <- renderPlot({
      par(mfrow = c(2,3))
      hist(AllFX17_cc$spin_rate[AllFX17_cc$PS_All == "other" & AllFX17_cc$pitcher == 506433], xlab = 'Spin Rate RPM', main = "YD Reg Season SR")
      hist(AllFX17_cc$spin_rate[AllFX17_cc$PS_All == "PS_All" & AllFX17_cc$pitcher == 506433], xlab = 'Spin Rate RPM', main = "YD PostSeason SR")
      hist(AllFX17_cc$spin_rate[AllFX17_cc$WS == "WS" & AllFX17_cc$pitcher == 506433], xlab = 'Spin Rate RPM', main = "YD World Series SR")
      
      hist(AllFX17_cc$spin_dir[AllFX17_cc$PS_All == "other" & AllFX17_cc$pitcher == 506433], xlab = 'Spin Direction', main = "YD Reg Season SD")
      hist(AllFX17_cc$spin_dir[AllFX17_cc$PS_All == "PS_All" & AllFX17_cc$pitcher == 506433], xlab = 'Spin Direction', main = "YD PostSeason SD")
      hist(AllFX17_cc$spin_dir[AllFX17_cc$WS == "WS" & AllFX17_cc$pitcher == 506433], xlab = 'Spin Direction', main = "YD World Series SD")
      
    })
    output$plot4 <- renderPlot({
      par(mfrow = c(2,3))
      
      hist(AllFX17_cc$spin_rate[AllFX17_cc$PS_All == "other" & AllFX17_cc$pitch_type == 'SL' & AllFX17_cc$pitcher == 506433], xlab = 'Spin Rate RPM', main = "YD Reg Season Slider SR")
      hist(AllFX17_cc$spin_rate[AllFX17_cc$PS_All == "PS_All" & AllFX17_cc$pitch_type == 'SL' & AllFX17_cc$pitcher == 506433], xlab = 'Spin Rate RPM', main = "YD PostSeason Slider SR")
      hist(AllFX17_cc$spin_rate[AllFX17_cc$WS == "WS" & AllFX17_cc$pitch_type == 'SL' & AllFX17_cc$pitcher == 506433], xlab = 'Spin Rate RPM', main = "YD WS Slider SR")
      
      hist(AllFX17_cc$spin_rate[AllFX17_cc$PS_All == "other" & AllFX17_cc$pitch_type != 'SL' & AllFX17_cc$pitcher == 506433], xlab = 'Spin Rate RPM', main = "YD Reg Season Non-Slider SR")
      hist(AllFX17_cc$spin_rate[AllFX17_cc$PS_All == "PS_All" & AllFX17_cc$pitch_type != 'SL' & AllFX17_cc$pitcher == 506433], xlab = 'Spin Rate RPM', main = "YD PostSeason Non-Slider SR")
      hist(AllFX17_cc$spin_rate[AllFX17_cc$WS == "WS" & AllFX17_cc$pitch_type != 'SL' & AllFX17_cc$pitcher == 506433], xlab = 'Spin Rate RPM', main = "YD World Series Non-Slider SR")
    })
    
    output$plot5 <- renderPlot({
      par(mfrow = c(2,3))
      hist(AllFX17_cc$spin_rate[AllFX17_cc$PS_All == "other" & AllFX17_cc$pitcher == 572971], xlab = 'Spin Rate RPM', main = "DK Reg Season SR")
      hist(AllFX17_cc$spin_rate[AllFX17_cc$PS_All == "PS_All" & AllFX17_cc$pitcher == 572971], xlab = 'Spin Rate RPM', main = "DK PostSeason SR")
      hist(AllFX17_cc$spin_rate[AllFX17_cc$WS == "WS" & AllFX17_cc$pitcher == 572971], xlab = 'Spin Rate RPM', main = "DK World Series SR")
      
      hist(AllFX17_cc$spin_dir[AllFX17_cc$PS_All == "other" & AllFX17_cc$pitcher == 572971], xlab = 'Spin Direction', main = "DK Reg Season SD")
      hist(AllFX17_cc$spin_dir[AllFX17_cc$PS_All == "PS_All" & AllFX17_cc$pitcher == 572971], xlab = 'Spin Direction', main = "DK PostSeason SD")
      hist(AllFX17_cc$spin_dir[AllFX17_cc$WS == "WS" & AllFX17_cc$pitcher == 572971], xlab = 'Spin Direction', main = "DK World Series SD")
      
    })
    output$plot6 <- renderPlot({
      par(mfrow = c(2,3))
      
      hist(AllFX17_cc$spin_rate[AllFX17_cc$PS_All == "other" & AllFX17_cc$pitch_type == 'SL' & AllFX17_cc$pitcher == 572971], xlab = 'Spin Rate RPM', main = "DK Reg Season Slider SR")
      hist(AllFX17_cc$spin_rate[AllFX17_cc$PS_All == "PS_All" & AllFX17_cc$pitch_type == 'SL' & AllFX17_cc$pitcher == 572971], xlab = 'Spin Rate RPM', main = "DK PostSeason Slider SR")
      hist(AllFX17_cc$spin_rate[AllFX17_cc$WS == "WS" & AllFX17_cc$pitch_type == 'SL' & AllFX17_cc$pitcher == 572971], xlab = 'Spin Rate RPM', main = "DK WS Slider SR")
      
      hist(AllFX17_cc$spin_rate[AllFX17_cc$PS_All == "other" & AllFX17_cc$pitch_type != 'SL' & AllFX17_cc$pitcher == 572971], xlab = 'Spin Rate RPM', main = "YD Reg Season Non-Slider SR")
      hist(AllFX17_cc$spin_rate[AllFX17_cc$PS_All == "PS_All" & AllFX17_cc$pitch_type != 'SL' & AllFX17_cc$pitcher == 572971], xlab = 'Spin Rate RPM', main = "YD PostSeason Non-Slider SR")
      hist(AllFX17_cc$spin_rate[AllFX17_cc$WS == "WS" & AllFX17_cc$pitch_type != 'SL' & AllFX17_cc$pitcher == 572971], xlab = 'Spin Rate RPM', main = "YD World Series Non-Slider SR")
    })
  }
)

library(rsconnect)
deployApp()