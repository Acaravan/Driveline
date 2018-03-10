install.packages("shiny")
install.packages("plotly")
library(plotly)

library(shiny)

setwd("C:/Users/acara/OneDrive/Documents/SlickBall")

devtools::install_github('rstudio/packrat')
devtools::install_github('rstudio/rsconnect')

#Shiny Reg Season/PS/WS for slider v nonslider
####################
################
###############
###########
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

####
#more nuanced view on each pitcher
shinyApp(
  ui = fluidPage(
    tabsetPanel(
      tabPanel("Game To Game Slider SR", fluid = TRUE,
               mainPanel(
                 plotOutput('plot1')
               )
      ),
      tabPanel("Pitch Usage #1", fluid = TRUE, 
               mainPanel(
                 plotOutput('plot3')
               )
      ),
      tabPanel("Pitch Usage #2", fluid = TRUE,
               mainPanel(
                 plotOutput('plot4')
               )
      ),
      tabPanel("Pitch Usage #3", fluid = TRUE, 
               mainPanel(
                 plotOutput('plot5')
               )
      )
    )
  ), #sl_dt_CK$SL_spin_rate ~ sl_dt_CK$game_date
  server = function(input, output) {
    output$plot1 <- renderPlot({
      plot(sl_dt_CK$SL_spin_rate ~ sl_dt_CK$game_date, lwd = 4, type = "l", ylim = c(1500, 2800),
           main = "CK Slider Spin Rate",
           xlab = "Date", ylab = "Spin Rate")
      points(sl_dt_CK$SL_spin_rate
             ~ jitter(as.numeric(sl_dt_CK$game_date)),
             pch = 16, col = "#99004450")
    })
    output$plot2 <- renderPlot({
      barplot(type_prop$Difference, names.arg = type_prop$Pitch, 
              main = "Pitch Usage in PS vs. Reg Season", 
              ylab = "Percentage Change in PS", 
              ylim = c(-0.3, 0.3))
    })
    output$plot3 <- renderPlot({
      barplot(t_type_late_prop, beside = TRUE, col = c("red", "blue"), 
              main = "Early vs. Late In Game Pitch Selection", 
              ylab = "Pitch Selection Proportion", 
              legend = rownames(t_type_late_prop))
    })
    output$plot4 <- renderPlot({
      barplot(OB_prop, beside = TRUE, col = c("green", "yellow", "orange", "red"),
              main = "Situational Pitch Type by Farthest Runner", 
              ylab = "Pitch Selection Proportion", 
              legend = rownames(OB_prop), args.legend = list(x = "topleft"))
    })
  }
)

