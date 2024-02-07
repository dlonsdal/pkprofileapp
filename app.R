library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("PKprofile"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of clearance ----
      sliderInput(inputId = "cl",
                  label = "Clearance (L/hr)",
                  min = 0.1,
                  max = 50,
                  value = 5),
      # Input: Slider for dose; ----
      sliderInput(inputId = "d",
                  label = "Dose (mg)",
                  min = 0.1,
                  max = 5000,
                  value = 1000),
      # Input: Slider for dose interval; ----
      sliderInput(inputId = "tor",
                  label = "Dosing interval (hrs)",
                  min = 0.1,
                  max = 48,
                  value = 6),
      # Input: Slider for absorption coefficient; ----
      sliderInput(inputId = "ka",
                  label = "ka",
                  min = 0.01,
                  max = 5,
                  value = 0.8),
      # Input: Slider for bioavailability; ----
      sliderInput(inputId = "F",
                  label = "Bioavailability",
                  min = 0.01,
                  max = 1,
                  value = 0.8),
      # Input: Slider for volume of distribution; ----
      sliderInput(inputId = "v",
                  label = "Apparent volume of distribution (L)",
                  min = 0.1,
                  max = 1000,
                  value = 50)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "PKplot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
 
  library(tidyverse)
  library(linpk)
  
  output$PKplot <- renderPlot({

    
    time<-seq(0,72,by=0.01)
    cl<-input$cl
    v<-input$v
    d<-input$d
    ke<-cl/v
    ka<-input$ka
    F<-input$F
    tor<-input$tor
    doses<-seq(0,72,by=tor)
    y <- pkprofile(time, cl=cl, vc=v, ka=ka, dose=list(t.dose=doses, amt=d,f=F))
    dat<-data.frame(y)
    th_calc=data.frame(result=round(log(2)*v/cl,2))
    ss_calc=data.frame(result=round( d/(cl*tor),2))
    
    ggplot(data=data.frame(y), aes(x=time, y=conc,))+
      geom_line()+
      geom_point()+
      theme_classic()+
      xlab("time (hours)")+
      ylab("concentration of drug (mg/L)")+
      geom_text(aes(x = 60, y = 5, label = paste("Half life =",round(result, 2),"hours")), data = th_calc)+
      geom_text(aes(x = 30, y = 2, label = paste("Mean steady state concentration =",round(result, 2),"mg")), data = ss_calc)
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)