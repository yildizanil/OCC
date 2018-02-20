library(shiny)
library(plotly)
library(rootSolve)
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


shinyUI(fluidPage(
  titlePanel("Original Cam Clay"),
  sidebarLayout(
    sidebarPanel(selectInput(inputId="testtype",label="Choose the test type",choices=c("Drained","Undrained"),selected="Drained"),
                 hr(),
                 sliderInput(inputId = "cp",label = "Initial consolidation pressure [kPa]",min = 1,max = 1000,value = 200),
                 uiOutput("p0"),
                 hr(),
                 sliderInput(inputId = "M",label = "M",min=0.1,max=2,value = 0.95),
                 sliderInput(inputId = "l",label =intToUtf8(0x03BB),min=0.1,max=1,value = 0.2),
                 sliderInput(inputId = "k",label = intToUtf8(0x03BA),min=0.01,max=0.1,value = 0.04,step=0.001),
                 sliderInput(inputId = "N",label = intToUtf8(0x0393),min=1.9,max=3,value = 2.5),
                 hr(),
                 helpText("For questions and comments please contact anil.yildiz@wsl.ch")
    ),
    mainPanel("Isotropically Consolidated Drained/Undrained Triaxial Test",
              tabsetPanel(type = "tabs",
                          tabPanel("Input",plotlyOutput("inputgraph")),
                          tabPanel("q - p Plot", plotlyOutput("qpPlot")),
                          tabPanel("v - ln p Plot",plotlyOutput("vlnpPlot")),
                          tabPanel("3D Plot",plotlyOutput("ThreeDPlot"))
              )
    )
  )
))

