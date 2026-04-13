library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)

# Load data
heart <- read.csv("HeartDiseaseData.csv")

# Change family history data to numerical data
heart$famhist <- ifelse(heart$famhist == "Present", 1, 0)

# Make CHD labels nicer
heart$chd <- factor(heart$chd,
                    levels = c(0, 1),
                    labels = c("No CHD", "CHD"))

# Named variables to be more appealing and clear
var_choices <- c(
  "Age" = "age",
  "Family History of CHD" = "famhist",
  "Type A Behaviors" = "typea",
  "Tobacco" = "tobacco",
  "LDL Cholesterol" = "ldl",
  "Adiposity" = "adiposity",
  "Obesity" = "obesity",
  "Alcohol" = "alcohol",
  "Systolic Blood Pressure" = "sbp"
)

ui <- fluidPage(
  
  titlePanel("Interactive Analysis of Cardiovascular Risk Factors and Coronary Heart Disease"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      checkboxGroupInput("variables",
                         "Select Risk Factors:",
                         choices = var_choices)
      
    ),
    
    mainPanel(
      
      plotOutput("plot")
      
    )
  )
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)