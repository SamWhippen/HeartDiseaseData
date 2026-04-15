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
                         choices = var_choices)),
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("summary_stats"),
      verbatimTextOutput("chd_compare"),
      verbatimTextOutput("regression")
    )))

server <- function(input, output) {
  #regression model
  model <- reactive({
    
    req(input$variables)
    
    glm(chd ~ age + tobacco + ldl + obesity,
        data = heart,
        family = binomial)
    
  })
  
  output$plot <- renderPlot({
    #Require at least one variable to be selected
    req(input$variables)
    #Outputs summary statistics
  output$summary_stats <- renderPrint({
      
      req(input$variables)
      
      heart %>%
        select(all_of(input$variables)) %>%
        summary()
      
    }) 
  #Outputs CHD/non-CHD comparison  
  output$chd_compare <- renderPrint({
    # requires a selection of at least one input
    req(input$variables)
    
    heart %>%
      group_by(chd) %>%
      summarise(
        Age = mean(age, na.rm = TRUE),
        Tobacco = mean(tobacco, na.rm = TRUE),
        LDL = mean(ldl, na.rm = TRUE),
        Adiposity = mean(adiposity, na.rm = TRUE),
        Alcohol = mean(alcohol, na.rm = TRUE),
        SBP = mean(sbp, na.rm = TRUE)
      )
    
  })
    
  output$regression <- renderPrint({
    req(input$variables)
    summary(model())
    
  })
  
  #Pivot from wide to long format for plotting
    heart_long <- heart %>%
      select(chd, all_of(input$variables)) %>%
      pivot_longer(cols = -chd, 
                   names_to = "variable",
                   values_to = "value") %>%
      mutate(variable = factor(variable,
                               levels = input$variables,
                               labels = names(var_choices)[var_choices %in% input$variables]))
    
    #Creating visualization boxplot
    ggplot(heart_long, aes(x = chd, y = value, fill = chd)) +
      geom_boxplot() +
      facet_wrap(~ variable, scales = "free_y", ncol = 3) +
      scale_fill_manual(values = c("No CHD" = "green", 
                                   "CHD" = "red")) +
      labs(x = NULL,
           y = "Value", 
           fill = "CHD Status"
      ) + 
      theme_minimal(base_size = 14) + 
      theme(strip.text = element_text(face = "bold"),
            legend.position = "bottom",
            panel.grid.minor = element_blank()
      )
  })
}
shinyApp(ui = ui, server = server)