library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(bslib)

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
  "Adiposity" = "adiposity",
  "Age" = "age",
  "Alcohol" = "alcohol",
  "Family History of CHD" = "famhist",
  "LDL Cholesterol" = "ldl",
  "Obesity" = "obesity",
  "Systolic Blood Pressure" = "sbp",
  "Tobacco" = "tobacco",
  "Type A Behaviors" = "typea"
)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C7BE5"
  ),
  
  titlePanel("Interactive Analysis of Cardiovascular Risk Factors and Coronary Heart Disease"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Controls"),
      helpText("Select variables to explore their relationship with CHD."),
      
      checkboxGroupInput("variables",
                         "Select Risk Factors:",
                         choices = var_choices,
                         selected = c("age", "ldl")
      )
    ),
    
    mainPanel(
      card(card_header("Plot"), plotOutput("plot")),
      card(card_header("Summary Stats"), tableOutput("summary_stats")),
      card(card_header("CHD Comparison"), tableOutput("chd_compare")),
      card(card_header("Regression Model"), tableOutput("regression"))
    )
  ))

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
    
    #Pivot from wide to long format for plotting
    heart_long <- heart %>%
      select(chd, all_of(input$variables)) %>%
      pivot_longer(cols = -chd, 
                   names_to = "variable",
                   values_to = "value") %>%
      mutate(variable = factor(variable,
                               levels = input$variables,
                               labels = names(var_choices)[var_choices %in% 
                                                             input$variables]))
    
    
    #Creating visualization boxplot
    ggplot(heart_long, aes(x = chd, y = value, fill = chd)) +
      geom_boxplot(alpha = 0.8, outlier.color = "black") +
      facet_wrap(~ variable, scales = "free_y", ncol = 3) +
      scale_fill_manual(values = c("No CHD" = "green", 
                                   "CHD" = "red")) +
      labs(x = NULL,
           y = "Value", 
           fill = "CHD Status"
      ) + 
      theme_minimal(base_size = 14) + 
      theme(plot.title = element_text(face = "bold", size = 16),
            strip.text = element_text(face = "bold"),
            legend.position = "bottom"
      )
  })
  
  #summary stats
  output$summary_stats <- renderTable({
    req(input$variables)
    
    data <- heart %>%
      select(all_of(input$variables))
    
    data.frame(
      Variable = names(data),
      Mean = sapply(data, mean, na.rm = TRUE),
      SD = sapply(data, sd, na.rm = TRUE),
      Min = sapply(data, min, na.rm = TRUE),
      Max = sapply(data, max, na.rm = TRUE)
    )
  })
  
  #CHD/no-CHD comparison
  output$chd_compare <- renderTable({
    req(input$variables)
    
    heart %>%
      group_by(chd) %>%
      summarise(
        Age = round(mean(age, na.rm = TRUE), 2),
        Tobacco = round(mean(tobacco, na.rm = TRUE), 2),
        LDL = round(mean(ldl, na.rm = TRUE), 2),
        Adiposity = round(mean(adiposity, na.rm = TRUE), 2),
        Alcohol = round(mean(alcohol, na.rm = TRUE), 2),
        SBP = round(mean(sbp, na.rm = TRUE), 2)
      )
  })

  #regression
  output$regression <- renderTable({
    req(input$variables)
    m <- summary(model())
    
    coef_table <- as.data.frame(m$coefficients)
    coef_table$Variable <- rownames(coef_table)
    
    colnames(coef_table) <- c("Estimate", "Std Error", "z value", "p value", "Variable")
    
    coef_table <- coef_table[, c("Variable", "Estimate", "Std Error", "z value", "p value")]
    coef_table$Odds_Ratio <- round(exp(coef_table$Estimate), 3)
    coef_table
  })
}

shinyApp(ui = ui, server = server)