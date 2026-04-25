library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(bslib)
library(rsconnect)

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
  "Type A Behaviors" = "typea")

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "black"),
  
  # Formatting of title, sidebars, and main panels
  tags$head(
    tags$style(HTML("
      .container-fluid {
        max-width: 1200px;
      }
      .card {
        margin-bottom: 12px;
      }
      .title-center {
        text-align: center;
        width: 100%;
      }
    "))),
  
  titlePanel(
    div(class = "title-center",
        "Interactive Analysis of Cardiovascular Risk Factors and Coronary 
        Heart Disease")),
  
  helpText("This app explores how key cardiovascular risk factors relate to 
           coronary heart disease (CHD). Select variables to compare 
           distributions, summary statistics, and model based effects across 
           CHD status."),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Controls"),
      helpText("Select variables to explore their relationship with CHD."),
      
      checkboxGroupInput("variables",
                         "Select Risk Factors:",
                         choices = var_choices,
                         selected = c("age", "ldl")),
      
      card(
        card_header("Variable Descriptions"),
        HTML("
          <b>Age:</b> Age of the individual in years.
          <b>LDL Cholesterol:</b> Low density lipoprotein cholesterol level, 
          often referred to as 'bad' cholesterol.
          <b>Adiposity:</b> A measure of body fat distribution.
          <b>Obesity:</b> Body mass index.
          <b>Systolic Blood Pressure (SBP):</b> Pressure in arteries during 
          heartbeats.
          <b>Tobacco:</b> Lifetime tobacco usage in kilograms.
          <b>Alcohol:</b> Current alcohol consumption level.
          <b>Type A Behaviors:</b> Measure of stress prone, competitive 
          personality traits.
          <b>Family History of CHD:</b> Whether close relatives have had 
          coronary heart disease.
          <b>CHD:</b> Presence or absence of coronary heart disease in the 
          individual."))
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 uiOutput("plot_description"),
                 card(uiOutput("plot_facet"))),
        tabPanel("Summary Stats",
                 uiOutput("summary_description"),
                 card(tableOutput("summary_stats"))),
        tabPanel("CHD Comparison",
                 uiOutput("chd_description"),
                 card(tableOutput("chd_compare"))),
        tabPanel("Logistic Regression",
                 uiOutput("regression_description"),
                 card(tableOutput("regression")))
      )          
    )        
  )
)

server <- function(input, output) {
  #logistic regression model
  model <- reactive({
    req(input$variables)
    
    glm(chd ~ ., data = heart %>% select(chd, all_of(input$variables)),
        family = binomial)
  })
  
  #plot tab description
  output$plot_description <- renderUI({
    req(input$variables)
    selected_labels <- names(var_choices)[var_choices %in% input$variables]
    var_list <- paste(selected_labels, collapse = ", ")
    is_famhist <- "famhist" %in% input$variables
    chart_types <- if (is_famhist && length(input$variables) > 1) {
      "boxplots for continuous variables and a proportional bar chart for Family 
      History of CHD"
    } else if (is_famhist) {
      "a proportional bar chart"
    } else {
      "boxplots"
    }
    helpText(paste0(
      "The plot(s) below display(s) ", chart_types, " comparing the distribution of ",
      var_list, " between individuals with and without CHD. ",
      "Green represents No CHD and red represents CHD. ",
      "Each panel corresponds to one selected variable, with the y-axis scaled ",
      "independently to best display each variable's distribution."
    ))
  })
  
  #summary stats tab description
  output$summary_description <- renderUI({
    req(input$variables)
    selected_labels <- names(var_choices)[var_choices %in% input$variables]
    var_list <- paste(selected_labels, collapse = ", ")
    helpText(paste0(
      "The table below displays summary statistics for the following selected ",
      "variables: ", var_list, ". ",
      "For each variable, the mean, standard deviation, minimum, and maximum ",
      "are reported across all individuals in the dataset, regardless of CHD status."
    ))
  })
  
  #chd comparison tab description
  output$chd_description <- renderUI({
    req(input$variables)
    selected_labels <- names(var_choices)[var_choices %in% input$variables]
    var_list <- paste(selected_labels, collapse = ", ")
    helpText(paste0(
      "The table below compares the average values of ", var_list,
      " between individuals with and without CHD. ",
      "Higher or lower averages between groups may suggest an association ",
      "between that variable and CHD status."
    ))
  })
  
  #logistic regression tab description
  output$regression_description <- renderUI({
    req(input$variables)
    selected_labels <- names(var_choices)[var_choices %in% input$variables]
    var_list <- paste(selected_labels, collapse = ", ")
    helpText(paste0(
      "The table below displays the results of a logistic regression model ",
      "predicting CHD status using the following variables: ", var_list, ". ",
      "The Estimate column shows the log-odds coefficient for each predictor. ",
      "The Odds Ratio indicates how much the odds of having CHD change for a ",
      "one unit increase in that variable. ",
      "Variables with a p-value below 0.05 are considered statistically ",
      "significant predictors of CHD."
    ))
  })
  
  
  #making the faceted plot window height dynamic according to selected variables
  output$plot_facet <- renderUI({
    #Require at least one variable to be selected
    req(input$variables)
    plot_height <- pmax(400, length(input$variables) * 200)
    plotOutput("plot", height = paste0(plot_height, "px"))
  })
    
  output$plot <- renderPlot({
    req(input$variables)
    #Pivot from wide to long format for plotting
    heart_long <- heart %>%
      select(chd, all_of(input$variables)) %>%
      pivot_longer(cols = -chd, 
                   names_to = "variable",
                   values_to = "value") %>%
      mutate(
        variable = factor(variable,
                          levels = input$variables,
                          labels = names(var_choices)[var_choices %in% 
                                                        input$variables]),
        value_cat = ifelse(variable == "Family History of CHD",
                           ifelse(value == 1, "Present", "Absent"),
                           NA)
      )
    
    #Creating visualization boxplot
    ggplot() +
      
      # Boxplots for continuous variables
      geom_boxplot(
        data = heart_long %>% filter(variable != "Family History of CHD"),
        aes(x = chd, y = value, fill = chd),
        alpha = 0.8,
        outlier.color = "black"
      ) +
      
      # Proportional bar chart for family history 
      geom_bar(
        data = heart_long %>% filter(variable == "Family History of CHD"),
        aes(x = chd, fill = value_cat),
        position = "fill"
      ) +
      
      facet_wrap(~ variable, scales = "free_y", ncol = 3) +
      
      scale_fill_manual(
        values = c(
          "No CHD" = "chartreuse3",
          "CHD" = "tomato3",
          "Absent" = "chartreuse3",
          "Present" = "tomato3"
        ),
        name = "Legend"
      ) +
      
      labs(x = NULL,
           y = "Value") + 
      
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
      Variable = names(var_choices)[match(names(data), var_choices)],
      Mean = sapply(data, mean, na.rm = TRUE),
      SD = sapply(data, sd, na.rm = TRUE),
      Min = sapply(data, min, na.rm = TRUE),
      Max = sapply(data, max, na.rm = TRUE)
    )
  })

  #CHD/no-CHD comparison
  output$chd_compare <- renderTable({
    req(input$variables)
    
    chd_result <- heart %>%
      group_by(chd) %>%
      summarise(across(all_of(input$variables),
                       ~ round(mean(.x, na.rm = TRUE), 2))) %>%
      ungroup()
    
    colnames(chd_result)[-1] <- names(var_choices)[match(colnames(chd_result)[-1], var_choices)]
    colnames(chd_result)[1] <- "CHD Status"
    
    chd_result
  })

  #logistic regression
  output$regression <- renderTable({
    req(input$variables)
    m <- summary(model())
    
    coef_table <- as.data.frame(m$coefficients)
    coef_table$Variable <- rownames(coef_table)
    
    colnames(coef_table) <- c("Estimate", "Std Error", "z value", "p value", 
                              "Variable")
    
    coef_table <- coef_table[, c("Variable", "Estimate", "Std Error", "z value", 
                                 "p value")]
    coef_table$Odds_Ratio <- round(exp(coef_table$Estimate), 3)
    
    name_map <- c(setNames(names(var_choices), var_choices), "(Intercept)" = 
                    "(Intercept)")
    coef_table$Variable <- ifelse(coef_table$Variable %in% names(name_map),
                                  name_map[coef_table$Variable],
                                  coef_table$Variable)
    
    coef_table
    
  })
}

shinyApp(ui = ui, server = server)
