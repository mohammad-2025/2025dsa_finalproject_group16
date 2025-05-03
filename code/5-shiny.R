library(shiny)
library(tidyverse)
library(recipes)
library(tidymodels)
library(ranger)
library(vip)
library(forcats)

# Load and prepare data
weather <- read_csv("../data/merged_f.csv") %>%
  select(-DAP)
weather_train <- read_csv("../data/weather-data-train.csv") %>% drop_na()
weather_test <- read_csv("../data/weather-data-test.csv")
fitted_rf <- read_rds("../data/final_rf_model.rds")

# Preprocess data
weather_recipe <- recipe(yield_mg_ha ~ ., data = weather_train) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%  # Convert categorical variables to dummies
  step_normalize(all_numeric(), -all_outcomes())

weather_prep <- prep(weather_recipe, training = weather_train)

# UI
ui <- fluidPage(
  titlePanel("Corn Yield Prediction with Random Forest Model"),
  
  navbarPage("Menu",
             
             tabPanel("Model-Thanks",
                      h3("Random Forest Model Explanation"),
                      p("The Random Forest model is an ensemble method used to predict corn yield."),
                      p("Special thanks to Dr.Bastos for his valuable guidance and support.")
             ),
             
             tabPanel("Variable Importance",
                      fluidPage(
                        h3("Variable Importance"),
                        plotOutput("importance_plot")
                      )
             ),
             
             tabPanel("Observed vs. Predicted",
                      fluidPage(
                        h3("Observed vs. Predicted (Test Set)"),
                        plotOutput("obsVsPredPlot")
                      )
             ),
             
             tabPanel("Predictions",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("new_data", "Upload New Data", accept = c(".csv")),
                          actionButton("predict_button", "Make Predictions")
                        ),
                        mainPanel(
                          tableOutput("predictions_table"),
                          plotOutput("predictions_plot"),
                          textOutput("model_performance")
                        )
                      )
             )
  )
)

# Server
server <- function(input, output) {
  
  # Variable Importance
  output$importance_plot <- renderPlot({
    vi(fitted_rf) %>%
      mutate(Variable = fct_reorder(Variable, Importance)) %>%
      ggplot(aes(x = Importance, y = Variable)) +
      geom_col(fill = "steelblue") +
      labs(y = NULL, x = "Importance", title = "Variable Importance") +
      theme_minimal()
  })
  
  # Observed vs Predicted
  output$obsVsPredPlot <- renderPlot({
    true_values <- weather_test$yield_mg_ha
    predicted_values <- predict(fitted_rf, weather_test)$.pred
    ggplot(data.frame(true_values, predicted_values), aes(x = true_values, y = predicted_values)) +
      geom_point(color = "darkgreen") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      geom_smooth(method = "lm", color = "blue") +
      labs(
        x = "Observed Yield (mg/ha)",
        y = "Predicted Yield (mg/ha)",
        title = "Observed vs. Predicted (Test Set)"
      ) +
      theme_minimal()
  })
  
  # Prediction with new data
  observeEvent(input$predict_button, {
    req(input$new_data)
    new_data <- read_csv(input$new_data$datapath)
    
    new_data_processed <- bake(weather_prep, new_data)
    
    predictions <- predict(fitted_rf, new_data_processed) %>%
      bind_cols(new_data %>% select(site)) %>%
      rename(predicted_yield = .pred)
    
    output$predictions_table <- renderTable({
      predictions
    })
    
    output$predictions_plot <- renderPlot({
      ggplot(predictions, aes(x = site, y = predicted_yield)) +
        geom_col(fill = "skyblue") +
        theme_minimal() +
        labs(title = "Seed Yield Predictions by Site", x = "Site", y = "Predicted Yield (mg/ha)")
    })
    
    # Model performance (if needed for test set)
    rsq_value <- cor(true_values, predicted_values)^2
    rmse_value <- sqrt(mean((true_values - predicted_values)^2))
    
    output$model_performance <- renderText({
      paste("R²: ", round(rsq_value, 3), "\nRMSE: ", round(rmse_value, 3))
    })
  })
}

# Run app
shinyApp(ui = ui, server = server)
