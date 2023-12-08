# Load necessary libraries
library(caret)
library(readr)
library(dplyr)
library(shiny)
library(caret)
library(FactoMineR)
library(factoextra)
library(readr)
library(ggplot2)
library(plotly)
library(tidyr) 

# Load the dataset
dataset <- read_csv("C:/Users/nzacc/OneDrive/Desktop/Project/Heart_disease_cleveland_new.csv")

# Preprocess the data
# Convert binary categorical variables to factors, ensure numerical variables are numeric
dataset_clean2 <- dataset %>%
  mutate(across(c(sex, fbs, exang), as.factor),  # Convert binary categories to factors
         across(c(age, trestbps, chol, thalach, oldpeak), as.numeric), # Ensure numeric variables are treated as such
         cp = factor(cp), restecg = factor(restecg), 
         slope = factor(slope), ca = factor(ca), 
         thal = factor(thal),
         target = factor(target, levels = c("0", "1"))) %>%
  na.omit()  # Remove rows with missing values

dataset_clean2 <- dataset_clean2 %>% 
  rename(condition = target)

# Split data into training and test sets
set.seed(123)
train_i <- createDataPartition(dataset_clean2$condition, p = 0.6, list = FALSE)
train_data <- dataset_clean2[train_i, ]
test_data <- dataset_clean2[-train_i, ]

# Train a logistic regression model for classification
model <- train(condition ~ ., data = train_data, method = "glm", family = "binomial")

# Evaluate the model
predictions <- predict(model, test_data, type = "raw")
confusionMatrix(predictions, test_data$condition)

# Load the dataset
dat_boxplt <- read_csv("C:/Users/nzacc/OneDrive/Desktop/Project/Heart_disease_cleveland_new.csv")

# Preprocess the data
dat_plt <- dat_boxplt %>%
  mutate(
         across(c(age, trestbps, chol, thalach, oldpeak), as.numeric),
         sex = factor(sex, levels = c(0, 1), labels = c("Female", "Male")),
         fbs = factor(fbs, levels = c(0, 1), labels = c("False", "True")),
         exang = factor(exang, levels = c(0, 1), labels = c("No", "Yes")),
         cp = factor(cp, levels = c(0, 1, 2, 3), labels = c("Typical Angina", "Atypical Angina", "Non-Anginal Pain", "Asymptomatic")),
         restecg = factor(restecg, levels = c(0, 1, 2), labels = c("Normal", "ST-T Wave Abnormality", "Left Ventricular Hypertrophy")),
         slope = factor(slope, levels = c(0, 1, 2), labels = c("Upsloping", "Flat", "Downsloping")),
         thal = factor(thal, levels = c(0, 1, 2, 3), labels = c("NULL", "Normal", "Fixed Defect", "Reversible Defect")),
         target = factor(target, levels = c("0", "1"), labels = c("No Disease", "Disease"))) %>%
  na.omit()







# Load your data
dat <- read_csv("C:/Users/nzacc/OneDrive/Desktop/Project/Heart_disease_cleveland_new.csv", show_col_types = FALSE)

# Calculate the correlation matrix
correlation_matrix <- cor(dat)

# Set row and column names to variable names
colnames(correlation_matrix) <- rownames(correlation_matrix) <- colnames(dat)

# Create a heatmap using plotly
heatmap_plot <- plot_ly(z = correlation_matrix,
                        x = colnames(correlation_matrix),
                        y = colnames(correlation_matrix),
                        type = "heatmap",
                        colorscale = "Viridis") %>%
  layout(title = "Correlation Heatmap",
         xaxis = list(title = "Variables"),
         yaxis = list(title = "Variables"))

# box plots n
boxplot_plots <- lapply(colnames(dat), function(var_name) {
  plot_ly(x = ~dat[[var_name]], type = "box", name = var_name, orientation = "v")  
}) %>%
  subplot(nrows = 3)

# Define UI
ui <- fluidPage(
  titlePanel("Heart Disease Analysis App"),
  tabsetPanel(
    tabPanel("Data Visualization",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variableType", "Select Variable Type", choices = c("Numerical", "Categorical"))
               ),
               mainPanel(
                 plotlyOutput("dataVisPlot")
               )
             )
    ),
    
    tabPanel("PCA & Correlation Heatmap", 
             fluidRow(
               column(6,
                      plotlyOutput("pcaBiplot")
               ),
               column(6,
                      plotlyOutput("heatmap")
               )
             )
    ),
    tabPanel("Prediction",
             sidebarLayout(
               sidebarPanel(
                 numericInput("age", "Age", value = 55),
                 selectInput("sex", "Sex", choices = c("Male" = 1, "Female" = 0)),
                 numericInput("cp", "Chest Pain Type", value = 0),
                 numericInput("trestbps", "Resting Blood Pressure", value = 130),
                 numericInput("chol", "Serum Cholestoral in mg/dl", value = 250),
                 selectInput("fbs", "Fasting Blood Sugar > 120 mg/dl", choices = c("No" = 0, "Yes" = 1)),
                 numericInput("restecg", "Resting Electrocardiographic Results", value = 0),
                 numericInput("thalach", "Maximum Heart Rate Achieved", value = 150),
                 selectInput("exang", "Exercise Induced Angina", choices = c("No" = 0, "Yes" = 1)),
                 numericInput("oldpeak", "ST Depression Induced by Exercise", value = 1.0),
                 numericInput("slope", "Slope of the Peak Exercise ST Segment", value = 0),
                 numericInput("ca", "Number of Major Vessels Colored by Flourosopy", value = 0),
                 selectInput("thal", "Thalassemia", choices = c("1" = "1", "2" = "2", "3" = "3"), selected = "1"),
                 actionButton("predict", "Predict")
               ),
               mainPanel(
                 uiOutput("prediction") # Use uiOutput to render HTML
               )
             )
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  
  # Box plot output
  output$dataVisPlot <- renderPlotly({
    data <- dat_plt
    # Depending on the input, show either numerical or categorical plots
    if (input$variableType == "Numerical") {
      # Convert to long format for numerical variables
      long_data <- pivot_longer(data, cols = where(is.numeric), names_to = "variable", values_to = "value")
      # Create boxplots for all numerical variables
      p <- ggplot(long_data, aes(x = variable, y = value)) +
        geom_boxplot() +
        theme_minimal() +
        ggtitle("Boxplot of Numerical Variables") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else {
      # Convert to long format for categorical variables
      long_data <- pivot_longer(data, cols = where(is.factor), names_to = "variable", values_to = "value")
      
      # Create horizontal bar plots for all categorical variables
      p <- ggplot(long_data, aes(x = value)) +
        geom_bar(stat = "count") +
        coord_flip() +  # Makes the bar plots horizontal
        facet_wrap(~variable, ncol = 2, scales = "free_y") +  # Display 2 subplots per row
        theme_minimal() +
        ggtitle("Bar Plot of Categorical Variables") +
        theme(axis.text.y = element_text(angle = 45, hjust = 1, size = 7),  # Rotate and resize y-axis labels
              strip.text = element_text(size = 8),  # Resize facet labels
              plot.margin = unit(c(1,1,1,1), "lines"))  # Adjust plot margins
      
      # Display or save the plot
      # For example, in a Shiny app, use plotOutput() to set the desired width and height
      
      
    }
    ggplotly(p) 
  })
  
  
  # Prediction output
  output$prediction <- renderUI({
    req(input$predict) 
    
    # Predict using the pre-trained model
    new_data <- data.frame(
      age = as.numeric(input$age),
      sex = as.factor(input$sex),
      cp = as.factor(input$cp),
      trestbps = as.numeric(input$trestbps),
      chol = as.numeric(input$chol),
      fbs = as.factor(input$fbs),
      restecg = as.factor(input$restecg),
      thalach = as.numeric(input$thalach),
      exang = as.factor(input$exang),
      oldpeak = as.numeric(input$oldpeak),
      slope = as.factor(input$slope),
      ca = as.factor(input$ca),
      thal = factor(input$thal, levels = c("1", "2", "3"))
    )
    
    prediction <- predict(model, new_data, type = "raw")
    
    # Create and return formatted prediction statement
    if (prediction == 1) {
      HTML(" <span style='font-size: 24px; font-weight: bold; color: red;'>Presence of heart disease.</span>")
    } else {
      HTML("<span style='font-size: 24px; font-weight: bold; color: green;'>No presence of heart disease.</span>")
    }
  })
  
  # PCA Biplot output
  output$pcaBiplot <- renderPlotly({
    # Read and preprocess the dataset
    data_path <- "C:/Users/nzacc/OneDrive/Desktop/Project/Heart_disease_cleveland_new.csv"
    dat <- read_csv(data_path, show_col_types = FALSE) %>%
      select(-c(sex, cp, fbs, restecg, exang, slope, ca, thal)) %>%
      mutate(condition = as.factor(target))
    
    # Perform PCA
    pca_result <- PCA(select(dat, -condition), scale.unit = TRUE, ncp = 5, graph = FALSE)
    
    # Generate the PCA biplot using fviz_pca_biplot
    pca_biplot <- fviz_pca_biplot(pca_result,
                                  geom.ind = "point",  # Use point for individuals
                                  geom.var = "arrow",  # Use arrow for variables
                                  col.ind = dat$condition,  # Color by condition
                                  palette = c("#00AFBB", "#E7B800"),
                                  addEllipses = FALSE)  # No ellipses around groups
    
    # Convert PCA results to a data frame for geom_text
    var_coords <- as.data.frame(pca_result$var$coord)
    names(var_coords) <- c("Dim.1", "Dim.2")
    
    # Add labels for variables
    pca_biplot <- pca_biplot + 
      geom_text(data = var_coords, 
                aes(label = rownames(var_coords), x = Dim.1, y = Dim.2), 
                vjust = 1.5, hjust = 1.5)  # Adjust text position
    
    # Convert to a plotly object
    ggplotly(pca_biplot)
  })
  
  # Correlation Heatmap output
  output$heatmap <- renderPlotly({
    heatmap_plot
  })
}

# Run the app
shinyApp(ui = ui, server = server)
