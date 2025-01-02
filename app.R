# Install and load necessary libraries
if (!requireNamespace("shiny")) install.packages("shiny")
if (!requireNamespace("shinydashboard")) install.packages("shinydashboard")
if (!requireNamespace("shinyjs")) install.packages("shinyjs")
if (!requireNamespace("ggplot2")) install.packages("ggplot2")
if (!requireNamespace("dplyr")) install.packages("dplyr")
if (!requireNamespace("DT")) install.packages("DT")
if (!requireNamespace("randomForest")) install.packages("randomForest")
if (!requireNamespace("leaflet")) install.packages("leaflet")
if (!requireNamespace("plotly")) install.packages("plotly")
if (!requireNamespace("shinycssloaders")) install.packages("shinycssloaders")
if (!requireNamespace("shinyBS")) install.packages("shinyBS")

library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(DT)
library(randomForest)
library(leaflet)
library(plotly)
library(shinycssloaders)
library(shinyBS)

# Load LifeCycleSavings dataset
data("LifeCycleSavings")
lifecycle_data <- LifeCycleSavings
lifecycle_data$sr <- as.numeric(lifecycle_data$sr)  # Ensure numeric format for savings rate
lifecycle_data$Index <- seq_len(nrow(lifecycle_data))  # Add index for trends

# Train Random Forest Model
set.seed(123)
train_index <- sample(seq_len(nrow(lifecycle_data)), size = 0.8 * nrow(lifecycle_data))
train_data <- lifecycle_data[train_index, ]
test_data <- lifecycle_data[-train_index, ]

# Random Forest model
rf_model <- randomForest(sr ~ pop15 + pop75 + dpi + ddpi, 
                         data = train_data, na.action = na.omit)

# Custom CSS for styling
custom_css <- "
  body {
    background: linear-gradient(to right, #f8cdd7, #ffffff); /* Light pink to white gradient */
    font-family: 'Roboto', sans-serif;
    color: #333333; /* Dark text for better readability */
  }
  .welcome-page {
    height: 100vh;
    display: flex;
    flex-direction: column;
    justify-content: center;
    align-items: center;
    text-align: center;
    animation: fadeIn 1s;
  }
  .welcome-page h1 {
    font-size: 3.5em;
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5);
    margin-bottom: 20px;
  }
  .welcome-page p {
    font-size: 1.5em;
    text-shadow: 1px 1px 3px rgba(0, 0, 0, 0.5);
    margin-bottom: 40px;
  }
  .input-group {
    margin-top: 20px;
    border: 2px solid #ffffff;
    border-radius: 5px;
    padding: 15px;
    background-color: rgba(255, 255, 255, 0.9); /* Semi-transparent white */
  }
  .form-control {
    width: 280px;
    font-size: 1.2em;
    padding: 10px;
    border: none;
    background-color: transparent;
    color: #333333;
  }
  .form-control:focus {
    outline: none;
    box-shadow: 0 0 5px rgba(0, 114, 255, 0.5);
  }
  .btn-success {
    background-color: #ff6f61; /* Warm coral color */
    border: none;
    transition: background-color 0.3s;
    font-weight: bold;
    padding: 10px 15px; /* Button size */
    font-size: 1em;
    border-radius: 5px;
    margin-top: 10px;
  }
  .btn-success:hover {
    background-color: #e55850; /* Darker shade on hover */
  }
  .dashboard-header {
    background-color: #007bff; /* Blue header */
    color: white;
  }
  .dashboard-sidebar {
    background-color: #0056b3; /* Darker blue sidebar */
  }
  .dashboard-body {
    background-color: #ffffff; /* White body */
  }
  .box {
    border-radius: 15px;
    transition: all 0.3s ease;
    box-shadow: 0 4px 20px rgba(0, 0, 0, 0.2);
  }
  .box:hover {
    box-shadow: 0 6px 30px rgba(0, 0, 0, 0.4);
  }
  .service-icon {
    text-align: center;
    margin-bottom: 15px;
  }
  .service-title {
    font-weight: bold;
    font-size: 1.5em;
  }
  .service-description {
    font-size: 1em;
    color: #555;
  }
  @keyframes fadeIn {
    from { opacity: 0; }
    to { opacity: 1; }
  }
  .tooltip-inner {
    background-color: #1976d2;
    color: #fff;
  }
"

# UI
ui <- tagList(
  useShinyjs(),  # Enable shinyjs for show/hide functionality
  tags$head(tags$style(HTML(custom_css))),
  
  # Welcome page
  div(
    id = "welcome_page",
    class = "welcome-page",
    h1("Welcome to the Life Cycle Savings Dashboard"),
    p("Your gateway to insightful financial analytics."),
    div(class = "input-group",
        textInput("username", "Username", placeholder = "Enter your username"),
        passwordInput("password", "Password", placeholder = "Enter your password")
    ),
    actionButton("login_button", "Login", class = "btn btn-success")
  ),
  
  # Main dashboard (initially hidden)
  hidden(
    div(
      id = "dashboard",
      dashboardPage(
        skin = "purple",
        dashboardHeader(title = "Life Cycle Savings Dashboard"),
        dashboardSidebar(
          sidebarMenu(
            menuItem("Overview", tabName = "overview", icon = icon("home")),
            menuItem("Life Cycle Data", tabName = "lifecycle_data", icon = icon("database")),
            menuItem("Predictions", tabName = "predictions", icon = icon("calculator")),
            menuItem("Interactive Map", tabName = "interactive_map", icon = icon("map")),
            menuItem("About", tabName = "about", icon = icon("info-circle")),
            menuItem("Additional Services", tabName = "additional_services", icon = icon("cogs"))
          )
        ),
        dashboardBody(
          useShinyjs(),
          tags$head(tags$style(HTML(custom_css)), tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Roboto:300,400,500,700&display=swap")),
          tabItems(
            tabItem(tabName = "overview",
                    fluidRow(
                      valueBoxOutput("avg_savings_rate", width = 4),
                      valueBoxOutput("total_population", width = 4),
                      valueBoxOutput("num_countries", width = 4)
                    ),
                    fluidRow(
                      box(
                        title = "Welcome to the Dashboard!", width = 12, solidHeader = TRUE, status = "info",
                        h4("Explore insights into life cycle savings data, predictive models, and visual analytics."),
                        p("Navigate through the tabs for different functionalities like viewing data, making predictions, and exploring trends."),
                        p("This dashboard provides an interactive way to analyze savings rates and demographic insights."),
                        plotlyOutput("overview_plot", height = 250)
                      )
                    )
            ),
            
            tabItem(tabName = "lifecycle_data",
                    fluidRow(
                      box(
                        title = "Life Cycle Data Table", width = 12, solidHeader = TRUE, status = "primary",
                        DTOutput("lifecycle_table") %>% shinycssloaders::withSpinner()
                      )
                    )),
            
            tabItem(tabName = "predictions",
                    fluidRow(
                      box(
                        title = "Make Predictions", width = 6, solidHeader = TRUE, status = "success",
                        numericInput("pred_pop15", "Youth Population (%)", value = 30, min = 0, max = 100),
                        bsTooltip("pred_pop15", "Enter youth population percentage", "right"),
                        numericInput("pred_pop75", "Elderly Population (%)", value = 10, min = 0, max = 100),
                        bsTooltip("pred_pop75", "Enter elderly population percentage", "right"),
                        numericInput("pred_dpi", "Disposable Income", value = 5000, min = 0, max = 1e5),
                        bsTooltip("pred_dpi", "Enter average disposable income", "right"),
                        numericInput("pred_ddpi", "Growth Rate of Income", value = 2, min = 0, max = 10),
                        bsTooltip("pred_ddpi", "Enter growth rate of income", "right"),
                        actionButton("predict_btn", "Predict", class = "btn btn-success")
                      ),
                      box(
                        title = "Prediction Result", width = 6, solidHeader = TRUE, status = "success",
                        textOutput("prediction_output")
                      )
                    )),
            
            tabItem(tabName = "interactive_map",
                    fluidRow(
                      box(
                        title = "Savings Rate Across Countries", width = 12, solidHeader = TRUE, status = "info",
                        leafletOutput("savings_map", height = 500) %>% shinycssloaders::withSpinner()
                      )
                    )),
            tabItem(tabName = "about",
                    fluidRow(
                      box(
                        title = "About This Dashboard", width = 12, solidHeader = TRUE, status = "info",
                        p("This dashboard provides a comprehensive overview of the LifeCycleSavings dataset."),
                        p("Built with R and Shiny, featuring interactive charts and predictive modeling."),
                        p("The goal is to facilitate data-driven decision-making regarding savings in different demographics."),
                        h3("Features:"),
                        tags$ul(
                          tags$li("Interactive Data Visualization"),
                          tags$li("Predictive Modeling using Random Forest"),
                          tags$li("Dynamic Maps for Geographic Insights")
                        ),
                        h3("How to Use:"),
                        p("1. Enter your credentials on the login page."),
                        p("2. Navigate through different tabs to explore data, make predictions, and visualize trends."),
                        p("3. Use the predictions tab to input demographic data and receive a savings rate prediction."),
                        h3("Resources:"),
                        tags$ul(
                          tags$li(a(href = "https://www.r-project.org/", "R Project Homepage")),
                          tags$li(a(href = "https://shiny.rstudio.com/", "Shiny by RStudio")),
                          tags$li(a(href = "https://cran.r-project.org/web/packages/randomForest/index.html", "Random Forest Package"))
                        ),
                        h3("Author:"),
                        p("Developed by Shiksha Bhardwaj and Pragati Naruka"),
                        img(src = "https://www.r-project.org/logo/Rlogo.png", height = "100px", align = "center")
                      )
                    )),
            tabItem(tabName = "additional_services",
                    fluidRow(
                      box(
                        title = "Explore Our Additional Services", width = 12, solidHeader = TRUE, status = "warning",
                        div(class = "additional-services",
                            fluidRow(
                              column(4,
                                     div(class = "service-icon", icon("chart-line"), style = "font-size: 3em; color: #007bff;"),
                                     h4(class = "service-title", "Financial Planning"),
                                     p(class = "service-description", "Get expert advice on managing your finances effectively.")
                              ),
                              column(4,
                                     div(class = "service-icon", icon("hands-helping"), style = "font-size: 3em; color: #007bff;"),
                                     h4(class = "service-title", "Investment Strategies"),
                                     p(class = "service-description", "Receive tailored strategies to maximize your investments.")
                              ),
                              column(4,
                                     div(class = "service-icon", icon("piggy-bank"), style = "font-size: 3em; color: #007bff;"),
                                     h4(class = "service-title", "Savings Optimization"),
                                     p(class = "service-description", "Learn how to save more effectively with proven techniques.")
                              )
                            )
                        )
                      )
                    ))
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Simple authentication
  observeEvent(input$login_button, {
    # Predefined username and password
    valid_username <- "user"
    valid_password <- "password"
    
    if (input$username == valid_username && input$password == valid_password) {
      shinyjs::hide("welcome_page")  # Hide welcome page
      shinyjs::show("dashboard")      # Show dashboard
    } else {
      showModal(modalDialog(
        title = "Login Failed",
        "Invalid username or password. Please try again.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Data Table
  output$lifecycle_table <- renderDT({
    datatable(lifecycle_data, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Overview Value Boxes
  output$avg_savings_rate <- renderValueBox({
    avg_savings <- mean(lifecycle_data$sr, na.rm = TRUE)
    valueBox(round(avg_savings, 2), "Average Savings Rate", icon = icon("piggy-bank"), color = "purple")
  })
  
  output$total_population <- renderValueBox({
    total_population <- sum(lifecycle_data$pop15 + lifecycle_data$pop75, na.rm = TRUE)
    valueBox(total_population, "Total Population", icon = icon("users"), color = "purple")
  })
  
  output$num_countries <- renderValueBox({
    num_countries <- nrow(lifecycle_data)
    valueBox(num_countries, "Number of Countries", icon = icon("flag"), color = "purple")
  })
  
  # Overview Plot
  output$overview_plot <- renderPlotly({
    plot_ly(lifecycle_data, x = ~Index, y = ~sr, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#007bff'), marker = list(size = 5, symbol = 'circle')) %>%
      layout(title = "Savings Rate Trends", xaxis = list(title = "Index"), yaxis = list(title = "Savings Rate"),
             hovermode = "closest") %>%
      config(displayModeBar = FALSE)
  })
  
  # Prediction
  observeEvent(input$predict_btn, {
    new_data <- data.frame(pop15 = input$pred_pop15,
                           pop75 = input$pred_pop75,
                           dpi = input$pred_dpi,
                           ddpi = input$pred_ddpi)
    
    prediction <- predict(rf_model, newdata = new_data)
    output$prediction_output <- renderText({
      paste("Predicted Savings Rate:", round(prediction, 2), "%")
    })
  })
  
  # Interactive Map
  output$savings_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        lng = jitter(runif(nrow(lifecycle_data), -180, 180)),
        lat = jitter(runif(nrow(lifecycle_data), -90, 90)),
        radius = lifecycle_data$sr * 0.5,
        label = paste0("Savings Rate: ", lifecycle_data$sr),
        color = "blue", fillOpacity = 0.5
      ) %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addLegend("bottomright", pal = colorFactor(c("blue", "lightblue"), NULL),
                values = NULL, title = "Savings Rate",
                labFormat = labelFormat(suffix = "%"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

