library(shiny)
library(shinyjs)

shinyUI(fluidPage(
  
  
  # Application title
  headerPanel("Assesment Dataset"),
  shinyjs::useShinyjs(),
  
  sidebarPanel(width=3,
    selectInput("panel_select", label = "Navigation Menu", 
                choices = list("Missing Data" = 1
                               , "Feature Analysis" = 2
                               , "Feature Selection" = 3
                               , "Model Validation" = 4), 
                selected = 1),
    actionButton("submit_btn", "Select"),
    div(id='modelSliders_div', style="display:none;",
        br(),
        br(),
        hr(),
      sliderInput("classWeights_sld", "Class weights:",
                  min=0, max=40, value=12),
      sliderInput("bias_sld", "Bias:",
                  min = 0, max = 0.25, value = 0.025, step= 0.025),
      actionButton("update_btn", "Update"))
  ),
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(width=9,
    div(id = "missing_div",
      tabsetPanel(id="missing_panel", 
                  tabPanel("Details", p(tags$ul(
                    tags$li('32.25% of records are complete (no missing values)'),
                    tags$li('The feature loan_amount has 1953 missing values (45.55% missing)'),
                    tags$li('The Random Forest algorithm was used for data imputation.')
                  ))
                  ),
                  tabPanel("Missing Data", plotOutput("missing_plot")), 
                  tabPanel("Summary", verbatimTextOutput("summary"))
        # tabPanel("Table", tableOutput("table"))
      )
    )
    ,div(id = "correlation_div", style="display:none;",
         tabsetPanel(id="next_panel",
                      tabPanel("Details", p(tags$ul(
                                tags$li('Pearson product-moment correlation coefficient was used to measure the linear correlation between features.'),
                                tags$li('Highly correlated (0.99) features were removed before data imputation.'),
                                tags$li('Features with a correlation coefficient greater than 0.85 or less than -0.85 were removed from the data set. Principal component analysis was performed on the removed correlated features and the first four principal components were added to the data set.')
                              ))),
                     tabPanel("Corrgram"
                              , plotOutput("correlation_plot")
                              , br()
                              , h4('Features')
                              , verbatimTextOutput("correlationFeatures_txt")),
                     tabPanel("Split Corrgram"
                              , plotOutput("h_correlation_plot")
                              , br()
                              , h4('Features')
                              , verbatimTextOutput("h_correlationFeatures_txt")
                              , br()
                              , plotOutput("l_correlation_plot")
                              , br()
                              , h4('Features')
                              , verbatimTextOutput("l_correlationFeatures_txt")),
                     tabPanel("PCA"
                              , plotOutput("scree_plot")
                              , br()
                              , verbatimTextOutput("PCA_txt"))
         )
    )
    ,div(id = "featureSelection_div", style="display:none;",
         tabsetPanel(id="featureSelection_panel",
                     tabPanel("Details", p(tags$ul(
                       tags$li('The wrapper feature selection function rfe from the caret package was used.'),
                       tags$li('10 fold cross validation with 3 repetitions was performed on the training set.'),
                       tags$li('The feature ‘Number.Of.Meeting.Of.Creditors’ was removed because it contained no information.')
                     ))
                     ),
                     tabPanel("Feature Selection"
                              , plotOutput("featureSelection_plot")
                              , br()
                              , h4('Selected Features')
                              , verbatimTextOutput("featureSelection_txt"))
                     
         )
    )
    ,div(id = "modelValidation_div", style="display:none;",
         tabsetPanel(id="modelValidation_panel",
                     tabPanel("Details", p(tags$ul(
                       tags$li('The training set and test set were split using stratified sampling'),
                       tags$li('The training set consists of 75% of the data and the test set the remaining 25%.'),
                       tags$li('The model was validated using 10 fold cross validation.')
                     ))
                              ),
                     tabPanel("Model Validation"
                              , p(verbatimTextOutput("modelValidation_txt"))),
                     tabPanel("Model Performance"
                              , p(verbatimTextOutput("modelPerformance_txt")))
                     
         )
    )
  )
))

