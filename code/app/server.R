library(shiny)
library(shinyjs)
source("functions.R") 

library(VIM) #missing data

# Define server logic for random distribution application
shinyServer(function(input, output, session) {
  
  
  ####################################
  # Missing Data
  ####################################
  output$missing_plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    plot(propMissing)
  })
  # Generate a summary of the data
  output$summary <- renderPrint({
    out <- propMissing$missings
    out$Variable <- strtrim(out$Variable, 60)
    rownames(out) <- 1:dim(out)[1]
    out
  })
  
  
  ####################################
  # Correlation
  ####################################
  output$correlation_plot <- renderPlot({
    
    out <- ad[,-c(1,2)]
    colnames(out) <- 1:dim(out)[2]
    
    corrgram(out, order=NULL, lower.panel=panel.shade,
             upper.panel=NULL, text.panel=panel.txt,
             main="Assesment Dataset Correlations")
  })
  output$correlationFeatures_txt <- renderPrint({
    data.frame(Names=colnames(ad[,-c(1,2)]))
  })
  
  output$l_correlation_plot <- renderPlot({
    
    out <- reducedImputedData
    colnames(out) <- 1:dim(out)[2]
    
    corrgram(out, order=NULL, lower.panel=panel.shade,
             upper.panel=NULL, text.panel=panel.txt,
             main="Uncorrelated Features")
  })
  output$l_correlationFeatures_txt <- renderPrint({
    data.frame(Names=colnames(reducedImputedData))
  })
  
  
  
  output$h_correlation_plot <- renderPlot({
    
    out <- removedImputedData
    colnames(out) <- 1:dim(out)[2]
    
    corrgram(out, order=NULL, lower.panel=panel.shade,
             upper.panel=NULL, text.panel=panel.txt,
             main="Correlated Features")
  })
  output$h_correlationFeatures_txt <- renderPrint({
    data.frame(Names=colnames(removedImputedData))
  })
  
  output$scree_plot <- renderPlot({
    plot(pcaImputedData
       , main='Variances'
       ,xlab = 'Principal Components')
  })
  # Generate a summary of the data
  output$PCA_txt <- renderPrint({
    summary(pcaImputedData)
  })
  
  ####################################
  # Feature Selection
  ####################################
  
  output$featureSelection_plot <- renderPlot({
    plot(glm_rfe, type = c("g", "o"))
  })
  # Generate a summary of the data
  output$featureSelection_txt <- renderPrint({
    output <- predictors(glm_rfe)
    data.frame(Predictors=output)
  })
  
  ####################################
  # Model Validation
  ####################################
  
  # Generate a summary of the data
  output$modelValidation_txt <- renderPrint({
    input$update_btn
    
    glmWeights <<- isolate(input$classWeights_sld)
    glmBias <<- isolate(input$bias_sld)
    
    trainPredictions()
  })
  # Generate a summary of the data
  output$modelPerformance_txt <- renderPrint({
    input$update_btn
    
    glmWeights <<- isolate(input$classWeights_sld)
    glmBias <<- isolate(input$bias_sld)
   
    
    testPredictions()
    
  })
  
  shiny::observeEvent(input$submit_btn, {
    selId <- as.numeric(input$panel_select)
    
    if(selId==1){
      show("missing_div")
      hide("correlation_div")
      hide("featureSelection_div")
      hide("modelValidation_div")
      
      hide("modelSliders_div")
    }else if(selId==2){
      hide("missing_div")
      show("correlation_div")
      hide("featureSelection_div")
      hide("modelValidation_div")
      
      hide("modelSliders_div")
    }else if(selId==3){
      hide("missing_div")
      hide("correlation_div")
      show("featureSelection_div")
      hide("modelValidation_div")
      
      hide("modelSliders_div")
    }else if(selId==4){
      hide("missing_div")
      hide("correlation_div")
      hide("featureSelection_div")
      show("modelValidation_div")
      
      show("modelSliders_div")
    }else{
      
      
    }
  })
})