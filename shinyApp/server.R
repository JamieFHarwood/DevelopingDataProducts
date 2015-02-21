## Shiny Server.R file for Coursera Developing Data Products project
## Author : Jamie Harwood
## Date : 21/02/2015



shinyServer(
  function(input, output, session) {
    
    ## observe so we can set input$value to either 
    ## text input or radio button selection
    observe({
      userUrl <- "empty"
      ## isolate reactions to the button push
      input$goButton
      isolate({
        if(input$goButton == 0){
          userUrl <- "empty"
          return()
        }
        if(input$url != "" & !url.exists(input$url)){
            
          ## Invalid url - conditional formatting to warn user
          output$inputValue <- renderText({"<span style='color:red; font-size: 1.5em'>Invalid Url - try again</span>"})
          updateTextInput(session, "url", value = "")
          userUrl <- "empty"
          return()
        }
        if(url.exists(input$url)){
            ## Use user input
            userUrl <- input$url
            
        }
        else {
            ##use user selection
            userUrl <- input$radio
         
        }
         userUrl
      })
      
      ## Reactive function to generate userCorpus
      userCorpus <- reactive({
        if(userUrl != "empty")
          withProgress(message = "Fetching data...",{
          getUserCorpus(userUrl) 
          })
      })
      
      ## Reactive function that uses uses userCorpus to generate prediction
      userObs <- reactive({
        withProgress(message = "Preparing Data..",{
  
                   prepareData(userCorpus())
                })
      })
      
      ## Reactive function to get the probability of the prediction
      predictionProb <- reactive({
        
         withProgress(message = "Predicting..",{
  
                    round(getProbability(userObs()) * 100,2)
                })
      })
      
      ## Reactive function to get the prediction text
      prediction <- reactive({
                
          withProgress(message = "Predicting..",{
  
                    getPrediction(userObs())
                }) 
               
      })
        
      ## Reactive function that uses userCorpus to generate termFreqs
      userTermFreqs <-  reactive({
        
          getUserTermFreqs(userCorpus())
      })
      
      ## Reactive function that uses userCorpus to generate modelFreqs
      userModelFreqs <- reactive({
          getUserModelFreqs(userCorpus())
        
      })
      
      ## Reactive function that uses prediction to generate
      ## the terms we should include in the charts
      generateCourseModelFreqs <- reactive({
          cmf <- as.data.frame(courseModelFreqs[prediction()])
          names(cmf) <- c("terms", "frequency", "source")
          topN <-quantile(cmf$frequency, input$topN/100)
          cmf <- cmf[cmf$frequency >= topN,]
          cmf
        
      })
   
    ## From here we are binding values to ui widgets
    output$inputValue <- renderText({
     
       userUrl
    })
    
    output$outputValue <- renderText({

      if( userUrl != "empty") {
        pred <- prediction()
        prob <- predictionProb()
        if(prob > 0){
          paste0(pred," (with estimated ", prob, "% accuracy)")
        }
        else
        {
          paste0(pred, ", but the overall counts for significant terms was low - are you sure this site is about data science?")
        }
      }
      else{
        ""
      }
      
      
    })
      
    ## Labels for word clouds
    output$userCloud <- renderText({
      userUrl
    })  
    output$courseCloud <- renderText({
      prediction()
    }) 
    
    ## Word cloud plots
    output$plotUser <- renderPlot({
       
      top200 <- head(userTermFreqs(), 200)
      if( userUrl != "empty") {
        wordcloud(names(top200), userTermFreqs(),colors=brewer.pal(6,"Dark2"),random.order=FALSE, max.words = input$maxWords)
      }
      else{
        return()
      }
    })
    
    output$plotCourse <- renderPlot({
       
      withProgress(message = "Plotting...",{
      if( userUrl != "empty") {
        wordcloud(names(courseTermFreqs[prediction()][[1]]), courseTermFreqs[prediction()][[1]],colors=brewer.pal(6,"Dark2"),random.order=FALSE, max.words = input$maxWords)
      }
      else{
        return()
      }
      })
    })
      
    ## highchart plots  
    output$userChart <- renderChart2({
      umf <- as.data.frame(userModelFreqs())
      umf$source <- rep(userUrl, nrow(umf))
      names(umf) <- c("terms", "frequency", "source")
      ## we want the topN terms so we need to get them first
      cmf <- generateCourseModelFreqs()
      umf <- umf[umf$term %in% cmf$terms,]
      h1<- hPlot(
        x = "terms", 
        y = "frequency", 
        data = umf, 
        type = "line",
        title="Raw Term frequencies for the url loaded data")
      h1$colors("teal","orange")
      h1$yAxis(title = list(enabled = TRUE, text = 'Raw Term Frequency'))
      h1$xAxis(title = list(enabled = TRUE, text = 'Term index'))
      h1$params$width <- 1000
      h1
        
    })
      
    output$courseChart <- renderChart2({

      cmf <- generateCourseModelFreqs()
      h1<- hPlot(
        x = "terms", 
        y = "frequency", 
        data = cmf, 
        type = "line",
        title="Average Term frequencies per lecture for the course material",
        color = "green")
      h1$colors("orange", "lightgreen")
      h1$yAxis(title = list(enabled = TRUE, text = 'Average Term Frequency'))
      h1$xAxis(title = list(enabled = TRUE, text = 'Term index'))
      h1$params$width <- 1000
      h1
        
    })
      

   
  })

})

