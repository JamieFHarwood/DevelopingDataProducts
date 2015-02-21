## Shiny Ui.R file for Coursera Developing Data Products project
## Author : Jamie Harwood
## Date : 21/02/2015



shinyUI(
  fluidPage(
    tags$head(
      ## Some custom style for my input box
      ## that makes it look like textInput but can render html syle
      ## this is needed for conditinal formatting
        tags$style(HTML("
  
         #inputValue {
            background-color: #f5f5f5;
            border: 1px solid #ccc;
            border-radius: 4px;
            color: #333;
            display: block;
            font-size: 13px;
            line-height: 1.42857;
            margin: 0 0 10px;
            padding: 9.5px;
            word-break: break-all;
            word-wrap: break-word;
          }
    
        "))
      ),
  
  
  headerPanel("Course Recommender"),
    sidebarPanel(
      textInput('url', 'URL'),
      helpText("Type in the url of an english language based",
        "tutorial or article on Data", 
        "Science you found interesting",
        "and I will predict which course from the",
        "Data Science Specialization will be",
        "of most benefit to you"),
      h6("NOTE: Sites using the https protocol are not supported"),
      hr(),
      helpText("Alternatively, use one of the examples below:"),
      radioButtons("radio", label = "",
          choices = list("Caret Tutorial" = "http://topepo.github.io/caret/training.html", 
            "Wikipedia on Students T-test" = "http://en.wikipedia.org/wiki/Student%27s_t-test", 
            "R programming Turtorial" = "http://www.ddiez.com/teac/r/basics.php",
            "CNN News" = "http://cnn.com/news")),
      actionButton("goButton", "Recommend Course"),
      hr(),
      h4("Word Cloud Control"),
      helpText("Use the slider to control how many terms appear in",
        "the word cloud.",
        "The top 200 terms for both the url content and the predicted courseware are available"),
      sliderInput("maxWords",
         "Maximumn words to include in clouds:",
          min = 1,  max = 200,  value = 100),
      hr(),
      h4("Chart Control"),
      helpText("Use the slider to choose a Quantile value.",
        "The quantile value will be used to determine how many of the terms",
        "used to build the prediction model are displayed in the line chart.",
        "There are two charts, one showing the frequency of terms in the url content",
        "and another showing the mean frequency for the same terms in the courseware"),
      sliderInput("topN",
          "Select the Nth percentile of term frequencies in the model:",
          min = 1, max = 100, value = 50)
    
      ),
    
    mainPanel(
      fluidRow(
        h3('Course Recommendatation'),
        h4('You entered'),
        htmlOutput("inputValue"),
        h4('Based upon similarity with previous course content, we recommend you enrol for the following course: '),
        verbatimTextOutput("outputValue")
      ),
      fluidRow(
        column(textOutput("userCloud"),plotOutput("plotUser"), width = 6),
        column(textOutput("courseCloud"),plotOutput("plotCourse"), width = 6)
      ),
      showOutput("userChart", "highcharts"),
      showOutput("courseChart", "highcharts")
    )
  )
)