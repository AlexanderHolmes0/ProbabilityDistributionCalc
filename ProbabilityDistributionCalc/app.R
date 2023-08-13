library(shiny)
library(shinyWidgets)
library(Ryacas)
library(plotly)
ui <- fluidPage(

  # Application title
  titlePanel("Probability Distro Magician"),
  tabsetPanel(
    tabPanel(
      title = "Discrete", icon = icon("bar-chart"),
      sidebarLayout(
        sidebarPanel(
          textInput("Dfun",
            label = "Original Function",
            placeholder = "Function"
          ),
          textInput("D_PMF",
            label = "Probability Mass Function (Populates if Valid PMF)",
            placeholder = "PMF"
          ),
          numericRangeInput("D_Range",
            value = c(0, 10),
            label = "Discrete Range",
          ),
          verbatimTextOutput("Dmean"),
          verbatimTextOutput("Dsd"),
          verbatimTextOutput("Dmode"),
          fluidRow(
            column(
              width = 6,
              prettyRadioButtons("question",
                label = "Above | Below | Between",
                choices = c("Above", "Below", "Between"),
                icon = icon("check"),
                selected = "Above",
                animation = "jelly"
              )
            ),
            column(
              width = 6,
              uiOutput("questionNum"),
            )
          ),
          verbatimTextOutput("Dans")
        ),
        mainPanel(
          plotOutput("DfunPlot", height = "250"),
          plotOutput("D_PMF_Plot", height = "275"),
          plotOutput("D_CDF_Plot", height = "275")
        )
      )
    ),
    tabPanel(
      title = "Continuous", icon = icon("line-chart"),
      sidebarLayout(
        sidebarPanel(
          textInput("C_Fun",
            label = "Original Function",
            placeholder = "Function"
          ),
          textInput("C_PDF",
            label = "Probability Density Function",
            placeholder = "PDF"
          ),
          textInput("C_CDF",
            label = "Cumulative Distribution Function",
            placeholder = "CDF"
          ),
          numericRangeInput("C_Range",
            value = c(0, 10),
            label = "Continuum Range",
            step = 0.01
          ),
          verbatimTextOutput('area'),
          verbatimTextOutput('PDFarea'),
          verbatimTextOutput("Cmean"),
          verbatimTextOutput("Csd"),
          verbatimTextOutput("Cmode"),
          fluidRow(
            column(
              width = 6,
              prettyRadioButtons("Cquestion",
                                 label = "Above | Below | Between",
                                 choices = c("Above", "Below", "Between"),
                                 icon = icon("check"),
                                 selected = "Above",
                                 animation = "jelly"
              )
            ),
            column(
              width = 6,
              uiOutput("questionNumC"),
            )
          ),
          verbatimTextOutput("Cans")
        ),
        mainPanel(
          plotOutput("Cfun_Plot",height = "250"),
          plotOutput("C_PDF_Plot",height = "275"),
          plotOutput("C_CDF_Plot",height = "275")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  DFun <- reactive({
    req(input$Dfun)
    req(input$D_Range)

    shape <- input$Dfun
    x <- input$D_Range[1]:input$D_Range[2]

    shape <- parse(text = shape)
    shape <- eval(shape)
    list(shape = shape, x = x)
  })

  D_PMF <- reactive({
    if (is.null(input$D_PMF)) {
      req(DFun())
    }
    shape <- DFun()$shape

    if (sum(shape) != 1) {
      shape <- shape / sum(shape)
      updateTextInput(inputId = "D_PMF", value = "Shape has been scaled")
    } else {
      shape
      updateTextInput(inputId = "D_PMF", value = input$Dfun)
    }
    shape
  })

  D_CDF <- reactive({
    req(D_PMF())
    d_cdf <- cumsum(D_PMF())
    d_cdf
  })



  output$Dmean <- renderText({
    req(DFun()$x)
    paste("Mean:", sum(DFun()$x * D_PMF()))
  })

  output$Dsd <- renderText({
    req(DFun()$x)
    mu <- sum(DFun()$x * D_PMF())

    paste("Standard Deviation:", sqrt(sum((DFun()$x - mu)^2 * D_PMF())))
  })

  output$Dmode <- renderText({
    req(DFun()$x)
    paste("Mode:", DFun()$x[which.max(D_PMF())])
  })


  output$DfunPlot <- renderPlot({
    req(DFun()$x)
    barplot(DFun()$shape, names.arg = DFun()$x, main = "Original Function Plot")
  })

  output$D_PMF_Plot <- renderPlot({
    req(DFun()$x)
    barplot(D_PMF(), names.arg = DFun()$x, main = "Valid PMF Plot")
  })

  output$D_CDF_Plot <- renderPlot({
    req(DFun()$x)
    Fx <- D_CDF()
    n <- length(DFun()$x)
    x <- DFun()$x
    plot(
      x = NA, y = NA, pch = NA,
      xlim = c(0, max(x)),
      ylim = c(0, 1),
      xlab = "x",
      ylab = "Probability",
      main = "Valid CDF Plot"
    )

    points(x = x[-n], y = Fx[-1], pch = 19)
    points(x = x[-1], y = Fx[-1], pch = 1)
    for (i in 1:(n - 1)) points(x = x[i + 0:1], y = Fx[c(i, i) + 1], type = "l")
  })

  output$questionNum <- renderUI({
    req(DFun()$x)
    if (input$question == "Between") {
      list(
        numericRangeInput("Dquest_range",
          label = "Range", value = c(0, 10)
        ),
        prettyRadioButtons("equalTo",
          label = "Inclusive or Exclusive",
          choices = c("Inclusive", "Exclusive"),
          selected = "Inclusive",
          animation = "jelly",
          icon = icon("check")
        )
      )
    } else {
      list(
        numericInput("Dquest_range",
          label = "Value",
          value = c(5)
        ),
        prettyRadioButtons("equalTo",
          label = "Inclusive or Exclusive",
          choices = c("Inclusive", "Exclusive"),
          selected = "Inclusive",
          animation = "jelly",
          icon = icon("check")
        )
      )
    }
  })

  output$Dans <- renderPrint({
    req(D_PMF())
    req(DFun())
    req(input$equalTo)
    if (length(input$Dquest_range) == 2) {
      if (input$equalTo == "Inclusive") {
        sum(D_PMF()[which(DFun()$x >= input$Dquest_range[1] & DFun()$x <= input$Dquest_range[2])])
      } else if (input$equalTo == "Exclusive") {
        sum(D_PMF()[which(DFun()$x > input$Dquest_range[1] & DFun()$x < input$Dquest_range[2])])
      }
    } else if (input$question == "Below") {
      if (input$equalTo == "Inclusive") {
        sum(D_PMF()[which(DFun()$x <= input$Dquest_range)])
      } else if (input$equalTo == "Exclusive") {
        sum(D_PMF()[which(DFun()$x < input$Dquest_range)])
      }
    } else if (input$question == "Above") {
      if (input$equalTo == "Inclusive") {
        sum(D_PMF()[which(DFun()$x >= input$Dquest_range)])
      } else if (input$equalTo == "Exclusive") {
        sum(D_PMF()[which(DFun()$x > input$Dquest_range)])
      }
    }
  })

  
  output$questionNumC <- renderUI({
    req(C_CDF())
    if (input$Cquestion == "Between") {
      list(
        numericRangeInput("Cquest_range",
                          label = "Range", value = c(0, 10),step = 0.01
        )
      )
    } else {
      list(
        numericInput("Cquest_range",
                     label = "Value",
                     value = c(5),step = 0.01
        )
        
      )
    }
  })
  
  output$Cans <- renderPrint({
    req(input$Cquest_range)
    req(input$C_CDF)
    cdf <- input$C_CDF
    C_CDF <- \(x) eval(parse(text = cdf))
    if(input$Cquestion == "Between"){
      C_CDF(input$Cquest_range[2]) - C_CDF(input$Cquest_range[1])
    }else if(input$Cquestion == "Above"){
      1-C_CDF(input$Cquest_range)
    }else if(input$Cquestion == "Below"){
      C_CDF(input$Cquest_range)
    }
  })

  CFun <- reactive({
    req(input$C_Fun)
    C_Fun <- input$C_Fun
    \(x) eval(parse(text = C_Fun))
  })

  C_PDF <- reactive({
    req(input$C_PDF)
    C_PDF <- input$C_PDF
    \(x) eval(parse(text = C_PDF))
  })

  output$area <- renderPrint({ 
    req(CFun())
    paste("Area Under Original Curve:",integrate(CFun(),input$C_Range[1],input$C_Range[2])$val)
  })
  
  output$PDFarea <- renderPrint({ 
    req(input$C_PDF)
    paste("Area Under PDF Curve:",integrate(C_PDF(),input$C_Range[1],input$C_Range[2])$val)
  })
  output$Cmean <- renderText({
    req(input$C_PDF)

    C_PDF <- input$C_PDF
    paste("Mean:", integrate(\(x) x* eval(parse(text = C_PDF)) ,input$C_Range[1],input$C_Range[2] )$val)
  })
  
  output$Csd <- renderText({
    req(input$C_PDF)
    C_PDF <- input$C_PDF
    muC <- integrate(\(x) x* eval(parse(text = C_PDF)) ,input$C_Range[1],input$C_Range[2] )$val
    paste("Standard Deviation:", integrate( \(x)  (x-muC)^2 * eval(parse(text = C_PDF)),input$C_Range[1],input$C_Range[2])$val)
  })
  
  output$Cmode <- renderText({
    req(C_PDF)
    paste("Mode:", optimize( C_PDF(), interval=c(input$C_Range[1],input$C_Range[2]), maximum=TRUE)$maximum)
  })
  
  C_CDF <- reactive({

    if(input$C_PDF != ""){
      req(input$C_PDF)
    cdf <- deparse(yac_expr(paste0("Integrate(x,", input$C_Range[1], ",x) ", noquote(input$C_PDF)))[[1]])
    updateTextInput(inputId = "C_CDF",value = cdf)
     CDF_FUN <- \(x) eval(parse(text = cdf))
     CDF_FUN
    }else if(input$C_PDF == ""){
      req(input$C_CDF)
      
      pdf <- deparse(yac_expr(paste0("D(x)", noquote(input$C_CDF)))[[1]])
      updateTextInput(inputId = "C_PDF",value = pdf)
      CDF_FUN <- \(x) eval(parse(text = input$C_CDF))
      CDF_FUN
    }
    
    })

  output$Cfun_Plot <- renderPlot({
    req(CFun())
    req(input$C_Fun)
    
    C1 <- input$C_Fun
    
    C_funs <- \(x) eval(parse(text=C1))
    
    curve(C_funs,from=input$C_Range[1], to=input$C_Range[2],ylab = "Function",main="Original Function")
  })
  
  output$C_PDF_Plot <- renderPlot({
    
    req(input$C_PDF)
    C2 <- input$C_PDF
    
    C_pdfs <- \(x) eval(parse(text=C2))
    
    curve(C_pdfs,from=input$C_Range[1], to=input$C_Range[2],ylab = "PDF",main="PDF")
  })
  
  output$C_CDF_Plot <- renderPlot({
    req(C_CDF())
    req(input$C_CDF)
    C_CDF1 <- \(x) eval(parse(text=input$C_CDF))

  curve(C_CDF1,from=input$C_Range[1], to=input$C_Range[2],ylab = "CDF",main="CDF")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
