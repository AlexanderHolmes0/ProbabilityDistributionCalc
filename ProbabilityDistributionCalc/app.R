library(shiny)
library(shinyWidgets)
library(Ryacas)
library(plotly)
ui <- fluidPage(
  tags$head(HTML(
    '<!-- Quick & Dirty HTML Meta Tags -->
<title>Probability Distribution Calculator</title>
<meta name="description" content="Easily explore and calculate probabilities!">

<!-- Google / Search Engine Tags -->
<meta itemprop="name" content="Probability Distribution Calculator">
<meta itemprop="description" content="Easily explore and calculate probabilities!">
<meta itemprop="image" content="https://media1.giphy.com/media/l378c04F2fjeZ7vH2/giphy.gif?cid=ecf05e47mljfvaz4iiraroizmm0ohbwjkf6ns0qu5649no2d&ep=v1_gifs_search&rid=giphy.gif&ct=g">

<!-- Google / Search Engine Tags -->
<meta name="title" content="Probability Distribution Calculator">
<meta name="description" content="Easily explore and calculate probabilities!">
<meta name="image" content="https://media1.giphy.com/media/l378c04F2fjeZ7vH2/giphy.gif?cid=ecf05e47mljfvaz4iiraroizmm0ohbwjkf6ns0qu5649no2d&ep=v1_gifs_search&rid=giphy.gif&ct=g">

<!-- Facebook Meta Tags -->
<meta property="og:url" content="https://aholmes25.shinyapps.io/ProbabilityDistributionCalc/">
<meta property="og:type" content="website">
<meta property="og:title" content="Probability Distribution Calculator">
<meta property="og:description" content="Easily explore and calculate probabilities!">
<meta property="og:image" content="https://media1.giphy.com/media/l378c04F2fjeZ7vH2/giphy.gif?cid=ecf05e47mljfvaz4iiraroizmm0ohbwjkf6ns0qu5649no2d&ep=v1_gifs_search&rid=giphy.gif&ct=g">

<!-- Twitter Meta Tags -->
<meta name="twitter:card" content="summary_large_image">
<meta name="twitter:url" content="https://aholmes25.shinyapps.io/ProbabilityDistributionCalc/">
<meta name="twitter:title" content="Probability Distribution Calculator">
<meta name="twitter:description" content="Easily explore and calculate probabilities!">
<meta name="twitter:image" content="https://media1.giphy.com/media/l378c04F2fjeZ7vH2/giphy.gif?cid=ecf05e47mljfvaz4iiraroizmm0ohbwjkf6ns0qu5649no2d&ep=v1_gifs_search&rid=giphy.gif&ct=g">'),

tags$link(rel = "shortcut icon", href = "https://raw.githubusercontent.com/Tarikul-Islam-Anik/Animated-Fluent-Emojis/master/Emojis/Symbols/Part%20Alternation%20Mark.png")),
  # Application title
  titlePanel("Probability Distribution Circus"),
  tabsetPanel(
    id = "tabs",
    tabPanel(
      title = "Discrete Calc", icon = icon("chart-column"),
      sidebarLayout(
        sidebarPanel(
          textInput("Dfun",
            label = "Original Function",
            placeholder = "Function | Try x*(12-x)"
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
          verbatimTextOutput("Dans"),
         actionButton(inputId = 'discrete',
                      label = "Calculate!",
                      icon = icon('magnifying-glass-chart'))
        ),
        mainPanel(
          plotOutput("DfunPlot", height = "250"),
          plotOutput("D_PMF_Plot", height = "275"),
          plotOutput("D_CDF_Plot", height = "275")
        )
      )
    ),
    tabPanel(
      title = "Continuous Calc", icon = icon("line-chart"),
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
          verbatimTextOutput("area"),
          verbatimTextOutput("PDFarea"),
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
          verbatimTextOutput("Cans"),
          actionButton(inputId = 'continuous',
                       label = "Calculate!",
                       icon = icon('magnifying-glass-chart'))
        ),
        mainPanel(
          plotOutput("Cfun_Plot", height = "250"),
          plotOutput("C_PDF_Plot", height = "275"),
          plotOutput("C_CDF_Plot", height = "275")
        )
      )
    ),
    tabPanel(
      value = "tab1",
      title = "Discrete Model Zoo",
      icon = icon("chart-column"),
      hr(),
      fluidRow(
        column(
          4,
          sliderInput("unifslider",
            label = "Range for Uniform", min = 0,
            max = 20, value = c(5, 10)
          )
        ),
        # column(8,textOutput("unifPMF"))
        column(8, plotOutput("unifPMF"))
      ),
      hr(),
      fluidRow(
        column(
          4,
          sliderInput("geomp",
            "p for Geometric",
            min = .001,
            max = .999,
            value = .1
          )
        ),
        column(8, plotOutput("geomPMF"))
      ),
      hr(),
      fluidRow(
        column(
          4,
          sliderInput("lambda",
            "Lambda for Poisson",
            min = .1,
            max = 50,
            value = 3
          ),
          radioButtons("pois0", "Have bars start at x=0?",
            choices = c("No", "Yes"),
            selected = "No"
          )
        ),
        column(8, plotOutput("poisPMF"))
      ),
      hr(),
      fluidRow(
        column(
          4,
          sliderInput("n",
            "n for Binomial",
            min = 1,
            max = 100,
            value = 25
          ),
          sliderInput("p",
            "p for Binomial",
            min = .01,
            max = .99,
            value = .5
          ),
          radioButtons("binom0", "Have bars start at x=0?",
            choices = c("No", "Yes"),
            selected = "No"
          ),
          radioButtons("binomn", "Have bars end at x=n?",
            choices = c("No", "Yes"),
            selected = "No"
          )
        ),
        column(8, plotOutput("binomPMF"))
      ),
      hr(),
      fluidRow(
        column(
          4,
          sliderInput("pnbinom",
            "p for Negative Binomial",
            min = .001,
            max = .999,
            value = .1
          ),
          sliderInput("nsuccess",
            "Target number of successes (size parameter)",
            min = 1,
            max = 25,
            value = 5
          ),
          radioButtons("nbinom0", "Have bars start at x=0?",
            choices = c("No", "Yes"),
            selected = "No"
          ),
        ),
        column(8, plotOutput("nbinomPMF"))
      )
    ), tabPanel(
      value = "tab2",
      title = "Continuous Model Zoo",
      icon = icon("line-chart"),
      hr(),
      fluidRow(
        column(
          4,
          sliderInput("unifsliderc",
            label = "Range for Uniform",
            min = -4,
            max = 15,
            value = c(5, 10)
          )
        ),
        
        column(8, plotOutput("unifPDF"))
      ),
      hr(),
      fluidRow(
        column(
          4,
          sliderInput("mu",
            "mu (mean) for Normal",
            min = -2,
            max = 10,
            value = 5
          ),
          sliderInput("sigma",
            "sigma (standard deviation) for Normal",
            min = .2,
            max = 4,
            value = 1
          ),
        ),
        column(8, plotOutput("normalPDF"))
      ),
      hr(),
      fluidRow(
        column(
          4,
          sliderInput("meanlog",
            "meanlog for lognormal",
            min = .9,
            max = 5,
            value = 3.8
          ),
          sliderInput("sdlog",
            "sdlog for lognormal",
            min = .1,
            max = 2,
            value = 0.7
          ),
        ),
        column(8, plotOutput("lnormPDF"))
      ),
      hr(),
      fluidRow(
        column(
          4,
          sliderInput("lambdaexp",
            "Lambda for Exponential",
            min = .05,
            max = 5,
            value = 1
          ),
        ),
        column(8, plotOutput("expPDF"))
      ),
      hr(),
      fluidRow(
        column(
          4,
          sliderInput("shape",
            "shape for Weibull",
            min = .1,
            max = 5,
            value = 1
          ),
          sliderInput("scale",
            "scale for Weibull",
            min = .1,
            max = 5,
            value = 2
          ),
        ),
        column(8, plotOutput("weibullPDF"))
      ),
      hr(),
      fluidRow(
        column(
          4,
          sliderInput("alpha",
            "alpha for Beta",
            min = .1,
            max = 5,
            value = 3
          ),
          sliderInput("beta",
            "beta for Beta",
            min = .1,
            max = 5,
            value = 2
          ),
        ),
        column(8, plotOutput("betaPDF"))
      ),
      hr(),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  DFun <- reactive({
   req(input$discrete)
    
    shape <- isolate(input$Dfun)
    x <- isolate(input$D_Range[1]:input$D_Range[2])

    shape <- parse(text = shape)
    shape <- eval(shape)
    list(shape = shape, x = x)
  })

  D_PMF <- reactive({
    if (is.null(input$D_PMF)) {
      req(DFun())
    }
    shape <- DFun()$shape

    if (round(sum(shape),2) == 1) {
      updateTextInput(inputId = "D_PMF", value = input$Dfun)
    } else if(sum(shape) != 1){
      shape <- shape / sum(shape)
      updateTextInput(inputId = "D_PMF", value = "Shape has been scaled")

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
    req(DFun()$shape)
    barplot(isolate(DFun()$shape), names.arg = DFun()$x, main = "Original Function Plot")
  })

  output$D_PMF_Plot <- renderPlot({
    req(D_PMF())
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

    if (input$Cquestion == "Between") {
      list(
        numericRangeInput("Cquest_range",
          label = "Range", value = c(0, 10), step = 0.01
        )
      )
    } else {
      list(
        numericInput("Cquest_range",
          label = "Value",
          value = c(5), step = 0.01
        )
      )
    }
  })

  output$Cans <- renderPrint({
    req(input$Cquest_range)
    req(input$C_CDF)
    cdf <- input$C_CDF
    C_CDF <- \(x) eval(parse(text = cdf))
    if (input$Cquestion == "Between") {
      C_CDF(input$Cquest_range[2]) - C_CDF(input$Cquest_range[1])
    } else if (input$Cquestion == "Above") {
      1 - C_CDF(input$Cquest_range)
    } else if (input$Cquestion == "Below") {
      C_CDF(input$Cquest_range)
    }
  })

  CFun <- reactive({

    req(input$continuous)
    C_Fun <- isolate(input$C_Fun)
    if(C_Fun != ""){
    \(x) eval(parse(text = C_Fun))
    }else{
      NULL
    }
  })

  C_PDF <- reactive({
    req(input$continuous)

    C_PDF <- isolate(input$C_PDF)
    if(C_PDF!=""){
    \(x) eval(parse(text = C_PDF))
    }else {
      NULL
    }
  })

  output$area <- renderPrint({
    req(input$continuous)
    if(!is.null(CFun())){
    paste("Area Under Original Curve:", integrate(CFun(), input$C_Range[1], input$C_Range[2])$val)
    }
  })

  output$PDFarea <- renderPrint({
    req(input$continuous)
    if(!is.null(C_PDF())){
    paste("Area Under PDF Curve:", integrate(C_PDF(), input$C_Range[1], input$C_Range[2])$val)
    }
  })
  output$Cmean <- renderText({
    req(input$continuous)
    if(!is.null(C_PDF())){
    C_PDF <- isolate(input$C_PDF)
    paste("Mean:", integrate(\(x) x * eval(parse(text = C_PDF)), input$C_Range[1], input$C_Range[2])$val)
    }
  })

  output$Csd <- renderText({
    req(input$continuous)
    if(!is.null(C_PDF())){
    C_PDF <- isolate(input$C_PDF)
    muC <- integrate(\(x) x * eval(parse(text = C_PDF)), input$C_Range[1], input$C_Range[2])$val
    paste("Standard Deviation:", integrate(\(x)  (x - muC)^2 * eval(parse(text = C_PDF)), input$C_Range[1], input$C_Range[2])$val)
    }
  })

  output$Cmode <- renderText({
    if(!is.null(C_PDF())){
    paste("Mode:", optimize(C_PDF(), interval = c(input$C_Range[1], input$C_Range[2]), maximum = TRUE)$maximum)
    }
  })

  C_CDF <- reactive({
    req(input$continuous)
    if (input$C_PDF != "") {
      req(C_PDF())
      cdf <- deparse(yac_expr(paste0("Integrate(x,", input$C_Range[1], ",x) ", noquote(input$C_PDF)))[[1]])
      updateTextInput(inputId = "C_CDF", value = cdf)
      CDF_FUN <- \(x) eval(parse(text = cdf))
      CDF_FUN
    } else if (input$C_PDF == "") {
      req(input$C_CDF)
      pdf <- deparse(yac_expr(paste0("D(x)", noquote(input$C_CDF)))[[1]])
      updateTextInput(inputId = "C_PDF", value = pdf)
      CDF_FUN <- \(x) eval(parse(text = input$C_CDF))
      CDF_FUN
    }
  })

  output$Cfun_Plot <- renderPlot({
    req(input$continuous)
    C1 <- isolate(input$C_Fun)

    C_funs <- \(x) eval(parse(text = C1))
    if(!is.null(CFun())){
    curve(C_funs, from = input$C_Range[1], to = input$C_Range[2], ylab = "Function", main = "Original Function")
    }
  })

  output$C_PDF_Plot <- renderPlot({
    req(input$continuous)
    C2 <- isolate(input$C_PDF)

    C_pdfs <- \(x) eval(parse(text = C2))
    if(!is.null(C_PDF())){
    curve(C_pdfs, from = input$C_Range[1], to = input$C_Range[2], ylab = "PDF", main = "PDF")
    }
  })

  output$C_CDF_Plot <- renderPlot({
req(C_CDF())
    req(input$continuous)
if(input$C_CDF != ""){
    C3 <- input$C_CDF
    C_CDF1 <- \(x) eval(parse(text = C3))

    curve(C_CDF1, from = input$C_Range[1], to = input$C_Range[2], ylab = "CDF", main = "CDF")
}
  })


#---------------------------PETRIE SERVER CODE BELOW----------------------------

  tableData <- reactiveVal(data.frame(x = 1:10, y = LETTERS[1:10]))
  plotData <- reactiveVal()

  observeEvent(input$tabs, {
    if (input$tabs == "tab2") {
      # Code for tab 2
      req(is.null(plotData()))
      print("Tab 2 code is run")
      plotData(runif(100))
    }
  })

  output$unifPMF <- renderPlot({
    lower <- input$unifslider[1]
    upper <- input$unifslider[2]
    x <- lower:upper
    p <- c(rep(0, 5), rep(1 / length(x), length(x)), rep(0, 5))
    x <- c(sort(seq(from = lower - 1, by = -1, length = 5)), x, seq(from = upper + 1, by = 1, length = 5))
    barplot(p,
      names.arg = x, xlab = "x", ylab = "Probability",
      main = paste("Uniform between", lower, "and", upper), ylim = c(0, .5)
    )
  })

  output$geomPMF <- renderPlot({
    lower <- 0
    upper <- 5 + qgeom(.999, input$geomp)
    x <- lower:upper
    p <- dgeom(x, input$geomp)
    barplot(p, names.arg = x, xlab = "x", ylab = "Probability", main = paste("Geometric with p =", input$geomp))
  })

  output$poisPMF <- renderPlot({
    lower <- ifelse(input$pois0 == "Yes", 0, max(0, qpois(.001, input$lambda) - 5))
    upper <- 5 + qpois(.999, input$lambda)
    x <- lower:upper
    p <- dpois(x, input$lambda)
    barplot(p, names.arg = x, xlab = "x", ylab = "Probability", main = paste("Poisson with lambda =", input$lambda))
  })

  output$binomPMF <- renderPlot({
    lower <- ifelse(input$binom0 == "Yes", 0, max(0, qbinom(.001, size = input$n, prob = input$p) - 5))
    upper <- ifelse(input$binomn == "Yes", input$n, min(input$n, (5 + qbinom(.999, size = input$n, prob = input$p))))
    x <- lower:upper
    p <- dbinom(x, size = input$n, prob = input$p)
    barplot(p, names.arg = x, xlab = "x", ylab = "Probability", main = paste("Binomial with n =", input$n, "and p =", input$p))
  })

  output$nbinomPMF <- renderPlot({
    lower <- ifelse(input$nbinom0 == "Yes", 0, max(0, qnbinom(.001, size = input$nsuccess, prob = input$pnbinom) - 5))
    upper <- 5 + qnbinom(.999, size = input$nsuccess, prob = input$pnbinom)
    x <- lower:upper
    p <- dnbinom(x, size = input$nsuccess, prob = input$pnbinom)
    barplot(p,
      names.arg = x, xlab = paste("Number of Failures before", input$nsuccess, "Successes"), ylab = "Probability",
      main = paste("Negative Binomial with p =", input$pnbinom, "and r =", input$nsuccess)
    )
  })

  output$unifPDF <- renderPlot({
    lower <- input$unifsliderc[1]
    upper <- input$unifsliderc[2]
    x <- c(lower, upper)
    p <- c(1 / diff(x), 1 / diff(x))
    makewhite <- c(0, 0)
    plot(p ~ x, xlab = "x", ylab = "f(x)", ylim = c(0, 0.7), xlim = c(-4, 15), type = "l", main = paste("Uniform between", lower, "and", upper))
    abline(h = 0)
    lines(makewhite ~ x, lty = 1, col = "white")
  })
  output$expPDF <- renderPlot({
    curve(dexp(x, input$lambdaexp),
      xlab = "x", ylab = "f(x)", xlim = c(0, qexp(.99, input$lambdaexp)), n = 50001,
      main = paste("Exponential with lambda =", input$lambda)
    )
  })
  output$weibullPDF <- renderPlot({
    curve(dweibull(x, input$shape, input$scale),
      xlab = "x", ylab = "f(x)", xlim = c(qweibull(.001, input$shape, input$scale), qweibull(.99, input$shape, input$scale)), n = 50001,
      main = paste("Weibull with shape =", input$shape, "and scale =", input$scale)
    )
  })
  output$lnormPDF <- renderPlot({
    curve(dlnorm(x, input$meanlog, input$sdlog),
      xlab = "x", ylab = "f(x)", xlim = c(qlnorm(.001, input$meanlog, input$sdlog), qlnorm(.99, input$meanlog, input$sdlog)), n = 50001,
      main = paste("Lognormal with meanlog =", input$meanlog, "and sdlog =", input$sdlog)
    )
  })
  output$normalPDF <- renderPlot({
    curve(dnorm(x, input$mu, input$sigma),
      xlab = "x", ylab = "f(x)", ylim = c(0, 0.75), xlim = c(-10, 18), n = 10001,
      main = paste("Normal with mu =", input$mu, "and sigma =", input$sigma)
    )
  })
  output$betaPDF <- renderPlot({
    curve(dbeta(x, input$alpha, input$beta),
      xlab = "x", ylab = "f(x)", xlim = c(0, 1), n = 50001,
      main = paste("Beta with alpha =", input$alpha, "and beta =", input$beta)
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
