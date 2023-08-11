library(shiny)

ui <- fluidPage(
    titlePanel(
        "Notorious Probability Models"
    ),
    tabsetPanel(id = "tabs",
                tabPanel(
                    value = "tab1",
                    title = "Discrete Distributions",
                    hr(),
                    fluidRow(
                        column(4, 
                               sliderInput("unifslider", label = "Range for Uniform", min = 0, 
                                           max = 20, value = c(5, 10))
                        ),
                        #column(8,textOutput("unifPMF"))
                        column(8, plotOutput("unifPMF") )  
                    ),
                    hr(),
                    fluidRow(
                        column(4, 
                               sliderInput("geomp",
                                           "p for Geometric",
                                           min = .001,
                                           max = .999,
                                           value = .1)
                        ),
                        column(8, plotOutput("geomPMF") )  
                    ),
                    hr(),
                    fluidRow(
                        column(4, 
                               sliderInput("lambda",
                                           "Lambda for Poisson",
                                           min = .1,
                                           max = 50,
                                           value = 3),
                               radioButtons("pois0","Have bars start at x=0?",choices=c("No","Yes"),selected="No")
                        ),
                        column(8, plotOutput("poisPMF") )
                    ),
                    hr(),
                    fluidRow(
                        column(4, 
                               sliderInput("n",
                                           "n for Binomial",
                                           min = 1,
                                           max = 100,
                                           value = 25),
                               sliderInput("p",
                                           "p for Binomial",
                                           min = .01,
                                           max = .99,
                                           value = .5),
                               radioButtons("binom0","Have bars start at x=0?",choices=c("No","Yes"),selected="No"),
                               radioButtons("binomn","Have bars end at x=n?",choices=c("No","Yes"),selected="No")
                               
                        ),
                        column(8, plotOutput("binomPMF") )  
                    ),
                    hr(),
                    fluidRow(
                        column(4, 
                               sliderInput("pnbinom",
                                           "p for Negative Binomial",
                                           min = .001,
                                           max = .999,
                                           value = .1),
                               sliderInput("nsuccess",
                                           "Target number of successes (size parameter)",
                                           min = 1,
                                           max = 25,
                                           value = 5),
                               radioButtons("nbinom0","Have bars start at x=0?",choices=c("No","Yes"),selected="No"),
                        ),
                        column(8, plotOutput("nbinomPMF") )  
                    ),
                    
                ),
                tabPanel(value = "tab2", 
                         title = "Continuous Distributions",
                         hr(),
                         fluidRow(
                             column(4, 
                                    sliderInput("unifsliderc", label = "Range for Uniform", min = -4, 
                                                max = 15, value = c(5, 10))
                             ),
                             #column(8,textOutput("unifPMF"))
                             column(8, plotOutput("unifPDF") )  
                         ),
                         hr(),
                         fluidRow(
                             column(4, 
                                    sliderInput("mu",
                                                "mu (mean) for Normal",
                                                min = -2,
                                                max = 10,
                                                value = 5),
                                    sliderInput("sigma",
                                                "sigma (standard deviation) for Normal",
                                                min = .2,
                                                max = 4,
                                                value = 1),
                             ),
                             column(8, plotOutput("normalPDF") )  
                         ),
                         hr(),
                         fluidRow(
                             column(4, 
                                    sliderInput("meanlog",
                                                "meanlog for lognormal",
                                                min = .9,
                                                max = 5,
                                                value = 3.8),
                                    sliderInput("sdlog",
                                                "sdlog for lognormal",
                                                min = .1,
                                                max = 2,
                                                value = 0.7),
                             ),
                             column(8, plotOutput("lnormPDF") )  
                         ),
                         hr(),
                         
                         fluidRow(
                             column(4, 
                                    sliderInput("lambdaexp",
                                                "Lambda for Exponential",
                                                min = .05,
                                                max = 5,
                                                value = 1),
                             ),
                             column(8, plotOutput("expPDF") )
                         ),
                         hr(),
                         fluidRow(
                             column(4, 
                                    sliderInput("shape",
                                                "shape for Weibull",
                                                min = .1,
                                                max = 5,
                                                value = 1),
                                    sliderInput("scale",
                                                "scale for Weibull",
                                                min = .1,
                                                max = 5,
                                                value = 2),
                             ),
                             column(8, plotOutput("weibullPDF") )  
                         ),
                         hr(),
                         fluidRow(
                             column(4, 
                                    sliderInput("alpha",
                                                "alpha for Beta",
                                                min = .1,
                                                max = 5,
                                                value = 3),
                                    sliderInput("beta",
                                                "beta for Beta",
                                                min = .1,
                                                max = 5,
                                                value = 2),
                             ),
                             column(8, plotOutput("betaPDF") )  
                         ),
                         hr(),
                         
                         
                )
    )
)

server <- function(input, output, session) {
    
    tableData = reactiveVal(data.frame(x = 1:10, y = LETTERS[1:10]))
    plotData = reactiveVal()
    
    observeEvent(input$tabs, {
        
        if(input$tabs == "tab2"){
            #Code for tab 2
            req(is.null(plotData()))
            print("Tab 2 code is run")
            plotData(runif(100))
            
        }
        
    })
    
    output$unifPMF <- renderPlot({
        lower <- input$unifslider[1]
        upper <- input$unifslider[2]
        x    <- lower:upper
        p    <- c(rep(0,5),rep(1/length(x),length(x)),rep(0,5))
        x <- c( sort(seq(from=lower-1,by=-1,length=5) ),x,seq(from=upper+1,by=1,length=5))
        barplot(p,names.arg=x,xlab="x",ylab="Probability",
                main=paste("Uniform between",lower,"and",upper) ,ylim=c(0,.5))
    })
    
    output$geomPMF <- renderPlot({
        lower <- 0
        upper <- 5+qgeom(.999,input$geomp)
        x    <- lower:upper
        p    <- dgeom(x,input$geomp)
        barplot(p,names.arg=x,xlab="x",ylab="Probability",main=paste("Geometric with p =",input$geomp))
    })
    
    output$poisPMF <- renderPlot({
        lower <- ifelse(input$pois0=="Yes",0,max(0,qpois(.001,input$lambda)-5))
        upper <- 5+qpois(.999,input$lambda)
        x    <- lower:upper
        p    <- dpois(x,input$lambda)
        barplot(p,names.arg=x,xlab="x",ylab="Probability",main=paste("Poisson with lambda =",input$lambda))
    })
    
    output$binomPMF <- renderPlot({
        lower <- ifelse(input$binom0=="Yes",0,max(0,qbinom(.001,size=input$n,prob=input$p)-5))
        upper <- ifelse(input$binomn=="Yes",input$n,min(input$n,(5+qbinom(.999,size=input$n,prob=input$p))))
        x    <- lower:upper
        p    <- dbinom(x,size=input$n,prob=input$p)
        barplot(p,names.arg=x,xlab="x",ylab="Probability",main=paste("Binomial with n =",input$n,"and p =",input$p))
    })
    
    output$nbinomPMF <- renderPlot({
        lower <- ifelse(input$nbinom0=="Yes",0,max(0,qnbinom(.001,size=input$nsuccess,prob=input$pnbinom)-5))
        upper <- 5+qnbinom(.999,size=input$nsuccess,prob=input$pnbinom)
        x    <- lower:upper
        p    <- dnbinom(x,size=input$nsuccess,prob=input$pnbinom)
        barplot(p,names.arg=x,xlab=paste("Number of Failures before",input$nsuccess,"Successes"),ylab="Probability",
                main=paste("Negative Binomial with p =",input$pnbinom,"and r =",input$nsuccess))
    })
    
    output$unifPDF <- renderPlot({
        lower <- input$unifsliderc[1]
        upper <- input$unifsliderc[2]
        x    <- c(lower,upper)
        p    <- c(1/diff(x),1/diff(x))
        makewhite <- c(0,0)
        plot(p~x,xlab="x",ylab="f(x)",ylim=c(0,0.7),xlim=c(-4,15),type="l",main=paste("Uniform between",lower,"and",upper))
        abline(h=0)
        lines(makewhite~x,lty=1,col="white")
    })
    output$expPDF <- renderPlot({
        curve( dexp(x,input$lambdaexp),xlab="x",ylab="f(x)",xlim=c(0,qexp(.99,input$lambdaexp) ),n=50001,
               main=paste("Exponential with lambda =",input$lambda ) )
        
    })
    output$weibullPDF <- renderPlot({
        curve( dweibull(x,input$shape,input$scale),xlab="x",ylab="f(x)",xlim=c(qweibull(.001,input$shape,input$scale),qweibull(.99,input$shape,input$scale)),n=50001,
               main=paste("Weibull with shape =",input$shape,"and scale =",input$scale) )
        
    })
    output$lnormPDF <- renderPlot({
        curve( dlnorm(x,input$meanlog,input$sdlog),xlab="x",ylab="f(x)",xlim=c(qlnorm(.001,input$meanlog,input$sdlog),qlnorm(.99,input$meanlog,input$sdlog)),n=50001,
               main=paste("Lognormal with meanlog =",input$meanlog,"and sdlog =",input$sdlog) )
    })
    output$normalPDF <- renderPlot({
        curve( dnorm(x,input$mu,input$sigma),xlab="x",ylab="f(x)",ylim=c(0,0.75),xlim=c(-10,18),n=10001,
               main=paste("Normal with mu =",input$mu,"and sigma =",input$sigma) )
    })
    output$betaPDF <- renderPlot({
        curve( dbeta(x,input$alpha,input$beta),xlab="x",ylab="f(x)",xlim=c(0,1),n=50001,
               main=paste("Beta with alpha =",input$alpha,"and beta =",input$beta) )
        
    })
}

shinyApp(ui, server)
