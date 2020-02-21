
library(shiny)
library("ggplot2")
library("reshape2")


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Investment Report"),
    
    # Sidebar with input and values 
    fluidRow(
        column(3,
               sliderInput(inputId = "initial",
                           label = "Initial Amont:",
                           min = 0,
                           max = 10000,
                           step = 100,
                           value = 1000,
                           pre = "$"),
               sliderInput(inputId = "annual",
                           label = "Annual Contribution:",
                           min = 0,
                           max = 5000,
                           step = 100,
                           value = 200,
                           pre = "$"),
               sliderInput(inputId = "growth",
                           label = "Annual Growth Rate:",
                           min = 0,
                           max = 20,
                           post = "%",
                           step = 0.1,
                           value = 2)),
        column(3,
               sliderInput(inputId = "yield",
                           label = "High Yield rate:",
                           min = 0,
                           max = 20,
                           post = "%",
                           step = 0.1,
                           value = 2),
               sliderInput(inputId = "income",
                           label = "Fixed Income rate:",
                           min = 0,
                           max = 20,
                           post = "%",
                           step = 0.1,
                           value = 5 ),
               sliderInput(inputId = "equity",
                           label = "US Equity rate:",
                           min = 0,
                           max = 20,
                           post = "%",
                           step = 0.1,
                           value = 10 )),
        column(3,
               sliderInput(inputId = "hyv",
                           label = "High Yield volatility:",
                           min = 0,
                           max = 20,
                           post = "%",
                           step = 0.1,
                           value = 0.1 ),
               sliderInput(inputId = "fiv",
                           label = "Fixed Income volatility:",
                           min = 0,
                           max = 20,
                           post = "%",
                           step = 0.1,
                           value = 4.5 ),
               sliderInput(inputId = "eyv",
                           label = "US Equity volatility:",
                           min = 0,
                           max = 20,
                           post = "%",
                           step = 0.1,
                           value = 15 )),
        column(3,
               sliderInput(inputId = "year",
                           label = "Years:",
                           min = 0,
                           max = 50,
                           step = 1,
                           value = 20 ),
               numericInput(inputId = "seed",
                            label = "Random Seed:",
                            value = 12345),
               selectInput(inputId = "facet",
                           label = "Facet?",
                           choice = c("Yes","No"),
                           selected = "Yes")),
    ),
    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("distPlot") )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        # rate of return
        set.seed(input$seed)
        yield_rate <- rnorm(1,mean = input$yield / 100,sd = input$hyv / 100)
        set.seed(input$seed)
        bound_rate <- rnorm(1,mean = input$income / 100,sd = input$fiv / 100)
        set.seed(input$seed)
        stock_rate <- rnorm(1,mean = input$equity / 100,sd = input$eyv / 100)
        
        # for loop
        yield_output <- input$initial
        for (i in 2: (input$year + 1)) {
            yield_output[i] <- yield_output[i - 1] * (1+yield_rate) + input$annual * (1 + (input$growth / 100))^(i - 2)
        }
        
        bound_output <- input$initial
        for (i in 2: (input$year + 1)) {
            bound_output[i] <- bound_output[i - 1] * (1 + bound_rate) + input$annual * (1 + (input$growth / 100))^(i - 2)
        }
        
        
        stock_output <- input$initial
        for (i in 2: (input$year + 1)) {
            stock_output[i] <- stock_output[i - 1] * (1+stock_rate) + input$annual * (1 + (input$growth / 100))^(i - 2)
        }
        
        # data frame
        dat <- data.frame( year = 0:input$year, 
                           high_yield = yield_output, 
                           us_bound = bound_output,
                           us_stock = stock_output)
        dfm <- melt(dat,
                    id = "year")
        names(dfm) <- c("year", "index", "value")
        
        
        if (input$facet == "Yes") {
            ggplot(dfm, aes(year, value)) +
                geom_point(aes(colour = index)) +
                geom_line(aes(colour = index)) +
                ylab("amount") +
                facet_wrap(~index)+
                ggtitle("Three indices") +
                geom_area(aes(fill = index), alpha = 0.4) +
                theme_bw() +
                xlim(0, input$year)
        } else {
            ggplot(dfm, aes(year, value)) +
                geom_point(aes(colour = index)) +
                geom_line(aes(colour = index)) +
                ylab("amount") +
                ggtitle("Three indices") +
                theme_bw() +
                xlim(0, input$year)
        }
        
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
