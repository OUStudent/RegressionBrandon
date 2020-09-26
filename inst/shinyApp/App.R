# Load libraries needed
require(shiny)
require(purrr)
require(plotly)
source("Rcode.R")

spruce.df = read.csv("SPRUCE.csv")

d = spruce.df$BHDiameter

IND = reactiveValues() # indices for root


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Spruce Data Set: Piecewise Regression"),

  # Sidebar with a slider input for number of bins

  sidebarLayout(
    sidebarPanel(
      sliderInput("xk1",
                  "Choose knot 1:",
                  min = min(d),
                  max = max(d),
                  value = 11,
                  step=0.01),
      sliderInput("xk2",
                  "Choose knot 2:",
                  min = min(d),
                  max = max(d),
                  value = 19,
                  step=0.01),
      sliderInput("iv1",
                  "Root Interval for knots:",
                  min = min(d),
                  max = max(d),
                  value = c(8,21),
                  step=0.01),
      sliderInput("knotAmount",
                  "Choose number of divisions for Grid Approximation:",
                  min=50,
                  max = 300,
                  value = 150,
                  step = 5),
      sliderInput("r2Amount",
                  "Choose filter value for R2 in Grid Approximation:",
                  min = 0.70,
                  max = 0.783,
                  value = 0.75,
                  step = 0.01)

    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("regressPlot"),
      plotlyOutput("R2"),
      tableOutput("root"),
      # table of data
      plotOutput("allroots")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  observe({
    updateSliderInput(session, inputId = "xk1", max = input$xk2)
    updateSliderInput(session, inputId = "xk2", min = input$xk1)
  })


  output$regressPlot <- renderPlot({

    plot(spruce.df,main="Piecewise regression",pch=21,bg="black")
    cf = coeff(input$xk1, input$xk2, spruce.df)
    curve(myf2(x, input$xk1, input$xk2, cf), add=TRUE, lwd=2, col="Blue")

    # points for user estimated knots
    points(input$xk1,myf2(input$xk1, input$xk1,input$xk2,cf),col="black",pch=21,bg="green",cex=2)
    points(input$xk2,myf2(input$xk2, input$xk1,input$xk2,cf),col="black",pch=21,bg="green",cex=2)
    # points for actual knots
    points(11.3875,myf2(11.3875, input$xk1, input$xk2, cf),col="black",pch=21,bg="purple",cex=2)
    points(18.88,myf2(18.88, input$xk1, input$xk2, cf),col="black",pch=21,bg="purple",cex=2)

    temp = rsqMult(input$xk1, input$xk2, spruce.df)
    txt = sprintf("R2 = %0.4f", temp)
    text(input$xk1,16,txt)

  })


  output$R2 <- renderPlotly({

    knots = seq(input$iv1[1], input$iv1[2], length=input$knotAmount)
    myGrid = expand.grid(knots, knots)
    r2 = map2_dbl(myGrid$Var1, myGrid$Var2, ~rsqMult(.x, .y, spruce.df))
    df = data.frame(x=myGrid$Var1, y=myGrid$Var2, z=r2)
    sd = sd(df$z)

    if(min(df$z) < input$r2Amount) {
      df = df %>% filter(z>= mean(df$z)+sd)
    }
    else {
      df = df %>% filter(z>= input$r2Amount)
    }

    indices = which(df$z == max(df$z), arr.ind = TRUE)
    ind = df[indices,  ]
    IND$ind = c(max(df$z), ind[1,1], ind[1, 2])

    r2 = df$z
    knot2 = df$x
    knot1 = df$y
    plot_ly(x=~knot1, y=~knot2, z=~r2, type="mesh3d")
  })



  output$root<-renderTable({
    ind = input$indices
    temp = isolate(IND)$ind
    t2 = matrix(temp, ncol=3)
    colnames(t2) = c("Estimated Root", "xKnot1", "xKnot2")
    t3=as.data.frame(t2)
    t3
  })


}

# Run the application
shinyApp(ui = ui, server = server)

