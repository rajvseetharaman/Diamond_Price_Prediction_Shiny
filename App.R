library(shiny)

ui <- 
  fluidPage(
    headerPanel("What factors influence the price of a diamond?"),
    sidebarPanel(selectInput(inputId = "xe",label="Choose variable to plot against diamond price.",choices = c("carat","cut","color","clarity","x","y","z","depth","table")),
                 br(),
                 p("A simple linear model using multiple predictors has been created."),
                 br(),
                 p("Choose values of predictor variables to see influence on price. Red line indicates price."),
                 sliderInput(inputId = "carat",min = 0.2,max=5.01,value = 2,label = "carat"),
                 selectInput(inputId = "cut",label="cut",choices = c("Fair", "Good", "Very Good", "Premium", "Ideal")),
                 selectInput(inputId = "color",label="color",choices = c("D","E","F","G","H","I","J")),
                 selectInput(inputId = "clarity",label="clarity",choices = c("I1", "SI1", "SI2", "VS1", "VS2", "VVS1", "VVS2", "IF")),
                 sliderInput(inputId = "x",min = 0,max=10.74,value = 5,label="x"),
                 sliderInput(inputId = "y",min = 0,max=58.9,value = 30,label = "y"),
                 sliderInput(inputId = "z",min = 0,max=31.8,value = 15,label = "z"),
                 sliderInput(inputId = "depth",min = 43,max=79,value = 60,label = "depth"),
                 sliderInput(inputId = "table",min = 43,max=95,value = 60,label = "table")
                 ),
    mainPanel
    (
      plotOutput("plt"),
      textOutput('pred')
    )
    
  )



server <- function(input,output)
{
  output$plt <- renderPlot(
    {
      
      library(ggplot2)
      diamonds1 <- ggplot2 :: diamonds
      ip <- input$xe
      mod <- lm(price ~ .,data=diamonds1)
      newval <- data.frame("carat"=input$carat,"cut"=input$cut,"color"=input$color,"clarity"=input$clarity,"x"=input$x,"y"=input$y,"z"=input$z,"depth"=input$depth,"table"=input$table)
      yval=predict(mod,newval)
      if(ip %in% c('carat','x','y','z','depth','table'))
      {
        ggplot(data=diamonds1,aes(x=get(ip),y=price))+geom_point()+geom_smooth(method='lm')+labs(x=ip)+ggtitle(paste("Diamond ",ip," vs price"))+geom_line(aes(y=yval,col="red"))
      }
      else
      {
        ggplot(data=diamonds1,aes(x=get(ip),y=price))+geom_boxplot(aes(fill=get(ip)))+labs(x=ip)+ggtitle(paste("Diamond ",ip," vs price"))+geom_point(aes(y=yval,col="red",size=5))
      }

      
    })
  output$pred <- renderText(
    {
      diamonds1 <- ggplot2 :: diamonds
      ip <- input$xe
      mod <- lm(price ~ .,data=diamonds1)
      newval <- data.frame("carat"=input$carat,"cut"=input$cut,"color"=input$color,"clarity"=input$clarity,"x"=input$x,"y"=input$y,"z"=input$z,"depth"=input$depth,"table"=input$table)
      yval=predict(mod,newval)
      print(paste("Predicted diamond price: $ ",yval))
    }
  )
}

shinyApp(ui=ui,server=server)

