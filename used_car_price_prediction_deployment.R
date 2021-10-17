# Import libraries
library(shiny)
library(data.table)
library(randomForest)
library(shinyWidgets)

# Read in the RF model
model <-readRDS("multi_linear_reg.rds")


####################################
# User interface                   #
####################################

ui <- fluidPage(
  
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "radial",
    direction = c("top", "left")
  ),
  # Page header
  headerPanel('Used Car Price Predictor'),
  tags$label(h3('Input parameters')),
  # Input values
  fluidRow(
    #HTML("<h3>Input parameters</h3>"),
    column(width =2,
    numericInput("Age", 
                 label = "Age of car in months", 
                 value = 20),
    numericInput("KM", 
                 label = "KM DRIVED", 
                 value =40000),
    selectInput("Fuel_Type","Fuel_Type",c("Diesel","Petrol","CNG")),
    numericInput("HP", 
                 label = "HOURSE_POWER", 
                 value = 120),
    selectInput("Met_Color","METALIC COLOUR",c("YES","NO"))),
    column(width = 2,
    selectInput("Automatic","Automatic",c("YES","NO")),
    numericInput("cc", 
                 label = "cc", 
                 value = 2000),
    selectInput("Doors","Doors",c(2,3,4,5)),
    selectInput("Gears","Gears",c(3,4,5,6)),
    selectInput("Mfr_Guarantee","Mfr_Guarantee",c("YES","NO"))),
    column(width = 2,
    selectInput("BOVAG_Guarantee","BOVAG_Guarantee",c("YES","NO")),
    numericInput("Guarantee_Period", 
                 label = "Guarantee_Period in moths", 
                 value = 5),
    selectInput("ABS","ABS",c("YES","NO")),
    selectInput("Airbag_1","Airbag_1",c("YES","NO")),
    selectInput("Airbag_2","Airbag_2",c("YES","NO"))),
    column(width = 2,
    selectInput("Airco","Airco",c("YES","NO")),
    selectInput("Automatic_airco","Automatic_airco",c("YES","NO")),
    selectInput("Boardcomputer","Boardcomputer",c("YES","NO")),
    selectInput("CD_Player","CD_Player",c("YES","NO")),
    selectInput("Central_Lock","Central_Lock",c("YES","NO"))),
    column(width = 2,
    selectInput("Powered_Windows","Powered_Windows",c("YES","NO")),
    selectInput("Power_Steering","Power_Steering",c("YES","NO")),
    selectInput("Radio","Radio",c("YES","NO")),
    selectInput("Mistlamps","Mistlamps",c("YES","NO")),
    selectInput("Sport_Model","Sport_Model",c("YES","NO"))),
    column(width = 2,
    selectInput("Backseat_Divider","Backseat_Divider",c("YES","NO")),
    selectInput("Metallic_Rim","Metallic_Rim",c("YES","NO")),
    selectInput("Radio_cassette","Radio_cassette",c("YES","NO")),
    selectInput("Tow_Bar","Tow_Bar",c("YES","NO")),
    
    
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  )),
  
  
    tags$label(h3('Used Car Price Prediction')), # Status/Output Text Box
    
    tableOutput('tabledata') # Prediction results table
    
  
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    if(input$Fuel_Type=="CNG"){
      t <- 1
    }else if(input$Fuel_Type == "Diesel"){
      t <- 2
    }else{
      t<-3
    }
    
    
    if(input$Met_Color=="YES"){
      a <- 1
    }else{
      a<- 0
    }
    if(input$Automatic=="YES"){
      b <- 1
    }else{
      b <- 0
    }
    if(input$Mfr_Guarantee=="YES"){
      c <- 1
    }else{
      c <- 0
    }
    if(input$BOVAG_Guarantee=="YES"){
      d <- 1
    }else{
      d <- 0
    }
    if(input$ABS=="YES"){
      e <- 1
    }else{
      e <- 0
    }
    
    if(input$Airbag_1=="YES"){
      f <- 1
    }else{
      f <- 0
    }
    if(input$Airbag_2=="YES"){
      g <- 1
    }else{
      g <- 0
    }
    
    if(input$Airco=="YES"){
      h <- 1
    }else{
      h <- 0
    }
    if(input$Automatic_airco=="YES"){
      i <- 1
    }else{
      i <- 0
    }
    
    if(input$Boardcomputer=="YES"){
      u <- 1
    }else{
      u <- 0
    }
    if(input$CD_Player=="YES"){
      j <- 1
    }else{
      j <- 0
    }
    if(input$Central_Lock=="YES"){
      k <- 1
    }else{
      k <- 0
    }
    if(input$Powered_Windows=="YES"){
      l <- 1
    }else{
      l <- 0
    }
    if(input$Power_Steering=="YES"){
      m <- 1
    }else{
      m <- 0
    }
    if(input$Radio=="YES"){
      n <- 1
    }else{
      n <- 0
    }
    if(input$Mistlamps=="YES"){
      o <- 1
    }else{
      o <- 0
    }
    if(input$Metallic_Rim=="YES"){
      p <- 1
    }else{
      p <- 0
    }
    if(input$Radio_cassette=="YES"){
      q <- 1
    }else{
      q <- 0
    }
    if(input$Tow_Bar=="YES"){
      r <- 1
    }else{
      r <- 0
    }
    if(input$Sport_Model=="YES"){
      s <- 1
    }else{
      s <- 0
    }
    if(input$Backseat_Divider == "YES"){
      v<- 1
    }else{
      v<- 0
    }
    df <- data.frame(
      Name = c("Age","KM","Fuel_Type","HP","Met_Color","Automatic","cc","Doors","Gears","Mfr_Guarantee","BOVAG_Guarantee","Guarantee_Period","ABS","Airbag_1","Airbag_2","Airco",
               "Automatic_airco","Boardcomputer","CD_Player","Central_Lock","Powered_Windows","Power_Steering","Radio","Mistlamps","Sport_Model","Backseat_Divider","Metallic_Rim","Radio_cassette","Tow_Bar"),
      Value =  c(input$Age,input$KM,t,input$HP,a,b,input$cc,input$Doors,input$Gears,c,d,input$Guarantee_Period,e,f,g,h,i,u,j,k,l,m,n,o,s,v,p,q,r),
      stringsAsFactors = FALSE)
    
    Price <- 0
    df <- rbind(df,Price)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(model,test))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
