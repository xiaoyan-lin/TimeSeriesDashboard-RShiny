library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
library(prophet)
library(forecast)

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  titlePanel("Forecasting", windowTitle = "Forecasting"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      fileInput("file1", "Load File (Only csv files and only time series per day)",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),
      
      checkboxInput("header", "Header", TRUE),
      
      h4("Select Variables"),      # Third level header: Plotting
      
      # Select variable for date 
      selectInput(inputId = "datevar", 
                  label = "Date (format:YYYY-MM-DD):",
                  choices = names(df)),
      
      # Select variable for Time Series 
      selectInput(inputId = "tsvar", 
                  label = "Time Series:",
                  choices = names(df)),
      
      # Set forecasting horizon
      sliderInput(inputId = "horizon", 
                  label = "Days ahead for forecasting:", 
                  min = 0, max = 100, 
                  value = 30),
      #Parameters
      h4("Select Parameters"), 
      sliderInput(inputId = "train.size", 
                  label = "Training sample size(%):", 
                  min = 0, max = 100, 
                  value = 80),
      # Set Zoom in
      sliderInput(inputId = "zoomin", 
                  label = "Choose time period to zoom in on all the plots(%)", 
                  min = 0, max = 100, 
                  value = c(0,100))
    ),
    
    # Output:
    mainPanel(
      
      tabsetPanel(id = "tabspanel", type = "tabs",
                  tabPanel(title = "File Content", 
                           tableOutput("contents")),
                  
                  tabPanel(title = "Data Visualization", 
                           plotOutput(outputId = "tseriesplot")),
                  
                  tabPanel(title = "Regression", 
                           fluidRow(h4('The result of the model'),
                                    tableOutput("regressionresult")),
                           fluidRow(h4('Plot of testing data set (red & black) and forecast values (blue)'),
                                    plotOutput(outputId = 'regressionplot')),
                           fluidRow(h4('The forecast values'),
                                    tableOutput("regressionforecast"))),
                  
                  tabPanel(title = "Classical Decomposition",
                           fluidRow(h4('The result of the model'),
                                    tableOutput("decompositionresult")),
                           fluidRow(h4('Plot of testing data set (red & black) and forecast values (blue)'),
                                    plotOutput(outputId = 'decompositionplot')),
                           fluidRow(h4('The forecast values'),
                                    tableOutput("decompositionforecast"))),
                  
                  tabPanel(title = "Smoothing Method", 
                           fluidRow(h4('The result of the model'),
                                    tableOutput("smoothresult")),
                           fluidRow(h4('Plot of testing data set (red & black) and forecast values (blue)'),
                                    plotOutput(outputId = 'smoothplot')),
                           fluidRow(h4('The forecast values'),
                                    tableOutput("smoothforecast"))
                           ),
                  
                  tabPanel(title = "Arima", 
                           fluidRow(h4('The result of the model'),
                                    tableOutput("arimaresult")),
                           fluidRow(h4('Plot of testing data set (red & black) and forecast values (blue)'),
                                    plotOutput(outputId = 'arimaplot')),
                           fluidRow(h4('The forecast values'),
                                    tableOutput("arimaforecast"))
                  ),
                  
                  tabPanel(title = "Prophet", 
                           fluidRow(h4('The result of the model'),
                                    tableOutput("prophetresult")),
                           fluidRow(h4('Plot of testing data set (red & black) and forecast values (blue)'),
                                    plotOutput(outputId = 'prophetplot')),
                           fluidRow(h4('The forecast values'),
                                    tableOutput("prophetforecast"))
                           ),
                  
                  tabPanel(title = "Summary",
                           h4('The results of the models'),
                           tableOutput("summary"))
                  
                  
      )
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  
  # Create plot object the plotOutput function is expecting 
  output$tseriesplot <- renderPlot({
    data=df()
    data[,input$datevar]=as.Date(data[,input$datevar])
    ggplot(data = data[round(input$zoomin[1]*nrow(data)/100):round(input$zoomin[2]*nrow(data)/100),], 
           aes_string(x = input$datevar, y = input$tsvar)) +
      geom_line()+scale_x_date(date_labels = "%b %y")
    
  })
  
  
  #Create file content table
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    #inFile <- input$file1
    
    if (is.null(df()))
      return(NULL)
    
    df()
  })
  
  
  # For regression method
  output$regressionresult <- renderTable({
    data <- df()[,c(input$datevar,input$tsvar)]
    names(data)=c("ds","y")
    data$ds=as.Date(data$ds)
    data$trend = c(1:nrow(df()))
    train.data=data[1:ceiling(nrow(df())*input$train.size/100),]
    
    for (i in 1:length(train.data$ds)){
      train.data$quarter[i]=(quarters(train.data$ds[i]))
    }
    
    for (i in 1:length(train.data$ds)){
      train.data$weekday[i]=(weekdays(train.data$ds[i]))
    }
    
    train.data$q1=0
    train.data$q1[train.data$quarter=='Q1']=1
    train.data$q2=0
    train.data$q2[train.data$quarter=='Q2']=1
    train.data$q3=0
    train.data$q3[train.data$quarter=='Q3']=1
    train.data$mon=0
    train.data$mon[train.data$weekday=='Monday']=1
    train.data$tue=0
    train.data$tue[train.data$weekday=='Tuesday']=1
    train.data$wed=0
    train.data$wed[train.data$weekday=='Wednesday']=1
    train.data$thu=0
    train.data$thu[train.data$weekday=='Thursday']=1
    train.data$fri=0
    train.data$fri[train.data$weekday=='Friday']=1
    train.data$sat=0
    train.data$sat[train.data$weekday=='Saturday']=1
    
    train.data$lag1=lag(train.data$y,1)
    train.data$lag2=lag(train.data$y,2)
    train.data$lag3=lag(train.data$y,3)
    train.data$lag4=lag(train.data$y,4)
    train.data$lag5=lag(train.data$y,5)
    train.data$lag6=lag(train.data$y,6)
    train.data$lag7=lag(train.data$y,7)
    train.data$lag8=lag(train.data$y,8)
    train.data$lag9=lag(train.data$y,9)
    train.data$lag10=lag(train.data$y,10)
    train.data$lag14=lag(train.data$y,14)
    
    rm=lm(y~trend+q1+q2+q3+mon+tue+wed+thu+fri+sat+lag1+lag2+lag6+lag7+lag8+lag14, data = train.data)
    
    for (i in 1:(nrow(df())-ceiling(nrow(df())*input$train.size/100))){
      newrow=train.data[nrow(train.data),]
      newrow$trend=newrow$trend+1
      newrow$ds=newrow$ds+1
      newrow$quarter=quarters(newrow$ds)
      newrow$weekday=weekdays(newrow$ds)
      newrow$q1=0
      newrow$q1[newrow$quarter=='Q1']=1
      newrow$q2=0
      newrow$q2[newrow$quarter=='Q2']=1
      newrow$q3=0
      newrow$q3[newrow$quarter=='Q3']=1
      newrow$mon=0
      newrow$mon[newrow$weekday=='Monday']=1
      newrow$tue=0
      newrow$tue[newrow$weekday=='Tuesday']=1
      newrow$wed=0
      newrow$wed[newrow$weekday=='Wednesday']=1
      newrow$thu=0
      newrow$thu[newrow$weekday=='Thursday']=1
      newrow$fri=0
      newrow$fri[newrow$weekday=='Friday']=1
      newrow$sat=0
      newrow$sat[newrow$weekday=='Saturday']=1
      train.data=rbind(train.data,newrow)
      rownames(train.data) = 1:nrow(train.data)
      train.data[nrow(train.data),]$lag1=train.data[nrow(train.data)-1,]$y
      train.data[nrow(train.data),]$lag2=train.data[nrow(train.data)-2,]$y
      train.data[nrow(train.data),]$lag3=train.data[nrow(train.data)-3,]$y
      train.data[nrow(train.data),]$lag4=train.data[nrow(train.data)-4,]$y
      train.data[nrow(train.data),]$lag5=train.data[nrow(train.data)-5,]$y
      train.data[nrow(train.data),]$lag6=train.data[nrow(train.data)-6,]$y
      train.data[nrow(train.data),]$lag7=train.data[nrow(train.data)-7,]$y
      train.data[nrow(train.data),]$lag8=train.data[nrow(train.data)-8,]$y
      train.data[nrow(train.data),]$lag9=train.data[nrow(train.data)-9,]$y
      train.data[nrow(train.data),]$lag10=train.data[nrow(train.data)-10,]$y
      train.data[nrow(train.data),]$lag14=train.data[nrow(train.data)-14,]$y
      train.data[nrow(train.data),]$y=predict(rm, newdata = train.data[nrow(train.data),], interval = "prediction")[1,'fit']
    }
      
      #restore
      test.data=train.data[(ceiling(nrow(df())*input$train.size/100)+1):nrow(train.data),]
      train.data=data[1:ceiling(nrow(df())*input$train.size/100),]
      
      error=accuracy(data[ceiling(nrow(df())*input$train.size/100+1):nrow(df()), 'y'], test.data$y)
      results=data.frame("RMSE"=error['Test set','RMSE'],"MAE"=error['Test set','MAE'], 
                         "MAPE"=paste0(round(error['Test set','MAPE'],2), "%"), 
                         "Total Records"=nrow(df()),
                         "Training Percentage"=paste0(input$train.size,"%"),
                         "Count Train"=nrow(train.data), "Count Test"=nrow(test.data), 
                         "Count Forecast"=input$horizon)
  })
  
  output$regressionplot <- renderPlot({
    data <- df()[,c(input$datevar,input$tsvar)]
    names(data)=c("ds","y")
    data$ds=as.Date(data$ds)
    data$trend = c(1:nrow(df()))
    dataall = data
    
    for (i in 1:length(data$ds)){
      data$quarter[i]=(quarters(data$ds[i]))
    }
    
    for (i in 1:length(data$ds)){
      data$weekday[i]=(weekdays(data$ds[i]))
    }
    
    data$q1=0
    data$q1[data$quarter=='Q1']=1
    data$q2=0
    data$q2[data$quarter=='Q2']=1
    data$q3=0
    data$q3[data$quarter=='Q3']=1
    data$mon=0
    data$mon[data$weekday=='Monday']=1
    data$tue=0
    data$tue[data$weekday=='Tuesday']=1
    data$wed=0
    data$wed[data$weekday=='Wednesday']=1
    data$thu=0
    data$thu[data$weekday=='Thursday']=1
    data$fri=0
    data$fri[data$weekday=='Friday']=1
    data$sat=0
    data$sat[data$weekday=='Saturday']=1
    
    data$lag1=lag(data$y,1)
    data$lag2=lag(data$y,2)
    data$lag3=lag(data$y,3)
    data$lag4=lag(data$y,4)
    data$lag5=lag(data$y,5)
    data$lag6=lag(data$y,6)
    data$lag7=lag(data$y,7)
    data$lag8=lag(data$y,8)
    data$lag9=lag(data$y,9)
    data$lag10=lag(data$y,10)
    data$lag14=lag(data$y,14) ## For all data
    
    train.data=data[1:ceiling(nrow(df())*input$train.size/100),]
    
    rm=lm(y~trend+q1+q2+q3+mon+tue+wed+thu+fri+sat+lag1+lag2+lag6+lag7+lag8+lag14, data = train.data)
    
    for (i in 1:(nrow(df())-ceiling(nrow(df())*input$train.size/100))){
      newrow=train.data[nrow(train.data),]
      newrow$trend=newrow$trend+1
      newrow$ds=newrow$ds+1
      newrow$quarter=quarters(newrow$ds)
      newrow$weekday=weekdays(newrow$ds)
      newrow$q1=0
      newrow$q1[newrow$quarter=='Q1']=1
      newrow$q2=0
      newrow$q2[newrow$quarter=='Q2']=1
      newrow$q3=0
      newrow$q3[newrow$quarter=='Q3']=1
      newrow$mon=0
      newrow$mon[newrow$weekday=='Monday']=1
      newrow$tue=0
      newrow$tue[newrow$weekday=='Tuesday']=1
      newrow$wed=0
      newrow$wed[newrow$weekday=='Wednesday']=1
      newrow$thu=0
      newrow$thu[newrow$weekday=='Thursday']=1
      newrow$fri=0
      newrow$fri[newrow$weekday=='Friday']=1
      newrow$sat=0
      newrow$sat[newrow$weekday=='Saturday']=1
      train.data=rbind(train.data,newrow)
      rownames(train.data) = 1:nrow(train.data)
      train.data[nrow(train.data),]$lag1=train.data[nrow(train.data)-1,]$y
      train.data[nrow(train.data),]$lag2=train.data[nrow(train.data)-2,]$y
      train.data[nrow(train.data),]$lag3=train.data[nrow(train.data)-3,]$y
      train.data[nrow(train.data),]$lag4=train.data[nrow(train.data)-4,]$y
      train.data[nrow(train.data),]$lag5=train.data[nrow(train.data)-5,]$y
      train.data[nrow(train.data),]$lag6=train.data[nrow(train.data)-6,]$y
      train.data[nrow(train.data),]$lag7=train.data[nrow(train.data)-7,]$y
      train.data[nrow(train.data),]$lag8=train.data[nrow(train.data)-8,]$y
      train.data[nrow(train.data),]$lag9=train.data[nrow(train.data)-9,]$y
      train.data[nrow(train.data),]$lag10=train.data[nrow(train.data)-10,]$y
      train.data[nrow(train.data),]$lag14=train.data[nrow(train.data)-14,]$y
      train.data[nrow(train.data),]$y=predict(rm, newdata = train.data[nrow(train.data),], interval = "prediction")[1,'fit']
    }
    
    test.data=train.data[(ceiling(nrow(df())*input$train.size/100)+1):nrow(train.data),]
    # For test data
    
    
    for (i in 1:input$horizon){
      newrow=data[nrow(data),]
      newrow$trend=newrow$trend+1
      newrow$ds=newrow$ds+1
      newrow$quarter=quarters(newrow$ds)
      newrow$weekday=weekdays(newrow$ds)
      newrow$q1=0
      newrow$q1[newrow$quarter=='Q1']=1
      newrow$q2=0
      newrow$q2[newrow$quarter=='Q2']=1
      newrow$q3=0
      newrow$q3[newrow$quarter=='Q3']=1
      newrow$mon=0
      newrow$mon[newrow$weekday=='Monday']=1
      newrow$tue=0
      newrow$tue[newrow$weekday=='Tuesday']=1
      newrow$wed=0
      newrow$wed[newrow$weekday=='Wednesday']=1
      newrow$thu=0
      newrow$thu[newrow$weekday=='Thursday']=1
      newrow$fri=0
      newrow$fri[newrow$weekday=='Friday']=1
      newrow$sat=0
      newrow$sat[newrow$weekday=='Saturday']=1
      data=rbind(data,newrow)
      rownames(data) = 1:nrow(data)
      data[nrow(data),]$lag1=data[nrow(data)-1,]$y
      data[nrow(data),]$lag2=data[nrow(data)-2,]$y
      data[nrow(data),]$lag3=data[nrow(data)-3,]$y
      data[nrow(data),]$lag4=data[nrow(data)-4,]$y
      data[nrow(data),]$lag5=data[nrow(data)-5,]$y
      data[nrow(data),]$lag6=data[nrow(data)-6,]$y
      data[nrow(data),]$lag7=data[nrow(data)-7,]$y
      data[nrow(data),]$lag8=data[nrow(data)-8,]$y
      data[nrow(data),]$lag9=data[nrow(data)-9,]$y
      data[nrow(data),]$lag10=data[nrow(data)-10,]$y
      data[nrow(data),]$lag14=data[nrow(data)-14,]$y
      data[nrow(data),]$y=predict(rm, newdata = data[nrow(data),], 
                                        interval = "prediction")[1,'fit']
    }
    
    forecast <- data[(nrow(df())+1):(nrow(df())+input$horizon),]
    
    ggplot(data = forecast, aes(x=ds, y=y, color = 'Forecasting \n values'))+
      geom_line()+
      geom_line(data = test.data, aes(x=ds, y=y, color = 'Testing \n set'))+
      geom_line(data = dataall[round(input$zoomin[1]*nrow(dataall)/100):round(input$zoomin[2]*nrow(dataall)/100),]
                , aes(x=ds, y=y, color = 'True \n values'))+
      scale_x_date(date_labels = "%b %y")+
      scale_color_manual(name = "", values = c("True \n values" = "black", 'Testing \n set' = 'deepskyblue2',
                                               'Forecasting \n values' = 'firebrick2'))+
      ylab('Value')
  })
  
  output$regressionforecast <- renderTable({
    data <- df()[,c(input$datevar,input$tsvar)]
    names(data)=c("ds","y")
    data$ds=as.Date(data$ds)
    data$trend = c(1:nrow(df()))
    
    for (i in 1:length(data$ds)){
      data$quarter[i]=(quarters(data$ds[i]))
    }
    
    for (i in 1:length(data$ds)){
      data$weekday[i]=(weekdays(data$ds[i]))
    }
    
    data$q1=0
    data$q1[data$quarter=='Q1']=1
    data$q2=0
    data$q2[data$quarter=='Q2']=1
    data$q3=0
    data$q3[data$quarter=='Q3']=1
    data$mon=0
    data$mon[data$weekday=='Monday']=1
    data$tue=0
    data$tue[data$weekday=='Tuesday']=1
    data$wed=0
    data$wed[data$weekday=='Wednesday']=1
    data$thu=0
    data$thu[data$weekday=='Thursday']=1
    data$fri=0
    data$fri[data$weekday=='Friday']=1
    data$sat=0
    data$sat[data$weekday=='Saturday']=1
    
    data$lag1=lag(data$y,1)
    data$lag2=lag(data$y,2)
    data$lag3=lag(data$y,3)
    data$lag4=lag(data$y,4)
    data$lag5=lag(data$y,5)
    data$lag6=lag(data$y,6)
    data$lag7=lag(data$y,7)
    data$lag8=lag(data$y,8)
    data$lag9=lag(data$y,9)
    data$lag10=lag(data$y,10)
    data$lag14=lag(data$y,14) ## For all data
    
    train.data=data[1:ceiling(nrow(df())*input$train.size/100),]
    
    rm=lm(y~trend+q1+q2+q3+mon+tue+wed+thu+fri+sat+lag1+lag2+lag6+lag7+lag8+lag14, data = train.data)
    
    
    lwr = c()
    upr = c()
    for (i in 1:input$horizon){
      newrow=data[nrow(data),]
      newrow$trend=newrow$trend+1
      newrow$ds=newrow$ds+1
      newrow$quarter=quarters(newrow$ds)
      newrow$weekday=weekdays(newrow$ds)
      newrow$q1=0
      newrow$q1[newrow$quarter=='Q1']=1
      newrow$q2=0
      newrow$q2[newrow$quarter=='Q2']=1
      newrow$q3=0
      newrow$q3[newrow$quarter=='Q3']=1
      newrow$mon=0
      newrow$mon[newrow$weekday=='Monday']=1
      newrow$tue=0
      newrow$tue[newrow$weekday=='Tuesday']=1
      newrow$wed=0
      newrow$wed[newrow$weekday=='Wednesday']=1
      newrow$thu=0
      newrow$thu[newrow$weekday=='Thursday']=1
      newrow$fri=0
      newrow$fri[newrow$weekday=='Friday']=1
      newrow$sat=0
      newrow$sat[newrow$weekday=='Saturday']=1
      data=rbind(data,newrow)
      rownames(data) = 1:nrow(data)
      data[nrow(data),]$lag1=data[nrow(data)-1,]$y
      data[nrow(data),]$lag2=data[nrow(data)-2,]$y
      data[nrow(data),]$lag3=data[nrow(data)-3,]$y
      data[nrow(data),]$lag4=data[nrow(data)-4,]$y
      data[nrow(data),]$lag5=data[nrow(data)-5,]$y
      data[nrow(data),]$lag6=data[nrow(data)-6,]$y
      data[nrow(data),]$lag7=data[nrow(data)-7,]$y
      data[nrow(data),]$lag8=data[nrow(data)-8,]$y
      data[nrow(data),]$lag9=data[nrow(data)-9,]$y
      data[nrow(data),]$lag10=data[nrow(data)-10,]$y
      data[nrow(data),]$lag14=data[nrow(data)-14,]$y
      data[nrow(data),]$y=predict(rm, newdata = data[nrow(data),], 
                                  interval = "prediction")[1,'fit']
      lwr=c(lwr, predict(rm, newdata = data[nrow(data),],
                         interval = "prediction")[1,'lwr'])
      upr=c(upr, predict(rm, newdata = data[nrow(data),],
                         interval = "prediction")[1,'upr'])
      
    }
    
    forecast <- data[(nrow(df())+1):(nrow(df())+input$horizon),'y']
    forecastdates <- seq(from=(as.Date(data$ds[nrow(df())])+1), by=1, length.out=input$horizon)
    forecastdates <- as.character(forecastdates)
    forecastvalue <- data.frame('ds'=forecastdates,
                                'yhat'=forecast, 
                                'yhat_lower'=lwr, 
                                'yhat_upper'=upr)
    
  })
  
  
  
  # For classical decomposition method
  output$decompositionresult <- renderTable({
    train.data=df()[1:ceiling(nrow(df())*input$train.size/100),c(input$datevar,input$tsvar)]
    test.data=df()[(ceiling(nrow(df())*input$train.size/100)+1):nrow(df()),c(input$datevar,input$tsvar)]
    names(train.data)=c("ds","y")
    names(test.data)=c("ds","y")
    train.data$ds=format(as.Date(train.data$ds))
    test.data$ds=format(as.Date(test.data$ds))
    
    ms=msts(train.data$y,seasonal.periods=c(7,2*7,30.5),ts.frequency=30.5)
    model=mstl(ms) # lambda = 0 do the multiplicative decomposition
    fc=forecast(model,h = nrow(test.data), level = 0,allow.multiplicative.trend = T)
    trainerror=data.frame(accuracy(fc$x,fc$fitted))
    testerror=data.frame(accuracy(fc$mean,test.data$y))
    
    results=data.frame("RMSE"=testerror$RMSE,"MAE"=testerror$MAE, 
                       "MAPE"=paste0(round(testerror$MAPE,2), "%"), 
                       "Total Records"=nrow(df()),
                       "Training Percentage"=paste0(input$train.size,"%"),
                       "Count Train"=nrow(train.data), "Count Test"=nrow(test.data), 
                       "Count Forecast"=input$horizon)
  })
  
  output$decompositionplot <- renderPlot({
    data <- df()[,c(input$datevar,input$tsvar)]
    names(data)=c("ds","y")
    data$ds=as.Date(data$ds)
    
    train.data=df()[1:ceiling(nrow(df())*input$train.size/100),c(input$datevar,input$tsvar)]
    test.data=df()[(ceiling(nrow(df())*input$train.size/100)+1):nrow(df()),c(input$datevar,input$tsvar)]
    names(train.data)=c("ds","y")
    names(test.data)=c("ds","y")
    
    train.data$ds=format(as.Date(train.data$ds))
    test.data$ds=format(as.Date(test.data$ds))
    
    msall=msts(data$y,seasonal.periods=c(7,2*7,30.5),ts.frequency=30.5)
    model2=mstl(msall) 
    forecast=forecast(model2,h = input$horizon, level = 0,allow.multiplicative.trend = T)
    
    ms=msts(train.data$y,seasonal.periods=c(7,2*7,30.5),ts.frequency=30.5)
    model=mstl(ms) # lambda = 0 do the multiplicative decomposition
    testresult=forecast(model,h = nrow(test.data), level = 0,allow.multiplicative.trend = T)
    
    data2 <- window(msall,start=c(1,round(input$zoomin[1]*nrow(data)/100)),
                    end=c(1,round(input$zoomin[2]*nrow(data)/100)))
    autoplot(data2, series = "True \n values", col = "black")+ 
      autolayer(testresult$mean,series = "Testing \n set")+
      autolayer(forecast$mean,series="Forecasting \n values")+
      ylab('Value')+theme_bw()
    
  })
  
  
  output$decompositionforecast <- renderTable({
    data=df()[,c(input$datevar,input$tsvar)]
    names(data)=c("ds","y")
    data$ds=format(as.Date(data$ds))
    
    
    train.data=df()[1:ceiling(nrow(df())*input$train.size/100),c(input$datevar,input$tsvar)]
    test.data=df()[(ceiling(nrow(df())*input$train.size/100)+1):nrow(df()),c(input$datevar,input$tsvar)]
    names(train.data)=c("ds","y")
    names(test.data)=c("ds","y")
    
    msall=msts(data$y,seasonal.periods=c(7,2*7,30.5),ts.frequency=30.5)
    model2=mstl(msall) 
    forecastall=forecast(model2,h = input$horizon, level = 95,allow.multiplicative.trend = T)
    
    forecastdates <- seq(from=(as.Date(data$ds[nrow(data)])+1), by=1, length.out=input$horizon)
    forecastdates <- as.character(forecastdates)
    forecastvalue <- data.frame('ds'=forecastdates,
                                'yhat'=forecastall[['mean']][1:input$horizon], 
                                'yhat_lower'=forecastall[['lower']][1:input$horizon], 
                                'yhat_upper'=forecastall[['upper']][1:input$horizon])
    
  })

  # For smoothing method
  output$smoothresult <- renderTable({
    train.data=df()[1:ceiling(nrow(df())*input$train.size/100),c(input$datevar,input$tsvar)]
    test.data=df()[(ceiling(nrow(df())*input$train.size/100)+1):nrow(df()),c(input$datevar,input$tsvar)]
    names(train.data)=c("ds","y")
    names(test.data)=c("ds","y")
    train.data <- ts(train.data$y, start=c(1,1), frequency = 7)
    test.data <- ts(test.data$y, start=c(1,ceiling(nrow(df())*input$train.size/100)+2), frequency = 7)
    m_smooth <- ets(train.data, allow.multiplicative.trend = T)
    forecast <- forecast(m_smooth,h=nrow(df())-ceiling(nrow(df())*input$train.size/100),level=95)
    error=accuracy(forecast, test.data)
    results=data.frame("RMSE"=error['Test set','RMSE'],"MAE"=error['Test set','MAE'], 
                       "MAPE"=paste0(round(error['Test set','MAPE'],2), "%"), 
                       "Total Records"=nrow(df()),
                       "Training Percentage"=paste0(input$train.size,"%"),
                       "Count Train"=length(train.data), "Count Test"=length(test.data), 
                       "Count Forecast"=input$horizon
                       )
  })
  
  output$smoothplot <- renderPlot({
    data <- df()[,c(input$datevar,input$tsvar)]
    names(data)=c("ds","y")
    data1 <- ts(data$y, start=c(1,1), frequency = 7)
    m_smooth1 <- ets(data1, allow.multiplicative.trend = T)
    forecast <- forecast(m_smooth1, h=input$horizon,level=95)
    train.data=df()[1:ceiling(nrow(df())*input$train.size/100),c(input$datevar,input$tsvar)]
    test.data=df()[(ceiling(nrow(df())*input$train.size/100)+1):nrow(df()),c(input$datevar,input$tsvar)]
    names(train.data)=c("ds","y")
    names(test.data)=c("ds","y")
    train.data <- ts(train.data$y, start=c(1,1), frequency = 7)
    test.data <- ts(test.data$y, start=c(1,ceiling(nrow(data)*input$train.size/100)+2), frequency = 7)
    m_smooth2 <- ets(train.data, allow.multiplicative.trend = T)
    testresult <- forecast(m_smooth2,h=nrow(data)-ceiling(nrow(data)*input$train.size/100),level=95)
    data2 <- window(data1,start=c(1,round(input$zoomin[1]*nrow(data)/100)),
                    end=c(1,round(input$zoomin[2]*nrow(data)/100)))
    autoplot(data2, series = "True \n values", col = "black")+  
      autolayer(testresult$mean,series = "Testing \n set")+
      autolayer(forecast$mean,series="Forecasting \n values")+
      ylab('Value')+theme_bw()
      
  })
  
  output$smoothforecast <- renderTable({
    data <- df()[,c(input$datevar,input$tsvar)]
    names(data)=c("ds","y")
    data1 <- ts(data$y, start=c(1,1), frequency = 7)
    m_smooth1 <- ets(data1, allow.multiplicative.trend = T)
    forecast <- forecast(m_smooth1, h=input$horizon,level=95)
    forecastdates <- seq(from=(as.Date(data$ds[nrow(data)])+1), by=1, length.out=input$horizon)
    forecastdates <- as.character(forecastdates)
    forecastvalue <- data.frame('ds'=forecastdates,
               'yhat'=forecast['mean'], 
               'yhat_lower'=forecast['lower'], 
               'yhat_upper'=forecast['upper'])
  })
  
  
  # For Arima method
  output$arimaresult <- renderTable({
    train.data=df()[1:ceiling(nrow(df())*input$train.size/100),c(input$datevar,input$tsvar)]
    test.data=df()[(ceiling(nrow(df())*input$train.size/100)+1):nrow(df()),c(input$datevar,input$tsvar)]
    names(train.data)=c("ds","y")
    names(test.data)=c("ds","y")
    train.data <- ts(train.data$y, start=c(1,1), frequency = 7)
    test.data <- ts(test.data$y, start=c(1,ceiling(nrow(df())*input$train.size/100)+2), frequency = 7)
    m_arima <- Arima(log(train.data), order=c(3,1,1),seasonal=c(0,1,1))
    forecast <- forecast(m_arima,h=nrow(df())-ceiling(nrow(df())*input$train.size/100),level=95)
    error=accuracy(exp(forecast$mean), test.data)
    data.frame("RMSE"=error['Test set','RMSE'],"MAE"=error['Test set','MAE'], 
                       "MAPE"=paste0(round(error['Test set','MAPE'],2), "%"), 
                       "Total Records"=nrow(df()),
                       "Training Percentage"=paste0(input$train.size,"%"),
                       "Count Train"=length(train.data), "Count Test"=length(test.data), 
                       "Count Forecast"=input$horizon
                       )
  })
  
  output$arimaplot <- renderPlot({
    data <- df()[,c(input$datevar,input$tsvar)]
    names(data)=c("ds","y")
    data1 <- ts(data$y, start=c(1,1), frequency = 7)
    m_arima1 <- Arima(log(data1), order=c(3,1,1),seasonal=c(0,1,1))
    forecast <- forecast(m_arima1, h=input$horizon,level=95)
    train.data=df()[1:ceiling(nrow(df())*input$train.size/100),c(input$datevar,input$tsvar)]
    test.data=df()[(ceiling(nrow(df())*input$train.size/100)+1):nrow(df()),c(input$datevar,input$tsvar)]
    names(train.data)=c("ds","y")
    names(test.data)=c("ds","y")
    train.data <- ts(train.data$y, start=c(1,1), frequency = 7)
    test.data <- ts(test.data$y, start=c(1,ceiling(nrow(data)*input$train.size/100)+2), frequency = 7)
    m_arima2 <- Arima(log(train.data), order=c(3,1,1),seasonal=c(0,1,1))
    testresult <- forecast(m_arima2,h=nrow(data)-ceiling(nrow(data)*input$train.size/100),level=95)
    data2 <- window(data1,start=c(1,round(input$zoomin[1]*nrow(data)/100)),
                    end=c(1,round(input$zoomin[2]*nrow(data)/100)))
    autoplot(data2, series = "True \n values", col = "black")+ 
      autolayer(exp(testresult$mean),series = "Testing \n set")+
      autolayer(exp(forecast$mean),series="Forecasting \n values")+
      ylab('Value')+theme_bw()
    
  })
  
  output$arimaforecast <- renderTable({
    data <- df()[,c(input$datevar,input$tsvar)]
    names(data)=c("ds","y")
    data1 <- ts(data$y, start=c(1,1), frequency = 7)
    m_arima1 <- Arima(log(data1), order=c(3,1,1),seasonal=c(0,1,1))
    forecast <- forecast(m_arima1, h=input$horizon,level=95)
    forecastdates <- seq(from=(as.Date(data$ds[nrow(data)])+1), by=1, length.out=input$horizon)
    forecastdates <- as.character(forecastdates)
    forecastvalue <- data.frame('ds'=forecastdates,
                                'yhat'=exp(forecast$mean), 
                                'yhat_lower'=exp(forecast$lower[1:input$horizon]), 
                                'yhat_upper'=exp(forecast$upper[1:input$horizon]))
  })
  
  
  
  
  # For prophet
  output$prophetresult <- renderTable({
    train.data=df()[1:ceiling(nrow(df())*input$train.size/100),c(input$datevar,input$tsvar)]
    test.data=df()[(ceiling(nrow(df())*input$train.size/100)+1):nrow(df()),c(input$datevar,input$tsvar)]
    names(train.data)=c("ds","y")
    names(test.data)=c("ds","y")
    train.data$ds=format(as.Date(train.data$ds))
    test.data$ds=format(as.Date(test.data$ds))
    
    m_prophet <- prophet(train.data,
                         growth = 'linear',
                         seasonality.mode = 'multiplicative',
                         changepoint.prior.scale = 30,
                         seasonality.prior.scale = 35
    )
    
    future <- make_future_dataframe(m_prophet, periods = nrow(df())-ceiling(nrow(df())*input$train.size/100))
    forecast <- predict(m_prophet, future)
    forecast$ds=format(forecast$ds)
    # Function that returns Root Mean Squared Error
    rmse <- function(error)
    {
      sqrt(mean(error^2))
    }
    
    # Function that returns Mean Absolute Error
    mae <- function(error)
    {
      mean(abs(error))
    }
    # Function that returns Mean Absolute Error
    mape <- function(error, truevalue)
    {
      mean(abs(error/truevalue))
    }
    error=forecast[(ceiling(nrow(df())*input$train.size/100)+1):nrow(df()),'yhat']-test.data$y
    results=data.frame("RMSE"=rmse(error),"MAE"=mae(error), 
                       "MAPE"=paste0(round(mape(error, test.data$y)*100,2), "%"), 
                       "Total Records"=nrow(df()),
                       "Training Percentage"=paste0(input$train.size,"%"),
                       "Count Train"=nrow(train.data), "Count Test"=nrow(test.data), 
                       "Count Forecast"=input$horizon)
  })
  
  output$prophetplot <- renderPlot({
    data <- df()[,c(input$datevar,input$tsvar)]
    names(data)=c("ds","y")
    data$ds=as.Date(data$ds)
    m_prophet <- prophet(data,
                         growth = 'linear',
                         seasonality.mode = 'multiplicative',
                         changepoint.prior.scale = 30,
                         seasonality.prior.scale = 35
    )
    future <- make_future_dataframe(m_prophet, periods = input$horizon, include_history = FALSE)
    forecast <- predict(m_prophet, future)
    forecast$ds=as.Date(forecast$ds)
    testdata <- data.frame(data[-(1:round(nrow(data)*input$train.size/100)),]$ds)
    names(testdata)=c('ds')
    testresult <- predict(m_prophet, testdata)
    testresult$ds=as.Date(testresult$ds)
    ggplot(data = forecast, aes(x=ds, y=yhat, color = 'Forecasting \n values'))+
      geom_line()+
      geom_line(data = testresult, aes(x=ds, y=yhat, color = 'Testing \n set'))+
      geom_line(data = data[round(input$zoomin[1]*nrow(data)/100):round(input$zoomin[2]*nrow(data)/100),]
                , aes(x=ds, y=y, color = 'True \n values'))+
      scale_x_date(date_labels = "%b %y")+
      scale_color_manual(name = "", values = c("True \n values" = "black", 'Testing \n set' = 'deepskyblue2',
                                               'Forecasting \n values' = 'firebrick2'))+
      ylab('Value')
  })
  

  
  output$prophetforecast <- renderTable({
    data=df()[,c(input$datevar,input$tsvar)]
    names(data)=c("ds","y")
    data$ds=format(as.Date(data$ds))
    
    m_prophet <- prophet(data,
                         growth = 'linear',
                         seasonality.mode = 'multiplicative',
                         changepoint.prior.scale = 30,
                         seasonality.prior.scale = 35
    )
    future <- make_future_dataframe(m_prophet, periods = input$horizon)
    forecast <- predict(m_prophet, future)
    forecast$ds=format(forecast$ds)
    tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')],input$horizon)
  })
  
  # For summary
  output$summary <- renderTable({
    data <- df()[,c(input$datevar,input$tsvar)]
    names(data)=c("ds","y")
    data$ds=as.Date(data$ds)
    data$trend = c(1:nrow(df()))
    train.data=data[1:ceiling(nrow(df())*input$train.size/100),]
    
    for (i in 1:length(train.data$ds)){
      train.data$quarter[i]=(quarters(train.data$ds[i]))
    }
    
    for (i in 1:length(train.data$ds)){
      train.data$weekday[i]=(weekdays(train.data$ds[i]))
    }
    
    train.data$q1=0
    train.data$q1[train.data$quarter=='Q1']=1
    train.data$q2=0
    train.data$q2[train.data$quarter=='Q2']=1
    train.data$q3=0
    train.data$q3[train.data$quarter=='Q3']=1
    train.data$mon=0
    train.data$mon[train.data$weekday=='Monday']=1
    train.data$tue=0
    train.data$tue[train.data$weekday=='Tuesday']=1
    train.data$wed=0
    train.data$wed[train.data$weekday=='Wednesday']=1
    train.data$thu=0
    train.data$thu[train.data$weekday=='Thursday']=1
    train.data$fri=0
    train.data$fri[train.data$weekday=='Friday']=1
    train.data$sat=0
    train.data$sat[train.data$weekday=='Saturday']=1
    
    train.data$lag1=lag(train.data$y,1)
    train.data$lag2=lag(train.data$y,2)
    train.data$lag3=lag(train.data$y,3)
    train.data$lag4=lag(train.data$y,4)
    train.data$lag5=lag(train.data$y,5)
    train.data$lag6=lag(train.data$y,6)
    train.data$lag7=lag(train.data$y,7)
    train.data$lag8=lag(train.data$y,8)
    train.data$lag9=lag(train.data$y,9)
    train.data$lag10=lag(train.data$y,10)
    train.data$lag14=lag(train.data$y,14)
    
    rm=lm(y~trend+q1+q2+q3+mon+tue+wed+thu+fri+sat+lag1+lag2+lag6+lag7+lag8+lag14, data = train.data)
    
    for (i in 1:(nrow(df())-ceiling(nrow(df())*input$train.size/100))){
      newrow=train.data[nrow(train.data),]
      newrow$trend=newrow$trend+1
      newrow$ds=newrow$ds+1
      newrow$quarter=quarters(newrow$ds)
      newrow$weekday=weekdays(newrow$ds)
      newrow$q1=0
      newrow$q1[newrow$quarter=='Q1']=1
      newrow$q2=0
      newrow$q2[newrow$quarter=='Q2']=1
      newrow$q3=0
      newrow$q3[newrow$quarter=='Q3']=1
      newrow$mon=0
      newrow$mon[newrow$weekday=='Monday']=1
      newrow$tue=0
      newrow$tue[newrow$weekday=='Tuesday']=1
      newrow$wed=0
      newrow$wed[newrow$weekday=='Wednesday']=1
      newrow$thu=0
      newrow$thu[newrow$weekday=='Thursday']=1
      newrow$fri=0
      newrow$fri[newrow$weekday=='Friday']=1
      newrow$sat=0
      newrow$sat[newrow$weekday=='Saturday']=1
      train.data=rbind(train.data,newrow)
      rownames(train.data) = 1:nrow(train.data)
      train.data[nrow(train.data),]$lag1=train.data[nrow(train.data)-1,]$y
      train.data[nrow(train.data),]$lag2=train.data[nrow(train.data)-2,]$y
      train.data[nrow(train.data),]$lag3=train.data[nrow(train.data)-3,]$y
      train.data[nrow(train.data),]$lag4=train.data[nrow(train.data)-4,]$y
      train.data[nrow(train.data),]$lag5=train.data[nrow(train.data)-5,]$y
      train.data[nrow(train.data),]$lag6=train.data[nrow(train.data)-6,]$y
      train.data[nrow(train.data),]$lag7=train.data[nrow(train.data)-7,]$y
      train.data[nrow(train.data),]$lag8=train.data[nrow(train.data)-8,]$y
      train.data[nrow(train.data),]$lag9=train.data[nrow(train.data)-9,]$y
      train.data[nrow(train.data),]$lag10=train.data[nrow(train.data)-10,]$y
      train.data[nrow(train.data),]$lag14=train.data[nrow(train.data)-14,]$y
      train.data[nrow(train.data),]$y=predict(rm, newdata = train.data[nrow(train.data),], interval = "prediction")[1,'fit']
    }
    
    #restore
    test.data=train.data[(ceiling(nrow(df())*input$train.size/100)+1):nrow(train.data),]
    train.data=data[1:ceiling(nrow(df())*input$train.size/100),]
    
    error=accuracy(data[ceiling(nrow(df())*input$train.size/100+1):nrow(df()), 'y'], test.data$y)
    model1 = 'Regression'
    rmse1 = error['Test set','RMSE']
    mae1 = error['Test set','MAE']
    mape1 = paste0(round(error['Test set','MAPE'],2), "%")
    
    
    
    train.data=df()[1:ceiling(nrow(df())*input$train.size/100),c(input$datevar,input$tsvar)]
    test.data=df()[(ceiling(nrow(df())*input$train.size/100)+1):nrow(df()),c(input$datevar,input$tsvar)]
    names(train.data)=c("ds","y")
    names(test.data)=c("ds","y")
    train.data$ds=format(as.Date(train.data$ds))
    test.data$ds=format(as.Date(test.data$ds))
    
    ms=msts(train.data$y,seasonal.periods=c(7,2*7,30.5),ts.frequency=30.5)
    model=mstl(ms) # lambda = 0 do the multiplicative decomposition
    fc=forecast(model,h = nrow(test.data), level = 0,allow.multiplicative.trend = T)
    trainerror=data.frame(accuracy(fc$x,fc$fitted))
    testerror=data.frame(accuracy(fc$mean,test.data$y))
    
    model2 = 'Classical Decomposition'
    rmse2 = testerror$RMSE
    mae2 = testerror$MAE
    mape2 = paste0(round(testerror$MAPE,2), "%")
    
    
    train.data=df()[1:ceiling(nrow(df())*input$train.size/100),c(input$datevar,input$tsvar)]
    test.data=df()[(ceiling(nrow(df())*input$train.size/100)+1):nrow(df()),c(input$datevar,input$tsvar)]
    names(train.data)=c("ds","y")
    names(test.data)=c("ds","y")
    train.data <- ts(train.data$y, start=c(1,1), frequency = 7)
    test.data <- ts(test.data$y, start=c(1,ceiling(nrow(df())*input$train.size/100)+2), frequency = 7)
    m_smooth <- ets(train.data, allow.multiplicative.trend = T)
    forecast <- forecast(m_smooth,h=nrow(df())-ceiling(nrow(df())*input$train.size/100),level=95)
    error=accuracy(forecast, test.data)
    
    model3 = 'Smoothing Method'
    rmse3 = error['Test set','RMSE']
    mae3 = error['Test set','MAE']
    mape3 = paste0(round(error['Test set','MAPE'],2), "%")
    
    
    train.data=df()[1:ceiling(nrow(df())*input$train.size/100),c(input$datevar,input$tsvar)]
    test.data=df()[(ceiling(nrow(df())*input$train.size/100)+1):nrow(df()),c(input$datevar,input$tsvar)]
    names(train.data)=c("ds","y")
    names(test.data)=c("ds","y")
    train.data <- ts(train.data$y, start=c(1,1), frequency = 7)
    test.data <- ts(test.data$y, start=c(1,ceiling(nrow(df())*input$train.size/100)+2), frequency = 7)
    m_arima <- Arima(log(train.data), order=c(3,1,1),seasonal=c(0,1,1))
    forecast <- forecast(m_arima,h=nrow(df())-ceiling(nrow(df())*input$train.size/100),level=95)
    error=accuracy(exp(forecast$mean), test.data)
    
    model4 = 'Arima'
    rmse4 = error['Test set','RMSE']
    mae4 = error['Test set','MAE']
    mape4 = paste0(round(error['Test set','MAPE'],2), "%")
    
    
    train.data=df()[1:ceiling(nrow(df())*input$train.size/100),c(input$datevar,input$tsvar)]
    test.data=df()[(ceiling(nrow(df())*input$train.size/100)+1):nrow(df()),c(input$datevar,input$tsvar)]
    names(train.data)=c("ds","y")
    names(test.data)=c("ds","y")
    train.data$ds=format(as.Date(train.data$ds))
    test.data$ds=format(as.Date(test.data$ds))
    
    m_prophet <- prophet(train.data,
                         growth = 'linear',
                         seasonality.mode = 'multiplicative',
                         changepoint.prior.scale = 30,
                         seasonality.prior.scale = 35
    )
    
    future <- make_future_dataframe(m_prophet, periods = nrow(df())-ceiling(nrow(df())*input$train.size/100))
    forecast <- predict(m_prophet, future)
    forecast$ds=format(forecast$ds)
    # Function that returns Root Mean Squared Error
    rmse <- function(error)
    {
      sqrt(mean(error^2))
    }
    
    # Function that returns Mean Absolute Error
    mae <- function(error)
    {
      mean(abs(error))
    }
    # Function that returns Mean Absolute Error
    mape <- function(error, truevalue)
    {
      mean(abs(error/truevalue))
    }
    error=forecast[(ceiling(nrow(df())*input$train.size/100)+1):nrow(df()),'yhat']-test.data$y
    
    model5 = 'Prophet'
    rmse5 = rmse(error)
    mae5 = mae(error)
    mape5 = paste0(round(mape(error, test.data$y)*100,2), "%")
    
    results=data.frame("Models"=c(model1, model2, model3, model4, model5),
                       "RMSE"=c(rmse1,rmse2,rmse3,rmse4,rmse5),
                       "MAE"=c(mae1,mae2,mae3,mae4,mae5), 
                       "MAPE"=c(mape1,mape2,mape3,mape4,mape5), 
                       "Total Records"=nrow(df()),
                       "Training Percentage"=paste0(input$train.size,"%"),
                       "Count Train"=nrow(train.data), "Count Test"=nrow(test.data), 
                       "Count Forecast"=input$horizon)
  })
  
  
  
  # Reactive objects
  df <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, header = input$header)
  })
  
  observeEvent(df(), {
    updateSelectInput(session, "datevar", choices=colnames(df()))
    updateSelectInput(session, "tsvar", choices=colnames(df()), selected = colnames(df())[2])
  })
  
}

# Create Shiny app object
shinyApp(ui = ui, server = server)
