##### Interactive Shiny App User Interface End ####
################### Anna Mengjie Yu ###############

library(shiny)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(shinyjs)
library(d3heatmap)
library(networkD3)

ui <- shinyUI(fluidPage(
  titlePanel("Interactive Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        
        tabPanel("File Upload",
                 fileInput("file","Upload the file"),
                 tags$hr(),
                 hr(helpText("Select the parameters below")),
                 checkboxInput(inputId = "header",label = "Header",FALSE),
                 checkboxInput(inputId = "stringAsFactors","stringsAsFactors",FALSE),
                 br(),
                 radioButtons(inputId = "sep",label = "Separator",choices = c(Comma=",",Semicolon=";",Tab="\t",Space=""),selected = ","),
                 checkboxInput(inputId = "rowname", label = "row.name",FALSE)
        ), #tabPanel_FileUpload
        
        tabPanel("Histogram",
                 uiOutput("hist"),
                 textInput("binwidth","Enter binwidth for histogram",2),
                 colourInput("colorHistogram", "Select the colour of histogram,","blue")
        ), #tabPanel_Histogram
        
        tabPanel("ScatterPlot",
                 uiOutput("xVar"),
                 uiOutput("yVar"),
                 uiOutput("scatterColorBy"),
                 uiOutput("pointSize"),
                 h4("Download the plot"),
                 textInput("downloadFilename","Name the download file:",""),
                 radioButtons("filetype","Select the file type",choices = list("png","pdf")),
                 downloadButton("download","Download the plot")
        ), #tabPanel_Scatterplot
        
        tabPanel("BarPlot",
                 h4("One varialbe bar plot:"),
                 uiOutput("oneVarBar"),
                 uiOutput("one_additionalVar"),
                 
                 #h4("Two varialbes bar plot:"),
                 uiOutput("BarPlotColorBy"),
                 uiOutput("BarPlotGroup_checkBox"),
                 uiOutput("BarPlotGroup_var")
                 
                 #uiOutput("xVarBar"), 
                 # checkboxInput(inputId ="countXvarBar",label = "Convert x variable to category count",FALSE),
                 #uiOutput("yVarBar"), 
                 # checkboxInput(inputId = "countYvarBar", label = "Convert Y variable to category count", FALSE),
                 
        ),
        
        tabPanel("BoxPlot",
                 uiOutput("xVarBoxPlot"),
                 uiOutput("yVarBoxPlot"),
                 br(),
                 colourInput("BoxPlotColor", "Select the colour of box plot,","pink")
        ),
        
        tabPanel("LinePlot",
                 uiOutput("xVarLinePlot"),
                 uiOutput("yVarLinePlot1"),
                 uiOutput("lineGroupBy"),
                 uiOutput("lineColorBy")
        ),
        
        tabPanel("Map",
                 sliderInput("MapZoomSlide","Select the zoom in level",min = 3,max = 21,value = 5,step = 1),
                 uiOutput("LngVar"),
                 uiOutput("LatVar"),
                 uiOutput("MapColorBy"),
                 uiOutput("MapSizeBy")
        ),
        
        tabPanel("Analysis",
                 checkboxInput(inputId = "TTest_OneSample",label = "One Sample t-test for continuous data",FALSE),
                 uiOutput("TTest_1"),
                 uiOutput("Mu"),
                 uiOutput("TTest_1_result"),
                 
                 checkboxInput(inputId = "TTest_TwoSample",label = "Two Sample t-test for continuous data",FALSE), 
                 uiOutput("TTest_2_var1"),
                 uiOutput("TTest_2_var2"),
                 uiOutput("TTest_Paired"),
                 uiOutput("TTest_2_result"),
                 
                 
                 checkboxInput(inputId = "ANOVA_Test",label = "One way ANOVA test",FALSE),
                 uiOutput("ANOVA_varX"),
                 uiOutput("ANOVA_varY"),
                 uiOutput("ANOVA_result"),
                 
                 checkboxInput(inputId = "Regression",label = "Regression",FALSE),
                 uiOutput("Regression_varX"),
                 uiOutput("Regression_varY"),
                 uiOutput("Regression_result")
        ),
        
        
        tabPanel("Specific Example",
                 uiOutput("xVarAnalysis"),
                 uiOutput("yVarAnalysis"),
                 uiOutput("KMeansColorBy"),
                 uiOutput("KMeansFacetWrap"),
                 checkboxInput(inputId = "KMeansCheck",label = "Apply K Means Clustering",FALSE),
                 uiOutput("KNumber")
        ),
        
        tabPanel("Reference",
                 h5("This is the user reference page for this app."),
                 br(),
                 h5("1: upload your data, choose approriate header and separator info."),
                 br(),
                 h5("2:, to visualize the continuous variable distribution, click on the Histogram tab on both the left and right panel."),
                 br(),
                 h5("3: to visualize the relationship between any two variables, click on the ScatterPlot tab on both the left and right panel."),
                 br(),
                 h5("This app requires the following R packages: shiny, shinyjs, d3heatmap, networkD3"),
                 p(),
                 h4("More functions tabs to be added..."),
                 h5("Anna Mengjie Yu 2016"))
        
      ) #tabsetPanel
    ), #sidebarPanel
    mainPanel(
      uiOutput("tb")
    ) #mainPanel
    
    
  ) #sidebarLayout
) #fluidPage
) #shinyUI

##### Interactive Shiny App Server End ############
################### Anna Mengjie Yu ###############

options(shiny.maxRequestSize = 9*1024^2)

server <- shinyServer(function(input,output){
  
  data <- reactive({
    inputFile <- input$file
    if(is.null(inputFile)){return()}
    if (input$rowname == FALSE){
      read.table(file = inputFile$datapath,sep = input$sep,header = input$header,stringsAsFactors = input$stringAsFactors)
    }else{
      read.table(file = inputFile$datapath,sep = input$sep,header = input$header,stringsAsFactors = input$stringAsFactors, row.names = 1)
    }
    #read.table(file = inputFile$datapath,sep = input$sep,header = input$header,stringsAsFactors = input$stringAsFactors)
    #read.table(file = inputFile$datapath,sep = input$sep,header = input$header,stringsAsFactors = input$stringAsFactors, row.names = input$rowname)
  })
  
  output$sum <- renderTable({
    if(is.null(data())) {return()}
    summary(data())
  })
  
  output$table <- renderTable({
    if(is.null(data())) {return ()}
    data()
  })
  
  
  
  output$tb <- renderUI({
    if(is.null(data()))  {
      h5(paste("Welcome to use Interactive Data Visualization App!Please upload your data file."))
    }
    
    else
      tabsetPanel(
        tabPanel("Data",tableOutput("table")),
        tabPanel("Summary",tableOutput("sum")),
        tabPanel("Histogram",plotOutput("myHist")),
        tabPanel("ScatterPlot",
                 plotOutput("scatterPlot",brush = "scatterPlot_brush"),
                 radioButtons(inputId = "showBrushed",label = "Show Brushed Data Details",choices = c("Raw Data","Summary Statistics"),selected = "Raw Data"),
                 verbatimTextOutput("info")),
        tabPanel("BarPlot",
                 plotOutput("barPlot", click = "barPlot_click"),
                 h5("Statistical summary of addtional variable in selected bars:"),
                 #verbatimTextOutput("selected_bars"),
                 checkboxInput(inputId = "showHistogram_oneVar",label = "Show additional variable Histogram",TRUE),
                 plotOutput("oneBarHist")),
        tabPanel("BoxPlot",
                 plotOutput("boxPlot",click = "boxPlot_click"),
                 h5("Statistical summary of selected box:"),
                 verbatimTextOutput("selected_box")),
        tabPanel("LinePlot",
                 plotOutput("linePlot",click = "linePlot_click")
        ),
        
        tabPanel("Interactive earth map",
                 plotOutput("mapPlot")
        ),
        
        tabPanel("Heatmap",
                 selectInput("palette", "Palette", c("YlOrRd", "RdYlBu", "Greens", "Blues")),
                 sliderInput("pixels", "Height", value = 400, min = 100, max = 1000),
                 checkboxInput("cluster", "Apply clustering"),
                 uiOutput("heatmapDynamic")
        ),
        
        tabPanel("Network",
                 checkboxInput("ViewNetwork","View simpleD3network", FALSE),
                 numericInput("weightMin","Set the minimum weight cut off",-1),
                 numericInput("weightMax","Set the maximum weight cut off",1),
                 networkD3::simpleNetworkOutput("netPlot")
        ),
        
        tabPanel("Analysis",
                 # if none check boxes selected
                 textOutput("AnalysisPageText"),
                 verbatimTextOutput("TTest_1_Result_Output"),
                 verbatimTextOutput("TTest_2_Result_Output"),
                 verbatimTextOutput("ANOVA_Result_Output"),
                 verbatimTextOutput("Regression_Result_Output")
        ),
        
        
        tabPanel("Specific Example",
                 plotOutput("KMeansPlot"),
                 verbatimTextOutput("KMeansSummary")
        )
        
      )
  })
  
  ################################################ histogram #############################################################
  # show dropdown manual of continuous histogram variable
  output$hist <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    names(items)=items
    selectInput("hist", "Choose continuous variable to visualize histogram:",items)
  }) 
  
  output$myHist <- renderPlot({
    ggplot(data(),aes_string(x=input$hist)) + geom_histogram(binwidth = as.numeric(input$binwidth), fill = input$colorHistogram)
  })
  
  ################################################ scatter plot #############################################################
  # show dropdown manual of x variable
  output$xVar <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    names(items)=items
    selectInput("x", "x variable:",items)
  })
  
  # show dropdown manual of y variable
  output$yVar <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    names(items)=items
    selectInput("y", "y variable:",items)
  })
  
  # show color by option for scatterplot
  output$scatterColorBy <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    names(items)=items
    selectInput("colorBy", "Color by:",c("None",items ))
  })
  
  # show size by option for scatterplot
  output$pointSize <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    names(items)=items
    selectInput("pointSize", "Size By:",c("None",items))
  })
  
  scatterPlotObject <- reactive({
    p <- ggplot(data(),aes_string(x=input$x,y=input$y)) + geom_point()
    if (input$colorBy != "None" & input$pointSize == "None")
      p <- p + aes_string(color=input$colorBy)
    if (input$colorBy == "None" & input$pointSize != "None")
      p <- p  + aes_string(size=input$pointSize)
    if (input$colorBy != "None" & input$pointSize != "None")
      p <- p  + aes_string(color=input$colorBy,size=input$pointSize)
    
    p
  })
  
  output$scatterPlot <- renderPlot({
    scatterPlotObject()
  })
  
  output$info <- renderPrint({
    brush_data <- brushedPoints(data(), input$scatterPlot_brush, xvar = input$x, yvar = input$y)
    if (input$showBrushed == "Raw Data")
      brush_data
    else
      summary(brush_data)
  })
  
  # ScatterPlot download
  output$download <- downloadHandler(
    # specify file name
    filename = function(){
      paste(input$downloadFilename,input$filetype,sep = ".")
    },
    content = function(file){
      # open the device, create the plot, close the device png() or pdf()
      if (input$filetype == "png")
        png(file)
      else
        pdf(file)
      print(scatterPlotObject())
      dev.off()
    }
  )
  
  ################################################ bar plot #############################################################
  # show dropdown manual of y variable
  output$oneVarBar <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    names(items)=items
    selectInput("oneVar", "select one categorical variable:",items)
  })
  
  # show dropdown manual of additional variable
  output$one_additionalVar <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    items = items[items != input$oneVar]
    names(items)=items
    selectInput("one_additional", "select continuous variable to view histogram for each bar:",items)
  })
  
  output$BarPlotColorBy <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    # items = items[items != input$oneVar]
    names(items)=items
    selectInput("BarPlotColor", "select one variable to color by:",c("None",items),multiple = FALSE, selected = "None")
  })
  
  
  output$BarPlotGroup_checkBox <- renderUI({
    if (is.null(data())) return(NULL)
    checkboxInput(inputId = "BarGroupVar_checkBox",label = "Group by multiple categorical variable",FALSE)
  })
  
  output$BarPlotGroup_var <- renderUI({
    if (is.null(data())) return(NULL)
    if ( input$BarGroupVar_checkBox == TRUE ) {
      items=names(data())
      names(items)=items
      selectInput("BarPlotMultipleVar", "select 2 or more categorical variable to color by",items, multiple = TRUE)
    }
    
  })
  
  
  oneVarBarRows <- reactive({
    if (is.null(input$barPlot_click$x)) return("")
    varIndex <- match(input$oneVar,names(data()))
    round(input$barPlot_click$x) == as.numeric(factor(data()[,varIndex]))
  })
  
  oneVarBarCol <- reactive({
    match(input$one_additional,names(data()))
  })
  
  # Print the rows of the data frame which match the x value
  output$selected_bars <- renderPrint({
    if (is.null(input$barPlot_click$x)) return("")
    else {
      summary(factor(data()[oneVarBarRows(), oneVarBarCol()]))
      #summary(factor(data()[oneVarBarRows(), ]))
    }
  })
  
  # show additional variable histogram
  output$oneBarHist <- renderPlot({
    if (is.null(input$barPlot_click$x)) return("")
    if (input$showHistogram_oneVar == TRUE)
      oneBarVarData <- data()[oneVarBarRows(), oneVarBarCol()]
    #ggplot(data()[],aes_string(x=input$one_additional)) + geom_histogram()
    hist(oneBarVarData,main = paste("Histogram of",input$one_additional,"in selected bar"),col = "grey",xlab = input$one_additional)
  })
  
  barPlotObject <- reactive({
    barPlotData <- data()
    barPlotData[,input$oneVar] <- factor(barPlotData[,input$oneVar])
    
    p <- ggplot(barPlotData,aes_string(x=input$oneVar)) + geom_bar()
    if (input$BarPlotColor != "None" & input$BarGroupVar_checkBox == FALSE)
      p <- p + aes_string(fill=input$BarPlotColor)
    # if multiple groups selected
    if (input$BarGroupVar_checkBox == TRUE){
      # create new column to concatenate selected var combination
      temp_barPlotData <- barPlotData
      temp_barPlotData$newcol <- apply(temp_barPlotData[,input$BarPlotMultipleVar],1,paste,collapse = "_")
      p <- ggplot(temp_barPlotData,aes_string(x=input$oneVar, fill = "newcol")) + geom_bar()
    }
    
    p
  })
  
  # barPlot
  output$barPlot <- renderPlot({
    barPlotObject()
  })
  ################################################ box plot #############################################################
  # x variable for boxPlot
  output$xVarBoxPlot <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    names(items)=items
    selectInput("xBoxPlot", "select one categorical variable:",items)
  })
  
  # y variable for boxPlot
  output$yVarBoxPlot <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    names(items)=items
    selectInput("yBoxPlot", "select one continuous variable:",items)
  })
  
  # boxPlot
  output$boxPlot <- renderPlot({
    boxPlotData <- data()
    # factor x varaible in boxPlot
    boxPlotData[,input$xBoxPlot] <- factor(boxPlotData[,input$xBoxPlot])
    p <- ggplot(boxPlotData,aes_string(x=input$xBoxPlot, y = input$yBoxPlot)) + geom_boxplot(fill = input$BoxPlotColor)
    p
  })
  
  # clicked Box
  clickedBoxRows <- reactive({
    if (is.null(input$boxPlot_click$x)) return("")
    round(input$boxPlot_click$x) == as.numeric(factor(data()[,input$xBoxPlot]))
  })
  
  # Print statistical summary of the clicked box
  output$selected_box <- renderPrint({
    if (is.null(input$boxPlot_click$x)) return("")
    else {
      summary(data()[clickedBoxRows(),])
    }
  })
  
  ############################################## line plot ###################################################################
  # x variable for linePlot
  output$xVarLinePlot <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    names(items)=items
    selectInput("xLinePlot", "select x variable for line plot:",items)
  })
  
  # y1 variable for linePlot
  output$yVarLinePlot1 <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    names(items)=items
    selectInput("yLinePlot1", "select 1st continuous y variable:",items)
  })
  
  # group by for linePlot
  output$lineGroupBy <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    names(items)=items
    selectInput("lineGroupBy", "select variable to group by:",c("None",items))
  })
  
  # colour by for linePlot
  output$lineColorBy <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    names(items)=items
    selectInput("lineColorBy", "select variable to color by:",c("None",items))
  })
  
  # linePlot
  linePlotObject <- reactive({
    p <- ggplot(data(),aes_string(x=input$xLinePlot, y = input$yLinePlot1)) + geom_line()
    if (input$lineGroupBy != "None" & input$lineColorBy == "None")
      p <- p + aes_string(group = input$lineGroupBy)
    if (input$lineGroupBy == "None" & input$lineColorBy != "None")
      p <- p  + aes_string(colour = input$lineColorBy)
    if (input$lineGroupBy != "None" & input$lineColorBy != "None")
      p <- p  + aes_string(group = input$lineGroupBy,colour = input$lineColorBy)
    
    p
  })
  
  output$linePlot <- renderPlot({
    linePlotObject()
  })
  
  ################################################ map #############################################################
  # select longitutidal variable
  output$LngVar <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    names(items)=items
    selectInput("longituteVar", "select longitudinal variable:",c("None",items))
  })
  
  # select latitudinal variable
  output$LatVar <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    names(items)=items
    selectInput("latitudinalVar", "select latitudinal variable:",c("None",items))
  })
  
  # select latitudinal variable
  output$MapColorBy <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    names(items)=items
    selectInput("MapColorVar", "select categorical variable to color by:",c("None",items))
  })
  
  # select latitudinal variable
  output$MapSizeBy <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    names(items)=items
    selectInput("MapSizeVar", "select variable to size by:",c("None",items))
  })
  
  # mapPlot
  mapPlotObject <- reactive({
    
    # specify the geographic position scope
    left <- min(data()[,input$longituteVar])
    right <- min(data()[,input$longituteVar])
    
    currentMap <- get_map(location = c(left =min(data()[,input$longituteVar]), right = max(data()[,input$longituteVar]), bottom = min(data()[,input$latitudinalVar]), top = max(data()[,input$latitudinalVar]) ), zoom = input$MapZoomSlide)
    p <- ggmap(currentMap, fullpage = TRUE)
    if (input$MapColorVar == "None" & input$MapSizeVar == "None")
      p <- p + geom_point(aes_string(x = input$longituteVar, y = input$latitudinalVar), data = data())
    if (input$MapColorVar != "None" & input$MapSizeVar == "None")
      p <- p + geom_point(aes_string(x = input$longituteVar, y = input$latitudinalVar, colour =  input$MapColorVar), data = data())
    if (input$MapColorVar == "None" & input$MapSizeVar != "None")
      p <- p + geom_point(aes_string(x = input$longituteVar, y = input$latitudinalVar, size = input$MapSizeVar), data = data())
    if  (input$MapColorVar != "None" & input$MapSizeVar != "None")
      p <- p + geom_point(aes_string(x = input$longituteVar, y = input$latitudinalVar, colour =  input$MapColorVar, size = input$MapSizeVar), data = data())
    
    p
  })
  
  output$mapPlot <- renderPlot({
    mapPlotObject()
  })
  
  ################################################ Analysis #############################################################
  # if none of the check boxes checked
  output$AnalysisPageText <- renderPrint({
    if (is.null(data())) return()
    
    if (input$TTest_OneSample == FALSE & input$TTest_TwoSample == FALSE & input$ChiSquare == FALSE & input$ANOVA_Test == FALSE & input$Regression == FALSE )
      paste( "Please select on of the check boxes for your analysis!" )
  })
  
  ################################################# one sample T test ##################################################
  output$TTest_1 <- renderUI({
    if (is.null(data())) return()
    items=names(data())
    names(items)=items
    
    if (input$TTest_OneSample == TRUE){
      selectInput("TTest_One", "Select continuous variable for one sample t-test:",c("None",items))
    }
  })
  
  # Value of Mu
  output$Mu <- renderUI({
    if (is.null(data())) return()
    items=names(data())
    names(items)=items
    
    if (input$TTest_OneSample == TRUE){
      numericInput("Mu", "Enter mu for one sample t-test",0) 
    }
  })
  
  # View result checkbox
  output$TTest_1_result <- renderUI({
    if (is.null(data())) return()
    items=names(data())
    names(items)=items
    
    if (input$TTest_OneSample == TRUE){
      checkboxInput("View_TTest_1_Result","View Result",FALSE) 
    }
  })
  
  output$TTest_1_Result_Output <- renderPrint({
    if (is.null(data())) return() 
    if (input$TTest_OneSample == FALSE) return()
    if(input$TTest_One == "None") return()
    if (input$View_TTest_1_Result == TRUE & input$View_TTest_1_Result != "None")
      t.test(data()[,input$TTest_One], mu = input$Mu)
  })
  
  
  ####################################### two sample T test ########################################################
  output$TTest_2_var1 <- renderUI({
    if (is.null(data())) return()
    items=names(data())
    names(items)=items
    
    if (input$TTest_TwoSample == TRUE){
      selectInput("TTest_Two_Var1", "Select 1st continuous variable for two sample t-test:",c("None",items))
    }
  })
  
  output$TTest_2_var2 <- renderUI({
    if (is.null(data())) return()
    items=names(data())
    names(items)=items
    
    if (input$TTest_TwoSample == TRUE){
      selectInput("TTest_Two_Var2", "Select 2nd continuous variable for two sample t-test:",c("None",items))
    }
  })
  
  output$TTest_Paired <- renderUI({
    if (is.null(data())) return()
    if (input$TTest_TwoSample == TRUE){
      checkboxInput("Paired_TTest", "Paired",FALSE)
    }
  })
  
  # View result checkbox
  output$TTest_2_result <- renderUI({
    if (is.null(data())) return()
    if (input$TTest_TwoSample == TRUE){
      checkboxInput("View_TTest_2_Result","View Result",FALSE) 
    }
  })
  
  output$TTest_2_Result_Output <- renderPrint({
    if (is.null(data())) return()
    if (input$TTest_TwoSample == FALSE) return()
    if(input$TTest_Two_Var1 == "None" | input$TTest_Two_Var2 == "None" ) return()
    if (input$View_TTest_2_Result == TRUE)
      t.test(data()[,input$TTest_Two_Var1], data()[,input$TTest_Two_Var2], paired = input$Paired_TTest )
  })
  
  
  ####################################### ANOVA ########################################################
  output$ANOVA_varX <- renderUI({
    if (is.null(data())) return()
    items=names(data())
    names(items)=items
    
    if (input$ANOVA_Test == TRUE){
      selectInput("ANOVA_x", "Select categorical x variable for one way ANOVA:",c("None",items))
    }
  })
  
  output$ANOVA_varY <- renderUI({
    if (is.null(data())) return()
    items=names(data())
    names(items)=items
    
    if (input$ANOVA_Test == TRUE){
      selectInput("ANOVA_y", "Select y variable for one way ANOVA:",c("None",items))
    }
  })
  
  # View result checkbox
  output$ANOVA_result <- renderUI({
    if (is.null(data())) return()
    if (input$ANOVA_Test == TRUE){
      checkboxInput("View_ANOVA_Result","View ANOVA Result",FALSE) 
    }
  })
  
  output$ANOVA_Result_Output <- renderPrint({
    if (is.null(data())) return()
    if (input$ANOVA_Test == FALSE) return()
    if(input$ANOVA_x == "None" | input$ANOVA_y == "None") return()
    if (input$View_ANOVA_Result == TRUE){
      ANOVA_X <- factor(data()[,input$ANOVA_x])
      ANOVA_Y <- data()[,input$ANOVA_y]
      ANOVA_Model <- aov(ANOVA_Y ~ ANOVA_X, data())
      summary(ANOVA_Model)
    }
  })
  
  ####################################### Regression ########################################################
  output$Regression_varX <- renderUI({
    if (is.null(data())) return()
    items=names(data())
    names(items)=items
    
    if (input$Regression == TRUE){
      selectInput("Regression_x", "Select 1 or more continuous x variable (max 3) for Regression:",c("None",items), multiple = TRUE, selected = "None")
    }
  })
  
  output$Regression_varY <- renderUI({
    if (is.null(data())) return()
    items=names(data())
    names(items)=items
    
    if (input$Regression == TRUE){
      selectInput("Regression_y", "Select continuous y variable for Regression:",c("None",items))
    }
  })
  
  # View result checkbox
  output$Regression_result <- renderUI({
    if (is.null(data())) return()
    if (input$Regression == TRUE){
      checkboxInput("View_Regression_Result","View Regression Result",FALSE) 
    }
  })
  
  output$Regression_Result_Output <- renderPrint({
    if (is.null(data())) return()
    if (input$Regression == FALSE) return()
    if(input$Regression_x == "None" | input$Regression_y == "None") return()
    if (input$View_Regression_Result == TRUE){
      Regression_X <- data()[,input$Regression_x]
      Regression_Y <- data()[,input$Regression_y]
      if (length(input$Regression_x) == 1)
        Regression_Model <- lm(Regression_Y ~ data()[,input$Regression_x], data())
      if (length(input$Regression_x) == 2)
        Regression_Model <- lm(Regression_Y ~ data()[,input$Regression_x[1]] + data()[,input$Regression_x[2]], data())
      if (length(input$Regression_x) == 3)
        Regression_Model <- lm(Regression_Y ~ data()[,input$Regression_x[1]] + data()[,input$Regression_x[2]] + data()[,input$Regression_x[3]], data())
      summary(Regression_Model)
    }
  })
  
  ################################################ Heatmap #############################################################
  
  output$heatmap <- renderD3heatmap({
    d3heatmap(
      data(),
      colors = input$palette,
      dendrogram = if (input$cluster) "both" else "none"
    )
  })
  
  output$heatmapDynamic <- renderUI({
    d3heatmapOutput("heatmap", height = paste0(input$pixels, "px"))
  })
  
  
  ################################################ Network #############################################################
  
  output$netPlot <- networkD3::renderSimpleNetwork({
    
    if (input$ViewNetwork == TRUE){
      network_Data <- data.frame(data())
      network_Data_filter <- filter(network_Data, network_Data[,3] >= input$weightMin)
      network_Data_filter <- filter(network_Data_filter, network_Data_filter[,3] <= input$weightMax)
      networkD3:: simpleNetwork(network_Data_filter[,1:2], width = NULL, height = NULL, zoom = T)
    }
  })
  
  ################################################ KMeans #############################################################
  output$xVarAnalysis <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    names(items)=items
    selectInput("xVar_Analysis", "select x variable:",c("None",items))
  })
  
  output$yVarAnalysis <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    names(items)=items
    selectInput("yVar_Analysis", "select y variable:",c("None",items))
  })
  
  output$KMeansColorBy <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    names(items)=items
    selectInput("colorBy_Analysis", "Color By:",c("None",items))
  })
  
  output$KMeansPlot <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    names(items)=items
    selectInput("colorBy_Analysis", "Color By:",c("None",items))
  })
  
  output$KMeansFacetWrap <- renderUI({
    if (is.null(data())) return(NULL)
    items=names(data())
    names(items)=items
    selectInput("FacetWrap_Analysis", "Facet By:",c("None",items))
  })
  
  
  # KMeans Plot
  KMeansPlotObject <- reactive({
    if(input$xVar_Analysis == "None" | input$yVar_Analysis == "None"){return()}
    KMeansData_temp <- data()
    KMeansData_temp <- data.frame(KMeansData_temp)
    #remove NA
    KMeansData_temp <- filter(KMeansData_temp, !is.na(KMeansData_temp[,input$xVar_Analysis]))
    KMeansData_temp <- filter(KMeansData_temp, !is.na(KMeansData_temp[,input$yVar_Analysis]))
    
    p <- ggplot(KMeansData_temp, aes_string(input$xVar_Analysis,input$yVar_Analysis)) + geom_point()
    if (input$colorBy_Analysis != "None" & input$FacetWrap_Analysis == "None")
      p <- p + aes_string(colour = input$colorBy_Analysis)
    if (input$colorBy_Analysis == "None" & input$FacetWrap_Analysis != "None")
      p <- p +  facet_wrap(as.formula(paste("~", input$FacetWrap_Analysis)))
    if (input$colorBy_Analysis != "None" & input$FacetWrap_Analysis != "None")
      p <- p +  aes_string(colour = input$colorBy_Analysis) + facet_wrap(as.formula(paste("~", input$FacetWrap_Analysis)))
    p
  })
  
  output$KMeansPlot <- renderPlot({
    KMeansPlotObject()
  })
  
  output$KNumber <- renderUI({
    if(input$KMeansCheck == TRUE)
      numericInput("KMeans_Number", "Enter the number of cluster (K value):",3)
  })
  
  #     
  #     output$KMeansSummary <- renderPrint({
  #       KMeansResultObject()$centers
  #     })
  #     
  
}
            
)

shinyApp(ui,server)