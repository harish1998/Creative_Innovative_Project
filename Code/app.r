#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(LICORS)
library(leaflet)
#library(shinyIncubator)
library(cluster)
library(ggmap)
source("helper.R")

#if(interactive()){
ui <- fluidPage( dashboardPage(
  dashboardHeader(title = "Crime Analysis System",titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(
      menuItem("HOME", tabName = "home", icon = icon("home")),
      menuItem("UPLOAD DATA", tabName = "uploadData", icon = icon("upload")),
      menuItem("ANALYZE DATA", tabName = "analyseData", icon = icon("wpexplorer")),
      menuItem("VISUALIZE DATA", tabName = "visualizeData", icon = icon("pie-chart")),
      menuItem("HELP", tabName = "help", icon = icon("question-circle"))
    )),
  dashboardBody(
    
    tags$head(tags$style(HTML('
                              .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 24px;}'))),
    
    
    
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                #img(src = 'image2.png', height = '200px', width = '1000px'),
                h2("About the application"),
                div(style = "color:grey;text-align:justify", h4(
                  paste(" Crime analysis is a law enforcement function that
                        involves systematic analysis for identifying and 
                        analyzing patterns and trends in crime and disorder. 
                        Information on patterns can help law enforcement agencies
                        deploy resources in a more effective manner, and assist detectives
                        in identifying and apprehending suspects. Crime analysis also plays 
                        a role in devising solutions to crime problems, and formulating crime
                        prevention strategies. Quantitative social science data analysis methods
                        are part of the crime analysis process, though qualitative methods such
                        as examining police report narratives also play a role.
                        ")),h4(paste("       \nCrime analysis can occur at 
                                     various levels, including tactical, operational, and strategic
                                     This application is to demonstrate the efficiency of clustering
                                     algorithm which is employed and efficiency is analysed with metrics,
                                     the dataset based on the severity(high rate to low rate)level of the crime along
                                     Map view for the resultant",sep = "\n\n"))
                )
              )
      ),
      tabItem(tabName = "uploadData",
              fluidRow(
                titlePanel("Upload Your Data"),
                sidebarLayout(
                  sidebarPanel(
                    fileInput('file1', 'Choose CSV File',
                              accept=c('text/csv', 
                                       'text/comma-separated-values,text/plain', 
                                       '.csv')),
                    
                    
                    tags$br(),
                    checkboxInput('header', 'Header', TRUE),
                    radioButtons('sep', 'Separator',
                                 c(Comma=',',
                                   Semicolon=';',
                                   Tab='\t'),
                                 ','),
                    radioButtons('quote', 'Quote',
                                 c(None='',
                                   'Double Quote'='"',
                                   'Single Quote'="'"),
                                 '"')),
                  mainPanel(
                    # fetch input csv file from the backend as data table
                    dataTableOutput('content')
                  )
                ))),
      tabItem(tabName = "analyseData",
              fluidRow(
                navbarPage("",
                           tabPanel("Optimal Cluster Value",
                                    
                                    plotOutput("plot3")
                           ),
                           tabPanel("Efficiency",
                                    pageWithSidebar(
                                      headerPanel(''),
                                      sidebarPanel(
                                        
                                        #selectInput('xcol', 'X Variable', ""),
                                        #selectInput('ycol', 'Y Variable', "", selected = ""),
                                        
                                        numericInput('clusters', 'Cluster count',0),
                                        helpText("Algorithm 1 Ratio"),
                                        textOutput("ratio"),
                                        helpText("Algorithm 2 Ratio"),
                                        textOutput("ratio1")
                                        
                                      ),
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel("Algorithm 1",
                                                   plotOutput('plot4'),h6("Centers Matrix"),
                                                   tableOutput('table1')
                                          ),
                                          tabPanel("Algorithm 2",
                                                   plotOutput('plot5'),
                                                   h6("centers Matrix"),
                                                   tableOutput('table2')
                                          )
                                          
                                        )
                                      )
                                    ) ),
                           tabPanel("Analysis for two attributes",
                                    pageWithSidebar(
                                      headerPanel(''),
                                      sidebarPanel(
                                        
                                        selectInput('xcol', 'Choose crime type for X axis', ""),
                                        selectInput('ycol', 'Choose crime type for Y axis', "", selected = ""),
                                        
                                        numericInput('clust', 'Cluster count',0),
                                        helpText("Efficiency Ratio"),
                                        textOutput("ratio3")
                                        
                                      ),
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel("Clustered Plot",
                                                   plotOutput('plot1'),h6("Centers Matrix"),
                                                   tableOutput('table3')),
                                          tabPanel("Normal Plot",
                                                   plotOutput('plot2')
                                          )))
                                    )
                                    
                           ))
              )),
      tabItem(tabName = "visualizeData",
              
              fluidRow(
                titlePanel("Reports"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput('zcol', "Choose a crime type", ""),
                    numericInput('clust1', "Required level",0)
                    
                  ),
                  mainPanel(
                    navbarPage(" ",
                               tabPanel("Chart",
                                        div(style="display: inline-block;width:150px;float:right",
                                            downloadButton(outputId = "down", label = "Download the plot")),
                                        plotOutput('plot6')
                               ),tabPanel("Table",
                                          div( style="display: inline-block;width:150px",  numericInput('level', 'level', 0)),
                                          
                                          div(style="display: inline-block;width:150px;float:right",tags$b("Total Count"),
                                              verbatimTextOutput('count',placeholder = TRUE)),
                                          tags$br(),
                                          div(style="display:inline-block;float:right",downloadLink("downloadData1", "Click to download the Result table")),
                                          DT::dataTableOutput('table14'),plotOutput('bplot')     ),
                               tabPanel("Map",
                                        leafletOutput("mymap"))) )
                ))),
      tabItem(tabName = "help",
              fluidRow(
                tags$iframe(style="height:600px; width:100%", src="HelpGuide.pdf"))
      )))
  
)






)






























server <- function(input, output,session) {
  
  
  data <- reactive({ 
    req(input$file1) 
    
    inFile <- input$file1 
    
    #To Fetch the CSV that is uploaded in the UI
    
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    
    updateSelectInput(session, inputId = 'xcol', label = 'Choose crime type for X axis',
                      choices = names(df[,2:ncol(df)]), selected = names(df[,2:ncol(df)]))
    updateSelectInput(session, inputId = 'ycol', label = 'Choose crime type for Y axis',
                      choices = names(df), selected = names(df)[3])
    updateSelectInput(session, inputId = 'zcol', label = 'Choose crime type ',
                      choices = names(df[,2:ncol(df)]), selected = names(df[,2:ncol(df)]))
    updateNumericInput(session,inputId = 'clusters',label='Cluster count',value=3,
                       min = 1,max = 9)
    updateNumericInput(session,inputId = 'clust',label='Cluster count',value=3,
                       min = 1,max = 9)
    updateNumericInput(session,inputId = 'clust1',label='Required level',value=3,
                       min = 1,max = 9)
    updateNumericInput(session,inputId = 'level',label='level count',value=1,
                       min = 1,max = 9)
    #updateTextInput(session, inputId='count', label = 'Total Count', value =nrow(lels())) )
    
    crime<-df
    crime19<-df[,2:ncol(df)]
    print(crime19)
    
    # To perform elbow curve method
    kmeans.wss.k <- function(crime19, k){
      km = kmeanspp(crime19, k)
      return (km$tot.withinss)
    }
    kmeans.dis <- function(crime19, maxk){
      dis=(nrow(crime19)-1)*sum(apply(crime19,2,var))
      dis[2:maxk]=sapply (2:maxk, kmeans.wss.k, crime19=crime19)
      return(dis)
    }
    maxk = 10
    dis = kmeans.dis(crime19, maxk)
    
    # Plot for optimal K- Value
    output$plot3<-renderPlot({plot(1:maxk,dis, type='b', xlab="Number of Clusters",
                                   ylab="Distortion",
                                   col="blue")})  
    
    #analysis for two attributes.
    selectedData <- reactive({
      crime19[, c(input$xcol,input$ycol)]
    })
    
    clusters1 <- reactive({
      kmeanspp(selectedData(), input$clust)
    })
    
    output$plot1 <- renderPlot({
      clusplot(selectedData(), clusters1()$cluster, color=TRUE, shade=TRUE, labels=2,
               main = paste("CLUSPLOT FOR THE GIVEN CRIME TYPES"),
               sub = paste(" "),
               xlab = "X axis", ylab = "Y axis")
    })
    
    output$table3<-renderTable({
      clusters1()$centers
    })
    output$ratio3 <- renderText({(clusters1()$betweenss /clusters1()$totss)*100})
    
    output$plot2 <- renderPlot({
      palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
      
      par(mar = c(5.1, 4.1, 0, 1))
      plot(selectedData(),
           col = clusters1()$cluster,
           pch = 20, cex = 3)
      points(clusters1()$centers, pch = 4, cex = 4, lwd = 4)
    })
    # to obtain the colnames
    k4<-reactive({
      df[,1:(grep(input$zcol,colnames(df)))]
    })
    
    # To perform the kmeanspp 
    k3<-reactive({
      
      kmeanspp(df[,input$zcol],input$clust1)
      
    })
    #plotting using data in the indexing
    plotInput<-function(){
      l1<-sort.list(k3()$centers,decreasing = TRUE)
      for(i in 1:length(l1)){
        ind[i]<-i
      }
      for(i in 1:length(l1))
      {
        cm<-data.frame()
        
        cm<-subset(k4(),k3()$cluster==l1[i])
        
        cm<-data.frame(cm,ind[i])
        
        colnames(cm)[ncol(cm)]<-"index"
        
        cm1<-rbind(cm1,cm)
        print(cm1)
      }
      
      # pie chart construction
      for(i in 1:length(l1))
      {
        count[i]<-nrow(subset(cm1,index==i))
        lbls[i]<-paste(c("level",i),collapse =  " ")
        lbls1[i]<-paste(c("level",i),collapse = " ")
        
      }
      count<-matrix(unlist(count),ncol=1,byrow=TRUE)
      print(count)
      pct <- round(count/sum(count)*100)
      lbls<-matrix(unlist(lbls),ncol=1,byrow=TRUE)
      lbls <- paste(lbls,pct,sep="\n")
      lbls <- paste(lbls,"%",sep="")
      lbls<-unique(lbls)
      print(lbls)
      
      print(count)
      plotname<-paste("Plot for crime type", input$zcol,"with",input$clust1,"levels", sep=" ")
      pie(count, labels = lbls, main = plotname,col = cm.colors(length(count)))
      legend("topright", lbls1, cex = 0.8,
             fill = cm.colors(length(count)))
    }
    
    
    output$plot6<-renderPlot({
      print(plotInput())
      
    })
    
    
    
    output$down <- downloadHandler(
      filename =  function() {
        paste("Crime","pdf", sep=".")
      },
      content = function(file) {
        
        pdf(file) # open the pdf device
        plotInput()
        dev.off()  # turn the device off
        
      })
    
    output$downloadData1 <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(lels(), file)
      })
    
    
    
    
    
    
    
    
    
    
    
    plotBar<-reactive({
      k41<-crime
      k31<-kmeanspp(crime[,input$zcol],input$clust1)
      l11<-list()
      ind1<-list()
      cm11<-data.frame()
      l11<-sort.list(k31$centers)
      for(j in 1:length(l11))
      {
        ind1[j]<-j
      }
      for(a in 1:length(l11))
      {
        cm12<-data.frame()
        cm12<-subset(k41,k31$cluster==ind1[a])
        cm12<-data.frame(cm12,ind1[a])
        colnames(cm12)[ncol(cm12)]<-"index"
        cm11<-rbind(cm11,cm12)
        
      }
      
      cm11<-data.frame(cm11[,c("STATES",input$zcol,"index")])
      print(cm11)
      cm21<-subset(cm11,cm11$index==input$level)
      #cm21<-cm11
      dfc<-data.frame(Region = cm21$STATES , crime_count = cm21[,input$zcol])
      addline_format <- function(x,...){
        gsub('_','\n',x)
      }
      dfc$Region<-reorder(dfc$Region,-dfc$crime_count)
      ggplot(dfc, aes(x=Region,y = crime_count)) +geom_bar(width = 0.3,position = position_dodge(width = 0.3),stat = "identity",show.legend = TRUE,color="blue", fill="steelblue")+
        
        scale_x_discrete(breaks=unique(dfc$Region), 
                         labels=addline_format(dfc$Region))+ggtitle(paste("Region:",sep = ""))+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
      
    })
    
    
    
    
    
    output$bplot<-renderPlot({
      plotBar()
    })
    
    
    
    
    
    
    levls<-reactive({
      print(k3()$centers)
      l1<-sort.list(k3()$centers,decreasing = TRUE)
      for(i in 1:length(l1)){
        ind[i]<-i
      }
      for(i in 1:length(l1))
      {
        cm<-data.frame()
        
        cm<-subset(k4(),k3()$cluster==l1[i])
        
        cm<-data.frame(cm,ind[i])
        
        colnames(cm)[ncol(cm)]<-"index"
        
        cm1<-rbind(cm1,cm)
        print(cm1)
      }
      print(input$lev)
      subset(cm1,index==input$level)
      
      
    })
    lels<-function(){
      print("entered lels")
      # print(levls())
      print(levls()$STATES)
      lis<-data.frame(levls()$STATES)
      #colnames("States",lis$levls...States)
      colnames(lis)<-c("STATES")
      print(lis)
      print("exited lels")
      print(lis)
      returnValue(lis)
    }
    
    output$count<-renderText({
      
      nrow(lels())
      
    })
    
    
    output$table14<-DT::renderDataTable({
      
      DT::datatable(lels())
    })
    
    
    
    
    # To render the map for the states in the table 
    output$mymap <- renderLeaflet({
      
      
      
      # if(input$level==0) return(NULL)
      
      progress <- Progress$new(session, min=1, max=15)
      on.exit(progress$close())
      
      progress$set(message = 'Mapping in progress',
                   detail = 'This may take a while...Please wait..')
      
      for (i in 1:15) {
        progress$set(value = i)
        Sys.sleep(0.5)
      }
      
      # to fetch the lat lon for each state and place it in the df for rendering
      crimemap<-levls()
      for (i in 1:nrow(crimemap)) {
        
        latlon = geocode(as.character(crimemap[i,1]))
        
        crimemap$lon[i] = as.numeric(latlon[1])
        
        crimemap$lat[i] = as.numeric(latlon[2])
        
      }
      
      
      crimemap<-data.frame(crimemap,levls()$index)
      # colnames(crimemap)[ncol(crimemap)]<-"index"
      print(crimemap)
      #  crimemap<-subset(crimemap,index == 4)
      lon<-crimemap$lon
      lat<-crimemap$lat
      states<-as.character(crimemap$STATES)
      
      
      lon<-as.list(lon)
      lat<-as.list(lat)
      states<-as.list(states)
      df1<-cbind(lon,lat,states)
      lon<-as.numeric(lon)
      lat<-as.numeric(lat)
      states<-as.character(states)
      
      
      
      
      
      
      
      
      
      leaflet(df1) %>%
        addTiles() %>%
        setView(lon[1],lat[1],zoom =4)%>%
        addCircles(lng =~lon,lat = ~lat, weight =15,fill = TRUE,fillColor= "0D2D6B",label=~states)
      
      
      
    })
    
    
    return(df)
  })
  output$content<-DT::renderDataTable({
    
    DT::datatable(data())
  })
  
  k1<-reactive({
    
    kmeans(data()[,2:ncol(data())],input$clusters)
    
  })
  
  k2<-reactive({
    
    kmeanspp(data()[,2:ncol(data())],input$clusters)
    
  })
  
  
  output$table1<-renderTable({
    
    k1()$center
    
  })
  output$table2<-renderTable({
    
    k2()$center
    
  })
  
  output$plot4<-renderPlot({
    clusplot(data()[,2:ncol(data())], k1()$cluster, color=TRUE, shade=TRUE, labels=2,
             main = paste("CLUSPLOT FOR THE GIVEN CRIME DATA"),
             sub = paste(" "),
             xlab = "X axis", ylab = "Y axis")
  })
  
  output$plot5<-renderPlot({
    clusplot(data()[,2:ncol(data())], k2()$cluster, color=TRUE, shade=TRUE, labels=2,
             main = paste("CLUSPLOT FOR THE GIVEN CRIME DATA"),
             sub = paste(" "),
             xlab = "X axis", ylab = "Y axis")
  })
  
  output$ratio <- renderText({(k1()$betweenss /k1()$totss)*100})
  output$ratio1 <- renderText({(k2()$betweenss /k2()$totss)*100})
  
  
  
  
}















# Run the application 
shinyApp(ui = ui, server = server)

