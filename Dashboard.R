library(leaflet)
library(shiny)
library(DT)
#library(ggplot2)
library(plotly)
source('global.R')
library(htmltools)
library(shinythemes)
library(shinydashboard)
library(rpivotTable)
library(geosphere)

# UI -------------------------------------------------- 

ui <- dashboardPage(skin="black",
  dashboardHeader(title = "SPM data in Seminole County"),
  dashboardSidebar(width=300,
    sidebarMenu(id = "sidebarMenu",
      menuItem("Location of Detectors", tabName="Map", icon=icon("dashboard")),
      #menuItem("Tube Count Data", tabName="TubeData", icon=icon("dashboard")),
      menuItem("Comparison Plot", tabName="Comparison", icon=icon("dashboard")),
      menuItem("Summary Table", tabName="Summary", icon=icon("dashboard")),
      menuItem("AoG", tabName="AoG", icon=icon("dashboard"))
      )
  ),
  dashboardBody(
    conditionalPanel(
      "input.sidebarMenu!='AoG' & input.sidebarMenu!='Summary'",
    fluidRow(
      column(4,selectInput("PairNo", label = "Select a Pair of SPM and Tube", choices = c(1:nrow(SPMTubePair_v2)))),
      #column(2,verbatimTextOutput("Pair_Accuracy")),
      column(6,helpText("Select Pair No. on the left OR",br(),"Select from drop-down List OR",br(), "Click from Map"))
    ),
      
    fluidRow(
      box(title="Select SPM",status="primary",collapsible = TRUE,solidHeader=TRUE,
          selectInput("SPMSignalID", label = "Selected SPM Signal ID", choices = UniqueSPMSignalIDs),
          selectInput("SPMDirection", label = "Selected SPM Approach Direction", choices = UniqueSPMDirections)
      ),
      
      box(title="Select Tube",status="primary",collapsible = TRUE,solidHeader=TRUE,
          selectInput("TubeStationID", label = "Selected Tube Station ID", choices = UniqueTubeStationIDs),
          selectInput("TubeDirection", label = "Selected Tube Approach Direction", choices = UniqueTubeDirections)
      )
    )
    ),
    tabItems(
      tabItem(tabName = "Map", 
              helpText("Data Source: Seminole County SPM Achival Database and D5 Tube Counts in the year of 2016"),
              helpText("Red Circles are SPM stations; Blue Circles are Tube Count Stations; Black Circles are stations with no available counts"),
              leafletOutput("map",width="100%",height="650px")
      ),
      tabItem(tabName = "TubeData", 
              dataTableOutput("TubeDataTable")),
      
      tabItem(tabName = "Comparison", 
              plotlyOutput("ByTimeComparison"),br(), br(),br(),
              fluidRow(column(6,plotlyOutput("ByVolumeComparison")), column(6,plotlyOutput("DifferenceByVolume")))
      ),
      tabItem(tabName = "Summary", 
            h3("Instruction"),
            helpText("Please drag variables into the Row/Column head of pivot table and Select analysis type"),
            rpivotTableOutput("pivot")
      ),
      tabItem(tabName = "AoG",
              helpText("The aim is to group signals based on their green time ratio and arrival on green rate",
                       "and to help operators focus on those signals demonstrating poor coordination?? TBD",br(),
                       "Data currently only includes 2016/10/11 07:00-9:00 AM for testing purpose"),
              fluidRow(column(6,plotlyOutput("AoG_AoR"))
                       #column(6,verbatimTextOutput("Text_AoG"))
                       )
      )
    )
  )
)


# Server -------------------------------------------------- 

server <- function(input, output, session) {
  
  # Draw All Basic Elements that will not change with inputs
  
  output$map<- renderLeaflet({ 
    
    p<-leaflet() %>% 
      addProviderTiles("Esri.WorldImagery", group = "WorldImagery") %>%
      addTiles(group = "OSM (default)") %>%
      #addPolygons(data = Seminole_County, color = "grey") %>%
      addPolylines(data = Seminole_NLanes, color = Seminole_NLanes_Color, opacity = 0.75, weight = Seminole_NLanes_Weight, popup = ~ROADWAY,group = "NoLanes") %>%
      addCircleMarkers(Location_TUBE$Lon, Location_TUBE$Lat, radius=3,color="blue",opacity = TubeOpacity,layerId= Location_TUBE$Cosite, popup=content_TUBE,group = "Tube (Blue)") %>%
      addCircleMarkers(Location_SPM$Longitude, Location_SPM$Latitude, radius=3,color="red",opacity = SPMOpacity, layerId= Location_SPM$Signal_Id,popup=content_SPM, group = "SPM (Red)") %>%
      
      #addCircles(Location_SPM$Longitude, Location_SPM$Latitude, radius=Location_SPM$Avg_Dis * 0.3048,color=SPMColor,layerId= Location_SPM$Signal_Id,popup=content_SPM, group = "SPM (Red)") %>%
      #addPopups(Location_SPM$Longitude, Location_SPM$Latitude,as.character(Location_SPM$Signal_Id)) %>%
      #addPopups(Location_TUBE$Lon, Location_TUBE$Lat,as.character(Location_TUBE$Cosite)) %>%
      
      addCircleMarkers(Location_SPM$Longitude, Location_SPM$Latitude, radius=10,color=SPMColor_20161011_PR,opacity=Opacity_20161011_PR,fillOpacity = Opacity_20161011_PR,popup=content_SPM,group = "SPM (PR)") %>%
      addCircleMarkers(Location_SPM$Longitude, Location_SPM$Latitude, radius=10,color=SPMColor_20161011_VC,opacity=Opacity_20161011_VC,fillOpacity = Opacity_20161011_VC,popup=content_SPM,group = "SPM (VC)") %>%
      addMarkers(Location_SPM$Longitude, Location_SPM$Latitude, icon = DirectionIcons[Location_SPM$Direction],popup=content_SPM, group = "SPM Directions") %>%
      addLayersControl(baseGroups = c("OSM (default)", "WorldImagery"),
                       overlayGroups = c("Tube (Blue)","SPM (Red)", "SPM (PR)", "SPM (VC)", "SPM Directions", "NoLanes", "Bluetooth"), 
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup( c("SPM (PR)", "SPM (VC)", "SPM Directions", "NoLanes", "Bluetooth") ) %>%
      addMeasure(primaryLengthUnit = "feet")
      #addPolylines(data = Seminole_roads, color = "green", popup = ~ROADWAY) %>% 
      #addMeasure(position = "bottomleft",primaryLengthUnit = "feet",primaryAreaUnit = "sqmeters",activeColor = "#3D535D",completedColor = "#7D4479")
    
    
    for(index in 1:nrow(PairTable))
    {pair = gcIntermediate(PairTable[index,.(s_lon,s_lat)], 
                           PairTable[index,.(e_lon,e_lat)], 
                           n = 25, sp = TRUE, addStartEnd = TRUE)
    p<-p %>% addPolylines(data=pair, color = "green", weight=5, group="Bluetooth",layerId=PairTable[index, PairID],popup=as.character(PairTable[index,PairID]))}
    p<-p %>% addCircleMarkers(DeviceTable$lon,DeviceTable$lat, radius=3,color="yellow",opacity =1, popup=DeviceTable$DeviceID, group = "Bluetooth")
    p
    
  })  
  
  # Toggle legend Layers
  observeEvent(input$map_groups,{
    map <- leafletProxy("map") %>% clearControls()
    if ('SPM (PR)' %in% input$map_groups) {
      map %>% addLegend("bottomright", pal = pal_PR, values = AoGTable_v2[, PR], title = "Platoon Ratio",opacity = 1)
    }
    else if ('SPM (VC)' %in% input$map_groups) {
      map %>% addLegend("bottomright", pal = pal_VC, values = Volume_Agg[,VC_Ratio], title = "V/C Ratio",opacity = 1) 
    }
    else if ('NoLanes' %in% input$map_groups) {
      map %>% addLegend("bottomright", colors = c("green","blue","purple","orange","red"), labels = c("1", "2-3", "4-5", "6-7", ">=7"), title = "Number of Lanes", opacity = 1) 
    }
    
    else if ('Bluetooth' %in% input$map_groups) {
  
    }
    
  })
  
  # Bluetooth line highlight
  
  observeEvent(input$map_shape_click,{

    selected = input$map_shape_click$id
    if(is.null(selected))
      return()
    
    p <- leafletProxy("map")
    index = which(PairTable[,PairID] == selected)
    pair = gcIntermediate(PairTable[index,.(s_lon,s_lat)], 
                          PairTable[index,.(e_lon,e_lat)], 
                          n = 25, sp = TRUE, addStartEnd = TRUE)
    p%>% addPolylines(data=pair, color = "red", weight=5, group="Bluetooth",
                          layerId="current",popup=as.character(PairTable[index,PairID]))
    p
    
  })
  
  
  # If click circles on map, update select boxes
  
  observe({ 
    req(input$map_marker_click$id)
    
    
    
    if (input$map_marker_click$id %in% UniqueSPMSignalIDs == TRUE){
      updateSelectInput(session, "SPMSignalID","Selected SPM Signal ID",
                      UniqueSPMSignalIDs,selected = input$map_marker_click$id)
    }
    if (input$map_marker_click$id %in% UniqueTubeStationIDs == TRUE){
      updateSelectInput(session, "TubeStationID","Selected Tube Count ID",
                        UniqueTubeStationIDs,selected = input$map_marker_click$id
      )
    }
  })

  # If select any pair no, update 2 select boxes and zoom in
  
  observe({
      
    input$PairNo
    
      SignalID_Pair = SPMTubePair_v2$SPMSignalID[strtoi(input$PairNo)]
      SPMDirection_Pair = SPMTubePair_v2$SPMDirection[strtoi(input$PairNo)]
      TubeStationID_Pair = SPMTubePair_v2$TubeStationID[strtoi(input$PairNo)]
      TubeDirection_Pair = SPMTubePair_v2$TubeDirection[strtoi(input$PairNo)]
      
      updateSelectInput(session, "SPMSignalID","Selected SPM Signal ID",
                        UniqueSPMSignalIDs,selected = SignalID_Pair)
      updateSelectInput(session, "SPMDirection","Selected SPM Approach Direction",
                        UniqueSPMDirections,selected = SPMDirection_Pair)
      updateSelectInput(session, "TubeStationID","Selected Tube Count ID",
                        UniqueTubeStationIDs,selected = TubeStationID_Pair)
      updateSelectInput(session, "TubeDirection","Selected Tube Approach Direction",
                        UniqueTubeDirections,selected = TubeDirection_Pair)
      
      Lon1 = Location_TUBE[Cosite == TubeStationID_Pair, Lon]
      Lon2 = Location_SPM[Signal_Id == SignalID_Pair, Longitude]
      Lat1 = Location_TUBE[Cosite == TubeStationID_Pair, Lat]
      Lat2 = Location_SPM[Signal_Id == SignalID_Pair, Latitude]        
      
      leafletProxy('map') %>%
        setView(lng = (Lon1 + Lon2)/2, lat = (Lat1 + Lat2)/2, zoom = 15)
    })
  
  
  # If select boxes are changed, draw yellow circles
  
  observe({ 
    
    input$TubeStationID
    input$SPMSignalID
    
    site_TUBE <- subset(Location_TUBE, Cosite == input$TubeStationID)
    site_SPM <- subset(Location_SPM, Signal_Id == input$SPMSignalID)
    
    isolate({
      new_zoom <- 10
      if(!is.null(input$map_zoom)) new_zoom <- input$map_zoom 
      leafletProxy('map') %>%
        addCircles(lng = site_TUBE$Lon[1], lat = site_TUBE$Lat[1], radius = 50,layerId = "previouss",
                   stroke = TRUE, color = "yellow", weight = 5, opacity = 1, fill = FALSE,dashArray = '5,5') %>%
        addCircles(lng = site_SPM$Longitude[1], lat = site_SPM$Latitude[1], radius = 50,layerId = "previous",
                   stroke = TRUE, color = "yellow", weight = 5, opacity = 1, fill = FALSE,dashArray = '5,5')
    })
  })
  
  
  # Data_1, query Tube Volume based on select box
  
  Data_1<-reactive({
     ByDirectonTubeVolume [StationID == input$TubeStationID & Direction == input$TubeDirection,.(Time,Volume)][order(Time)]
  })
  
  # Data_2, query SPM Volume based on select box
  
  Data_2<-reactive({
    StartTime = min(Data_1()[,Time])
    EndTime = max(Data_1()[,Time])   
    ByDirectonSPMVolume [SignalID == input$SPMSignalID & Direction == input$SPMDirection,.(Time,Volume)][Time>=StartTime & Time<=EndTime][order(Time)]
  })
  
  # Data_Pred, query predicted SPM Volume
  
  Data_pred<-reactive({
    StartTime = min(Data_1()[,Time])
    EndTime = max(Data_1()[,Time]) 
    ByDirectonSPMVolume [SignalID == input$SPMSignalID & Direction == input$SPMDirection,.(Time,Prediction_ls,Prediction_rf, Prediction_anova)][Time>=StartTime & Time<=EndTime][order(Time)]
  })
  
  
  Comparison<-reactive({
   merge(x = Data_1(), y = Data_2(), by.x = c("Time"),by.y= c("Time"), all.x =TRUE)
  })
  
  
  # Comparison Plot 1
  
  output$ByTimeComparison<- renderPlotly({
    
    p <- plot_ly(x=Data_1()[,Time], y = Data_1()[,Volume], colors='blue', type = "scatter", mode = "lines+markers", name="Tube")%>%
      add_trace(x=Data_2()[,Time],y = Data_2()[,Volume], colors='red', type = "scatter", mode = "lines+markers", name="SPM")%>%
      #add_trace(x=Data_pred()[,Time],y = Data_pred()[,Prediction_ls], colors='purple', type = "scatter", mode = "lines+markers", name="SPM_Prediction_ls")%>%
      #add_trace(x=Data_pred()[,Time],y = Data_pred()[,Prediction_rf], colors='yellow', type = "scatter", mode = "lines+markers", name="SPM_Prediction_rf")%>%
      #add_trace(x=Data_pred()[,Time],y = Data_pred()[,Prediction_anova], colors='green', type = "scatter", mode = "lines+markers", name="SPM_Prediction_anova")%>%
      #add_trace(x=Data_2()[,Time],y = Data_2()[,Volume] * 1.25,type = 'scatter', mode = 'lines',fill = 'tonexty', fillcolor='rgba(255,165,165,0.2)', line = list(color = 'transparent'), name = 'SPM +25%')%>%
      layout(
        title='Comparison between 15min SPM Volume and Tube Count',
        xaxis=list(
          title='Time (15 min',
          gridcolor = "grey",
          autotick = F, dtick = 3600.0 * 1000
        ),
        yaxis=list(
          title='Volume (vehs)'
        )
      )%>%
      layout(autosize = T, width = 1600, height = 400, margin = list(
        l = 50,
        r = 50,
        b = 80,
        t = 50,
        pad=0)
        )
    p
    
  })
  
  
  # Comparison Plot 2
  
  output$ByVolumeComparison<- renderPlotly({
    
    p <- plot_ly(Comparison(), x=~Volume.x, y = ~Volume.y, type = "scatter",  mode = "markers", name="SPM-Tube")%>%
      add_trace(x = c(0,1000), y = c(0,1000), color = I('grey'), mode = "lines", dash = 'dash',name = "45degree")%>%
      layout(
        title='',
        xaxis=list(
          title='Tube 15-min Volume',
          rangemode = "tozero"
        ),
        yaxis=list(
          title='SPM 15-min Volume',
          rangemode = "tozero"
        )
      ) %>%
    layout(autosize = F, width = 500, height = 400)
    p
  })
  
  # Comparison Plot 3
  
  output$DifferenceByVolume<- renderPlotly({
    
    p <- plot_ly(Comparison(), x=~Volume.x, y = ~(Volume.y - Volume.x), type = "scatter", mode = "markers", name="Tube")%>%
      #add_trace(x = c(0,1000), y = c(0,1000), color = I('grey'), mode = "lines", dash = 'dash',name = "45degree")%>%
      layout(
        title='',
        xaxis=list(
          title='[SPM 15-min Volume]',
          rangemode = "tozero"
        ),
        yaxis=list(
          title='[SPM 15-min Volume - Tube 15-min Volume]',
          rangemode = "tozero"
        )
      )%>%
      layout(autosize = F, width = 500, height = 400)
    p
  })
  
  
  output$AoG_AoR<- renderPlotly({
  

  plot_ly(AoGTable_v2, x = ~(GT*100), y = ~(AoG*100), key = ~Index, type = "scatter",  mode = 'markers', 
          text = ~ paste("SignalID: " , as.character(SignalID) ,
                         "</br>Direction: " ,  as.character(Direction), 
                         "</br>Number of Cycles:" , as.character(NoOfCycles)), 
          color = ~as.factor(Cluster))%>%
    layout(
      title='',
      xaxis=list(
        title='Green Time %',
        zeroline = TRUE,
        range=c(0,100)
      ),
      yaxis=list(
        title='Arrival on Green %',
        range=c(0,100)
      ),
      autosize = F, width = 800, height = 500,
      dragmode = "select"
    )
  })
  
  output$Text_AoG<-renderText({
    d <- event_data("plotly_hover")
    if(is.null(d)) "Hover on points"
    m <-AoGTable_v2[Index == d[["key"]], SignalID]  
    paste("Signal_ID: ",m,
          "Other information...",sep="\n")
    
  })
  
  output$Pair_Accuracy<-renderText({
    SPMTubePair_v2[strtoi(input$PairNo), Accuracy]
  })
  
  
  output$pivot<-renderRpivotTable({
    rpivotTable(Final_Data_Merge,rendererName="Table")
  })

 # Below is only for making notes for pairs, to be deleted
  if (FALSE){
  v = reactiveValues(PairTable =  PairTable)
  
  observeEvent(input$AddPair,{
    newline = c(input$TubeStationID, input$TubeDirection, input$SPMSignalID,input$SPMDirection)
    v$PairTable = rbind(v$PairTable, newline)
  })
  
  output$Pair_Table<- DT::renderDataTable({
    v$PairTable
  },
  extensions = 'Buttons',caption="Table for pairs of SPM and tuBE",
  options=list(extensions='Buttons', searching=FALSE, scrollX=400, dom='Bfrtip',buttons=c('csv','excel','print'))
  )
  
  }
  
  
}

shinyApp(ui, server)
