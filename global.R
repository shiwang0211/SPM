library(leaflet) 
library(shiny)
library(ggplot2)
library(plotly)
library(data.table)
library(Metrics)
library(randomForest)
library(RColorBrewer)

load("./rda/Location.rda") # from ReadConfig.R
load("./rda/ByDirectonSPMVolume.rda") # from ReadSPM.R
load("./rda/ByDirectonTubeVolume.rda") # from ReadTube.R
load("./rda/Final_Data_Merge.rda")
load("./rda/AoGTable.rda")
load("./rda/Volume_Agg.rda")
load("./rda/RoadShape.rda")
load("./rda/Bluetooth.rda")

content_TUBE = paste(names(Location_TUBE)[1], ": ", Location_TUBE[,Cosite])
content_SPM = paste(names(Location_SPM)[1], ": ", Location_SPM[,Signal_Id])

#for(i in 1:(ncol(Location_SPM)-1)){
#  content_SPM <- paste(sep = "<br/>",
#                         content_SPM,
#                         paste(colnames(Location_SPM[i+1]), ": ", Location_SPM[[i+1]]))
#}

#content_TUBE <- paste(sep = "<br/>",
#                        content_TUBE, sapply(Location_TUBE[,Cosite], function (x) 
#                          paste(sep="<br/>",as.character(TubeNLanes [StationID  == x, Dir_NLane])))
#)



ValidSignalIDs = unique(ByDirectonSPMVolume[,SignalID])
ValidSignalIDDirections = unique(ByDirectonSPMVolume[,.(SignalID,Direction)])
IfValidSignalID = Location_SPM$Signal_Id %in% ValidSignalIDs
SPMOpacity = sapply(IfValidSignalID, function(x) if (x == TRUE) 1 else 0)

ValidTubeStations_2016 = unique(ByDirectonTubeVolume[year(Time) == 2016,StationID])
ValidTubeStations_2015 = unique(ByDirectonTubeVolume[year(Time) == 2015,StationID])
TubeOpacity = sapply(Location_TUBE$Cosite, function(x) if (x %in% ValidTubeStations_2016) 1 else 0)

IconSize = 40
DirectionIcons <- iconList(
  All = makeIcon("./images/All.png", "All.png", IconSize *0.5 , IconSize*0.5),
  NS = makeIcon("./images/NS.png", "NS.png", IconSize * 0.5, IconSize),
  EW = makeIcon("./images/EW.png", "EW.png", IconSize, IconSize),
  N = makeIcon("./images/N.png", "N.png", IconSize * 0.5, IconSize),
  S = makeIcon("./images/S.png", "S.png", IconSize * 0.5, IconSize),
  E = makeIcon("./images/E.png", "E.png", IconSize, IconSize),
  W = makeIcon("./images/W.png", "W.png", IconSize, IconSize),
  Unknown = makeIcon("./images/Unknown.png", "Unknown.png", IconSize, IconSize)
)

pal_PR <-  colorNumeric(
  "RdYlGn",
  domain = AoGTable_v2[, PR])

palette_rev <- rev(brewer.pal(5, "RdYlGn"))
pal_VC <-  colorNumeric(
  palette_rev,
  domain = Volume_Agg[, VC_Ratio])

SPMColor_20161011_PR = sapply(Location_SPM$Signal_Id, function(x) pal_PR(AoGTable_v2[SignalID == x, min(PR)]))
Opacity_20161011_PR = sapply(Location_SPM$Signal_Id, function(x) if (x %in% AoGTable_v2[,SignalID]) 1 else 0)

SPMColor_20161011_VC = sapply(Location_SPM$Signal_Id, function(x) pal_VC(Volume_Agg[SignalID == x, max(VC_Ratio)]))
Opacity_20161011_VC= sapply(Location_SPM$Signal_Id, function(x) if (x %in% Volume_Agg[,SignalID]) 1 else 0)

Seminole_NLanes_Color = sapply(Seminole_NLanes$LANES, function(x) if(x <=1) "green" else if (x<=3) "blue" 
                                                                  else if (x<=5) "purple" else if (x<=7) "orange" else "red")
Seminole_NLanes_Weight = sapply(Seminole_NLanes$LANES, function(x) if(x <=1) 5 else if (x<=3) 7 
                                                                  else if (x<=5) 9 else if (x<=7) 11 else 13)