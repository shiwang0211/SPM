library(data.table)

Location_TUBE<-fread("../Tube Location/2011 FDOT ptms.csv")
Location_TUBE=na.omit(Location_TUBE) #18 locations NA
Location_TUBE=subset(Location_TUBE, Lon>=-81.54 & Lon<=-81.07 & Lat>=28.6 & Lat<=28.9) # Seminole County

DetectorTable_Raw = fread("../SPM Location/Graph_detector.csv")
DetectorTable = DetectorTable_Raw[,.(Signal_ID, Det_Channel,Direction, Phase)]
NumberOfLanes = DetectorTable[,.N, by = .(Signal_ID, Direction)]
DistanceFromStop =  DetectorTable_Raw[,.(Distance = max(DistanceFromStopBar)), by = .(Signal_ID, Direction)]

Location_SPM <- fread("../SPM Location/Signals.csv") #ID 2225 is wrong, manually fixed
Location_SPM = merge(Location_SPM, DistanceFromStop [, .(Avg_Dis = mean(Distance)), by = .(Signal_ID)], 
                     by.x=c("Signal_Id"), by.y = c("Signal_ID"), all.x=TRUE)
Location_SPM[is.na(Location_SPM$Avg_Dis) | Avg_Dis == 0, Avg_Dis := 5]

SPMTubePair = fread("../SPM Location/PairWithTube.csv") 
SPMTubePair = subset(SPMTubePair, Check == "GOOD")
SPMTubePair_v2 = subset(SPMTubePair, Check == "GOOD" & InOut == "GOOD")
SPMTubePair_v2[,PairID:= c(1:nrow(SPMTubePair_v2))]

UniqueSPMSignalIDs = unique(Location_SPM[["Signal_Id"]])
UniqueSPMDirections = c("Northbound", "Southbound", "Eastbound", "Westbound")
UniqueTubeStationIDs = unique(Location_TUBE[["Cosite"]])
UniqueTubeDirections = c("N", "S", "E", "W")


Direction<-function(x){
  
  N = if("Northbound" %in% x ) 1 else 0 
  S = if("Southbound" %in% x ) 1 else 0 
  E = if("Eastbound" %in% x) 1 else 0 
  W = if("Westbound" %in% x ) 1 else 0 
  
  if (N+S+E+W ==4) "All" else
    if (N+S == 2) "NS" else
      if (E+W == 2) "EW" else
        if(N == 1 & S == 0) "N" else
          if(N == 0 & S == 1) "S" else
            if(W == 1 & E == 0) "W" else
              if(W == 0 & E == 1) "E" else
                "Unknown"
  
}

DetectorTable[Det_Channel == 0, Direction := "Unknown"]
SignalDirection = DetectorTable[, lapply(.SD, Direction) , by=Signal_ID,.SDcols = c("Direction")]
Location_SPM = merge(Location_SPM, SignalDirection, by.x = c("Signal_Id"), by.y = c("Signal_ID"), all.x = TRUE)
Location_SPM[is.na(Location_SPM$Direction), Direction := "Unknown"]

TubeNLanes = unique(ByDirectonTubeVolume[,.(StationID, Direction, NLanes)])[order(StationID)]
TubeNLanes [, Dir_NLane := paste("No of Lanes for " , Direction," Direction: " , NLanes, sep = "")]
TubeNLanes [, StationID := as.numeric(StationID)]


save(Location_TUBE, Location_SPM, DetectorTable, SignalDirection ,
     NumberOfLanes, DistanceFromStop ,SPMTubePair,SPMTubePair_v2,
     UniqueSPMSignalIDs, UniqueSPMDirections, 
     UniqueTubeStationIDs, UniqueTubeDirections,
     file="./rda/Location.rda")









