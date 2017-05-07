library(data.table)
load("./rda/Location.rda")
load("./rda/ByLaneTubeVolume.rda")

EventTable <- fread("E:/SPM_20170227/Data_Sample_EC82_15MIN_20160101_20160630.csv")
EventTable_BackUp = EventTable
colnames(EventTable) = c("Timestamp", "SignalID","EventParam","EventCode","Volume")

EventTable = merge(x = EventTable, y = DetectorTable, by.x = c("SignalID", "EventParam"),by.y= c("Signal_ID", "Det_Channel"), all.x =TRUE)
EventTable = EventTable[Direction != "NA"][, .(SPMVol = sum(Volume)), by = .(SignalID, Direction, Timestamp, EventParam)][order(Timestamp)]
date_time = as.POSIXct(EventTable$Timestamp, format = "%Y-%m-%d %H:%M:%S",tz="GMT") #create time
EventTable[,Timestamp := date_time]

# Summary of Lane Utilization
Summary = EventTable[, .(MinVol = min(SPMVol), MaxVol = max(SPMVol)), by = .(SignalID,Direction, Timestamp)]
Summary [,Diff := MaxVol - MinVol]
SPMDiffBtwLane = Summary [, .(DiffBtwLane  = sum(abs(Diff))), by = .(SignalID, Direction, Timestamp)][order(-DiffBtwLane)]

DiffLevel_series = sapply(SPMDiffBtwLane [,DiffBtwLane], function(x) if (x <7) "Low" else if (x>=7 & x < 23) "Medium" else "High")
SPMDiffBtwLane[,DiffBtwLaneLevel := factor(DiffLevel_series, c("Low","Medium","High"))]

save(SPMDiffBtwLane, file="./rda/SPMDiffBtwLane.rda")





# added on 0417 just for figures
plot_ly(x=DetectorTable_Raw[DistanceFromStopBar>0 &DistanceFromStopBar<500 ,DistanceFromStopBar], type="histogram") %>%
layout(
  title='Distance of Detector From Stop Bar',
  xaxis=list(
    title='Distance (feet)'
  ),
  yaxis=list(
    title='Frequency'
  )
)

plot_ly(x=SPMTubePair_v2[,LocationCheck], type="histogram",orientation = "v") %>%
  layout(
    title='Location of SPM Detector',
    xaxis=list(
      title='Type'
    ),
    yaxis=list(
      title='Number of Approaches'
    ),
    margin = list(
      l = 50,
      r = 50,
      b = 200,
      t = 50,
      pad=0)
  )


SPMTubePair_v2[LocationCheck == "GOOD", LocationCheck := "ALL MOVEMENTS"]
SPMTubePair_v2[LocationCheck == "NEARLY GOOD (MISSING HALF LEFT TURN LANE)", LocationCheck := "JUST AFTER BEGINNING OF LEFT TURN TAPER"]
