library(data.table)

load("./rda/Location.rda")

minDatetime = as.POSIXct("2015-01-01 00:00:00",format = "%Y-%m-%d %H:%M:%S",tz="GMT") # This is the range for tube count
maxDatetime = as.POSIXct("2015-03-31 23:59:59",format = "%Y-%m-%d %H:%M:%S",tz="GMT")

EventTable = fread("E:/SPM_20170227/Data_Sample_EC82_15MIN_20150101_20150331.csv")

for (i in 1:length(UniqueSPMSignalIDs)){ # save individual signal
  
  Signalid = UniqueSPMSignalIDs[i]
  SPMVolumeTable = EventTable[SignalID == Signalid]
  SPMVolumeTable = merge(x = SPMVolumeTable, y = DetectorTable, by.x = c("SignalID", "EventParam"),by.y= c("Signal_ID", "Det_Channel"), all.x =TRUE)
  setnames(SPMVolumeTable,"RecordCount","Volume")
  date_time = as.POSIXct(SPMVolumeTable$RollupTimestamp, format = "%Y-%m-%d %H:%M:%S",tz="GMT") #create time
  ByDirectonSPMVolume = SPMVolumeTable[,Time:=date_time][Direction != "NA"][, sum(Volume), by = .(SignalID, Direction, Time)][Time >= minDatetime & Time <= maxDatetime][order(Time)]
  setnames(ByDirectonSPMVolume,"V1","Volume")
  save(ByDirectonSPMVolume, file=paste("E:/SPM_20170227/BySignalVolumes_2015/SPM_SignalID_",Signalid,".rda",sep=""));
  cat(i)
}

for (i in 1:length(UniqueSPMSignalIDs)){ # merge individual signals
  Signalid = UniqueSPMSignalIDs[i]
  load(paste("E:/SPM_20170227/BySignalVolumes_2015/SPM_SignalID_",Signalid,".rda",sep=""))
  if (i == 1) MergedByDirectonSPMVolume = ByDirectonSPMVolume
  if (i > 1)  MergedByDirectonSPMVolume = rbind(MergedByDirectonSPMVolume,ByDirectonSPMVolume)
}

ByDirectonSPMVolume_2015 = MergedByDirectonSPMVolume
save(ByDirectonSPMVolume_2015, file="./rda/ByDirectonSPMVolume_2015.rda")
  
