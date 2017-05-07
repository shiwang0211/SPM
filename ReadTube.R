library(data.table)

load("./rda/Location.rda")

TubeData<-fread("../Tube Counts/AllCounts_2015and2016.csv")
stationid_string = sapply(TubeData[,StationID], function(x) sprintf("%04d",x))
TubeData[,StationID:=paste(County,stationid_string,sep="")]
setnames(TubeData, "UNKNOWN_1","LANE_INDEX")

time_string = sapply(TubeData[,Time], function(x) sprintf("%04d",x))
date_time_string =  paste(TubeData[,Date], time_string)
date_time = as.POSIXct(date_time_string, format = "%m/%d/%Y %H%M",tz="GMT") - 15*60  #create time, raw data is end of 15-min interval

TubeVolumeTable = TubeData[,.(Class,StationID,Volume, Direction, LANE_INDEX)][,Time:=date_time]

ByLaneTubeVolume <- TubeVolumeTable
save(ByLaneTubeVolume, file="./rda/ByLaneTubeVolume.rda")

ByDirectonTubeVolume = TubeVolumeTable[,.(Volume = sum(Volume), NLanes = .N), by=.(StationID, Time, Direction)]

ByDirectonTubeVolume_2015 = ByDirectonTubeVolume[year(Time) == 2015]
ByDirectonTubeVolume = ByDirectonTubeVolume[year(Time) == 2016]
save(ByDirectonTubeVolume_2015, file="./rda/ByDirectonTubeVolume_2015.rda")
save(ByDirectonTubeVolume, file="./rda/ByDirectonTubeVolume.rda")
