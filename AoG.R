library(data.table) 
# Read Volume Data ( For all signals) ----

Volume_All <- fread("E:/SPM_20170227/Event82_20161011_20161013_0600_0900.csv")  # testing purpose, to be changed
Phase_All <- fread("E:/SPM_20170227/Phase_20161011_20161013_0600_0900.csv")  # testing purpose, to be changed
ValidSignalIDs_v2 = sort(unique(Volume_All[,SignalID]))

Volume_All <- Volume_All[Timestamp  >= '2016-10-11 07:00:00.100' & Timestamp < '2016-10-11 09:00:00.100'] # testing purpose, to be changed
Phase_All <- Phase_All[Timestamp  >= '2016-10-11 07:00:00.100' & Timestamp < '2016-10-11 09:00:00.100'] # testing purpose, to be changed

# Iterate though each Signal to get "AoG Table" ----

AoGTable= data.frame(SignalID = numeric(), Direction = character(), GT = numeric(), AoG = numeric(), t1=numeric(), t2=numeric())

for (i in 1:length(ValidSignalIDs_v2)){ # Iterate through each Signal ID
  
  Signalid = ValidSignalIDs_v2[i]
  
  # Filter Volume Data
  Volume <- Volume_All[SignalID == Signalid & EventCode == 82]
  Volume <- merge(x = Volume, y = DetectorTable, 
                  by.x = c("SignalID", "EventParam"),
                  by.y= c("Signal_ID", "Det_Channel"), all.x =TRUE)
  Volume <- Volume[Direction != "NA"][order(Timestamp)]
  
  if(nrow(Volume)>0){
    
    Volume[,TimeSec := hour(Timestamp) * 3600 + minute(Timestamp) * 60 + second(Timestamp)]
    Volume_2_dir = Volume
    
    # Read and Filter Phase data
    Phase = Phase_All[SignalID == Signalid ]
    Phase[, TimeSec := hour(Timestamp) * 3600 + minute(Timestamp) * 60 + second(Timestamp)]
    Phase_2_dir = Phase
    
    # Extract direction(s) from volumes
    Directions = sort(unique(Volume[,Direction]))
    
    # Calculate Metrics for each direction
    
    for (j in 1:length(Directions)){
      
      # GetDirection
      SPMDirection = Directions[j]
      
      # Select Volume for direction
      Volume <- Volume_2_dir[Direction == SPMDirection]
      
      # Get Phase Number
      PhaseParam = Volume[1,Phase]
      
      # Select Timing for phase
      Phase <- Phase_2_dir[EventParam == PhaseParam][order(Timestamp)] 
      
      # Calculate Delay
      NoOfLanes = NumberOfLanes[Signal_ID == Signalid & Direction == SPMDirection, N]
      Delay(Volume[,.(EventCode,TimeSec)],Phase[,.(EventCode,TimeSec)],NoLanes)
      
      # Truncate beginning and ending to have complete cycles
      SofG = Phase[EventCode == 1, TimeSec]
      SofR = Phase[EventCode == 10, TimeSec]
      
      S_Time =  min(SofG) 
      E_Time =  max(SofG)
      
      Phase = Phase[TimeSec >= S_Time & TimeSec <= E_Time ]
      Volume = Volume[TimeSec >= S_Time & TimeSec <= E_Time ]
      
      SofR = Phase[EventCode == 10,TimeSec] # truncated
      SofG = Phase[EventCode == 1,TimeSec] # truncated
      
      
      # Calculate GT, RT, etc
      GT = SofR - SofG[-length(SofG)]
      RT = SofG[-1] - SofR
      CY = GT+ RT
      Per_GT = sum(GT) / sum(GT + RT)
      Per_GT
      
      # Defind a function to determin AoG or AoR
      FindArrGreenRed <- function(Arr_time){
        GorR = Phase[(TimeSec - Arr_time)>=0][1,EventCode]
        GorR = if (GorR == 1) "AoR" else "AoG"
      }
      
      # Calculate AoG or AoR for each Volume record
      GorR_ = sapply(as.vector(Volume[,TimeSec]), FindArrGreenRed)
      Volume[,GorR := GorR_]
      
      AoG = Volume[GorR == "AoG",.N] / nrow(Volume)
      AoR = 1 - AoG
      AoG
      
      t1=length(SofR)
      t2 = length(SofG[-length(SofG)])
      
      if(t1 == t2) NoOfCycles = t1
      
      else{ # fix error
        NoOfCycle = 0
        GT = 0
        tmp1 = 0
        for(i in 1:(length(SofG)-1)){
          tmp1 = max(SofG[i], tmp1)
          tmp2 = min(SofR[SofR>=tmp1])
          GT = GT + (tmp2 - SofG[i])
          tmp1 = tmp2
          NoOfCycle = NoOfCycle + 1
        }
        RT = SofG[length(SofG)] - SofG[1] - GT
        Per_GT = sum(GT) / sum(GT + RT)
      }
      
      
      newline = data.frame(SignalID = Signalid,Direction =  SPMDirection, 
                           GT = Per_GT, AoG = AoG,t1=t1, t2=t2, 
                           NoOfCycles = NoOfCycles)
      AoGTable = rbind(AoGTable, newline)
      
      print(newline)
      cat("\n",length(SofR),length(SofG[-length(SofG)]),"\n") ### to check, some errors for 5 stations
      
    } # if nrow(Volume)>0
    
  } # For loop for two directions
  
} # For loop for signal id

write.csv(AoGTable, file="../Results/AoGTable_20161011_20161011_0700_0900.csv", row.names = FALSE)
AoGTable = read.csv(file="../Results/AoGTable_20161011_20161011_0700_0900.csv")

# Cluster Analysis ----

setDT(AoGTable)
AoGTable_v2 = AoGTable[ abs(t1 - t2) <= 1 ]
AoGTable_v2 = AoGTable_v2[complete.cases(AoGTable_v2)]   # Next Setp is to quick see why there are NAs 
AoGCluster <- kmeans(AoGTable_v2[,.(GT,AoG)], 3, nstart = 20)
AoGTable_v2[,Cluster:= as.character(AoGCluster$cluster)]
AoGTable_v2[Cluster == 1,Cluster:= "High GT, High AoG"]
AoGTable_v2[Cluster == 2,Cluster:= "2TBD"]
AoGTable_v2[Cluster == 3,Cluster:= "3TBD"]

AoGTable_v2[,Index := c(1:nrow(AoGTable_v2))] 
write.csv(AoGTable_v2, file="../Results/AoGTable_20161011_20161011_0700_0900_step2.csv", row.names = FALSE)
save(AoGTable_v2, file="./rda/AoGTable.rda");

# Get some simple visualization ----
Volume_Agg <- merge(x = Volume_All, y = DetectorTable, 
                    by.x = c("SignalID", "EventParam"),
                    by.y= c("Signal_ID", "Det_Channel"), all.x =TRUE)
Volume_Agg <- Volume_Agg[Direction != "NA"][order(Timestamp)]
Volume_Agg <- Volume_Agg[,.(Vol = .N), by=.(SignalID,Direction)]
Volume_Agg = merge(x = Volume_Agg, y = NumberOfLanes, by.x =  c("SignalID", "Direction"), by.y =  c("Signal_ID", "Direction")) # From SPM 
Volume_Agg[,VC_Ratio := Vol / 2 / (N * 1800)]

save(Volume_Agg, file="./rda/Volume_Agg.rda") # Save back
