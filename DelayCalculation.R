library(data.table)
load("./rda/Location.rda")
load("./rda/Bluetooth.rda") 
Last <- function(x) { tail(x, n = 1) }
All<- fread("C:/Users/KRKITWS.D5-ITS/Desktop/20161011.csv") # already removed millisecond
setnames(All,"Timestamp_","Timestamp")
Volume_All = All[EventCode == 82]
Phase_All = All[EventCode == 1 | EventCode == 10]

## Prepare data

#Volume_All <- fread("D:/SPM_20170227/Event82_20161011_20161013_0600_0900.csv")  # testing purpose, to be changed
#Phase_All <- fread("D:/SPM_20170227/Phase_20161011_20161013_0600_0900.csv")  # testing purpose, to be changed 
#MinTime = "2016-10-11 06:00:00.000"
#MaxTime = "2016-10-11 09:00:00.000"
#Volume_All = Volume_All [Timestamp >= MinTime & Timestamp <= MaxTime]
#Phase_All = Phase_All [Timestamp >= MinTime & Timestamp <= MaxTime]

## Read Pair Table
BT_Pair_Table <- fread("../Bluetooth/Configuration/BT_SPM_Table.csv")
BT_Pair_Table = setDT(merge(BT_Pair_Table, PairTable, by = c("PairID")))
Unique_Pairid = unique(BT_Pair_Table[,PairID])

## Read Bluetooth TT Function

Read_BT_TT<-function(Pairid){
  testdata<-fread(paste("../Bluetooth/Data/bluetoad-export ",Pairid," from 2016-10-11 06_00_00 - 2016-10-11 09_00_59.csv",sep=""),skip = 7)
  colnames(testdata) = c("DOW","Date","Time","DK","TT","Speed")
  testdata = testdata[TT != "Not enough matches"]
  DISTANCE = BT_Pair_Table[PairID == Pairid, Distance][1]
  FFS = BT_Pair_Table[PairID == Pairid, FFS][1]
  testdata[,FFTT := DISTANCE / FFS * 3600]
  testdata[,Delay := as.numeric(TT) - FFTT]
  testdata[Delay <= 0,Delay := 0]
  testdata[,Time := as.POSIXct(paste(Date,Time,sep = " "), format = "%m/%d/%Y %H:%M")]
  testdata
}

### Start

for(Pairid in Unique_Pairid){
  
    SPMDirection = BT_Pair_Table [PairID == Pairid,SPMDirection][1]
    if(exists("Summary_final")) remove(Summary_final)
    
    for(Signalid in BT_Pair_Table [PairID == Pairid,SignalID]){
        
        NoOfLanes = NumberOfLanes[Signal_ID == Signalid & Direction == SPMDirection, N]
        Volume <- Volume_All[SignalID == Signalid & EventCode == 82]
        Volume <- merge(x = Volume, y = DetectorTable, 
                        by.x = c("SignalID", "EventParam"),
                        by.y= c("Signal_ID", "Det_Channel"), all.x =TRUE)
        Volume <- Volume[Direction == SPMDirection][order(Timestamp)]
        Volume[,TimeSec := hour(Timestamp) * 3600 + minute(Timestamp) * 60 + second(Timestamp)]
        PhaseParam = Volume[1,Phase]
        Volume = Volume[,.(EventCode,TimeSec,Timestamp)]
        
        Phase = Phase_All[SignalID == Signalid & EventParam == PhaseParam][order(Timestamp)] 
        Phase[, TimeSec := hour(Timestamp) * 3600 + minute(Timestamp) * 60 + second(Timestamp)]
        Phase = Phase[,.(EventCode,TimeSec,Timestamp)]
        
        
        # Order Volume and Phase, Trim
        Combine = rbind(Volume, Phase)
        Combine = Combine[order(Combine$TimeSec)]
        first_ = which(Combine[,EventCode] == 10)[1]
        last_ = Last(which(Combine[,EventCode] == 10))
        Combine = Combine[first_:last_]
        
        #Constants 
        s = 1900 * NoOfLanes / 3600 
        
        # Extract arrays
        L = nrow(Combine)
        
        start_green_min = min(which(Combine[,EventCode] == 1))
        start_green_max = max(which(Combine[,EventCode] == 1))
        
        SofG = Combine[EventCode==1, TimeSec]
        EofG = Combine[EventCode==10, TimeSec]
        
        All_Time = Combine[,TimeSec]
        All_Event = Combine[,EventCode]
        
        # Determine if arrive on Green
        Green_ = array(0, dim = L)
        
        for (index in start_green_min:start_green_max){
          x = All_Time[index]
          Green_[index] = (min( SofG [which(SofG<= x)] ) >= min(EofG[which(EofG <= x)])) & (min( SofG [which(SofG>= x)] ) >= min(EofG[which(EofG >= x)]))
        }
        
        Green_[1: start_green_min] = 0
        Green_[start_green_max: L] = 1
        
        # Determine Cycle Index
        Cycle_ = array(0, dim = L)
        for (index in 2:L){
          x = All_Time[index]
          Cycle_[index] =  length(EofG[which(EofG < x)]) 
        }
        
        # Calculate Interval
        Tk_ = array(0, dim = L)
        Tk_ = c(0, All_Time[-1] - All_Time[-L])
        
        # Calculate Ck
        Ck_ = Green_ * Tk_ * s
        
        # Calculate Queue
        
        Td_ =  array(0, dim = L)
        Qk_ =  array(0, dim = L)
        
        for (index in 2:L){
          
          if(Green_[index] == 0){
            if(All_Event[index]==82) Qk_[index] = Qk_[index-1] +1
            if(All_Event[index]==1) Qk_[index] = Qk_[index-1]
          }
          
          if(Green_[index] == 1){
            if(All_Event[index]==82) Qk_[index] = max(0, Qk_[index-1] - Ck_[index] +1)
            if(All_Event[index]==10) Qk_[index] = max(0, Qk_[index-1] - Ck_[index])
            if(Ck_[index-1] >= Qk_[index-1])  Qk_[index]=0
          }  
          
          Td_[index] = Qk_[index-1] / s
        }
        
        
        # Calculate Delay
        
        Dk_ = array(0, dim = L)
        
        for (index in 2:L){
          if(Green_[index] == 0) Dk_[index] = Qk_[index-1] * Tk_[index]
          if(Green_[index] == 1) {
            if(Ck_[index] <  Qk_[index-1]) Dk_[index] = (Qk_[index-1] - 0.5 * Ck_[index]) * Tk_[index]
            if(Ck_[index] >= Qk_[index-1]) Dk_[index] = (Qk_[index-1] * 0.5) * Td_[index]
          }    
        }       
        
        
        # Attach back to dataframe    
        
        Combine[,Cycle:=Cycle_][,Green:=Green_][,Tk:=Tk_][,Td:=Td_][,Ck:=Ck_][,Qk:=Qk_][,Dk:= Dk_]
        Delay_ = Combine[Cycle>0,.(Delay=sum(Dk), Cyclength = sum(Tk), Time = max(Timestamp)), by=.(Cycle)]
        Volume_ = Combine[EventCode == 82,.(Volume = .N), by=.(Cycle)]
        GreenTime_ = Combine[Green == 1,.(GreenTime = sum(Tk)), by=.(Cycle)]
        AoG_ = Combine[EventCode == 82 & Green == 1,.(ArrsOnGreen = .N), by=.(Cycle)]
        
        Summary<-merge(Delay_, Volume_, by="Cycle", all.x = TRUE)
        Summary_v2<-merge(Summary, GreenTime_, by="Cycle", all.x = TRUE)
        Summary_v3<-merge(Summary_v2, AoG_, by="Cycle", all.x = TRUE)
        
        Summary_v3[,GT := GreenTime / Cyclength][,AoG:= ArrsOnGreen / Volume][,AvgDelay := Delay / Volume]
        Summary_v3[,VC := Volume / (s * Cyclength * GT)]
        Summary_v3[,SignalID := Signalid]
        
        Temp_Minute = floor(minute(Summary_v3[,Time])/5)*5 - minute(Summary_v3[,Time]) # round to the closest 5 minute !!
        Temp_Second = second(Summary_v3[,Time])
        Summary_v3[,Time_5Min := as.POSIXct("2016-10-20 06:02:33")]  #arbitrary value for formatting
        for(row in 1:nrow(Summary_v3)) Summary_v3[row,Time_5Min := as.POSIXlt(Summary_v3[row,Time]) + Temp_Minute[row]*60 - Temp_Second[row]]

        if(exists("Summary_final")) Summary_final = rbind(Summary_final,Summary_v3)
        if(!exists("Summary_final")) Summary_final = Summary_v3
        
    }  
        
    Summary_final[is.na(AvgDelay),AvgDelay :=0][is.na(Volume),Volume :=0][is.na(AoG),AoG :=0][is.na(VC),VC :=0]# na means no arrs
    Report = Summary_final[, .(Delay = mean(AvgDelay), m_V = mean(Volume / Cyclength), m_AoG = mean(AoG), m_VC = mean(VC)) ,by=.(SignalID,Time_5Min)][,.(TotalDelay = sum(Delay), M_Volume = mean(m_V), M_AoG = mean(m_AoG), M_VC = mean(m_VC)),by=.(Time_5Min)][order(Time_5Min)]

    # Visualization 1: Delay versus (AoG,GT)
    if(FALSE){
      
      library(plot3D)
      library(plotly)
      axx <- list(
        nticks = 10,
        range = c(0,1),
        title = "Green Time (%)"
      )
      
      axy <- list(
        nticks = 10,
        range = c(0,1),
        title = "Vehicle Arrival on Green (%)"
      )
      
      axz <- list(
        nticks = 10,
        range = c(0,100),
        title = "Average Delay (sec/veh)"
      )
      
      temp = list(xaxis=axx,yaxis=axy,zaxis=axz)
      plot_ly(data = Summary_v3, x = ~ GT, y = ~ AoG , z = ~AvgDelay, type='mesh3d') %>% layout(scene = temp)
      
    }
    
    # Visualization 2: By Time Variance
    BT_TT = Read_BT_TT(Pairid)
    pdf(paste("../Bluetooth/Results/",Pairid,"_Delay Comparison.pdf",sep=""))

    plot <- ggplot(Report, aes(x = Time_5Min,y = TotalDelay)) +
      geom_point() +
      geom_line(colour = "red" ,size = 0.5) + 
      geom_line(data = BT_TT, aes(x = Time, y = Delay), colour = "blue" ,size = 1.5) + 
      geom_smooth(colour = "red") +
      xlab("Time") +
      scale_y_continuous("Average Delay (sec/veh)", limits = c(0,100)) +
      ggtitle(paste("Pair ID: " , Pairid , ", Direction: " , SPMDirection, ", 2016-10-11 6-9AM",sep=""))
    
    print(plot)
    dev.off()

}   

library(randomForest)
Data_Merge = merge(Report, BT_TT[,.(Time,BT_Delay = Delay)], by.x = c("Time_5Min"), by.y = "Time")
fit.rf <- randomForest(BT_Delay ~ TotalDelay + M_Volume + M_AoG + M_VC, data = Data_Merge, importance = TRUE)
fit.lm <- lm(BT_Delay ~ TotalDelay + M_Volume + M_AoG + M_VC, data = Data_Merge)
summary(fit.lm)

Data_Merge[,p.rf :=predict(fit.rf, Data_Merge, type="response")]
Data_Merge[,p.lm :=predict(fit.lm, Data_Merge, type="response")]


ggplot(Report, aes(x = Time_5Min,y = M_VC*100)) +
  geom_point() +
  geom_line(colour = "red" ,size = 0.5) + 
  geom_line(data=Data_Merge,aes(x =Time_5Min, y = p.rf), colour = "green" ,size = 1.5) + 
  geom_line(data = BT_TT, aes(x = Time, y = Delay), colour = "blue" ,size = 1.5) + 
  geom_smooth(colour = "red") +
  xlab("Time") +
  scale_y_continuous("Average Delay (sec/veh)", limits = c(0,100)) +
  ggtitle(paste("Pair ID: " , Pairid , ", Direction: " , SPMDirection, ", 2016-10-11 6-9AM",sep=""))




    
