library(data.table)
Last <- function(x) { tail(x, n = 1) }
 
Delay<-function(Volume, Phase, NUM_LANES){
  
  # Order Volume and Phase, Trim
  Combine = rbind(Volume, Phase)
  Combine = Combine[order(Combine$TimeSec)]
  first_ = which(Combine[,EventCode] == 10)[1]
  last_ = Last(which(Combine[,EventCode] == 10))
  Combine = Combine[first_:last_]
  
  #Constants 
  s = 1900 * NUM_LANES / 3600 
  
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
  
  Combine[,Cycle:=Cycle_][,Green:=Green_][,Tk:=Tk_][,Td:=Td_][,Ck:=Ck_][,Qk:=Qk_][,Dk := Dk_]
  Delay_ = Combine[Cycle>0,.(Delay=sum(Dk), Cyclength = max(TimeSec) - min(TimeSec)), by=.(Cycle)]
  Volume_ = Combine[EventCode == 82,.(Volume = .N), by=.(Cycle)]
  GreenTime_ = Combine[Green == 1,.(GreenTime = max(TimeSec) - min(TimeSec)), by=.(Cycle)]
  AoG_ = Combine[EventCode == 82 & Green == 1,.(ArrsOnGreen = .N), by=.(Cycle)]
  
  Summary<-merge(Delay_, Volume_, by="Cycle")
  Summary_v2<-merge(Summary, GreenTime_, by="Cycle")
  Summary_v3<-merge(Summary_v2, AoG_, by="Cycle")
  
  Summary_v3[,GT := GreenTime / Cyclength][,AoG:= ArrsOnGreen / Volume][,AvgDelay := Delay / Volume]
  Summary_v3[,Delay := NULL][,GreenTime := NULL][,ArrsOnGreen := NULL]
  
  # Visualization
  
  library(plot3D)
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
