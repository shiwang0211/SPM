library(data.table)
library(Metrics)
library(randomForest)
library(lmtest)
source('global.R')

load("./rda/Location.rda")
load("./rda/ByDirectonSPMVolume.rda")
load("./rda/ByDirectonTubeVolume.rda")
load("./rda/ByDirectonTubeVolume_2015.rda")
load("./rda/ByDirectonSPMVolume_2015.rda")
load("./rda/SPMDiffBtwLane.rda")

# Merge SPM and Tube by 15 min Interval ----


for (i in 1:nrow(SPMTubePair_v2)) # Note that there is Pair v2 from 03302017
  
{
  
  SPMSignalID = SPMTubePair_v2[["SPMSignalID"]][i]
  TubeStationID = SPMTubePair_v2[["TubeStationID"]][i]
  SPMDirection = SPMTubePair_v2[["SPMDirection"]][i]
  TubeDirection = SPMTubePair_v2[["TubeDirection"]][i]
  
  Data_Tube = ByDirectonTubeVolume [StationID == TubeStationID & Direction == TubeDirection,.(Time,Volume)][order(Time)]
  StartTime = min(Data_Tube[,Time]) # Only query SPM during the tube collection period
  EndTime = max(Data_Tube[,Time])   
  Data_SPM = ByDirectonSPMVolume [SignalID == SPMSignalID & Direction == SPMDirection,.(Time,Volume)][Time>=StartTime & Time<=EndTime][order(Time)]
  
  Data_Merge = merge(x = Data_Tube, y = Data_SPM, by.x = c("Time"),by.y= c("Time"), all.x =TRUE)
  Data_Merge = setDT(Data_Merge)
  colnames(Data_Merge)[2:3] = c("TubeVol", "SPMVol")
  Data_Merge[is.na(Data_Merge[,SPMVol]),SPMVol:=0] # zero SPM volume duing mid night
  Data_Merge[,Signal_ID := SPMSignalID][,StationID := TubeStationID][,Direction := SPMDirection]
  
  Data_Merge = merge(x = Data_Merge, y = NumberOfLanes, by =  c("Signal_ID", "Direction")) # From SPM 
  Data_Merge = merge(x = Data_Merge, y = DistanceFromStop, by =  c("Signal_ID", "Direction")) # From SPM
  
  #print(nrow(Data_Merge))
  
  if (i == 1) Final_Data_Merge = Data_Merge
  if (i > 1)  Final_Data_Merge = rbind(Final_Data_Merge,Data_Merge)
  
}


# Basic Post-Process -----

## Add calculated fields for Final_Data_Mergem

setnames(Final_Data_Merge, "N", "NoLanes")
Final_Data_Merge[,Hour := hour(Time)]
Final_Data_Merge[,TubeVolPerLane := TubeVol / NoLanes]
Final_Data_Merge[,SPMVolPerLane := SPMVol / NoLanes ]
Final_Data_Merge[,Difference := (SPMVol - TubeVol) ]
Final_Data_Merge[,Distance := as.numeric(Distance) ]
Final_Data_Merge[,Accuracy := (SPMVol / TubeVol) ]
Final_Data_Merge[,DifferencePerLane := Difference / NoLanes ]

VolLevel_series = sapply(Final_Data_Merge[,SPMVolPerLane], function(x) if (x <85) "Low" else if (x>=85 & x < 135) "Medium" else "High")
Final_Data_Merge[,VolLevel := factor(VolLevel_series, c("Low","Medium","High"))]

Distance_series = sapply(Final_Data_Merge[,Distance], function(x) if (x <= 170) "<= 170 ft" else  ">170 ft")
Final_Data_Merge[,DistanceLevel := factor(Distance_series, c("<= 170 ft",">170 ft"))]

## Save rda file

save(Final_Data_Merge, file="./rda/Final_Data_Merge.rda");
load("./rda/Final_Data_Merge.rda")


# Filter Out outliers ------------

Final_Data_Merge = Final_Data_Merge[paste(Signal_ID,StationID) %in% SPMTubePair_v2[,paste(SPMSignalID, TubeStationID)]] # Added on 20170330 to exlude them bad pairs
Final_Data_Merge = merge(Final_Data_Merge, SPMTubePair_v2[,.(SPMSignalID, TubeStationID, PairID, LocationCheck)], 
                                                            by.x = c("Signal_ID","StationID"), by.y = c("SPMSignalID", "TubeStationID"))
Final_Data_Merge = Final_Data_Merge [TubeVol > 50 & SPMVol > 50 & Hour >= 6 & Hour <= 21 ]
Final_Data_Merge = Final_Data_Merge[! Final_Data_Merge$Accuracy %in% boxplot.stats(Final_Data_Merge$Accuracy)$out, ]


# Join with Lane Utilization Difference
#SPMTubePair_v2[,Accuracy := Final_Data_Merge[,.(Avg_Acc = mean(Accuracy)), by = .(PairID)][order(PairID)][,Avg_Acc]]
Final_Data_Merge = merge(Final_Data_Merge, SPMDiffBtwLane, by.x = c("Signal_ID","Direction","Time"), by.y = c("SignalID", "Direction", "Timestamp"), all.x = TRUE)


# Statistical Tests 1: ANOVA ------

# Plot Difference / Accuracy 
Final_Data_Merge = Final_Data_Merge[LocationCheck == "GOOD"]
plot_ly(Final_Data_Merge[,.N,by=.(LocationCheck)], x =~LocationCheck, y= ~N,type="bar") %>%
  layout(autosize = T, width = 1000, height = 800, margin = list(
    l = 50,
    r = 50,
    b = 200,
    t = 50,
    pad=0)
  )

plot_ly(SPMTubePair_v2[,.(Accuracy, LocationCheck)], y =~Accuracy, color = ~as.factor(LocationCheck),type="box") %>%
  layout(autosize = T, width = 1000, height = 800, margin = list(
    l = 50,
    r = 50,
    b = 200,
    t = 50,
    pad=0),showlegend = FALSE,yaxis=list(range = c(0.6,1.3))
  )

hist( Final_Data_Merge $ NoLanes)
plot_ly(Final_Data_Merge, y =~Accuracy, color = ~as.factor(NoLanes),type="box") %>%
  layout(showlegend = FALSE,yaxis=list(range = c(0.6,1.3)),xaxis = list(title= "Number of Lanes"))

plot( Final_Data_Merge $ VolLevel)
plot_ly(Final_Data_Merge, y =~Accuracy, color = ~as.factor(VolLevel),type="box") %>%
  layout(showlegend = FALSE,yaxis=list(range = c(0.6,1.3)),xaxis = list(title= "Level of Traffic Volume"))

hist( Final_Data_Merge $ Hour)
plot_ly(Final_Data_Merge, y =~Accuracy, color = ~as.factor(Hour),type="box") %>%
  layout(showlegend = FALSE,yaxis=list(range = c(0.6,1.3)),xaxis = list(title= "Hour of Day",autotick=FALSE, dtick = 1))

plot( Final_Data_Merge $ DistanceLevel)
plot_ly(Final_Data_Merge, y =~Accuracy, color = ~as.factor(DistanceLevel),type="box") %>%
  layout(showlegend = FALSE,yaxis=list(range = c(0.6,1.3)),xaxis = list(title= "Distance of SPM Detector from Stop Bar"))


# ANOVA Test

mod = lm(Accuracy ~ as.factor(DistanceLevel) + as.factor(NoLanes) + as.factor(VolLevel)  , data = Final_Data_Merge[LocationCheck == "GOOD"])
fit.anova <- aov(mod)
bptest(fit.anova)
summary(fit.anova)
plot(fit.anova,1)
plot(fit.anova,2)
tuk <- TukeyHSD(fit.anova)

# Summary of mean and percentiles for Accuracy within each group

PIVOT = Final_Data_Merge[,.(.N, MEAN = mean(Accuracy),MIN = min(Accuracy),
                LB_25 = quantile(Accuracy,0.25), UB_75 = quantile(Accuracy,0.75),
                MAX = max(Accuracy), SD = sd(Accuracy)), 
             by=.(VolLevel, NoLanes)]
write.csv(PIVOT, "../Results/ByGroupAccuracy.csv", row.names = FALSE)

PIVOT = Final_Data_Merge[,.(.N, MEAN = mean(Accuracy),MIN = min(Accuracy),
                LB_25 = quantile(Accuracy,0.25), UB_75 = quantile(Accuracy,0.75),
                MAX = max(Accuracy), SD = sd(Accuracy)), 
             by=.(Signal_ID)]
write.csv(PIVOT, "../Results/BySignalAccuracy.csv", row.names = FALSE)

PIVOT = Final_Data_Merge[,.(.N, MEAN = mean(Accuracy),MIN = min(Accuracy),
                            LB_25 = quantile(Accuracy,0.25), UB_75 = quantile(Accuracy,0.75),
                            MAX = max(Accuracy), SD = sd(Accuracy)), 
                         by=.(Hour)]
write.csv(PIVOT, "../Results/ByHourAccuracy.csv", row.names = FALSE)

pred_anova = Final_Data_Merge[,.(MEAN_ACCURACY = mean(Accuracy)), by=.(VolLevel, NoLanes, DistanceLevel)] 


# Statistical Tests 2: Regression ----

r2 <- function(pred.y, true.y)
{ 1 - length(true.y)*mse(pred.y, true.y)/((length(true.y)-1)*var(true.y)) }

set.seed(1234)
L = nrow(Final_Data_Merge)
L_train = L * 0.70
train_label <-sample(c(1:L),L_train)
test_label <- c(1:L)[! c(1:L) %in% train_label]

Final_Data_Merge[,Hour:=as.factor(Hour)] # set hour_ as factor variable
train <- Final_Data_Merge[train_label, .(DifferencePerLane, SPMVolPerLane, NoLanes, Distance, Hour)]
test <-  Final_Data_Merge[test_label, .(DifferencePerLane, SPMVolPerLane, NoLanes, Distance, Hour)]

r2(test[,SPMVolPerLane],test[,SPMVolPerLane - DifferencePerLane]) # benchmark
plot(test[,SPMVolPerLane],test[,SPMVolPerLane - DifferencePerLane]) # benchmark

## Statistical Tests 2.1:  Linear Regression ----

mod.ls <- lm(DifferencePerLane~. , data = train)
mod.ls.null <- lm(DifferencePerLane~1 , data = train)
summary(mod.ls)
p.ls <- predict(mod.ls,newdata = test)

r2(test[,SPMVolPerLane] - p.ls,test[,SPMVolPerLane - DifferencePerLane])
plot(test[,SPMVolPerLane] - p.ls,test[,SPMVolPerLane - DifferencePerLane])

## Statistical Tests 2.2:  Random Forest ----

fit.rf <- randomForest(DifferencePerLane ~., data = train, importance = TRUE)
p.rf<-predict(fit.rf, test, type="response")

if(FALSE){
  attr.utl<-sort(importance(fit.rf)[,1],decreasing = TRUE)
  asets<-
    'names<-'(lapply(c(10,25,50,100),
                     function(p)
                       names(attr.utl)[1:round(p*length(attr.utl)/100)]),
              paste(c(10,25,50,100),"percent",sep=" "))
  
  varImpPlot(fit.rf, type=1)
  importance(fit.rf)
}

r2(test[,SPMVolPerLane] - p.rf,test[,SPMVolPerLane - DifferencePerLane])
plot(test[,SPMVolPerLane] - p.rf,test[,SPMVolPerLane - DifferencePerLane])


# Apply the seleted model to the entire dataset ----

ByDirectonSPMVolume_Copy <- ByDirectonSPMVolume
setnames(ByDirectonSPMVolume_Copy, "Volume", "SPMVol")
setnames(ByDirectonSPMVolume_Copy, "SignalID", "Signal_ID")

Data_Merge = merge(x = ByDirectonSPMVolume_Copy, y = NumberOfLanes, by =  c("Signal_ID", "Direction"))
Data_Merge = merge(x = Data_Merge, y = DistanceFromStop, by =  c("Signal_ID", "Direction"))

Data_Merge[,Hour:=hour(Time)]
Data_Merge = Data_Merge[SPMVol > 50 & Hour >= 6 & Hour <= 21] # Range for selected model

setnames(Data_Merge, "N", "NoLanes")
Data_Merge[,Hour:=as.factor(Hour)]
Data_Merge[,Distance := as.numeric(Distance) ]
Data_Merge[,SPMVolPerLane := SPMVol / NoLanes ]

VolLevel_series = sapply(Data_Merge[,SPMVolPerLane], function(x) if (x <150) "Low" else if (x>=150 & x < 150) "High" else "High")
Data_Merge[,VolLevel := factor(VolLevel_series, c("Low","Medium","High"))]
Distance_series = sapply(Data_Merge[,Distance], function(x) if (x <= 170) "Close" else  "Far")
Data_Merge[,DistanceLevel := factor(Distance_series, c("Close","Far"))]

p.anova = merge(x = Data_Merge, y = pred_anova, by =  c("VolLevel", "NoLanes", "DistanceLevel"), all.x = TRUE)[,MEAN_ACCURACY]
p.anova[is.na(p.anova)]=1  # levels not appearing in selected dataset
p.ls<-predict(mod.ls, newdata = Data_Merge) * Data_Merge[,NoLanes]# Apply model from statistical tests "mod.ls"
p.rf<-predict(fit.rf, newdata = Data_Merge , type = "response") * Data_Merge[,NoLanes]

setnames(ByDirectonSPMVolume, "SPMVol", "Volume")
setnames(ByDirectonSPMVolume, "Signal_ID", "SignalID")

ByDirectonSPMVolume[Volume > 50 & hour(Time) >= 6 & hour(Time) <= 21, Prediction_ls:= Volume - p.ls] # Append Prediction
ByDirectonSPMVolume[Volume > 50 & hour(Time) >= 6 & hour(Time) <= 21, Prediction_rf:= Volume - p.rf] # Append Prediction
ByDirectonSPMVolume[Volume > 50 & hour(Time) >= 6 & hour(Time) <= 21, Prediction_anova:= Volume / p.anova] # Append Prediction
save(ByDirectonSPMVolume, file="./rda/ByDirectonSPMVolume.rda") # Save back
