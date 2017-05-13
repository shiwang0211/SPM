library(data.table)
library(ggplot2)
PairTable<-fread("../Bluetooth/Configuration/PairTable.csv")
PairTable<-PairTable[complete.cases(PairTable)]
DeviceTable<-fread("../Bluetooth/Configuration/DeviceTable.csv")
save(PairTable,DeviceTable, file="./rda/Bluetooth.rda")

ggplot(testdata,aes (x = Delay)) +
  geom_histogram() +
  geom_vline(xintercept=median(testdata$Delay), linetype = "dashed", colour = "red", size = 2)

