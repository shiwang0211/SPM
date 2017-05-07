library(data.table)
PairTable<-fread("../Bluetooth/Configuration/PairTable.csv")
PairTable<-PairTable[complete.cases(PairTable)]
DeviceTable<-fread("../Bluetooth/Configuration/DeviceTable.csv")
save(PairTable,DeviceTable, file="./rda/Bluetooth.rda")


testdata<-fread("../Bluetooth/Reference/bluetoad-export 7035 from 2016-10-11 07-00-00 - 2016-10-11 09-00-59 (5-min smooth).csv")
colnames(testdata) = c("DOW","Date","Time","NOTUSE","TT","Speed")
testdata = testdata[TT != "Not enough matches"]
testdata[,FFTT := 1 / 45 * 3600]
testdata[,Delay := as.numeric(TT) - FFTT]
