library(dplyr)
library(splitstackshape)
library(data.table)
library(xlsx)

setwd("C:/Users/mahe/Desktop/Jet_new_data")
data <- as.data.frame(read.table("etd_May2018.csv",header=TRUE,sep=","))

x<-tolower(data$REASON)

unique(x)

for(i in 1:nrow(data))
{
  if(sapply("air traffic", grepl, x[i])==TRUE || sapply("atc", grepl, x[i])==TRUE
     || sapply("slot", grepl, x[i])==TRUE || sapply("traffic", grepl, x[i])==TRUE
     || sapply("air", grepl, x[i])==TRUE)
    
    a[i]<-"ATC"
  
  else if(sapply("weather", grepl, x[i])==TRUE || sapply("wx", grepl, x[i])==TRUE
          || sapply("rain", grepl, x[i])==TRUE || sapply("wind", grepl, x[i])==TRUE
          || sapply("lightning", grepl, x[i])==TRUE
          || sapply("lighting", grepl, x[i])==TRUE)
    
    a[i]<-"Bad Weather"
  
  else if(sapply("crew", grepl, x[i])==TRUE || sapply("guest", grepl, x[i])==TRUE)
    
    a[i]<-"Connecting Guest/Crew"
  
  else if(sapply("tech", grepl, x[i])==TRUE ||
          sapply("maintainance", grepl, x[i])==TRUE ||
          sapply("requirement", grepl, x[i])==TRUE || 
          sapply("inflight", grepl, x[i])==TRUE ||
          sapply("services", grepl, x[i])==TRUE)
    
    a[i]<-"Technical"
  
  else if(sapply("medical", grepl, x[i])==TRUE)
    
    a[i]<-"Medical"
  
  else if(sapply("vvip", grepl, x[i])==TRUE)
    
    a[i]<-"VVIP Movement"
  
  else if(sapply("rwy", grepl, x[i])==TRUE || sapply("runway", grepl, x[i])==TRUE
          || sapply("clouser", grepl, x[i])==TRUE || sapply("clsr", grepl, x[i])==TRUE
          || sapply("parking", grepl, x[i])==TRUE)
    
    a[i]<-"Runway/Airport Clouser/Non Availability"
  
  else if(sapply("disabled aircraft", grepl, x[i])==TRUE)
    
    a[i]<-"Disabled Aircraft"
  
  else if(sapply("operational", grepl, x[i])==TRUE || sapply("ops", grepl, x[i])==TRUE
          || sapply("operation", grepl, x[i])==TRUE)
    
    a[i]<-"Operational"
  
  else if(sapply("grnd", grepl, x[i])==TRUE || sapply("ground", grepl, x[i])==TRUE)
    
    a[i]<-"Ground Services"
  
  else if(sapply("slot restriction", grepl, x[i])==TRUE)
    
    a[i]<-"Slot Restriction"
  
  else if(sapply("air force", grepl, x[i])==TRUE || sapply("airforce", grepl, x[i])==TRUE)
    
    a[i]<-"Air Force"
  
  else if(sapply("commercial", grepl, x[i])==TRUE)
    
    a[i]<-"Commercial"
  
  else if(sapply("payload restriction", grepl, x[i])==TRUE)
    
    a[i]<-"Payload Restriction"
  
  else if(sapply("aog", grepl, x[i])==TRUE)
    
    a[i]<-"AOG"
  
  else if(sapply("ctot", grepl, x[i])==TRUE)
    
    a[i]<-"CTOT"
  else
    
    a[i]<-"NA"
}

cat<-as.data.frame(table(a))

write.xlsx(cat, file = "Categories.xlsx")
