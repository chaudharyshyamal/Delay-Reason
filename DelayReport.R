library(lubridate)
library(data.table)
library(xlsx)
library(chron)

setwd("C:/Users/mahe/Desktop/Jet_new_data/DataForDelayReport")
data <- as.data.frame(read.table("ETD_Report.csv",header=TRUE,sep=","))

Date<- readline(prompt="Enter Date(in this format(24-May-2018)): ")
dat<-strftime(as.POSIXct(data$DATE, format="%d-%b-%Y"), format = "%d")
data<-subset(data,dat=Date)

Month <- readline(prompt="Enter Month(in this format(May)): ")
mon<-strftime(as.POSIXct(data$DATE, format="%d-%b-%Y"), format = "%b")
data<-subset(data,mon=Month)

x<-tolower(data$REASON)


for(i in 1:nrow(data))
{
  
  if(sapply("tech", grepl, x[i])==TRUE ||
     sapply("maintainance", grepl, x[i])==TRUE ||
     sapply("requirement", grepl, x[i])==TRUE ||
     sapply("services", grepl, x[i])==TRUE || sapply("grounding", grepl, x[i])==TRUE ||
     sapply("aog", grepl, x[i])==TRUE)
    data$Reason[i]<-"TECH"
  
  
  
  else if(sapply("grnd", grepl, x[i])==TRUE || sapply("ground", grepl, x[i])==TRUE ||
          sapply("commercial", grepl, x[i])==TRUE || sapply("guest", grepl, x[i])==TRUE)
    data$Reason[i]<-"GRND SVC"
  
  
  
  else if(sapply("inflight", grepl, x[i])==TRUE || sapply("inflt", grepl, x[i])==TRUE)
    data$Reason[i]<-"INFLIGHT"
  
  
  
  else if(sapply("operational", grepl, x[i])==TRUE || sapply("ops", grepl, x[i])==TRUE
          || sapply("operation", grepl, x[i])==TRUE)
    data$Reason[i]<-"OPERATIONS"
  
  
  
  else if(sapply("weather", grepl, x[i])==TRUE || sapply("wx", grepl, x[i])==TRUE
          || sapply("rain", grepl, x[i])==TRUE || sapply("wind", grepl, x[i])==TRUE)
    data$Reason[i]<-"WX"
  
  
  
  else if(sapply("air traffic", grepl, x[i])==TRUE || sapply("atc", grepl, x[i])==TRUE
          || sapply("slot", grepl, x[i])==TRUE || sapply("traffic", grepl, x[i])==TRUE
          || sapply("air", grepl, x[i])==TRUE || sapply("restriction", grepl, x[i])==TRUE ||
          sapply("ctot", grepl, x[i])==TRUE || sapply("ctoct", grepl, x[i])==TRUE)
    data$Reason[i]<-"ATC"
  
  
  
  else if(sapply("lightning", grepl, x[i])==TRUE
          || sapply("lighting", grepl, x[i])==TRUE)
    data$Reason[i]<-"Miscellaneous"
  
  
  
  else
    data$Reason[i]<-"Miscellaneous"
}



for(i in 1:nrow(data))
{
  if(sapply("air traffic", grepl, x[i])==TRUE || sapply("atc", grepl, x[i])==TRUE
     || sapply("slot", grepl, x[i])==TRUE || sapply("traffic", grepl, x[i])==TRUE
     || sapply("air", grepl, x[i])==TRUE || sapply("restriction", grepl, x[i])==TRUE)
    
    data$Reason[i]<-"ATC"
  
  else if(sapply("weather", grepl, x[i])==TRUE || sapply("wx", grepl, x[i])==TRUE
          || sapply("rain", grepl, x[i])==TRUE || sapply("wind", grepl, x[i])==TRUE
          || sapply("lightning", grepl, x[i])==TRUE
          || sapply("lighting", grepl, x[i])==TRUE)
    
    data$Reason[i]<-"Bad Weather"
  
  else if(sapply("crew", grepl, x[i])==TRUE || sapply("guest", grepl, x[i])==TRUE)
    
    data$Reason[i]<-"Connecting Guest/Crew"
  
  else if(sapply("tech", grepl, x[i])==TRUE ||
          sapply("maintainance", grepl, x[i])==TRUE ||
          sapply("requirement", grepl, x[i])==TRUE || 
          sapply("inflight", grepl, x[i])==TRUE ||
          sapply("services", grepl, x[i])==TRUE)
    
    data$Reason[i]<-"Technical"
  
  else if(sapply("medical", grepl, x[i])==TRUE)
    
    data$Reason[i]<-"Medical"
  
  else if(sapply("vvip", grepl, x[i])==TRUE || sapply("vip", grepl, x[i])==TRUE)
    
    data$Reason[i]<-"VVIP Movement"
  
  else if(sapply("rwy", grepl, x[i])==TRUE || sapply("runway", grepl, x[i])==TRUE
          || sapply("clouser", grepl, x[i])==TRUE || sapply("clsr", grepl, x[i])==TRUE
          || sapply("parking", grepl, x[i])==TRUE)
    
    data$Reason[i]<-"Runway/Airport Closure/Non Availability"
  
  else if(sapply("disabled aircraft", grepl, x[i])==TRUE)
    
    data$Reason[i]<-"Disabled Aircraft"
  
  else if(sapply("operational", grepl, x[i])==TRUE || sapply("ops", grepl, x[i])==TRUE
          || sapply("operation", grepl, x[i])==TRUE)
    
    data$Reason[i]<-"Operational"
  
  else if(sapply("grnd", grepl, x[i])==TRUE || sapply("ground", grepl, x[i])==TRUE
          || sapply("grounding", grepl, x[i])==TRUE)
    
    data$Reason[i]<-"Ground Services"
  
  else if(sapply("slot restriction", grepl, x[i])==TRUE)
    
    data$Reason[i]<-"Slot Restriction"
  
  else if(sapply("air force", grepl, x[i])==TRUE || sapply("airforce", grepl, x[i])==TRUE)
    
    data$Reason[i]<-"Air Force"
  
  else if(sapply("commercial", grepl, x[i])==TRUE)
    
    data$Reason[i]<-"Commercial"
  
  else if(sapply("payload restriction", grepl, x[i])==TRUE)
    
    data$Reason[i]<-"Payload Restriction"
  
  else if(sapply("aog", grepl, x[i])==TRUE)
    
    data$Reason[i]<-"AOG"
  
  else if(sapply("ctot", grepl, x[i])==TRUE)
    
    data$Reason[i]<-"CTOT"
  
  else if(sapply("eet", grepl, x[i])==TRUE)
    
    data$Reason[i]<-"EET"
  
  else if(sapply("awtg", grepl, x[i])==TRUE)
    
    data$Reason[i]<-"AWTG"
  
  else
    
    data$Reason[i]<-"Miscellaneous"
}

data$REASON<-NULL
#data$STD.UTC<-NULL
data$STD..Airport.Local.Time.<-NULL
data$STA.UTC<-NULL
data$STA.in.Local.Time<-NULL
#data$ATD.UTC<-NULL
data$ATD..Airport.Local.Time.<-NULL
data$ATA.UTC<-NULL
data$ATA.in.Local.Time<-NULL
data$Entry.Date.UTC<-NULL
data$Entry.Date..Airport.Local.Time.<-NULL
data$ETD..Airport.Local.Time.<-NULL

UniqueDat<-unique(data$DATE)
UniqueDate<-as.data.table(UniqueDat)


df<-data
df$STD.UTC<-NULL
df$ETD.UTC<-NULL
df$Reason<-NULL
df$ATD.UTC<-NULL
d<-unique(df)

d$DelatDuration<-NULL
d$Reason<-NULL
d$DelayCount<-NULL
d$STD_UTC<-NULL
d$ETD_UTC<-NULL

for(i in 1:nrow(d))
{
  d$DelayCount[i]<-0
  d$Reason[i]<-""
  d$STD_UTC[i]<-""
  d$ETD_UTC[i]<-""
  
  for(j in 1:nrow(data))    
  {
    d$STD_UTC[i]<-strftime(as.POSIXct(data$STD.UTC[i], format="%d-%b-%Y %H:%M"), format = "%H:%M")
    
    if(data$DATE[j]==d$DATE[i] && data$FLIGHT.NO[j]==d$FLIGHT.NO[i] 
       && data$DEPSEC[j]==d$DEPSEC[i] && data$ARRSEC[j]==d$ARRSEC[i])
    {
      d$DelayCount[i]<-d$DelayCount[i]+1
      d$Reason[i]<-paste(d$Reason[i], data$Reason[j],sep = "  ")
      
      
      d$DelatDuration[i]<-round(difftime(strptime(data$ATD.UTC[j],"%d-%b-%Y %H:%M"),
                                         strptime(data$STD.UTC[j],"%d-%b-%Y %H:%M"),
                                         units = "mins"),digits = 2)
      
      t<-strftime(as.POSIXct(data$ETD.UTC[j], format="%d-%b-%Y %H:%M"), format = "%H:%M")
      d$ETD_UTC[i]<-paste(d$ETD[i], t,sep = "  ")
    }
  }
}


dt<-subset(d,d$DelatDuration>0)

write.xlsx(dt, file = paste("DelayReportBT",Month,".xlsx"))


dtMultipledelay<-subset(d,d$DelayCount>1)

write.xlsx(dtMultipledelay, file = paste("DelayReportMultipleDelayBT",Month,".xlsx"))



#No of flights (delay)
nrow(dt)

#No of flights (multiple delay)
nrow(dtMultipledelay)






