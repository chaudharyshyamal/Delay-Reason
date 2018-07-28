setwd("C:/Users/mahe/Desktop/Jet_new_data/DataForDelayReport")
data <- as.data.frame(read.table("ETD_Report.csv",header=TRUE,sep=","))


Date <- readline(prompt="Enter Date(in this format(24-May-2018)): ")

DataByDate<-subset(d,d$DATE==Date)

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


atc<-0
t<-0
bw<-0
mis<-0
o<-0
gs<-0
aog<-0


for(j in 1:nrow(DataByDate))
{
  if(DataByDate$Reason[j]=="TECH")
    t=t+1
  
  else if(DataByDate$Reason[j]=="GRND SVC")
    gs=gs+1
  
  else if(DataByDate$Reason[j]=="INFLIGHT")
    inf=inf+1
  
  else if(DataByDate$Reason[j]=="OPERATIONS")
    o=o+1
  
  else if(DataByDate$Reason[j]=="WX")
    bw=bw+1
  
  else if(DataByDate$Reason[j]=="ATC")
    atc=atc+1
  
  else if(DataByDate$Reason[j]=="Miscellaneous")
    mis=mis+1
  
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
#df$Reason<-NULL
df$ATD.UTC<-NULL
d<-unique(df)

UniqueR<-nrow(as.data.frame(unique(d$Reason)))


atc<-0
cgc<-0
t<-0
bw<-0
med<-0
mis<-0
vip<-0
rc<-0
o<-0
gs<-0
com<-0
ctot<-0
aog<-0


for(j in 1:nrow(DataByDate))
{
  if(DataByDate$Reason[j]=="ATC")
    atc=atc+1
  
  else if(DataByDate$Reason[j]=="Connecting Guest/Crew")
    cgc=cgc+1
  
  else if(DataByDate$Reason[j]=="Technical")
    t=t+1
  
  else if(DataByDate$Reason[j]=="Bad Weather")
    bw=bw+1
  
  else if(DataByDate$Reason[j]=="Medical")
    med=med+1
  
  else if(DataByDate$Reason[j]=="Miscellaneous")
    mis=mis+1
  
  else if(DataByDate$Reason[j]=="VVIP Movement")
    vip=vip+1
  
  else if(DataByDate$Reason[j]=="Runway/Airport Closure/Non Availability")
    rc=rc+1
  
  else if(DataByDate$Reason[j]=="Operational")
    o=o+1
  
  else if(DataByDate$Reason[j]=="Ground Services")
    gs=gs+1
  
  else if(DataByDate$Reason[j]=="Commercial")
    com=com+1
  
  else if(DataByDate$Reason[j]=="CTOT")
    ctot=ctot+1
  
  else if(DataByDate$Reason[j]=="AOG")
    aog=aog+1
}


Reason<-unique(d$Reason)
Count<-c(atc,t,bw,mis,o,gs,aog)
x<-data.frame(Reason,Count)


write.xlsx(x, file = paste("DelayReport(DelayReasonCount)BT",Date,".xlsx"))


df1<-subset(DataByDate,DataByDate$Reason=="GRND SVC" && DataByDate$DATE="")



