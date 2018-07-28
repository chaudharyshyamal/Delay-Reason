setwd("C:/Users/mahe/Desktop/Jet_new_data/DataForDelayReport")
data <- as.data.frame(read.table("ETD_Report.csv",header=TRUE,sep=","))


Date <- readline(prompt="Enter Date(in this format(24-May-2018)): ")


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

DataByDate<-subset(d,d$DATE==Date)

UniqueR<-nrow(as.data.frame(unique(d$Reason)))




t<-0
gs<-0
infd<-0
o<-0
bw<-0
atc<-0
mis<-0


for(j in 1:nrow(DataByDate))
{
  if(DataByDate$Reason[j]=="TECH")
    t=t+1
  
  else if(DataByDate$Reason[j]=="GRND SVC")
    gs=gs+1
  
  else if(DataByDate$Reason[j]=="INFLIGHT")
    infd=infd+1
  
  else if(DataByDate$Reason[j]=="OPERATIONS")
    o=o+1
  
  else if(DataByDate$Reason[j]=="WX")
    bw=bw+1
  
  else if(DataByDate$Reason[j]=="ATC")
    atc=atc+1
  
  else if(DataByDate$Reason[j]=="Miscellaneous")
    mis=mis+1
  
}


Reason<-unique(d$Reason)
Count<-c(atc,t,gs,o,bw,mis,infd)
x<-data.frame(Reason,Count)


write.xlsx(x, file = paste("DelayReport(DelayReasonCount)BT",Date,".xlsx"),row.names=FALSE)

