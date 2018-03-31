
#get data
load("data/d.RData")

#get visits:
v <- d[["Live"]][["getLastVisitsDetails:Visits"]]
v$idVisit <- as.numeric(levels(v$idVisit))[v$idVisit]
v$actions <- as.numeric(levels(v$actions))[v$actions]

#get actions:
a <- d[["Live"]][["getLastVisitsDetails:Actions"]]

#remove useless variables:
a <- a[a$field!="icon",]
a <- a[a$field!="pageTitle",]
a <- a[a$field!="timeSpentPretty",]
a <- a[a$field!="serverTimePretty",]
a <- a[a$field!="pageId",]

#remove duplicate rows:
a <- unique(a)

#discard incomplete visits:
a$step <- as.numeric(a$step)
a.nb.visits <- setNames(aggregate(a$step,by=list(a$idVisit),function(x) max(x)+1),
                        c("idVisit","actions"))
av.nb.visits <- merge(x=v[,c("idVisit","actions")],y=a.nb.visits,by="idVisit")
av.nb.visits$bad <- F
av.nb.visits[av.nb.visits$actions.x!=av.nb.visits$actions.y,]$bad <- T
v$bad <- F
bad.visits <- av.nb.visits[av.nb.visits$bad,]$idVisit
v[v$idVisit %in% bad.visits,]$bad <- T
a <- a[!a$idVisit %in% bad.visits,] #remove visits with incomplete actions
rm(a.nb.visits,av.nb.visits,bad.visits)

#discard visits with incomplete actions:
D <- setNames(aggregate(a$idVisit,by=list(a$idVisit,a$step),FUN=length),
              c("idVisit","step","x"))
bad.visits <- unique(D[D$x<max(D$x),]$idVisit)
v[v$idVisit %in% bad.visits,]$bad <- T
a <- a[!a$idVisit %in% bad.visits,]
rm(bad.actions,bad.visits,D)

#convert generationTime to numeric
t <- a[a$field=="generationTime",]$value
t.s <- suppressWarnings(as.numeric(sub(".*?([0-9,\\.]+)s$","\\1",t)))
t.m <- suppressWarnings(as.numeric(sub("^([0-9,\\.]+)\\smin.+$","\\1",t)))
t.tot.s <- apply(cbind(t.m*60, t.s), 1, function(x) ifelse(all(is.na(x)), NA, sum(x, na.rm=T)))
a[a$field=="generationTime",]$value <- t.tot.s
rm(t.tot.s,t.s,t.m,t)

#estimate timeSpent on last action:

altTimeSpent <- function(func) {
  
  a.mult <- a[a$idVisit %in% v[v$actions>1 & !v$bad,]$idVisit,]
  t.s <- a.mult[a.mult$field=="timeSpent",c("idVisit","value")]
  t.s$value <- as.numeric(t.s$value)
  func.var <- paste("timeSpent",func,sep=".")
  if (func.var %in% names(a)) a[,c(func.var)] <- NULL #add 03-02-18 21:53
  
  if (func=="mean") {
    ts.func <- aggregate(.~idVisit,t.s,mean,na.rm=T,na.action=na.pass)
  } else if (func=="last") {
    ts.func <- aggregate(.~idVisit,t.s,tail,1) 
  } else if (func=="randmean") {
    ts.func <- aggregate(.~idVisit,t.s,function(x)
      sample(0:mean(x,na.rm=T,na.action=na.pass),1)
      )
  } else if (func=="randmedian") {
    ts.func <- aggregate(.~idVisit,t.s,function(x)
      sample(0:median(x,na.rm=T,na.action=na.pass),1)
      )
  } else if (func=="slr") {
      t.s <- a.mult[a.mult$field=="timeSpent",c("idVisit","step","value")]
      t.s$value <- as.numeric(t.s$value)
      s <- split(t.s,t.s$idVisit)
      ss <- lapply(s,head,-1)
      ss.lm <- lapply(ss,function(x) lm(value ~ step, x))
      rm(ss)
      sss <- lapply(s,tail,1)
      rm(s)
      ts.func <- unlist(suppressWarnings(
        lapply(1:length(ss.lm),function(x)
          round(predict(ss.lm[[x]],sss[[x]]))
        )
      ))
      rm(ss.lm)
      ts.func <- ifelse(ts.func<0,1,ts.func) #replace negative values by 1
      ts.func <- ifelse(ts.func>1799,1,ts.func) #replace high values (above ~30m min) by 1 #06-02-2018
      ts.func <- as.data.frame(cbind(as.numeric(names(sss)),ts.func))
      rm(sss)
  } else if (func=="harmonize") {
    ts.func <- aggregate(.~idVisit,t.s,function(x) 1)
  }
  rm(t.s)
  ts.func <- setNames(ts.func,c("idVisit",func.var))
  
  a.maxstep <- setNames(aggregate(a.mult$step,by=list(a.mult$idVisit),FUN=max),
                        c("idVisit","step"))
  t <- cbind(merge(a.maxstep,ts.func,by='idVisit'),"timeSpent",stringsAsFactors=F)
  rm(a.maxstep,ts.func)
  t <- setNames(t,c(head(names(t),3),"field"))
  tt <- merge(a,t,by=c("idVisit","step","field"),all.x=T)
  rm(t)
  ttt <- tt[tt$field=="timeSpent",c("value",func.var)]
  ttt$value <- as.numeric(ttt$value)
  ttt[,c(func.var)] <- rowSums(ttt,na.rm=T)
  tt[tt$field=="timeSpent",c(func.var)] <- round(ttt[,c(func.var)])
  rm(ttt,func.var)
  tt <- tt[,c(4,1:3,5:ncol(tt))]
  return(tt)
  
}

mm <- c("mean","last","randmean","randmedian","slr","harmonize")
# mm <- c("slr")
for (m in mm) {
  print(m)
  a <- altTimeSpent(m)
}
rm(mm)

#export data to files:

pat.5 <- "^2017-05.+$"
pat.3.4 <- "^2017-0[3,4].+$"
pat.3.4.5 <- "^2017-0[3,4,5].+$"

  #visits non detailed:

v.f <- c("idVisit","visitorType","visitorId","visitIp","visitDurationPretty","visitCount","serverTimePretty","referrerType","referrerName","date","country","actions","browser","browserCode","browserFamily","browserFamilyDescription","browserName","browserVersion","city","continent","continentCode","countryCode","daysSinceFirstVisit","daysSinceLastVisit","deviceBrand","deviceModel","deviceType","firstActionTimestamp","language","languageCode","lastActionDateTime","lastActionTimestamp","latitude","location","longitude","operatingSystem","operatingSystemCode","operatingSystemName","operatingSystemVersion","plugins","referrerKeyword","referrerKeywordPosition","referrerSearchEngineUrl","referrerTypeName","referrerUrl","region","regionCode","resolution","searches","serverDate","serverDatePretty","serverDatePrettyFirstAction","serverTimePrettyFirstAction","serverTimestamp","visitDuration","visitLocalHour","visitLocalTime","visitServerHour","bad")

v.file.pref <- "ourstatistics_visits-individual_2017-"
v.5 <- grepl(pat.5,v$date)
v.3.4 <- grepl(pat.3.4,v$date)
v.3.4.5 <- grepl(pat.3.4.5,v$date)

write.csv(v[v.3.4.5,v.f],paste0(v.file.pref,"03-04-05.csv"),row.names=F)
write.csv(v[v.3.4,v.f],paste0(v.file.pref,"03-04.csv"),row.names=F)
write.csv(v[v.5,v.f],paste0(v.file.pref,"05.csv"),row.names=F)

rm(v.f,v.5,v.3.4,v.3.4.5,v.file.pref)

  #actions:

a.file.pref <- "ourstatistics_actions-individual_2017-"
a.5 <- grepl(pat.5,a$date)
a.3.4 <- grepl(pat.3.4,a$date)
a.3.4.5 <- grepl(pat.3.4.5,a$date)

write.csv(a[a.3.4.5,],paste0(a.file.pref,"03-04-05.csv"),row.names=F)
write.csv(a[a.3.4,],paste0(a.file.pref,"03-04.csv"),row.names=F)
write.csv(a[a.5,],paste0(a.file.pref,"05.csv"),row.names=F)

rm(a.5,a.3.4,a.3.4.5,a.file.pref,pat.5,pat.3.4,pat.3.4.5)

#end
