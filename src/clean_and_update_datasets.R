
#get data
# load("data/d.RData")

print("* start cleaning individual data")

#get visits:
v <- d[["Live"]][["getLastVisitsDetails:Visits"]]
v$idVisit <- as.numeric(v$idVisit)
v$actions <- as.numeric(v$actions)

#get actions:
a <- d[["Live"]][["getLastVisitsDetails:Actions"]]

#remove useless variables for visits dataset:
var.useless <- sapply(names(v), function(x) length(unique(v[,c(x)])))
var.useless <- var.useless[var.useless==1]
cat(paste("discard following variables in visits dataset:\n",paste(names(var.useless),collapse=", ")))
cat("\n")
v[,names(var.useless)] <- NULL
rm(var.useless)

#remove useless variables for actions dataset:
a <- a[a$field!="icon",]
a <- a[a$field!="pageTitle",]
a <- a[a$field!="timeSpentPretty",]
a <- a[a$field!="serverTimePretty",]
a <- a[a$field!="pageId",]
a <- a[a$field!="generationTime",]
a <- a[a$field!="siteSearchKeyword",]
 
#remove duplicate rows:
dupl.rows <- duplicated(a)
print(paste("remove",length(dupl.rows),"duplicated rows in actions dataset, related to",
            length(unique(a[dupl.rows,]$idVisit)),"visits"))
a <- unique(a)
rm(dupl.rows)

#discard fake actions:
a$step <- as.numeric(a$step)+1
a <- merge(x = a, y = v[,c("idVisit","actions")], by = "idVisit", all.x = TRUE)
aa <- a[a$step<=a$actions,]
print(paste("discard",nrow(a)-nrow(aa),"fake actions"))
a <- aa
a$actions<-NULL
rm(aa)

#flag visits with incomplete actions:
D <- setNames(aggregate(a$idVisit,by=list(a$idVisit,a$step),FUN=length),
              c("idVisit","step","x"))
DD <- setNames(aggregate(D$idVisit,by=list(D$idVisit,D$x),function(z) max(z)-min(z)),
              c("idVisit","x","y"))
bad.visits <- DD[DD$y>0,]$idVisit
v$bad <- F
if (length(bad.visits)>0) {
  v[v$idVisit %in% bad.visits,]$bad <- T
  a <- a[!a$idVisit %in% bad.visits,]
}
print(paste(length(bad.visits),"visits flagged as incomplete"))
rm(bad.visits,D,DD)

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

# mm <- c("mean","last","randmean","randmedian","slr","harmonize")
mm <- c("harmonize")
for (m in mm) {
  print(paste(m,"timeSpent"))
  a <- altTimeSpent(m)
}
rm(mm)

a[a$field=="timeSpent" & a$value!=a$timeSpent.harmonize,]$value <-
  a[a$field=="timeSpent" & a$value!=a$timeSpent.harmonize,]$timeSpent.harmonize
a$timeSpent.harmonize <- NULL

#transpose the action variables:
require(tidyr)
print("transpose variables")
a <- spread(a,field,value)
a <- a[,c(1:3,7,8,10,11,4:6,9)]

#set null generationTime to 1ms
print(paste("set null generationTime to 1ms,",nrow(a[a$generationTimeMilliseconds=="",]),"visits"))
a$generationTimeMilliseconds[a$generationTimeMilliseconds==""] <- "0"

#export data:
f <- "os-visits+actions.RData"
save(v,a,file=f)
print(paste("cleaned data exported to",f))
rm(f)

print("* end cleaning individual data")

#end
