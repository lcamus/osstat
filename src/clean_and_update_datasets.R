
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
var.useless <- c(names(var.useless),"countryFlag")
cat(paste("discard following variables in visits dataset:\n",paste(var.useless,collapse=", ")))
cat("\n")
v[,var.useless] <- NULL
rm(var.useless)

#remove useless variables for actions dataset:
a <- a[a$field!="icon",]
a <- a[a$field!="pageTitle",]
a <- a[a$field!="timeSpentPretty",]
a <- a[a$field!="serverTimePretty",]
a <- a[a$field!="pageId",]
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

#transpose the action variables:
require(tidyr)
print("transpose variables")
a <- spread(a,field,value)
a <- a[,c(1:3,7,8,10,11,4:6,9)]

#remove last visit action when NA-action:
var.na <- names(a)[!names(a) %in% c("date","idVisit","step","type")]
a.na <- which(!rowSums(!is.na(a[,var.na])))
rm(var.na)
require(dplyr)
v.na <- inner_join(a[a.na,],v[,c("idVisit","actions")],"idVisit")
v.na <- v.na[v.na$step==v.na$actions,]$idVisit
#discard NA-actions:
a <- a[-a.na,]
#update visits:
actions.na <- a[a$idVisit %in% v.na,] %>% group_by(idVisit) %>% summarise(actions.na=n())
v <- left_join(v,actions.na,by="idVisit")
v[!is.na(v$actions.na),]$actions <- v[!is.na(v$actions.na),]$actions.na
v[v$idVisit %in% v.na,]$bad <- T
print(paste(length(a.na),"last actions as NA-actions discarded"))
rm(a.na,v.na,actions.na)

# harmonize timeSpent for last action-visit to 1ms
require(dplyr)
a.na <- inner_join(a[is.na(a$timeSpent),],v[,c("idVisit","actions")],"idVisit")
a.na <- a.na[a.na$step==a.na$actions,]
a.na$timeSpent <- "1"
a <- left_join(a,
               setNames(a.na[,c("idVisit","step","timeSpent")],c("idVisit","step","timeSpent.y")),
               by=c("idVisit","step"))
a[is.na(a$timeSpent),]$timeSpent <- a[is.na(a$timeSpent),]$timeSpent.y
a$timeSpent.y <- NULL
print(paste("harmonize timeSpent for tail action to 1 ms, updated",nrow(a.na),"visits"))
rm(a.na)

#processed generationTime
#set null to 1ms
print(paste("set null generationTime to 1ms,",nrow(a[a$generationTimeMilliseconds=="",]),"visits"))
a$generationTimeMilliseconds[a$generationTimeMilliseconds==""] <- "0"
#convert to numeric:
a$generationTimeMilliseconds <- as.numeric(a$generationTimeMilliseconds)
#replace missing values by average:
gt <- a %>% group_by(pageIdAction) %>% summarise(gt.avg=mean(generationTimeMilliseconds,na.rm=T))
a <- left_join(a,gt,by="pageIdAction")
a[is.na(a$generationTimeMilliseconds),]$generationTimeMilliseconds <-
  a[is.na(a$generationTimeMilliseconds),]$gt.avg
a$gt.avg <- NULL
#remove pretty variable (useless for recent period):
a$generationTime <- NULL
rm(gt)

#export data:
f <- paste0("os-visits+actions_",head(sort(a$date),1),"_",tail(sort(a$date),1),".RData")
save(v,a,file=f)
print(paste("cleaned data exported to",f))
rm(f)

print("* end cleaning individual data")

#end
