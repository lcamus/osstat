#init (get data)
load("data/d.RData")
a <- d[["Live"]][["getLastVisitsDetails:Actions"]]

# url duplicate regarding pageIdAction:
a.pia <- a[a$field=="pageIdAction",c(1:5)]
a.url <- a[a$field=="url",c(2:5)]
m <- merge(a.pia,a.url,by=c("idVisit","step"))
m <- aggregate(.~value.x+value.y,m,nrow)
m <- m[,c("value.x","value.y")]
m <- setNames(m,c("pageIdAction","url"))
mm <- aggregate(pageIdAction~url,m,FUN=length)
m <-merge(m,mm,by="url")
rm(mm)
m <- m[,c(2,1,3)]
m <- setNames(m,c("pageIdAction","url","freq.url"))

#extract
write.csv(m,"pageIdAction_url_duplicates.csv",row.names=F)
rm(m)
