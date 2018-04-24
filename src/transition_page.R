#matrix transition pageIdAction

suppressPackageStartupMessages(require(dplyr))

pr2 <- pr
pr2$pg <- sub("^.+sdw.*\\.ecb\\.(europa\\.eu|int).+$","SDW",pr2$pg)
pr2$pg <- sub("^file:.+$","FILE",pr2$pg)
pr2$pg <- sub("^.+www.(ecb|bankingsupervision).europa.eu.+$","ECB",pr2$pg)

dn <- sort(pr$pageIdAction)
m <- matrix(0,nrow=nrow(pr)+1,ncol=nrow(pr)+1,dimnames=list(c(dn,"BEGIN"),c(dn,"END")))
rm(dn)
aa <- a[a$type!="search",]
aa$prev.a <- lag(aa$pageIdAction)
aa$next.a <- lead(aa$pageIdAction)

aa <- aa %>% group_by(idVisit) %>% mutate(prev.a=ifelse(row_number()==1,NA,prev.a))
aa <- aa %>% group_by(idVisit) %>% mutate(next.a=ifelse(row_number()==n(),NA,next.a))

apply(aa,1,function(x) {
  val.next <- x["next.a"]
  # val.next <- ifelse(val.next=="NA","END",val.next)
  val.next <- ifelse(is.na(val.next),"END",val.next)
  val.next <- gsub(" ","",val.next)
  pia <- gsub(" ","",x["pageIdAction"])
  m[pia,val.next] <<- m[pia,val.next]+1
})

# rm(aa)

require(visNetwork)

nodes <- data.frame(id=rownames(m),label=rownames(m))
edges <- data.frame(from=)
  
