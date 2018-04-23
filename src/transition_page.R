#matrix transition pageIdAction

suppressPackageStartupMessages(require(dplyr))
dn <- sort(pr$pageIdAction)
m <- matrix(0,nrow=nrow(pr)+1,ncol=nrow(pr)+1,dimnames=list(c(dn,"BEGIN"),c(dn,"END")))
rm(dn)
a$prev.a <- lag(a$pageIdAction)
a$next.a <- lead(a$pageIdAction)

a <- a %>% group_by(idVisit) %>% mutate(prev.a=ifelse(row_number()==1,NA,prev.a))
a <- a %>% group_by(idVisit) %>% mutate(next.a=ifelse(row_number()==n(),NA,next.a))

apply(a,1,function(x) {
  val.next <- ifelse(is.na(as.character(x$next.a)),"END",as.character(x$next.a))
  m[as.character(x$pageIdAction),val.next] <<- m[as.character(x$pageIdAction),val.next]+1
})

  
