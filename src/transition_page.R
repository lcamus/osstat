#matrix transition pageIdAction

suppressPackageStartupMessages(require(dplyr))
dn <- c(sort(pr$pageIdAction),NA)
m <- matrix(nrow=nrow(pr)+1,ncol=nrow(pr)+1,dimnames=list(dn,dn))
rm(dn)
a$prev.a <- lag(a$pageIdAction)
a$next.a <- lead(a$pageIdAction)

a <- a %>% group_by(idVisit) %>% mutate(prev.a=ifelse(row_number()==1,NA,prev.a))
a <- a %>% group_by(idVisit) %>% mutate(next.a=ifelse(row_number()==n(),NA,next.a))

lapply(a,function(x) m[x$pageIdAction,x$next.a] <<- m[x$pageIdAction,x$next.a]+1)
