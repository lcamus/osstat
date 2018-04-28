#matrix transition pageIdAction

suppressPackageStartupMessages(require(dplyr))

pr2 <- pr[pr$bad==F,]
pr2$pg <- sub("^.+sdw.*\\.ecb\\.(europa\\.eu|int).+$","SDW",pr2$pg)
pr2$pg <- sub("^file:.+$","(local)",pr2$pg)
pr2$pg <- sub("^.+(ecb|bankingsupervision).europa.eu.+$","ECB",pr2$pg)
pr2$pg <- sub("^.+trans(late)?.*$","(translate)",pr2$pg)
pr2$pg <- sub("^.*www\\.imf\\.org.*$","IMF",pr2$pg)
pr2$pg <- sub("^.*www\\.ebf-fbe\\.eu.*$","EBF",pr2$pg)
pr2$pg <- sub("^.+ec\\.europa\\.eu.*$","EC",pr2$pg)
pr2$pg <- sub("^.*www\\.youtube\\.com.*$","YouTube",pr2$pg)
pr2$pg <- sub("^.+sdmx\\.org.*$","SDMX.org",pr2$pg)
pr2$pg <- sub("^.+/insights-atom\\.xml$","(rss)",pr2$pg)
pr2$pg <- sub("^.+/banks-corner-\\w{2,3}/\\w{2,3}codelist\\.xlsx$","SDW",pr2$pg)
pr2$pg <- sub("^.*/embed.*$","(embed)",pr2$pg)
pr2$pg <- sub("^.+/data$","(data)",pr2$pg)
pr2$pg <- sub("^.*/www\\.oecd\\.org.*$","OECD",pr2$pg)
pr2$pg <- sub("^.*/www\\.compareyourcountry\\.org.*$","OECD",pr2$pg)

ncbs <- c("http://www.nbb.be/","http://www.bundesbank.de/","http://www.eestipank.ee/","http://www.centralbank.ie/","http://www.bankofgreece.gr/","http://www.bde.es/","http://www.banque-france.fr/","http://www.bancaditalia.it/","http://www.centralbank.gov.cy/","http://www.bank.lv/","http://www.lb.lt/","http://www.bcl.lu/","http://www.centralbankmalta.org/","http://www.dnb.nl/","http://www.oenb.at/","http://www.bportugal.pt/","http://www.bsi.si/","http://www.nbs.sk/","http://www.suomenpankki.fi")
invisible(lapply(ncbs,function(x){
  pr2$pg <<- sub(paste0(x,".*$"),"NCBs",pr2$pg)
}))

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

# nodes <- data.frame(id=rownames(m),label=rownames(m))
# edges <- data.frame(from=)
  
