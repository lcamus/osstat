#matrix transition pageIdAction

suppressPackageStartupMessages(require(dplyr))

fSiteHierarchy <- "./data/refSiteHierarchy.RData"
url.root <- "https://www.euro-area-statistics.org/"

#refine repository page:

pr2 <- pr[pr$bad==F,]
pr2$bad <- NULL

pr2$pg <- sub("^.*sdw-wsrest\\.ecb\\.europa\\.eu/service/data/(\\w{2,3})/(.+)$","/bankscorner/\\1/sdw/\\2",pr2$pg)
pr2$pg <- sub("^.*sdw\\.ecb\\.europa\\.eu/datastructure.do$","/bankscorner/sdw/datastructure",pr2$pg)
pr2$pg <- sub("^.*sdw\\.ecb\\.(europa\\.eu|int)/(\\w+)?(\\.do)?$","/outlink/sdw/\\2",pr2$pg)
pr2$pg <- sub("^file:.+$","local",pr2$pg)
pr2$pg <- sub("^.*www\\.(\\w+\\.)?(ecb|bankingsupervision)\\.europa\\.eu/.*$","/outlink/ECB",pr2$pg)
pr2$pg <- sub("^.+trans(late)?.*$","/shared/translate",pr2$pg)
pr2$pg <- sub("^.*www\\.imf\\.org.*$","/outlink/IMF",pr2$pg)
pr2$pg <- sub("^.*www\\.ebf-fbe\\.eu.*$","/outlink/EBF",pr2$pg)
pr2$pg <- sub("^.+ec\\.europa\\.eu.*$","/outlink/EC",pr2$pg)
pr2$pg <- sub("^.*www\\.youtube\\.com.*$","/outlink/YouTube",pr2$pg)
pr2$pg <- sub("^.+sdmx\\.org.*$","/outlink/SDMX.org",pr2$pg)
pr2$pg <- sub("^.+/insights-atom\\.xml$","/shared/rss",pr2$pg)
pr2$pg <- sub("^.*/embed.*$","/shared/embed",pr2$pg)
pr2$pg <- sub("^.*/data$","/shared/data",pr2$pg)
pr2$pg <- sub("^.*/www\\.oecd\\.org.*$","/outlink/OECD",pr2$pg)
pr2$pg <- sub("^.*/www\\.compareyourcountry\\.org.*$","/outlink/OECD",pr2$pg)
pr2$pg <- sub("^/classic/banks-corner$","/bankscorner/",pr2$pg)
pr2$pg <- sub("^.+/banks-corner-(\\w{2,3})/\\w{2,3}codelist\\.xlsx$","/bankscorner/\\1/sdw/datastructure",pr2$pg)
pr2$pg <- sub("^(/classic)?/banks-corner-(\\w{2,3})$","/bankscorner/\\2",pr2$pg)
pr2$pg <- sub("^/classic/(.+)$","/insights/\\1",pr2$pg)
pr2$pg <- sub("^/((\\w|-)+)$","/indicators/\\1",pr2$pg)
pr2$pg <- sub("^/$","/homepage",pr2$pg)

ncbs <- c("http://www.nbb.be/","http://www.bundesbank.de/","http://www.eestipank.ee/","http://www.centralbank.ie/","http://www.bankofgreece.gr/","http://www.bde.es/","http://www.banque-france.fr/","http://www.bancaditalia.it/","http://www.centralbank.gov.cy/","http://www.bank.lv/","http://www.lb.lt/","http://www.bcl.lu/","http://www.centralbankmalta.org/","http://www.dnb.nl/","http://www.oenb.at/","http://www.bportugal.pt/","http://www.bsi.si/","http://www.nbs.sk/","http://www.suomenpankki.fi")
invisible(lapply(ncbs,function(x){
  pr2$pg <<- sub(paste0(x,".*$"),"/outlink/NCBs",pr2$pg)
}))

f <- grep("^/bankscorner/\\w{2,3}/sdw/.+$",pr2$pg)
pr2[f,]$args <- sub("^/bankscorner/\\w{2,3}/sdw/","",pr2[f,]$pg)
pr2[f,]$pg <- strsplit(pr2[f,]$pg,"/(\\w|\\.|\\+)+$")
rm(f)

pr2$pg <- tolower(pr2$pg)

#merge identical (functional) pages:
pages.to.merge <- list(c(26143,27926),c(26149,27854),c(70275,69417))
invisible(lapply(pages.to.merge,function(x){
  a[which(a$pageIdAction==x[2]),]$pageIdAction <<- x[1]
  invisible(lapply(c("n","n.sum"),function(y){
    pr2[pr2$pageIdAction==x[1],y] <<- pr2[pr2$pageIdAction==x[1],y] + pr2[pr2$pageIdAction==x[2],y]
  }))
  pr2 <<- pr2[pr2$pageIdAction!=x[2],]
}))
rm(pages.to.merge)

#reflect site hierarchy:
if (file.exists(fSiteHierarchy))
  load(fSiteHierarchy) else
  {
    require("rvest")
    h <- read_html(url.root) %>%
      html_nodes("body > section:nth-child(3) > div:nth-child(1) > div > ul") %>% html_children()
    refSiteHierarchy <- setNames(data.frame(matrix(ncol = 3, nrow = 0),stringsAsFactors=F),
                                 c("parent","child.pg","child.lib"))
    invisible(lapply(h,function(x){
      parent <- html_children(x)[1] %>% html_text() %>% tolower() %>% gsub(pattern=" ",replacement="-")
      children <- html_children(x)[2] %>% html_children()
      invisible(lapply(children,function(y){
        child.pg <- y %>% html_children() %>% html_attr(name="href") %>% strsplit(split="?",fixed=T) %>% unlist() %>% head(1)
        child.lib <-y %>% html_children() %>% html_text() %>% gsub(pattern=" ",replacement="-")
        refSiteHierarchy[nrow(refSiteHierarchy)+1,] <<- c(parent,child.pg,child.lib)
      }))
    }))
    refSiteHierarchy$child.pg <- tolower(paste("/indicators",refSiteHierarchy$child.pg,sep="/"))
    refSiteHierarchy$child.path <- tolower(paste("/indicators",refSiteHierarchy$parent,refSiteHierarchy$child.lib,sep="/"))
    save(refSiteHierarchy,file=fSiteHierarchy)
    rm(url.root,fSiteHierarchy)
  }
pr2 <- left_join(pr2,refSiteHierarchy[,c("child.pg","child.path")],by=c("pg"="child.pg"))
pr2[!is.na(pr2$child.path),]$pg <- pr2[!is.na(pr2$child.path),]$child.path
pr2$child.path <- NULL

#update sum group:
pr2 <- pr2 %>% group_by(pg) %>% mutate(n.sum=sum(n))

#create network:

dn <- sort(unique(pr2$pg))
m <- matrix(0,nrow=length(dn)+2,ncol=length(dn)+2,dimnames=list(c(dn,"ERR","BEGIN"),c(dn,"ERR","END")))
rm(dn)

aa <- a[a$type!="search",]
aa <- left_join(aa,pr2[,c("pageIdAction","pg")],by="pageIdAction")
aa[is.na(aa$pg),]$pg<- "ERR"
aa$prev.a <- lag(aa$pg)
aa$next.a <- lead(aa$pg)

aa <- aa %>% group_by(idVisit) %>% mutate(prev.a=ifelse(row_number()==1,NA,prev.a))
aa <- aa %>% group_by(idVisit) %>% mutate(next.a=ifelse(row_number()==n(),NA,next.a))

invisible(apply(aa,1,function(x) {
  val.prev <- x["prev.a"]
  val.next <- x["next.a"]
  val.next <- ifelse(is.na(val.next),"END",val.next)
  val.next <- gsub(" ","",val.next)
  pia <- gsub(" ","",x["pg"])
  m[pia,val.next] <<- m[pia,val.next]+1
  if(is.na(val.prev))
    m["BEGIN",pia] <<- m["BEGIN",pia]+1
}))

require(visNetwork)

getTitle <- function() {
  
  incoming <- c(colSums(m),0)
  outcoming <- c(rowSums(m)-m[,dim(m)[1]],tail(rowSums(m),1),rowSums(m)[dim(m)[1]])
  bouncing <- round((incoming-outcoming)/incoming*100,0)
  bouncing <- sapply(bouncing,function(x){
    if (is.infinite(x))
      res <- "-"
    else
      res <- paste0(as.character(x),"%")
    return(res)
  })
  
  res <- paste0(c(gsub("/"," > ",sub("^/","",colnames(m))),"BEGIN"),
         "<br><br>incoming traffic ",incoming,
         "<br>outcoming traffic ",outcoming,
         "<br>bouncing rate ",bouncing)
  return(res)
}

nodes <- data.frame(id=c(colnames(m),"BEGIN"),
                    label=c(sub("^/\\w+/","",colnames(m)),"BEGIN"),
                    value=c(colSums(m),rowSums(m)[dim(m)[1]]),
                    title=getTitle(),
                    stringsAsFactors=F)
nodes$group <- sub("^/(\\w+)/.*$","\\1",nodes$id)
nodes[nodes$id %in% c("BEGIN","END","ERR","local"),]$group <- "event"
nodes[nodes$id=="/homepage",]$group <- "shared"

edges <- setNames(data.frame(matrix(ncol=3, nrow=0),stringsAsFactors=F),c("from","to","value"))

invisible(mapply(function(r,c){
  if (m[r,c]!=0) edges[nrow(edges)+1,] <<- c(rownames(m)[r],colnames(m)[c],m[r,c])
},row(m),col(m)))

#display network:

groups <- data.frame(label=c("indicators","insights","bankscorner","shared","outlink","event"),
                     color=c("#6fb871","#5cbde3","#D9685E","#004996","darkorange","darkmagenta")
                     ,stringsAsFactors=F)
lnodes <- data.frame(label=groups$label,color=groups$color)

network <- visNetwork(nodes,edges,width="100%",
                      main=paste0("Our statistics network (from ",min(a$date)," to ",max(a$date),")")) %>%
  visLegend(main="group", useGroups=F,addNodes=lnodes) %>%
  visOptions(highlightNearest=list(enabled=T, degree=0),nodesIdSelection=T,
             selectedBy=list(variable="group",multiple=T,selected="indicators")) %>%
  visInteraction(navigationButtons=T) %>%
  visPhysics(stabilization=F,solver="forceAtlas2Based")

invisible(apply(groups,1,function(x){
  network <<- network %>% visGroups(groupname=as.character(x[1]),color=as.character(x[2]))
}))

network

visSave(network, file = "network.html")

# rm(aa)


