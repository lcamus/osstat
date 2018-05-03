#matrix transition pageIdAction

suppressPackageStartupMessages(require(dplyr))

fSiteHierarchy <- "./data/refSiteHierarchy.RData"
url.root <- "https://www.euro-area-statistics.org/"

#refine repository page:

pr2 <- pr[pr$bad==F,]
pr2$bad <- NULL

pr2$pg <- sub("^.*sdw-wsrest\\.ecb\\.europa\\.eu/service/data/(\\w{2,3})/(.+)$","/bankscorner/\\1/sdw/\\2",pr2$pg)
pr2$pg <- sub("^.*sdw\\.ecb\\.europa\\.eu/datastructure.do$","/outlink/sdw/datastructure",pr2$pg)
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
pr2$pg <- sub("^.+/banks-corner-(\\w{2,3})/\\w{2,3}codelist\\.xlsx$","/bankscorner/\\1",pr2$pg)
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

aa <- aa %>% group_by(idVisit) %>% mutate(prev.a=ifelse(row_number()==1,"BEGIN",prev.a))
aa <- aa %>% group_by(idVisit) %>% mutate(next.a=ifelse(row_number()==n(),"END",next.a))

invisible(apply(aa,1,function(x) {
  val.prev <- x["prev.a"]
  val.next <- x["next.a"]
  # val.next <- ifelse(is.na(val.next),"END",val.next)
  val.next <- gsub(" ","",val.next)
  pia <- gsub(" ","",x["pg"])
  m[pia,val.next] <<- m[pia,val.next]+1
  # if(is.na(val.prev))
  if(val.prev=="BEGIN")
    m["BEGIN",pia] <<- m["BEGIN",pia]+1
}))

require(visNetwork)

getTitle <- function() {
  
  require(htmltools)
  
  getBouncing <- function(node.name,noderef.index) {
  
    # if (node.name=="BEGIN")
    #   bouncing <- "-"
    # else if (node.name=="END")
    #   bouncing <- "100%"
    # else {
    #   incoming <- sum(m[1:nrow(m),node.name])
    #   outcoming <- sum(m[node.name,1:ncol(m)-1])
    #   bouncing <- paste0(as.character(round((incoming-outcoming)/incoming*100,0)),"%")
    # }
    noderef.name <- c(colnames(m),"BEGIN")[noderef.index]
    if (noderef.name=="BEGIN")
      bouncing <- "-"
    else if (noderef.name=="END")
      bouncing <- "100%"
    else {
      incoming <- nrow(aa[aa$pg==noderef.name & aa$prev.a==node.name,])
      end.visit <- nrow(aa[aa$pg==noderef.name & aa$prev.a==node.name & aa$next.a=="END",])
      bouncing <- paste0(as.character(round(100*end.visit/incoming,0)),"%")
    }
    
    return(bouncing)
    
  }
  
  getNode <- function(node.index) {
    
    depth <- 3
    
    #incoming:
    if (node.index==dim(m)[2]+1) { #virtual node BEGIN
      incoming <- data.frame(matrix(c("-","",""))[,c(1,1,1)],stringsAsFactors=F)
    } else {
      incoming.node <- names(head(sort(m[,node.index],decreasing=T),depth))
      incoming.traffic <- m[incoming.node,node.index]
      incoming.bouncing <- sapply(incoming.node,function(x) getBouncing(x,node.index))
      incoming <- data.frame(incoming.node,incoming.traffic,incoming.bouncing,stringsAsFactors=F)
    }
    
    #outcoming:
    if (node.index==dim(m)[2]) { #virtual node END
      outcoming <- data.frame(matrix(c("-","",""))[,c(1,1,1)],stringsAsFactors=F)
    } else {
      if (node.index==dim(m)[2]+1) node.index <- dim(m)[1] #virtual node BEGIN 
      outcoming.node <- names(head(sort(m[node.index,],decreasing=T),depth))
      outcoming.traffic <- m[node.index,outcoming.node]
      outcoming.bouncing <- sapply(outcoming.node,function(x) getBouncing(x,node.index))
      outcoming <- data.frame(outcoming.node,outcoming.traffic,outcoming.bouncing,stringsAsFactors=F)
    }
    
    res <- as.list(apply(bind_cols(incoming,outcoming),1,function(x){
      tags$tr(lapply(x,tags$td))
    }))
    
    return(res)
    
  }
  
  incoming <- c(colSums(m),0)
  outcoming <- c(rowSums(m[1:nrow(m)-1,1:ncol(m)-1]),0,rowSums(m)[dim(m)[1]])
  bouncing <- round((incoming-outcoming)/incoming*100,0)
  bouncing <- sapply(bouncing,function(x){
    if (is.infinite(x))
      res <- "-"
    else
      res <- paste0(as.character(x),"%")
    return(res)
  })
  
  io <- c("incoming","outcoming")
  ntb <- c("node","traffic","bouncing")
  
  sketch <- lapply(1:(ncol(m)+1),function(x){
    htmltools::withTags(table(
      thead(
        tr(
          th(colspan=3,io[1]),
          th(colspan=3,io[2])
        ),
        tr(lapply(rep(ntb,2),th))
      ),
      tbody(
        getNode(x)
      )
    ))
  })
  
  # res <- paste0(c(gsub("/"," > ",sub("/$","",sub("^/","",colnames(m)))),"BEGIN"),
  #               "<br><br>incoming traffic ",incoming,
  #               "<br>outcoming traffic ",outcoming,
  #               "<br>bouncing rate ",bouncing)
  res <- lapply(seq_along(sketch),function(x){
    tags$div(
      tags$p(paste0(c(gsub("/"," > ",sub("/$","",sub("^/","",colnames(m)))),"BEGIN"))[x]),
      tags$p(paste("incoming traffic",incoming[x])),
      tags$p(paste("outcoming traffic",outcoming[x])),
      tags$p(paste("bouncing rate",bouncing[x])),
      sketch[[x]]
    )
  })
  
  res <- sapply(res,as.character)
  return(res)
  
} #getTitle

groups.order <- c("indicators","insights","bankscorner","shared","outlink","event")

nodes <- data.frame(id=c(colnames(m),"BEGIN"),
                    label=c(sub("^/(\\w{3})\\w+/",paste0("\\1","~"),colnames(m)),"BEGIN"),
                    value=c(colSums(m),rowSums(m)[dim(m)[1]]),
                    title=getTitle(),
                    stringsAsFactors=F)
nodes$group <- sub("^/(\\w+)/.*$","\\1",nodes$id)
nodes[nodes$id %in% c("BEGIN","END","ERR","local"),]$group <- "event"
nodes[nodes$group=="event",]$label <- paste0("eve~",nodes[nodes$group=="event",]$label)
nodes[nodes$id=="/homepage",c("group","label")] <- c("shared","sha~homepage")
nodes[nodes$id=="/bankscorner/",]$label <- "ban~bankscorner"
nodes <- nodes[unlist(lapply(groups.order,function(x) which(nodes$group %in% x))),]

edges <- setNames(data.frame(matrix(ncol=3, nrow=0),stringsAsFactors=F),c("from","to","value"))

invisible(mapply(function(r,c){
  if (m[r,c]!=0) edges[nrow(edges)+1,] <<- c(rownames(m)[r],colnames(m)[c],m[r,c])
},row(m),col(m)))

#display network:

groups <- data.frame(label=groups.order,
                     color=c("#6fb871","#5cbde3","#D9685E","#004996","darkorange","darkmagenta"),
                     desc=c("indicators","Insights into euro area statistics","Banks' Corner",
                            "shared pages and features (incl. homepage)",
                            "outlinks to external pages (institutional websites and web ressources)",
                            "events related to visit (begin, end, error and save to local)"
                            ),stringsAsFactors=F)
lnodes <- data.frame(label=groups$label,color=groups$color,shape="square",
                     title=groups$desc)

network <- visNetwork(nodes,edges,width="100%",
                      main=paste0("Our statistics network (from ",min(a$date)," to ",max(a$date),")")) %>%
  visLegend(main="group", useGroups=F,addNodes=lnodes) %>%
  visOptions(highlightNearest=list(enabled=T, degree=0),nodesIdSelection=T,
             selectedBy=list(variable="group",selected="indicators",values=groups.order)) %>%
  visInteraction(navigationButtons=T) %>%
  visPhysics(stabilization=F,solver="forceAtlas2Based")

invisible(apply(groups,1,function(x){
  network <<- network %>% visGroups(groupname=as.character(x[1]),color=as.character(x[2]))
}))

network

visSave(network, file = "network.html")

# rm(aa)


