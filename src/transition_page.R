#matrix transition pageIdAction

saveNetwork2html <- function (tl, file, selfcontained = T, libdir = "./lib") {
  
  if (is.null(libdir)) {
    libdir <- paste(tools::file_path_sans_ext(basename(file)), 
                    "_files", sep = "")
  }
  htmltools::save_html(tl, file = file, libdir = libdir)
  if (selfcontained) {
    if (!htmlwidgets:::pandoc_available()) {
      stop("Saving a widget with selfcontained = TRUE requires pandoc. For details see:\n", 
           "https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md")
    }
    htmlwidgets:::pandoc_self_contained_html(file, "output.html")
    unlink(libdir, recursive = TRUE)
  }
  return(htmltools::tags$iframe(src= file, height = "400px", width = "100%", style="border:0;"))
  
}

extendRepositoryPage <- function() {
  
  suppressPackageStartupMessages(require(dplyr))
  
  fSiteHierarchy <- "./data/refSiteHierarchy.RData"
  url.root <- "https://www.euro-area-statistics.org/"
  
  #refine repository page:
  
  pr2 <- pr[pr$bad==F,]
  pr2$bad <- NULL
  
  pr2$pg <- sub("^.*sdw-wsrest\\.ecb\\.europa\\.eu/service/data/(\\w{2,3})/(.+)$","/bankscorner/\\1/export/\\2",pr2$pg)
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
  
  f <- grep("^/bankscorner/\\w{2,3}/export/.+$",pr2$pg)
  pr2[f,]$args <- sub("^/bankscorner/\\w{2,3}/export/","",pr2[f,]$pg)
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
  
  return(pr2)
  
} #extendRepositoryPage

extendActions <- function(pr2) {
  
  aa <- a[a$type!="search",]
  aa <- left_join(aa,pr2[,c("pageIdAction","pg")],by="pageIdAction")
  aa[is.na(aa$pg),]$pg<- "ERR"
  aa$prev.a <- lag(aa$pg)
  aa$next.a <- lead(aa$pg)
  
  aa <- aa %>% group_by(idVisit) %>% mutate(prev.a=ifelse(row_number()==1,"BEGIN",prev.a))
  aa <- aa %>% group_by(idVisit) %>% mutate(next.a=ifelse(row_number()==n(),"END",next.a))
  
  return(aa)
  
} #extendActions
  
genTransitionMatrix <- function(pr2,aa) {
  
  #create network:
  
  dn <- sort(unique(pr2$pg))
  m <- matrix(0,nrow=length(dn)+2,ncol=length(dn)+2,dimnames=list(c(dn,"ERR","BEGIN"),c(dn,"ERR","END")))
  rm(dn)
  
  invisible(apply(aa,1,function(x) {
    val.prev <- x["prev.a"]
    val.next <- x["next.a"]
    val.next <- gsub(" ","",val.next)
    pia <- gsub(" ","",x["pg"])
    m[pia,val.next] <<- m[pia,val.next]+1
    if(val.prev=="BEGIN")
      m["BEGIN",pia] <<- m["BEGIN",pia]+1
  }))
  
  return(m)
  
} #genTransitionMatrix

setGroup <- function() {
  
  groups <- data.frame(label=c("indicators","insights","bankscorner","shared","outlink","event"),
                       color=c("#6fb871","#5cbde3","#D9685E","#004996","darkorange","darkmagenta"),
                       desc=c("indicators","Insights into euro area statistics","Banks' Corner",
                              "shared pages and features (incl. homepage)",
                              "outlinks to external pages (institutional websites and web ressources)",
                              "events related to visit (begin, end, error and save to local)"
                       ),stringsAsFactors=F)
  groups$color <- sapply(groups$color,function(x)paste0("#",paste(as.hexmode(col2rgb(x)),collapse="")))
  
  return(groups)
  
} #setGroup
groups <- setGroup()

getJSEventHandler <- function(e) {
  
  require(htmlwidgets)
  
  f <- paste0("./src/js/",e,".js")
  func <- JS(readChar(f, file.info(f)$size))
  return(func)
} #getJSEventHandler

genNetwork <- function(m) {
  
  require(visNetwork)
  
  getTitle <- function() {
    
    require(htmltools)
    
    getPageDesc <- function() {
      
    }
    
    getBouncing <- function(node.incoming,node.outcoming) {
      
      if (node.outcoming %in% c("BEGIN","END") | node.incoming=="END")
        bouncing <- "-"
      else {
        incoming <- nrow(aa[aa$pg==node.outcoming & aa$prev.a==node.incoming,])
        end.visit <- nrow(aa[aa$pg==node.outcoming & aa$prev.a==node.incoming & aa$next.a=="END",])
        bouncing <- round(100*end.visit/incoming,0)
        bouncing <- paste0(as.character(round(100*end.visit/incoming,0)),"%")
      }
      
      return(bouncing)
      
    }
    
    getNode <- function(node.index) {
      
      # depth <- ncol(m)
      depth <- 1
      
      #incoming:
      if (node.index==dim(m)[2]+1) { #virtual node BEGIN
        incoming <- data.frame(matrix(c("-","",""))[,c(1,1,1)],stringsAsFactors=F)
      } else {
        incoming.node <- names(head(sort(m[,node.index],decreasing=T),depth))
        incoming.traffic <- m[incoming.node,node.index]
        incoming.bouncing <- sapply(incoming.node,function(x) getBouncing(x,c(colnames(m),"BEGIN")[node.index]))
        incoming <- data.frame(incoming.node,incoming.traffic,incoming.bouncing,stringsAsFactors=F)
      }
      
      #outcoming:
      if (node.index==dim(m)[2]) { #virtual node END
        outcoming <- data.frame(matrix(c("-","",""))[,c(1,1,1)],stringsAsFactors=F)
      } else {
        if (node.index==dim(m)[2]+1) node.index <- dim(m)[1] #virtual node BEGIN 
        outcoming.node <- names(head(sort(m[node.index,],decreasing=T),depth))
        outcoming.traffic <- m[node.index,outcoming.node]
        outcoming.bouncing <- sapply(outcoming.node,function(x) getBouncing(c(colnames(m),"BEGIN")[node.index],x))
        outcoming <- data.frame(outcoming.node,outcoming.traffic,outcoming.bouncing,stringsAsFactors=F)
      }
      
      ntb <- c("node","freq","bouncing")
      
      require(DT)
      res <- list(
        datatable(outcoming,colnames=ntb,options=list(pageLength=5))
      )
      
      return(res)
      
    } #getNode
    
    incoming <- c(colSums(m),0)
    outcoming <- c(rowSums(m[1:nrow(m)-1,1:ncol(m)-1]),0,rowSums(m)[dim(m)[1]])
    bouncing <- round((incoming-outcoming)/incoming*100,0)
    bouncing <- sapply(seq_along(bouncing),function(x){
      if (is.infinite(bouncing[x]) | c(colnames(m),"BEGIN")[x]=="END")
        res <- "-"
      else
        res <- paste0(as.character(bouncing[x]),"%")
      return(res)
    })
    
    sketch <- lapply(1:(ncol(m)+1),function(x){
      getNode(x)
    })
    
    res <- lapply(seq_along(sketch),function(x){
      lib <- paste0(gsub("/"," > ",sub("/$","",sub("^/","",c(colnames(m),"BEGIN")[x]))))
      return(htmltools::withTags(div(
        h3(lib),
        table(
          tr(td("incoming traffic"),td(incoming[x],class="figure")),
          tr(td("outcoming traffic"),td(outcoming[x],class="figure")),
          tr(td("bouncing rate"),td(bouncing[x],class="figure"))
        ))
      ))
    })
    
    res <- sapply(res,as.character)
    return(res)
    
  } #getTitle
  
  setNodes <- function() {
    nodes <- data.frame(id=c(colnames(m),"BEGIN"),
                        # label=c(sub("^/(\\w{3})\\w+/",paste0("\\1","~"),colnames(m)),"BEGIN"),
                        label=c(sub("^/\\w+/","",colnames(m)),"BEGIN"),
                        value=c(colSums(m),rowSums(m)[dim(m)[1]]),
                        title=getTitle(),
                        stringsAsFactors=F)
    nodes$group <- sub("^/(\\w+)/.*$","\\1",nodes$id)
    nodes[nodes$id %in% c("BEGIN","END","ERR","local"),]$group <- "event"
    # nodes[nodes$group=="event",]$label <- paste0("eve~",nodes[nodes$group=="event",]$label)
    # nodes[nodes$id=="/homepage",c("group","label")] <- c("shared","sha~homepage")
    nodes[nodes$id=="/homepage",]$group <- "shared"
    # nodes[nodes$id=="/bankscorner/",]$label <- "ban~bankscorner"
    nodes <- nodes[unlist(lapply(groups$label,function(x) which(nodes$group %in% x))),]
    return(nodes)
  } #setNodes
  
  nodes <- setNodes()
  
  edges <- setNames(data.frame(matrix(ncol=3, nrow=0),stringsAsFactors=F),c("from","to","value"))
  invisible(mapply(function(r,c){
    if (m[r,c]!=0) edges[nrow(edges)+1,] <<- c(rownames(m)[r],colnames(m)[c],m[r,c])
  },row(m),col(m)))
  
  #
  
  network <- visNetwork(nodes,edges,
                        main="Our statistics network",
                        submain=paste0("(from ",min(a$date)," to ",max(a$date),")")) %>%
    visLegend(main="group", useGroups=F,
              addNodes=data.frame(label=groups$label,color=groups$color,shape="square",
                                  title=groups$desc)) %>%
    visOptions(highlightNearest=list(enabled=T, degree=0),
               nodesIdSelection=list(enabled=T,useLabels=F),
               # selectedBy=list(variable="group",selected="indicators",values=groups$label)) %>%
               selectedBy=list(variable="group",values=groups$label)) %>%
    visInteraction(navigationButtons=T,hover=T,
                   tooltipStyle = '
                     position: fixed;
                     visibility: hidden;
                     white-space: pre-wrap;
                     background-color: #ffffca;
                     padding: 15px;
                     z-index: 1;
                     border-radius: 30px;') %>%
    visPhysics(stabilization=F,solver="forceAtlas2Based") %>%
    visNodes(font=list(strokeWidth=1)) %>%
    visEvents(hoverNode=getJSEventHandler("hoverNode"),
              blurNode=getJSEventHandler("blurNode"),
              selectNode=getJSEventHandler("selectNode"),
              stabilized=getJSEventHandler("stabilized"),
              startStabilizing="function(e){window.network=this;}"
              # startStabilizing=getJSEventHandler("startStabilizing")
    )
  
  invisible(apply(groups,1,function(x){
    assign("network",
           network %>% visGroups(groupname=as.character(x[1]),color=as.character(x[2])),
           envir=parent.env(environment()))
  }))
  
  return(network)
  
} #genNetwork

genSrcDatatables <- function(m,nodes.ref) {
  
  suppressPackageStartupMessages(require(dplyr))
  
  getBouncing <- function(node.incoming,node.outcoming) {
    
    if (node.outcoming %in% c("BEGIN","END") | node.incoming=="END")
      bouncing <- "-"
    else {
      incoming <- nrow(aa[aa$pg==node.outcoming & aa$prev.a==node.incoming,])
      end.visit <- nrow(aa[aa$pg==node.outcoming & aa$prev.a==node.incoming & aa$next.a=="END",])
      bouncing <- round(100*end.visit/incoming,0)
      bouncing <- paste0(as.character(round(100*end.visit/incoming,0)),"%")
    }
    
    return(bouncing)
    
  }
  
  getNode <- function(node.index) {
    
    depth <- ncol(m)
    node.id <- ifelse(node.index==dim(m)[2]+1,"BEGIN",colnames(m)[node.index])
    
    #incoming:
    if (node.index==dim(m)[2]+1) { #virtual node BEGIN
      incoming <- data.frame(matrix(c(node.id,"event",0,rep("-",3)),nrow=1),stringsAsFactors=F)
    } else {
      incoming.node <- names(head(sort(m[,node.index],decreasing=T),depth))
      incoming.traffic <- m[incoming.node,node.index]
      incoming.bouncing <- sapply(incoming.node,function(x) getBouncing(x,c(colnames(m),"BEGIN")[node.index]))
      incoming <- data.frame(rep(node.id,depth),
                             left_join(setNames(data.frame(matrix(incoming.node,ncol=1),stringsAsFactors=F),"id"),nodes.ref,by="id")$group,
                             1:depth,
                             sub("^[a-z]+/(.+)$","\\1",sub("^/(.+)","\\1",incoming.node)),
                             as.character(incoming.traffic),
                             incoming.bouncing,
                             stringsAsFactors=F)
    }
    
    #outcoming:
    if (node.index==dim(m)[2]) { #virtual node END
      outcoming <- data.frame(matrix(c(node.id,"event",0,rep("-",3)),nrow=1),stringsAsFactors=F)
    } else {
      if (node.index==dim(m)[2]+1) node.index <- dim(m)[1] #virtual node BEGIN 
      outcoming.node <- names(head(sort(m[node.index,],decreasing=T),depth))
      outcoming.traffic <- m[node.index,outcoming.node]
      outcoming.bouncing <- sapply(outcoming.node,function(x) getBouncing(c(colnames(m),"BEGIN")[node.index],x))
      outcoming <- data.frame(rep(node.id,depth),
                              left_join(setNames(data.frame(matrix(outcoming.node,ncol=1),stringsAsFactors=F),"id"),nodes.ref,by="id")$group,
                              1:depth,
                              sub("^[a-z]+/(.+)$","\\1",sub("^/(.+)","\\1",outcoming.node)),
                              as.character(outcoming.traffic),
                              outcoming.bouncing,
                              stringsAsFactors=F)
    }
    
    rnfb <- c("ref","group","rank","node","freq","bouncing")
    
    incoming[,3] <- as.numeric(incoming[,3]) #rank
    outcoming[,3] <- as.numeric(outcoming[,3])
    
    res <- list(
      setNames(incoming[incoming[,5]!="0",],rnfb),
      setNames(outcoming[outcoming[,5]!="0",],rnfb)
    )
    
    return(res)
    
  } #getNode
  
  gotnodes <- lapply(1:(ncol(m)+1),function(x){
    getNode(x)
  })
  
  res <- list(
    bind_rows(lapply(gotnodes,`[[`,1)),
    bind_rows(lapply(gotnodes,`[[`,2))
  )
  return(res)
  
} #genSrcDatatables

genDatatables <- function(t,cap) {
  require(DT)
  require(htmltools)
  
  res <- datatable(t,rownames=F,height=250,width=600,fillContainer=F,autoHideNavigation=T,
                   filter="none",class="compact",
                   caption=htmltools::tags$caption(
                     htmltools::em(cap)
                   ),
                   options=list(pageLength=5,dom = 'tpr',autoWidth=T,
                                columnDefs = list(list(visible=F, searchable=T, targets=0),
                                                  list(searchable=F,targets=c(1:5)),
                                                  list(width = '10px', targets=c(1,2)),
                                                  list(width = '200px', targets=3),
                                                  list(width = '25px', targets=c(4,5)),
                                                  list(
                                                    targets=3,
                                                    render = JS(
                                                      "function(data, type, row, meta) {",
                                                      "return type === 'display' && data.length > 25 ?",
                                                      "'<span title=\"' + data + '\">' + data.substr(0,25) + '...</span>' : data;",
                                                      "}")),
                                                  list(className = 'dt-center', targets=c(2,4,5))
                                )
                   )) %>%
    formatStyle(
      "group",
      backgroundColor=styleEqual(groups$label,groups$color),
      color=styleEqual(groups$label,groups$color)
    )
  
  return(res)
  
} #genDatatables

displayNetwork <- function(n,t) {
  
  require(visNetwork)
  require(htmltools)
  
  # browsable(
  withTags(tagList(list(
      tags$html(
      tags$head(
          tags$style('td.figure {font-weight:bold; text-align: center;}
                     * {font-family: "Century Gothic", CenturyGothic, AppleGothic, sans-serif !important;}'
                     ),
          tags$script(HTML(getJSEventHandler("selectNodeById")))
        )
      ,
        tags$body(
          tags$table(
            tags$tr(
              tags$td(n,rowspan=2,width="70%",valign="top")
              ,
              tags$td(t[[1]],width="30%")
            )
            ,
            tags$tr(
              tags$td(t[[2]],width="30%")
            )
          )
      )
      )
    )))
  # )
  
} #displayNetwork

pr2 <- extendRepositoryPage()
save(pr2,file="./data/pr2.RData")
aa <- extendActions(pr2)
m <- genTransitionMatrix(pr2,aa)
n <- genNetwork(m)
sd <- genSrcDatatables(m,n$x$nodes)
tbl <- lapply(seq_along(sd),function(x)genDatatables(sd[[x]],c("Incoming","Outcoming")[x]))

res <- displayNetwork(n,tbl)

browsable(res)

save_html(res,"network.html")

