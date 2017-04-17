#---
#get structure of the website
#---

nodes <- data.frame(id=0,label="homepage",group="global",value=1,title="home",url=url_root)
edges <- data.frame(from=0, to=0, label=NA, title="homepage")
edges <- data.frame(from=0, to=0, label=NA, title="langue")

homepage <- read_html(url_root)


# Indicators :

h <- homepage %>% html_nodes("#container > div > div > h2") %>% html_text()
for (i in h) {
  id <- max(nodes$id)+1
  nodes <- rbind(nodes,data.frame(id,label=i,group="Indicators",value=1,title=i,url=NA))
  edges <- rbind(edges,data.frame(from=0, to=id,label=NA,title=NA))
}

h <- homepage %>% html_nodes("#container > div > div")
getId <- function(x) {nrow(nodes)+x}
getLabel <- function(x) {x %>% html_text()}
getUrl <- function(x) {x %>% html_attrs()}
for (i in 1:length(h)) {
  nodes <- rbind(nodes,
                 data.frame(
                   id=sapply(seq_along(h[i] %>% html_nodes("a")),function(x){getId(x)}),
                   label=sapply(h[i] %>% html_nodes("a"),function(x){getLabel(x)}),
                   group="Indicators",
                   value=1,
                   title=sapply(seq_along(h[i] %>% html_nodes("a")),function(x){paste0("#",getId(x),"<br>",
                                                                                       getLabel((h[i] %>% html_nodes("a"))[x]),"<br>",
                                                                                       getUrl((h[i] %>% html_nodes("a"))[x]))}),
                   url=sapply(h[i] %>% html_nodes("a"),function(x){getUrl(x)})
                 )
  )
  edges <- rbind(edges,
                 data.frame(
                   from=i,
                   to=sapply(seq_along(h[i] %>% html_nodes("a")),function(x){nrow(nodes)-x+1}),
                   label=NA,
                   title=NA)
  )
}


# Insights :

h <- homepage %>% html_nodes("body > div.main-container.main > div > div > div:nth-child(3) a")
url_insights <- as.character(h %>% html_attrs())
nodes <- rbind(nodes,
               data.frame(
                 id=nrow(nodes)+1,
                 label="Insights",
                 group="Insights",
                 value=1,
                 title=paste0(nrow(nodes)+1,"Insights"),
                 url=url_insights)
)
edges <- rbind(edges,
               data.frame(
                 from=0,
                 to=nrow(nodes),
                 label=NA,
                 title=NA)
)

insights <- read_html(paste(url_root,url_insights,sep="/"))
h <- insights %>% html_nodes("#ieas > aside > div.insights > ul li a") %>% html_attrs()
getUrl <- function(x) {x["href"]}
getLabel <- function(x) {x["title"]}
nodes <- rbind(nodes,
               data.frame(
                 id=sapply(seq_along(h),function(x){getId(x)}),
                 label=sapply(h,function(x){getLabel(x)}),
                 group="Insights",
                 value=1,
                 title=sapply(seq_along(h),function(x){paste0("#",getId(x),"<br>",getLabel(h[[x]]),"<br>",getUrl(h[[x]]))}),
                 url=sapply(h,function(x){getUrl(x)})
               )
)
edges <- rbind(edges,
               data.frame(
                 from=nrow(nodes)-length(h),
                 to=sapply(seq_along(h),function(x){nrow(nodes)-length(h)+x}),
                 label=NA,
                 title=NA)
)


#Bank's corner :

h <- homepage %>% html_nodes("body > div.main-container.main > div > div > div:nth-child(4) a")
url_bankscorner <- (h %>% html_attrs())[[1]]["href"]
nodes <- rbind(nodes,
               data.frame(
                 id=nrow(nodes)+1,
                 label="Bank's corner",
                 group="Bank's corner",
                 value=1,
                 title=paste0("#",id,"<br>Bank's corner<br>",url_bankscorner),
                 url=url_bankscorner
               )
)
edges <- rbind(edges,
               data.frame(
                 from=0,
                 to=nrow(nodes),
                 label=NA,
                 title=NA)
)

bankscorner <- html_session(paste(url_root,url_bankscorner,sep="/"))
u <- bankscorner$url
h <- bankscorner %>% html_nodes("#mainText > p:nth-child(2) a")
getUrl <- function(x) {
  suffixe <- strsplit(x,"../")[[1]]
  t <- strsplit(gsub(url_root,"",u),"/")[[1]]
  t <- t[2:(length(t)-1)]
  t <- head(t,length(t)-(length(suffixe)-1))
  slash <- paste(rep("/",length(t)),collapse="")
  return(paste0(slash,t,slash,tail(suffixe,1)))
}
nodes <- rbind(nodes,
               data.frame(
                 id=sapply(seq_along(h),function(x){getId(x)}),
                 label=sapply(h %>% html_text(),function(x){x}),
                 group="Bank's corner",
                 value=1,
                 title=sapply(seq_along(h),function(x){paste0("#",getId(x),"<br>",
                                                              (h %>% html_text())[x],"<br>",
                                                              getUrl((h %>% html_attrs())[[x]])
                 )}),
                 url=sapply(h %>% html_attrs(),function(x){getUrl(x)})
               )
)
edges <- rbind(edges,
               data.frame(
                 from=nrow(nodes)-length(h),
                 to=(nrow(nodes)-length(h)):nrow(nodes),
                 label=NA,
                 title=NA)
)
