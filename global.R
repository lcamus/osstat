source("src/util.R")

loadPackages("rvest")
loadPackages("visNetwork")

#variables partagées
url_root <- "https://www.euro-area-statistics.org"
nodes <- data.frame()
edges <- data.frame()

nodes <- data.frame(id=0,label="homepage",group="global",value=1,title="home",url=url_root)
edges <- data.frame(from=0, to=0, label=NA, title="homepage")
edges <- data.frame(from=0, to=0, label=NA, title="langue")

homepage <- read_html(url_root)

# traitement du bloc des statistiques par domaine
h <- homepage %>% html_nodes("#container > div > div > h2") %>% html_text()
for (i in h) {
  id <- max(nodes$id)+1
  nodes <- rbind(nodes,data.frame(id,label=i,group="Indicators",value=1,title=i,url=NA))
  edges <- rbind(edges,data.frame(from=0, to=id,label=NA,title=NA))
}

h <- homepage %>% html_nodes("#container > div > div")
for (i in 1:length(h)) {
  nodes <- rbind(nodes,
                 data.frame(
                   id=sapply(seq_along(h[i] %>% html_nodes("a")),function(x){nrow(nodes)+x}),
                   label=sapply(h[i] %>% html_nodes("a") %>% html_text(),function(x){x}),
                   group="Indicators",
                   value=1,
                   title="title",
                   url=sapply(h[i] %>% html_nodes("a") %>% html_attrs(),function(x){x}))
                 )
  edges <- rbind(edges,
                 data.frame(
                   from=i,
                   to=sapply(seq_along(h[i] %>% html_nodes("a")),function(x){nrow(nodes)-x+1}),
                   label=NA,
                   title=NA)
  )
}

# Insights
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
nodes <- rbind(nodes,
               data.frame(
                 id=nrow(nodes)+1,
                 label="Insights",
                 group="Insights",
                 value=1,
                 title=paste0(nrow(nodes)+1,"Insights"),
                 url=sapply(h,FUN=function(x){x["href"]})
               )
)

#Bank's corner
h <- homepage %>% html_nodes("body > div.main-container.main > div > div > div:nth-child(4) a")
nodes <- rbind(nodes,
               data.frame(
                 id=nrow(nodes)+1,
                 label="Bank's corner",
                 group="Bank's corner",
                 value=1,
                 title="Bank's corner",
                 url=as.character(h %>% html_attrs()))
)
edges <- rbind(edges,
               data.frame(
                 from=0,
                 to=nrow(nodes),
                 label=NA,
                 title=NA)
)


visNetwork(nodes, edges, height="700px", width="100%") %>%
  visLegend()


