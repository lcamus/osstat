source("src/util.R")

loadPackages("rvest")
loadPackages("visNetwork")

#variables partagées
url_root <- "https://www.euro-area-statistics.org"
nodes <- data.frame()
edges <- data.frame()

nodes <- data.frame(id=0,label="homepage",group="ext",value=1,title="homepage",url=url_root)
edges <- data.frame(from=0, to=0, label=NA, title=NA)

homepage <- read_html(url)

# traitement du bloc des statistiques par domaine
h <- homepage %>% html_nodes("#container > div > div > h2") %>% html_text()
for (i in h) {
  id <- max(nodes$id)+1
  nodes <- rbind(nodes,data.frame(id,label=i,group="int",value=1,title=i,url=NA))
  edges <- rbind(edges,data.frame(from=0, to=id,label=NA,title=NA))
}

h <- homepage %>% html_nodes("#container > div > div")
for (i in 1:length(h)) {
  nodes <- rbind(nodes,
                 data.frame(
                   id=sapply(seq_along(h[i] %>% html_nodes("a")),function(x){nrow(nodes)+x}),
                   label=sapply(h[i] %>% html_nodes("a") %>% html_text(),function(x){x}),
                   group="ext",
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
nodes <- rbind(nodes,
               data.frame(
                 id=nrow(nodes)+1,
                 label="Insights",
                 group="ext",
                 value=1,
                 title="Insights",
                 url=as.character(h %>% html_attrs()))
)
edges <- rbind(edges,
               data.frame(
                 from=0,
                 to=nrow(nodes),
                 label=NA,
                 title=NA)
)

#Bank's corner
h <- homepage %>% html_nodes("body > div.main-container.main > div > div > div:nth-child(4) a")
nodes <- rbind(nodes,
               data.frame(
                 id=nrow(nodes)+1,
                 label="Bank's corner",
                 group="ext",
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

visNetwork(nodes, edges)  

# sub_indicators <- homepage %>% html_nodes("#container > div > div li") %>% html_text()
# sub_indicators_lib <- setNames(sapply(sub_indicators,function(x) gsub("^[ ,\t,\n]+|[ ,\t,\n]+$","",x)),NULL)
# sub_indicators_ref <- sub("&lg=.*$","",
#                           homepage %>% html_nodes("#container > div > div li a") %>% html_attr("href"))
