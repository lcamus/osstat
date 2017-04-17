source("src/util.R")

loadPackages("rvest")
loadPackages("visNetwork")


#variables partagées :

base <- "https://utils.euro-area-statistics.org/admin/piwik/?module=API"
idSite <- "1"
format <- "CSV"
token_auth <- as.character(read.table("private/token",header=F)[,])

url_root <- "https://www.euro-area-statistics.org"
nodes <- data.frame()
edges <- data.frame()


# get site structure :
source("src/site.R")

#restitution :
visNetwork(nodes, edges, height="700px", width="100%") %>%
  visLegend()

# get data :
source("src/data.R")



