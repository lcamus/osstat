source("src/util.R")

loadPackages("rvest")
loadPackages("visNetwork")


#variables partagées :

fdata <- "data/d.RData"
base <- "https://utils.euro-area-statistics.org/admin/piwik/?module=API"
idSite <- "1"
format <- "CSV"
token_auth <- as.character(read.table("private/token",header=F)[,])

if (exists(fdata)) {
  load(fdata)
} else {
  d <<- list(UserCountry=list(getCountry=data.frame(), getContinent=data.frame(), getRegion=data.frame(), getCity=data.frame()),
             UserLanguage=list(getLanguage=data.frame())
  )
}

#hideCol <- list(UserCountry=list(getCountry=c("nb_visits_converted"), getContinent=c("nb_visits_converted"), getRegion=c("nb_visits_converted")))
hideCol <- list()
fieldstoremove <- c("metadata_logo","metadata_logoWidth","metadata_logoHeight", "nb_visits_converted")  

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
save(d, file=fdata)


