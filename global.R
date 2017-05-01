source("src/util.R")

loadPackages("rvest")
loadPackages("visNetwork")


#variables partagées :

fdata <- "data/d.RData"
base <- "https://utils.euro-area-statistics.org/admin/piwik/?module=API"
idSite <- "1"
format <- "CSV"
token_auth <- as.character(read.table("private/token",header=F)[,])

if (!exists("d") & file.exists(fdata)) {
  load(fdata)
} else if (!exists("d")) {
  d <<- list(UserCountry=list(getCountry=data.frame(), getContinent=data.frame(), getRegion=data.frame(), getCity=data.frame()),
             UserLanguage=list(getLanguage=data.frame()),
             VisitFrequency=list(get=data.frame()),
             VisitTime=list(getVisitInformationPerLocalTime=data.frame(), getVisitInformationPerServerTime=data.frame(),
                            getByDayOfWeek=data.frame()),
             VisitorInterest=list(getNumberOfVisitsPerVisitDuration=data.frame(), getNumberOfVisitsPerPage=data.frame(),
                                  getNumberOfVisitsByDaysSinceLast=data.frame(), getNumberOfVisitsByVisitCount=data.frame()),
             VisitsSummary=list(get=data.frame(), getVisits=data.frame(), getUniqueVisitors=data.frame(), getActions=data.frame(),
                                getMaxActions=data.frame(), getBounceCount=data.frame(), getSumVisitsLength=data.frame()),
             Actions=list(get=data.frame(), getPageUrls=data.frame(),
                          getEntryPageUrls=data.frame(), getExitPageUrls=data.frame(),
                          getPageTitles=data.frame(), getEntryPageTitles=data.frame(),
                          getExitPageTitles=data.frame(), getDownloads=data.frame())
  )
  #Actions.getPageUrl, Actions.getPageTitle, Actions.getDownload, Actions.getOutlink : à étudier
  #Actions.getPageTitles, Actions.getEntryPageTitles, Actions.getExitPageTitles : pb sur chaîne en cyrillique
}

#hideCol <- list(UserCountry=list(getCountry=c("nb_visits_converted"), getContinent=c("nb_visits_converted"), getRegion=c("nb_visits_converted")))
hideCol <- list()
fieldstoremove <- c("metadata_logo","metadata_logoWidth","metadata_logoHeight", "nb_visits_converted")  

url_root <- "https://www.euro-area-statistics.org"
nodes <- data.frame()
edges <- data.frame()


# get site structure :
#source("src/site.R")

#restitution :
# visNetwork(nodes, edges, height="700px", width="100%") %>%
#   visLegend()

# get data :
source("src/data.R")
#save(d, file=fdata)


