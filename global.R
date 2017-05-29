source("src/util.R")

loadPackages("stringi")
loadPackages("rvest")
loadPackages("visNetwork")
loadPackages("shiny")
loadPackages("DT")
loadPackages("digest")


#shared variables :

fdata <- "data/d.RData"
fcodeData <- "data/codeData.RData"
fCountrySrc <- "data/UserCountry_getCountryCodeMapping.csv"
fCountry <- "data/refCountry.RData"
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
                          getExitPageTitles=data.frame(), getDownloads=data.frame()),
             Live=list(getLastVisitsDetails=data.frame())
  )
  #Actions.getPageUrl, Actions.getPageTitle, Actions.getDownload, Actions.getOutlink : to study
  #Actions.getPageTitles, Actions.getEntryPageTitles, Actions.getExitPageTitles : issue related to enconding (fix result to check)
}

if (!exists("refCountry") & file.exists(fCountrySrc)) {
  refCountry <- read.table(fCountry, header=T, sep=",", fileEncoding="UTF-16")
  refCountry <- as.data.frame(t(refCountry))
  refCountry <- cbind(rownames(refCountry),refCountry)
  refCountry <- setNames(refCountry, c("code","lib"))
  save(refCountry, file=fCountry)
}
  

#hideCol <- list(UserCountry=list(getCountry=c("nb_visits_converted"), getContinent=c("nb_visits_converted"), getRegion=c("nb_visits_converted")))
hideCol <- list()
fieldstoremove <- c("metadata_logo","metadata_logoWidth","metadata_logoHeight", "nb_visits_converted","pluginsIcons","conversions","revenue")  

if (!exists("codeData") & file.exists(fcodeData)) {
  load(fcodeData)
} else {
  codeData <- list()
}

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



