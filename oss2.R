source("src/util.R")

# loadPackages("stringi")
# loadPackages("rvest")
# loadPackages("DT")
# loadPackages("digest")


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
  refCountry <- read.table(fCountrySrc, header=T, sep=",", fileEncoding="UTF-16")
  refCountry <- as.data.frame(t(refCountry))
  refCountry <- cbind(rownames(refCountry),refCountry)
  refCountry <- setNames(refCountry, c("code","lib"))
  save(refCountry, file=fCountry)
}
  

#hideCol <- list(UserCountry=list(getCountry=c("nb_visits_converted"), getContinent=c("nb_visits_converted"), getRegion=c("nb_visits_converted")))
hideCol <- list()
fieldstoremove <- c("metadata_logo","metadata_logoWidth","metadata_logoHeight", "nb_visits_converted","pluginsIcons","conversions","revenue")  
fieldstoremove <- c(fieldstoremove,"siteCurrency","Icon","Ecommerce")

if (!exists("codeData") & file.exists(fcodeData)) {
  load(fcodeData)
} else {
  codeData <- list()
}

# get data :
source("src/data.R")

#update data:
from <- readline("collect from (enter yyyy-mm-dd date, default today): ")
if (from=="") from <- Sys.Date()
to <- readline(paste0("collect to (enter yyyy-mm-dd date, default ",from,"): "))
if (to=="") to <- from
collectData(from, to, -1, updatemode=T, appendmode=F, visits=T, visitsonly=T)
rm(from,to)
#getData("2017-05-29","Live","getLastVisitsDetails", updatemode=T, appendmode=T, filter_limit=-1)
# getData(NULL,"Live","getLastVisitsDetails", updatemode=T, appendmode=F, filter_limit=-1,idVisit=211684)

# store updated data
u <- toupper(readline("save updated data and replace previous one? ('y' or 'Y' to agree, default no) "))
if (u=="Y") {
  save(d, file=fdata)
  print("updated data saved")
} else
  print("updated data not saved!")
rm(u)


