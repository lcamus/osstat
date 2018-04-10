source("src/util.R")

loadPackages("stringi")
loadPackages("digest")

#shared variables :
fdata <- "data/d.RData"
if (!exists("d") & file.exists(fdata))
  load(fdata)
  
if (!exists("d"))
    d <- list(UserCountry=list(getCountry=data.frame(), getContinent=data.frame(), getRegion=data.frame(), getCity=data.frame()),
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

#countries ref:
fCountrySrc <- "data/UserCountry_getCountryCodeMapping.csv"
fCountry <- "data/refCountry.RData"
if (!exists("refCountry") & file.exists(fCountrySrc)) {
  refCountry <- read.table(fCountrySrc, header=T, sep=",", fileEncoding="UTF-16")
  refCountry <- as.data.frame(t(refCountry))
  refCountry <- cbind(rownames(refCountry),refCountry)
  refCountry <- setNames(refCountry, c("code","lib"))
  save(refCountry, file=fCountry)
}
rm(fCountrySrc,fCountry)
  
#hideCol <- list(UserCountry=list(getCountry=c("nb_visits_converted"), getContinent=c("nb_visits_converted"), getRegion=c("nb_visits_converted")))
hideCol <- list()
fieldstoremove <- c("metadata_logo","metadata_logoWidth","metadata_logoHeight", "nb_visits_converted","pluginsIcons","conversions","revenue")  
fieldstoremove <- c(fieldstoremove,"siteCurrency","Icon","Ecommerce")

fcodeData <- "data/codeData.RData"
if (!exists("codeData") & file.exists(fcodeData)) {
  load(fcodeData)
} else {
  codeData <- list()
}
rm(fcodeData)

#functions to get data:
source("src/data.R")

#update data:
base <- "https://utils.euro-area-statistics.org/admin/piwik/?module=API"
idSite <- "1"
format <- "CSV"
token_auth <- as.character(read.table("private/token",header=F)[,])
collect <- function() {
  
  v <- d[["Live"]][["getLastVisitsDetails:Visits"]]
  from.def <- as.Date(tail(sort(unique(v$serverDate)),1))+1
  rm(v)
  to.def <- min(from.def+6,Sys.Date()-1)
  print(paste0("last individual data collected: ",from.def-1))
  from <- readline(paste0("collect from (enter yyyy-mm-dd date, default ",from.def,"): "))
  if (from=="") from <- from.def
  to <- readline(paste0("collect to (enter yyyy-mm-dd date, default ",to.def,"): "))
  if (to=="") to <- to.def
  collect.scope <- readline(paste0("collect aggregated data only (1), individual data only (2) or both (3), default both: "))
  if (collect.scope=="") collect.scope <- "3"
  if (collect.scope=="1") {
    V <- F
    VO <- F
  } else if (collect.scope=="2") {
    V <- T
    VO <- T
  } else if (collect.scope=="3") {
    V <- T
    VO <- F
  }
  modulescope <- readline(paste0("all analytical modules (default) or some (enter modules names separated by space)): "))
  if (modulescope=="")
    modulescope <- NULL
  else
    modulescope <- unlist(strsplit(modulescope," "))
  print(paste0("start to collect data from ",from," to ",to))
  print(paste0("aggregated data *",ifelse(VO,"no","yes"),
               "*, individual data *",ifelse(V,"yes","no"),
               "*"))
  print(paste("analytical modules:",modulescope))
  collectData(from=from, to=to, filter_limit=-1, updatemode=T, appendmode=F, visits=V, visitsonly=VO,
              modulescope=modulescope)
  print("data collected")
  rm(from,to,from.def,to.def,V,VO,collect.scope,modulescope)
  #getData("2017-05-29","Live","getLastVisitsDetails", updatemode=T, appendmode=T, filter_limit=-1)
  # getData(NULL,"Live","getLastVisitsDetails", updatemode=T, appendmode=F, filter_limit=-1,idVisit=211684)
  
  # store updated data
  u <- toupper(readline("save updated data and replace previous one? ('y' or 'Y' to agree, default yes) "))
  if (u %in% c("Y","")) {
    save(d, file=fdata)
    print("updated data saved")
  } else
    print("updated data not saved!")
  rm(u)
  
} #collect
collect()
rm(base,idSite,format,token_auth)
rm(fdata)

#enjoy
v <- d[["Live"]][["getLastVisitsDetails:Visits"]]
a <- d[["Live"]][["getLastVisitsDetails:Actions"]]

#cleanup
rm(codeData,hideCol,refCountry,fieldstoremove)
rm(collect,collectData,getData,getGID,loadPackages,setNumeric)


