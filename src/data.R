
#initialisation de la structure

getAllData <- function(date, period) {
  m <- list()
  m[["UserCountry"]] <- c("getCountry","getContinent","getRegion","getCity")
  m[["UserLanguage"]] <- c("getLanguage","getLanguageCode")
  m[["VisitFrequency"]] <- c("get")
  m[["VisitTime"]] <- c("getVisitInformationPerLocalTime","getByDayOfWeek")
  m[["VisitorInterest"]] <- c("getNumberOfVisitsPerVisitDuration","getNumberOfVisitsPerPage","getNumberOfVisitsByDaysSinceLast","getNumberOfVisitsByVisitCount")
  m[["VisitsSummary"]] <- c("get","getVisits","getUniqueVisitors","getUsers","getActions","getMaxActions","getBounceCount","getVisitsConverted","getSumVisitsLength","getSumVisitsLengthPretty")
  l <- setNames(vector("list", length(m)), names(m))
  #lapply(l, function(l) setNames(vector("list",length(m[[1]])),m[[1]]))
  for (i in 1:length(m)) {
    l[[i]] <- setNames(vector("list",length(m[[i]])),m[[i]])
    for (j in 1:length(m[[i]])) {
      ll = length(l[[i]][[j]])
      #print(paste(i,j, names(m)[i], m[[i]][j]))
      l[[i]][[j]][[ll+1]] <- getData(date, period, names(m)[i], m[[i]][j])
    }
  }
  #l$UserCountry = list(getCountry=list(), getContinent=list(), getRegion=list(), getCity=list())
  #l$UserCountry$getCountry[[length(l$UserCountry$getCountry)+1]] <- getData(date, period, "UserCountry","getCountry")
  return(l)
}

getData <- function(date, period, object, method) {
  
  filter_limit <- "filter_limit=-1"
  date <- paste("date=", date, sep="")
  period <- paste("period=", period, sep="")
  method <- paste("method=", object, ".", method, sep="")
  url <- paste(base, method, paste0("idSite=",idSite), date, period, paste0("format=",format), paste0("token_auth=",token_auth), filter_limit, sep="&")
  df <- read.csv(url(description=url, open="", blocking=TRUE, encoding="UTF-16",method="default"),header=T,sep=",")
  l <- list(paste(date,period,sep="+"), df)
  
  return(l)
  
}

useData <- function(alldata, date, period, object, method) {
  return(alldata[[object]][[method]][[1]][[2]])
}

samples <- function() {
  ##language
  #library(treemap)
  df <- useData(eas,"today","year","UserLanguage","getLanguage")
  treemap(cbind(df,bounce_ratio=df$bounce_count/df$nb_visits),c("label"), vSize="nb_visits", vColor="bounce_ratio", type="value")
  
  
  ##country
  #library(countrycode)
  #dfContinent <- useData(eas,"today","year","UserCountry","getContinent")
  dfCountry <- useData(eas,"today","year","UserCountry","getCountry")
  #levels(dfContinent$metadata_code) <- replace(levels(dfContinent$metadata_code),levels(dfContinent$metadata_code)=="North America","Northern America")
  #levels(dfContinent$metadata_code) <- replace(levels(dfContinent$metadata_code),levels(dfContinent$metadata_code)=="South America","Southern America")
  levels(dfCountry$metadata_code) <- toupper(levels(dfCountry$metadata_code))
  df <- merge(x=dfCountry, y=countrycode_data[c("continent","iso2c")], by.x="metadata_code", by.y="iso2c", all.x=T)
  
}


eas = getAllData("today", "year")
