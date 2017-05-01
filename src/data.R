
collectData <- function(from, to) {
  
  days <- seq(from=as.Date(from), to=as.Date(to), by='days')
  
  for (object in names(d))
    for (method in names(d[[object]]))
      for (day in seq_along(days))
        getData(day, object, method, update=F)
}

getData <- function(date, object, method, hideColumns, period, filter_limit, updatemode, appendmode) {
  
  if (missing(filter_limit)) filter_limit <- "-1"
  if (missing(period)) period <- "day"
  if (missing(updatemode)) updatemode <- FALSE
  if (missing(appendmode)) appendmode <- FALSE
  if (missing(hideColumns)) hideColumns <- paste(hideCol[[object]][[method]],collapse=",")

  updatable <- nrow(d[[object]][[method]][d[[object]][[method]]$date==date,])>0
  
  if (!updatable |(updatable & updatemode)) {
    u <- paste(base,
               paste("method=", object, ".", method, sep=""),
               paste0("idSite=",idSite),
               paste("date=", date, sep=""),
               paste0("period=",period),
               paste0("format=",format),
               paste0("token_auth=",token_auth),
               paste0("filter_limit=",filter_limit),
               paste0("hideColumns=",hideColumns),
               sep="&")
    df <- cbind(date=date, read.csv(url(description=u, open="", blocking=TRUE, encoding="UTF-16",method="default"),header=T,sep=","))
    df <- df[,!colnames(df) %in% fieldstoremove]
  }
  else
    return
  
  if (!updatable) {
    if (appendmode)
      d[[object]][[method]] <<- rbind(d[[object]][[method]],df)
    else
      print(df)
  } else {
    d[[object]][[method]][d[[object]][[method]]$date==date,] <- df
  }
  
}

#getData(Sys.Date()-1,"VisitorInterest","getNumberOfVisitsByVisitCount", updatemode=T, appendmode=T, filter_limit=-1)


