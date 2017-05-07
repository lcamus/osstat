getRowCol <- function(object,method) {
  codeObject <- as.character(sapply(strsplit(object,""),function(x){x %in% LETTERS})) 
  codeObject <- strsplit(object,"")[[1]][as.logical(codeObject)]
  codeObject <- paste0(codeObject,collapse="")
  return(codeObject)
}

collectData <- function(from, to) {
  
  days <- seq(from=as.Date(from), to=as.Date(to), by='days')
  
  for (object in names(d))
    for (method in names(d[[object]]))
      for (day in seq_along(days))
        getData(day, object, method, updatemode=T, appendmode=T)
  
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
    l <- readLines(url(description=u,encoding="UTF-16"), warn=F)
    #l <- strsplit(l[1],",")[[1]]
    df <- as.data.frame(t(as.data.frame(lapply(l[-1],function(x){strsplit(x,",")[[1]]}))))
    rownames(df) <- 1:nrow(df)
    df <- setNames(df[-1,],strsplit(l[1],",")[[1]])

    #df <- cbind(date=date, read.csv(url(description=u, open="", blocking=TRUE, encoding="windows-1252",method="default"),header=T,sep=","))
    df <- cbind(date=date, df)
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

#getData(Sys.Date()-1,"Actions","getExitPageTitles", updatemode=T, appendmode=T, filter_limit=-1)


