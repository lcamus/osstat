getGID <- function(module,method) {
  
  getCap <- function(s) {
    res <- as.character(sapply(strsplit(s,""),function(x){x %in% LETTERS})) 
    res <- strsplit(s,"")[[1]][as.logical(res)]
    res <- paste0(res,collapse="")
    return(res)
  }
  
  getCode <- function(m) {
    caps <- which(strsplit(m,"")[[1]] %in% LETTERS)
    s <- substring(m,caps[1])
    s <- strsplit(s,"(?=[A-Z])",perl=T)
    s <- sapply(s,function(x) substring(x,1,1))
    s <- paste0(as.character(s),collapse="")
  }
  
  if (is.null(codeData[[module]]))
    codeData[[module]] <- getCap(module)
  
  if (is.null(codeData[[paste(module,method,sep=":")]]))
    codeData[[paste(module,method,sep=":")]] <- getCode(method)
  
}

collectData <- function(from, to) {
  
  days <- seq(from=as.Date(from), to=as.Date(to), by='days')
  
  for (object in names(d))
    for (method in names(d[[object]]))
      #for (day in seq_along(days))
      for (day in days)
        getData(as.Date(day,origin="1970-01-01"), object, method, updatemode=T, appendmode=T)
  
  save(d, file=fdata)
  
}

getData <- function(date, object, method, hideColumns, period, filter_limit, updatemode, appendmode) {
  
  print(object)
  print(method)
  
  if (missing(filter_limit)) filter_limit <- "-1"
  if (missing(period)) period <- "day"
  if (missing(updatemode)) updatemode <- FALSE
  if (missing(appendmode)) appendmode <- FALSE
  if (missing(hideColumns)) hideColumns <- paste(hideCol[[object]][[method]],collapse=",")

  if (is.null(d[[object]][[method]]))
    updatable <- FALSE
  else
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
    print(paste("exec API for",object,method,sep=" "))
    l <- readLines(url(description=u,encoding="UTF-16"), warn=F)
    pattern <- ",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)"
    df <- as.data.frame(t(as.data.frame(lapply(l[-1],function(x){strsplit(x,pattern,perl=T)[[1]]}))))
    df <- setNames(df[-1,],strsplit(l[1],",")[[1]])
    rownames(df) <- paste(as.numeric(as.Date(date)),sprintf(paste0("%0",nchar(nrow(df)),"d"),1:nrow(df)),sep=":")
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
    #d[[object]][[method]][d[[object]][[method]]$date==date,] <<- df
    d[[object]][[method]] <- d[[object]][[method]][d[[object]][[method]]$date!=date,]
    d[[object]][[method]] <<- rbind(d[[object]][[method]],df)
  }
  
}

#getData(Sys.Date()-1,"UserCountry","getCountry", updatemode=T, appendmode=T, filter_limit=-1)


