<<<<<<< HEAD
exploreVisits <- function(visitorId) {
  #febbfc27e32025ad
  df <- d[["Live"]][["getLastVisitsDetails"]]
  df <- df[df$visitorId==visitorId,]
  sapply(names(df),function(x){if (df$x !="") print(df$x)})
}

=======
>>>>>>> origin/master

setNumeric <- function() {
  
  pattern <- c("nb_","max_","sum_","_count","avg_","min_")
  
  for (module in names(d))
    for (method in names(d[[module]]))
      for (col in colnames(d[[module]][[method]]))
        if (grepl("_rate",col) | grepl("_country",col) | grepl("_percentage",col))
          break
        else {
          for (p in pattern)
            if (grepl(p,col)) {
              print(paste(module,method,col,p,sep=":"))
              d[[module]][[method]][,c(col)] <<- as.numeric(as.character(d[[module]][[method]][,c(col)]))
              #d[[module]][[method]][,c("date")]
              break
            }          
        }

}

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
    df <- as.data.frame(t(as.data.frame(lapply(l[-1],function(x){stri_split(str=x,regex=pattern)[[1]]}))))
    df <- setNames(df,strsplit(l[1],",")[[1]])
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
    d[[object]][[method]] <- d[[object]][[method]][d[[object]][[method]]$date!=date,]
    d[[object]][[method]] <<- rbind(d[[object]][[method]],df)
  }
  
}

#getData(Sys.Date()-1,"Live","getLastVisitsDetails", updatemode=T, appendmode=T, filter_limit=-1)


