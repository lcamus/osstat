
exploreVisits <- function(visitorId) {
  #febbfc27e32025ad
  df <- d[["Live"]][["getLastVisitsDetails"]]
  df <- df[df$visitorId==visitorId,]
  sapply(names(df),function(x){if (df$x !="") print(df$x)})
}

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
  
  getData_Live_getLastVisitsDetails <- function() {
      
      c_actions <- grep(pattern="actionDetails_\\d+_",x=colnames(df))
      c_visits <- sort(colnames(df[,-c_actions]))

      t <-as.numeric(gsub("[a-z,A-Z,_]+","",colnames(df[,c_actions])))
      names(t) <- 1:length(t)
      t <- sort(t)
      c_actions <- colnames(df[,c_actions])[as.numeric(names(t))]
      
      c_visits_top <- sort(c("date","serverTimePretty","idVisit","visitorId","visitIp","country","visitorType","visitCount",
                        "visitDurationPretty","actions","referrerType","referrerName"), decreasing=T)
      l_visits <- as.list(c_visits)
      for (i in c_visits_top)
        l_visits[[grep(pattern=paste0("^",i,"$"),x=c_visits)]] <- NULL
      
      c_visits <- c(c_visits_top, sort(as.character(l_visits)))
      
      module <- "Live"
      
      #append visits general info :
      
      method <- "getLastVisitsDetails:Visits"
      if (is.null(d[[module]][[method]]))
        d[[module]][[method]] <<- data.frame(
          matrix(vector(),0,length(c_visits),dimnames=list(c(),c_visits)),
          stringsAsFactors=F)
      
      if (updatemode | appendmode) {
        d[[module]][[method]] <<- d[[module]][[method]][d[[module]][[method]]$date!=date,]
        d[[object]][[method]] <<- rbind(d[[object]][[method]],df[,c_visits])
      }
      
      #append visits actions :
      
      method <- "getLastVisitsDetails:Actions"
      h_visits <- c("idVisit","step","field","value")
      if (is.null(d[[module]][[method]]))
        d[[module]][[method]] <<- data.frame(
          matrix(vector(),0,4,dimnames=list(c(),h_visits)),
          stringsAsFactors=F)
      
      if (updatemode | appendmode) {
        
        d[[module]][[method]] <<- d[[module]][[method]][d[[module]][[method]]$date!=date,]
        df_a <- as.data.frame(as.vector(t(df[,c_actions])))
        #field :
        df_a <- cbind(rep(gsub("^[a-zA-Z]+_\\d+_","",c_actions),nrow(df)),df_a)
        #step :
        steps <- sapply(c_actions,function(x){gsub("[a-z,A-Z,_]+","",x)})
        df_a <- cbind(rep(steps,nrow(df)),df_a)
        #idVisit :
        df_a <- cbind(as.numeric(sapply(df$idVisit,function(x) rep(x,length(c_actions)))),df_a)
        
        #delete empty actions :
        df_a <- setNames(df_a,h_visits)
        df_a <- df_a[sapply(as.numeric(rownames(df_a[which(df_a$field=="type" & df_a$value!=""),])),function(x) seq(x,x+10)),]
        
        #append data in final structure :
        d[[object]][[method]] <<- rbind(d[[object]][[method]],df_a)
        
      }
    
  }
  
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
    
    # get the data from the API :
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
  
  if (object=="Live" & method=="getLastVisitsDetails")
    getData_Live_getLastVisitsDetails()
  else {
    if (!updatable) {
      if (appendmode)
        d[[object]][[method]] <<- rbind(d[[object]][[method]],df)
      else
        #print(df)
        print("no action")
    } else {
      d[[object]][[method]] <- d[[object]][[method]][d[[object]][[method]]$date!=date,]
      d[[object]][[method]] <<- rbind(d[[object]][[method]],df)
    }    
  }
  
}

#getData("2017-05-01","Live","getLastVisitsDetails", updatemode=T, appendmode=T, filter_limit=-1)


