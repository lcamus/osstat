
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

collectData <- function(from, to, filter_limit, updatemode, appendmode, visits) {
  
  if (missing(filter_limit)) filter_limit <- "-1"
  if (missing(visits)) visits <- F
  if (missing(updatemode)) updatemode <- F
  if (missing(appendmode)) appendmode <- F
  
  days <- seq(from=as.Date(from), to=as.Date(to), by='days')
  
  for (module in names(d)) {
    if (!visits)
      methods <- sapply(names(d[[module]]),function(x) gsub("^.*:.*$","",x))
    else
      methods <- unique(sapply(names(d[[module]]),function(x){gsub(":[a-zA-Z]+$","",x)})) 
    for (method in methods)
      for (day in days)
        getData(date=as.Date(day,origin="1970-01-01"),
                object=module, method=method,
                filter_limit=filter_limit,
                updatemode=updatemode, appendmode=appendmode) 
  }
  
  save(d, file=fdata)
  
}

getData <- function(date, object, method, hideColumns, period, filter_limit, updatemode, appendmode) {
  
  if (object=="" | method=="") return(-1)
  
  print(object)
  print(method)
  
  #update only empty dataset for the period
  if (object=="Live" & method=="getLastVisitsDetails")
    submethod <- "getLastVisitsDetails:Visits"
  else
    submethod <- "getLastVisitsDetails"
  if (!updatemode & !appendmode & nrow(d[[object]][[submethod]][d[[object]][[submethod]]$date==date,])>0) return(-1)
  print("cont")
  
  require(stringi)
  options(stringsAsFactors=F)
  
  getData_Live_getLastVisitsDetails <- function(df) {
      
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
      
      methodSub <- "getLastVisitsDetails:Visits"
      if (is.null(d[[module]][[methodSub]]))
        d[[module]][[methodSub]] <<- data.frame(
          matrix(vector(),0,length(c_visits),dimnames=list(c(),c_visits)),
          stringsAsFactors=F)
      
      if (updatemode | appendmode) {
        d[[module]][[methodSub]] <<- d[[module]][[methodSub]][d[[module]][[methodSub]]$date!=date |
                                                                (d[[module]][[methodSub]]$date==date & 
                                                                   !d[[module]][[methodSub]]$visitServerHour %in% unique(df$visitServerHour)),]
        d[[module]][[methodSub]] <<- rbind(d[[module]][[methodSub]],df[,c_visits])
      }
      
      #append visits actions :
      
      methodSub <- "getLastVisitsDetails:Actions"
      h_visits <- c("date","idVisit","step","field","value")
      if (is.null(d[[module]][[methodSub]]))
        d[[module]][[methodSub]] <<- data.frame(
          matrix(vector(),0,5,dimnames=list(c(),h_visits)),
          stringsAsFactors=F)
      
      if (updatemode | appendmode) {
        
        # erase any visits to be updated
        d[[module]][[methodSub]] <<- d[[module]][[methodSub]][!d[[module]][[methodSub]]$idVisit %in% df$idVisit,]
        
        df_a <- as.data.frame(as.vector(t(df[,c_actions])))
        #field :
        df_a <- cbind(rep(gsub("^[a-zA-Z]+_\\d+_","",c_actions),nrow(df)),df_a)
        #step :
        steps <- sapply(c_actions,function(x){gsub("[a-z,A-Z,_]+","",x)})
        df_a <- cbind(rep(steps,nrow(df)),df_a)
        #idVisit :
        df_a <- cbind(as.numeric(sapply(df$idVisit,function(x) rep(x,length(c_actions)))),df_a)
        #date :
        df_a <- cbind(as.character(sapply(df$date,function(x) rep(x,length(c_actions)))),df_a)
        
        #delete empty actions
        df_a <- setNames(df_a,h_visits)
        actions_begin <- rownames(df_a[which(df_a$field=="type"),])
        actions_end <- sapply(1:length(actions_begin),function(x){as.numeric(actions_begin[x+1])-1})
        actions_end[length(actions_end)] <- max(rownames(df_a))
        actions_full_begin <- rownames(df_a[which(df_a$field=="type" & df_a$value!=""),])
        actions_full_end <- actions_end[actions_begin %in% actions_full_begin]
        res<-unlist(sapply(seq_along(actions_full_begin), function(x){actions_full_begin[x]:actions_full_end[x]}))
        #df_a <- df_a[sapply(as.numeric(rownames(df_a[which(df_a$field=="type" & df_a$value!=""),])),function(x) seq(x,x+10)),]
        df_a <- df_a[res,]
        base <- max(nrow(d[[module]][[methodSub]]))
        row.names(df_a) <- seq(base+1,base+nrow(df_a))
        
        #append data in final structure
        d[[module]][[methodSub]] <<- rbind(d[[module]][[methodSub]],df_a)
        
      }
    
  }
  
  getData_APICall <- function() {
    
    separateFields <- function(l) {
      pattern <- ",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)"
      df <- as.data.frame(t(as.data.frame(lapply(l[-1],function(x){stri_split(str=x,regex=pattern)[[1]]}))))
      df <- setNames(df,strsplit(l[1],",")[[1]])
      rownames(df) <- paste(as.numeric(as.Date(date)),sprintf(paste0("%0",nchar(nrow(df)),"d"),1:nrow(df)),sep=":")
      df <- cbind(date=date, df)
      return(df)
    }
    
    removeNullFields <- function(df) {
      bye <- -unlist(sapply(fieldstoremove,function(x){which(grepl(x,colnames(df))==TRUE)}))
      if (length(bye)>0) df <- df[,bye]
      return(df)
    }
    
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
    
    if (object=="Live" & method=="getLastVisitsDetails") {
      # read data in several calls for individual visits (heavy)
      breaks <- c("visitServerHour%3C7","visitServerHour%3E=7;visitServerHour%3C10","visitServerHour%3E=10;visitServerHour%3C20","visitServerHour%3E=20")
      l <- lapply(breaks,function(x) {
        ou <- url(description=paste0(u,"&segment=",x),encoding="UTF-16")
        res <- readLines(ou, warn=F)
        close(ou)
        return(res)
        })
      df <- sapply(l,function(x) separateFields(x))
      df <- sapply(df,function(x) removeNullFields(x))
    }
    else {
      # nominal case (not heavy)
      ou <- url(description=u,encoding="UTF-16")
      l <- readLines(ou, warn=F)
      close(ou)
      df <- separateFields(l) # separate fields
      df <- removeNullFields(df) # remove unuseful fields
    }
      
    return(df)
    
  }
  
  addRows <- function(data) {
    # data requested lack columns :
    if (ncol(d[[object]][[method]])>ncol(data)) {
      n <- ncol(d[[object]][[method]])-ncol(data)
      cc <- tail(colnames(head(d[[object]][[method]])),n)
      cc <- c(colnames(data),cc)
      cc <- setNames(cbind(data,as.list(rep(NA,n))),cc)
      d[[object]][[method]] <<- rbind(d[[object]][[method]],cc)
      print(paste("*** alert :",n,"column(s) missing (compensated with NA column(s))"))
      # data requested has new columns :
    } else if (ncol(d[[object]][[method]])<ncol(data)) {
      stop("*** error :",n,"new columns detected")
      # otherwise (ok) :
    } else
      d[[object]][[method]] <<- rbind(d[[object]][[method]],data)
  }
    
  
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
    # get the data from the API
    data <- getData_APICall()
    
  }
  else
    return
  
  if (object=="Live" & method=="getLastVisitsDetails")
    lapply(data,function(x){getData_Live_getLastVisitsDetails(x)})
  else {
    if (!updatable) {
      addRows(data)
    } else {
      d[[object]][[method]] <<- d[[object]][[method]][d[[object]][[method]]$date!=date,]
      addRows(data)
    }    
  }
  
}

#getData("2017-05-01","Live","getLastVisitsDetails", updatemode=T, appendmode=T, filter_limit=-1)


