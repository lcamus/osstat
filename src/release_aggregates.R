#release aggregated datasets

#prepare
if (!exists("d"))
  load("data/d.RData")

ag <- d[-match("Live",names(d))]

#drop useless variables
to.drop <- c("metadata_","nb_users")
va <- names(unlist(ag))
va <- unique(gsub("\\d+$","",va))
va.drop <- va[unlist(lapply(to.drop,function(x) grep(x,va)))]
invisible(lapply(va.drop,function(x){
  y <- unlist(strsplit(x,"\\."))
  ag[[y[1]]][[y[2]]][,y[[3]]] <<- NULL
  }))
print(paste(length(va.drop),"useless variables dropped"))
rm(va.drop,to.drop)

#convert numeric variables coded in string to numeric:
to.num <- c()
to.match <- c("nb_","max_","sum_","avg_","min_","_count(_|$)","value")
to.exclude <- "nb_visits_percentage"

invisible(lapply(names(ag),function(x)
  lapply(names(ag[[x]]),function(y)
    lapply(names(ag[[x]][[y]]),function(z) {
      s <- sapply(to.match,function(r) grep(r,z))
      to.num <<- c(to.num,paste(x,y,z,s))
    }
    ))))

to.num <- to.num[grep("\\s1$",to.num)]
to.exclude <- grep(paste0(to.exclude,"\\s1$"),to.num)
to.num <- to.num[-to.exclude]
to.num <- gsub("\\s1$","",to.num)
to.num <- strsplit(to.num," ")

invisible(lapply(to.num,function(x)
  ag[[x[1]]][[x[2]]][,x[3]] <<- tryCatch({
    as.numeric(ag[[x[1]]][[x[2]]][,x[3]])
  }, warning=function(w) {
    print(w$message)
    print(paste(x[1],x[2],x[3]))
  })))

#convert to numeric percentage variables
invisible(lapply(strsplit(va[grep("rate",va)],"\\."),function(x)
  ag[[x[1]]][[x[2]]][,x[3]] <<- as.numeric(sub("%","",ag[[x[1]]][[x[2]]][,x[3]]))))

#export data
f <- paste0("data/os-aggregates_",min(as.Date(ag[[1]][[1]]$date)),"-",max(as.Date(ag[[1]][[1]]$date)),".RData")
save(ag,file=f)
print(paste("updated aggregates released:",f))

# rm(va,to.exclude,to.match,to.num,f,ag)

#end


