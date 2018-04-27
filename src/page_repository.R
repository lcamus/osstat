#------------------------
#generate page repository
#------------------------

# called from clean_and_update_datasets.R

#create table of requested pages

fRefShortUrl <- "./data/refShortUrl.RData"

suppressPackageStartupMessages(require(tidyr))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(httr))

pref <- "euro-area-statistics.org"

#clear url prefix:
pr <- a[a$type!="search",c("idVisit","step","pageIdAction","url")]
pr$url <- sub(paste0("https://(www\\.)?",sub(".","\\.",pref)),
              "",
              pr$url)
pr <- pr[,names(pr)[!names(pr) %in% c("idVisit","step")]]
pr <- pr %>% group_by(url) %>% mutate(n=n())
pr <- unique(pr)
pr <- pr[with(pr,order(-n)),]

#expand short URLs:
if (file.exists(fRefShortUrl)) {
  load(fRefShortUrl)
} else
  su <- data.frame(short.url=c("/e-MTUxNDIzMjU3MQ"),expanded.url=c("/inflation-rates"),stringsAsFactors=F)
su.new <- F
short.url <- invisible(sapply(grep("/e-MTU",pr$url),function(x){
  if (!pr[x,]$pg %in% su$short.url) {
    pat <- '"url":"([a-z,A-Z,-]+){1}"'
    print(paste0("*",x))
    g <- content(GET(paste0("https://",pref,pr[x,]$url)),"text")
    r <- regexec(pat,g)[[1]]
    su <<- rbind(su,
                 c(pr[x,]$pg,
                   paste0("/",substr(g,r[2],r[2]+attr(r,"match.length")[2]-1))))
    su.new <<- T
  }
  return(x)
}))
if (su.new) save(su,file=fRefShortUrl)
rm(fRefShortUrl,su.new)
pr[short.url,]$pg <- left_join(setNames(data.frame(pr[short.url,]$pg,stringsAsFactors=F),"pg"),su,by=c("pg"="short.url"))[,2]

#split the url:
pr$url <- gsub(" ","",pr$url) #clean-up
pr <- pr %>% mutate(pg=lapply(strsplit(url,"\\?"),`[`,1))
pr <- pr %>% mutate(args=lapply(strsplit(url,"\\?"),`[`,2))
pr$url <- NULL

#sort by trafic:
pr$pg <- as.character(pr$pg)
pr <- pr %>% group_by(pg) %>% mutate(n.sum=sum(n))
pr <- pr[with(pr,order(-n.sum,-n)),c("pageIdAction","n.sum","n","pg","args")]

#split url-args:
require(stringr)
pr$args <- as.character(pr$args)
#args used in web requests:
args.cat <- unique(unlist(lapply(sapply(strsplit(pr$args,"&"),function(x) strsplit(x,"=")),function(x)lapply(x,`[[`,1))))
args.cat <- as.character(na.omit(args.cat))
args.df <- lapply(args.cat,function(x) str_match(pr$args,paste0(x,"=(\\w+)")))
args.df <- as.data.frame(lapply(args.df,function(x) x[,2]),col.names=args.cat,stringsAsFactors=F)
#sort the args according to their frequency:
args.df <- args.df[,names(sort(sapply(args.df,function(x) sum(!is.na(x))),decreasing=T))]
rm(args.cat)

#finalise dataset of page repository
pr <- data.frame(pr,args.df,stringsAsFactors=F)
# pr$url <- NULL
rm(args.df)

#export data
f <- paste0("data/os-page_repository_",min(as.Date(a$date)),"-",max(as.Date(a$date)),".RData")
save(pr,file=f)
print(paste("page repository updated:",f))
rm(f)

#end

