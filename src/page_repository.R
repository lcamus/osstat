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

#create data frame:
pr <- a[a$type!="search",c("idVisit","step","pageIdAction","url")]
pr <- pr[,names(pr)[!names(pr) %in% c("idVisit","step")]]

#expand short URLs:
if (file.exists(fRefShortUrl)) {
  load(fRefShortUrl)
} else
  su <- data.frame(short.url=c("/e-MTUxNDIzMjU3MQ"),expanded.url=c("/inflation-rates"),stringsAsFactors=F)
su.new <- F
invisible(sapply(grep("/e-MTU",unique(pr$url),value=T),function(x){
  pg <- sub("^.+(/e-MTU\\w+){1}\\?.+$","\\1",x)
  if (!pg %in% su$short.url) {
    print(paste0("*new short-cut page ",x))
    g <- content(GET(x),"text")
    r <- sub('^.+"url":"([a-z,A-Z,-]+){1}".+$',"\\1",g)
    su <<- rbind(su,c(pg,paste0("/",r)))
    su.new <<- T
  }
  pr[pr$url==x,]$url <<- sub(pg,su[su$short.url==pg,]$expanded.url,x)
}))
if (su.new) save(su,file=fRefShortUrl)
rm(fRefShortUrl,su.new,su)

#group:
pr$url <- sub(paste0("https://(www\\.)?",sub(".","\\.",pref)),
              "",
              pr$url)
pr <- pr %>% group_by(url) %>% mutate(n=n())
pr <- unique(pr)
pr <- pr[with(pr,order(-n)),]

#split the url:
pr$url <- gsub(" ","",pr$url) #clean-up
pr <- pr %>% mutate(pg=lapply(strsplit(url,"\\?"),`[`,1))
pr$pg <- as.character(pr$pg)
pr <- pr %>% mutate(args=lapply(strsplit(url,"\\?"),`[`,2))
pr$url <- NULL

#sort by trafic:
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

#flag bad entries
bad <- c(71576,75583,74845,74797,73224,72902,71694,71079,69463,67865,46224,66106,65786,57711,76033,75133,73900,66105,65083,
         65477,65478,87,64432,70635,74382,70232,73726,73983,55271,65206,65863)
pr$bad <- F
pr[pr$pageIdAction %in% bad,]$bad <- T
rm(bad)

#finalise dataset of page repository
pr <- data.frame(pr,args.df,stringsAsFactors=F)
rm(args.df)

#export data
f <- paste0("data/os-page_repository_",min(as.Date(a$date)),"-",max(as.Date(a$date)),".RData")
save(pr,file=f)
print(paste("page repository updated:",f))
rm(f)

#end

