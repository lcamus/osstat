#------------------------
#generate page repository
#------------------------

# called from clean_and_update_datasets.R

#create table of requested pages

suppressPackageStartupMessages(require(tidyr))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(httr))

pref <- "euro-area-statistics.org"

#expand short URLs:
url.short <- grep("/e-MTU",pr$url)
invisible(lapply(url.short,GET))

#clear url prefix:
pr <- a[a$type!="search",c("idVisit","step","pageIdAction","url")]
pr$url <- sub(paste0("https://(www\\.)?",sub(".","\\.",pref)),
              "",
              pr$url)
pr <- pr[,names(pr)[!names(pr) %in% c("idVisit","step")]]
pr <- pr %>% group_by(url) %>% mutate(n=n())
pr <- unique(pr)
pr <- pr[with(pr,order(-n)),]

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

