#------------------------
#generate page repository
#------------------------

#get data
fdata <- "~/R/osstat/data/os_ind_visits+actions_2017-10-02_2018-03-31.RData"
load(fdata)
rm(fdata)

#create table of requested pages
require(tidyr, dplyr)
pr <- a[a$field %in% c("url","pageIdAction"),names(a)[!names(a) %in% c("date")]]
pr[pr$field=="url",]$value <- sub("https://www.euro-area-statistics.org","",pr[pr$field=="url",]$value)
pr <- spread(pr, field, value)
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
pr <- pr[with(pr,order(-n.sum,-n)),c("pageIdAction","n.sum","n","pg","args","url")]

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
pr$url <- NULL
rm(args.df)

#export data
save(pr,file="os-page_repository.RData")


