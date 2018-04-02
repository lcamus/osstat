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
pr <- pr %>% mutate(page=lapply(strsplit(url,"\\?"),`[`,1))
pr <- pr %>% mutate(args=lapply(strsplit(url,"\\?"),`[`,2))
# head(lapply(x, `[`, 2))

#sort by trafic:
pr$page <- as.character(pr$page)
pr <- pr %>% group_by(page) %>% mutate(n.sum=sum(n))
pr <- pr[with(pr,order(-n.sum,-n)),c("pageIdAction","n.sum","n","page","args","url")]

#split url-args:
#prepare:
pr$args <- as.character(pr$args)
pr <- pr %>% mutate(arg=lapply(strsplit(args,"&"),sort))
#split the args list:
max.arg <- max(unlist(lapply(pr$arg,length)))
for (arg.i in 1:max.arg) {
  varname.key <- paste0("arg.",arg.i,".key")
  varname.val <- paste0("arg.",arg.i,".val")
  pr <- pr %>% mutate(!!varname.key := lapply(sapply(lapply(arg,`[`,arg.i),function(x) strsplit(x,"=")),`[`,1),
                      !!varname.val := lapply(sapply(lapply(arg,`[`,arg.i),function(x) strsplit(x,"=")),`[`,2))
}
pr$arg <- NULL

#transpose the args as variables:
#protect original variables in the dataframe whose name is also used as query args name:  
pr <- setNames(pr,replace(names(pr),names(pr) %in% c("page","url"),c("original.page","original.u")))
pr <- as.data.frame(pr)
# for (arg.i in grep("^arg\\.[0-9]+\\.key$",names(pr))) {
for (arg.i in 1:max.arg) {
  pos.i <- match(paste("arg",arg.i,"key",sep="."),names(pr))
  pr <- spread(pr,pos.i,pos.i+1)
}
  