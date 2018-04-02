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
head(lapply(x, `[`, 2))