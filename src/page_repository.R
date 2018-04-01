#get data
load("~/R/osstat/data/os_ind_visits+actions_2017-10-02_2018-03-31.RData")

#create table of requested pages
pr <- a[a$field %in% c("url","idpageview","pageIdAction"),]
pr[pr$field=="url",]$value <- sub("https://www.euro-area-statistics.org","",pr[pr$field=="url",]$value)
