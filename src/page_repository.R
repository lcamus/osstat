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

extendRepositoryPage <- function(pr) {
  
  suppressPackageStartupMessages(require(dplyr))
  
  fSiteHierarchy <- "./data/refSiteHierarchy.RData"
  url.root <- "https://www.euro-area-statistics.org/"
  
  #refine repository page:
  
  pr2 <- pr[pr$bad==F,]
  # pr2$bad <- NULL
  
  pr2$pg <- sub("^.*sdw-wsrest\\.ecb\\.europa\\.eu/service/data/(\\w{2,3})/(.+)$","/bankscorner/\\1/export/\\2",pr2$pg)
  pr2$pg <- sub("^.*sdw\\.ecb\\.europa\\.eu/datastructure.do$","/outlink/sdw/datastructure",pr2$pg)
  pr2$pg <- sub("^.*sdw\\.ecb\\.(europa\\.eu|int)/(\\w+)?(\\.do)?$","/outlink/sdw/\\2",pr2$pg)
  pr2$pg <- sub("^file:.+$","local",pr2$pg)
  pr2$pg <- sub("^.*www\\.(\\w+\\.)?(ecb|bankingsupervision)\\.europa\\.eu/.*$","/outlink/ECB",pr2$pg)
  pr2$pg <- sub("^.+trans(late)?.*$","/shared/translate",pr2$pg)
  pr2$pg <- sub("^.*www\\.imf\\.org.*$","/outlink/IMF",pr2$pg)
  pr2$pg <- sub("^.*www\\.ebf-fbe\\.eu.*$","/outlink/EBF",pr2$pg)
  pr2$pg <- sub("^.+ec\\.europa\\.eu.*$","/outlink/EC",pr2$pg)
  pr2$pg <- sub("^.*www\\.youtube\\.com.*$","/outlink/YouTube",pr2$pg)
  pr2$pg <- sub("^.+sdmx\\.org.*$","/outlink/SDMX.org",pr2$pg)
  pr2$pg <- sub("^.+/insights-atom\\.xml$","/shared/rss",pr2$pg)
  pr2$pg <- sub("^.*/embed.*$","/shared/embed",pr2$pg)
  pr2$pg <- sub("^.*/data$","/shared/data",pr2$pg)
  pr2$pg <- sub("^.*/www\\.oecd\\.org.*$","/outlink/OECD",pr2$pg)
  pr2$pg <- sub("^.*/www\\.compareyourcountry\\.org.*$","/outlink/OECD",pr2$pg)
  pr2$pg <- sub("^/classic/banks-corner$","/bankscorner/",pr2$pg)
  pr2$pg <- sub("^.+/banks-corner-(\\w{2,3})/\\w{2,3}codelist\\.xlsx$","/bankscorner/\\1",pr2$pg)
  pr2$pg <- sub("^(/classic)?/banks-corner-(\\w{2,3})$","/bankscorner/\\2",pr2$pg)
  pr2$pg <- sub("^/classic/(.+)$","/insights/\\1",pr2$pg)
  pr2$pg <- sub("^/((\\w|-)+)$","/indicators/\\1",pr2$pg)
  pr2$pg <- sub("^/$","/homepage",pr2$pg)
  
  ncbs <- c("http://www.nbb.be/","http://www.bundesbank.de/","http://www.eestipank.ee/","http://www.centralbank.ie/","http://www.bankofgreece.gr/","http://www.bde.es/","http://www.banque-france.fr/","http://www.bancaditalia.it/","http://www.centralbank.gov.cy/","http://www.bank.lv/","http://www.lb.lt/","http://www.bcl.lu/","http://www.centralbankmalta.org/","http://www.dnb.nl/","http://www.oenb.at/","http://www.bportugal.pt/","http://www.bsi.si/","http://www.nbs.sk/","http://www.suomenpankki.fi")
  invisible(lapply(ncbs,function(x){
    pr2$pg <<- sub(paste0(x,".*$"),"/outlink/NCBs",pr2$pg)
  }))
  
  f <- grep("^/bankscorner/\\w{2,3}/export/.+$",pr2$pg)
  pr2[f,]$args <- sub("^/bankscorner/\\w{2,3}/export/","",pr2[f,]$pg)
  pr2[f,]$pg <- strsplit(pr2[f,]$pg,"/(\\w|\\.|\\+)+$")
  rm(f)
  
  pr2$pg <- tolower(pr2$pg)
  
  #reflect site hierarchy:
  if (file.exists(fSiteHierarchy))
    load(fSiteHierarchy) else
    {
      require("rvest")
      h <- read_html(url.root) %>%
        html_nodes("body > section:nth-child(3) > div:nth-child(1) > div > ul") %>% html_children()
      refSiteHierarchy <- setNames(data.frame(matrix(ncol = 3, nrow = 0),stringsAsFactors=F),
                                   c("parent","child.pg","child.lib"))
      invisible(lapply(h,function(x){
        parent <- html_children(x)[1] %>% html_text() %>% tolower() %>% gsub(pattern=" ",replacement="-")
        children <- html_children(x)[2] %>% html_children()
        invisible(lapply(children,function(y){
          child.pg <- y %>% html_children() %>% html_attr(name="href") %>% strsplit(split="?",fixed=T) %>% unlist() %>% head(1)
          child.lib <-y %>% html_children() %>% html_text() %>% gsub(pattern=" ",replacement="-")
          refSiteHierarchy[nrow(refSiteHierarchy)+1,] <<- c(parent,child.pg,child.lib)
        }))
      }))
      refSiteHierarchy$child.pg <- tolower(paste("/indicators",refSiteHierarchy$child.pg,sep="/"))
      refSiteHierarchy$child.path <- tolower(paste("/indicators",refSiteHierarchy$parent,refSiteHierarchy$child.lib,sep="/"))
      save(refSiteHierarchy,file=fSiteHierarchy)
      rm(url.root,fSiteHierarchy)
    }
  pr2 <- left_join(pr2,refSiteHierarchy[,c("child.pg","child.path")],by=c("pg"="child.pg"))
  pr2[!is.na(pr2$child.path),]$pg <- pr2[!is.na(pr2$child.path),]$child.path
  pr2$child.path <- NULL
  
  #update sum group:
  pr2 <- pr2 %>% group_by(pg) %>% mutate(n.sum=sum(n))
  
  return(pr2)
  
} #extendRepositoryPage

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

#extend repository page
pr <- extendRepositoryPage(pr)

#export data
f <- paste0("data/os-page_repository_",min(as.Date(a$date)),"-",max(as.Date(a$date)),".RData")
save(pr,file=f)
print(paste("page repository updated:",f))
rm(f)

#end

