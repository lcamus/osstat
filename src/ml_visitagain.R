#get ref data
load("os-visits+actions_2017-10-02_2018-04-15.RData")
load("./data/pr2.RData")

#extend actions
require(dplyr)
aa <- a[a$type!="search",]
aa <- left_join(aa,pr2[,c("pageIdAction","pg")],by="pageIdAction")
aa[is.na(aa$pg),]$pg<- "ERR"

aa$count <- 1
require(tidyr)
model_data <- aa %>% spread(pg,count) %>%
  group_by(idVisit) %>%
  select(matches("(idVisit)|(^/)")) %>%
  summarise_all(sum,na.rm=T)

smp_size <- floor(0.75*nrow(model_data))
train_ind <- sample(seq_len(nrow(model_data)),size=smp_size)
train <- model_data[train_ind,]
test <- model_data[-train_ind,]

predictors <- train[,]