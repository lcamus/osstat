#get ref data
load("os-visits+actions_2017-10-02_2018-04-15.RData")
load("os-page_repository_2017-10-02-2018-04-15")
# load("./data/pr2.RData")

#extend actions
require(dplyr)
aa <- a[a$type!="search",]
aa <- left_join(aa,pr[,c("pageIdAction","pg")],by="pageIdAction")
aa[is.na(aa$pg),]$pg<- "ERR"

aa$count <- 1
require(tidyr)
model_data <- aa %>% spread(pg,count) %>%
  group_by(idVisit) %>%
  select(matches("(idVisit)|(^/)")) %>%
  summarise_all(sum,na.rm=T)

#get if visitor come back again:
vv <- v[,c("idVisit","visitorId","date")]
vv$date <- as.Date(vv$date)
model_data <- left_join(model_data,vv,by="idVisit") %>% distinct()
vv <- vv %>% group_by(visitorId) %>% distinct() %>% arrange(visitorId,date) %>% summarise(last.visit=last(date))
model_data <- left_join(model_data,vv,by="visitorId") %>% mutate(comeback=as.numeric(last.visit-date>0))
model_data <- model_data[,names(model_data)[!names(model_data) %in% c("visitorId","date","last.visit")]]

smp_size <- floor(0.75*nrow(model_data))
train_ind <- sample(seq_len(nrow(model_data)),size=smp_size)
train <- model_data[train_ind,]
test <- model_data[-train_ind,]

predictors <- as.data.frame(train[,names(model_data)[!names(model_data) %in% c("idVisit","comeback")]],stringsAsFactors=F)
response <- as.factor(unlist(train[,"comeback"]))

#different models:

# predictor.var <- names(model_data)[!names(model_data) %in% c("idVisit","comeback")]
predictor_test <- as.data.frame(test[,predictor.var],stringsAsFactors=F)
response_test <- as.factor(unlist(test[,"comeback"]))

require(kernlab)
model.rf <- randomForest::randomForest(x = predictors, y = response)
model.glm <- glm(comeback~.,family=binomial,data=train[-1])
model.rpart <- rpart::rpart(comeback~.,data=train[-1])
model.lm <- lm(comeback~.,data=train[-1])
model.ksvm <- ksvm(comeback~.,data=train[-1],type="C-svc",kernel="vanilladot",C=1,cross=10,scaled=FALSE)
models <- list(lm=model.lm,ksvm=model.ksvm,glm=model.glm,rpart=model.rpart,rf=model.rf)
save(models,file="ml_visitagain.RData")

prediction <- lapply(models,function(x){
  res <- predict(x, predictor_test)
  if (is.factor(res)) res <- as.numeric(levels(res))[res]
  res <- ifelse(res>0.5,1,0)
  return(res)
  })

accuracy <- lapply(prediction,function(x)sum(x==response_test)/nrow(predictor_test))
names(accuracy) <- names(prediction)
lapply(accuracy,round,3)

