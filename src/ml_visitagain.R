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

predictor.var <- names(model_data)[!names(model_data) %in% c("idVisit","comeback")]
predictor_test <- as.data.frame(test[,predictor.var],stringsAsFactors=F)
response_test <- as.factor(unlist(test[,"comeback"]))

model.rf <- randomForest::randomForest(x = predictors, y = response)
model.glm <- glm(comeback~.,family=binomial,data=train[,c(predictor.var,"comeback")])
model.rpart <- rpart::rpart(comeback~.,data=train[,c(predictor.var,"comeback")])
models <- list(rf=model.rf,glm=model.glm,rpart=model.rpart)

# prediction <- as.data.frame(matrix(nrow=nrow(test),ncol=0),stringsAsFactors=F)
prediction <- lapply(models,function(x){
  res <- predict(x, predictor_test)
  if (is.factor(res)) res <- as.numeric(levels(res))[res]
  res <- ifelse(res>0.5,1,0)
  return(res)
  })
# prediction$rf <- predict(model.rf,predictor_test)
# prediction$glm <- ifelse(predict(model.glm, predictor_test)>0.5,1,0)
# prediction$rpart <- ifelse(predict(model.rpart, predictor_test)>0.5,1,0)

accuracy <- lapply(prediction,function(x)sum(x==response_test)/nrow(predictor_test))
names(accuracy) <- names(prediction)

