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

library(randomForest)
rf <- randomForest(x = predictors, y = response)

## once model done, we run it using test data and compare results to reality
predictor_test <- as.data.frame(test[,names(model_data)[!names(model_data) %in% c("idVisit","comeback")]],stringsAsFactors=F)
response_test <- as.factor(unlist(test[,"comeback"]))

## check result on test set
prediction <- predict(rf, predictor_test)
predictor_test$correct <- as.character(prediction) == as.character(response_test)

## How many were correct?
table(as.character(prediction) == as.character(response_test)) 
accuracy <- sum(predictor_test$correct) / nrow(predictor_test)

####### glm 
model.glm <- glm(comeback ~.,family=binomial(link='logit'),data=train[,tail(names(train),-1)])
predictor_test$correct <- NULL
prediction.glm <- predict(model.glm, predictor_test)
# fitted.results <- ifelse(fitted.results > 0.5,1,0)
predictor_test$correct <- as.character(prediction.glm) == as.character(response_test)
table(as.character(prediction.glm) == as.character(response_test)) 
accuracy <- sum(predictor_test$correct) / nrow(predictor_test)


