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
model_data <- aa %>% spread(pg,count,fill=0) %>% 
  group_by(idVisit) %>% mutate_at(.funs=cumsum,.vars=vars(matches("/"))) %>%
  select(matches("(idVisit)|(step)|(pageIdAction)|(^/)"))

#get if visitor reach the target:
target.pages <- pr2[grep("indicators/financing-and-investment-dynamics",pr2$pg),]$pageIdAction
model_data <- model_data %>% mutate(target=if_else(lead(pageIdAction) %in% target.pages,1,0,missing=0)) 
model_data <- model_data %>% ungroup %>% select(-idVisit,-step,-pageIdAction)

smp_size <- floor(0.75*nrow(model_data))
train_ind <- sample(seq_len(nrow(model_data)),size=smp_size)
train <- model_data[train_ind,]
test <- model_data[-train_ind,]

predictors <- as.data.frame(select(train,-target),stringsAsFactors=F)
response <- as.factor(unlist(train[,"target"]))

#different models:

predictor_test <- as.data.frame(select(test,-target),stringsAsFactors=F)
response_test <- as.factor(unlist(test[,"target"]))

require(kernlab)
model.rf <- randomForest::randomForest(x = predictors, y = response)
model.glm <- glm(target~.,family=binomial,data=train)
model.rpart <- rpart::rpart(target~.,data=train)
model.lm <- lm(target~.,data=train)
model.ksvm <- ksvm(target~.,data=train,type="C-svc",kernel="vanilladot",C=1,cross=10,scaled=FALSE)
models <- list(lm=model.lm,ksvm=model.ksvm,glm=model.glm,rpart=model.rpart,rf=model.rf)
save(models,file="data/islastaction.RData")

prediction <- lapply(models,function(x){
  res <- predict(x, predictor_test)
  if (is.factor(res)) res <- as.numeric(levels(res))[res]
  res <- ifelse(res>0.5,1,0)
  return(res)
  })

accuracy <- lapply(prediction,function(x)sum(x==response_test)/nrow(predictor_test))
names(accuracy) <- names(prediction)
lapply(accuracy,round,3)

