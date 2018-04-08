#release aggregated datasets

#prepare
if (!exists("d"))
  load("data/d.RData")

ag <- d[-match("Live",names(d))]

#export data
f <- paste0("data/os-aggregates_",min(as.Date(ag[[1]][[1]]$date)),"-",max(as.Date(ag[[1]][[1]]$date)),".RData")
save(ag,file=f)
print(paste("updated aggregates released:",f))
rm(f,ag)