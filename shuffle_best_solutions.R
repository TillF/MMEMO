#collect optimization results of available parameterizations and generate table
#to be used for restarting the optimization runs with these as initial estimates

resolution="1" #choose resultion (only respective parameterizations are scanned)
config="B"
n_best = 15 #number of best paramsets to retrieve overall
base_dir="./runs3/"
sed=TRUE

param_dirs = dir(path=base_dir, pattern=paste0("^",config, ".*_",resolution,"$" ))
#param_dirs = param_dirs[-7]

collected = data.frame()
#read data
  for (param_dir in param_dirs)
  {
    ddslog = paste0(base_dir, param_dir, "/dds.log")
    if (!file.exists(ddslog)) next
    dds_res = read.table(ddslog, sep="\t", header=TRUE)
    dds_res$objective_function=as.numeric(sub(pattern = ",", repl=".", as.character(dds_res$objective_function))) #quick fix for A+6_u_1 sed, don't know why this is needed
    
    best = which.min(dds_res$objective_function)[1]
    best_set = dds_res[best, ]
    if (grepl(x=param_dir, pattern="\\+2")) best_set$objective_function = 0 #treat subbasin-parameterisations differently, as they use RMSE as objective function
    collected = rbind(collected, cbind(param_dir=param_dir,best_set))
  
  }
which(is.na(as.numeric(sub(pattern = ",", repl=".", as.character(dds_res$objective_function)))))



collected = unique(collected) #remove duplicates
collected = collected[sort.int(collected$objective_function, index.return=TRUE)$ix,] #sort by performance
collected = collected [1:min(c(n_best, nrow(collected))),] #limit selection to desired number of parameter sets

write.table(file=paste0(base_dir,"/init_estimates_after_prerun/init_estimates",config, "_",resolution,ifelse(sed,"_sed",""),"_full" ,".txt"), collected[, ], sep="\t", quote=FALSE, row.names=FALSE)

write.table(file=paste0(base_dir,"/init_estimates_after_prerun/init_estimates",config, "_",resolution ,ifelse(sed,"_sed",""),".txt"), collected[, !(names(collected) %in% c("time","objective_function", "worker","param_dir"))], sep="\t", quote=FALSE, row.names=FALSE)







