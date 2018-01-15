#WASA calibration using PSO / DDS, assess reliability
# rerun best besults of replicate runs
# retrieve precision (95% quantile) of performance metrics from replicate runs
# store results in IP_precision.RData


library(ppso)
#base_dir="./replicates/"
#base_dir="E:/till/uni/r_lib/package_build/ppso-package/pso_vergleich/partxchange/replicates/"
base_dir="e:/till/uni/parameterisierung/Esera_2014/runs3/repl/"
#base_dir="./"

redo_run = FALSE #force redoing runs, even if already present (will be skipped otherwise)

#only used when redo_run=FALSE
redo_metrics =FALSE #force recalculation of metrics, even if already there

force_daily=TRUE #for hourly runs: force evaluation of performance measures in daily resolution

#select ONE of the following
#reps = dir(path = base_dir, pattern = "^B24_x3_[0-9]*$") #24 water
#reps = dir(path = base_dir, pattern = "^B24_x3_sed") #24 sed

#reps = dir(path = base_dir, pattern = "^B1_x3_[0-9]*$") #1 water
#reps = dir(path = base_dir, pattern = "^B1_x3_sed") #1 sed

#reps = dir(path = base_dir, pattern = "^A_u_24_wat_[0-9]*$") #24 water
reps = dir(path = base_dir, pattern = "^A_u_24_sed_") #24 sed

#reps = dir(path = base_dir, pattern = "^A_u_1_wat_") #1 water
#reps = dir(path = base_dir, pattern = "^A_u_1_sed_") #1 sed


runs2treat= reps

sed=any(grepl(x = reps, "sed")) #sediments or not?
res=ifelse(grepl(reps[1], pattern = "1_"), 1, 24)

# subs_runs = commandArgs(trailingOnly=TRUE)
# 
# if (!is.null(subs_runs)) #if restrained from outside, use subset
#   runs2treat=runs2treat[as.numeric(subs_runs)]

#do runs
for (run in runs2treat)
{  
  setwd(base_dir)
  setwd(run)
  print("")
  print(run)
  run=sub(run, pattern="/", repl="") #remove trailing slash
  
  sub_dir=""
     
  
  if (TRUE)
  {
    if (TRUE)
    {
  #extract best parameter set to generate "paramset.txt" 
    dds_res = read.table(paste0(sub_dir,"dds.log"), sep="\t", header=TRUE)
    if(is.factor(dds_res$objective_function)) dds_res$objective_function=as.numeric(as.character(dds_res$objective_function), as.is=FALSE)
    best = which.min(dds_res$objective_function)[1]
    best_set = dds_res[best, !(names(dds_res) %in% c("time", "objective_function", "worker"))]
    
    log_trans = grepl(names(best_set), pattern = "^log_") #find log-transformed parameters
    best_set[log_trans]=exp(best_set[log_trans]) #transform back to non-log scale
    names(best_set) = sub(names(best_set), pattern = "^log_", rep="") #remove "log_" from name
    
    best_dir="thread1" #directory to store the best run to
    run_done=FALSE
    pfile = paste0(best_dir,"/paramset.txt")
    if (!redo_run) #check if already performed and if this is the correct parameter set
    {
      if (file.exists(pfile))
      {
        pset_prev = read.table(pfile, sep="\t", header=TRUE)
        
        if (all(best_set == pset_prev$value) ||
            (all(abs(best_set - pset_prev$value)/ best_set< 1e-10)) ) #look at relative deviation (number of digits may be reduced in textfile)
          run_done=TRUE #dont repeat this run, because it is already there
      }  
    }
    
    templ_dir = sub(run, pattern = "[0-9]*$", repl="1") #where to get the init files
    templ_dir = paste0("../", templ_dir,"/")
    
    if (run_done) 
    {
      use_dir=paste0("../",run,"/",best_dir,"/") #use existing dir (instead of template_dir)
      
      if ((exists("redo_metrics") && redo_metrics) ) #use existing run, just recompute metrics
      {
        if (!file.exists("test_wrapper.R")) #get necessary R-files from temlate dir, if not present
        {
          file2copy = dir(path = templ_dir, pattern="\\.R$")
          file2copy = c(file2copy, dir(path = templ_dir, pattern="wasa_file_units.txt$"))
          file.copy(from=paste0(templ_dir, file2copy),                 to="./", overwrite=TRUE, recursive=TRUE) 
        }
        use_existing_run=TRUE
        source("test_wrapper.R") #do not re-run, just re-compute goodness
      } else
      {
        warning(paste0("skipped run ",run," because already there."))  #don't do anything
      } 
    }  else
    {  
      use_existing_run=FALSE
      unlink("thread1", force = TRUE, recursive=TRUE) #delete thread directory
      dir.create("thread1/")
      
      if (!file.exists("test_wrapper.R")) #get necessary R-files from temlate dir, if not present
      {
        file2copy = dir(path = templ_dir, pattern="\\.R$")
        file2copy = c(file2copy, dir(path = templ_dir, pattern="wasa_file_units.txt$"))
        file2copy = c(file2copy, dir(path = templ_dir, pattern="wasa_release.exe$"))
        file.copy(from=paste0(templ_dir, file2copy),                 to="./", overwrite=TRUE, recursive=TRUE) 
      }
      
      if (!sed)
        file.copy(from=paste0(templ_dir,"init_config/."),     to="thread1", overwrite=TRUE, recursive=TRUE) else
        file.copy(from=paste0(templ_dir,"init_config_sed/."), to="thread1", overwrite=TRUE, recursive=TRUE)
      
      
      #overwrite initial conditions obtained in this specific run
      if (!sed)
        file.copy(from=paste0("init_config/."),     to="thread1", overwrite=TRUE, recursive=TRUE) else
          file.copy(from=paste0("init_config_sed/."), to="thread1", overwrite=TRUE, recursive=TRUE)
      
      write(file="thread1/paramset.txt","#control file for modification of WASA-parameters, to be used by runWASAwWarmup.R (read)")
      write.table(file="thread1/paramset.txt",data.frame(parameter=names(best_set), value=as.numeric(t(best_set))), sep="\t", row.names=FALSE, quote=FALSE, append=TRUE)
      
      #run best parameter set
      outfiles="detail" #set detailed output
      
      use_dir="thread1/"
      source("test_wrapper.R") #re-run with best parameter set
    }
  }
  
   }  
  setwd("../")
}

#assemble goodness measures
parameterizations=data.frame()
for (run in runs2treat)
{  
  setwd(base_dir)
  setwd(run)
  print("")
  print(run)
  run=sub(run, pattern="/", repl="") #remove trailing slash
  sub_dir=""

  best_dir="thread1" #directory to store the best run to
  gfile = paste0(best_dir,"/curr_obj_fun_val_day.txt")
  g_measures = read.table(file=gfile, sep="\t", skip=3, header=FALSE)
  g2 = data.frame(t(g_measures[,2]))
  names(g2) = sub(g_measures[,1], pattern=":", repl="")
  parameterizations = rbind(parameterizations, data.frame(config=run, g2))      

  setwd("../")
}

#compute metrics 

metric_cols = c(
  "sub_wat_dyn", 
  "sub_wat_yil", 
  "out_wat_dyn", 
  "out_wat_yil", 
  "sub_sed_dyn", 
  "sub_sed_yil", 
  "out_sed_dyn", 
  "out_sed_yil")
parameterizations[, metric_cols] = NA


#compute metrics (for later evaluation of spread)
#water metrics
parameterizations$sub_wat_dyn = apply(    parameterizations[, grepl(pattern="rmse_qtotal(_|$)", x=names(parameterizations))] , MARGIN = 1, FUN=mean)   #mean of all subbasin RMSE
parameterizations$sub_wat_yil = apply(abs(parameterizations[, grepl(pattern="bias_total_sub" , x=names(parameterizations))]), MARGIN = 1, FUN=mean)  #mean of absolute volume error of subbasins
parameterizations$out_wat_dyn = parameterizations$rmse_qtotal_sub6
parameterizations$out_wat_yil = abs(parameterizations$bias_total_sub6)

if (sed)
{
  #sediment metrics
  parameterizations$sub_sed_dyn = apply(    parameterizations[, grepl(pattern="rmse_sed(_|$)", x=names(parameterizations))] , MARGIN = 1, FUN=mean)   #mean of all subbasin sediment RMSE
  parameterizations$sub_sed_yil = apply(abs(parameterizations[, grepl(pattern="bias_total_sed_sub" , x=names(parameterizations))]), MARGIN = 1, FUN=mean)  #mean of absolute volume error of subbasins
  parameterizations$out_sed_dyn = parameterizations$rmse_sed_sub6
  parameterizations$out_sed_yil = abs(parameterizations$bias_total_sed_sub6)
}

save(list = "parameterizations", 
                  file=paste0("rep_gmeas_",sub(x = reps[1], pattern="B([0-9]*).*", "\\1"),".RData"))

if (!sed)
{
hist(parameterizations$sub_wat_dyn)
hist(parameterizations$sub_wat_yil)
hist(parameterizations$out_wat_dyn)
hist(parameterizations$out_wat_yil)
} else
{
hist(parameterizations$sub_sed_dyn)
hist(parameterizations$sub_sed_yil)
hist(parameterizations$out_sed_dyn)
hist(parameterizations$out_sed_yil)
}


precision = function (x)
  {diff(quantile(x,c(0.025, 0.975)))} #inner 95% quantile interval of performance measures

if (sed)
  preci = apply(X = parameterizations[, c("sub_sed_dyn",
                                 "sub_sed_yil",
                                 "out_sed_dyn",
                                 "out_sed_yil" )], MAR=2,  FUN=precision) else
  preci = apply(X = parameterizations[, c("sub_wat_dyn",
                                          "sub_wat_yil",
                                          "out_wat_dyn",
                                          "out_wat_yil" )], MAR=2,  FUN=precision)
  

names(preci) = paste0("preci_", names(preci))

if (file.exists("IP_precision.RData"))
  load("IP_precision.RData") else
  {  
    IP_precision=data.frame(res=1, preci_sub_wat_dyn=NA,
                                   preci_sub_wat_yil=NA,
                                   preci_out_wat_dyn=NA,
                                   preci_out_wat_yil=NA,
                                   preci_sub_sed_dyn=NA,
                                   preci_sub_sed_yil=NA,
                                   preci_out_sed_dyn=NA,
                                   preci_out_sed_yil=NA)  
    IP_precision[2,] = IP_precision[1,]
    IP_precision[2,"res"] = 24
  }  


IP_precision[IP_precision$res==res, names(preci)] = preci
save(file="IP_precision.RData", list = "IP_precision")

