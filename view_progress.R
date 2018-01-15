#WASA calibration using PSO / DDS
#display progress
#run best and uncalibrated parameter set


library(ppso)
base_dir="E:/till/uni/parameterisierung/Esera_2014/runs3/"
#base_dir="./"
#base_dir="E:/till/uni/parameterisierung/Esera_2014/runs3/repl/"
#base_dir="E:/till/uni/parameterisierung/Esera_2014/runs2/"
sed=TRUE #consider sediments

redo_run = FALSE #force redoing runs, even if already present (will be skipped otherwise)

#only used when redo_run=FALSE
  redo_metrics =TRUE #force recalculation of metrics, even if already there

force_daily=TRUE #for hourly runs: force evaluation of performance measures in daily resolution

#setwd(base_dir)
#runs2treat = dir(path = base_dir, pattern = "^A_u_1_sed_") #1 water

runs2treat=
  c(
"A_u_1/",
"A+1_u_1/",
"A+2_u_1/",
"A+3_u_1/",
"A+4_u_1/",
"A+5_u_1/",
"A+6_u_1/",
"A+7_u_1/",

"B_u_1/",
"B-1_u_1/",
"B-2_u_1/",
"B-3_u_1/",
"B-4_u_1/",
"B-5_u_1/",
"B-6_u_1/",
"B-7_u_1/",

"A_u_24/",
"A+1_u_24/",
"A+2_u_24/",
"A+3_u_24/",
"A+4_u_24/",
"A+5_u_24/",
"A+6_u_24/",
"A+7_u_24/",

"B_u_24/",
"B-1_u_24/",
"B-2_u_24/",
"B-3_u_24/",
"B-4_u_24/",
"B-5_u_24/",
"B-6_u_24/",
"B-7_u_24/"


  )

subs_runs = commandArgs(trailingOnly=TRUE)
if (length(subs_runs)!=0) #if restrained from outside, use subset
  runs2treat=runs2treat[eval(parse(text=subs_runs))]

for (run in runs2treat)
{  
  setwd(base_dir)
  setwd(run)
  print("")
  print(run)
  run=sub(run, pattern="/", repl="") #remove trailing slash
  
  sub_dir=""
  if (sed==FALSE & file.exists("water_calib_files"))
    sub_dir="water_calib_files/"       #results of water calibration have already been moved to subdir
     
  if (FALSE)
  {
    plot_optimization_progress(logfile = paste0(sub_dir,"dds.log"), projectfile = paste0(sub_dir,"dds.pro"), verbose = TRUE, cutoff_quantile = .8)
    #save plots as png
    savePlot(filename = paste0(sub_dir,"dds_progress1_",run,".png"), type = "png", device = dev.prev())
    savePlot(filename = paste0(sub_dir,"dds_progress2_",run,".png"), type = "png", device = dev.cur())
  }
  
  if (TRUE)
  {
    if (FALSE)
    {
  #extract best parameter set to generate "paramset.txt" 
    dds_res = read.table(paste0(sub_dir,"dds.log"), sep="\t", header=TRUE)
    if(is.factor(dds_res$objective_function)) dds_res$objective_function=as.numeric(as.character(dds_res$objective_function), as.is=FALSE)
    best = which.min(dds_res$objective_function)[1]
    best_set = dds_res[best, !(names(dds_res) %in% c("time", "objective_function", "worker"))]
    
    log_trans = grepl(names(best_set), pattern = "^log_") #find log-transformed parameters
    best_set[log_trans]=exp(best_set[log_trans]) #transform back to non-log scale
    names(best_set) = sub(names(best_set), pattern = "^log_", rep="") #remove "log_" from name
    
    best_dir=ifelse(!sed,"thread1_best","thread1_best_sed") #directory to store the best run to
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
    
    if (run_done) 
    {
      if ((exists("redo_metrics") && redo_metrics) ) #use existing run, just recompute metrics
      {
          use_existing_run=TRUE
          use_dir=paste0(best_dir,"/") #use existing dir 
          source("test_wrapper.R") #do not re-run, just re-compute goodness
      } else
      {
        warning(paste0("skipped run ",run," because already there."))  #don't do anything
      } 
    } else
    {  
      use_existing_run=FALSE
      unlink("thread1", force = TRUE, recursive=TRUE) #delete thread directory
      dir.create("thread1/")
      if (!sed)
      file.copy(from=paste0("init_config/."),     to="thread1", overwrite=TRUE, recursive=TRUE) else
      file.copy(from=paste0("init_config_sed/."), to="thread1", overwrite=TRUE, recursive=TRUE)
  
      write(file="thread1/paramset.txt","#control file for modification of WASA-parameters, to be used by runWASAwWarmup.R (read)")
      write.table(file="thread1/paramset.txt",data.frame(parameter=names(best_set), value=as.numeric(t(best_set))), sep="\t", row.names=FALSE, quote=FALSE, append=TRUE)
      
      #run best parameter set
      outfiles="detail" #set detailed output
      use_dir="./thread1/" #use existing dir (instead of template_dir)
      source("test_wrapper.R") #re-run with best parameter set
      
      unlink(best_dir, force = TRUE, recursive = TRUE) #delete any old best directory
      file.rename(from="thread1", to = best_dir) #
    }
  }
  
  # run uncalibrated model
   if (TRUE)
    {   
    nocal_dir=ifelse(!sed,"thread1_nocal","thread1_nocal_sed") #directory to store the uncalibrated run to
  
    run_done=FALSE
    pfile = paste0(nocal_dir,"/wasa.log")
    if (!redo_run && file.exists(pfile)) #check if already performed
                 run_done=TRUE #dont repeat this run, because it is already there
    
     if (run_done) 
    {
      if ((exists("redo_metrics") && redo_metrics) ) #use existing run, just recompute metrics
      {
        use_existing_run=TRUE
        use_dir=paste0(nocal_dir,"/") #use existing dir 
        source("test_wrapper.R") #do not re-run, just re-compute goodness
      } else
      {
        warning(paste0("skipped run ",run," because already there.")) #don't do anything
      } 
     } else 
      {  
        use_existing_run=FALSE
        unlink("thread1", force = TRUE, recursive=TRUE) #delete thread directory
        dir.create("thread1/")
        
        if (!sed)
        file.copy(from=paste0("init_config/."),     to="thread1", overwrite=TRUE, recursive=TRUE) else
        file.copy(from=paste0("init_config_sed/."),     to="thread1", overwrite=TRUE, recursive=TRUE) 
        # if (sed)
        # {  
        #   sedfiles    =dir(path = "init_config_sed", recursive = TRUE)
        #   non_sedfiles=dir(path = "init_config"    , recursive = TRUE)
        #   copyfiles=setdiff(sedfiles, non_sedfiles[!grepl(non_sedfiles, pattern = "outfiles|do\\.dat")]) #find files that are extra in sediment version plus outfiles and do.dat
        #   copyfiles=copyfiles[!grepl(copyfiles, pattern = "\\.stat")] #remove *.stat
        #   file.copy(from=paste0("init_config_sed/", copyfiles), to=paste0("thread1/",copyfiles), overwrite=TRUE, recursive=TRUE)
        # 
        # }

        if (file.exists("thread1/input/isabena_2010-2013/init_conds_org"))  #use unmodified initial conditions, if available
        {
          unlink("thread1/input/isabena_2010-2013/init_conds", force=TRUE, recursive=TRUE)       #delete updated initial conds
          file.rename(from="thread1/input/isabena_2010-2013/init_conds_org", to="thread1/input/isabena_2010-2013/init_conds")  #use original conds
        }
          
        outfiles="detail" #set detailed output  
        #file.rename(from="thread1/input/isabena_2010-2013/outfiles.dat_detail", to = "thread1/input/isabena_2010-2013/outfiles.dat/") #enable detailed model output
        use_dir="thread1/"
        source("test_wrapper.R") #re-run with default parameter set
        nocal_dir=ifelse(!sed,"thread1_nocal","thread1_nocal_sed") #directory to store the best run to
        unlink(nocal_dir, force = TRUE, recursive = TRUE) #delete any old best directory
        file.rename(from="thread1", to = nocal_dir) #
      }
    }
   
  }  
  setwd("../")
}

  