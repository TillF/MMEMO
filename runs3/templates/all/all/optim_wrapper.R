#searches free thread directory among the directories "thread[nn]" (need to be present)
#modifies WASA parameters in parameter_file "paramset.txt" 
#calls runWASAwWarmup to run WASA and compute_goodness_measures to compute performance

#26.2.2019

optim_wrapper=function(parms=NULL)
#  names(parms)=c("gw_delay_f","soildepth_f","kf_bedrock_f","riverdepth_f","kf_scale_f","ksat_factor")[1:length(parms)] #name vector
{
  #return(runif(1))
  update_initial_conditions=TRUE                   # update initial conditions, if a new best has been achieved
  
 if (is.null(parms)) 
  {
    parms=rep(1, length(parnames)) #use 1's as default
    names(parms)=parnames #parnames need to be defined in the global scope
  }
  
    names(parms) = sub(pattern="^x\\.", replacement="", x=names(parms)) #MBO prependens "x.", remove it
#  return(runif(n=1)) #debug setting
  
  log_trans = grepl(names(parms), pattern = "^log_") #find log-transformed parameters
  parms[log_trans]=exp(parms[log_trans]) #transform back to non-log scale
  names(parms) = sub(names(parms), pattern = "^log_", rep="") #remove "log_" from name
  
  #settings
  n_threads=100 #number of parallel threads allowed
  template_dir='init_config/' #if empty, all directories ("thread[nn]") need to exist;
                  #otherwise (with trailing slash), template_dir is copied if the directory is not found
  
  if ("riv_erodibilityfactor" %in% names(parms))
        template_dir='init_config_sed/' #this is the sediment calibration, use other template dir

#  if (any(is.null(names(parms)))) #assign names to parameter vector, if missing
#  {  
#    parnames=c("gw_delay_f","soildepth_f","kf_bedrock_f","riverdepth_f","kf_scale_f","ksat_factor",
#               "riv_depth_f", "riv_width_f", "riv_side_ratio_f", "riv_bottom_width_of_floodplain_f", "riv_side_ratio_floodplains_f", "riv_channel_slope_f", "riv_length_f", "riv_manningn_f", "riv_manningn_floodplain_f", "riv_baseflowalphafactor_f", "riv_Muskingum_X_f","riv_Muskingum_K_f", "riv_Ksat_f")
#    names(parms)=parnames[1:length(parms)] #name vector
#  }

  print("choosing working dir")
  working_dir=""
  random_number=as.integer(runif(1,0,1e7))
  
  for (i in 1:n_threads)       #choose working directory according to already active threads marked by lockfiles
  {
    slave_no = try(mpi.comm.rank(), silent = TRUE) #try to get the slave ID, if working in Rmpi-mode
    if (i==1 && class(slave_no)=="integer")
      no=slave_no else  #try to use a thread-dir corresponding to slave-id
      no=i
    
    working_dir_t=paste("thread", no,sep="")
    lockfile=paste("thread", no,".lock",sep="")
    
    if (!file.exists(lockfile))    
    {
      if (template_dir=="" && !file.exists(working_dir_t)) next #thread dir not found and no template available
      
      lock_hdl <- file(lockfile, "w+")
     cat(paste0(random_number,"_",slave_no),file=lock_hdl)  #write random number and slave-id to lockfile
  	  flush(lock_hdl)                   #ensure that this isnt stuck in the I/O cache
      close(lock_hdl)

	  file_cont=scan(file=lockfile,what=character(),sep="_")
	  r_num = file_cont[1] #use only the random number, discard the slave-id
	  
      if (!is.null(file_cont[1]) &  !is.na(file_cont[1]) &  all(random_number==r_num)) #check if only this thread wrote to the lockfile. This is done because successive file.exist and file.create or file.copy may be too slow and don't ensure exclusive access
      {
        working_dir = paste0(working_dir_t,"/")
        if (!file.exists(working_dir_t)) 
          dir.create(working_dir_t)
        else
          if (template_dir!="")
          {    
            #        unlink(paste0(working_dir,"output/"), recursive=TRUE, force=TRUE)  #better remove everything in output 
            #file.rename(from=paste0(working_dir,"input/"), to=paste0(working_dir,"input_prev"))  #save previous run
            
            #infer input dir from location of do.dat
            wasa_input_dir = dir(path = working_dir_t, include.dirs = TRUE, pattern="^do\\.dat$", recursive=TRUE, full.names = T)
            wasa_input_dir=wasa_input_dir[!grepl(wasa_input_dir, pattern = "_prev/do.dat$")] #ignore the _prev version
            wasa_input_dir = sub(wasa_input_dir, pattern="/do\\.dat$", replacement = "" )
            
            unlink(paste0(wasa_input_dir,"_prev"), recursive = TRUE) #delete old previous
            file.rename(from=wasa_input_dir, to=paste0(wasa_input_dir,"_prev")) #rename last run to "_prev"
          }
        
        if (template_dir!="")
          file.copy(from=paste0(template_dir,"."), to=working_dir_t, overwrite=TRUE, recursive=TRUE)
   
        break
      }
    }
  }

  if (working_dir=="") stop("no free thread found")              #no free thread, this is certainly wrong
  
#write parameters to file paramset.txt 
print("write param file")
  parameter_file=paste(working_dir,"paramset.txt",sep="")                   #exchange-file for communicating parameters with runWASAwWarmup.R
  a=file.rename(parameter_file,paste(parameter_file,"_prev",sep=""))   #keep previous version of parameter file
  #write file header
  write.table("#control file for modification of WASA-parameters, to be used by optim_wrapper.R (write) and runWASAwWarmup.R (read)", file = parameter_file, quote = FALSE,row.names = FALSE,col.names = FALSE)
  write.table("parameter\tvalue", file = parameter_file, quote = FALSE,row.names = FALSE,col.names = FALSE,append=TRUE)
  write.table(parms, file = parameter_file, append = TRUE, quote = FALSE, sep = "\t",row.names = T, col.names=FALSE)


#run WASA with warmup period until equilibrium  
  print("starting wasa")
  init_conds_dir = "init_conds/"
  source("runWASAwWarmup.R")   
  res=runWASAwWarmup(working_dir=working_dir, init_conds_dir = init_conds_dir, detailed_output=detailed_output)   
  
  print("computing objfun")
  if (res<0) #stop execution right away
    stop("something went wrong")
  
  wasa_input_dir = attr(res, "wasa_input_dir")
  wasa_output_dir =attr(res, "wasa_output_dir")
   

  print("computing objfun")
if (res<0)
{
  measures = data.frame(ns_coeff=-9999)
  return_val = 9999 #something went wrong
} else 
{  
#compute performance measures  

  #settings
  target_component=c("River_Flow")   #compute goodness measures based on this flow component
  # needed for computation of goodness measures
  subbas_id=1:6                                     #load all discharge data
  start_date=as.POSIXct(ISOdate(2011, 6, 1, 0, min = 0, sec = 0, tz = "GMT"), tz = "GMT")   #begin of SESAM-II monitoring phase
  end_date  =as.POSIXct(ISOdate(2016,4,30,23, min = 0, sec = 0, tz = "GMT"), tz = "GMT")
  
  #start_date="obs"                                      #starting date for the calculation of the goodness measure: start with beginn of observation period, use all data
  #end_date=NULL
  
  if ("riv_erodibilityfactor" %in% names(parms)) #sediment calibration
  {
      target_component=c("River_Sediment_total","River_Flow") 
  }    
  
  source("compute_goodness_measures.R")
 
  measures = compute_goodness_measures(wasa_output_dir, wasa_input_dir, subbas_id, n_periods=1,start_date=start_date,end_date=end_date,target_component=target_component) 
  source("compute_obj_fun.R", local=TRUE)
  
}
  

print("saving objfun")
#save performance measures in log file (all_runs.log)
  parameter_logfile   =paste(working_dir,"all_runs.log",sep="")
  measures[measures==99999 | measures==-99999] =NA  #mask nodata values as such
  #record parameters in general logfile
  
  if (parameter_logfile!="")
  {
    if (file.exists(parameter_logfile))
    {
      append=TRUE                #append to existing log file
      col.names=FALSE
    } else
    {
      append=FALSE                 #create new log file
      col.names=TRUE
    }
    
    log_line=cbind(date=format(Sys.time()),t(parms),pre_runs=res["required_pre_runs"],measures)
    write.table(log_line, file = parameter_logfile, append = append, quote = F,row.names=F,col.names=col.names,sep="\t")
  }
 
print("check update IC")
#update initial conditions, when a new optimum was achieved
  if (update_initial_conditions && (!is.na(res["required_pre_runs"]) & res["required_pre_runs"]> 1))
  {  
    #browser()
    #check if this is a new optimum
	print("check IC update") #dd
    library(ppso)
    gbest = request_object ("globvars$fitness_gbest", verbose_slave=FALSE) #get current 
    
    if (gbest > return_val) #is this a new optimum for the current particle?
    {
      print("obj fun improved, try update IC") #dd
	  #update initial conditions from current run
      statfiles=list.files(path = wasa_output_dir, pattern = ".stat[s]*_start$",full.names=T)
      
      if (length(statfiles)!=0)
      {
        print("obj fun improved,  updating IC") #dd
        dest_dir  = dir(path = template_dir, pattern =sub(init_conds_dir, pattern = "/", replacement = "$"), recursive = TRUE, include.dirs = TRUE )
        dest_dir  =paste0(template_dir,dest_dir,"/")
        dest_dir_org=paste0(sub(pattern = "/$", repl="", dest_dir),"_org") #without slash
        print(paste("updating initial conditions from", wasa_input_dir,"to", dest_dir))
        if (!file.exists(dest_dir_org)) #save original files
        {  
          a = dir.create(dest_dir_org)
          a = file.copy(from = paste0(dest_dir,"."), to=dest_dir_org, copy.date = TRUE, recursive=TRUE)
        }  
        a = file.copy(statfiles, dest_dir, overwrite = TRUE)   #update initial conditions
        statfiles=list.files(path = dest_dir, pattern = ".stat[s]*_start$", full.names=T) #remove "_start" from name
        a = file.rename(statfiles, sub(pattern = "_start$", repl="", statfiles))   
      }  
    }
  }
  
  
  

unlink(lockfile)      #delete lockfile
print("returning val")
return( return_val)  #return objective function




}
