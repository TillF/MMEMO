#modifies WASA parameters according to parameter_file
#runs WASA for the first year of simulation period, until soil water storages are in equilibrium

#17.9.2019
#4.1.2018
#10.4.2018
#10.12.2015
#30.1.2015

runWASAwWarmup=function(
  working_dir="./",
  parameter_file=paste(working_dir,"paramset.txt",sep=""),
  obj_file=paste(working_dir,"curr_obj_fun_val.txt",sep=""),
  wasa_logfile_preruns=paste(working_dir,"wasa_prerun.log",sep=""),
  wasa_logfile=paste(working_dir,"wasa.log",sep=""),
  init_conds_dir=NULL
  ,...)
{
#  parameter_file      =paste(working_dir,"paramset.txt",sep="")                   #exchange file for communicate parameters written by PEST/genoud 
#  obj_file            =paste(working_dir,"curr_obj_fun_val.txt",sep="")                 #name of file to which to store the value of the objective function (read by PEST/genoud )
#  parameter_logfile   =paste(working_dir,"all_runs.log",sep="")                #name of file to which to append the value of the objective function plus the parameter set (leave empty of no logging is desired)
#  wasa_logfile_preruns=paste(working_dir,"wasa_prerun.log",sep="")          #logfile for wasa pre-run-phase
#  wasa_logfile        =paste(working_dir,"wasa.log",sep="")                         #logfile for wasa actual run
  
  #try to find wasa_input_dir, if not specified
  if (!exists("wasa_input_dir") || is.null(wasa_input_dir))
  {
    path2dodat=dir(working_dir, recursive = TRUE, pattern = "do.dat$") #search for do.dat
	path2dodat = path2dodat[!grepl(path2dodat, pattern="_prev/do\\.dat")]  #exclude any directories with "_prev"
    wasa_input_dir =paste0(working_dir, sub(path2dodat, pattern = "do.dat", repl=""))
  }
    
  fast_converge =TRUE #try to extrapolate change in state variables (only river storage so far) to accelerate conversion
  prerun_length=365 #length of prerun phase [days] (use 365 for a year and 28 for a month)
  
  wasa_executable_windows="WASA_release.exe"            #WASA executable for Windows
  wasa_executable_linux="./wasa.lin"            #WASA executable for Linux
  
  #pre-run phase
  max_pre_runs=20                                 #max number (years) of pre-run to achieve storage equilibrium (abortion criteria for pre-run phase)
  storage_tolerance=0.05                          # max allowed relative difference in storage volume between successive pre-runs (abortion criteria for pre-run phase)
  if (is.null(init_conds_dir))
    init_conds_dir="init_conds"

  # "none": delete any *.stats files in [wasa_output_dir] (WASA uses internal defaults)
  #"init_conds":         original pre-warmup conditions  (copy [wasa_input_dir]/init_conds/*.stat* [wasa_output_dir])
  #"init_conds_updated": updated  pre-warmup conditions  (copy [wasa_input_dir]/init_conds_updated/*.stat* [wasa_output_dir])
  # "": use existing *.stats in [wasa_output_dir]
  
 
  #try to read from a given file and wiat, in case it is not closed yet
  #(prevent suspected file caching issues)
  flushfile = function(filename)
  {
    con2 <- file(filename, open="r", blocking=TRUE)
    readLines(con2)
    while(isIncomplete(con2)) {
      Sys.sleep(1)
      z <- readLines(con2)
    }
    close(con2)
  }
  
  
  source("modify_wasa_input.R")          #load function
  
  if (!file.exists(parameter_file))
  {
    cat(paste(parameter_file,"not found, running WASA with existing parameters.\n"))
    parameters=data.frame(gw_delay_f=9999,soildepth_f=9999,kf_bedrock_f=9999,kfcorr=9999,f_gw_direct=9999,riverdepth_f=9999) #for denoting missing parameters in logfile
    }  else
    {
      parameters = as.data.frame(t(read.table(parameter_file, skip=1,header = T,row.names=1))) #import parameters from parameter control file
      modify_wasa_input(wasa_input_dir,parameters)
    }
    
  # modify do.dat for pre-run-phase
    target_file=paste(wasa_input_dir,"do.dat",sep="") #file that hold the parameters to be changed
    a=file.copy(target_file,paste(target_file,".full_time",sep=""))   #save original do.dat
    file_content = scan(target_file, what=character(), sep="\n")
    wasa_output_dir =paste0(wasa_input_dir, sub(file_content[3], pattern = "[ \t].*", repl=""))
    #remove simplify ".." in relative paths, if possible
	wasa_output_dir = gsub(wasa_output_dir, pattern = "/\\./", repl="/")
    wasa_output_dir = gsub(wasa_output_dir, pattern = "/[^/]*/\\.\\.", repl="")

    start_year=scan(text=file_content[4], what = numeric(), n=1)
    if (!is.finite(start_year)) stop("Could read start year from do.dat, please check format.")
    start_month=scan(text=file_content[6], what = numeric(), n=1)
    if (!is.finite(start_month)) stop("Could read start month from do.dat, please check format.")
    
    
    
	#repeat frst simulation year
	end_date_prerun=as.POSIXlt(ISOdate(start_year+1, start_month, 1, hour = 0, min = 0, sec = 0, tz = "GMT")-3600*24 )      #assemble date vector
    end_date_prerun=as.POSIXlt(ISOdate(start_year, start_month, 1, hour = 0, min = 0, sec = 0, tz = "GMT")+(prerun_length-1)*3600*24 )      #assemble date vector
    
    file_content[1]="v 1.2 Parameter specification for the WASA Model, pre-run phase (generated by runWASAwWarmup.R)"
    file_content[5]=end_date_prerun$year+1900  #configure model to run 1 year for prerun-phase
    file_content[7]=end_date_prerun$mon+1
	
	##repeat year prior to simulation period
	#start_date_prerun=as.POSIXlt(ISOdate(start_year, start_month,   1, hour = 0, min = 0, sec = 0, tz = "GMT")-(prerun_length)*3600*24 )      #assemble date vector
    #end_date_prerun  =as.POSIXlt(ISOdate(start_year, start_month, 1, hour = 0, min = 0, sec = 0, tz = "GMT")-1            *3600*24 )      #assemble date vector
    #file_content[1]="v 1.2 Parameter specification for the WASA Model, pre-run phase (generated by runWASAwWarmup.R)"
    #file_content[4]=start_date_prerun$year+1900  #configure model start
    #file_content[6]=start_date_prerun$mon+1  
    #file_content[5]=end_date_prerun$year+1900  #configure model end
    #file_content[7]=end_date_prerun$mon+1  
    
    file_content[36]=".t. //load state of storages from files (if present) at start (optional)"
    file_content[37]=".t. //save state of storages to files after simulation period (optional)"
    write.table(file_content, file = target_file, append = F, quote = F, row.names=F, col.names=F, sep="\t")   #rewrite file
    flushfile(target_file) #ensure file has been written completely

    if (file.exists(wasa_logfile_preruns))
      a=file.rename(wasa_logfile_preruns,paste(wasa_logfile_preruns,"_prev",sep=""))   #save previous runlog
    
    wasa_output_dir_noslash=substr(wasa_output_dir,1,nchar(wasa_output_dir)-1)
    unlink(paste0(wasa_output_dir_noslash,"_prev"), recursive = TRUE) #delete old previous output
    file.rename(from=wasa_output_dir, to=paste0(wasa_output_dir_noslash,"_prev")) #rename last run to "_prev"
    dir.create(wasa_output_dir) #create new output dir
    
    if (init_conds_dir!="") #use initial conditions stored in specified folder
    {
      statfiles=list.files(path = paste(wasa_input_dir,init_conds_dir,sep=""), pattern = ".stat*",full.names=T)
      if (length(statfiles)==0)
      {
        print  (paste0("Could not find any initial condition files (*.stat) in",wasa_input_dir,init_conds_dir, ", using WASA defaults."))
        warning(paste0("Could not find any initial condition files (*.stat) in",wasa_input_dir,init_conds_dir, ", using WASA defaults."))
        } else
    
      a=file.copy(statfiles,wasa_output_dir_noslash,overwrite = TRUE)   #restore initial conditions
    }
    
    
    if(.Platform$OS.type == "windows")  #set call to wrapper platform-specifically
         cmd=wasa_executable_windows else
         cmd=wasa_executable_linux
    cmd=paste(cmd," ",wasa_input_dir,"do.dat",sep="")
  
  storage_before_file=paste(wasa_output_dir,"storage.stats",sep="")       #get storage contents before run
  storage_sed_file=paste(wasa_output_dir,"sediment_storage.stat",sep="")       #get sediment storage contents before run
  storage_susp_sed_file=paste(wasa_output_dir,"susp_sediment_storage.stat",sep="")       #get sediment storage contents before run
  
  
  storage_sed_after =storage_susp_sed_after=NULL
  
  wasa_runtime_file=paste(wasa_output_dir,"parameter.out",sep="")       #WASA runtime file
  
  
  storages_log=NULL #collected record of storage developments
  r_val=max_pre_runs
  for (i in 1:max_pre_runs) #do pre-runs until equilibrium or maximum number achieved
  {
    unlink(wasa_runtime_file) #remove runtime file 

    if (!file.exists(storage_before_file))
  	storage_before=0 #no file available, start from scratch
  	else
  	storage_before =read.table(storage_before_file, skip=1,header = FALSE,row.names=1)
    
  	if (!file.exists(storage_sed_file))
  	  storage_sed_before=0 #no file available, start from scratch
  	else
  	  storage_sed_before = read.table(storage_sed_file, skip=1,header = TRUE)[,3]
  	
  	if (!file.exists(storage_susp_sed_file))
  	  storage_susp_sed_before=0 #no file available, start from scratch
  	else
  	  storage_susp_sed_before = read.table(storage_susp_sed_file, skip=1,header = TRUE)[,3]
  	
  	log_line=cbind(iteration=i, t(storage_before), storage_sed=sum(storage_sed_before), storage_susp_sed=sum(storage_susp_sed_before)) #log changes of storages during prerun
  	write.table(x=log_line, file=paste0(working_dir,"warmup_storages.log"), append=i!=1, sep="\t", row.names=FALSE, col.names=i==1, quote=FALSE)
  	
  	    cat(paste("\nPre-run iteration",i,"(of max",max_pre_runs,")\n"))
    zz <- file(wasa_logfile_preruns, open="at")       #send WASA screen output to logfile
    sink(zz)
    sink(zz, type="message")
    cat(paste("Pre-run iteration",i,"(of max",max_pre_runs,")\n"))

    failed=0
    while (is.null(a) || !file.exists(wasa_runtime_file)) #remove runtime file )
    {      
      a=system(command=cmd,intern = TRUE) #call WASA
      if (is.null(a) || !file.exists(wasa_runtime_file)) #sometimes, WASA seems to be called without a response. Then repeat
      {  
        con=file("emptycalls_pre.txt", open = "a+") #log empty calls
        writeLines(con = con, text=paste0(Sys.time(),": empty call in ", working_dir))
        a=system(command="ls",intern = TRUE) #try another call
        writeLines(con = con, text=a)
        
        close(con)
        if (failed > 10) {attr(a,"status") = "failed to execute"; break} #accept error
        failed = failed +1
        Sys.sleep(10*60) #perhaps waiting solves this problem
      }
    }

    cat(paste(a,collapse="\n")) 
    sink(type="message")
    sink()                            # remove redirect of screen output
    close(zz)
    if (!is.null(attr(a,"status"))) #runtime error, return -1 and error status
    {
      r_val=-1
      attr(r_val, "status")= attr(a,"status")
      return(r_val)
    }  
      
    #water storage
    storage_after_file=paste(wasa_output_dir,"storage.stats",sep="")       #get storage contents after run
    storage_after =read.table(storage_after_file, skip=1,header = F,row.names=1)
  
	if (identical(storage_before,0)) #ensure correct structure of variable
	{
	  storage_before = storage_after
	  storage_before$V2 = 0
	}
	contained_in_both = intersect(rownames(storage_after), rownames(storage_before)) #downward compatibility, when snow was not yet implemented
	
	#use only fileds contained both dataframes
	storage_after  = storage_after [contained_in_both, , drop=FALSE]
	storage_before = storage_before[contained_in_both, , drop=FALSE]
		
    rel_storage_change= abs(sum(storage_after)-sum(storage_before))
    if (sum(storage_before)!=0) rel_storage_change=rel_storage_change/sum(storage_before) #avoid NaNs sum(storage_before)==0
    if (rel_storage_change > storage_tolerance)    #check if storage changes are below tolerance limit
    {
      cat(paste(" large relative storage change (",sum(storage_before),"->",sum(storage_after),":",rel_storage_change,">",storage_tolerance,"), starting next iteration...\n"))
      
      #speed up convergence of storages
      storage_ratio = storage_before/storage_after
      nans=!is.finite(storage_ratio["river_storage",])
      storage_ratio["river_storage",nans]=1 #catch division by 0
      
      if (fast_converge & any(storage_ratio["river_storage",]>(1+2*storage_tolerance)) | any(storage_ratio["river_storage",]<1/(1+2*storage_tolerance)))
      {
        river_file = paste0(wasa_output_dir,"river_storage.stat"      )
		if (!file.exists(river_file)) next
		river_after =read.table(river_file, header = FALSE, nrows=1)
		if(any(grepl(unlist(river_after), pattern="UHG"))) next #don't do fastconverge with UHG routing
		river_after =read.table(river_file, skip=1,header = T)
        if (file.exists(paste0(wasa_output_dir,"river_storage.stat_start")))
          river_before=read.table(paste0(wasa_output_dir,"river_storage.stat_start"), skip=1,header = T)  else
          {river_before=river_after; river_before[,2]=0}  #no prior conditions available, assume zero
        print("fastconv")
        fast_converge_factor = river_after[,2]/river_before[match(river_after$Subbasin, river_before$Subbasin) ,2]
        fast_converge_factor[!is.finite(fast_converge_factor)] = 1 #set strange values to 1
        river_after[,2] = river_after[,2] * fast_converge_factor
        headerlines=scan(file = paste0(wasa_output_dir,"river_storage.stat"), what=character(), nlines = 2, sep="\n")
        write      (file=paste0(wasa_output_dir,"river_storage.stat"), x=headerlines)
        write.table(file=paste0(wasa_output_dir,"river_storage.stat"), river_after, col.names=FALSE, sep="\t", quote=FALSE, row.names=FALSE, append=TRUE)
        flushfile(paste0(wasa_output_dir,"river_storage.stat")) #ensure file has been written completely
      } 
      
      next
    } 
    else
    {
      r_val=c(required_pre_runs=i)
      cat(paste(" small relative storage change (",sum(storage_before),"->",sum(storage_after),":",rel_storage_change,"<",storage_tolerance,"), accepted as starting condition...\n"))
    }
#browser()
    #check sediment storage
    if (!file.exists(storage_sed_file))
      break #no file available, sediment storage will not be considered
    else
      storage_sed_after = read.table(storage_sed_file, skip=1,header = TRUE)[,3]
    
    if (!file.exists(storage_susp_sed_file))
      storage_susp_sed_after=0 #no file available, start from scratch
    else
      storage_susp_sed_after = read.table(storage_susp_sed_file, skip=1,header = TRUE)[,3]
    
    tot_sed_storage_after = sum(storage_sed_after) +sum(storage_susp_sed_after)
    tot_sed_storage_before= sum(storage_sed_before)+sum(storage_susp_sed_before)
    abs_storage_change= abs(tot_sed_storage_after-tot_sed_storage_before)
    rel_storage_change= abs_storage_change
    if (tot_sed_storage_before!=0) rel_storage_change=abs_storage_change/tot_sed_storage_before #avoid NaNs sum(tot_sed_storage_before)==0
    
    if (rel_storage_change > storage_tolerance & abs_storage_change>10)    #check if storage changes are below tolerance limit
    {
      #browser()
      
      cat(paste(" large relative sediment storage change (", tot_sed_storage_before,"->", tot_sed_storage_after,":",rel_storage_change,">",storage_tolerance,"), starting next iteration...\n"))
      
      #speed up convergence of sediment storages
      storage_ratio_sed      = sum(storage_sed_before)     /sum(storage_sed_after)
      storage_ratio_susp_sed = sum(storage_susp_sed_before)/sum(storage_susp_sed_after)
      if (!is.finite(storage_ratio_sed     )) storage_ratio_sed     =1  #catch division by 0
      if (!is.finite(storage_ratio_susp_sed)) storage_ratio_susp_sed=1
#      browser()
      if (FALSE & fast_converge) #disabled, as it tends to oscillate
      {  
        if(storage_ratio_sed>(1+1*storage_tolerance) | storage_ratio_sed<1/(1+1*storage_tolerance))
        {
          sediment_after =read.table(storage_sed_file, skip=1,header = T)
          if (file.exists(paste0(storage_sed_file,"_start")))
            sediment_before=read.table(paste0(storage_sed_file,"_start"), skip=1,header = T)  else
            {sediment_before=sediment_after; sediment_before[,3]=0}  #no prior conditions available, assume zero
          print("fastconv_sed")
          
          fast_converge_factor=0
          warmup_logfile=paste0(working_dir,"warmup_storages.log")
          if (file.exists(warmup_logfile))
          {  
            storages_log=read.table(warmup_logfile, header=TRUE)
            y=c(storages_log$storage_sed, sum(storage_sed_after))
            if (length(y)>3)
            {
              y_=y[length(y)-(2:0)] #get last three values
              if (diff(sign(diff(y_)))!=0) #do we have oscillations?
                fast_converge_factor = mean(y_) / y_[3] #adjust factor so that the next run starts with the average of the oscillation values
            }
          }
          if (fast_converge_factor==0) #factor not yet determined
          {  
            fast_converge_factor = sediment_after[,3]/sediment_before[match(sediment_after$Subbasin*100+sediment_after$particle_size_class, sediment_before$Subbasin*100+sediment_before$particle_size_class) ,3]
            fast_converge_factor[!is.finite(fast_converge_factor)] = 1 #set strange values to 1
          }
          
          sediment_after[,3] = sediment_after[,3] * fast_converge_factor
          headerlines=scan(file = storage_sed_file, what=character(), nlines = 2, sep="\n")
          write      (file= storage_sed_file, x=headerlines)
          write.table(file= storage_sed_file, sediment_after, col.names=FALSE, sep="\t", quote=FALSE, row.names=FALSE, append=TRUE)
          flushfile(storage_sed_file) #ensure file has been written completely
        }
        
        if(storage_ratio_susp_sed>(1+1*storage_tolerance) | storage_ratio_susp_sed<1/(1+1*storage_tolerance))
        {
          sediment_after =read.table(storage_susp_sed_file, skip=1,header = T)
          if (file.exists(paste0(storage_susp_sed_file,"_start")))
            sediment_before=read.table(paste0(storage_susp_sed_file,"_start"), skip=1,header = T)  else
            {sediment_before=sediment_after; sediment_before[,3]=0}  #no prior conditions available, assume zero
          print("fastconv_sed")
          
          storages_log=read.table("thread1/warmup_storages_new.log", header=TRUE)
          y=c(storages_log$storage_susp_sed, sum(storage_susp_sed_after))
          fast_converge_factor=0
          if (length(y)>3)
          {
            y_=y[length(y)-(2:0)] #get last three values
            if (diff(sign(diff(y_)))!=0) #do we have oscillations?
              fast_converge_factor = mean(y_) / y_[3] #adjust factor so that the next run starts with the average of the oscillation values
          }
          
          if (fast_converge_factor==0) #factor not yet determined
          {  
            fast_converge_factor = sediment_after[,3]/sediment_before[match(sediment_after$Subbasin*100+sediment_after$particle_size_class, sediment_before$Subbasin*100+sediment_before$particle_size_class) ,3]
            fast_converge_factor[!is.finite(fast_converge_factor)] = 1 #set strange values to 1
          }
          
          sediment_after[,3] = sediment_after[,3] * fast_converge_factor
          headerlines=scan(file = storage_susp_sed_file, what=character(), nlines = 2, sep="\n")
          write      (file= storage_susp_sed_file, x=headerlines)
          write.table(file= storage_susp_sed_file, sediment_after, col.names=FALSE, sep="\t", quote=FALSE, row.names=FALSE, append=TRUE)
          flushfile(storage_susp_sed_file) #ensure file has been written completely
        } 
        
      }
      next
    } 
    else
    {
      r_val=c(required_pre_runs=i)
      cat(paste(" small relative sediment storage change (", tot_sed_storage_before,"->", tot_sed_storage_after,":",rel_storage_change,"<",storage_tolerance,"), accepted as starting condition...\n"))
      break
    }

  }
  
  log_line=cbind(iteration=i+1, t(storage_after), sum(storage_sed_after), sum(storage_susp_sed_after)) #log changes of storages during prerun
  write.table(x=log_line, file=paste0(working_dir,"warmup_storages.log"), append=TRUE, sep="\t", row.names=FALSE, col.names = FALSE, quote=FALSE)
  
  
  
  #actual simulation
    if (file.exists(wasa_logfile))  
      a=file.rename(wasa_logfile,paste(wasa_logfile,"_prev",sep=""))   #save previous runlog
    a=file.rename(paste(target_file,".full_time",sep=""),target_file)   #restore original do.dat
    flushfile(target_file) #ensure file has been written completely
    cat(paste("Starting actual simulation...\n"))
    unlink(wasa_runtime_file) #remove runtime file 
    
    a=NULL
    zz <- file(wasa_logfile, open="at")       #send WASA screen output to logfile
    sink(zz)
    sink(zz, type="message")
    
    failed=0
    while (is.null(a) || !file.exists(wasa_runtime_file)) 
    {      
      a=system(command=cmd,intern = TRUE) #do actual WASA run
      if (is.null(a) || !file.exists(wasa_runtime_file)) #sometimes, WASA seems to be called without a response. Then repeat
      {  
        con=file("emptycalls.txt", open = "a+") #log empty calls
        writeLines(con = con, text=paste0(Sys.time(),": empty call in ", working_dir))
        a=system(command="ls",intern = TRUE) #try another call
        writeLines(con = con, text=a)
        
        close(con)
        if (failed > 10) {attr(a,"status") = "failed to execute"; break} #accept error
        failed = failed +1
        Sys.sleep(10*60) #perhaps waiting solves this problem
      }
    }
    
    
    cat(paste(a,collapse="\n")) 
      #cat(paste("Actual run needs to be activated!\n")) #debug message
    sink(type="message")
    sink()
    close(zz)

    if (!is.null(attr(a,"status"))) #runtime error, return -1 and error status
    {
      r_val=c(required_pre_runs=-1)
      attr(r_val, "status")= attr(a,"status")
      return(r_val)
    }
  
    cat(paste("Finished actual simulation...\n"))
  
    attr(r_val, "wasa_input_dir")  = wasa_input_dir  
    attr(r_val, "wasa_output_dir") = wasa_output_dir  
  
  return(r_val)
  
}  
  