#simply calls runWASAwWarmup.R (in working_dir with default arguments)
#i.e. uses "paramset.txt" if present, otherwise 0

if ((exists("use_dir") && use_dir!="")) #use existing directory
  working_dir=use_dir else
{  
  working_dir="./thread1/" #with slash
  template_dir='init_config/' #if empty, the current directory is not emptied before use
  if (exists("sed") && sed) #do sediment
    template_dir='init_config_sed/' #if empty, the current directory is not emptied before use
  
  #prepare new working dir
    if (template_dir!="")
    {  
      unlink(paste0(working_dir, dir(path = working_dir, pattern = "[^(paramset.txt)]")), recursive=TRUE) #delete all but the paramset-file
      dir.create(working_dir)
      file.copy(from=paste0(template_dir,"."), to=working_dir, overwrite=TRUE, recursive=TRUE)
    }
}

if (exists("outfiles") && outfiles=="detail") #enable full detailed output
{  
  detail_file = dir(path = working_dir, recursive = TRUE, pattern = "outfiles.dat_detail")
  file.copy(from = paste0(working_dir,detail_file), to=sub(pattern = "_detail$", repl="", paste0(working_dir,detail_file)), overwrite = TRUE)
}


if ((exists("use_existing_run") && use_existing_run)) #use existing run
  res=0 else
  {
    #run WASA with warmup period until equilibrium  
    source("runWASAwWarmup.R")   
    res=runWASAwWarmup(working_dir=working_dir, detailed_output=detailed_output)   

    if (res<0)
      stop("something went wrong")
  }    
  

  
#compute performance measures  

  #settings
  target_component=c("River_Flow")   #compute goodness measures based on this flow component
  # needed for computation of goodness
  subbas_id=1:6                                     #load all discharge data
  start_date=as.POSIXct(ISOdate(2011, 6, 1, 0, min = 0, sec = 0, tz = "GMT"), tz = "GMT")   #begin of SESAM-II monitoring phase
  end_date  =as.POSIXct(ISOdate(2016,4,30,23, min = 0, sec = 0, tz = "GMT"), tz = "GMT")
  
  #start_date="obs"                                      #starting date for the calculation of the goodness measure: start with beginn of observation period, use all data
  #end_date=NULL
  
  if (exists("sed") && sed) #do sediment
  {
    target_component=c("River_Sediment_total","River_Flow") 
  }      

  source("compute_goodness_measures.R")
wasa_input_dir =paste(working_dir,"input/isabena_2010-2013/",sep="")      #wasa input directory (with trailing /)
wasa_output_dir=paste(working_dir,"output/isabena_2010-2013/",sep="")      #wasa output directory (with trailing /)

if (!exists("force_daily")) force_daily=FALSE #do not aggregate hourly results, unless enabled
measures = compute_goodness_measures(wasa_output_dir, wasa_input_dir, subbas_id = subbas_id, n_periods=1,start_date=start_date,end_date=end_date,target_component=target_component, force_daily = force_daily)


obj_fun_file="compute_obj_fun.R" #default

if (exists("sed") && !sed) #use water/sediment specific computation of objective function 
  if (file.exists("water_calib_files/compute_obj_fun.R"))
      obj_fun_file="water_calib_files/compute_obj_fun.R" else #use file in water directory
  if (file.exists("sed_calib_files/compute_obj_fun.R"))
    obj_fun_file="sed_calib_files/compute_obj_fun.R"  #use file in sediment directory
   
source(obj_fun_file, local=TRUE) 




