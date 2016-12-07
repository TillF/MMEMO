#process the comparison table and determine the respective metrics

base_dir="./runs2/"


configs_dirs=read.table(file="configuration.txt", sep="\t", header=FALSE, skip=2, nrows=1) #read configurations directories
configs       =read.table(file="configuration.txt", sep="\t", header=TRUE, skip=1, stringsAsFactors=FALSE) #read configurations

parameterizations_header = read.table(file="parameterizations.txt", sep="\t", header=FALSE, nrows=3) #read parameterizations.txt
parameterizations=read.table(file="parameterizations.txt", sep="\t", header=TRUE, skip=3, stringsAsFactors=FALSE) #read parameterizations.txt

assemble_config= function(parameterization_id)
#assemble all necessary files according to info in "configs" into a directory residing in base_dir
{
  row_ix   =which(parameterizations$parameterization_ID==parameterization_id)
  config_id=parameterizations$config_ID[row_ix]
  t_res    =parameterizations$resolution[row_ix]
  config_row = which(configs$configuration.ID == config_id) #corresponding row in "configs"
  
  target_dir=paste0(base_dir,parameterization_id)
  
  target_dir=paste0(target_dir,"/")
           
  if (!dir.create(target_dir)) stop(paste("Couldn't create ",target_dir, sep=""))
  
  #copy common files for calibration (scripts, executable, etc.); default input
    src_files=dir(path=paste0(base_dir,"templates/all/all/",""), full.names=TRUE, all.files = TRUE, no.. = TRUE)    
    file.copy(from=src_files, to=target_dir, recursive=TRUE)
    jobscr=scan(file=paste0(target_dir,"job_parallel.pbs"), what = character(), sep = "\n")
    jobscr=sub(pattern="config_id", repl=paste0(config_id,"_u_",t_res), x=jobscr)
    write(x = jobscr, file = paste0(target_dir,"job_parallel.pbs"))  #modify jobscript to contain parameterization_id
    
  
    #target_dir=paste0(target_dir,"/all/",t_res, "/")  
  
  #copy time-dependent common files to directory
  #if (!dir.create(target_dir)) stop(paste("Couldn't create ",target_dir, sep=""))
  #copy common files
    #src_files=dir(paste0(base_dir,"templates/all/all/",""), full.names=TRUE)    
    #file.copy(from=src_files, to=target_dir, recursive=TRUE)
    #specific to temporal resolution
    src_files=dir(paste0(base_dir,"templates/all/",t_res,""), full.names=TRUE)    
    file.copy(from=src_files, to=target_dir, recursive=TRUE)
  
  #copy model enhancements (overwrites default files, if activated)
    for (i in 3:ncol(configs_dirs))
    {
      config_dir=as.character(configs_dirs[1,i])
      if (is.na(config_dir)) next
      if (configs[config_row,i] != 0)  #if an enhancement is selected for this configuration, copy respective files
      {
        #common files
        src_files=dir(paste0(base_dir,"templates/",config_dir,"/all/"), full.names=TRUE)    
        file.copy(from=src_files, to=target_dir, recursive=TRUE)
        #specific to temporal resolution
        src_files=dir(paste0(base_dir,"templates/",config_dir,"/",t_res), full.names=TRUE)    
        file.copy(from=src_files, to=target_dir, recursive=TRUE)
      }
        
    }  
  
  if(grepl(pattern="^A", x=configs$configuration.ID[config_row])) #for all A-parameterisations, we are done
    return()
  
  #special cases that don't fit into the scheme above because of interdependencies of model improvements
  #B* (basic files for B with interactions)
  if(grepl(pattern="^B", x=configs$configuration.ID[config_row]))
  {  
    src_files=dir(paste0(base_dir,"templates/B/"), full.names=TRUE)    
    file.copy(from=src_files, to=target_dir, recursive=TRUE)  
  }  
  
  #B-3
  if (configs[config_row,"landuse"]             == 0 &&
        configs[config_row,"LAI.C.factor"]      == 1 &&
        configs[config_row,"LAI.C.seasonality"] == 1 
    )
  {  
    
    #c-seasons, rainyseason.dat
    src_files=dir(paste0(base_dir,"templates/ts_seasons/all/"), full.names=TRUE)    
    file.copy(from=src_files, to=target_dir, recursive=TRUE)  
    
    #svc.dat
    src_files=dir(paste0(base_dir,"templates/B-3/"), full.names=TRUE)    
    file.copy(from=src_files, to=target_dir, recursive=TRUE)  

    #sdr_lu.dat
    src_files=dir(paste0(base_dir,"templates/ms_conn_ix/all/"), full.names=TRUE)    
    file.copy(from=src_files, to=target_dir, recursive=TRUE)  

    #terrain.dat 
    src_files=dir(paste0(base_dir,"templates/ms_flow_conc/all/"), full.names=TRUE)    
    file.copy(from=src_files, to=target_dir, recursive=TRUE)  
  }

  #B-4  
  if (configs[config_row,"LAI.C.factor"]            == 0  )
  {  
    #svc.dat, ...
    src_files=dir(paste0(base_dir,"templates/B-4/"), full.names=TRUE)    
    file.copy(from=src_files, to=target_dir, recursive=TRUE)  
  }
  
  #B-5  
  if (configs[config_row,"LAI.C.seasonality"]            == 0  )
  {  
    #svc.dat
    src_files=dir(paste0(base_dir,"templates/B-5/"), full.names=TRUE)    
    file.copy(from=src_files, to=target_dir, recursive=TRUE)  
    
    unlink(paste0(target_dir,"init_config/input/isabena_2010-2013/Hillslope/c_seasons.dat")) 
    unlink(paste0(target_dir,"init_config/input/isabena_2010-2013/Hillslope/rainy_season.dat")) 
  }
  
  
  #B-6
  if (configs[config_row,"connectivity.index"]            == 0 )
    #sdr_lu.dat
    unlink(paste0(target_dir,"init_config/input/isabena_2010-2013/Hillslope/sdr_lu.dat")) 
    
  #B-7
  if (configs[config_row,"flow.concentration"]            == 0 )
  {  
    #terrain.dat 
    src_files=dir(paste0(base_dir,"templates/B-7/"), full.names=TRUE)    
    file.copy(from=src_files, to=target_dir, recursive=TRUE)  
  }
  
  
}


treat_parameterization = function(parameterization_id){
#assemble, run and evaluate a parameterisation
  
  #lookup parameterization table 
  #assemble/prepare requested configuration
  assemble_config(parameterization_id)  
  #(calibrate or) simply run parameterization
  #compute metrics
  #store in file  
  print("Don't forget to convert linebreaks of job_parallel.pbs manually!")
}


#finished

# #running
# parameterization_id="A_u_24" 
parameterization_id="A+1_u_24"
# 
  parameterization_id="A+2_u_24"
    parameterization_id="A+3_u_24"
   parameterization_id="A+4_u_24"
   parameterization_id="A+5_u_24"
#   
   parameterization_id="A+6_u_24"
   parameterization_id="A+7_u_24"

   parameterization_id="B_u_24" 
   parameterization_id="B-1_u_24" 
   parameterization_id="B-2_u_24" 
   parameterization_id="B-3_u_24" 
   parameterization_id="B-4_u_24" 
   parameterization_id="B-5_u_24" 
   parameterization_id="B-7_u_24" 
#       
#    
#    parameterization_id="A_u_1"
#    parameterization_id="A+1_u_1"
#    # 
#    # 
#    parameterization_id="A+2_u_1"
#    parameterization_id="A+3_u_1"
#    parameterization_id="A+4_u_1"
#    parameterization_id="A+5_u_1"
#    parameterization_id="A+7_u_1"
#    
#    
#    #prepared   
# 
# #   #water
#   
#   #to be prepared
#  
# 
#    parameterization_id="B_u_1" 
#   parameterization_id="B-1_u_1" 
#   parameterization_id="B-2_u_1" 
#  parameterization_id="B-3_u_1" 
#   parameterization_id="B-4_u_1" 
#   parameterization_id="B-5_u_1" 
#   parameterization_id="B-7_u_1" 
# 



#treat_parameterization("B-3_u_1")



#treat_parameterization(parameterization_id)
params2gen = parameterizations$parameterization_ID
params2gen = params2gen[!grepl(pattern = "_c", params2gen)] #only use "_u_"
#params2gen = params2gen[grepl(pattern = "B", params2gen)] #all B
#params2gen = params2gen[grepl(pattern = "_1", params2gen)] # all 1
params2gen = params2gen[grepl(pattern = "B", params2gen)] #all B
params2gen = params2gen[!grepl(pattern = "7", params2gen)] #not 7


sapply(X=params2gen, treat_parameterization) #do all runs

