#WASA calibration using PSO / DDS
#to be called after calibration for water has finished
# use existing directory "thread1_best" (created by view_progress.R)

# run on cluster or
# when called locally before copying to cluster: delete dds.* and thread.* in run directories


setwd("E:/till/uni/parameterisierung/Esera_2014/runs3")

param_ids=c(
#  "A_u_24",
#    "A+1_u_24",
#  "A+2_u_24",
#  "A+3_u_24",
#  "A+4_u_24",
#  "A+5_u_24",
# "A+6_u_24",
#"A+7_u_24"

#"A_u_1",
#"A+1_u_1",
#   "A+2_u_1",
#    "A+3_u_1"
#    "A+4_u_1",
#    "A+5_u_1",
#  "A+7_u_1"
 # "B_u_24",
 #   "B-1_u_24",
 #   "B-2_u_24",
 #   "B-3_u_24",
 #   "B-4_u_24",
 #   "B-5_u_24",
 #   "B-7_u_24"
  # "B_u_1",
  # "B-1_u_1",  
  # "B-2_u_1",
  "B-3_u_1"
#   "B-4_u_1",
#   "B-5_u_1",
#   "B-7_u_1"
)



for (param_id in param_ids)
{
  setwd(param_id)
  #param_id="A_u_24"

  if (!file.exists("thread1_best"))
    stop(paste0(param_id, ": thread1_best not found. Run view_progress.R to create it."))  
  
  #save files of water calibration
  if (!file.exists("water_calib_files"))
  {
    dir.create("water_calib_files")
    movefiles=dir(pattern = "\\..*")
    file.rename(from = movefiles, paste0("water_calib_files/",movefiles))  
  } else
  unlink(x = dir(pattern="\\.", include.dirs = FALSE), recursive = FALSE) #empty files in directory
  
  #restore files that are still needed for sediment calibration
  copyfiles=dir(path = "water_calib_files", pattern = "\\.*.R|bat|pbs|exe|lin|psox")
  file.copy(from = paste0("water_calib_files/",copyfiles), "./", copy.date=TRUE)  
  
  #delete old thread dirs
    old_threaddirs=dir(pattern="thread[0-9]*$")
    unlink(old_threaddirs, recursive = TRUE)
  
  #delete old lock files
    unlink(dir(pattern="thread[0-9]*\\.lock$"), recursive = TRUE)
    
  
  #create new initialisation directory
  unlink("init_config_sed", recursive=TRUE)
  dir.create("init_config_sed")
  file.copy(from="thread1_best/.", to="init_config_sed", overwrite=TRUE, recursive=TRUE, copy.date = TRUE)

  
  #update initial conditions
  outdir="init_config_sed/output/isabena_2010-2013/"
  movefiles=dir(pattern = ".*_start$", path = outdir) #find storage files
  file.remove(dir(path="init_config_sed/input/isabena_2010-2013/init_conds/", full.names = TRUE))
  file.rename(from=paste0(outdir,movefiles), to=paste0("init_config_sed/input/isabena_2010-2013/init_conds/", sub(movefiles, pattern = "_start$", repl="")))
  
    
  #remove time series output of template run and pre-start model state files
  old_outfiles=dir(path = "init_config_sed", pattern = "\\.out|stat.*_start", recursive = TRUE)
  file.remove(paste0("init_config_sed/",old_outfiles)) 
  
  #remove logfiles of previous runs
  old_logfiles=dir(recursive=FALSE, path="init_config_sed/", include.dirs=FALSE, pattern="\\.+")
  file.remove(paste0("init_config_sed/",old_logfiles)) 
  
  #copy general files for sediment calibration
    file.copy(from=paste0("../templates_sed/all/."), to=".", overwrite=TRUE, recursive=TRUE, copy.date = TRUE)
    file.rename("init_config_sed/input/isabena_2010-2013/x_erosion.ctl","init_config_sed/input/isabena_2010-2013/erosion.ctl")
  
  #enable sediment modelling
    source("modify_wasa_input.R")
    modify_wasa_input(wasa_input_dir = "init_config_sed/input/isabena_2010-2013/", parameters = data.frame(dosediment=".t."))
  
  #set files modified in water calibration as new standard, i.e. delete old templates
    modified_inputfiles=dir(path = "init_config_sed", pattern="calib_bak", recursive = TRUE) #get names of input files modified in water calibration
    modified_inputfiles=paste0("init_config_sed/",modified_inputfiles)
    file.remove(modified_inputfiles)

    
  #copy configuration-specific files
    sed_templates = dir(path="../templates_sed/")
  
    cur_config = sub(pattern = ".*/", x = getwd(), rep="")
    to_copy=sapply(X = sed_templates, FUN = grepl, x=cur_config, fixed = TRUE)
    
    for (tdir in sed_templates[to_copy])
      file.copy(from=paste0("../templates_sed/",tdir,"/."), to=".", overwrite=TRUE, recursive=TRUE, copy.date = TRUE)  
  
    if (grepl(pattern = "B", cur_config) && !grepl(pattern = "B-2", cur_config)) #use subbasin data
      file.copy(from=paste0("../templates_sed/A+2/."), to=".", overwrite=TRUE, recursive=TRUE, copy.date = TRUE)  
      
  #modify jobscript
    tt=scan("job_parallel.pbs", what = character(), sep = "\n")
    tt=sub(x = tt, pattern="calibrate_main_dds_mpi", replacement = "calibrate_main_dds_mpi_sed")
    tt=sub(x = tt, pattern="(-N .*)", replacement = "\\1_sed")
    tt=sub(x = tt, pattern="#PBS -m abe", replacement = "#PBS -m ae")
    write(tt, file="job_parallel.pbs")
  
    setwd("../")    
  }
  
print("Don't forget to convert linebreaks of job_parallel.pbs manually!")
