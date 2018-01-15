# compute objective function from given measures of goodness in res
# modifies obj_fun_desc and return_val
# save goodness measures and objective function in "curr_obj_fun_val.txt"


  #return_val = measures$rmse_qtotal+2*measures$rmse_peak
  #return_val = - measures$cor_total + max(0,(abs(measures$bias_total_rel)-0.3)) ) #bias-penalized correlation
  #return_val = - measures$ns_coeff #simple NS-coefficient
  
  #obj_fun_desc="mean subbasin nash";
  #return_val = -mean(as.matrix(measures[1, grepl(pattern="ns_coeff(_|$)", x=names(measures))]))  #mean of all ns_coeffs
  
  obj_fun_desc="mean subbasin rmse";
  return_val = mean(as.matrix(measures[1, grepl(pattern="rmse_qtotal(_|$)", x=names(measures))]))  #mean of all rmse-values
  
  
  #save performance measures in log file (all_runs.log)
  vers=778
  content=paste("dummy file for the evaluation of the goodness-of-fit of a WASA-run\n",
                "current objective function:",obj_fun_desc,"\n",
                "computed by: ","compute_goodness_measures.R, ver ",vers,
                sep = "")
  content=paste(content,"\nobjective_f_val:\t",return_val)
  
  for (i in 1:ncol(measures))
  {
    content=paste(content,"\n",names(measures)[i],":\t",paste(measures[,i],collapse="\t"),sep = "")
  }
  
  if (exists("force_daily") && force_daily)
    obj_file=paste(working_dir,"curr_obj_fun_val_day.txt",sep="") else 
    obj_file=paste(working_dir,"curr_obj_fun_val.txt",sep="") 
  a=file.rename(obj_file,paste(obj_file,"_prev",sep=""))   #copy objective-function file
  write(content, file = obj_file)
  
  
