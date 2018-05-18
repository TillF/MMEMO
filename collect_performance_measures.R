#collect/compute the metrics of all available parameterisations
#for later display enhancement with coloured matrix plots (plot_performance_measures.R)

#base_dir="./runs2/"
base_dir="./runs3/"

#normalization of absolute improvement
#norm_ip = "by_reference_performance" #with performance of base parameterisation (first submission)
norm_ip = "by_max_improvement"      #with best improvement of all configurations using the same reference (first revised version of manuscript)
#norm_ip = "none"      #no normalization, test setting only
#warning("Normalisierung aktivieren!")

force_daily=TRUE #for hourly runs: use evaluation of performance measures in daily resolution (curr_obj_fun_val_day.txt)
                  # this is important for ME "resolution", which must compare performance metrics computed on the same temporal scale



library(xlsx)

#read general data ####
  configs_dirs      = read.xlsx(file = paste0(base_dir, "comparison.xlsx"), sheetName = "configuration", startRow = 3, endRow = 3, header = FALSE, stringsAsFactors=FALSE)
  configs           = read.xlsx(file = paste0(base_dir, "comparison.xlsx"), sheetName = "configuration", startRow = 2, endRow = 19,header = TRUE , stringsAsFactors=FALSE, colIndex = 1:10)
  enhancement_names = read.xlsx(file = paste0(base_dir, "comparison.xlsx"), sheetName = "configuration", startRow = 2, endRow = 2, header = FALSE,  stringsAsFactors=FALSE, colIndex = 2:10)
  
  parameterizations_header = read.xlsx(file = paste0(base_dir, "comparison.xlsx"), sheetName = "parameterization", startRow = 1, endRow = 3, header = FALSE, stringsAsFactors=FALSE)
  parameterizations        = read.xlsx(file = paste0(base_dir, "comparison.xlsx"), sheetName = "parameterization", startRow = 4,             header = TRUE,  stringsAsFactors=FALSE)
  

# read data from run-directories ####    
  parameterizations_both=list() #dirty solution: store both variants of IP-values (from daily and hourly resolution))
  parameterizations_both[[1]]=parameterizations
  parameterizations_both[[2]]=parameterizations
  
  for (tsc in 1:2) #1: metrics from original resolution (day and hour); 2: all metrics from daily resolution
  {  
    parameterizations = parameterizations_both[[tsc]]
    collected_metrics=data.frame()
    
  for (param_id in parameterizations$parameterization_ID)
  {
    #param_id="A+3_c_24"
    dirname=paste0(base_dir, sub(x = param_id, pattern = "_c_", replacement = "_u_"))
    if (!file.exists(dirname)) next
    
    if (grepl(x = param_id, pattern = "_c_")) #is this a calibrated parameterisation?
      dirname=paste0(dirname, "/thread1_best") else
      dirname=paste0(dirname, "/thread1_nocal") 
    
    for (sed in c( "", "_sed" )) #treat water and sediment runs
    {  
      if (force_daily && tsc==2) #for hourly runs: use evaluation of performance measures in daily resolution (curr_obj_fun_val_day.txt)
      metrics_file=paste0(dirname, sed, "/curr_obj_fun_val_day.txt") else
      metrics_file=paste0(dirname, sed, "/curr_obj_fun_val.txt")
      if (!file.exists(metrics_file)) 
          stop(paste0(metrics_file," not found."))
     
      metrics = read.table(file=metrics_file, sep = "\t", header=FALSE, skip = 4, na.strings = c("-99999", "NaN", "NA"))
      m2=metrics[,2]
      if (is.factor(m2)) #very long numbers are interpreted as factors, convert them
        m2=as.numeric(as.character(m2))
      names(m2) = sub(x=metrics[,1], pattern = ":", repl="") #remove ":"
      
      water_metrics2 = !grepl(names(m2), pattern = "sed") & !is.na(names(m2)) #water-related metrics (as opposed to sediment)

      if (sed=="")
        m3 = m2[water_metrics2] #save only water  metrics
      else 
        m3 = c(m3, m2[!water_metrics2]) #append sediment metrics
      
      #is this a calibrated parameterisation? Then, we can check the consistency of water metrics between sed and no_sed
      if (grepl(x = param_id, pattern = "_c_") & sed=="_sed") 
      {
        water_metrics3 = !grepl(names(m3), pattern = "sed") & !is.na(names(m3))
        abs_dev=mean(abs((m2[water_metrics2]-m3[water_metrics3])/m3[water_metrics3]), na.rm=TRUE)
        if (is.finite(abs_dev) & (abs_dev > 0.2))  
        {  
          print(paste0(param_id,": large mean absolute relative difference in water metrics (",abs_dev,", nosed & sed)"))
          browser()
        }
      } 
      
      # #collect parameters
      # param_file=paste0(dirname, sed, "/paramset.txt") 
      # if (!file.exists(param_file)) 
      #   stop(paste0(param_file," not found."))
      # 
      # params = read.table(file=param_file, sep = "\t", header=FALSE, skip = 4, na.strings = c("-99999", "NaN", "NA"))
      
    }
      
    collected_metrics=rbind(collected_metrics, data.frame(parameterization_ID=param_id, t(m3)))
    
  }  
  
  if (nrow(collected_metrics)==0)
    stop("no metrics collected, check result directories?")
  
  
  present_columns=intersect(names(parameterizations[,-1]), names(collected_metrics)) #find columns already present in source table
  for (cn in present_columns) parameterizations[,cn]=NULL #delete old columns that will be replaced in the next step (without deleting, the values are not updated during merging)
  parameterizations = merge(parameterizations, collected_metrics, all.y = TRUE, sort=FALSE) #join tables

#   #prevent script from crashing when data is still incomplete
#   if (is.null(parameterizations$rmse_sed_sub6))       parameterizations$rmse_sed_sub6=NA
#   if (is.null(parameterizations$bias_total_sed_sub6)) parameterizations$bias_total_sed_sub6=NA

#compute metrics
  
  metric_cols = c(
    "sub_wat_dyn", 
    "sub_wat_yil", 
    "sub_sed_dyn", 
    "sub_sed_yil", 
    "out_wat_dyn", 
    "out_wat_yil", 
    "out_sed_dyn", 
    "out_sed_yil")
  parameterizations[, metric_cols] = NA
  
#compute metrics (for later comparison with base parametrisation)
  #water metrics
  parameterizations$sub_wat_dyn = apply(    parameterizations[, grepl(pattern="rmse_qtotal(_|$)", x=names(parameterizations))] , MARGIN = 1, FUN=mean)   #mean of all subbasin RMSE
  #parameterizations$sub_wat_yil = apply(abs(parameterizations[, grepl(pattern="bias_total_rel_sub" , x=names(parameterizations))]), MARGIN = 1, FUN=mean)  #mean of absolute volume error of subbasins (only used for generating table of performance measures of reference configs)
  parameterizations$sub_wat_yil = apply(abs(parameterizations[, grepl(pattern="bias_total_sub" , x=names(parameterizations))]), MARGIN = 1, FUN=mean)  #mean of absolute volume error of subbasins
  parameterizations$out_wat_dyn = parameterizations$rmse_qtotal_sub6
  #parameterizations$out_wat_yil = abs(parameterizations$bias_total_rel_sub6) #(only used for generating table of performance measures of reference configs)
  parameterizations$out_wat_yil = abs(parameterizations$bias_total_sub6)

  
  #sediment metrics
  parameterizations$sub_sed_dyn = apply(    parameterizations[, grepl(pattern="rmse_sed(_|$)", x=names(parameterizations))] , MARGIN = 1, FUN=mean)   #mean of all subbasin sediment RMSE
  #parameterizations$sub_sed_yil = apply(abs(parameterizations[, grepl(pattern="bias_total_sed_rel_sub" , x=names(parameterizations))]), MARGIN = 1, FUN=mean)  #mean of absolute mass error of subbasins (only used for generating table of performance measures of reference configs)
  parameterizations$sub_sed_yil = apply(abs(parameterizations[, grepl(pattern="bias_total_sed_sub" , x=names(parameterizations))]), MARGIN = 1, FUN=mean)  #mean of absolute mass error of subbasins
  parameterizations$out_sed_dyn = parameterizations$rmse_sed_sub6
  #parameterizations$out_sed_yil = abs(parameterizations$bias_total_sed_rel_sub6) #(only used for generating table of performance measures of reference configs)
  parameterizations$out_sed_yil = abs(parameterizations$bias_total_sed_sub6)
  
#print missing parameterizations
  missing = is.na(parameterizations$sub_wat_dyn)
  print(paste0("missing water parameterizations: ", paste(parameterizations$parameterization_ID[missing], collapse = ", ")))
  missing = is.na(parameterizations$sub_sed_dyn)
  print(paste0("missing sediment parameterizations: ", paste(parameterizations$parameterization_ID[missing], collapse = ", ")))
  cat(paste(parameterizations$parameterization_ID[missing], collapse = "\n "))
  
    
  m_subcatchment_outlet = matrix(c(rep("sub", 8), rep("out",8))  , ncol=4)
  #m_target_var      = matrix(rep(c(rep("wat", 2), rep("sed",2)), 4), ncol=4)
  #m_day_hour            = matrix(rep(c(rep(24, 2), rep(1,2)), 4), ncol=4)
  m_A_B            = matrix(rep(c(rep("A", 2), rep("B",2)), 4), ncol=4)
  
  m_dynamics_yield      = matrix(rep(c("dyn","yil"), 8)  , ncol=4, byrow = TRUE)
  m_uncal_calibrated    = matrix(rep(c("u","c"), 8)  , ncol=4)
  

# compute improvement values
  #improvement values of regular enhancements
    #create columns for storing improvement values
    I_P_col_names = paste0("I_P_", metric_cols)
    parameterizations[, I_P_col_names] = NA
     
    norm_cols = paste0("norm_",metric_cols)  #create fields for holding normalization divisors
    parameterizations[, norm_cols]=NA 

    for (i in 1:nrow(parameterizations))
    {  
      ref = which( parameterizations$parameterization_ID == parameterizations$reference[i])  #row of reference parameterisation

      parameterizations[i, I_P_col_names] = (parameterizations[ref, metric_cols] - parameterizations[  i, metric_cols]) #absolute improvement
      
      #for later normalization of absolute improvement
      if (norm_ip == "by_reference_performance")
        parameterizations[i, norm_cols] = parameterizations[ref, metric_cols]
    }

    #IP-values of B-configs must be inverted
      B_configs = grepl(parameterizations$parameterization_ID, pattern = "^B")
      parameterizations[B_configs, I_P_col_names] = - parameterizations[B_configs, I_P_col_names]
    
  #interpret "calibration" as model enhancement 
    param_ids_uncalibrated =    parameterizations$parameterization_ID[grepl(parameterizations$parameterization_ID, pattern="_u_")]
    param_ids_calibrated   = sub(param_ids_uncalibrated, pattern = "_u_", repl="_c_")  
    
    for (i in 1:length(param_ids_uncalibrated))
    {
      dst = which( parameterizations$parameterization_ID == param_ids_calibrated[i])   #row of current parameterisation
      ref = which( parameterizations$parameterization_ID == param_ids_uncalibrated[i]) #row of reference parameterisation

      new_row =  nrow(parameterizations) + 1 #where to add new row
      parameterizations[new_row,] = NA #add empty row at bottom
      parameterizations$parameterization_ID[new_row] = sub(parameterizations$parameterization_ID[dst],pattern = "^([AB])[^_]*", replacement = "\\1+8")
      parameterizations$reference[new_row] = parameterizations$parameterization_ID[ref]

      parameterizations[new_row, I_P_col_names] = (parameterizations[ref, metric_cols] - parameterizations[dst, metric_cols]) #absolute improvement
      
      #for later normalization of absolute improvement
      if (norm_ip == "by_reference_performance")
        parameterizations[new_row,norm_cols] = parameterizations[ref, metric_cols]
    }

  #interpret "temporal resolution" as model enhancement 
    if (tsc==2)   # should only be done when using metrics computed in the same resolution, i.e. daily
    { 
      param_ids_daily =      parameterizations$parameterization_ID[grepl(parameterizations$parameterization_ID, pattern="[^89]_._24$")]
      param_ids_hourly   =sub(param_ids_daily, pattern = "_24$", repl="_1")  
      
#      subs = grepl(param_ids_daily, pattern = "^._")
#      warning("subs entfernen!!")
      for (i in 1:length(param_ids_daily))
#      for (i in which(subs)[1:4])
        {
        dst = which( parameterizations$parameterization_ID == param_ids_hourly[i])   #row of current parameterisation
        ref = which( parameterizations$parameterization_ID == param_ids_daily[i]) #row of reference parameterisation
        
        new_row =  nrow(parameterizations) + 1 #where to add new row
        parameterizations[new_row,] = NA #add empty row at bottom
        parameterizations$parameterization_ID[new_row] = sub(parameterizations$parameterization_ID[dst],pattern = "^([AB])[^_]*", replacement = "\\1+9")
        parameterizations$reference[new_row] = parameterizations$parameterization_ID[ref]
        
        parameterizations[new_row, I_P_col_names] = (parameterizations[ref, metric_cols] - parameterizations[dst, metric_cols]) #absolute improvement
        
        #for later normalization of absolute improvement
        if (norm_ip == "by_reference_performance")
          parameterizations[new_row,norm_cols] = parameterizations[ref, metric_cols]
      }
    }
    
    if (norm_ip == "none")
      parameterizations[,norm_cols] = 1  #no normalization, ie sheer difference in IP-values
    
    ##find normalizers given "by_max_improvement"
      if (norm_ip == "by_max_improvement")
      {  
        parameterizations$reference = sub(parameterizations$reference, pattern = "^([AB])[^_]*", replacement = "\\1")
        #simplify the reference to allow for cohorte assignment, ie. A+4_u_1 belongs to A_u_1
        
        least_nonzero_abs = function(x) #return smallest absolute non-zero number
        {return(min(abs(x[x!=0])))}

        for (ref in unique(parameterizations$reference))
        {
          same_reference = which(parameterizations$reference == ref) #find all parameterizations using the same reference, because they can be compared to each other
          same_reference_wo_ME8ME9 = setdiff(same_reference,           which( grepl(parameterizations$parameterization_ID, pattern = "[\\+\\-][89]"))) #exclude the special cases ME8 and ME9 from the scope where the maximum is determined
          same_reference_wo_ME8ME9 = setdiff(same_reference_wo_ME8ME9, which(!grepl(parameterizations$parameterization_ID, pattern = "[\\+\\-]"))) #exclude the special cases ME8 and ME9 from the scope where the maximum is determined
          best_improvements  = apply(parameterizations[same_reference_wo_ME8ME9, I_P_col_names], 2, max) #get best absolute improvement attained
          best_improvements2 = apply(parameterizations[same_reference_wo_ME8ME9, I_P_col_names], 2, least_nonzero_abs) #get min  absolute improvement attained (prevent 0 and negative scaling, if the best improvement was 0)
          #stop()
          for (i in same_reference) #loop is necessary, otherwise, the matrix is assigned column-wise
            parameterizations[i, norm_cols] = (pmax(best_improvements, best_improvements2)) #set best improvement in cohorte as normalizer; even if there are only deteorations (no positive values), the scaling should preserve the original sign
        }
      }  

          
    ###normalize IPs
      if (any(is.na(parameterizations[, norm_cols]))) stop("normalization is NA, please check.")
      if (any(parameterizations[, norm_cols]<=0)) stop("normalization has negative values, strange.")
      parameterizations[, I_P_col_names] = parameterizations[, I_P_col_names] / parameterizations[, norm_cols]
          
    ###aggregate special IPs (calib and temporal resolution)
        #aggregate calibration-IPs over model enhancements
        #aux4aggr = sub(x = I_P_special$parameterization_ID, pattern = "[+\\-][0-9]", repl="")
        aux4aggr = parameterizations$parameterization_ID
        agg_fun = function(x)
        {
          if (is.character(x)) return (x[1]) else
            return(mean(x))
        }
        parameterizations2 = aggregate(x = parameterizations[,-1], by = list(parameterization_ID=aux4aggr), FUN = agg_fun)
        
        #reorder to scheme of original file
          reorder = match(parameterizations$parameterization_ID[1:64], parameterizations2$parameterization_ID)
          parameterizations3 = parameterizations2[reorder,]
          #identical(str(parameterizations[1:64,]), str(parameterizations3[1:64,]))
          # for (i in 1:ncol(parameterizations))
          #   print(identical(parameterizations[1:64,i], parameterizations3[1:64,i]))
          # 
          #update other fields of added rows  
            new_ids = c(
          "A+8_c_1",
          "A+8_c_24",
          "B+8_c_1",
          "B+8_c_24",
          "A+9_c_1",
          "A+9_u_1",
          "B+9_c_1",
          "B+9_u_1")
          new_rows=64+(1:8)
            parameterizations3[new_rows,] = parameterizations[match(new_ids, parameterizations$parameterization_ID),]
          
          parameterizations = parameterizations3
          rm(list = c("parameterizations3","parameterizations2"))
          parameterizations$config_ID[new_rows] = sub(parameterizations$parameterization_ID[new_rows], pattern = "^([^_]+)_.*", repl="\\1")
          parameterizations$reference[new_rows] = c(
            "A+?_u_1",
            "A+?_u_24",
            "B+?_u_1",
            "B+?_u_24",
            "A+?_c_24",
            "A+?_u_24",
            "B+?_c_24",
            "B+?_u_24")
          
        parameterizations$calibrated..yes.no.[new_rows]=sub(parameterizations$parameterization_ID[new_rows], pattern = "^[^_]+_([^_]+).*", repl="\\1")
        parameterizations$resolution         [new_rows]=sub(parameterizations$parameterization_ID[new_rows], pattern = "^[^_]+_[^_]+_([^_]+).*", repl="\\1")
        
        parameterizations_both[[tsc]] = parameterizations #store these computed metrics
    }        


#use metrics computed on original temporal resolution, except for ME "resolution" (here, only metrics computed on the daily scale should be compared)    
  parameterizations = parameterizations_both[[1]] #daily/hourly metrics
  #parameterizations = parameterizations_both[[2]] #daily metrics only
  resolution_rows = grep(parameterizations_both[[2]]$parameterization_ID, pattern = "\\+9") #get rows containing ME "resolparameterizations[resolution_rows,] = parameterizations_both[[2]][resolution_rows,] #daily metrics only
  parameterizations[resolution_rows,] = parameterizations_both[[2]][resolution_rows,] #daily metrics only
  
  
  
  
  # for (i in 1:ncol(parameterizations_both[[2]]))
#    if (!identical(parameterizations_both[[2]][,i], parameterizations_both[[1]][,i]))
#      print(names(parameterizations_both[[2]])[i])
        
#add new data to xls-file ####
  #create backup of original file
  src_name=paste0(base_dir, "comparison.xlsx")
  info =  file.info(src_name)
  bk_name = paste0(base_dir, sub("comparison.xlsx", pattern = "\\.", repl=paste0("_",format(info$mtime,"%Y%m%d-%H%M."))))
  
  file.rename(from = src_name, to = bk_name)
  #file.copy(from = paste0(base_dir, "comparison.xlsx"), to = paste0(base_dir, sub("comparison.xlsx", pattern = "\\.", repl=paste0("_",format(Sys.time(),"%Y%m%d-%H%M.")))))
  
  wb <- loadWorkbook(file= bk_name)  
  sheets <- getSheets(wb)
  srows = getRows(sheets$parameterization)
  removeRow(sheets$parameterization, rows=srows[-(1:4)])
  #addDataFrame(parameterizations, sheets$parameterization, startRow=4, startColumn=17, row.names = FALSE, col.names = FALSE)
  addDataFrame(parameterizations, sheets$parameterization, startRow=4, startColumn=1, row.names = FALSE, col.names = TRUE)
  saveWorkbook(wb, file= src_name)
  
  #for debugging only
  write.table(file=paste0(base_dir, "comparison.txt"), parameterizations_header, sep="\t", quote=F, row.names=F, col.names = F, na = "")
  write.table(file=paste0(base_dir, "comparison.txt"), format(parameterizations, digits=10, scientific=FALSE)       , sep="\t", quote=F, row.names=F, append=TRUE)
  
  
  
  summary(parameterizations[, I_P_col_names])
  
  hist(unlist(matrix(parameterizations[-(67:72), I_P_col_names], ncol = 1)))
  which.min(parameterizations[, I_P_col_names[8]])
  
  
  

