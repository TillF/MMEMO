#collect/compute the metrics of all available parameterisations
#display enhancement with coloured matrix plots 

base_dir="./runs2/"

library(xlsx)

#read data
  configs_dirs      = read.xlsx(file = paste0(base_dir, "comparison.xlsx"), sheetName = "configuration", startRow = 3, endRow = 3, header = FALSE, stringsAsFactors=FALSE)
  configs           = read.xlsx(file = paste0(base_dir, "comparison.xlsx"), sheetName = "configuration", startRow = 2, endRow = 19,header = TRUE , stringsAsFactors=FALSE, colIndex = 1:10)
  enhancement_names = read.xlsx(file = paste0(base_dir, "comparison.xlsx"), sheetName = "configuration", startRow = 2, endRow = 2, header = FALSE,  stringsAsFactors=FALSE, colIndex = 2:10)
  
  parameterizations_header = read.xlsx(file = paste0(base_dir, "comparison.xlsx"), sheetName = "parameterization", startRow = 1, endRow = 3, header = FALSE, stringsAsFactors=FALSE)
  parameterizations        = read.xlsx(file = paste0(base_dir, "comparison.xlsx"), sheetName = "parameterization", startRow = 4,             header = TRUE,  stringsAsFactors=FALSE)
  
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
      metrics_file=paste0(dirname, sed, "/curr_obj_fun_val.txt")
      if (!file.exists(metrics_file)) 
          next
     
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
        
    }
      
    collected_metrics=rbind(collected_metrics, data.frame(parameterization_ID=param_id, t(m3)))
    
  }  
  
  
  
  
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
    "out_wat_dyn", 
    "out_wat_yil", 
    "sub_sed_dyn", 
    "sub_sed_yil", 
    "out_sed_dyn", 
    "out_sed_yil")
  parameterizations[, metric_cols] = NA
  
#compute metrics (for later comparison with base parametrisation)
  #water metrics
  parameterizations$sub_wat_dyn = apply(    parameterizations[, grepl(pattern="rmse_qtotal(_|$)", x=names(parameterizations))] , MARGIN = 1, FUN=mean)   #mean of all subbasin RMSE
  parameterizations$sub_wat_yil = apply(abs(parameterizations[, grepl(pattern="bias_total_sub" , x=names(parameterizations))]), MARGIN = 1, FUN=mean)  #mean of absolute volume error of subbasins
  parameterizations$out_wat_dyn = parameterizations$rmse_qtotal_sub6
  parameterizations$out_wat_yil = abs(parameterizations$bias_total_sub6)
  
  #sediment metrics
  parameterizations$sub_sed_dyn = apply(    parameterizations[, grepl(pattern="rmse_sed(_|$)", x=names(parameterizations))] , MARGIN = 1, FUN=mean)   #mean of all subbasin sediment RMSE
  parameterizations$sub_sed_yil = apply(abs(parameterizations[, grepl(pattern="bias_total_sed_sub" , x=names(parameterizations))]), MARGIN = 1, FUN=mean)  #mean of absolute volume error of subbasins
  parameterizations$out_sed_dyn = parameterizations$rmse_sed_sub6
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
      
    for (i in 1:nrow(parameterizations))
    {  
      ref = which( parameterizations$parameterization_ID == parameterizations$reference[i])  #the parameterization to compare to
      #nor = which( parameterizations$parameterization_ID == sub("B-4_u_24", pattern = ".*(_\\d+$)", repl="A_u\\1")) #the parameterization to normalize with
      nor=ref
      
      parameterizations[i, I_P_col_names] =
       (parameterizations[ref, metric_cols] -
        parameterizations[  i, metric_cols]) /  
        parameterizations[nor, metric_cols]
    }
    
    #IP-values of B-configs must be inverted
      B_configs = grepl(parameterizations$parameterization_ID, pattern = "^B")
      parameterizations[B_configs, I_P_col_names] = - parameterizations[B_configs, I_P_col_names]
    
  #interpret "calibration" as model enhancement 
    param_ids_uncalibrated =    parameterizations$parameterization_ID[grepl(parameterizations$parameterization_ID, pattern="_u_")]
    param_ids_calibrated   =sub(param_ids_uncalibrated, pattern = "_u_", repl="_c_")  
    
    I_P_calibration = data.frame()
    for (i in 1:length(param_ids_uncalibrated))
    {
      dst = which( parameterizations$parameterization_ID == param_ids_calibrated[i])
      ref = which( parameterizations$parameterization_ID == param_ids_uncalibrated[i])
      #nor = which( parameterizations$parameterization_ID == sub("B-4_u_24", pattern = ".*(_\\d+$)", repl="A_u\\1")) #the parameterization to normalize with
      nor=ref
            
      tt =
        (parameterizations[ref, metric_cols] -
           parameterizations[dst, metric_cols]) /  
        parameterizations[nor, metric_cols]
      
      I_P_calibration =  rbind(I_P_calibration, data.frame(parameterization_ID=param_ids_calibrated[i], tt)) #collect results
    }
    names(I_P_calibration)[-1] = paste0("I_P_",names(I_P_calibration)[-1]) 
    #aggregate over model enhancements
      aux4aggr = sub(x = I_P_calibration$parameterization_ID, pattern = "[+\\-][0-9]", repl="")
      I_P_calibration_mean = aggregate(x = I_P_calibration[,-1], by = list(parameterization_ID=aux4aggr), FUN = mean)

    #add to the list of the other parameterizations
      I_P_calibration_mean$parameterization_ID = sub(I_P_calibration_mean$parameterization_ID, pattern = "_c", repl="+8_c")
      new_rows =  nrow(parameterizations) + (1: nrow(I_P_calibration_mean))
      parameterizations[new_rows,] = NA #add empty rows at bottom
      parameterizations[new_rows, names(I_P_calibration_mean)] = I_P_calibration_mean
      

  #interpret "temporal resolution" as model enhancement
      param_ids_daily =      parameterizations$parameterization_ID[grepl(parameterizations$parameterization_ID, pattern="[^89]_._24$")]
      param_ids_hourly   =sub(param_ids_daily, pattern = "_24$", repl="_1")  
      
      I_P_resolution = data.frame()
      for (i in 1:length(param_ids_daily))
      {
        dst = which( parameterizations$parameterization_ID == param_ids_hourly[i])
        ref = which( parameterizations$parameterization_ID == param_ids_daily[i])
        #nor = which( parameterizations$parameterization_ID == sub("B-4_u_24", pattern = ".*(_\\d+$)", repl="A_u\\1")) #the parameterization to normalize with
        nor=ref
        
        tt =
          (parameterizations[ref, metric_cols] -
             parameterizations[dst, metric_cols]) /  
          parameterizations[nor, metric_cols]
        
        I_P_resolution =  rbind(I_P_resolution, data.frame(parameterization_ID=param_ids_hourly[i], tt)) #collect results
      }
      names(I_P_resolution)[-1] = paste0("I_P_",names(I_P_resolution)[-1]) 
      #aggregate over model enhancements
        aux4aggr = sub(x = I_P_resolution$parameterization_ID, pattern = "[+\\-][0-9]", repl="")
        I_P_resolution_mean = aggregate(x = I_P_resolution[,-1], by = list(parameterization_ID=aux4aggr), FUN = mean)

#         dst = match(param_ids_hourly, parameterizations$parameterization_ID)
#         ref = match(param_ids_daily, parameterizations$parameterization_ID)
#         tt =
#           (
#           apply(parameterizations[ref, metric_cols], MARGIN = 2, mean) -
#           apply(parameterizations[dst, metric_cols], MARGIN = 2, mean) )/  
#           apply(parameterizations[ref, metric_cols], MARGIN = 2, mean)
        
        
      #add to the list of the other parameterizations
        I_P_resolution_mean$parameterization_ID = sub(I_P_resolution_mean$parameterization_ID, pattern = "^(.)_(.)", repl="\\1+9_\\2")
        new_rows =  nrow(parameterizations) + (1: nrow(I_P_resolution_mean))
        parameterizations[new_rows,] = NA #add empty rows at bottom
        parameterizations[new_rows, names(I_P_resolution_mean)] = I_P_resolution_mean
        

#add new data to xls-file
  #create backup of original file
  file.copy(from = paste0(base_dir, "comparison.xlsx"), to = paste0(base_dir, sub("comparison.xlsx", pattern = "\\.", repl=paste0("_",format(Sys.time(),"%Y%m%d-%H%M.")))))
  
  wb <- loadWorkbook(file= paste0(base_dir, "comparison.xlsx"))  
  sheets <- getSheets(wb)
  srows = getRows(sheets$parameterization)
  removeRow(sheets$parameterization, rows=srows[-(1:4)])
  #addDataFrame(parameterizations, sheets$parameterization, startRow=4, startColumn=17, row.names = FALSE, col.names = FALSE)
  addDataFrame(parameterizations, sheets$parameterization, startRow=4, startColumn=1, row.names = FALSE, col.names = TRUE)
  saveWorkbook(wb, file= paste0(base_dir, "comparison.xlsx"))
  
  
  
  summary(parameterizations[, I_P_col_names])
  
  hist(unlist(matrix(parameterizations[-(67:72), I_P_col_names], ncol = 1)))
  which.min(parameterizations[, I_P_col_names[8]])
  
  which.min(I_P_resolution[, I_P_col_names[8]])
  
  
