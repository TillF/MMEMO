#functions to read output of wasa model and related files



read_observations=function(subbas_id, datevec, target_component="River_Flow", wasa_input_dir)
#target_component: c("River_Flow","rain", "River_Sediment_total")
  {
  obs_data=data.frame(datenum=datevec)
  units="date" #collect units for each column
  sub_names="" #collect subbasin names, if present
  
  gauges_info_file = paste(wasa_input_dir,"/","gauges_catchment_area.txt",sep="")  #for loading gauge catchment sizes
  if (file.exists(gauges_info_file))
  {  
    gauges_info = read.table(file= gauges_info_file,header=TRUE, sep="\t", stringsAsFactors = FALSE)  # read catchment sizes and subbasin IDs
    if (any(!is.finite(subbas_id)))
      subbas_id = gauges_info$SUBBAS_ID[which(gauges_info$GAUGE==subbas_id)]  #get ID of current subbasin
    
    #column_name = gsub(" ",".", gauges_info$GAUGE[match(subbas_id, gauges_info$SUBBAS_ID)])  #get names of current subbasins
    #subbasin_name = column_name
    subbasin_names = gauges_info$GAUGE[match(subbas_id, gauges_info$SUBBAS_ID)]
    subbasin_names = gsub("Villacarli\\.Bridge","Villacarli",subbasin_names)  #special case in naming convention
    if (length(subbasin_names) < length(subbas_id)) stop(paste0("Couldn't find all subbasin_IDs (",paste0(subbas_id, collapse=" ,"),") in ", gauges_info_file))
  } else  
  subbasin_names = subbas_id #if no name file found, just use the plain numbers
    
  if ("River_Flow" %in% target_component) # load observed sediment concentrations
  {

    if (as.numeric(difftime(datevec[2],datevec[1],units="hours"))==24) obsfile="discharge_obs_24.txt" else  #daily data requested
      obsfile="discharge_obs_1.txt"
    
    #col_names = read.table(paste(wasa_input_dir,"/Time_series/",obsfile,sep=""), header = FALSE, sep = "\t", dec = ".", na.strings = c("-9999.00","-9999","NA","NaN"), skip=4, nrows = 1, stringsAsFactors = FALSE)
    datain    = read.table(paste(wasa_input_dir,"/Time_series/",obsfile,sep=""), header = TRUE,  sep = "\t", dec = ".", na.strings = c("-9999.00","-9999","NA","NaN"), skip=4)
    datain$datenum=as.POSIXct(ISOdate(datain$YYYY, datain$MM, datain$DD, datain$HH, min = 0, sec = 0, tz = "GMT"), tz = "GMT")
    if (any(is.null(datain$datenum))) stop(paste0("Date conversion problem in ",obsfile))
    datain$YYYY=NULL
    datain$MM=NULL
    datain$DD=NULL 
    datain$HH=NULL #delete obsolete columns 
    #col_names = col_names[1,]
    #col_names = col_names[!col_names %in% c("YYYY","MM","DD","HH")]
    
    #obs_data=merge(obs_data,datain[,names(datain) %in% c(column_name,"datenum")],by="datenum",all.x=TRUE)
    subbasin_names = intersect(subbasin_names, names(datain))
    if(length(subbasin_names)==0) stop("no observation data for requested subbasins found.")
    obs_data  = merge(obs_data,datain[, c("datenum", subbasin_names)], by="datenum", all.x=TRUE)
    names(obs_data)[-1] = paste0("sub",subbas_id)
    sub_names = c(sub_names, subbasin_names)
    units = c(units, rep("m3/s", length(subbas_id))) #collect units for each column
  }   
  
  if ("rain" %in% target_component) # load observed rainfall
  {
    if (as.numeric(difftime(datevec[2],datevec[1],units="hours"))==24) obsfile="rain_daily.dat" else  #daily data requested
      obsfile="rain_hourly.dat"
    
    datain = read.table(paste(wasa_input_dir,"/Time_series/",obsfile,sep=""), header = TRUE, sep = "\t", dec = ".", na.strings = c("-9999.00","-9999","NA","NaN"), skip=2)
    if (obsfile=="rain_hourly.dat") hour=datain[,2] else
      hour=0
    datain$datenum=as.POSIXct(ISOdate(datain[,1] %% 1e4, datain[,1] %/% 1e4 %% 1e2, datain[,1] %/% 1e6, hour=hour, min = 0, sec = 0, tz = "GMT"), tz = "GMT")
    if (any(is.null(datain$datenum))) stop(paste0("Date conversion problem in ",obsfile))
    datain[,1]=NULL #delete obsolete columns 
    w <- options("warn")
    options(warn = -1) #disable warnings
    target_cols = as.numeric(sub(pattern="X", replacement="", x=names(datain))) %in% subbas_id
    options(w) # reset
    target_cols[ncol(datain)]=TRUE #datenum (last column) is needed anyway
    
    names(datain) = sub(pattern="^X", replacement="rain_", x=names(datain))
    obs_data=merge(obs_data, datain[, target_cols],by="datenum",all.x=TRUE)
    sub_names = c(sub_names, subbasin_names)
    units = c(units, rep("mm", sum(target_cols))) #collect units for each column
    
  }   
  
  if ("River_Sediment_total" %in% target_component) # load observed sediment concentrations
  {
      for (subbasin_name in subbasin_names)
      {  
        if (file.exists(paste(wasa_input_dir,"/Time_series/","ssc_intpl_quantForest_",subbasin_name,".txt",sep="")))
        {
          datain2 = read.table(paste(wasa_input_dir,"/Time_series/","ssc_intpl_quantForest_",subbasin_name,".txt",sep=""), header = TRUE, sep = "\t", dec = ".", na.strings = "-9999.00", skip=0)
          datain2$datenum=as.POSIXct(strptime(datain2$date,format="%d.%m.%y %H:%M", tz = "GMT"), tz = "GMT")
          time_interval_minutes=as.numeric(difftime(datain2$datenum[2],datain2$datenum[1],units="mins"))      #temporal resolution of read data
          datain2$sed_load=datain2$discharge.m3.s*1000*time_interval_minutes*60*datain2$SSC.g.l.*1e-6             #sediment load in t   #SSC_loCI.g.l.,SSC_hiCI.g.l.
          representative_time=datain2$datenum-time_interval_minutes*60/2 
          
          datain2$discharge.m3.s.=NULL
          datain2$SSC.g.l.=NULL
          datain2$SSC_loCI.g.l.=NULL
          datain2$SSC_hiCI.g.l.=NULL
          datain2$date=NULL
          datain2$datenum=NULL
          
          ##temporal resampling
          timestep_representation=0.0   #how are records assigned to output resolution. Example for 1 h resolution  
                                  #0: all data between 11:00 and 12:00 is assigned to record with timestamp 12:00
                                  #0.5: all data between 11:30 and 12:30 is assigned to record with timestamp 12:00
                                  #1: all data between 12:00 and 13:00 is assigned to record with timestamp 12:00                                
                                        intermittent=FALSE       #regular sampling interval
    
          time_interval_minutes=as.numeric(difftime(datevec[2],datevec[1],units="mins"))         #target temporal resolution
          aggr_fun=sum   
          resampled_data=rep(NA,length(datevec)) 
    
          
          for (k in 1:length(datevec))
          {
            time_from=datevec[k]-(1-timestep_representation)*time_interval_minutes*60    #timespan of each destination record is assumed according to time_representation
            time_till=time_from+time_interval_minutes*60
            
            find_records=((representative_time>time_from) & (representative_time<=time_till))         #find all records that fall in the current timestep
            if (any(find_records))
            {
              resampled_data[k]=aggr_fun(datain2$sed_load[find_records])
            } 
          }
    
          obs_data$ssc_load=resampled_data
          units = c(units, rep("t", length(subbas_id))) #collect units for each column
        }
      }
      
      monthly_yield_file=paste(wasa_input_dir,"/Time_series/","collected_yields.RData",sep="")
      if (file.exists(monthly_yield_file))
      {
        
        load(monthly_yield_file) #load "collected_yields" : monthly yields of all subcatchments
        collected_yields = collected_yields[,c("mean_yield.t.", "begin", "gauge")] #discard unnecessary columns
        for (si in 1:length(subbas_id))
        {
          subbas_yields = merge(obs_data[,1,drop=FALSE], collected_yields[collected_yields$gauge==subbasin_names[si],], by.x="datenum", by.y="begin", all.x=TRUE)
          if (nrow(subbas_yields)>0)
          {
            obs_data=cbind(obs_data, subbas_yields[,2]) #add to collection array  
            names(obs_data)[ncol(obs_data)] =paste0("monthly_yield_sub",subbas_id[si]) #label column
            units = c(units, "t") #collect units for each column
          }  
        }  
        #browser()

      }
       
      sub_names = c(sub_names, subbasin_names)
  }          
  attr(obs_data, "units") = units
  attr(obs_data, "subbasin_names") = sub_names
  return(obs_data)
  
}



read_wasa_results=function(ctrl_params, components_list, subbas_id)
#read output of wasa model
#ctrl_params: WASA runtime parameters as read with parse_wasa_ctrl_params()
#components: WASA-result files (without extension) to read
#subbas_id: ID of subbasin of interest
{
  if (file.exists("wasa_file_units.txt"))
    f_units = read.table("wasa_file_units.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE) else
    f_units=NULL

  check_file = function(res_file) #check existence of file and issue error message, if necessary
  {
    if (!file.exists(res_file)) stop(paste(res_file," not found. Add it to outfiles.dat and re-run WASA."))
  }  
  
  check_subbas = function(a, res_file, subbas_id) #check existence of subbasin in data and issue error message, if necessary
  {
    missing=!(paste0("X",subbas_id) %in% sub(names(a), pattern="\\.\\d+$", repl=""))
    if (any(missing)) stop(paste("Subbasin(s)",paste(subbas_id[missing], collapse=", "), "not contained in", res_file))
  }  
  
  read_wasa_file = function(component, subbas_id, read_date_col=FALSE) 
    #read standard wasa output file
  {
    res_file=paste(ctrl_params$output_dir,component,".out",sep="")
    check_file(res_file) #check existence of file and issue error message, if necessary
    a = read.table(file=res_file,header = T,skip = 1,na.strings = c("NaN","Inf","-Inf","*"))
    
    check_subbas(a, res_file, subbas_id)
    
    if(read_date_col)
    {
      hour= a[,which(names(a) %in% c("dt","DT","timestep","Timestep"))]-1
      if (NCOL(hour)==0) hour=0         #if no hour column is present, set to 0
      
      datevec=as.POSIXct(ISOdate(a[,1], 1, 1, hour = hour, min = 0, sec = 0, tz = "GMT"))       #assemble date vector
      if (ctrl_params$start_month!=1)      #treat simulation periods that start mid-year
      {
        time_offset=difftime(ISOdate(ctrl_params$start_year, ctrl_params$start_month, 1),ISOdate(ctrl_params$start_year, 1, 1),units="secs") #offset that occurs when starting simulation in midyear
        datevec[a[,1]==ctrl_params$start_year]=   datevec[a[,1]==ctrl_params$start_year]+as.numeric(time_offset)
      }
      datevec=datevec+(a[,2]-1)*3600*24           #add "day of model year" as contained in WASA-file
    } else datevec=NULL  
    
    if (component=="daily_sediment_production") #aggregate particle size classes in sediment output
    {
      tt=aggregate(t(a), by=list(subs=sub(names(a), pattern="\\.\\d+$", repl="")), FUN=sum)
      a= data.frame(t(tt[,-1])) 
      names(a)=tt[,1]
      rm(tt)
    }
    
    #a = as.matrix(a[, names(a) %in% paste("X", subbas_id, sep=""), drop=FALSE]) #keep only selected subbasins
    a = as.matrix(a[, paste0("X", subbas_id), drop=FALSE]) #keep only selected subbasins
    
    if (NCOL(a)>1)
      dimnames(a)[[2]] = paste0(component,"_", dimnames(a)[[2]]) else
      dimnames(a)[2] = component
    
    return(list(datevec=datevec, component=a))
  }  
  
  #read subbasin area from hymo.dat
    hymo_file=paste(ctrl_params$input_dir,"Hillslope/hymo.dat",sep="")
    a = read.table(file=hymo_file,header = F,skip = 2, na.strings = c("NaN","Inf","-Inf"), fill = TRUE)
    subbas_area = a[match(subbas_id, a[,1] ) ,2]
    rm(a)
  
  result_array=NULL     #array holding all components to be read

  datevec = NULL
  units = NULL
  #browser()
  for (component in components_list)
  {
    tt = read_wasa_file (component, subbas_id, read_date_col=is.null(datevec)) 
    result_array = cbind(result_array,tt[["component"]])
    if (is.null(datevec))  datevec = tt[["datevec"]]
    if (is.null(f_units))
      unit="unknown" else
      unit = f_units$unit[toupper(f_units$file) == toupper(component)]
    if (length(unit)==0) unit="unknown" 
    units = c(units, rep(unit, length(subbas_id) ) ) #collect units for each column
    
  }
  
#     if ("River_Sediment_total" %in% components_list)  
#     {
#         res_file=paste(ctrl_params$output_dir,"River_Sediment_total.out",sep="")
#         check_file(res_file)
#         a=read.table(file=res_file,header = T,skip = 1,na.strings = c("NaN","Inf","-Inf"))
#         check_subbas(a, res_file, subbas_id)
#         result_array=cbind(result_array,River_Sediment_total=as.matrix(a[,names(a)==paste("X",subbas_id,sep="")]))
#     }
    
  attr(result_array, "units") = units  
  attr(result_array, "subbas_area") = subbas_area  
  attr(result_array, "dt") = as.numeric(difftime(datevec[2], datevec[1], unit="h"))  

  return(list(datevec=datevec,result_array=result_array))
    
}

#retrieve wasa runtime ctrl_params from wasa runtime file parameters.out

#Till 14.4.08

parse_wasa_ctrl_params=function(wasa_param_file="../output/Esera_1998-2006/parameter.out")
{
  
  ctrl_params=data.frame(a=0)

  
  content=scan(wasa_param_file,what="character",sep="\n",blank.lines.skip=T,comment.char="#",strip.white=T,quiet=T)

  {
    start_line=1
    ctrl_params$input_dir=content[start_line]
    ctrl_params$output_dir=content[start_line+1]
    ctrl_params$start_year =as.numeric(strsplit(content[start_line+2], ":")[[1]][2])
    ctrl_params$end_year   =as.numeric(strsplit(content[start_line+3], ":")[[1]][2])
    tt                     =           strsplit(content[start_line+4], ":")[[1]][2]
    if (!is.finite(tt)) tt = na.omit(as.numeric(strsplit(tt, split="\\s+")[[1]]))[1]
    ctrl_params$start_month=tt
    
    tt                     =           strsplit(content[start_line+5], ":")[[1]][2]
    if (!is.finite(tt)) tt = na.omit(as.numeric(strsplit(tt, split="\\s+")[[1]]))[1]
    ctrl_params$end_month  =tt
    
  }

#  requested_pars=names(ctrl_params) %in% param_list  #find reqested parameters that have been extracted
#  ctrl_params=ctrl_params[,requested_pars]                #only return requested pars
  return(ctrl_params)
}