#compute goodness-of-fit measures between WASA-modelled and measured data

#Till 21.3.11: improved error messages
#Till 7.8.08: compute phase/membership specific rmse's
#Till 5.7.08
#Till 3.4.08
# compute goodness measures specified number of sub-periods

compute_goodness_measures=function(wasa_output_dir,wasa_input_dir,subbas_id=2,n_periods=12,start_date=NULL,end_date=NULL,target_component="River_Flow")
{
# start_date : 
#  NULL: use full simulation period for goodness measures
#  "obs":  only use extent of observation period for evaluation (these two are different in terms of the subdivision into subperiods; the time-periods are the same)
#  <date>:  start evaluation from <date> (needs also specification of end_date)



source("read_wasa_func.R")


`nashS` <- function (modelled, measured, weigth=NA) { #nash sutcliffe index
    t.na <- is.na(measured) | is.na(modelled)
    t.meas <- measured[!t.na]
    t.model <- modelled[!t.na]
    t.mean <- mean(t.meas)
    if(is.na(weigth[1])){
        weigth <- rep(1,NROW(t.meas))
    } else {
        weigth <- weigth[!t.na]
    }
    t.a <- (t.meas - t.model)*weigth
    t.b <- (t.meas - t.mean)*weigth
    ns <- 1- ( t.a %*% t.a / t.b %*% t.b)
    return(ns)
}

replaceNaNs = function (input,replace_value) 
{ #replace NaN values in input vector by its mean; if this is not available, use replace_value
   meanv=mean(input,na.rm=TRUE)
  if (is.finite(meanv))   
    replace_value=meanv                                  #use parsed replace_value only if no mean can be computed
  input[!is.finite(input)]= replace_value   #replace NaNs
  return(input)
}



#import modelled data
  ctrl_params=parse_wasa_ctrl_params(wasa_param_file=paste(wasa_output_dir,"parameter.out",sep=""))  #get WASA runtime information (input output paths etc)
  ctrl_params$input_dir  = wasa_input_dir   #override pathnames from WASA-file (useful in postprocessing)
  ctrl_params$output_dir = wasa_output_dir
  components_list=target_component           #River_Flow,"daily_actetranspiration","daily_water_subbasin","River_Sediment_total"
  xx=read_wasa_results(ctrl_params, components_list, subbas_id)    # read all date and model results selected in components_list
  datevec= xx[[1]]       #date information
  d.wasa.out <- t(xx[[2]]) #modelled components
  dim(d.wasa.out)=c(1,dim(d.wasa.out))    #reshape array to match style of WASIM-package
  dimnames(d.wasa.out)[2]= dimnames(xx[[2]])[2] #keep names
  data.types=data.frame(X1.29=nchar(components_list),beschreibung_en=components_list, prefix=components_list, has_stat=is.null(components_list))  #match style of WASIM-package



#load and convert observed discharges and sediment data

  d.meas.all = read_observations(subbas_id, datevec, target_component, wasa_input_dir)   #import measured data that match the timespan of the modelled data
  if (all(is.na(d.meas.all[,-1]))) stop("no observation data in timespan. Check discharge_obs_*.txt") 
  if (ncol(d.meas.all) < length(subbas_id)+1) stop("Couldn't find all subbasin data (found ", paste(names(d.meas.all)[-1], collapse=", "), "). Check discharge_obs_*.txt") 
  
  if (is.null(start_date))
  {
    start_date=min(datevec)    #get start and end of modelled time series
    end_date=max(datevec)
  } else  {
    if (is.character(start_date) && start_date=="obs")
    {
      start_date=d.meas.all$datenum[min(which(!is.na(d.meas.all[,2])))]    #get start end end of observed time series
      end_date=d.meas.all$datenum[max(which(!is.na(d.meas.all[,2])))]
    }
  }
  

  #trim observed and modelled data to desired time period
  d.meas.all=d.meas.all[d.meas.all$datenum>=start_date & d.meas.all$datenum<=end_date,]
  if (nrow(d.meas.all) == 0) stop("compute_goodness_measures: measured data doesn't cover required timespan")

  d.wasa.out = d.wasa.out[,,datevec>=start_date & datevec<=end_date, drop=FALSE]
  if (length(d.wasa.out)==0 || length(d.wasa.out[1,1,]) == 0) stop("compute_goodness_measures: modelled data doesn't cover required timespan")


  datevec = datevec[datevec>=start_date & datevec<=end_date]
  
return_val=data.frame()


for (p in 1:n_periods) #compute goodness measures for each of "n_periods" parts of the time series
{
  start_from = round((p-1)/n_periods*length(datevec))+1    #compute indices of begin and end of current period
  go_till    = round((p-0)/n_periods*length(datevec))
  curr_period= start_from:go_till

  
  
  discharge_component=which(target_component %in% c("River_Flow","daily_water_subbasin"))[1] #check, which of the loaded components correspondds to discharge
  array_offset_q = length(subbas_id) * (discharge_component-1) #for addressing the discharge data
  
  sed_component=which(target_component %in% c("River_Sediment_total"))[1] #check, which of the loaded components corresponds to sediment
  array_offset_sed = length(subbas_id) * (sed_component-1) #for addressing the sediment data
  
  #compute measure for goodness-of-fit
  for (subbas_counter in 1:length(subbas_id))
  {
#    browser()
    
    #assign water fluxes
    qtotal_mod  = d.wasa.out[1, array_offset_q + subbas_counter, curr_period]    #extract subperiod
    qtotal_meas = d.meas.all[curr_period, paste0("sub", subbas_id[subbas_counter])]    

    qbase_mod=0*qtotal_mod  #dummy assignment
    q_base_meas = qbase_mod
    
    #assign sediment fluxes
    q_sed_mod = d.wasa.out[1, array_offset_sed + subbas_counter, curr_period]
    q_sed_meas = d.meas.all[curr_period, paste0("monthly_yield_sub", subbas_id[subbas_counter])]    

    if (any(grepl(pattern="monthly_yield", x=names(d.meas.all)))) #if observations are monthly sediment yield
    {
      compiled=data.frame(datevec=datevec, q_sed_mod = q_sed_mod, q_sed_meas=q_sed_meas)
      no_nas=range(which(!is.na(q_sed_mod+q_sed_meas))) #find first and last no-na record
      #compiled=compiled[no_nas[1]:no_nas[2],]
      compiled$month_code = (as.POSIXlt(compiled$datevec)[["year"]]-100)*100 + (as.POSIXlt(compiled$datevec)[["mon"]]+1)
      compiled=compiled[compiled$month_code >= compiled$month_code[no_nas[1]] &
                        compiled$month_code <= compiled$month_code[no_nas[2]], ] 
      
      tt=aggregate(x=compiled[,-c(1,4)], by=list(compiled$month_code), FUN=sum, na.rm=TRUE)[,-1]
      q_sed_mod=tt$q_sed_mod
      q_sed_meas=tt$q_sed_meas
    }  

      
    
    if (any(is.nan(qtotal_mod)))
    {
      rmse=99999
      cor_total=-1
      bias_total=99999
      bias_total_rel=99999
      rmse_loflo=99999
      rmse_loflo=99999
      ns_coeff=-1
    } else {
      rmse=sqrt(mean((qtotal_meas-qtotal_mod)^2,na.rm=TRUE))
      cor_total=cor(qtotal_meas,qtotal_mod,use="complete.obs")
      bias_total=mean(qtotal_meas-qtotal_mod,na.rm=TRUE)
      bias_total_rel=bias_total/mean(qtotal_meas,na.rm=TRUE)  #relative bias
      flood_period=qtotal_meas>=2*median(qtotal_meas,na.rm=TRUE)  #mark flood periods
      rmse_hiflo=sqrt(mean((qtotal_meas[flood_period]-qtotal_mod[flood_period])^2,na.rm=TRUE))    #compute rmse for high flow conditions
      rmse_loflo=sqrt(mean((qtotal_meas[!flood_period]-qtotal_mod[!flood_period])^2,na.rm=TRUE))  #compute rmse for low flow conditions
      ns_coeff=nashS(qtotal_mod,qtotal_meas)
      if ((length(q_sed_mod)>0) & (length(q_sed_meas)>0)) #compute goodness measures for sediment
      {
        rmse_sed=sqrt(mean((q_sed_meas-q_sed_mod)^2,na.rm=TRUE))
        
        bias_total_sed=mean(q_sed_meas-q_sed_mod,na.rm=TRUE)
        bias_total_sed_rel=bias_total_sed/mean(q_sed_meas,na.rm=TRUE)  #relative bias
      }
      
    }
    
    if (any(is.nan(qbase_mod)))
    {
      rmse_qbase=99999
    } else {
      rmse_qbase=sqrt(mean((q_base_meas-qbase_mod)^2,na.rm=TRUE))
    }
        
    
    mean_q_base_meas=mean(q_base_meas,na.rm=TRUE)
    penalty_lo_qbas=max(0,0.5*mean_q_base_meas-mean(qbase_mod,na.rm=TRUE))/(0.5*mean_q_base_meas)       #penalize low mean base flow [0..1]
    
#     if ("River_Sediment_total" %in% target_component) # load observed sediment concentrations
#     {
#       ssc_load_mod=d.wasa.out[1,which(data.types$beschreibung_en == "River_Sediment_total"),curr_period]    #extract subperiod
#       ssc_load_meas=d.meas.all[curr_period,which(target_component == "River_Sediment_total")]    
#       sed_bias=sum(ssc_load_meas-ssc_load_mod,na.rm=TRUE)
#     }   else 
    sed_bias=-99999
    
    
  #  if ((is.nan(rmse)) || (is.na(rmse)))     #avoid nodata values
  #    rmse=99999
  #  if ((is.nan(rmse_qbase)) || (is.na(rmse_qbase)))
  #    rmse_qbase=99999
  #  if ((is.nan(bias_total)) || (is.na(bias_total)))
  #    bias_total=99999
  #  if ((is.nan(penalty_lo_qbas)) || (is.na(penalty_lo_qbas)))
  #    penalty_lo_qbas=99999
  #  if ((is.nan(ns_coeff)) || (is.na(ns_coeff)) || (ns_coeff==-Inf))
  #    ns_coeff=-99999
  #    
  
  measures = data.frame(rmse_qtotal=rmse,rmse_qbas=rmse_qbase,cor_total=cor_total, bias_total=bias_total, bias_total_rel=bias_total_rel, penalty_lo_qbas=penalty_lo_qbas,rmse_hiflo=rmse_hiflo,rmse_loflo=rmse_loflo,ns_coeff=ns_coeff,
                        sed_bias=sed_bias)
  if ((length(q_sed_mod)>0) & (length(q_sed_meas)>0))
    measures=cbind(measures, cbind(rmse_sed=rmse_sed, bias_total_sed=bias_total_sed, bias_total_sed_rel=bias_total_sed_rel))
  if (n_periods > 1)
    names(measures) = paste0(names(measures), "_",p)  #append period number to column names
                             
  if (length(subbas_id)>1)
    names(measures) = paste0(names(measures), "_sub", subbas_id[subbas_counter]) #append subbasin id to column names
  
  if (nrow (return_val)==0) return_val = measures else  
    return_val = cbind(return_val, measures) #assemble return value
  } #end loop thru all subbasins
}

# return_val$rmse_limit=return_val$rmse_qtotal         #create columns
# return_val$ns_coeff_limit=return_val$rmse_qtotal




# #period without major floods
#   start_date=ISOdatetime(2006, 10, 1, hour=0, min=0, sec=0, tz = "GMT")
#   end_date=ISOdatetime(2008, 3, 31, hour=0, min=0, sec=0, tz = "GMT")              
#   
#   stats_period=(datevec>=start_date & datevec<=end_date)
#   qtotal_mod=d.wasa.out[1,which(data.types$beschreibung_en == target_component[1]),stats_period]    #do calculation for entire period
#   qtotal_meas=d.meas.all[stats_period,2]    
#   
#   return_val$rmse_limit[]=sqrt(mean((qtotal_meas-qtotal_mod)^2,na.rm=TRUE))
#   return_val$ns_coeff_limit[]=nashS(qtotal_mod,qtotal_meas)
#   
#   if (0)
#   {
#     membership=membership[stats_period,]
#     return_val$rmse_peak_limit     =rep(sqrt(weighted.mean((qtotal_meas-qtotal_mod)^2,membership$membership_peak,     na.rm=TRUE)),nrow(return_val))      #compute membership-specific rmse's
#     return_val$rmse_lowflow_limit  =rep(sqrt(weighted.mean((qtotal_meas-qtotal_mod)^2,membership$membership_lowflow,  na.rm=TRUE)),nrow(return_val))
#     return_val$rmse_recession_limit=rep(sqrt(weighted.mean((qtotal_meas-qtotal_mod)^2,membership$membership_recession,na.rm=TRUE)),nrow(return_val))
#     return_val$rmse_rise_limit     =rep(sqrt(weighted.mean((qtotal_meas-qtotal_mod)^2,membership$membership_rise,     na.rm=TRUE)),nrow(return_val))
#   }



# return_val$rmse_qtotal    =replaceNaNs(return_val$rmse_qtotal,99999)  #set rmse of periods without data to mean of all periods (avoids nodata but doesnt introduce bias)
# return_val$rmse_qbas      =replaceNaNs(return_val$rmse_qbas,99999)  #set rmse_qbas of periods without data to mean of all periods (avoids nodata but doesnt introduce bias)
# return_val$bias_total     =replaceNaNs(return_val$bias_total,99999)  #set bias_total of periods without data to mean of all periods (avoids nodata but doesnt introduce bias)
# return_val$penalty_lo_qbas=replaceNaNs(return_val$penalty_lo_qbas,99999)  #set penalty_lo_qbas of periods without data to mean of all periods (avoids nodata but doesnt introduce bias)
# return_val$ns_coeff       =replaceNaNs(return_val$ns_coeff,-99999)  #set ns_coeff of periods without data to mean of all periods (avoids nodata but doesnt introduce bias)
# 
# return_val$rmse_hiflo     =replaceNaNs(return_val$rmse_hiflo,99999)  #set rmse of periods without hi flow to mean of all periods (avoids nodata but doesnt introduce bias)
# return_val$rmse_loflo     =replaceNaNs(return_val$rmse_loflo,99999)  #set rmse of periods without lo flow to mean of all periods (avoids nodata but doesnt introduce bias)
attr(return_val,"subbasin_names") = attr(d.meas.all, "subbasin_names")[-1]

return(return_val)
}
  
