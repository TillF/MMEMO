# modify specified parameter in WASA input files

#Till, 15.12.2009
# corrected bug river params (only Muskingum_K and Q_Spring were affected)

#Till, 13.11.2009
# added river params

#Till, 18.6.2009
# corrected case of input dirs

#Till, 2.4.2009
# modify calibration.dat (parameter ksat_factor)

# Till, 19.8.2008
# create files from template, also if not listed in "parameters" - overwrites possible older modifications of input files

# Till, 5.6.2008
#included kfcorr0 and kfcorr_a

# Till, 23.4.2008

modify_wasa_input=function(wasa_input_dir,parameters)
{
  #parameters : named dataframe with single row
  
  #create files from backup copies, if those exist
  # target_file=paste(wasa_input_dir,"Hillslope/soter",sep="") #file that holds the parameters to be changed
  # if (file.exists(paste(target_file,".calib_bak",sep="")))   
  #     file.copy(paste(target_file,".calib_bak",sep=""),paste(target_file,".dat",sep=""),overwrite = TRUE)
  
  # target_file=paste(wasa_input_dir,"do",sep="") #file that holds the parameters to be changed
  # if (file.exists(paste(target_file,".calib_bak",sep="")))   
  #     file.copy(paste(target_file,".calib_bak",sep=""),paste(target_file,".dat",sep=""),overwrite = TRUE)
  
  # target_file=paste(wasa_input_dir,"frac_direct_gw",sep="") #file that holds the parameters to be changed
  # if (file.exists(paste(target_file,".calib_bak",sep="")))   
  #     file.copy(paste(target_file,".calib_bak",sep=""),paste(target_file,".dat",sep=""),overwrite = TRUE)
  
  # target_file=paste(wasa_input_dir,"Others/scaling_factor",sep="") #file that holds the parameters to be changed
  # if (file.exists(paste(target_file,".calib_bak",sep="")))   
  #      file.copy(paste(target_file,".calib_bak",sep=""),paste(target_file,".dat",sep=""),overwrite = TRUE)
  
  # target_file=paste(wasa_input_dir,"River/river",sep="") #file that holds the parameters to be changed
  # if (file.exists(paste(target_file,".calib_bak",sep="")))   
  #      file.copy(paste(target_file,".calib_bak",sep=""),paste(target_file,".dat",sep=""),overwrite = TRUE)
  
  
  
  no_params=NCOL(parameters)
  while(no_params>0)
  {
##############################################
    if (names(parameters)[1] %in% c("Manning_n_f","Manning_n"))        #parameters in svc.dat
        {
      # multiply manning_n in svc.dat by factor
      
            target_file=paste(wasa_input_dir,"Hillslope/svc",sep="") #file that hold the parameters to be changed

           
            if (!file.exists(paste(target_file,".calib_bak",sep="")))   #create backup if not existing
                file.copy(paste(target_file,".dat",sep=""),paste(target_file,".calib_bak",sep=""))

                  #browser()
            file_content = read.table(paste(target_file,".calib_bak",sep=""), skip=2,header = FALSE, sep = "\t", dec = ".", fill = TRUE)
            if (all(!is.finite(file_content[,ncol(file_content)]))) file_content[,ncol(file_content)]=NULL  #discard last column if empty

            nn= which(names(parameters)=="Manning_n_f")
            if (length(nn)>0) #modify Manning_n_f
            {
              file_content[,ncol(file_content)]=file_content[,ncol(file_content)]*parameters[1,nn]
              parameters=parameters[-nn] #remove this parameter from list
              no_params=no_params-1
            }
            
            nn= which(names(parameters)=="Manning_n")
            if (length(nn)>0) #modify Manning_n_f
            {
              file_content[,ncol(file_content)]=parameters[1,nn]
              parameters=parameters[-nn] #remove this parameter from list
              no_params=no_params-1
            }
            
      #re-write file
            content=paste("Specifications of of soil-vegetation components and erosion parameters\nid	soil_id	veg_id	musle_k[(ton acre hr)/(acre ft-ton inch)]	musle_c1[-]	musle_p[-]	coarse_fraction[%]	manning_n",sep = "")
            write(content, file = paste(target_file,".dat",sep=""))     #write header
            write.table(file_content, file = paste(target_file,".dat",sep=""), append = TRUE, quote = F,row.names=F,col.names=F,sep="\t", na = "")
            next
        }

##############################################
    if (names(parameters)[1] %in% c("erosion_equation","ri_05_coeffs_a_f","ri_05_coeffs_b_f","transport_limit_mode","transp_cap_a", "transp_cap_b"))        #parameters in erosion.ctl
        {
          target_file=paste(wasa_input_dir,"erosion",sep="") #file that holds the parameters to be changed
            
            if (!file.exists(paste(target_file,".calib_bak",sep="")))   #create backup if not existing
                file.copy(paste(target_file,".ctl",sep=""),paste(target_file,".calib_bak",sep=""))

            #file_content = read.table(paste(target_file,".calib_bak",sep=""), skip=0,header = FALSE, sep = "$", dec = ".", fill = TRUE)
            #if (all(!is.finite(file_content[,ncol(file_content)]))) file_content[,ncol(file_content)]=NULL  #discard last column if empty
            file_content  = scan(paste(target_file,".calib_bak",sep=""),what="character",sep="\n",strip.white=T,quiet=T)
          
            file_content[1]=paste0(file_content[1],"  - modified by modify_wasa_input.R")
            
            nn= which(names(parameters)=="erosion_equation")
            if (length(nn)>0) #modify erosion equation
                {
                    cur_line=which(grepl(file_content, pattern="^erosion_equation"))
                    if (length(cur_line)==0) cur_line=length(file_content)+1
                    parameters[1,nn] <- round(parameters[1,nn])
                    parameters[1,nn] = max(1,parameters[1,nn])
                    parameters[1,nn] = min(4,parameters[1,nn])
                                        
                    file_content[cur_line]=paste("erosion_equation",parameters[1,nn],"#erosion equation to be used: 1: USLE, 2: Onstad-Foster, 3: MUSLE, 4: MUST",sep="\t")
                                        #modify erosion equation
                    parameters=parameters[-nn] #remove this parameter from list
                    no_params=no_params-1
                }
            
            #browser()
            if (length ( intersect (names(parameters), c("ri_05_coeffs_a_f","ri_05_coeffs_b_f")))>0)
            {
              cur_line=which(grepl(file_content, pattern="^ri_05_coeffs"))
              if (length(cur_line)==0) #add line, if absent
              {
                  cur_line=length(file_content)+1
                  file_content[cur_line]="ri_05_coeffs	1	1	#needed for USLE and OF: coefficients for estimation of maximum half-hour rainfall intensity (ri_05) from daily rainfall data (R_day): ri_05=a*R_day^b"
              }  
            }
            
            nn= which(names(parameters)=="ri_05_coeffs_a_f")
            if (length(nn)>0) #modify parameter a in rainfall intensity
                {
                    default <- unlist(strsplit(file_content[cur_line],split="\t")) #these are the default values for rainfall intensity parameters
                    file_content[cur_line]= paste(default[1],parameters[1,nn]*as.numeric(default[2]),default[3], default[4], sep="\t")
                    parameters=parameters[-nn] #remove this parameter from list
                    no_params=no_params-1
                }

            nn= which(names(parameters)=="ri_05_coeffs_b_f")
            if (length(nn)>0) #modify parameter b in rainfall intensity
                {
                    default <- unlist(strsplit(file_content[cur_line],split="\t")) #these are the default values for rainfall intensity parameters      
                    file_content[cur_line]= paste(default[1], default[2], parameters[1,nn]*as.numeric(default[3]),default[4], sep="\t")
                    parameters=parameters[-nn] #remove this parameter from list
                    no_params=no_params-1
                }

            nn= which(names(parameters)=="transport_limit_mode")
            if (length(nn)>0) #modify transport limit mode
                {
                    parameters[1,nn] = ceiling(parameters[1,nn])      
                    parameters[1,nn] = max(1,parameters[1,nn])
                    parameters[1,nn] = min(3,parameters[1,nn])
                    
                    cur_line=which(grepl(file_content, pattern="^transport_limit_mode"))
                    if (length(cur_line)==0) cur_line=length(file_content)+1
                    
                    file_content[cur_line]=paste("transport_limit_mode",parameters[1,nn],"#different modes how/if transport capacity of runoff is limited: 1: no transport capacity limit; 2: transport capacity according to Everaert (1991); 3:transport capacity computed from MUSLE with maximum erodibility)",sep="\t")
                    parameters=parameters[-nn] #remove this parameter from list
                    no_params=no_params-1
              }
            
            
            nn= which(names(parameters)=="transp_cap_a")
            if (length(nn)>0) #modify coefficients for transport capacity in river
            {
              cur_line=which(grepl(file_content, pattern="^transp_cap_a"))
              if (length(cur_line)==0) cur_line=length(file_content)+1
              file_content[cur_line]=paste("transp_cap_a",parameters[nn],"empirical factor for computing suspended sediment transport capacity in river (a * vel_peak ** b)",sep="\t")
              parameters=parameters[-nn] #remove this parameter from list
              no_params=no_params-1
            }
            
            nn= which(names(parameters)=="transp_cap_b")
            if (length(nn)>0) #modify coefficients for transport capacity in river
            {
              cur_line=which(grepl(file_content, pattern="^transp_cap_b"))
              if (length(cur_line)==0) cur_line=length(file_content)+1
              file_content[cur_line]=paste("transp_cap_b",parameters[nn],"empirical factor for computing suspended sediment transport capacity in river (a * vel_peak ** b)",sep="\t")
              parameters=parameters[-nn] #remove this parameter from list
              no_params=no_params-1
            }
            
            #re-write file
            write.table(file_content, file = paste(target_file,".ctl",sep=""), append = F, quote = F,row.names=F,col.names=F,sep="\t")
            
#            parameters=parameters[-1] #remove this parameter from list
 #           no_params=no_params-1
            next
        }


##############################################  
    if (names(parameters)[1] %in% c("gw_delay_f","soildepth_f","kf_bedrock_f","riverdepth_f"))        #parameters in soter.dat
    {
      # multiply ground water delay by specified factor in soter.dat
      # multiply soil depth by specified factor in soter.dat
      
      target_file=paste(wasa_input_dir,"Hillslope/soter",sep="") #file that hold the parameters to be changed
      
      if (!file.exists(paste(target_file,".calib_bak",sep="")))   #create backup if not existing
        file.copy(paste(target_file,".dat",sep=""),paste(target_file,".calib_bak",sep=""))
      
      #browser()
      file_content = read.table(paste(target_file,".calib_bak",sep=""), skip=2,header = FALSE, sep = "\t", dec = ".", fill = TRUE)
      if (all(!is.finite(file_content[,ncol(file_content)]))) file_content[,ncol(file_content)]=NULL  #discard last column if empty
      
      #consider shorter lines (LUs with less TCs) and bring fields to consistent position in matrix
      max_n_tcs=max(file_content[,2])
      shorter_lines=which(file_content[,2] != max_n_tcs)
      n_fields=ncol(file_content)-max_n_tcs -2 #number of fields after specification of TC-IDs
      for (ll in shorter_lines)
      {
        n_tcs = file_content[ll,2]
        
        file_content[ll, 2+max_n_tcs+(1:n_fields)] = 
          file_content[ll, 2+n_tcs    +(1:n_fields)] 
        
      }  
      
      nn= which(names(parameters)=="gw_delay_f")
      if (length(nn)>0) #modify ground water delay
      {
        file_content[,ncol(file_content)]=file_content[,ncol(file_content)]*parameters[1,nn]
        parameters=parameters[-nn] #remove this parameter from list
        no_params=no_params-1
      }
      
      nn= which(names(parameters)=="soildepth_f")
      if (length(nn)>0) #modify soil depth
      {
        not_minus1=file_content[,ncol(file_content)-4]!=-1   #only modify entries not having flag=-1
        file_content[not_minus1,ncol(file_content)-4]=file_content[not_minus1,ncol(file_content)-4]*parameters[1,nn]
        not_minus1=file_content[,ncol(file_content)-5]!=-1
        file_content[not_minus1,ncol(file_content)-5]=file_content[not_minus1,ncol(file_content)-5]*parameters[1,nn]
        parameters=parameters[-nn] #remove this parameter from list
        no_params=no_params-1
      }
      
      nn= which(names(parameters)=="kf_bedrock_f")
      if (length(nn)>0) #modify bedrock conductivity
      {
        file_content[,ncol(file_content)-7]=file_content[,ncol(file_content)-7]*parameters[1,nn]
        parameters=parameters[-nn] #remove this parameter from list
        no_params=no_params-1
      }
      
      nn= which(names(parameters)=="riverdepth_f")
      if (length(nn)>0) #modify depth of riverbed
      {
        file_content[,ncol(file_content)-3]=file_content[,ncol(file_content)-3]*parameters[1,nn]
        parameters=parameters[-nn] #remove this parameter from list
        no_params=no_params-1
      }
      
      
      #consider shorter lines (LUs with less TCs) and bring fields to "sparse" representation (unequal number of fields)
      for (ll in shorter_lines)
      {
        file_content[ll,]
        n_tcs = file_content[ll,2]
        
        file_content[ll, 2+n_tcs    +(1:n_fields)] =
          file_content[ll, 2+max_n_tcs+(1:n_fields)]  
        file_content[ll, ncol(file_content)+1-1:(max_n_tcs-n_tcs)] = NA #mask obsolete fields with NA
      }    
      
      
      #re-write file
      content=paste("Specification of landscape units\nLU-ID[id]  No._of_TC[-]	TC1[id]	TC2[id]	TC3[id]	kfsu[mm/d]	length[m]	meandep[mm]	maxdep[mm]	riverbed[mm]	gwflag[0/1]	gw_dist[mm]	frgw_delay[day]",sep = "")
      write(content, file = paste(target_file,".dat",sep=""))     #write header
      write.table(file_content, file = paste(target_file,".dat",sep=""), append = TRUE, quote = F,row.names=F,col.names=F,sep="\t", na = "")
      next
    }
    
    if (names(parameters)[1] %in% c("kfcorr","kfcorr0","kfcorr_a", "dosediment"))                #params in do.dat
    {
      # adjust kfcorr in do.dat
      target_file=paste(wasa_input_dir,"do",sep="") #file that hold the parameters to be changed
      
      if (!file.exists(paste(target_file,".calib_bak",sep="")))   #create backup if not existing
        file.copy(paste(target_file,".dat",sep=""),paste(target_file,".calib_bak",sep=""))
      
      if (!file.exists(paste(target_file,".calib_bak",sep="")))   #create backup if not existing
        stop(paste(target_file,".calib_bak or .dat not found.",sep=""))
      
      file_content = read.table(paste(target_file,".calib_bak",sep=""), skip=0,header = FALSE, sep = "$",stringsAsFactors=FALSE)

      file_content[1,1]=paste0(file_content[1,1],"  - modified by modify_wasa_input.R")
      nn= which(names(parameters)=="kfcorr")
      if (length(nn)>0) #modify kfcorr
      {
        file_content[24,1]=paste(parameters[1,nn],"  //kfcorr:  hydraulic conductivity factor (for daily model version) (kfcorr)",sep="")
        #modify kfcorr
        parameters=parameters[-nn] #remove this parameter from list
        no_params=no_params-1
      }
      
      nn= which(names(parameters)=="kfcorr0")
      if (length(nn)>0) #modify kfcorr0, kfcorr_a
      {
        file_content[24,1]=paste(parameters[1,nn]," ",sep="")
        parameters=parameters[-nn] #remove this parameter from list
        no_params=no_params-1
        nn= which(names(parameters)=="kfcorr_a")
        file_content[24,1]=paste(file_content[24,1],parameters[1,nn]," 1	//kfcorr:  hydraulic conductivity factor (for daily model version) (kfcorr0) [optional: a <tab> b for kfcorr=kfcorr0*(a*1/daily_precip+b) ",sep="")
        #modify kfcorr0 and kfcorr_a
        parameters=parameters[-nn] #remove this parameter from list
        no_params=no_params-1
      }
      
      nn= which(names(parameters)=="dosediment")
      if (length(nn)>0) #modify dosediment
      {
        file_content[31,1]=paste(parameters[1,nn],"  //dosediment")
        parameters=parameters[-nn] #remove this parameter from list
        no_params=no_params-1
      }
      
      
      #rewrite file         
      write.table(file_content, file = paste(target_file,".dat",sep=""), append = F, quote = F,row.names=F,col.names=F,sep="\t")
      
      parameters=parameters[-1] #remove this parameter from list
      no_params=no_params-1
      next
    }
    if (names(parameters)[1] %in% c("f_gw_direct"))                                    #params in frac_direct_gw.dat
    {
      # adjust frac_direct_gw in frac_direct_gw.dat
      target_file=paste(wasa_input_dir,"frac_direct_gw",sep="") #file that hold the parameters to be changed
      
      
      if (!file.exists(paste(target_file,".calib_bak",sep="")))   #create backup if not existing
        file.copy(paste(target_file,".dat",sep=""),paste(target_file,".calib_bak",sep=""))
      
      if (!file.exists(paste(target_file,".calib_bak",sep="")))   #create backup if not existing
        stop(paste(target_file,".calib_bak or .dat not found.",sep=""))
      
      #modify frac_direct_gw
      
      write.table(parameters[1], file = paste(target_file,".dat",sep=""), append = F, quote = F,row.names=F,col.names=F,sep="\t")
      parameters=parameters[-1] #remove this parameter from list
      no_params=no_params-1
      next
    } 
    
    if (names(parameters)[1] %in% c("kf_scale_f"))                             #params in  scaling_factor.dat
    {
      # adjust scaling factors for kf in scaling_factors.dat
      target_file=paste(wasa_input_dir,"Others/scaling_factor",sep="") #file that hold the parameters to be changed
      
      if (!file.exists(paste(target_file,".calib_bak",sep="")))   #create backup if not existing
        file.copy(paste(target_file,".dat",sep=""),paste(target_file,".calib_bak",sep=""))
      
      if (!file.exists(paste(target_file,".calib_bak",sep="")))   #create backup if not existing
        stop(paste(target_file,".calib_bak or .dat not found.",sep=""))
      
      #modify scaling_factor
      file_content = read.table(paste(target_file,".calib_bak",sep=""), skip=1,row.names=NULL, header = FALSE, sep = "\t", dec = ".")
      file_content[,2]=file_content[,2]*parameters[1,1]
      
      content=paste("Subasin-ID","mean_kf-calib-factor",sep="\t")
      write(content, file = paste(target_file,".dat",sep=""))     #write header
      write.table(file_content, file = paste(target_file,".dat",sep=""), append = TRUE, quote = F,row.names=F,col.names=F,sep="\t")
      parameters=parameters[-1] #remove this parameter from list
      no_params=no_params-1
      next
    } 
    
    if (names(parameters)[1] %in% c("ksat_factor"))                            #params in calibration.dat
    {
      # adjust ksat calibration factors in calibration.dat
      target_file=paste(wasa_input_dir,"Others/calibration",sep="") #file that holds the parameters to be changed
      
      if (!file.exists(paste(target_file,".calib_bak",sep="")))   #create backup if not existing
        file.copy(paste(target_file,".dat",sep=""),paste(target_file,".calib_bak",sep=""))
      
      if (!file.exists(paste(target_file,".calib_bak",sep="")))   #create backup if not existing
        stop(paste(target_file,".calib_bak or .dat not found.",sep=""))
      
      #modify scaling_factor
      file_content = read.table(paste(target_file,".calib_bak",sep=""), skip=1,row.names=NULL, header = FALSE, sep = "\t", dec = ".")
      file_content[,2]=file_content[,2]*parameters[1,1]
      
      content=paste("Soil-ID","Ksat-calib-factor",sep="\t")
      write(content, file = paste(target_file,".dat",sep=""))     #write header
      write.table(file_content, file = paste(target_file,".dat",sep=""), append = TRUE, quote = F,row.names=F,col.names=F,sep="\t")
      parameters=parameters[-1] #remove this parameter from list
      no_params=no_params-1
	  next
    } 
    
    river_param_names=c("riv_depth", "riv_width", "riv_side_ratio", "riv_bottom_width_of_floodplain", "riv_side_ratio_floodplains", "riv_channel_slope", "riv_length", "riv_manningn", "riv_manningn_floodplain", "riv_Ksat", "riv_erodibilityfactor", "riv_coverfactor", "riv_riverbedrock", "riv_baseflowalphafactor", "riv_Muskingum_X", "riv_Muskingum_K", "riv_Q_spring")
    if (      sub(names(parameters)[1],pattern ="_f$*", repl="") %in% river_param_names ) 
    {
      target_file=paste(wasa_input_dir,"River/river",sep="") #file that holds the parameters to be changed
      
      if (!file.exists(paste(target_file,".calib_bak",sep="")))   #create backup if not existing
        file.copy(paste(target_file,".dat",sep=""),paste(target_file,".calib_bak",sep=""))
      
      if (!file.exists(paste(target_file,".calib_bak",sep="")))   #create backup if not existing
        stop(paste(target_file,".calib_bak or .dat not found.",sep=""))
      
      file_header  = scan(paste(target_file,".calib_bak",sep=""),what="character",sep="\n",n=2,strip.white=T,quiet=T)
      file_content = read.table(paste(target_file,".calib_bak",sep=""), skip=2,row.names=NULL, header = FALSE, sep = "\t", dec = ".")
      
      
      #modify river parameters by specified factor or use value directly
      while (any(sub(names(parameters),pattern ="_f$*", repl="") %in% river_param_names))
      {
        cur_param=as.vector(na.omit(match(river_param_names, sub(names(parameters),pattern ="_f$*", repl="")))[1])            #find the next river parameter in 'parameters'
        param_col=which(river_param_names==sub(pattern="_f$", repl="", names(parameters)[cur_param]))+1  #find the corresponding column to current parameter in 'river_param_names' and river.dat 
        if (grepl(pattern = "_f$", x = names(parameters)[cur_param]))
          file_content[,param_col]=file_content[,param_col]*parameters[1,cur_param] else  #use as factor modifying the current value
          file_content[,param_col]=                         parameters[1,cur_param]   #use directly
        parameters=parameters[-cur_param] #remove this parameter from list
        no_params=no_params-1 
      }
      write(file_header, file = paste(target_file,".dat",sep=""))     #write header
      write.table(file_content, file = paste(target_file,".dat",sep=""), append = TRUE, quote = F,row.names=F,col.names=F,sep="\t")
      next
    } else
    {
      stop(paste("Unknown parameter",names(parameters)[1]))
      parameters=parameters[-1] #remove this parameter from list
      no_params=no_params-1
    }
    
    
  } #end loop thru all parameter/value pairs
  
}
