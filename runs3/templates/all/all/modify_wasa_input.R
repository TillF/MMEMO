# modify specified parameter in WASA input files

#13.4.2018

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
  while(length(parameters)>0)
  {
#parameters in svc.dat ####
    if (names(parameters)[1] %in% c("Manning_n_f","Manning_n"))        
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

#parameters in erosion.ctl ####
    if (names(parameters)[1] %in% c("erosion_equation","ri_05_coeffs_a_f","ri_05_coeffs_b_f","transport_limit_mode","transp_cap_a", "transp_cap_b"))        
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

#parameters in soter.dat ####
    if (names(parameters)[1] %in% c("gw_delay_f","soildepth_f","kf_bedrock_f","riverdepth_f"))        
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
    
#params in do.dat ####
    if (names(parameters)[1] %in% c("kfcorr","kfcorr0","kfcorr_a", "intcf", "dosediment"))                
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
      
      nn= which(names(parameters)=="intcf")
      if (length(nn)>0) {
        file_content[25,1]=paste(round(parameters[nn], 2)," //intcf: interception capacity per unit LAI (mm)",sep="")
        parameters <- parameters[-nn]
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
    
#params in frac_direct_gw.dat    ####
    if (names(parameters)[1] %in% c("f_gw_direct"))                                    
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
    
#params in  scaling_factor.dat ####
    if (names(parameters)[1] %in% c("kf_scale_f"))                             
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
    
#params in calibration.dat ####
    if (names(parameters)[1] %in% c("ksat_factor"))                            
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

#params in vegetation.dat ####
    if (any(names(parameters) %in% c("stomr_f", "rootd_f", "albedo_f"))) {
        # file that hold the parameters to be changed
        target_file <- paste(wasa_input_dir,"Hillslope/vegetation.dat",sep="/")
        # read data
        file_content <- read.table(target_file, skip=2, header = FALSE, sep = "\t", dec = ".", fill = TRUE)
        if (all(!is.finite(file_content[,ncol(file_content)]))) file_content[,ncol(file_content)]=NULL  #discard last column if empty
        # multiply stomatal resistance in vegetation.dat by factor
        nn= which(names(parameters)=="stomr_f")
        if (length(nn)>0) {
          file_content[,2]=file_content[,2]*parameters[1,nn]
          parameters <- parameters[-nn]
        }
        # multiply rootdepth in vegetation.dat by factor (all 4 values)
        nn= which(names(parameters)=="rootd_f")
        if (length(nn)>0) {
          file_content[,c(9:12)]=file_content[,c(9:12)]*parameters[1,nn]
          parameters <- parameters[-nn]
        }
        # multiply albedo in vegetation.dat by factor (all 4 values)
        nn= which(names(parameters)=="albedo_f")
        if (length(nn)>0) {
          file_content[,c(17:20)]=pmin(1,file_content[,c(17:20)]*parameters[1,nn])
          parameters <- parameters[-nn]
        }
        #re-write file
        content=paste("Specification of vegetation parameters\nVeg-ID	Stomata_Resistance[s/m]	minsuction[hPa]	maxsuction[hPa]	height1[m]	height2[m]	height3[m]	height4[m]	rootdepth1[m]	rootdepth2[m]	rootdepth3[m]	rootdepth4[m]	LAI1[-]	LAI2[-]	LAI3[-]	LAI4[-]	albedo1[-]	albedo2[-]	albedo3[-]	albedo4[-]",sep = "")
        write(content, file = target_file)     #write header
        write.table(round(file_content, 3), file = target_file, append = TRUE, quote = F,row.names=F,col.names=F,sep="\t", na = "")
        next
      }
      
#params in soil.dat ####
    if (any(names(parameters) %in% c("n_f", "cfr"))) {
      # file that hold the parameters to be changed
      target_file=paste(wasa_input_dir,"Hillslope/soil.dat",sep="/")
      # read data
      file_content = read.table(target_file, skip=2,header = FALSE, sep = "\t", dec = ".", fill = TRUE)
      if (all(!is.finite(file_content[,ncol(file_content)]))) file_content[,ncol(file_content)]=NULL  #discard last column if empty
      
      # multiply porosity in soil.dat by factor (equally for every horizon and soil)
      nn= which(names(parameters)=="n_f")
      if (length(nn)>0) {
        # iterate over soils (lines in data)
        for (s in 1:nrow(file_content)) {
          # get number of horizons
          nhor = file_content[s,2]
          # update parameter
          file_content[s,12+(1:nhor-1)*13] = pmin(1,file_content[s,12+(1:nhor-1)*13]*parameters[1,nn])
        }
        parameters <- parameters[-nn]
      }
      # add 'cfr' to coarse fragments in soil.dat (additive!, equally for every horizon and soil)
      nn= which(names(parameters)=="cfr")
      if (length(nn)>0) {
        # iterate over soils (lines in data)
        for (s in 1:nrow(file_content)) {
          # get number of horizons
          nhor = file_content[s,2]
          # update parameter
          file_content[s,14+(1:nhor-1)*13] = pmin(1,pmax(0,file_content[s,14+(1:nhor-1)*13]+parameters[nn]))
        }
        parameters <- parameters[-nn]
      }
      #re-write file
      content=paste("Specification of soil parameters\nSoil-ID[-]	number(horizons)[-]	(n_res[Vol-]	n_PWP[-]	n_FK2.6[-]	n_FK1.8[-]	n_nFK[-]	n_saturated[-]	n_thickness[mm]	n_ks[mm/d]	n_suction[mm]	n_pore-size-index[-]	n_bubblepressure[cm]	n_coarse_frag[-]*n	n_shrinks[0/1])	bedrock[0/1]	alluvial[0/1]",sep = "")
      write(content, file = target_file)     #write header
      write.table(round(file_content,4), file = target_file, append = TRUE, quote = F,row.names=F,col.names=F,sep="\t", na = "")
      next
    }

#params in lake.dat / lake_maxvol.dat ####
    if (any(names(parameters) %in% c("f_lakemaxvol"))) {
      nn= which(names(parameters)=="f_lakemaxvol")
      # file that holds the parameters to be changed
      target_file=paste(wasa_input_dir,"Reservoir/lake_maxvol.dat",sep="/")
      if(file.exists(target_file)) {
        # read data
        file_content = read.table(target_file, header = FALSE, sep = "\t", skip=2)
        # update parameters
        file_content[,2:ncol(file_content)] <- file_content[,2:ncol(file_content)] * parameters[nn]
        # write updated parameters
        write(c("Specification of water storage capacity for the reservoir size classes",
                "Sub-basin-ID, maxlake[m**3] (five reservoir size classes)"),
              file = target_file, sep="\n")
        write.table(round(file_content, 2), target_file, append = T, row.names=F, col.names=F, quote=F, sep="\t")
      }
      # file that holds the parameters to be changed
      target_file=paste(wasa_input_dir,"Reservoir/lake.dat",sep="/")
      if(file.exists(target_file)) {
        # read data
        file_content = read.table(target_file, header = FALSE, sep = "\t", skip=2)
        # update parameters
        file_content[,2] <- file_content[,2] * parameters[nn]
        # write updated parameters
        write(c("Specification of parameters for the reservoir size classes",
                "Reservoir_class-ID, maxlake0[m**3], lake_vol0_factor[-], lake_change[-], alpha_Molle[-], damk_Molle[-], damc_hrr[-], damd_hrr[-]"),
              file = target_file, sep="\n")
        write.table(round(file_content, 2), target_file, append = T, row.names=F, col.names=F, quote=F, sep="\t")
      }
      parameters <- parameters[-nn]
      next
    }

#params in calib_wind.dat ####
    if (any(names(parameters) %in% c("f_wind"))) {
      # file that holds the parameters to be changed
      target_file=paste(wasa_input_dir,"Others/calib_wind.dat",sep="/")
      # read data
      file_content = read.table(target_file, header = FALSE, sep = "\t")
      nn= which(names(parameters)=="f_wind")
      if (length(nn)>0) {
        file_content=parameters[nn]
        parameters <- parameters[-nn]
      }
      write.table(round(file_content,3), file = target_file, quote = F,row.names=F,col.names=F,sep="\t")
      next
    }

#params in response.dat ####
    if (any(names(parameters) %in% c("uhg_f"))){
      # file that hold the parameters to be changed
      target_file=paste(wasa_input_dir,"River/response.dat",sep="/")
      # read data
      file_content = read.table(target_file, skip=2,header = FALSE, sep = "\t", dec = ".", fill = TRUE)
      if (all(!is.finite(file_content[,ncol(file_content)]))) file_content[,ncol(file_content)]=NULL  #discard last column if empty
      # multiply lag and response times in response.dat by factor
      nn= which(names(parameters)=="uhg_f")
      if (length(nn)>0) {
        file_content[,2:3] <- file_content[,2:3]*parameters[1,nn]
        parameters <- parameters[-nn]
      }
      #re-write file
      content=paste("Specification of routing parameter\nSubbasin-ID	lag time [d]	retention [d]",sep = "")
      write(content, file = target_file)     #write header
      write.table(round(file_content,2), file = target_file, append = TRUE, quote = F,row.names=F,col.names=F,sep="\t", na = "")
      next
    }
    
    
    
#params in river.dat ####    
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
    } 
    
#params in snow_params.ctl ####
    if(names(parameters)[1] %in% c("tempAir_crit", "weightAirTemp","specCapRet","emissivitySnowMin","a0"))
    {
      target_file=paste(wasa_input_dir,"Hillslope/snow_params",sep="") #file holding parameters to be changed
      
      if (!file.exists(paste(target_file,".calib_bak",sep="")))   #create backup if not existing
        file.copy(paste(target_file,".ctl",sep=""),paste(target_file,".calib_bak",sep=""))
      
      if (!file.exists(paste(target_file,".calib_bak",sep=""))) 
        stop(paste(target_file,".calib_bak or .ctl not found.",sep=""))
      
      #Modify selected parameters snow routine
      file_content = read.table(paste(target_file,".calib_bak",sep=""), skip=1,row.names=NULL, header = FALSE, sep = "\t", dec = ".")
      file_content[,2] <- as.character(file_content[,2])
      
      if( "tempAir_crit" %in% names(parameters) ){
        file_content[which(grepl(pattern = "tempAir", x=file_content[,1])),2] = parameters[which(names(parameters) == "tempAir_crit")]
        parameters=parameters[-which(names(parameters) == "tempAir_crit")] #remove paramerters from list
      }
      
      if( "weightAirTemp" %in% names(parameters) ){
        file_content[which(grepl(pattern = "weight", x=file_content[,1])),2] = parameters[which(names(parameters) == "weightAirTemp")]
        parameters=parameters[-which(names(parameters) == "weightAirTemp")] #remove paramerters from list
      }
      
      if( "specCapRet" %in% names(parameters) ){
        file_content[which(grepl(pattern = "spec", x=file_content[,1])),2] = parameters[which(names(parameters) == "specCapRet")]
        parameters=parameters[-which(names(parameters) == "specCapRet")] #remove paramerters from list
      }
      
      if( "emissivitySnowMax" %in% names(parameters) ){
        file_content[which(grepl(pattern = "emissivitySnowMax", x=file_content[,1])),2] = parameters[which(names(parameters) == "emissivitySnowMax")]
        parameters=parameters[-which(names(parameters) == "emissivitySnowMax")] #remove paramerters from list
      }
      
      if( "emissivitySnowMin" %in% names(parameters) ){
        file_content[which(grepl(pattern = "emissivitySnowMin", x=file_content[,1])),2] = parameters[which(names(parameters) == "emissivitySnowMin")]
        parameters=parameters[-which(names(parameters) == "emissivitySnowMin")] #remove paramerters from list
      }
      
      if( "a0" %in% names(parameters) ){
        file_content[which(grepl(pattern = "a0", x=file_content[,1])),2] = parameters[which(names(parameters) == "a0")]
        parameters=parameters[-which(names(parameters) == "a0")] #remove paramerters from list
      }
      
      if( "a1" %in% names(parameters) ){
        file_content[which(grepl(pattern = "a1", x=file_content[,1])),2] = parameters[which(names(parameters) == "a1")]
        parameters=parameters[-which(names(parameters) == "a1")] #remove paramerters from list
      }
      
      content=paste("#WASA-control file for snow routines;",sep="\t")
      write(content, file = paste(target_file,".ctl",sep=""))     #write header
      write.table(file_content, file = paste(target_file,".ctl",sep=""), append = TRUE, quote = F,row.names=F,col.names=F,sep="\t")
      no_params=NCOL(parameters)
      next
    }
    
#params in reservoir.dat ####
    reservoir_param_names=c("maxlevel", "damflow", "damqrac", "withdrawal", "damdead", "damalert", "dama", "damb", "qoutlet", "damc", "damd")
    if ( sub(names(parameters)[1], pattern ="_f$*", repl="") %in% reservoir_param_names ) 
    {
      target_file=paste(wasa_input_dir,"Reservoir/reservoir",sep="") #file holding parameters to be changed
      
      if (!file.exists(paste0(target_file, ".calib_bak")))   #create backup if not existing
        file.copy(paste0(target_file,".dat"), paste0(target_file,".calib_bak"))
      
      if (!file.exists(paste(target_file,".calib_bak",sep=""))) 
        stop(paste(target_file,".calib_bak or .dat not found.",sep=""))
      
      file_header  = scan(paste(target_file,".calib_bak",sep=""),what="character",sep="\n",n=2,strip.white=T,quiet=T)
      file_content = read.table(paste(target_file,".calib_bak",sep=""), skip=2,row.names=NULL, header = FALSE, sep = "\t", dec = ".")
      #get column names
      file_cols = strsplit(x = file_header[2], split = c(",","\t"))[[1]] 
      file_cols = sub(file_cols, pattern = " *", repl="") #remove spaces
      file_cols = sub(file_cols, pattern = "[\\[\\(].*", repl="") #remove parts in braces
      
      #modify reservoir parameters by specified factor or use value directly
      while (any(sub(names(parameters),pattern ="_f$*", repl="") %in% reservoir_param_names))
      {
        cur_param=as.vector(na.omit(match(reservoir_param_names, sub(names(parameters),pattern ="_f$*", repl="")))[1])            #find the next river parameter in 'parameters'
        param_col=which(file_cols==sub(pattern="_f$", repl="", names(parameters)[cur_param]))  #find the corresponding column to current parameter in 'reservoir_param_names' and reservoir.dat 
        if (grepl(pattern = "_f$", x = names(parameters)[cur_param]))
          file_content[,param_col]=file_content[,param_col]*parameters[1,cur_param] else  #use as factor modifying the current value
            file_content[,param_col]=                         parameters[1,cur_param]   #use directly
        parameters=parameters[-cur_param] #remove this parameter from list
        no_params=no_params-1 
      }
      write(file_header, file = paste(target_file,".dat",sep=""))     #write header
      write.table(file_content, file = paste(target_file,".dat",sep=""), append = TRUE, quote = F,row.names=F,col.names=F,sep="\t")
      next
    } 
    else
    {
      stop(paste("Unknown parameter",names(parameters)[1]))
      parameters=parameters[-1] #remove this parameter from list
      no_params=no_params-1
    }
    
    
  } #end loop thru all parameter/value pairs
  
}
