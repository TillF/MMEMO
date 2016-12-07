#########visualization of improvement values
## to be run after collect_performance_measures.R

base_dir="./runs2/"
saveplots=TRUE

rescale_ip_values = FALSE #rescale IP-values so their IQR falls within -1...1




library(xlsx)
library(berryFunctions)
#library(grid)
library(magic)

configs_dirs      = read.xlsx(file = paste0(base_dir, "comparison.xlsx"), sheetName = "configuration", startRow = 3, endRow = 3, header = FALSE, stringsAsFactors=FALSE)
configs           = read.xlsx(file = paste0(base_dir, "comparison.xlsx"), sheetName = "configuration", startRow = 2, endRow = 23,header = TRUE , stringsAsFactors=FALSE, colIndex = 1:11, check.names=FALSE)
enhancement_names = read.xlsx(file = paste0(base_dir, "comparison.xlsx"), sheetName = "configuration", startRow = 2, endRow = 2, header = FALSE,  stringsAsFactors=FALSE, colIndex = 2:11)


parameterizations_header = read.xlsx(file = paste0(base_dir, "comparison.xlsx"), sheetName = "parameterization", startRow = 1, endRow = 3, header = FALSE, stringsAsFactors=FALSE)
parameterizations        = read.xlsx(file = paste0(base_dir, "comparison.xlsx"), sheetName = "parameterization", startRow = 4,             header = TRUE,  stringsAsFactors=FALSE)

m_subcatchment_outlet = matrix(c(rep("sub", 8), rep("out",8))  , ncol=4)
#m_target_var      = matrix(rep(c(rep("wat", 2), rep("sed",2)), 4), ncol=4)
#m_day_hour            = matrix(rep(c(rep(24, 2), rep(1,2)), 4), ncol=4)
m_A_B            = matrix(rep(c(rep("A", 2), rep("B",2)), 4), ncol=4)

m_dynamics_yield      = matrix(rep(c("dyn","yil"), 8)  , ncol=4, byrow = TRUE)
m_uncal_calibrated    = matrix(rep(c("u","c"), 8)  , ncol=4)




#cpalette = colorRampPalette(c("red", "yellow", "blue")) #colorpalette
cpalette = colorRampPalette(c("red", "yellow", "green")) #colorpalette
# cpalette_red_yellow = colorRampPalette(c("red", "yellow"))
#  cpalette_yellow_green = colorRampPalette(c("yellow", "green")) #colorpalette

berry_pal = divPal(n = 16, reverse = FALSE, alpha = 1, extr = FALSE, yb = FALSE,
                   yr = FALSE, colors = NULL)                              




#plot legends
  windows(width = 8*2/3, height = 6)
  
  font_size = 3
  #layout(matrix(1:6, nrow = 2))
  par(mfrow=c(nr=2, nc=2), #divide into subplots
      oma=c(0,0.7,1.3,0), mar=c(0,1.3,1.3,0))                                         #set outer margin at top for title   
  
  image(col = c("grey","black"), z = arev(t(m_subcatchment_outlet=="out"),c(FALSE, TRUE))     , axes=FALSE)
  grid(nx = 4, ny = 4)
  text(x = 0.2+c(0, 0.6), y=0.5, labels=c("sub-\nbasin", "outlet"), col = c("black", "white"), cex = font_size)
  
  #   image(col = c("grey","black"), z = arev(t(m_day_hour==1),c(FALSE, TRUE))     , axes=FALSE)
  #   grid(nx = 4, ny = 4)
  #   text(y = 0.2+c(0.6, 0), x=0.5, labels=c("day", "hour"), col = c("black", "white"), cex = font_size)
  
  image(col = c("grey","black"), z = arev(t(m_A_B=="A"),c(FALSE, TRUE))     , axes=FALSE)
  grid(nx = 4, ny = 4)
  text(y = 0.2+c(0.6, 0), x=0.5, labels=c("ME included \n(A)", "ME withheld \n(B)"), col = c("white", "black"), cex = font_size)
  
 
  image(col = c("grey","black"), z = arev(t(m_dynamics_yield=="yil"),c(FALSE, TRUE))     , axes=FALSE)
  grid(nx = 4, ny = 4)
  text(srt=90, x = 0.0+c(0.015, 0.35), y=0.5, labels=c("dynamics", "yield"), col = c("black", "white"), cex = font_size)
  
  image(col = c("grey","black"), z = arev(t(m_uncal_calibrated=="c"),c(FALSE, TRUE))     , axes=FALSE)
  grid(nx = 4, ny = 4)
  text(y = 0.3+c(0.015, 0.35), x=0.5, labels=c("uncalibrated", "calibrated"), col = c("black", "white"), cex = font_size)
  
  
  if (saveplots) savePlot(filename = paste0(base_dir,"plots/ip_matrix/legend.wmf"), type = "wmf")
  if (saveplots) savePlot(filename = paste0(base_dir,"plots/ip_matrix/legend.png"), type = "png")

#colour bar
  windows(width = 2, height = 6)
  par(  oma=c(0,0.7,1.3,0), mar=c(1,0,1.3,0))       
  font_size = 2.5
 
  image(col = berry_pal, z = arev(t(cbind(15:1, NA)),c(FALSE, TRUE)), axes=FALSE)
  mtext(side = 3, outer = FALSE, adj = 0., padj=0, text=I [P]~"-value", cex = font_size)
  
  
  y = 0.0+c(0.95, 0.5, 0.05)
  x= rep(-0.4, 3)
  
  shadowtext <- function(x, y=NULL, labels, col='white', bg='black', #create text with halo / shadow
                         
                         theta= seq(pi/4, 2*pi, length.out=8), r=0.1, ... ) {
    xy <- xy.coords(x,y)
    xo <- r*strwidth('A')
    yo <- r*strheight('A')
    for (i in theta) {
      
      text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, 
            
            labels, col=bg, ... )
      
    }
    
    text(xy$x, xy$y, labels, col=col, ... )
    
  }
  
  #shadowtext(adj = c(0,0.5), y = y, x=x, labels=c("improved", "no \nchange", "degraded"), cex = font_size, font=3, col = "black", bg = "white")
  shadowtext(adj = c(0,0.5), y = y, x=x, labels=c("improved", "no \nchange", "degraded"), cex = font_size, font=3, col = "black", bg = "#ffffff")
  
  #text(adj = c(0,0.5), y = y, x=x, labels=c("improved", "no \nchange", "degraded"), cex = font_size)

  #text with semitransparent background box
  #textField(adj = c(0,0.5), y = y, x=x, labels=c("improved", "no \nchange", "degraded"), cex = font_size, fill="#FFFFFF66")
  if (saveplots) savePlot(filename = paste0(base_dir,"plots/ip_matrix/legend_colorbar.wmf"), type = "wmf")
  if (saveplots) savePlot(filename = paste0(base_dir,"plots/ip_matrix/legend_colorbar.png"), type = "png")
  
  

#plot improvement matrices for each ME

  #enhancement_names = setdiff(names(configs), c("configuration.ID","X","X.1")) #names of model enhancements
  
  
  #for testing: set matrics for unavailable benchmark parameterisations
  #   benchmark_params = parameterizations$parameterization_ID %in% parameterizations$reference
  #   for (metric in metric_cols )
  #   parameterizations [benchmark_params & is.na(parameterizations[,metric]), metric] = mean(parameterizations[,metric], na.rm=TRUE)
  
  ##rescale metrics to mean 0 and std 1
  #  parameterizations [, metric_cols] = scale(parameterizations [, metric_cols])
  
  
  #select (sub-)set for plotting
  model_settings = c("A", "B")
  #model_settings = c("A")
  resolutions=c(1,24)
  #resolutions=c(24)
  target_vars=c("wat", "sed")
  #target_vars=c("wat")
  
  collected_ip_matrices=array(NA, c(length(target_vars)*4, length(resolutions)*4, length(enhancement_names))) #collect all IP-matrices for later rescaling
  
  if (rescale_ip_values)
  {  
    load("collected_ip_matrices.RData")
    
    rescaling_factors = 1/
      apply(collected_ip_matrices, MARGIN = 1:2, FUN = IQR, na.rm=TRUE)
  }
  
  windows(width = length(resolutions)*4, height = length(target_vars)*4.5)
  
  for (enhancement in enhancement_names)
  {  
    #windows()
    
    #layout(matrix(1:(length(target_vars)*length(resolutions)), ncol = length(resolutions), byrow=TRUE))
    #layout.show()
    par(mfrow=c(nr=length(target_vars), nc=length(resolutions)), #divide into subplots
        oma=c(0,0.7,2,0), mar=c(0,1.3,2,0))                                         #set outer margin at top for title   
    
    for (target_var in target_vars) #loop filling rows in layout
    {
      for (resolution in resolutions) #loop filling columns in layout
      {
        improvement_matrix  = array(NA, c(4,4))
        
        #loops assembling rows of 4x4 matrix
        for(uncal_calibrated    in unique(	as.vector(m_uncal_calibrated  	)))
          for(subcatchment_outlet  in unique(	as.vector(m_subcatchment_outlet 	)))
            for(model_setting          in unique(	as.vector(model_settings      	)))
              for(dynamics_yield    	in unique(	as.vector(m_dynamics_yield      	)))
              {  
                enhancement_present = ifelse(model_setting=="A",1,0) #if A+, select row where enhancement was added, for B- select row where it was removed
                curr_configuration = configs$`configuration-ID`[
                  grepl(configs$`configuration-ID`, pattern = model_setting) &
                    configs[,enhancement] == enhancement_present
                  ]  #determine configuration-ID
                
                #treat special cases calibration and resolution
                if (enhancement == "calibration")
                  curr_configuration = curr_configuration[grepl(curr_configuration, pattern = "+8$")]
                if (enhancement == "resolution")
                  curr_configuration = curr_configuration[grepl(curr_configuration, pattern = "+9$")]
                if (!(enhancement %in% c("resolution", "calibration")))
                  curr_configuration = curr_configuration [1]
                
                
                #get row of current parameterization
                curr_parameterization_row = which(
                  parameterizations$config_ID == curr_configuration &
                    parameterizations$resolution == resolution         &
                    parameterizations$calibrated..yes.no. == uncal_calibrated &
                    grepl(parameterizations$config_ID, pattern = model_setting)
                )  
                
                if (length(curr_parameterization_row)==0) next
                if (length(curr_parameterization_row)>1) browser()
                
                improvement_value = parameterizations[curr_parameterization_row, paste0("I_P_", subcatchment_outlet,"_",target_var,"_", dynamics_yield)]
                
                array_index= #determine position in matrix where to write improvement value
                  subcatchment_outlet ==  m_subcatchment_outlet &
                  model_setting       == m_A_B &
                  dynamics_yield      == m_dynamics_yield &
                  uncal_calibrated    == m_uncal_calibrated 
                
                improvement_matrix[array_index] = improvement_value
                
              } 
        
        collected_ip_matrices[
          (which(target_var == target_vars)-1) *4 + 1:4,
          (which(resolution == resolutions)-1) *4 + 1:4, 
          which(enhancement == enhancement_names)
            ] = improvement_matrix #store for later use (i.e. rescaling of IP-values)
          
        
        plot(1, axes=FALSE, type="n", xlab="", ylab="") #dummy plot
        
        
        if (target_var==target_vars[1]) #first row in plot
          mtext(text=paste(resolution, "h"), side=3, outer=FALSE, cex=font_size) #write resolution
        if (resolution==resolutions[1]) #first col in plot
          mtext(text = ifelse(target_var=="wat","water","sediment"), side=2, outer=FALSE, cex=font_size) #write target variable
        
        if (all(is.na(improvement_matrix))) next    
        
        if (rescale_ip_values) #rescale IP-values for more consistent display
          improvement_matrix = improvement_matrix * 
                rescaling_factors[(which(target_var == target_vars)-1) *4 + 1:4,
            (which(resolution == resolutions)-1) *4 + 1:4 
            ] 
          
        par(new=TRUE)      
        #plot improvement matrix
        t_var = ifelse(target_var=="wat", "water", "sediment")
        #trans_improvement_matrix =  sign(improvement_matrix) * abs(improvement_matrix)^(1/3)
        
        image(col = berry_pal, z = arev(t(improvement_matrix),c(FALSE, TRUE)), axes=FALSE, 
              #            main=plot_title,
              zlim = c(-1,1)*max(abs(improvement_matrix), na.rm=TRUE) )
        grid(nx = 4, ny = 4)
        
        #label extremes
        
        max_val = max(improvement_matrix, na.rm=TRUE)  
        max_ix = which(improvement_matrix == max_val, arr.ind=TRUE)
        max_ix  = (max_ix-1) / (dim(improvement_matrix)-1)
        text(adj = c(0.5,0.5), x=max_ix[1,2], y=1-max_ix[1,1], labels=format(round(max_val, digits=2), digits = 2), cex = font_size)
        
        min_val = 	min(improvement_matrix, na.rm=TRUE)  
        min_ix = which(improvement_matrix == 	min_val, arr.ind=TRUE)
        min_ix  = (	min_ix-1) / (dim(improvement_matrix)-1)
        text(adj = c(0.5,0.5), x=	min_ix[1,2], y=1-	min_ix[1,1], labels=format(round(min_val, digits=2), digits = 2), cex = font_size)
      }
      
      
    }  
   
    mtext(text=paste0("ME", which(enhancement==enhancement_names),": ", enhancement), side=3, outer=TRUE, cex=font_size) #write window title
    if (saveplots) 
    {
      enhancement=sub(enhancement,pattern = "/", repl="_")
      savePlot(file=paste0(base_dir,"plots/ip_matrix/",enhancement,ifelse(rescale_ip_values,"_rescaled",""),".wmf"), type = "wmf" ) #flawed in RStudio
      savePlot(file=paste0(base_dir,"plots/ip_matrix/",enhancement,ifelse(rescale_ip_values,"_rescaled",""),".png"), type = "png")
    }  
    save(list = "collected_ip_matrices", file="collected_ip_matrices.RData")
  }



##compute range in improvement values for each enhancement (sigma)
  #relate respective A+ and B- runs
  col_index_ip_values = which(grepl(names(parameterizations), pattern = "^I_P"))
  aux4aggr=sub(x = parameterizations$parameterization_ID, pattern = "B", repl="A")
  aux4aggr=sub(x = aux4aggr, pattern = "-", repl="+")
  #special case: resolution
  res_params = grepl(parameterizations$parameterization_ID, pattern = "+9") #these are the parameterizations dealing with resolution
  res_aggr = sub(x = parameterizations$parameterization_ID[res_params], pattern = "B", repl="A")
  
  rel_range = function(x){ #returns relative range of a vector
    mean_x = mean(x)
    if (mean_x==0) mean_x=1 #avoid division by 0
    return(abs(diff(range(x)/mean_x)))
  } 
  rel_range_I_P = aggregate(x = parameterizations[, col_index_ip_values], by = list(parameterization_ID=aux4aggr), FUN = rel_range)
 
  #aggregate by ME and resolution
    tt=matrix(as.matrix(rel_range_I_P[,-1]), ncol=1)
    aux4aggr= rep(sub(x = rel_range_I_P$parameterization_ID, pattern = "(.).(\\d)*_._(\\d+)", repl="\\2_\\3"),8)
    med_rel_range_I_P = aggregate(x = tt, by = list(parameterization_ID=aux4aggr), FUN = median)
    med_rel_range_I_P$ME =as.numeric(sub(med_rel_range_I_P$parameterization_ID, pattern = "(^\\d).*" , repl="\\1"))
    med_rel_range_I_P = na.omit(med_rel_range_I_P)
    med_rel_range_I_P$res=sub(med_rel_range_I_P$parameterization_ID, pattern = "^\\d_(\\d*)" , repl="\\1")
    me_s=sort(unique(med_rel_range_I_P$ME))
    res_tab = merge(
      med_rel_range_I_P[med_rel_range_I_P$res==1 ,],
      med_rel_range_I_P[med_rel_range_I_P$res==24,],
      by="ME", all.x = TRUE
    )[,c("ME","V1.x", "V1.y")]
    round(res_tab, digits=2 )
    apply(res_tab, MARGIN = 1, mean)      
  
  windows(width = length(resolutions)*4, height = length(target_vars)*4.5)
  
  for (enhancement in enhancement_names)
  {  
    
    par(mfrow=c(nr=length(target_vars), nc=length(resolutions)), #divide into subplots
        oma=c(0,0.7,1.3,0), mar=c(0,1.3,1.3,0))                                         #set outer margin at top for title   
    
    for (target_var in target_vars) #loop filling rows in layout
    {
      for (resolution in resolutions) #loop filling columns in layout
      {
        improvement_matrix  = array(NA, c(4,4))
        
        #loops assembling rows of 4x4 matrix
        for(uncal_calibrated    in unique(	as.vector(m_uncal_calibrated  	)))
          for(subcatchment_outlet  in unique(	as.vector(m_subcatchment_outlet 	)))
            for(model_setting          in unique(	as.vector(model_settings      	)))
              for(dynamics_yield    	in unique(	as.vector(m_dynamics_yield      	)))
              {  
                enhancement_present = ifelse(model_setting=="A",1,0) #if A+, select row where enhancement was added, for B- select row where it was removed
                curr_configuration = configs$`configuration-ID`[
                  grepl(configs$`configuration-ID`, pattern = model_setting) &
                    configs[,enhancement] == enhancement_present
                  ]  #determine configuration-ID
                
                #treat special cases calibration and resolution
                if (enhancement == "calibration")
                  curr_configuration = curr_configuration[grepl(curr_configuration, pattern = "+8$")]
                if (enhancement == "resolution")
                  curr_configuration = curr_configuration[grepl(curr_configuration, pattern = "+9$")]
                if (!(enhancement %in% c("resolution", "calibration")))
                  curr_configuration = curr_configuration [1]
                
                
                #get row of current parameterization
                parameterization_ID = paste0(curr_configuration, "_", uncal_calibrated, "_", resolution)
                curr_parameterization_row = which(rel_range_I_P$parameterization_ID == parameterization_ID)
                
                if (length(curr_parameterization_row)==0) next
                if (length(curr_parameterization_row)>1) browser()
                
                improvement_value = rel_range_I_P[curr_parameterization_row, paste0("I_P_", subcatchment_outlet,"_",target_var,"_", dynamics_yield)]
                
                array_index= #determine position in matrix where to write improvement value
                  subcatchment_outlet ==  m_subcatchment_outlet &
                  model_setting       == m_A_B &
                  dynamics_yield      == m_dynamics_yield &
                  uncal_calibrated    == m_uncal_calibrated 
                
                improvement_matrix[array_index] = improvement_value
                
              } 
        
        plot(1, axes=FALSE, type="n", xlab="", ylab="") #dummy plot
        
        
        if (target_var==target_vars[1]) #first row in plot
          mtext(text=paste(resolution, "h"), side=3, outer=FALSE, cex=1.5) #write window title
        if (resolution==resolutions[1]) #first col in plot
          mtext(text = ifelse(target_var=="wat","water","sediment"), side=2, outer=FALSE, cex=1.5) #write window title
        
        if (all(is.na(improvement_matrix))) next    
        
        
        par(new=TRUE)      
        #plot improvement matrix
        t_var = ifelse(target_var=="wat", "water", "sediment")
        
        image(col = berry_pal, z = arev(t(improvement_matrix),c(FALSE, TRUE)), axes=FALSE, 
              #            main=plot_title,
              zlim = c(-1,1)*max(abs(improvement_matrix), na.rm=TRUE) )
        grid(nx = 4, ny = 4)
        
        #label extremes
        max_val = max(improvement_matrix, na.rm=TRUE)  
        max_ix = which(improvement_matrix == max_val, arr.ind=TRUE)
        max_ix  = (max_ix-1) / (dim(improvement_matrix)-1)
        text(adj = c(0.5,0.5), x=max_ix[1,2], y=1-max_ix[1,1], labels=format(max_val, digits = 2), cex = 2)
        
        min_val = 	min(improvement_matrix, na.rm=TRUE)  
        min_ix = which(improvement_matrix == 	min_val, arr.ind=TRUE)
        min_ix  = (	min_ix-1) / (dim(improvement_matrix)-1)
        text(adj = c(0.5,0.5), x=	min_ix[1,2], y=1-	min_ix[1,1], labels=format(	min_val, digits = 2), cex = 2)
      }
    }  
    mtext(text=enhancement, side=3, outer=TRUE, cex=1.5) #write window title
    if (saveplots) 
    {
      enhancement=sub(enhancement,pattern = "/", repl="_")
      savePlot(file=paste0(base_dir,"plots/ip_range/",enhancement,"_IP_range",".wmf"), type = "wmf" ) #flawed in RStudio
      savePlot(file=paste0(base_dir,"plots/ip_range/",enhancement,"_IP_range",".png"), type = "png")
    }  
    
  }
  
  


#analysis
  enhancements_list = sub(pattern = "..(\\d)*_.*", x = parameterizations$parameterization_ID, repl="\\1")
  p2                = parameterizations[!(enhancements_list %in% c("A","B")),]  #discard reference parametrisations, as there are no improvement values for them
  enhancements_list = enhancements_list[!(enhancements_list %in% c("A","B"))]
  
  col_index_ip_values = which(grepl(names(p2), pattern = "^I_P"))
  mean_improvements=aggregate(x = p2[, col_index_ip_values], by = list(me=enhancements_list), FUN = mean, na.rm=TRUE) 
  
  #rank (aggregated) improvement aspects for each ME
  ranks_ME=t(apply(as.matrix(-mean_improvements[, -1]), MARGIN = 1, FUN = rank))
  
  #rank ME for each (aggregated) improvement aspect
  ranks_aspect=t(apply(as.matrix(-mean_improvements[, -1]), MARGIN = 2, FUN = rank))
  
  #total_mean_improvements=apply(X = mean_improvements[,-1], MARGIN = 1, FUN = mean, na.rm=TRUE) 
  

#aggregate improvement values by ME - overview
  
  #improvements_all = list()
  improvements_all = NULL
  
  for (me in unique(enhancements_list))
    for (ab in model_settings)
    {  
      sel_rows = which(enhancements_list==me &  substr(p2$config_ID, start = 1, stop=1)==ab)
      sel_rows = sel_rows[1:4] #force vector length of 4 (can be less for enhancements 8 and 9)
      improvements_all = cbind(improvements_all,
                               matrix(as.matrix(p2[sel_rows ,  col_index_ip_values ] ), ncol=1))
    }  

  windows(width = 6, height = 5)
  par(mar=c(8,5,2,2))
  boxplot(improvements_all, ylim=quantile(improvements_all, probs = c(0.05,0.95), na.rm=TRUE), col=c("white","grey"), axes=FALSE, ylab=I [P]~"[-]")
#  boxplot(improvements_all, ylim=quantile(improvements_all, probs = c(0.2,0.8), na.rm=TRUE), col=c("white","grey"), axes=FALSE, ylab=I [P]~"[-]")
  #  , xlim=c(0, length(enhancement_names)*2+.5)
  #which(diff(apply(improvements_all, MARGIN = 2, FUN = median, na.rm=TRUE))[(1:9)*2-1]<0) #find MEs where the median is better for A than for B
  #sum(apply(improvements_all>0, MARGIN = 2, FUN = sum, na.rm=TRUE))/sum(!is.na(improvements_all)) #percentage of parameterizations with positive IP-value

  
  abline(v=(1:(length(enhancement_names)-1)*2+.5))
  abline(h=0, lty="dashed")
  enhancement_names2 =
    sub(enhancement_names, pattern = "(factor |LAI/C |tivity |flow )", repl="\\1\n")
  axis(side=2)
  par(lheight=.7)
  axis(mgp=c(3,2,0), hadj = 1, side=1, labels = c(NA, enhancement_names2, NA), at = (0:(length(enhancement_names)+1))*2-.5, las=2)
  mtext(side=1, line = 0.8, text = paste0("ME",1:9), at = (1:(length(enhancement_names)))*2-.5, las=1)
  
  perc_improved=apply(improvements_all>0, MARGIN = 2, FUN = sum, na.rm=TRUE)/(apply(improvements_all, MAR=2, FUN = function(x){sum(!is.na(x), na.rm=TRUE)})) #comute percentage of parameterizations that have positive improvement
  #mean(perc_improved) #mean percentagte of improved parameterization
  #mean(perc_improved[(1:9)*2])
  #mean(perc_improved[(1:9)*2-1])
  
  mtext(side=3, text = format(perc_improved*100, digits = 2), at = 1:(length(enhancement_names)*2), las=2)
  mtext(side=3, text = I [P]~"> 0 [%]:", at = -1.5, las=1)
  axis(side=3, tick = TRUE, lwd.ticks = 0,  at = (-1:length(enhancement_names))*2-.5, labels=NA)
    
  
  
  legend("topright", legend = c("A","B"), fill =  c("white","grey"))
  if (saveplots) 
  {
    savePlot(file=paste0(base_dir,"plots/","A_vs_B",".wmf"), type = "wmf" ) 
    savePlot(file=paste0(base_dir,"plots/","A_vs_B",".png"), type = "png")
  }      


#plot ranks of improvement aspects for each ME
  windows(width = 5, height = 2.5)
  par(#mfrow=c(nr=1, nc=2), #divide into subplots
    oma=c(0,0,0,0), mar=c(0.1,10,0.1,0))                                         #set outer margin at top for title   
  
  ip_labels=colnames(ranks_ME) #formatted version of improvement value names
  ip_labels=sub(ip_labels, pattern = "I_P_", repl="")
  ip_labels=sub(ip_labels, pattern = "sub_", repl="subbasin ")
  ip_labels=sub(ip_labels, pattern = "out_", repl="outlet " )
  ip_labels=sub(ip_labels, pattern = "wat_", repl="water ")
  ip_labels=sub(ip_labels, pattern = "sed_", repl="sed. " )
  ip_labels=sub(ip_labels, pattern = "dyn", repl="dynamics ")
  ip_labels=sub(ip_labels, pattern = "yil", repl="yield " )
  
  #assemble palette
  for (i in 1:2)
  {
    if (i==1)
      matrix2plot = ranks_ME else
        matrix2plot = ranks_aspect
      
      ncol2plot=ncol(matrix2plot)
      nrow2plot=nrow(matrix2plot)
      pal_blue2white= colorRampPalette(c("blue", "white")) #colorpalette
      pal_white2red = colorRampPalette(c("white", "red"))  #colorpalette
      cpalette=c(pal_blue2white(4)[-4],rep("#FFFFFF",ncol2plot-6),pal_white2red(4)[-1])
      
      image(col = cpalette, z = arev(t(matrix2plot),c(FALSE, TRUE)), axes=FALSE, 
            # main="effect of improvements",
            zlim = range(matrix2plot, na.rm=TRUE) )
      if (i==1)
        rowlabels = paste0("ME",1:length(enhancement_names)," ", enhancement_names) else
          rowlabels = ip_labels
      
      mtext(side=2, at = 0.0+1/(nrow2plot-1)*(0:(nrow2plot-1)), adj = 1, text=rev(rowlabels), cex = 1, las=1, line=0.2)
      #text(adj = c(1, 0.5), y = 0.0+1/(nrow2plot-1)*(0:(nrow2plot-1)), x=0.10, labels=rev(enhancements), cex = 1)
      grid(nx = 1, ny = nrow2plot, lty=1, lwd = 3, col = "white")  
      
      if (i==1)
        collabels = ip_labels else
          collabels = paste0("ME",1:length(enhancement_names)," ", enhancement_names)
      
      text(adj = c(0.5, 0.5), x = 0.0+1/(ncol2plot-1)*(0:(ncol2plot-1)), y=.50, labels=collabels, cex = 1, srt=90)
      
      if (saveplots) 
      {
        plotname = ifelse(i==1, "ranks4ME","ranks4IP")
        savePlot(file=paste0(base_dir,"plots/ranks/",plotname,".wmf"), type = "wmf" ) #flawed in RStudio
        savePlot(file=paste0(base_dir,"plots/ranks/",plotname,".png"), type = "png")
      }
  }  
  
  #plot legend
  windows(width = 1, height = 2.5)
  par(oma=c(0,0,0,0), mar=c(0.1,0.1,0.1,0.1))                                         #set outer margin at top for title     
  
  image(col = cpalette, z = arev(t(as.matrix(1:length(cpalette)))), axes=FALSE)
  text(adj = c(0.5,0.5), y = 0.0+1/(ncol2plot-1)*(0:2), x=-0.10, labels=paste0("rank ",-(1:3)), cex = 1.)
  text(adj = c(0.5,0.5), y = 0.0+1/(ncol2plot-1)*ncol2plot %/% 2, x=-0.10, labels="[other]", cex = 1.)
  text(adj = c(0.5,0.5), y = 0.0+1/(ncol2plot-1)*(-(3:1)+ncol2plot), x=-0.10, labels=paste0("rank ",3:1), cex = 1.)
  if (saveplots) 
  {
    savePlot(file=paste0(base_dir,"plots/ranks/","legend_ranks",".wmf"), type = "wmf" ) #flawed in RStudio
    savePlot(file=paste0(base_dir,"plots/ranks/","legend_ranks",".png"), type = "png")
  }



  
#plot best two MEs for each parameterisation
  #me_colors=c("cyan","blue", "darkgreen", "lightgreen", "orange", "grey", "magenta")
  me_colors=c("#a6cee3","#1f78b4","#fdbf6f","#b2df8a","#33a02c","#fb9a99","#e31a1c") #colorbrewer palette
  
  #omit +8 and +9 (resolution and calibration)
  parameterizations = parameterizations[!grepl(parameterizations$parameterization_ID, pattern = "(^.+8)|(^.+9)"),]
  enhancement_names = setdiff(enhancement_names, c("resolution", "calibration"))
  
  windows(width = length(resolutions)*4, height = length(target_vars)*4.5)
  par(mfrow=c(nr=length(target_vars), nc=length(resolutions)), #divide into subplots
      oma=c(0,0.7,1.3,0), mar=c(0,1.3,1.3,0))                                         #set outer margin at top for title   
  
  
  for (target_var in target_vars) #loop filling rows in layout
  {
    for (resolution in resolutions) #loop filling columns in layout
    {
      bestME_matrix  = array(NA, c(4,4,2)) #matrix for collecting the best and second best ME for each parameterisation
      
      #loops assembling rows of 4x4 matrix
      for(uncal_calibrated    in unique(	as.vector(m_uncal_calibrated  	)))
        for(subcatchment_outlet  in unique(	as.vector(m_subcatchment_outlet 	)))
          for(model_setting          in unique(	as.vector(model_settings      	)))
            for(dynamics_yield    	in unique(	as.vector(m_dynamics_yield      	)))
            {  
              enhancement_present = ifelse(model_setting=="A",1,0) #if A+, select row where enhancement was added, for B- select row where it was removed                
              
              curr_configurations = configs$`configuration-ID`[
                grepl(configs$`configuration-ID`, pattern = paste0(model_setting,"."))
                ]  #determine configuration-IDs
              
              #browser()
              #get row of current parameterization
              curr_parameterization_rows = which(
                parameterizations$config_ID %in% curr_configurations &
                  parameterizations$resolution == resolution         &
                  parameterizations$calibrated..yes.no. == uncal_calibrated &
                  grepl(parameterizations$config_ID, pattern = model_setting)
              )  
              
              i_p_column=paste0("I_P_", subcatchment_outlet,"_",target_var,"_", dynamics_yield) #assemble name of current improvement value
              
              i_p_values = parameterizations[curr_parameterization_rows, i_p_column] #extract I_P-values
              
              enhancements=sub(pattern = "..(\\d)*_.*", x = parameterizations$parameterization_ID[curr_parameterization_rows], repl="\\1")
              i_p_values = i_p_values[sort.int(enhancements, index.return = TRUE)$ix]     #sort i_p-values by enhancement number (in case it isnt)
              
              #rank MEs of this parameterisation 
              ranks_ME=sort.int(i_p_values, decreasing = TRUE, index.return = TRUE)$ix #get ordered list of MEs according to their IP-value
              if (length(ranks_ME)==0) ranks_ME=c(NA, NA) #catch NA-case
              ranks_ME[i_p_values[ranks_ME[1:2]]<=0]=NA  #if the improvement was negative, omit plotting              
              
              array_index= #determine position in matrix where to write improvement value
                subcatchment_outlet ==  m_subcatchment_outlet &
                model_setting       == m_A_B &
                dynamics_yield      == m_dynamics_yield &
                uncal_calibrated    == m_uncal_calibrated 
              
              bestME_matrix[,,1][array_index] = ranks_ME[1] #store first and second best
              bestME_matrix[,,2][array_index] = ranks_ME[2] #store first and second best
              
            } 
      
      plot(1, axes=FALSE, type="n", xlab="", ylab="") #dummy plot
      
      
      if (target_var==target_vars[1]) #first row in plot
        mtext(text=paste(resolution, "h"), side=3, outer=FALSE, cex=1.5) #write window title
      if (resolution==resolutions[1]) #first col in plot
        mtext(text = ifelse(target_var=="wat","water","sediment"), side=2, outer=FALSE, cex=1.5) #write window title
      
      if (all(is.na(bestME_matrix))) next    
      
      #plot best ME as image pixel
      par(new=TRUE)      
      improvement_matrix = bestME_matrix[,,1] #for re-using code: plot best ME now
      
      t_var = ifelse(target_var=="wat", "water", "sediment")
      
      image(col=me_colors, z = arev(t(improvement_matrix),c(FALSE, TRUE)), axes=FALSE,
            zlim = range(1:length(enhancements)))
      grid(nx = 4, ny = 4)
      
      #plot second-best ME as coloured dot
      improvement_matrix = bestME_matrix[,,2] #
      for (nc in 1:ncol(improvement_matrix))
        for (nr in 1:nrow(improvement_matrix))
          points((nc-1)/3, 1-(nr-1)/3, bg=me_colors[improvement_matrix[nr, nc]], pch=21, cex=8, col=0)
      
    }
  }  
  
  if (saveplots) 
  {
    savePlot(file=paste0(base_dir,"plots/ranks/","best_me",".wmf"), ) #flawed in RStudio
    savePlot(file=paste0(base_dir,"plots/ranks/","best_me",".png"), type = "png")
  }
  
  #plot legend
  windows(width = 1.5, height = 2.5)
  par(oma=c(0,0,0,0), mar=c(0.1,0.1,0.1,0.1))                                         #set outer margin at top for title     
  
  image(col = me_colors, z = arev(t(as.matrix(1:length(me_colors)))), axes=FALSE)
  text(adj = c(0.5,0.5), y = 0.0+(0:(length(me_colors)-1)/(length(me_colors)-1)), x=-0.0, labels=rev(enhancement_names), cex = 1.)
  if (saveplots) 
  {
    savePlot(file=paste0(base_dir,"plots/ranks/","legend_best_me",".wmf"), ) #flawed in RStudio
    savePlot(file=paste0(base_dir,"plots/ranks/","legend_best_me",".png"), type = "png")
  }



