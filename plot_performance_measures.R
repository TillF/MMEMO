#########visualization of improvement values
## to be run after collect_performance_measures.R

#----settings----
base_dir="./runs3/"
saveplots=TRUE

rescale_ip_values = FALSE #rescale IP-values so their IQR falls within -1...1

#windows = function(...) {x11(...)}
#### sadf

library(xlsx)
library(berryFunctions)
#library(grid)
library(magic)

#----load results from files----

configs_dirs      = read.xlsx(file = paste0(base_dir, "comparison.xlsx"), sheetName = "configuration", startRow = 3, endRow = 3, header = FALSE, stringsAsFactors=FALSE)
configs           = read.xlsx(file = paste0(base_dir, "comparison.xlsx"), sheetName = "configuration", startRow = 2, endRow = 23,header = TRUE , stringsAsFactors=FALSE, colIndex = 1:11, check.names=FALSE)
enhancement_names = read.xlsx(file = paste0(base_dir, "comparison.xlsx"), sheetName = "configuration", startRow = 2, endRow = 2, header = FALSE,  stringsAsFactors=FALSE, colIndex = 2:11)


parameterizations_header = read.xlsx(file = paste0(base_dir, "comparison.xlsx"), sheetName = "parameterization", startRow = 1, endRow = 3, header = FALSE, stringsAsFactors=FALSE)
parameterizations        = read.xlsx(file = paste0(base_dir, "comparison.xlsx"), sheetName = "parameterization", startRow = 4,             header = TRUE,  stringsAsFactors=FALSE)

m_subcatchment_outlet = matrix(c(rep("sub", 8), rep("out",8))  , ncol=4)
#m_target_var      = matrix(rep(c(rep("wat", 2), rep("sed",2)), 4), ncol=4)
m_day_hour            = matrix(rep(c(rep(24, 2), rep(1,2)), 4), ncol=4)
#m_A_B                  = matrix(rep(c(rep("A", 2), rep("B",2)), 4), ncol=4)

m_dynamics_yield      = matrix(rep(c("dyn","yil"), 8)  , ncol=4, byrow = TRUE)
m_uncal_calibrated    = matrix(rep(c("u","c"), 8)  , ncol=4)

#select (sub-)set for plotting
model_settings = c("A", "B")
#model_settings = c("A")
resolutions=c(1,24)
#resolutions=c(24)
target_vars=c("wat", "sed")
#target_vars=c("wat")



#---- collect performance of reference configurations, add to xlsx (Table 6 ) ----
{
  try(detach("package:xlsx"    , unload=TRUE, force = TRUE), silent = TRUE)
  try(detach("package:xlsxjars", unload=TRUE, force = TRUE), silent = TRUE)
  try(detach("package:XLConnect", unload=TRUE, force = TRUE), silent = TRUE)
  try(detach("package:XLConnectJars", unload=TRUE, force = TRUE), silent = TRUE)
  
  library(XLConnect)
  
  #prepare sheets for each ME, fill in values (using XLConnect because of better copy options)
  wb <- loadWorkbook(file = paste0(base_dir, "IP_matrices.xlsx"))  
  setStyleAction(wb, XLC$"STYLE_ACTION.NONE") #when writing data, do not touch cell styles
 
  sheetName="reference_p"

  performance_matrix      = array(NA,c(8,8))
  performance_matrix_part = array(NA,c(4,4))
  
  for (model_setting in model_settings) #loop filling rows in layout
    for (target_var in target_vars) #loop filling columns in layout
    {
            #loops assembling rows of 4x4 matrix
      for(uncal_calibrated    in unique(	as.vector(m_uncal_calibrated  	)))
        for(subcatchment_outlet  in unique(	as.vector(m_subcatchment_outlet 	)))
          for(resolution          in unique(	as.vector(resolutions      	)))
            for(dynamics_yield    	in unique(	as.vector(m_dynamics_yield      	)))
            {  
              curr_param = paste(model_setting,uncal_calibrated,resolution,sep="_") #determine configuration-ID
              
              #get row of current parameterization
              curr_parameterization_row = which(parameterizations$parameterization_ID == curr_param  )  
              
              p_col = paste0(subcatchment_outlet,"_",target_var,"_", dynamics_yield)
              performance_value = parameterizations[curr_parameterization_row, p_col]
              
              array_index= #determine position in matrix where to write performance value
                subcatchment_outlet ==  m_subcatchment_outlet &
                resolution       == m_day_hour &
                dynamics_yield      == m_dynamics_yield &
                uncal_calibrated    == m_uncal_calibrated 
              
              performance_matrix_part  [array_index] = performance_value

            } 
  
    #performance_matrix_part = round(performance_matrix_part, digits=0)
  
    performance_matrix[
    (which(model_setting == model_settings)-1) *4 + 1:4, 
    (which(target_var == target_vars)-1) *4 + 1:4
    ] = performance_matrix_part #assemble parts
    }
  
  writeWorksheet(object = wb, data = performance_matrix, sheet = sheetName, startRow = 4, startCol = 5, header = FALSE)

  saveWorkbook(wb, file = paste0(base_dir, "IP_matrices.xlsx"))
  detach("package:XLConnect", unload=TRUE, force=TRUE)
  detach("package:XLConnectJars", unload=TRUE, force=TRUE)
}


#---- compute range in improvement values for each enhancement (ASD) ----
{
  font_size = 2.5
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
  
  #aggregate by ME and resolution (Table 5)
  # tt=matrix(as.matrix(rel_range_I_P[,-1]), ncol=1) #write all IP-values into a single vector
  # aux4aggr= rep(sub(x = rel_range_I_P$parameterization_ID, pattern = "(.).(\\d)*_._(\\d+)", repl="\\2_\\3"),8) #create aggregation key from IDs
  # med_rel_range_I_P = aggregate(x = tt, by = list(parameterization_ID=aux4aggr), FUN = median)
  # med_rel_range_I_P$ME =as.numeric(sub(med_rel_range_I_P$parameterization_ID, pattern = "(^\\d).*" , repl="\\1"))
  # med_rel_range_I_P = na.omit(med_rel_range_I_P)
  # med_rel_range_I_P$res=sub(med_rel_range_I_P$parameterization_ID, pattern = "^\\d_(\\d*)" , repl="\\1")
  # me_s=sort(unique(med_rel_range_I_P$ME))
  # res_tab = merge(
  #   med_rel_range_I_P[med_rel_range_I_P$res==1 ,],
  #   med_rel_range_I_P[med_rel_range_I_P$res==24,],
  #   by="ME", all.x = TRUE
  # )[,c("ME","V1.x", "V1.y")]
  # round(res_tab, digits=2 )
  
  tt=matrix(as.matrix(rel_range_I_P[,-1]), ncol=1) #write all IP-values into a single vector
  aux4aggr= rep(sub(x = rel_range_I_P$parameterization_ID, pattern = "(.).(\\d)*_._(\\d+)", repl="\\2"),8) #create aggregation key from IDs
  med_rel_range_I_P = aggregate(x = tt, by = list(parameterization_ID=aux4aggr), FUN = median)
  med_rel_range_I_P$ME =as.numeric(sub(med_rel_range_I_P$parameterization_ID, pattern = "(^\\d).*" , repl="\\1"))
  med_rel_range_I_P = na.omit(med_rel_range_I_P)
  med_rel_range_I_P$res=sub(med_rel_range_I_P$parameterization_ID, pattern = "^\\d_(\\d*)" , repl="\\1")
  #round(med_rel_range_I_P$V1, digits=2 )
}  


#cpalette = colorRampPalette(c("red", "yellow", "green")) #colorpalette
berry_pal = divPal(n = 16, reverse = FALSE, alpha = 1, extr = FALSE, yb = FALSE,
                   yr = FALSE, colors = NULL)                              


#---- plot legends ----
{
  windows(width = 8*2/3, height = 6)
  
  font_size = 3
  #layout(matrix(1:6, nrow = 2))
  par(mfrow=c(nr=2, nc=2), #divide into subplots
      oma=c(0,0.7,1.3,0), mar=c(0,1.3,1.3,0))                                         #set outer margin at top for title   
  
  image(col = c("grey","black"), z = arev(t(m_subcatchment_outlet=="out"),c(FALSE, TRUE))     , axes=FALSE)
  grid(nx = 4, ny = 4)
  text(x = 0.2+c(0, 0.6), y=0.5, labels=c("sub-\nbasin", "outlet"), col = c("black", "white"), cex = font_size)
  
  #   image(col = c("grey","black"), z = arev(t(m_A_B_x==1),c(FALSE, TRUE))     , axes=FALSE)
  #   grid(nx = 4, ny = 4)
  #   text(y = 0.2+c(0.6, 0), x=0.5, labels=c("day", "hour"), col = c("black", "white"), cex = font_size)
  
  image(col = c("grey","black"), z = arev(t(m_day_hour==24),c(FALSE, TRUE))     , axes=FALSE)
  grid(nx = 4, ny = 4)
  text(y = 0.2+c(0.6, 0), x=0.5, labels=c("daily", "hourly"), col = c("white", "black"), cex = font_size)
  
 
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
}  
  
#---- load IP-precision data ----
load(paste0(base_dir, "repl/IP_precision.RData")) #load precision data (IP_precision) for IPs (generated with rerun_best.R)


#---- plot improvement matrices for each ME ----
{
  #enhancement_names = setdiff(names(configs), c("configuration.ID","X","X.1")) #names of model enhancements
  
  
  #for testing: set matrics for unavailable benchmark parameterisations
  #   benchmark_params = parameterizations$parameterization_ID %in% parameterizations$reference
  #   for (metric in metric_cols )
  #   parameterizations [benchmark_params & is.na(parameterizations[,metric]), metric] = mean(parameterizations[,metric], na.rm=TRUE)
  
  ##rescale metrics to mean 0 and std 1
  #  parameterizations [, metric_cols] = scale(parameterizations [, metric_cols])
  
  collected_reference_p  =array(NA, c(length(target_vars)*4, length(resolutions)*4)) #collect reference performance measures
  
  collected_ip_matrices  =array(NA, c(length(target_vars)*4, length(resolutions)*4, length(enhancement_names))) #collect all IP-matrices for later rescaling
  collected_sign_matrices=array(NA, c(length(target_vars)*4, length(resolutions)*4, length(enhancement_names))) #collect all IP-matrices for later rescaling
  
  if (rescale_ip_values)
  {  
    load(paste0(base_dir,"collected_ip_matrices.RData"))
    
    rescaling_factors = 1/
      apply(collected_ip_matrices, MARGIN = 1:2, FUN = IQR, na.rm=TRUE)
  }
  
  windows(width = length(resolutions)*4*1.1, height = length(target_vars)*4.5)
  
  for (enhancement in enhancement_names)
  {  
    #windows()
    
    #layout(matrix(1:(length(target_vars)*length(resolutions)), ncol = length(resolutions), byrow=TRUE))
    #layout.show()
    par(mfrow=c(nr=length(model_settings), nc=length(target_vars)), #divide into subplots
        oma=c(0,1,3,0), mar=c(0,1.3,2,0))                                         #set outer margin at top for title   
    
  for (model_setting in model_settings) #loop filling rows in layout
  {
    for (target_var in target_vars) #loop filling columns in layout
    {
        improvement_matrix  = array(NA, c(4,4))
        significance_matrix  = array(NA, c(4,4))
      
        #loops assembling rows of 4x4 matrix
        for(uncal_calibrated    in unique(	as.vector(m_uncal_calibrated  	)))
          for(subcatchment_outlet  in unique(	as.vector(m_subcatchment_outlet 	)))
            for(resolution          in unique(	as.vector(resolutions      	)))
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
                
                ip_col = paste0("I_P_", subcatchment_outlet,"_",target_var,"_", dynamics_yield)
                improvement_value = parameterizations[curr_parameterization_row, ip_col]
                
                normalizer = parameterizations[curr_parameterization_row,     sub(ip_col, pattern = "I_P_", repl="norm_")]
                precision_thresh = IP_precision[IP_precision$res==resolution, sub(ip_col, pattern = "I_P_", repl="preci_")] / normalizer
                if (is.na(precision_thresh)) stop("strange things happen")
                
                array_index= #determine position in matrix where to write improvement value
                  subcatchment_outlet ==  m_subcatchment_outlet &
                  resolution       == m_day_hour &
                  dynamics_yield      == m_dynamics_yield &
                  uncal_calibrated    == m_uncal_calibrated 
                
                improvement_matrix  [array_index] = improvement_value
                significance_matrix [array_index] = abs(improvement_value) < precision_thresh  #threshold for being INsignificant: larger than 95%percentile range
                
                #store performance of reference configurations in a similar matrix (unnecessarily replicated in this loop, but, alas)
                if (!enhancement %in% c("calibration", "resolution"))
                {
                  #browser()
                  ref_id = parameterizations[curr_parameterization_row, "reference"] #get ID of reference
                  ref_p  = parameterizations[ref_id == parameterizations$parameterization_ID, sub(ip_col, pattern="I_P_", repl="")] #performance of reference parameterization
                  matr_offset =c(ifelse(model_setting=="A",0,4), ifelse(target_var=="wat",0,4))
                  arr_index2 = which(array_index, arr.ind = TRUE) + matr_offset
                  collected_reference_p[array_index] =  ref_p
                }
              } 
        
    
        collected_ip_matrices[
          (which(model_setting == model_settings)-1) *4 + 1:4, 
          (which(target_var == target_vars)-1) *4 + 1:4,
          which(enhancement == enhancement_names)
            ] = improvement_matrix #store for later use (plotting)
          
        
        collected_sign_matrices[
          (which(model_setting == model_settings)-1) *4 + 1:4, 
          (which(target_var == target_vars)-1) *4 + 1:4,
          which(enhancement == enhancement_names)
          ] = significance_matrix #store for later use (plotting)
        
        plot(1, axes=FALSE, type="n", xlab="", ylab="") #dummy plot
        
        if (target_var==target_vars[1]) #first col in plot
          mtext(text=model_setting, side=2, outer=FALSE, cex=font_size) #write model_setting
        if (model_setting==model_settings[1]) #first row in plot
          mtext(text = ifelse(target_var=="wat","water","sediment"), side=3, outer=FALSE, cex=font_size) #write target variable
        
        
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
        
        n = length(berry_pal)
        extra_rg = 1/(n-2) #to accomodate values outside -1..1
        
        dmatrix = improvement_matrix  
        dmatrix = apply(dmatrix, MARGIN = 1, pmax, -1 - extra_rg ) #confine to [-1 .. 1] (otherwise, extreme values are plotted in white only)
        dmatrix = apply(dmatrix, MARGIN = 1, pmin,  1 + extra_rg)
        image(col = berry_pal, z = arev(t(dmatrix),c(FALSE, TRUE)), axes=FALSE, 
              #            main=plot_title,
              zlim = c(-1 -extra_rg*1.1, 1 +extra_rg*2), breaks = c(-1e10,seq(from=-1.01, to=1.01, length.out = n-1), 1e10))  
        
        
        
        grid(nx = 4, ny = 4)
        
        #add information on (in)significance
        insig_indx = (which(significance_matrix, arr.ind = TRUE) - 1) / (nrow(significance_matrix)-1)
        if (nrow(insig_indx)>0)
        {
          insig_indx[,1] = 1-insig_indx[,1] #flip direction to match plotting of matrix
          cell_width=1/(nrow(significance_matrix)-1)/2
        
          # mark_cell = function(coords) #plot hatched rectangle
          # {
          #   rect(xleft=coords[2]-cell_width, ybottom = coords[1]-cell_width, xright = coords[2]+cell_width, ytop = coords[1] + cell_width, border = NULL, density = 20, col="white")    
          # }
          
          mark_cell = function(coords) #plot hatched rectangle
          {
            lines(x=coords[2]-c(-1,1)*cell_width, y = coords[1]-c(-1,1)*cell_width, col="white")    
          }
          
          for (jj in 1:nrow(insig_indx))
            mark_cell(insig_indx[jj,])
        }  
        
        
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
   
    mtext(padj = -0.3, text=paste0("ME", which(enhancement==enhancement_names),": ", enhancement), side=3, outer=TRUE, cex=font_size) #write window title
    if (saveplots) 
    {
      enhancement=sub(enhancement,pattern = "/", repl="_")
      savePlot(file=paste0(base_dir,"plots/ip_matrix/",enhancement,ifelse(rescale_ip_values,"_rescaled",""),".wmf"), type = "wmf" ) #flawed in RStudio
      savePlot(file=paste0(base_dir,"plots/ip_matrix/",enhancement,ifelse(rescale_ip_values,"_rescaled",""),".png"), type = "png")
    }  
    
  }
  save(list = c("collected_ip_matrices", "collected_sign_matrices", "collected_reference_p"), file=paste0(base_dir,"collected_ip_matrices.RData"))
}


#---- insert IP-matrices into xlsx-file (for unknown reasons, this may require restarting R to work) ----
{
  load(file=paste0(base_dir,"collected_ip_matrices.RData"))
  
    try(detach("package:xlsx"    , unload=TRUE, force = TRUE), silent = TRUE)
    try(detach("package:xlsxjars", unload=TRUE, force = TRUE), silent = TRUE)
    try(detach("package:XLConnect", unload=TRUE, force = TRUE), silent = TRUE)
    try(detach("package:XLConnectJars", unload=TRUE, force = TRUE), silent = TRUE)
  
  library(XLConnect)
  
  #template_sheet = read.xlsx(file = paste0(base_dir, "IP_matrices.xlsx"), sheetName = "template", header = FALSE,  stringsAsFactors=FALSE)
  #Oct 2017: xlsx implementation does not properly handle *reading* cell styles
  # xlsconnect only allows setting pre-defined colours
  
  #aggregate over temporal resolution, A/B and calibration
  tt=matrix(as.matrix(rel_range_I_P[,-1]), ncol=1) #write all IP-values into a single vector
  aux4aggr= sub(x = rel_range_I_P$parameterization_ID, pattern = "(.).(\\d)*_._(\\d+)", repl="\\2") #create aggregation key from IDs
  mean_rel_range_I_P = aggregate(x = rel_range_I_P[,-1], by = list(parameterization_ID=aux4aggr), FUN = mean)
  mean_rel_range_I_P = mean_rel_range_I_P[is.finite(as.numeric(mean_rel_range_I_P$parameterization_ID)),]  #discard row that contain results from rows without ASDs

  # aux4aggr= sub(x = names(mean_rel_range_I_P)[-1], pattern = "_dyn|_yil", repl="") #create aggregation key from metric names
  # mean_rel_range_I_P2 = aggregate(x = t(mean_rel_range_I_P[-1]), by = list(metric=aux4aggr), FUN = mean)
  # mean_rel_range_I_P3 = data.frame(t(mean_rel_range_I_P2[,-1]))
  # names(mean_rel_range_I_P3) =mean_rel_range_I_P2$metric  
  # mean_rel_range_I_P3$ME=rownames(mean_rel_range_I_P)
  
#prepare sheets for each ME, fill in values (using XLConnect because of better copy options)
  wb <- loadWorkbook(file = paste0(base_dir, "IP_matrices.xlsx"))  
  setStyleAction(wb, XLC$"STYLE_ACTION.NONE") #when writing data, do not touch cell styles
  sheets <- getSheets(wb)
  for (i in 1:length(enhancement_names))
  {
    sheetName=sub(enhancement_names[1,i],pattern = "/", repl="_")
    if (existsSheet(wb,sheetName))
      removeSheet(wb, sheet=sheetName)
    cloneSheet(wb,"template",sheetName)
    
    writeWorksheet(object = wb, data = paste0("ME",i,": ", sheetName), sheet = sheetName, startRow = 1, startCol = 1, header = FALSE)
    
    XLConnect::writeWorksheet(object = wb, data = collected_ip_matrices[,,i], sheet = sheetName, startRow = 4, startCol = 5, header = FALSE)
    #summary lines below
    mean_IP=apply(collected_ip_matrices[,,i], MARGIN = 2, FUN=median, na.rm=TRUE)
    mean_IP=as.numeric(sapply(round(mean_IP, digits=2), FUN = format, digits=3))
    mean_IP=matrix(nrow=1, mean_IP) # Reshape to matrix
    writeWorksheet(object = wb, data = mean_IP, sheet = sheetName, startRow = 13, startCol = 5, header = FALSE)
    
    #summary lines right
    mean_IP=aggregate(as.vector(collected_ip_matrices[,,i]), by=list(rws = rep(rep(1:2, each=4),8)), FUN=median, na.rm=TRUE)
    mean_IP=matrix(ncol=1, rep(mean_IP$x, each=4)) #because we fill duplicated cells. Reshape to matrix
    mean_IP=as.numeric(sapply(round(mean_IP, digits=2), FUN = format, digits=3))
    writeWorksheet(object = wb, data = mean_IP, sheet = sheetName, startRow = 4, startCol = 14, header = FALSE)
    
    mean_asd = median(as.matrix(mean_rel_range_I_P[i,-1]))
    mean_asd=as.numeric(sapply(round(mean_asd, digits=2), FUN = format, digits=3))
    mean_asd = matrix(ncol=1, rep(mean_asd, 4)) #because we fill quadrupled cells. Reshape to matrix
    writeWorksheet(object = wb, data = mean_asd, sheet = sheetName, startRow = 4, startCol = 15, header = FALSE)
  
    # # colouring cells - only preset colours possible :-(
    # cs <- createCellStyle(wb)
    # # Specify the fill background color for the cell style created above
    # setFillBackgroundColor(cs, color = XLC$"COLOR.CORNFLOWER_BLUE")
    # 
    # setCellStyle(wb, sheet = "rainfall", row = 1, col = 1, cellstyle = cs)
  }  
  saveWorkbook(wb, file = paste0(base_dir, "IP_matrices.xlsx"))
  detach("package:XLConnect", unload=TRUE, force=TRUE)
  detach("package:XLConnectJars", unload=TRUE, force=TRUE)
  
#colour cells (using xlsx because of better formatting options) 
  library(xlsx)
  
  wb <- loadWorkbook(file = paste0(base_dir, "IP_matrices.xlsx"))  
  sheets <- getSheets(wb)
  
  # #determine number of columns and rows
  # tt = strsplit(names(template_cells_values[length(template_cells_values)]), split="\\.")
  # nrows=as.numeric(tt[[1]][1]) 
  # ncols=as.numeric(tt[[1]][2])
  library(berryFunctions)
  berry_pal = divPal(n = 16, reverse = FALSE, alpha = 1, extr = FALSE, yb = FALSE,
                     yr = FALSE, colors = NULL)
  
  mlapply <- function(lol,FUN,...){
    llol <- sapply(lol, length)
    nrows <- llol[1]
    if (any(llol != nrows)) stop("lists not of same length")
    
    arglists <- lapply(as.list(1:length(lol[[1]])),
                       function(i) lapply(lol, `[[`, i))
    names(arglists) <- names(lol[[1]])
    lapply(arglists, function(x)do.call(FUN,c(x,...)))
  }
   
  for (i in 1:length(enhancement_names))
  {
    significance_matrix = collected_sign_matrices[,,i]
    
    
    sheetName=sub(enhancement_names[1,i],pattern = "/", repl="_")

    new_sheet=sheets[[sheetName]]
    new_rows  <- getRows(new_sheet, rowIndex=3+(1:8))
    new_cells <- getCells(new_rows, colIndex=4+(1:8)) 
    
    n = length(berry_pal)
    #colour_indx = collected_ip_matrices[,,i] * 7.5 + 8.5  
    colour_indx = round(collected_ip_matrices[,,i] * ((n-2)/2-0.5) + (n-2)/2 + 1)  #rescale [-1..1] to length of palette
    colour_indx =   pmax(colour_indx, 1) #move values outside [-1..1] to extreme colours
    colour_indx =   pmin(colour_indx, n)
    colour_indx =   t(colour_indx)
    patterns = array(data = "SOLID_FOREGROUND", dim(significance_matrix))
    patterns[significance_matrix] = "THIN_HORZ_BANDS"  #mark insignificant cells
    
    #tt=lapply(FUN = Fill, X = berry_pal[colour_indx], backgroundColor="#000000",
    #            pattern="SOLID_FOREGROUND") #assemble colours

    tt=mlapply(FUN = Fill, lol=list(foregroundColor=berry_pal[colour_indx],
              pattern=t(patterns)), backgroundColor="#FFFFFF") #assemble colours
    
    plus=function(a,b){b+a}
    tt = lapply(X = tt, FUN = plus, CellStyle(wb)) #add wb properties to make it a valid cell style
    
    
    plus=function(a,b){a+b}
    tt = lapply(X = tt, FUN = plus, Alignment(horizontal="ALIGN_CENTER", vertical="VERTICAL_CENTER")) #add wb properties to make it a valid cell style
    
    tt2=lapply(FUN = Font, X = berry_pal[colour_indx], wb=wb, heightInPoints=7) #assemble font colours
    tt = mlapply(lol = list(a = tt, b=tt2), FUN = plus) #add font and other properties

    #label extremes
    values_vector= collected_ip_matrices[,,i]
    values_vector[significance_matrix] = NA #discard insignificant values
    values_vector= matrix(t(values_vector), ncol=1)
    xtrme = c(which.max(values_vector), which.min(values_vector))
    tt[xtrme] = lapply(X = tt[xtrme], FUN = plus, Font(color="black", wb=wb, heightInPoints=7)) #make extremes black
    
   

    
        
    tt                = lapply(X = tt,        FUN = plus, Border(color="black", position=c("BOTTOM","TOP","LEFT","RIGHT"), pen="BORDER_DOTTED")) #add wb properties to make it a valid cell style
    #tt[1:8]           = lapply(X = tt[1:8],   FUN = plus, Border(color="black", position=c("TOP","RIGHT"), pen=c("BORDER_THICK","BORDER_DOTTED"))) #add wb properties to make it a valid cell style
     tt[((1:8)*8)]  = lapply(X = tt[((1:8)*8)],  FUN = plus, Border(color="black", position=c("RIGHT","BOTTOM"),   pen=c("BORDER_THICK","BORDER_DOTTED"))) #add wb properties to make it a valid cell style
     tt[(64-(0:7))]    = lapply(X = tt[(64-(0:7))],   FUN = plus, Border(color="black", position=c("BOTTOM","LEFT"),  pen=c("BORDER_THICK","BORDER_DOTTED"))) #add wb properties to make it a valid cell style
    # tt[((0:7)*8+1)]    = lapply(X = tt[((0:7)*8+1)],    FUN = plus, Border(color="black", position=c("LEFT","TOP"),  pen=c("BORDER_THICK","BORDER_DOTTED"))) #add wb properties to make it a valid cell style
     tt[64] = lapply(X = tt[64],   FUN = plus, Border(color="black", position=c("BOTTOM","TOP","RIGHT","LEFT"),  pen=c("BORDER_THICK","BORDER_DOTTED"))) #add wb properties to make it a valid cell style
    

    mapply(FUN = setCellStyle, cell=new_cells,  cellStyle=tt) #set styles (rows first, as in new_cells)
    
    #setCellStyle(cell=new_cells[[2]], cellStyle = cs) #set style
    #mapply(FUN = setCellStyle, cell=new_cells,  cellStyle=cs) #set styles
    #lapply(FUN = setCellStyle, X=new_cells, cellStyle=cs) #set styles
  }  
  
  saveWorkbook(wb, file = paste0(base_dir, "IP_matrices.xlsx"))
  detach("package:xlsx", unload=TRUE)
}

#---- add summary IP-table to xlsx (Fig. 7)----
{
  treat_ME8ME9=FALSE  #should "resolution" and "calibration" be treated as ordinary ME?
  library(xlsx)

  wb <- loadWorkbook(file = paste0(base_dir, "IP_matrices.xlsx"))
  sheets <- getSheets(wb)

  library(berryFunctions)
  berry_pal = divPal(n = 16, reverse = FALSE, alpha = 1, extr = FALSE, yb = FALSE,
                     yr = FALSE, colors = NULL)

  mlapply <- function(lol,FUN,...){
    llol <- sapply(lol, length)
    nrows <- llol[1]
    if (any(llol != nrows)) stop("lists not of same length")
    
    arglists <- lapply(as.list(1:length(lol[[1]])),
                       function(i) lapply(lol, `[[`, i))
    names(arglists) <- names(lol[[1]])
    lapply(arglists, function(x)do.call(FUN,c(x,...)))
  }
  
  summary_vals=NULL

  #collect summarized IP-values from the sheets
  for (i in 1:length(enhancement_names))
  {
    sheetName=sub(enhancement_names[1,i],pattern = "/", repl="_")

    row  <- getRows(sheet = sheets[[sheetName]], rowIndex=13)
    cls= getCells(row=row, colIndex=5:12, simplify=TRUE)
    a=unlist(lapply(cls, getCellValue))
    summary_vals=rbind(summary_vals,a)
  }

  # ranks_in_col = apply(summary_vals, MARGIN = 2, rank) #compute ranks of mean IPs
  # ranks_in_row = t(apply(summary_vals, MARGIN = 1, rank))
  #

  sheet=sheets[["summary_ip_2"]]
  for (i in 1:length(enhancement_names))
  {
    row  <- getRows(sheet = sheet, rowIndex=5+i)
    cls  = getCells(row=row, colIndex=5:12, simplify=TRUE)

    mapply(setCellValue, cls, summary_vals[i,])

    n = length(berry_pal)
    colour_indx = round(summary_vals[i,] * ((n-2)/2-0.5) + (n-2)/2 + 1)  #rescale [-1..1] to length of palette
    colour_indx =   pmax(colour_indx, 1) #move values outside [-1..1] to extreme colours
    colour_indx =   pmin(colour_indx, n)
    colour_indx =   t(colour_indx)
    
    #tt=lapply(FUN = Fill, X = berry_pal[colour_indx], backgroundColor="#000000",  pattern="SOLID_FOREGROUND") #assemble colours

    tt=mlapply(FUN = Fill, lol=list(foregroundColor=berry_pal[colour_indx],
                                    pattern=rep("SOLID_FOREGROUND", length(colour_indx)), backgroundColor=rep("#FFFFFF", length(colour_indx)))) #assemble colours
    plus=function(a,b){b+a}
    tt = lapply(X = tt, FUN = plus, CellStyle(wb)) #add wb properties to make it a valid cell style

    plus=function(a,b){a+b}
    tt = lapply(X = tt, FUN = plus, Alignment(horizontal="ALIGN_CENTER", vertical="VERTICAL_CENTER"))

    tt = lapply(X = tt, FUN = plus, Font(wb=wb, heightInPoints=7)) #font size

    #underline best and worst per row
    
    if (!(i %in% c(8,9)) || treat_ME8ME9) 
    {
      sorted = sort.int(summary_vals[i,]) #
      max_in_row  = which((summary_vals[i,] > 0) & summary_vals[i,] == sorted[length(sorted)]) #indices to best and worst two
      max_in_row2 = which((summary_vals[i,] > 0) & summary_vals[i,] == sorted[length(sorted)-1])
      min_in_row  = which(                         summary_vals[i,] == sorted[1])
      min_in_row2 = which(                         summary_vals[i,] == sorted[2])
  
  
      position_str = rep(list(NULL), length(tt))
      color_str    = rep(list(NULL), length(tt))
      pen_str      = rep(list(NULL), length(tt))
  
      position_str[c(max_in_row, max_in_row2, min_in_row, min_in_row2)]  = list("BOTTOM")
  
      color_str   [c(max_in_row, max_in_row2)]     = list("green")
      color_str   [c(min_in_row, min_in_row2)]     = list("red")
  
      pen_str     [c(max_in_row2, min_in_row2)]       = list("BORDER_THIN")
      pen_str     [c(max_in_row, min_in_row)]         = list("BORDER_THICK")
  
  
      #underline best and worst per column
      for (j in 1:8)
      {
        if (treat_ME8ME9)
          rows2use=1:length(enhancement_names) else #use all model enhancements
          rows2use=1:(length(enhancement_names)-2) #ignore "resolution" and "calibration"
        #underline best and worst per col
        sorted =   sort.int(summary_vals[rows2use,j]) #sort the column values
        max_in_col  = which((summary_vals[rows2use,j] > 0) & summary_vals[rows2use,j] == sorted[length(sorted)]) #indices to best and worst two
        max_in_col2 = which((summary_vals[rows2use,j] > 0) & summary_vals[rows2use,j] == sorted[length(sorted)-1])
        min_in_col  = which(                                 summary_vals[rows2use,j] == sorted[1])
        min_in_col2 = which(                                 summary_vals[rows2use,j] == sorted[2])
  
        if (i %in% c(max_in_col, max_in_col2, min_in_col, min_in_col2))
          position_str [[j]]  = c(position_str [[j]], "LEFT")
        if (i %in% c(max_in_col, max_in_col2))
          color_str [[j]]  = c(color_str [[j]], "green")
        if (i %in% c(min_in_col, min_in_col2))
          color_str [[j]]  = c(color_str [[j]], "red")
  
        if (i %in% c(max_in_col2, min_in_col2))
          pen_str [[j]]  = c(pen_str [[j]], "BORDER_THIN")
  
        if (i %in% c(max_in_col, min_in_col))
          pen_str [[j]]  = c(pen_str [[j]], "BORDER_THICK")
  
      }
  
      for (i in 1:length(position_str))
        if (length(position_str[[i]])!=0)
         tt[[i]]  = tt[[i]] + Border(position=position_str[[i]], pen=pen_str[[i]],color=color_str[[i]])
    }
    mapply(FUN = setCellStyle, cell=cls,  cellStyle=tt) #set styles (rows first, as in new_cells)
  }

  saveWorkbook(wb, file = paste0(base_dir, "IP_matrices.xlsx"))
}
  
    
#---- plot A vs. B, single aspect (barplot 2) (not used)----
  {
    
  load(paste0(base_dir,"collected_ip_matrices.RData"))  

    aggr_cal_res = FALSE #aggregate over temporal resolution and calibration? Or use primary data?
    
    if (aggr_cal_res)
    aggr_key=list(config_id=parameterizations$config_ID) else #aggregate over temporal resolution and calibration
    aggr_key=list(config_id=parameterizations$parameterization_ID) #Or use primary data?
      
    aggr = aggregate(x = parameterizations[, grepl(x = names(parameterizations), "^I_P_")], by = aggr_key, FUN = mean)
    
    #1: 
    target_var = "wat"
    resolution = 24
    uncal_calibrated="u"
    subcatchment_outlet="out"
    dynamics_yield="dyn"
    
    
    resolution = 24
    uncal_calibrated="u"
    target_var="sed"
    improvement_matrices = array(NA, c(2, length(enhancement_names)))
    
    #warning("nur Wasser aktiviert!")
    for(uncal_calibrated  in unique(	m_uncal_calibrated))
    for(resolution  in unique(	resolutions))
    for(subcatchment_outlet  in unique(	as.vector(m_subcatchment_outlet 	)))
       for(target_var          in unique(	as.vector(target_vars    	)[1]))
         for(dynamics_yield    	in unique(	as.vector(m_dynamics_yield      	)))
      {
          {
            improvement_matrices[, ] = NA
            for (enhancement in enhancement_names)
            {  
              if (enhancement == "calibration")
                 uncal_calibrated2 = ifelse(uncal_calibrated=="c","u","c") else #because for this enhancement, all records were stored as "calibrated". However, the IP value of this ME and an already calibrated config cannot not exist, and will be NA 
                  uncal_calibrated2 = uncal_calibrated
                
                if (enhancement == "resolution")
                   resolution2 = ifelse(resolution=="1","24","1") else #because for this enhancement, all records were stored as "1". However, the IP value of this ME and an already hourly config cannot not exist, and will be NA 
                    resolution2 = resolution #use as prespecified
                  
                  row_no = grepl(x = aggr$config_id, pattern=paste0("[\\+\\-]",which(enhancement == enhancement_names)))
                  aggr$config_id[row_no]
                  
                  if (!aggr_cal_res)
                    row_no = row_no & grepl(x = aggr$config_id, pattern = paste0(uncal_calibrated2,"_", resolution2))
                  
                  if (sum(row_no) != 2) next
                  
                  col_name = paste("I_P",subcatchment_outlet,target_var, dynamics_yield, sep="_")
                  
                  improvement_matrices[, enhancement == enhancement_names] = aggr[row_no, col_name]
            }     
            #improvement_matrices[, enhancement == enhancement_names]= improvement_matrices[, enhancement == enhancement_names]/count  #use mean when aggregation is on
            
            graphics.off()
            windows(width = 7, height = 5)
            par(mar=c(8,5,2,2))
            ylim = extendrange(improvement_matrices[,-9], f = 0.13) #treat improvement by resolution differently, as this often bumps off everything else
            a=barplot(pmax(improvement_matrices, ylim[1]), beside = TRUE, ylab=I [P]~"" , ylim = ylim, col =  c("white","grey"))
            abline(v=seq(from=3.5, by=3, length.out = length(enhancement_names)-1))
            
            
            abline(h=0, lty="dashed")
            enhancement_names2 =
              sub(enhancement_names, pattern = "(factor |LAI/C |tivity |flow )", repl="\\1\n")
            axis(side=2)
            par(lheight=.7)
            at = apply(a, 2, mean)
            spc = at[2]- at[1]
            axis(mgp=c(3,2,0), hadj = 1, side=1, labels = c(NA, enhancement_names2, NA), at = c(min(at)- spc, at, max(at)+ spc), las=2)
            mtext(side=1, line = 0.8, text = paste0("ME",1:9), at = at, las=1)
            
            if (any(improvement_matrices[,9] < ylim[1], na.rm = TRUE))
            {
              rect(xleft = a[1,9]-.9, ybottom = ylim[1]+diff(range(ylim))/100, xright = a[2,9]+0.9, ytop = diff(range(ylim))/5 + ylim[1], col = "white", border = "white")
              text(x = a[1,9], y = diff(range(ylim))/10 + ylim[1], labels = format(improvement_matrices[1,9], digits = 3), srt=90)
              text(x = a[2,9], y = diff(range(ylim))/10 + ylim[1], labels = format(improvement_matrices[2,9], digits = 3), srt=90)
            }
            
            legend("topright", legend = c("A","B"), fill =  c("white","grey"))
            
            if (aggr_cal_res)
              pp = "aggr" else
              pp= paste(resolution, uncal_calibrated, sep="_")
            combi=paste(target_var, pp, subcatchment_outlet, dynamics_yield, sep="_")
            
            if (saveplots) 
            {
             #savePlot(file=paste0(base_dir,"plots/single_aspect/","A_vs_B_",combi,".png"), type = "png")
              savePlot(file=paste0(base_dir,"plots/single_aspect/","A_vs_B_",combi,".wmf"), type = "wmf")
            } 
            
          }
        
      }
  
     
  }
  
#---- plot range in improvement values for each enhancement (ASD) (Fig. 5)----
{
  treat_ME8ME9=FALSE  #should "resolution" and "calibration" be treated as ordinary ME?
  asd_pal = colorRampPalette(colors = c("blue","red") )(16)
  
  #windows(width = length(resolutions)*4, height = length(target_vars)*4.5)
  windows(width = length(target_vars)*4*1.1, height = length(model_settings[1])*3+2)
  
  for (enhancement in enhancement_names)
  {  

    par(mfrow=c(nr=length(model_settings[1]), nc=length(target_vars)), #divide into subplots
        oma=c(0,1,3,0), mar=c(0,1.3,2,0))                                         #set outer margin at top for title   
    #par(mfrow=c(nr=length(model_settings), nc=length(target_vars)), #divide into subplots
    #    oma=c(0,1,3,0), mar=c(0,1.3,2,0))                                         #set outer margin at top for title   
    
    for (model_setting in model_settings[1]) #loop filling rows in layout
    {
      for (target_var in target_vars) #loop filling columns in layout
      {
    
       improvement_matrix  = array(NA, c(4,4))
        
        #loops assembling rows of 4x4 matrix
        for(uncal_calibrated    in unique(	as.vector(m_uncal_calibrated  	)))
          for(subcatchment_outlet  in unique(	as.vector(m_subcatchment_outlet 	)))
            for(resolution          in unique(	as.vector(resolutions      	)))
              for(dynamics_yield    	in unique(	as.vector(m_dynamics_yield      	)))
              {  
                enhancement_present = ifelse(model_setting=="A",1,0) #if A+, select row where enhancement was added, for B- select row where it was removed
                
                if (enhancement == "calibration")
                  uncal_calibrated2 = ifelse(uncal_calibrated=="c","u","c") else #because for this enhancement, all records were stored as "calibrated". However, the IP value of this ME and an already calibrated config cannot not exist, and will be NA 
                    uncal_calibrated2 = uncal_calibrated
                  
                  if (enhancement == "resolution")
                    resolution2 = ifelse(resolution=="1","24","1") else #because for this enhancement, all records were stored as "1". However, the IP value of this ME and an already hourly config cannot not exist, and will be NA 
                      resolution2 = resolution #use as prespecified
                    
#                  row_no = which(grepl(x = rel_range_I_P$parameterization_ID, pattern=paste0(which(enhancement == enhancement_names),"_")))
#                  col_name = paste("I_P",subcatchment_outlet,target_var, dynamics_yield, sep="_")
                    
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
                  resolution       == m_day_hour &
                  dynamics_yield      == m_dynamics_yield &
                  uncal_calibrated    == m_uncal_calibrated 
                
                improvement_matrix[array_index] = improvement_value
                
              } 
        
        plot(1, axes=FALSE, type="n", xlab="", ylab="") #dummy plot
        #browser()
      #  if (target_var==target_vars[1]) #first col in plot
      #    mtext(text=model_setting, side=2, outer=FALSE, cex=font_size) #write model_setting
        if (model_setting==model_settings[1]) #first row in plot
          mtext(text = ifelse(target_var=="wat","water","sediment"), side=3, outer=FALSE, cex=font_size) #write target variable
        
        if (all(is.na(improvement_matrix))) next    
        
        
        par(new=TRUE)      
        #plot improvement matrix
        t_var = ifelse(target_var=="wat", "water", "sediment")
        
        dmatrix = apply(improvement_matrix, MARGIN = 1, pmax, 0) #confine to [0..3]
        dmatrix = apply(dmatrix,            MARGIN = 1, pmin, 3)
        image(col = asd_pal, z = arev(t(dmatrix),c(FALSE, TRUE)), axes=FALSE, 
              #            main=plot_title,
              zlim = c(0,3))
        
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
    
    title= paste0("ME", which(enhancement==enhancement_names),": ", enhancement) 
    if (!treat_ME8ME9 && (enhancement %in% c("resolution", "calibration")))
      title= enhancement
          
    mtext(padj = -0.3, text=title, side=3, outer=TRUE, cex=font_size) #write window title
    if (saveplots) 
    {
      enhancement=sub(enhancement,pattern = "/", repl="_")
      savePlot(file=paste0(base_dir,"plots/ASD/",enhancement,"_ASD",".wmf"), type = "wmf" ) #flawed in RStudio
      savePlot(file=paste0(base_dir,"plots/ASD/",enhancement,"_ASD",".png"), type = "png")
    }  
    
  }
}  
  

#---- further analysis (II) ----
  enhancements_list = sub(pattern = "..(\\d)*_.*", x = parameterizations$parameterization_ID, repl="\\1")
  p2                = parameterizations[!(enhancements_list %in% c("A","B")),]  #discard reference parametrisations, as there are no improvement values for them
  enhancements_list = enhancements_list[!(enhancements_list %in% c("A","B"))]
  
  col_index_ip_values = which(grepl(names(p2), pattern = "^I_P"))
  median_improvements=aggregate(x = p2[, col_index_ip_values], by = list(me=enhancements_list), FUN = median, na.rm=TRUE) 
  
  #rank (aggregated) improvement aspects for each ME
  ranks_ME=t(apply(as.matrix(-median_improvements[, -1]), MARGIN = 1, FUN = rank))
  
  #rank ME for each (aggregated) improvement aspect
  ranks_aspect=t(apply(as.matrix(-median_improvements[, -1]), MARGIN = 2, FUN = rank))
  
  #total_median_improvements=apply(X = median_improvements[,-1], MARGIN = 1, FUN = mean, na.rm=TRUE) 
  

#---- aggregate improvement values by ME - overview barplot ----
{ 
  treat_ME8ME9=FALSE
  improvements_all = NULL
  
  for (me in unique(enhancements_list))
    for (ab in model_settings)
    {  
      sel_rows = which(enhancements_list==me &  substr(p2$config_ID, start = 1, stop=1)==ab)
      sel_rows = sel_rows[1:4] #force vector length of 4 (can be less for enhancements 8 and 9)
      improvements_all = cbind(improvements_all,
                               matrix(as.matrix(p2[sel_rows ,  col_index_ip_values ] ), ncol=1))
    }  

  windows(width = 6, height = 5.5)
  par(mar=c(8.5,5,2,2))
  ylim = apply(FUN=quantile, MARGIN = 2, X=improvements_all, probs = c(0.45,0.75), na.rm=TRUE)
  ylim = c(min(ylim[1,]), min(10,max(ylim[2,])))
  boxplot(improvements_all, ylim=ylim, col=c("white","grey"), axes=FALSE, ylab=I [P]~"[-]")
#  boxplot(improvements_all, ylim=quantile(improvements_all, probs = c(0.2,0.8), na.rm=TRUE), col=c("white","grey"), axes=FALSE, ylab=I [P]~"[-]")
  #  , xlim=c(0, length(enhancement_names)*2+.5)
  #which(diff(apply(improvements_all, MARGIN = 2, FUN = median, na.rm=TRUE))[(1:9)*2-1]<0) #find MEs where the median is better for A than for B
  #sum(apply(improvements_all>0, MARGIN = 2, FUN = sum, na.rm=TRUE))/sum(!is.na(improvements_all)) #percentage of parameterizations with positive IP-value

  
  #notations at/below x-axis
  abline(v=(1:(length(enhancement_names)-1)*2+.5))
  abline(h=0, lty="dashed")
  enhancement_names2 =
    sub(enhancement_names, pattern = "(factor |LAI/C |tivity |flow )", repl="\\1\n")
  axis(side=2)
  par(lheight=.7)
  axis(mgp=c(3,1.5,0), hadj = 1, side=1, labels = c(NA, enhancement_names2, NA), at = (0:(length(enhancement_names)+1))*2-.5, las=2, tick=FALSE)
  axis(mgp=c(3,2,0), hadj = 1, side=1, labels = NA, at = (0:(length(enhancement_names)+1))*2-1.5, las=2)
  if (treat_ME8ME9)
    n = length(enhancement_names) else
    n = length(enhancement_names)-2
  mtext(side=1, line = 0.2, text = paste0("ME",1:n), at = (1:n)*2-.5, las=1) 
    
  
  #perc_improved=apply( improvements_all>0, MARGIN = 2, FUN = sum, na.rm=TRUE)/(apply(improvements_all, MAR=2, FUN = function(x){sum(!is.na(x) & (x != 0), na.rm=TRUE)})) #compute percentage of parameterizations that have positive improvement
  perc_improved=apply( improvements_all>0, MARGIN = 2, FUN = sum, na.rm=TRUE)/(apply(improvements_all, MAR=2, FUN = function(x){sum(!is.na(x) , na.rm=TRUE)})) #compute percentage of parameterizations that have positive improvement
  #mean(perc_improved) #mean percentage of improved parameterization
  print(paste("mean perc IP>0 ",mean(perc_improved)))
  print(paste("mean perc IP>0 (A):", mean(perc_improved[   seq(from=1, by=2, length.out=length(enhancement_names))])))
  print(paste("mean perc IP>0 (B):", mean(perc_improved[1+ seq(from=1, by=2, length.out=length(enhancement_names))])))
  
  
  mtext(side=3, text = round(perc_improved*100, digits = 0), at = 1:(length(enhancement_names)*2), las=2, adj = 1, line=1.5)
  mtext(side=3, text = I [P]~"> 0 [%]:", at = -3.5, las=1, adj = 0)
  axis(side=3, tick = TRUE, at = (0:(length(enhancement_names)+1))*2-1.5, labels=NA)

  #mtext(side=1, text = "ASD:", at = -3.5, las=1, adj = 0, padj = 11)
  mtext(side=1, text = "ASD:", at = -3.5, las=1, adj = 0, line = 7.2)
  mtext(side=1, text = format(round(med_rel_range_I_P$V1, digits=2), digits = 2 ), at = (1:(length(enhancement_names)))*2-.5, las=1, line=7.2)
  
  axis(mgp=c(3,2,0), hadj = 1, side=1, labels = NA, at = (0:(length(enhancement_names)+1))*2-1.5, las=2, line=7, tcl=0.5)
  

  
  legend("bottomright", legend = c("A","B"), fill =  c("white","grey"), bg = "white")
  if (saveplots) 
  {
    savePlot(file=paste0(base_dir,"plots/","A_vs_B",".wmf"), type = "wmf" ) 
    savePlot(file=paste0(base_dir,"plots/","A_vs_B",".png"), type = "png")
  }      

}
  

  
#---- plot ranks of improvement aspects for each ME (first version of manuscript only) ----
{  
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
}


#---- plot best two MEs for each parameterisation () ----
{
    #me_colors=c("cyan","blue", "darkgreen", "lightgreen", "orange", "grey", "magenta")
  me_colors=c("#a6cee3","#1f78b4","#fdbf6f","#b2df8a","#33a02c","#fb9a99","#e31a1c") #colorbrewer palette
  
  #omit +8 and +9 (resolution and calibration)
  parameterizations = parameterizations[!grepl(parameterizations$parameterization_ID, pattern = "(^.+8)|(^.+9)"),]
  enhancement_names = setdiff(enhancement_names, c("resolution", "calibration"))
  
  windows(width = length(resolutions)*4, height = length(target_vars)*4.5)
  par(mfrow=c(nr=length(target_vars), nc=length(resolutions)), #divide into subplots
      oma=c(0,0.7,1.3,0), mar=c(0,1.3,1.3,0))                                         #set outer margin at top for title   
  
  
  for (model_setting in model_settings) #loop filling rows in layout
  {
    for (target_var in target_vars) #loop filling columns in layout
    {
      bestME_matrix  = array(NA, c(4,4,2)) #matrix for collecting the best and second best ME for each parameterisation
      
      #loops assembling rows of 4x4 matrix
      for(uncal_calibrated    in unique(	as.vector(m_uncal_calibrated  	)))
        for(subcatchment_outlet  in unique(	as.vector(m_subcatchment_outlet 	)))
          for(resolution          in unique(	as.vector(resolutions      	)))
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
                resolution       == m_day_hour &
                dynamics_yield      == m_dynamics_yield &
                uncal_calibrated    == m_uncal_calibrated 
              
              bestME_matrix[,,1][array_index] = ranks_ME[1] #store first and second best
              bestME_matrix[,,2][array_index] = ranks_ME[2] #store first and second best
              
            } 
      
      plot(1, axes=FALSE, type="n", xlab="", ylab="") #dummy plot
      
      
      if (target_var==target_vars[1]) #first col in plot
        mtext(text=model_setting, side=2, outer=FALSE, cex=font_size) #write model_setting
      if (model_setting==model_settings[1]) #first row in plot
        mtext(text = ifelse(target_var=="wat","water","sediment"), side=3, outer=FALSE, cex=font_size) #write target variable
      
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
    savePlot(file=paste0(base_dir,"plots/ranks/","best_me",".wmf")) #flawed in RStudio
    savePlot(file=paste0(base_dir,"plots/ranks/","best_me",".png"), type = "png")
  }
  
  #plot legend
  windows(width = 1.5, height = 2.5)
  par(oma=c(0,0,0,0), mar=c(0.1,0.1,0.1,0.1))                                         #set outer margin at top for title     
  
  image(col = me_colors, z = arev(t(as.matrix(1:length(me_colors)))), axes=FALSE)
  text(adj = c(0.5,0.5), y = 0.0+(0:(length(me_colors)-1)/(length(me_colors)-1)), x=-0.0, labels=rev(enhancement_names), cex = 1.)
  if (saveplots) 
  {
    savePlot(file=paste0(base_dir,"plots/ranks/","legend_best_me",".wmf")) #flawed in RStudio
    savePlot(file=paste0(base_dir,"plots/ranks/","legend_best_me",".png"), type = "png")
  }
}


