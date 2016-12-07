#WASA calibration using PSO / DDS
#to be called like:
#nohup R --vanilla -f calibrate_main_dds.R --slave > console_output.txt &


    param_ranges=rbind(         #define parameter ranges
    gw_delay_f=c(1e1,100),
    soildepth_f=c(1e-2,15),
    kf_bedrock_f=c(1e-5,1e3),
    riverdepth_f=c(1e-1,15),
    log_kf_scale_f=c(log(1e-2),log(100)),         #5 
    log_ksat_factor=c(log(1e-2),log(20)),

#    riv_depth_f=c(0.01,30),
#     riv_width_f=c(0.001,10),            #8
#     riv_side_ratio_f=c(0.1,10),
#     riv_bottom_width_of_floodplain_f=c(0.1,40),     #
#     riv_side_ratio_floodplains_f=c(0.1,20),
#     riv_channel_slope_f=c(0.1,20),                  #
#    riv_length_f=c(0.3,3),
    riv_manningn_f=c(0.1,10),
#     riv_manningn_floodplain_f=c(0.1,10),            #
#     riv_baseflowalphafactor_f=c(0.1,10),            #
     riv_Muskingum_X_f=c(0.05,1/0.3),     
     riv_Muskingum_K_f=c(0.005,5),                     
      log_riv_Ksat_f=c(log(0.0001),log(20))
    )

starting.values=rbind(               #initial estimates
    gw_delay_f=c(1),
    soildepth_f=c(1),
    kf_bedrock_f=c(0.1),
    riverdepth_f=c(1),
    log_kf_scale_f=c(log(1)),
    log_ksat_factor=c(log(1)),

#     riv_depth_f=1,
#     riv_width_f=1,
#     riv_side_ratio_f=1,
#     riv_bottom_width_of_floodplain_f=1,
#     riv_side_ratio_floodplains_f=1,
#     riv_channel_slope_f=1,
#     riv_length_f=1,
    riv_manningn_f=1,
#     riv_manningn_floodplain_f=1,
#     riv_baseflowalphafactor_f=1,
    riv_Muskingum_X_f=1,
    riv_Muskingum_K_f=1,                            
  log_riv_Ksat_f=log(0.1)
    )
    
max_number_function_calls = 5000
    
if (file.exists("init_estimates.txt")) #load initial estimates, if present
{  
  starting.values = t(read.table(file="init_estimates.txt", header=TRUE))
  max_number_function_calls =  max_number_function_calls + 2000
}
library(ppso)
source("optim_wrapper.R") #include objective function
    
    

#full call
    res <- optim_pdds_robust(objective_function=optim_wrapper, number_of_particles=15, number_of_parameters=NROW(param_ranges), parameter_bounds=param_ranges, initial_estimates=starting.values, lhc_init=TRUE, part_xchange=0,
                                logfile="dds.log",projectfile="dds.pro", load_projectfile="try", break_file="stop.pso", nslaves=-1, max_wait_iterations=1000, max_number_function_calls=max_number_function_calls, tryCall=TRUE, execution_timeout=10)
    
#    res <- optim_dds(objective_function=optim_wrapper, number_of_particles=1, number_of_parameters=NROW(param_ranges), parameter_bounds=param_ranges, initial_estimates=starting.values, lhc_init=TRUE, part_xchange=0,
#                                logfile="dds.log",projectfile="dds.pro", load_projectfile="try", break_file="stop.pso", max_wait_iterations=1000, max_number_function_calls=max_number_function_calls, tryCall=TRUE)
    
 print(res)






