#WASA calibration using PSO / DDS, sediment part
#to be called like:
#nohup R --vanilla -f calibrate_main_dds_mpi_sed.R --slave > console_output.txt &


  param_ranges=rbind(         #define parameter ranges
    Manning_n_f=c(0.00001,2),
    ri_05_coeffs_a_f=c(0.1,10),
    ri_05_coeffs_b_f=c(0.001,5),
    erosion_equation=c(0.5,4.5),
    transport_limit_mode=c(0.5,3.5),
    riv_erodibilityfactor=c(0.0,1),
    riv_coverfactor=c(0.0,1),
    log_transp_cap_a=log10(c(1e-5,0.04)),
    transp_cap_b=	c(0.5, 4)
  )

starting.values=rbind(               #initial estimates
  Manning_n_f=1,
  ri_05_coeffs_a_f=1,
  ri_05_coeffs_b_f=1,
  erosion_equation=3,
  transport_limit_mode=1,
  riv_erodibilityfactor=0.5,
  riv_coverfactor=0.5,
  log_transp_cap_a=log10(0.01),
  transp_cap_b=1.
    )
    
max_number_function_calls = 5000
    
if (file.exists("init_estimates_sed.txt")) #load initial estimates, if present
{  
  starting.values = t(read.table(file="init_estimates_sed.txt", header=TRUE))
  max_number_function_calls =  max_number_function_calls + 2000
}
library(ppso)
source("optim_wrapper.R") #include objective function

#  optim_wrapper(parms=starting.values[,1]) #test call

#full call
    res <- optim_pdds_robust(objective_function=optim_wrapper, number_of_particles=15, number_of_parameters=NROW(param_ranges), parameter_bounds=param_ranges, initial_estimates=starting.values, lhc_init=TRUE, part_xchange=3,
                                logfile="dds.log",projectfile="dds.pro", load_projectfile="try", break_file="stop.pso", nslaves=-1, max_wait_iterations=1000, max_number_function_calls=max_number_function_calls, tryCall=TRUE, execution_timeout=10)
    
#    res <- optim_dds(objective_function=optim_wrapper, number_of_particles=1, number_of_parameters=NROW(param_ranges), parameter_bounds=param_ranges, initial_estimates=starting.values, lhc_init=TRUE, part_xchange=3,
#                                logfile="dds.log",projectfile="dds.pro", load_projectfile="try", break_file="stop.pso", max_wait_iterations=1000, max_number_function_calls=max_number_function_calls, tryCall=TRUE)
    
 print(res)






