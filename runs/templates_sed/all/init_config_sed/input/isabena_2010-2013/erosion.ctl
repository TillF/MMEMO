#WASA-control file for erosion routines;
application_scale	0	#0: apply equations on TC-scale; 1: apply on subbasin-scale
erosion_equation	1	#erosion equation to be used: 1: USLE, 2: Onstad-Foster, 3: MUSLE, 4: MUST
ri_05_coeffs	1.911	0.807	#needed for USLE and OF: coefficients for estimation of maximum half-hour rainfall intensity (ri_05) from daily rainfall data (R_day): ri_05=a*R_day^b 
transport_limit_mode	1	#different modes how/if transport capacity of runoff is limited: 1: no transport capacity limit; 2: transport capacity according to Everaert (1991); 3:	transport capacity computed from MUSLE with maximum erodibility
