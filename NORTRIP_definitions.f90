!****************************************************************************
!  NORTRIP_definitions.f90 
!****************************************************************************
!
!   MODULE:         NORTRIP_index_definitions
!   PURPOSE:        Defines all the array index constants
!   USED IN:        NORTRIP_definitions subroutines
!                   NORTRIP_salt_constants
!
!   MODULE:         NORTRIP_definitions
!   PURPOSE:        Declares and initialises arrays and variables
!   USED IN:        All routines
!
!   MODULE:         NORTRIP_salt_constants
!   PURPOSE:        Sets all the salt related constants
!   USED IN:        All salt related routines
!   SUBROUTINES:    set_salt_parameters
!    
!   SUBROUTINE:     set_constant_string_values
!   PURPOSE:        Sets all the search string parameters for reading in data
!   USED IN:        NORTRIP_fortran_control
!
!   SUBROUTINE:     allocate_NORTRIP_arrays
!   PURPOSE:        Allocates the allocatable arrays, except input data arrays
!   USED IN:        NORTRIP_fortran_control
!
!   SUBROUTINE:     deallocate_NORTRIP_arrays
!   PURPOSE:        Deallocates the allocatable arrays, including input data arrays
!   USED IN:        NORTRIP_fortran_control
!
!   VERSION:        14.10.2015
!   AUTHOR:         Bruce Rolstad Denby 
!                   Norwegian Meteorological Institute (www.met.no)
!
!****************************************************************************

!==========================================================================
!   NORTRIP model index definitions
!==========================================================================
    module NORTRIP_index_definitions

    implicit none
    !private
    
    logical :: NORTRIP_fortran_combined_flag=.false.

    integer ii,jj
    private ii,jj 
    !Indexes for arrays

    !-----------------------------------------------------------------------
    !vehicle clases
    integer he,li,num_veh
    parameter(he=1,li=2,num_veh=2)
    
    !tyre type
    integer st,wi,su,num_tyre
    parameter(st=1,wi=2,su=3,num_tyre=3)
    
    !Salt type
    integer na,mg,cma,ca,pfo
    parameter(na=1,mg=2,cma=3,ca=4,pfo=5)
    integer num_salt_max
    parameter (num_salt_max=5)
    
    !Moisture type
    integer water_index,snow_index,ice_index,num_moisture
    parameter(water_index=1,snow_index=2,ice_index=3,num_moisture=3)
    integer snow_ice_index(2)
    data (snow_ice_index(ii),ii=1,2) /snow_index,ice_index/
    
    !Size fraction index. pm_exhaust included here but only used for saving data purposes, not in the model
    integer, parameter :: pm_all=1
    integer, parameter :: pm_200=2
    integer, parameter :: pm_10=3
    integer, parameter :: pm_25=4

    integer, parameter :: num_size=4

    !Below is for saving gridded emissions. Continuing the index numbering for sizes.
    integer, parameter :: pm_exhaust=5
    integer, parameter :: nox_exhaust=6
    integer, parameter :: pm_co_road=7
    integer, parameter :: pm_co_tyre=8
    integer, parameter :: pm_co_brake=9
    integer, parameter :: pm_co_sand=10
    integer, parameter :: pm_co_salt1=11
    integer, parameter :: pm_co_tot=12
    integer, parameter :: pm_25_road=13
    integer, parameter :: pm_25_tyre=14
    integer, parameter :: pm_25_brake=15
    integer, parameter :: pm_25_sand=16
    integer, parameter :: pm_25_salt1=17
    integer, parameter :: pm_25_tot=18

    integer, parameter :: num_save_grid_vars = 18

    integer pm_sus(3)
    data (pm_sus(ii),ii=1,3) /pm_200,pm_10,pm_25/
    
    !Source index
    integer road_index,tyre_index,brake_index,sand_index,depo_index,fugitive_index,exhaust_index
    integer total_dust_index,crushing_index,abrasion_index
    integer salt1_index,salt2_index
    integer num_wear,num_dust,num_salt,num_source,num_source_all,num_source_all_extra
    parameter(road_index=1,tyre_index=2,brake_index=3,sand_index=4,depo_index=5,fugitive_index=6,exhaust_index=7)  
    parameter(total_dust_index=10,crushing_index=11,abrasion_index=12)!These last two used to index f_PM_dir and f_PM only
    parameter(salt1_index=8,salt2_index=9)
    parameter(num_wear=3,num_dust=7,num_salt=2,num_source=9,num_source_all=10,num_source_all_extra=12)
    !Additional index definitions for simplifying the calculations
    integer dust_index(7),dust_noexhaust_index(6),wear_index(3)
    data (dust_index(ii),ii=1,7) /road_index,tyre_index,brake_index,sand_index,depo_index,fugitive_index,exhaust_index/
    data (dust_noexhaust_index(ii),ii=1,6) /road_index,tyre_index,brake_index,sand_index,depo_index,fugitive_index/
    data (wear_index(ii),ii=1,3) /road_index,tyre_index,brake_index/
    integer all_source_index(num_source),all_source_noexhaust_index(num_source-1)
    data (all_source_index(ii),ii=1,num_source) /road_index,tyre_index,brake_index,sand_index,depo_index,fugitive_index,exhaust_index,salt1_index,salt2_index/
    data (all_source_noexhaust_index(ii),ii=1,num_source-1) /road_index,tyre_index,brake_index,sand_index,depo_index,fugitive_index,salt1_index,salt2_index/
    integer salt_index(2)
    data (salt_index(ii),ii=1,2) /salt1_index,salt2_index/

    !Dust balance indexes
    integer S_dusttotal_index,P_dusttotal_index,P_wear_index,S_dustspray_index,P_dustspray_index,S_dustdrainage_index
    integer S_suspension_index,S_windblown_index,S_cleaning_index,P_cleaning_index,S_dustploughing_index
    integer P_crushing_index,S_crushing_index,P_abrasion_index,P_depo_index
    integer num_dustbalance
    parameter (S_dusttotal_index=1,P_dusttotal_index=2,P_wear_index=3,S_dustspray_index=4,P_dustspray_index=5,S_dustdrainage_index=6)
    parameter (S_suspension_index=7,S_windblown_index=8,S_cleaning_index=9,P_cleaning_index=10,S_dustploughing_index=11)
    parameter (P_crushing_index=12,S_crushing_index=13,P_abrasion_index=14,P_depo_index=15)
    parameter (num_dustbalance=15)
 
    !Salt solution indexes
    integer RH_salt_index,melt_temperature_salt_index,dissolved_ratio_index,num_saltdata 
    parameter (RH_salt_index=1,melt_temperature_salt_index=2,dissolved_ratio_index=3,num_saltdata=3)
    !RH_salt_index and melt_temperature_salt_index not used in current solution
    
    !Dust process emission and concentration indexes
    integer E_direct_index,E_suspension_index,E_windblown_index,E_total_index
    integer C_direct_index,C_suspension_index,C_windblown_index,C_total_index
    integer num_process
    parameter (E_direct_index=1,E_suspension_index=2,E_windblown_index=3,E_total_index=4)
    parameter (C_direct_index=1,C_suspension_index=2,C_windblown_index=3,C_total_index=4)
    parameter (num_process=4)
    
    !Road meteorological data
    integer T_s_index,T_melt_index,r_aero_t_index,r_aero_t_notraffic_index,RH_s_index,RH_salt_final_index
    integer L_index,H_index,G_index,G_sub_index,evap_index,evap_pot_index
    integer rad_net_index,short_rad_net_index,long_rad_net_index,long_rad_out_index,H_traffic_index
    integer road_temperature_obs_index,road_wetness_obs_index,T_sub_index,short_rad_net_clearsky_index,T_s_dewpoint_index
    integer r_aero_q_index,r_aero_q_notraffic_index
    integer num_road_meteo
    integer E_corr_index
    integer E_diff_index
    parameter (T_s_index=1,T_melt_index=2,r_aero_t_index=3,r_aero_t_notraffic_index=4,RH_s_index=5,RH_salt_final_index=6)
    parameter (L_index=7,H_index=8,G_index=9,G_sub_index=10,evap_index=11,evap_pot_index=12)
    parameter (rad_net_index=13,short_rad_net_index=14,long_rad_net_index=15,long_rad_out_index=16,H_traffic_index=17)
    parameter (road_temperature_obs_index=18,road_wetness_obs_index=19,T_sub_index=20,short_rad_net_clearsky_index=21,T_s_dewpoint_index=22)
    parameter (r_aero_q_index=23,r_aero_q_notraffic_index=24)
    parameter (E_corr_index=25)
    parameter (E_diff_index=26)
    parameter (num_road_meteo=26)

    !Road moisture mass balance production and sink data
    integer S_melt_index,P_melt_index,P_freeze_index,S_freeze_index,P_evap_index
    integer S_evap_index,S_drainage_index,S_spray_index,R_spray_index,P_spray_index
    integer S_total_index,P_total_index,P_precip_index,P_roadwetting_index,S_drainage_tau_index
    integer R_drainage_index
    integer S_ploughing_index,R_ploughing_index
    integer num_moistbalance
    parameter (S_melt_index=1,P_melt_index=2,P_freeze_index=3,S_freeze_index=4,P_evap_index=5)
    parameter (S_evap_index=6,S_drainage_index=7,S_spray_index=8,R_spray_index=9,P_spray_index=10)
    parameter (S_total_index=11,P_total_index=12,P_precip_index=13,P_roadwetting_index=14,S_drainage_tau_index=15)
    parameter (R_drainage_index=16)
    parameter (S_ploughing_index=17,R_ploughing_index=18)
    parameter (num_moistbalance=18)

    !Date input parameter indexes
    integer year_index,month_index,day_index,hour_index,minute_index,datenum_index
    integer num_date_index
    parameter (year_index=1,month_index=2,day_index=3,hour_index=4,minute_index=5,datenum_index=6)
    parameter (num_date_index=6)
    
    !Set traffic input file indexes
    integer N_total_index,N_he_index,N_li_index
    integer N_st_he_index,N_wi_he_index,N_su_he_index
    integer N_st_li_index,N_wi_li_index,N_su_li_index
    integer V_he_index,V_li_index    
    integer num_traffic_index
    parameter (N_total_index=1,N_he_index=2,N_li_index=3)
    parameter (N_st_he_index=4,N_wi_he_index=5,N_su_he_index=6)
    parameter (N_st_li_index=7,N_wi_li_index=8,N_su_li_index=9)   
    parameter (V_he_index=10,V_li_index=11)
    parameter (num_traffic_index=11)
    integer N_v_index(num_veh)
    data (N_v_index(ii),ii=he,li) /N_he_index,N_li_index/
    integer N_t_v_index(num_tyre,num_veh)
    data ((N_t_v_index(ii,jj),ii=1,num_tyre),jj=1,num_veh) /N_st_he_index,N_wi_he_index,N_su_he_index,N_st_li_index,N_wi_li_index,N_su_li_index/ 
    integer V_veh_index(num_veh)
    data (V_veh_index(ii),ii=he,li) /V_he_index,V_li_index/ 
 
    !Set meteo input file indexes
    integer T_a_index,T2_a_index,FF_index,DD_index,RH_index,Rain_precip_index,Snow_precip_index
    integer short_rad_in_index,long_rad_in_index,short_rad_in_clearsky_index
    integer cloud_cover_index,road_temperature_obs_input_index,road_wetness_obs_input_index
    integer pressure_index,T_dewpoint_index,T_sub_input_index
    integer num_meteo_index
    parameter (T_a_index=1,T2_a_index=2,FF_index=3,DD_index=4,RH_index=5,Rain_precip_index=6,Snow_precip_index=7)
    parameter (short_rad_in_index=8,long_rad_in_index=9,short_rad_in_clearsky_index=10)
    parameter (cloud_cover_index=11,road_temperature_obs_input_index=12,road_wetness_obs_input_index=13)
    parameter (pressure_index=14,T_dewpoint_index=15,T_sub_input_index=16)
    parameter (num_meteo_index=16)

    !Set activity input file indexes
    integer M_sanding_index,t_ploughing_index,t_cleaning_index,g_road_wetting_index
    integer M_salting1_index,M_salting2_index,M_fugitive_index
    integer num_activity_index
    parameter (M_sanding_index=1,t_ploughing_index=2,t_cleaning_index=3,g_road_wetting_index=4)
    parameter (M_salting1_index=5,M_salting2_index=6,M_fugitive_index=7)
    parameter (num_activity_index=7)
    integer M_salting_index(2)
    data (M_salting_index(ii),ii=1,2) /M_salting1_index,M_salting2_index/
    
    !Set extra date indexes for the activity file since this is not necessarilly in chronological order 
    integer activity_year_index,activity_month_index,activity_day_index,activity_hour_index,activity_minute_index
    integer activity_roadID_index
    integer num_activity_input_index
    parameter (activity_year_index=8,activity_month_index=9,activity_day_index=10,activity_hour_index=11,activity_minute_index=12)
    parameter (activity_roadID_index=13)
    parameter (num_activity_input_index=13)
    
    !Set air quality input file indexes
    integer PM10_obs_index,PM10_bg_index,PM10_net_index
    integer PM25_obs_index,PM25_bg_index,PM25_net_index
    integer NOX_obs_index,NOX_bg_index,NOX_net_index
    integer NOX_emis_index,EP_emis_index,f_conc_index
    integer num_airquality_index
    parameter (PM10_obs_index=1,PM10_bg_index=2,PM10_net_index=3)
    parameter (PM25_obs_index=4,PM25_bg_index=5,PM25_net_index=6)
    parameter (NOX_obs_index=7,NOX_bg_index=8,NOX_net_index=9)
    parameter (NOX_emis_index=10,EP_emis_index=11,f_conc_index=12)
    parameter (num_airquality_index=12)
    integer PM_obs_index(num_size),PM_bg_index(num_size),PM_net_index(num_size)
    data (PM_obs_index(ii),ii=pm_10,pm_25) /PM10_obs_index,PM25_obs_index/
    data (PM_bg_index(ii),ii=pm_10,pm_25) /PM10_bg_index,PM25_bg_index/
    data (PM_net_index(ii),ii=pm_10,pm_25) /PM10_net_index,PM25_net_index/
    
    !Efficiency indexes
    integer ploughing_eff_index,cleaning_eff_index,drainage_eff_index,spraying_eff_index
    parameter (ploughing_eff_index=1,cleaning_eff_index=2,drainage_eff_index=3,spraying_eff_index=4)

    !Track indexes
    integer alltrack_type,outtrack_type,intrack_type,shoulder_type,kerb_type
    integer num_track_max
    parameter (alltrack_type=1,outtrack_type=2,intrack_type=3,shoulder_type=4,kerb_type=5)
    parameter (num_track_max=5)
 
    !Road type indexes
    integer normal_roadtype,tunnel_roadtype,bridge_roadtype,bicyclepath_roadtype,runway_roadtype,tunnelportal_roadtype
    parameter (normal_roadtype=1,tunnel_roadtype=2,bridge_roadtype=3,bicyclepath_roadtype=4,runway_roadtype=7,tunnelportal_roadtype=6)
    
    !Road type activity. Only in Fortran version since it is multiroad only
    integer road_type_salting_index,road_type_sanding_index,road_type_cleaning_index,road_type_ploughing_index,road_type_binding_index
    parameter (road_type_salting_index=1,road_type_sanding_index=2,road_type_cleaning_index=3,road_type_ploughing_index=4,road_type_binding_index=5)
    integer num_road_type_activity
    parameter (num_road_type_activity=5)   
    integer road_type_salt_index(2)
    data (road_type_salt_index(ii),ii=1,2) /road_type_salting_index,road_type_binding_index/

    !Set netcdf output file indexes (save_<var name>_index)
    integer,parameter :: save_T_surf_mod_index = 1
    integer,parameter :: save_T_sub_mod_index = 2
    integer,parameter :: save_T_freeze_mod_index = 3
    integer,parameter :: save_T_surf_meteo_index = 4
    integer,parameter :: save_T_air_index = 5
    integer,parameter :: save_Td_air_index = 6
    integer,parameter :: save_RH_air_index = 7
    integer,parameter :: save_Rain_index = 8
    integer,parameter :: save_Snow_index = 9
    integer,parameter :: save_Wind_FF_index = 10
    integer,parameter :: save_Wind_DD_index = 11
    integer,parameter :: save_SW_rad_cls_index = 12
    integer,parameter :: save_SW_rad_in_index = 13
    integer,parameter :: save_SW_rad_net_index = 14
    integer,parameter :: save_LW_rad_net_index = 15
    integer,parameter :: save_LW_rad_in_index = 16
    integer,parameter :: save_H_in_index = 17
    integer,parameter :: save_L_in_index = 18
    integer,parameter :: save_G_sub_index = 19
    integer,parameter :: save_G_net_index = 20
    integer,parameter :: save_Energy_correction_index = 21
    integer,parameter :: save_Energy_difference_index = 22
    integer,parameter :: save_W_surf_mod_index = 23
    integer,parameter :: save_I_surf_mod_index = 24
    integer,parameter :: save_S_surf_mod_index = 25
    integer, parameter :: save_PM10_Emissions_tot_index = 26
    integer, parameter :: save_PM25_Emissions_tot_index = 27
    integer, parameter :: save_PM10_Emissions_roadwear_index = 28
    integer, parameter :: save_PM10_Emissions_tyrewear_index = 29
    integer, parameter :: save_PM10_Emissions_brakewear_index = 30
    integer, parameter :: save_PM10_Emissions_sand_index = 31
    integer, parameter :: save_PM10_Emissions_fugitive_index = 32
    integer, parameter :: save_PM10_Emissions_exhaust_index = 33
    integer, parameter :: save_PM10_Emissions_salt1_index = 34
    integer, parameter :: save_PM10_Emissions_salt2_index = 35
    integer, parameter :: save_PM10_Emissions_direct_index = 36
    integer, parameter :: save_PM10_Emissions_suspension_index = 37
    integer, parameter :: save_PM10_Emissions_windblown_index = 38
    integer, parameter :: save_NOX_Emissions_tot_index = 39
    integer, parameter :: save_Traffic_index = 40
    integer, parameter :: save_HDV_index = 41
    integer, parameter :: save_Studs_li_index = 42
    integer, parameter :: save_Speed_li_index = 43
    integer, parameter :: save_Studs_he_index = 44
    integer, parameter :: save_Mass_dust_PM200_index = 45
    integer, parameter :: save_Mass_dust_PM10_index = 46
    integer, parameter :: save_Mass_dust_PM25_index = 47
    integer, parameter :: save_Mass_sand_PM200_index = 48
    integer, parameter :: save_Salt1_a_index = 49
    integer, parameter :: save_Salt2_a_index = 50
    integer, parameter :: save_Sand_a_index = 51
    integer, parameter :: save_Wetting_a_index = 52
    integer, parameter :: save_Ploughing_a_index = 53
    integer, parameter :: save_Cleaning_a_index = 54
    integer, parameter :: save_Mass_salt1_index = 55
    integer, parameter :: save_Mass_salt2_index = 56
    
    integer, parameter :: save_PM10_Concentration_tot_index = 57
    integer, parameter :: save_PM25_Concentration_tot_index = 58
    integer, parameter :: save_PM10_Concentration_exhaust_index = 59
    integer, parameter :: save_Mass_sand_PMall_index = 60
    integer, parameter :: save_Mass_sand_PM10_index = 61 
    integer, parameter :: save_Mass_cleaning_PMall_index = 62
    integer, parameter :: save_Mass_cleaning_PM200_index = 63
    integer, parameter :: save_Mass_cleaning_PM10_index = 64
    integer, parameter :: save_Mass_drainage_PM10_index = 65
    integer, parameter :: save_Mass_spraying_PM10_index = 66
    integer, parameter :: save_Mass_ploughing_PM10_index = 67
    integer, parameter :: save_f_q_index = 68

    integer, parameter :: save_Speed_li_avg_index = 69
    integer, parameter :: save_NOX_Emissions_sum_index = 70      
    integer, parameter :: save_PM10_Emissions_sum_index =  71   
    integer, parameter :: save_PM25_Emissions_sum_index =   72
    integer, parameter :: save_PM10_Emissions_road_sum_index =  73
    integer, parameter :: save_PM10_Emissions_tyre_sum_index =  74 
    integer, parameter :: save_PM10_Emissions_brake_sum_index = 75
    integer, parameter :: save_PM10_Emissions_sand_sum_index =  76
    integer, parameter :: save_PM10_Emissions_fugitive_sum_index = 77 
    integer, parameter :: save_PM10_Emissions_exhaust_sum_index =  78
    integer, parameter :: save_PM10_Emissions_salt1_sum_index = 79
    integer, parameter :: save_PM10_Emissions_salt2_sum_index = 80
    integer, parameter :: save_PM10_Emissions_direct_sum_index =  81
    integer, parameter :: save_PM10_Emissions_suspension_sum_index = 82 
    integer, parameter :: save_PM10_Emissions_windblown_sum_index = 83
    integer, parameter :: save_Mass_salt1_sum_index = 84
    integer, parameter :: save_Mass_salt2_sum_index = 85
    integer, parameter :: save_Mass_sand_PM200_sum_index = 86
    integer, parameter :: save_Mass_sand_PMall_sum_index = 87
    integer, parameter :: save_Mass_sand_PM10_sum_index = 88 
    integer, parameter :: save_Mass_cleaning_PMall_sum_index = 89
    integer, parameter :: save_Mass_cleaning_PM200_sum_index = 90
    integer, parameter :: save_Mass_cleaning_PM10_sum_index = 91 
    integer, parameter :: save_Mass_drain_PM10_sum_index = 92
    integer, parameter :: save_Mass_spray_PM10_sum_index = 93
    integer, parameter :: save_Mass_ploughing_PM10_sum_index = 94
    integer, parameter :: save_PM10_Wear_road_sum_index = 95
    integer, parameter :: save_PM10_Wear_tyre_sum_index = 96
    integer, parameter :: save_PM10_Wear_brake_sum_index =  97 
    integer, parameter :: save_Mass_dust_road_PM200_sum_index = 98
    integer, parameter :: save_Mass_dust_road_PM10_sum_index = 99
    integer, parameter :: save_Traffic_sum_index = 100
    integer, parameter :: save_st_he_sum_index = 101
    integer, parameter :: save_fr_hdv_sum_index = 102
    integer, parameter :: save_st_li_sum_index = 103
    integer, parameter :: save_AirportName_index = 104
    integer, parameter :: save_AirportICAO_index = 105
    integer, parameter :: save_RunwaySection_index = 106
    integer, parameter :: save_PhysicalRunway_index = 107


    !TODO: These can be put with the other indexes. 
    integer, parameter :: save_road_id_index = 1
    integer, parameter :: save_lat_index = 2
    integer, parameter :: save_lon_index = 3
    integer, parameter :: num_1d_index = 3
    
    end module NORTRIP_index_definitions    

!==========================================================================
!   NORTRIP model definitions
!==========================================================================
    module NORTRIP_definitions

    use NORTRIP_index_definitions
    
    implicit none
    
    type :: save_var !! Derived type used to save and write variables to netcdf output variables. 
        character(100) :: varname
        character(15) :: units
        character(256) :: long_name
        character(256) :: description 
        real, allocatable :: data_1d(:)
        character(20), allocatable :: data_char_1d(:)
        real, allocatable :: data_2d(:,:)
        logical :: save_in_summary =.false.
        logical :: save_in_emissions =.false.
        logical :: save_in_meteo =.false.
        logical :: save_in_activity =.false.
    end type

    type(save_var), dimension(110) :: save_vars !! Put all save variables in this array TODO: Could make this into an allocatable, to make length match the number of unique variables. 

    !Define the flag that determines if a single road option is to be used for large datasets
    logical :: use_single_road_loop_flag=.true.
    integer :: operating_system=1
    
    real :: nodata_activity=-999.
    real :: nodata=-999.
    real :: nodata_input=-999.
    real :: nodata_orig=-999.

    character(256) :: blank_str=''

    !Number of roads dependent on input data
    !n_roads_total is used as the actual number of roads in the input file
    !n_roads is the size of the internal loop
    integer :: n_roads=1
    integer :: n_roads_total=1
    integer :: n_roads_start=1
    integer :: n_roads_end=1
    
    !Number of tracks dependent on parameter file 
    integer :: num_track=1
    
    !The length of the input time data array
    integer :: n_time=1
    
    !Hours between saving initialisation files
    integer :: hours_between_init=0
    
    !Road surface temperature forecast hour
    integer :: forecast_hour=0
    integer :: forecast_type=1
    
    !Type of calculation indicator, controls some processes and outputs
    !Normal, Bedre Byluft, Forecast, Road weather, uEMEP
    character(256) :: calculation_type='normal'
    
!Declare string search arrays for input data
!-----------------------------------------------------------------------
    character(64) :: date_match_str(num_date_index)=''
    character(64) :: traffic_match_str(num_traffic_index)=''
    character(64) :: meteo_match_str(num_meteo_index)=''
    character(64) :: activity_match_str(num_activity_input_index)=''
    character(64) :: airquality_match_str(num_airquality_index)=''
    character(64) :: salt_match_str(num_salt_max)=''

!Declare logical for the availability of input data
!-----------------------------------------------------------------------
    logical :: available_date_data(num_date_index)=.false.
    logical :: available_traffic_data(num_traffic_index)=.false.
    logical :: available_meteo_data(num_meteo_index)=.false.
    logical :: available_activity_data(num_activity_input_index)=.false.
    logical :: available_airquality_data(num_airquality_index)=.false.

!Declare logicals for the saving routines. These are chosen based on the calculation_type
!-----------------------------------------------------------------------
    logical :: NORTRIP_save_init_data_flag=.false.
    logical :: NORTRIP_save_episode_emissions_flag=.false.
    logical :: NORTRIP_save_episode_grid_emissions_flag=.false.
    logical :: NORTRIP_save_road_meteo_data_flag=.false.
    logical :: NORTRIP_save_road_emission_and_mass_data_flag=.false.
    logical :: NORTRIP_save_road_emission_and_mass_data_stats_flag=.false.
    logical :: NORTRIP_save_road_summary_data_flag=.false.
    logical :: NORTRIP_save_all_data_flag=.false.
    logical :: NORTRIP_save_uEMEP_emissions_flag=.false.
    logical :: NORTRIP_save_uEMEP_grid_emissions_flag=.false.
    logical :: NORTRIP_save_road_emission_activity_data_flag=.false.

!Declare unit numbers for saving and writing data
!-----------------------------------------------------------------------
    integer :: unit_read_init_data=100
    integer :: unit_save_init_data=101
    integer :: unit_save_emissions=50
    integer :: unit_save_grid_emissions=60
    integer :: unit_save_road_meteo_data=70
    integer :: unit_save_road_emission_and_mass_data=71
    integer :: unit_save_road_emission_and_mass_data_stats=72
    integer :: unit_save_road_summary_data=73
    integer :: unit_save_road_activity_data=74
    integer :: unit_save_all_data=75
    integer :: unit_read_NORTRIP_inputdata=10
    integer :: unit_read_NORTRIP_parameters=11
    integer :: unit_read_NORTRIP_flags=12
    integer :: unit_read_NORTRIP_activities=13
    integer :: unit_read_NORTRIP_pathnames=14
    !Set logfile unit. Will be set to 0 (screen) if no log file name is specified
    integer :: unit_logfile=5
    

!File paths and names
!-----------------------------------------------------------------------
    character(256) path_inputparam
    character(256) path_inputdata
    character(256) path_outputdata
    character(256) path_outputfig
    character(256) filename_inputparam
    character(256) filename_inputdata
    character(256) filename_outputdata
    character(256) path_ospm
    character(256) path_fortran
    character(256) path_fortran_output
    character(256) filename_log
    character(256) path_init
    character(256) path_init_out
    character(256) filename_init
    character(256) filename_init_netcdf
    character(256) path_output_emis
    character(256) filename_output_emis
    character(256) filename_output_grid_emis
    character(256) path_output_roadmeteo
    character(256) filename_output_roadmeteo

    integer :: ncid_init !! Identifier for the init file that are being read.
    integer :: n_save_links_netcdf !! The number of roads for which output will be saved

!Model parameter variables
!-----------------------------------------------------------------------
    real W_0(num_wear,num_tyre,num_veh)
    real a_wear(num_wear,5)
    real s_roadwear_thresh
    integer num_pave,num_dc
    real h_pave(50),h_drivingcycle(50)
    character(64) h_pave_str(50),h_drivingcycle_str(50)
    real h_0_sus(num_source,num_size)
    real h_0_q_road(num_size)
    real f_0_suspension(num_source,num_size,num_tyre,num_veh)
    real a_sus(5)
    real h_0_abrasion(num_size),h_0_crushing(num_size)
    real p_0_abrasion(num_source),p_0_crushing(num_source)
    real f_0_abrasion(num_tyre,num_veh),f_0_crushing(num_tyre,num_veh)
    real V_ref_abrasion,V_ref_crushing
    real f_0_dir(num_source_all_extra)!Includes total, crushing and abrasion index
    real f_PM(num_source_all_extra,num_size,num_tyre)
    real f_PM_bin(num_source_all_extra,num_size,num_tyre)
    real V_ref_pm_fraction,c_pm_fraction
    real FF_thresh,tau_wind
    real h_eff(4,num_source,num_size)
    real w_dep(num_size)
    real conc_min,emis_min
    real R_0_spray(num_veh,num_moisture)
    real V_ref_spray(num_moisture),g_road_sprayable_min(num_moisture)
    real a_spray(num_moisture),V_thresh_spray(num_moisture)
    real g_road_drainable_min,snow_dust_drainage_retainment_limit,tau_road_drainage,g_road_drainable_thresh
    real h_ploughing_moisture(num_moisture),ploughing_thresh(num_moisture),ploughing_min_thresh(num_moisture)
    real g_road_evaporation_thresh
    real z0,z0t,z0q,z0m_in,z0t_in,z0q_in
    real albedo_snow
    real dzs
    real sub_surf_param(3)
    real sub_surf_average_time
    real a_traffic(num_veh)
    real H_veh(num_veh)
    real g_retention_thresh(num_source)
    real g_retention_min(num_source)
    real texture_scaling(5)
    real f_track(num_track_max)
    real veh_track(num_track_max)
    real mig_track(num_track_max)
    real track_type(num_track_max)

  
    !Model flags
    integer :: wear_flag(num_wear)
    data wear_flag /1,1,1/
    integer :: exhaust_flag=1   
    integer :: road_suspension_flag=1   
    integer :: dust_drainage_flag=2   
    integer :: dust_spray_flag=1   
    integer :: dust_ploughing_flag=1   
    integer :: abrasion_flag=0   
    integer :: crushing_flag=0   
    integer :: dust_deposition_flag=0   
    integer :: wind_suspension_flag=0   
    integer :: retention_flag=2   
    integer :: use_obs_retention_flag=0   
    integer :: water_spray_flag=2
    integer :: drainage_type_flag=2
    integer :: surface_humidity_flag=2   
    integer :: use_salt_humidity_flag=1   
    integer :: evaporation_flag=2   
    integer :: canyon_shadow_flag=1   
    integer :: canyon_long_rad_flag=1   
    integer :: use_subsurface_flag=1   
    integer :: use_traffic_turb_flag=1   
    integer :: use_ploughing_data_flag=1   
    integer :: use_cleaning_data_flag=1   
    integer :: use_wetting_data_flag=1   
    integer :: auto_salting_flag=0   
    integer :: auto_binding_flag=0   
    integer :: auto_sanding_flag=0   
    integer :: auto_ploughing_flag=1   
    integer :: auto_cleaning_flag=0   
    integer :: plot_type_flag=2   
    integer :: save_type_flag=1   
    integer :: save_road_summary_data_as_netcdf_flag=0 
    integer :: save_init_data_as_netcdf_flag=0  
    integer :: use_ospm_flag=0   
    integer :: activity_in_tunnels_flag=0
    integer :: use_melt_freeze_energy_flag=0
    integer :: salt_after_ploughing_flag=0
    integer :: use_stability_flag=1
    integer :: use_energy_correction_flag=0
    !Also used for scaling so set to real
    real use_salting_data_flag(2)
    real :: use_sanding_data_flag=1
    real :: retain_water_by_snow_flag=1
    
    real, allocatable :: save_1d_vars(:,:) !! For saving vars to netcdf summary file !TODO: Could be incorporated into save_vars. 

    !Auto activity data
    real :: salting_hour_ref(2)
    data salting_hour_ref /5.0,20.0/
    real :: delay_salting_day_ref=0.20 !        	(day)
    real :: check_salting_day_ref=0.50 !       	(day)
    real :: min_temp_salt_ref=-6.00 !      	(C)
    real :: max_temp_salt_ref=0.00 !       	(C)
    real :: precip_rule_salt_ref=0.10 !       	(mm/hr)
    real :: RH_rule_salt_ref=90.00 !      	(!)
    real :: g_salting_rule_ref=0.25 !       	(mm)
    real :: salt_mass_ref=3.00 !       	(g/m2)
    real :: salt_dilution_ref=0.20 !       	(g_water/(g_water+g_salt))
    real :: salt_type_distribution_ref=1.00 !       	M(salt)=M(NaCl)*salt_type+M(MgCl2)*(1-salt_type)
    
    real :: sanding_hour_ref(2)
    data sanding_hour_ref /5.0, 5.0/
    real :: delay_sanding_day_ref=0.90 !       	(day)
    real :: check_sanding_day_ref=0.50 !       	(day)
    real :: min_temp_sand_ref=-12.00 !     	(C)
    real :: max_temp_sand_ref=-4.00  !     	(C)                    
    real :: precip_rule_sand_ref=0.10 !       	(mm/hr)
    real :: RH_rule_sand_ref=95.00  !     	(%)
    real :: g_sanding_rule_ref=0.10 !       	(mm)
    real :: sand_mass_ref=250.00 !     	(g/m2)
    real :: sand_dilution_ref=0.00 !       	(g_water/(g_water+g_sand))
    
    real :: delay_ploughing_hour_ref=3.00 !       	(hr)
    real :: ploughing_thresh_2_ref=3.00 !       	(mm.w.e.)
    real :: ploughing_min_thresh_2_ref=0.1 !       	(mm.w.e.)

    real :: cleaning_hour_ref(2)
    data cleaning_hour_ref /5.0,20.0/
    real :: delay_cleaning_hour_ref=72.00 !      	(hr)
    real :: delay_cleaning_day_ref=3.00 !      	(day)
    real :: min_temp_cleaning_ref=0.00 !       	(C)
    integer :: clean_with_salting_ref=0 !       	                                                                
    real :: start_month_cleaning_ref=2.00 !       	(MM_num)
    real :: end_month_cleaning_ref=3.00 !       	(MM_num)
    real :: wetting_with_cleaning_ref=0.10 !       	(mm)
    real :: efficiency_of_cleaning_ref=1.0

    real :: binding_hour_ref(2)
    data binding_hour_ref /5.0,20.0/
    real :: delay_binding_day_ref=0.20 !        	(day)
    real :: check_binding_day_ref=0.50 !       	(day)
    real :: min_temp_binding_ref=-6.00 !      	(C)
    real :: max_temp_binding_ref=0.00 !       	(C)
    real :: precip_rule_binding_ref=0.10 !       	(mm/hr)
    real :: RH_rule_binding_ref=50.00 !      	(!)
    real :: g_binding_rule_ref=0.25 !       	(mm)
    real :: binding_mass_ref=3.00 !       	(g/m2)
    real :: binding_dilution_ref=0.20 !       	(g_water/(g_water+g_salt))
    real :: start_month_binding_ref=2.00 !       	(MM_num)
    real :: end_month_binding_ref=3.00 !       	(MM_num)

    real, allocatable :: salting_hour(:,:)
    real, allocatable :: delay_salting_day(:)
    real, allocatable :: check_salting_day(:)
    real, allocatable :: min_temp_salt(:) 
    real, allocatable :: max_temp_salt(:)
    real, allocatable :: precip_rule_salt(:)
    real, allocatable :: RH_rule_salt(:) 
    real, allocatable :: g_salting_rule(:)
    real, allocatable :: salt_mass(:) 
    real, allocatable :: salt_dilution(:) 
    real, allocatable :: salt_type_distribution(:) 
    
    real, allocatable :: sanding_hour(:,:)
    real, allocatable :: delay_sanding_day(:) 
    real, allocatable :: check_sanding_day(:)
    real, allocatable :: min_temp_sand(:) 
    real, allocatable :: max_temp_sand(:)
    real, allocatable :: precip_rule_sand(:)
    real, allocatable :: RH_rule_sand(:) 
    real, allocatable :: g_sanding_rule(:) 
    real, allocatable :: sand_mass(:) 
    real, allocatable :: sand_dilution(:)
    
    real, allocatable :: delay_ploughing_hour(:)
    real, allocatable :: ploughing_thresh_2(:) 
 
    real, allocatable :: cleaning_hour(:,:)
    real, allocatable :: delay_cleaning_day(:)
    real, allocatable :: min_temp_cleaning(:)
    integer, allocatable :: clean_with_salting(:)
    real, allocatable :: start_month_cleaning(:)
    real, allocatable :: end_month_cleaning(:)
    real, allocatable :: wetting_with_cleaning(:)
    real, allocatable :: efficiency_of_cleaning(:)

    real, allocatable :: binding_hour(:,:)
    real, allocatable :: delay_binding_day(:)
    real, allocatable :: check_binding_day(:)
    real, allocatable :: min_temp_binding(:)
    real, allocatable :: max_temp_binding(:)
    real, allocatable :: precip_rule_binding(:)
    real, allocatable :: RH_rule_binding(:)
    real, allocatable :: g_binding_rule(:)
    real, allocatable :: binding_mass(:)
    real, allocatable :: binding_dilution(:)
    real, allocatable :: start_month_binding(:)
    real, allocatable :: end_month_binding(:)
    
    logical :: read_auto_activity_data=.false.
    
!Road metadata variables
    
    
!Road initial condition offsets.
    
    real :: long_rad_in_offset=0.0
    real :: RH_offset=0.0
    real :: T_a_offset=0.0
    real :: P_fugitive=0.0

!Override parameters in the flag file, for overriding metadata values
    real :: override_long_rad_in_offset
    real :: override_RH_offset
    real :: override_T_a_offset
    real :: override_wind_speed_correction
    real :: override_albedo_road_offset

!Salt type defined in the input file header name
    integer salt_type(2)
    data salt_type(1),salt_type(2) /na,mg/
!Observed moisture unit defined in the input file header name
    integer :: road_wetness_obs_in_mm=0
    real :: max_road_wetness_obs
    real :: min_road_wetness_obs

!Input data variables
!-----------------------------------------------------------------------
    !Order is (date_type,time)
    integer, allocatable :: date_data(:,:)
    character(24), allocatable :: date_str(:,:)
    !Order is (variable_type,time,road)
    real, allocatable :: traffic_data(:,:,:)
    real, allocatable :: meteo_data(:,:,:)
    real, allocatable :: airquality_data(:,:,:)
    real, allocatable :: activity_data(:,:,:)
    real, allocatable :: activity_input_data(:,:,:)
    !Order is (source_type,size,time,track,road)
    real, allocatable :: M_road_data(:,:,:,:,:)  
    real, allocatable :: M_road_bin_data(:,:,:,:,:) !
    !Order is (source_type,size,variable_type,time,track,road)
    real, allocatable :: M_road_bin_balance_data(:,:,:,:,:,:)!
    real, allocatable :: M_road_balance_data(:,:,:,:,:,:)
    !Order is (source_type,size,process_type,time,track,road)
    !real, allocatable :: C_bin_data(:,:,:,:,:,:)!
    real, allocatable :: C_data(:,:,:,:,:,:)
    real, allocatable :: E_road_data(:,:,:,:,:,:)
    real, allocatable :: E_road_bin_data(:,:,:,:,:,:)!
    !Order is (wearsource_type,time,track,road)
    real, allocatable :: WR_time_data(:,:,:,:)
    !Order is (salt_variable_type,salt_type,time,track,road)
    real, allocatable :: road_salt_data(:,:,:,:,:)
    !Order is (variable_type,time,track,road)
    real, allocatable :: road_meteo_data(:,:,:,:)
    !Order is (moisture_type,process_type,time,track,road)
    real, allocatable :: g_road_balance_data(:,:,:,:,:)
    !Order is (moisture_type,time,track,road)
    real, allocatable :: g_road_data(:,:,:,:)
    !Order is (source_type,time,track,road)
    real, allocatable :: f_q(:,:,:,:)
    !Order is (time,track,road)
    real, allocatable :: f_q_obs(:,:,:)

    ! For reading airport information for the SmartKjemi project
    character(256), allocatable :: runway_char_info_data(:,:)
    real, allocatable :: runway_real_info_data(:,:)
    integer, allocatable :: runway_int_info_data(:,:)

    integer, parameter :: Airport_ICAO_index = 1
    integer, parameter :: Airport_name_index = 2
    integer, parameter :: Airport_section_index = 3
    integer, parameter :: Airport_PhysRunway_index = 4
    integer, parameter :: n_char_columns = 4
    
    integer, parameter :: Airport_section_station_ID_index = 1
    integer, parameter :: Airport_section_RoadID_index = 2
    integer, parameter :: n_int_columns = 2
    
    integer, parameter :: Airport_section_lon_index = 1
    integer, parameter :: Airport_section_lat_index = 2
    integer, parameter :: n_real_columns = 2

!Road initial conditions
!-----------------------------------------------------------------------
    !Initial conditions arrays (source_type,size,track,road)
    real, allocatable :: M_road_init(:,:,:,:)
    !Initial conditions arrays (moisture_type,track,road)
    real, allocatable :: g_road_init(:,:,:)

!Road metadata allocatable to the number of roads
!-----------------------------------------------------------------------
    integer, allocatable :: d_index(:)
    integer, allocatable :: p_index(:)
    real, allocatable :: b_road(:)
    integer, allocatable :: n_lanes(:)
    real, allocatable :: b_road_lanes(:)
    real, allocatable :: b_lane(:)
    real, allocatable :: b_canyon(:)
    real, allocatable :: h_canyon(:,:) !Two sides, north and south
    real, allocatable :: ang_road(:)
    real, allocatable :: slope_road(:)
    integer, allocatable :: roadtype_index(:)

    real, allocatable :: LAT(:)
    real, allocatable :: LON(:)
    real, allocatable :: Z_SURF(:)
    real, allocatable :: z_FF(:)
    real, allocatable :: z_T(:)
    real, allocatable :: z2_T(:)
    real, allocatable :: albedo_road(:)
    real, allocatable :: DIFUTC_H(:)
    real, allocatable :: Pressure(:)

    !Correction factors
    real, allocatable :: wind_speed_correction(:)
    real, allocatable :: h_sus(:)
    real, allocatable :: h_texture(:)
    
    !OSPM factors
    integer, allocatable :: choose_receptor_ospm(:)
    real, allocatable :: SL1_ospm(:)
    real, allocatable :: SL2_ospm(:)
    real, allocatable :: f_roof_ospm(:)
    real, allocatable :: RecHeight_ospm(:)
    real, allocatable :: f_turb_ospm(:)
   
    !Single factors
    !real, allocatable :: nodata(:) !Already declared
    real, allocatable :: observed_moisture_cutoff_value(:)

    !Emission factors (num_veh,n_road)
    real, allocatable :: exhaust_EF(:,:)
    real, allocatable :: NOX_EF(:,:)
    
    integer, allocatable :: road_ID(:)
    integer, allocatable :: save_road_data_flag(:)
    integer, allocatable :: line_or_grid_data_flag(:) !1 is line, 2 is grid, 3 is both line and grid
    
    real, allocatable :: x_road(:,:)
    real, allocatable :: y_road(:,:)
    !real, allocatable :: adt_road(:)
    real, allocatable :: length_road(:)
    real, allocatable :: lon_road(:,:)
    real, allocatable :: lat_road(:,:)
    
    !Special BB declaration for database ID. Only reads when type is 'Bedre Byluft'
    character(256) :: BB_output_ID(num_size)='{}'
    
    !Activity control flags allocatable to each road (road_type_activity_index,road)
    integer, allocatable :: road_type_activity_flag(:,:)
    
    !Specify availability of emission factors in the metadata file
    integer :: exhaust_EF_available=0
    integer :: NOX_EF_available=0

    !Grid data
    real :: grid_0(2)=0.
    real :: grid_delta(2)=0.
    integer :: grid_dim(2)=0
    !real :: grid_adt_cutoff(2)=0.
    logical :: grid_road_data_flag=.true.
    real, allocatable :: emis_grid(:,:,:,:)

!radiation parameters
!-----------------------------------------------------------------------
    !(time and road)
    real, allocatable :: azimuth_ang(:,:)
    real, allocatable :: zenith_ang(:,:)
    
!Skyview
!-----------------------------------------------------------------------
    integer :: n_skyview=0   !Number of skyview angles
    real, allocatable :: az_skyview(:,:)
    real, allocatable :: zen_skyview(:,:)

    
!Auto road maintenace activity variable (road)
!These are slightly different to the Matlab version due to the need of the init files
!-----------------------------------------------------------------------
    real, allocatable :: time_since_last_salting(:) !Days
    real, allocatable :: time_since_last_binding(:) !Days
    real, allocatable :: time_since_last_sanding(:) !Days
    real, allocatable :: time_since_last_cleaning(:) !Hours
    real, allocatable :: time_since_last_ploughing(:) !Hours
    
!loop variables commonly used
!-----------------------------------------------------------------------
    integer x,s,t,v,i,j,k,m
    integer ro,ti,tr,tf
    integer ro_tot

!bin dimmension variables to retain same structure as before but reduce memory
!-----------------------------------------------------------------------
    integer ti_bin,ro_bin
    parameter (ti_bin=1,ro_bin=1)
    
!Shared time variables
!-----------------------------------------------------------------------
    integer min_time,max_time
    integer max_time_inputdata
    integer min_time_save,max_time_save
    real dt
    character(24) :: date_format_str='dd.mm.yyyy HH:MM'
    character(24) :: start_date_str=''
    character(24) :: end_date_str=''
    character(24) :: start_date_save_str=''
    character(24) :: end_date_save_str=''
    character(24) :: min_date_str=''
    character(24) :: max_date_str=''
    
!Command line filename for initial path definition
!-----------------------------------------------------------------------
    character(256) commandline_filename
    
!Control variables
!-----------------------------------------------------------------------
    character(256) :: message_str=''

    logical :: read_metadata_in_zip_format=.false.
    logical :: read_timeseriesdata_in_zip_format=.false.
    logical :: read_initialdata_in_zip_format=.false.
    logical :: save_initdata_in_zip_format=.false.  !Not implemented
    
    character(1) :: slash='\'
    character(256) :: delete_file_command='del /f'
    character(1024) :: command_line_zip=''
    
    character(256) :: finished_file_append=''
    character(256) :: finished_filename=''
    
    end module NORTRIP_definitions

!==========================================================================
!   NORTRIP_salt_constants
!==========================================================================
    module NORTRIP_salt_constants
    !Constants set in regard to salt

    use NORTRIP_index_definitions

    implicit none
    
    !Set constants for NaCl, MgCl2 and CMA
    !All constants for cma are guesses (not quite some data available)
    real M_atomic_water
    real M_atomic(num_salt_max)
    real saturated(num_salt_max)

    real RH_saturated(num_salt_max)
    real a_antoine_ice,b_antoine_ice,c_antoine_ice
    real a_antoine(num_salt_max)
    real b_antoine(num_salt_max)
    real c_antoine(num_salt_max)
    real melt_temperature_saturated(num_salt_max)
    real melt_temperature_oversaturated(num_salt_max)
    real f_salt_sat(num_salt_max)
    real over_saturated(num_salt_max)
    real RH_over_saturated(num_salt_max)
    real vp_correction(num_salt_max)
    real RH_over_saturated_fraction(num_salt_max)
    real salt_power
    real salt_power_val(num_salt_max)

    contains
    
    subroutine set_salt_parameters
    
    implicit none
        
    M_atomic_water=18.015
    
    M_atomic(na)=58.4
    M_atomic(mg)=95.2
    M_atomic(cma)=124
    M_atomic(ca)=111 !(g/mol)
    M_atomic(pfo)=84 !(g/mol)

    !Saturated ratio
    saturated(na)=0.086
    saturated(mg)=0.050
    saturated(cma)=0.066
    saturated(ca)=0.065
    saturated(pfo)=0.165
    !Not used
    RH_saturated(na)=75
    RH_saturated(mg)=33
    RH_saturated(cma)=40
    RH_saturated(ca)=31
    RH_saturated(pfo)=60

    !Antoine constants
    !Two alternatives 
    a_antoine_ice=10.3
    b_antoine_ice=2600
    c_antoine_ice=270
    !a_antoine_ice=13.9;b_antoine_ice=4655;c_antoine_ice=352;
    a_antoine(mg)=7.20
    b_antoine(mg)=1581.00
    c_antoine(mg)=225.00
    a_antoine(na)=7.40
    b_antoine(na)=1566.00
    c_antoine(na)=228.00
    a_antoine(cma)=7.28
    b_antoine(cma)=1581.00
    c_antoine(cma)=225.00
    a_antoine(ca)=5.8
    b_antoine(ca)=1087.00
    c_antoine(ca)=198.00
    !Guess for Potassium Formate. slightly rescaled water to get 60% vp depression at saturation
    a_antoine(pfo)=10.3*0.975
    b_antoine(pfo)=2600
    c_antoine(pfo)=270

    !Saturated melt/freezing temperatures
    melt_temperature_saturated(na)=-21
    melt_temperature_saturated(mg)=-33
    melt_temperature_saturated(cma)=-27.5
    melt_temperature_saturated(ca)=-51
    melt_temperature_saturated(pfo)=-51
    !These three paramters approximate the over saturated curve. Needs updating
    !melt_temperature_oversaturated(na)=0melt_temperature_oversaturated(mg)=-15melt_temperature_oversaturated(cma)=-7
    !Have set the saturated melt temperature higher than in the paper to emable melt
    !in saturated conditions which would not occur other wise. value half way. 
    melt_temperature_oversaturated(na)=-1
    melt_temperature_oversaturated(mg)=-15
    melt_temperature_oversaturated(cma)=-12
    melt_temperature_oversaturated(ca)=-1
    melt_temperature_oversaturated(pfo)=-25
    !melt_temperature_oversaturated(na)=melt_temperature_saturated(na)
    !melt_temperature_oversaturated(mg)=melt_temperature_saturated(mg)
    !melt_temperature_oversaturated(cma)=melt_temperature_saturated(cma)
    !melt_temperature_oversaturated(na)=-1

    !f_salt_sat cannot be set to 1.0
    f_salt_sat(na)=1.17
    f_salt_sat(mg)=1.5
    f_salt_sat(cma)=1.5
    f_salt_sat(ca)=1.4
    f_salt_sat(pfo)=1.5
    over_saturated(na)=f_salt_sat(na)*saturated(na)
    over_saturated(mg)=f_salt_sat(mg)*saturated(mg)
    over_saturated(cma)=f_salt_sat(cma)*saturated(cma)
    over_saturated(ca)=f_salt_sat(ca)*saturated(ca)
    over_saturated(pfo)=f_salt_sat(pfo)*saturated(pfo)
    !RH_over_saturated is not used, RH_over_saturated_fraction is used instead
    !RH_over_saturated(na)=100
    !RH_over_saturated(mg)=70
    !RH_over_saturated(cma)=85
    !RH_over_saturated(cma)=70
    !Set the interpolation power and corrections
    !salt_power=1.5
    !salt power index changed 24.04.2023 to a value closer to measured values. Based on Avinor work
    salt_power=1.2
    salt_power_val(na)=1.3
    salt_power_val(mg)=1.3
    salt_power_val(cma)=1.2
    salt_power_val(ca)=1.6
    salt_power_val(pfo)=1.2


    vp_correction(na)=0.035
    vp_correction(mg)=0.11
    vp_correction(cma)=0.17
    vp_correction(ca)=0.001
    vp_correction(pfo)=0.012

    !Set the fractiona distribution of oversaturated solution
    !RH_over_saturated_fraction(na)=0.1
    !RH_over_saturated_fraction(mg)=0.75
    !RH_over_saturated_fraction(cma)=0.75
    !RH_over_saturated_fraction(ca)=0.75
    RH_over_saturated_fraction(na)=0.25
    RH_over_saturated_fraction(mg)=0.99
    RH_over_saturated_fraction(cma)=0.99
    RH_over_saturated_fraction(ca)=0.99
    RH_over_saturated_fraction(pfo)=0.99
        
    end subroutine set_salt_parameters

    
    end module NORTRIP_salt_constants
    
!==========================================================================
!   set_constant_string_values
!==========================================================================
    subroutine set_constant_string_values

    use NORTRIP_definitions

    date_match_str(year_index)='Year'
    date_match_str(month_index)='Month'
    date_match_str(day_index)='Day'
    date_match_str(hour_index)='Hour'
    date_match_str(minute_index)='Minute'
    
    meteo_match_str(T_a_index)='T2m'
    meteo_match_str(T2_a_index)='T25m'
    meteo_match_str(FF_index)='FF'
    meteo_match_str(DD_index)='DD'
    meteo_match_str(RH_index)='RH'
    meteo_match_str(Rain_precip_index)='Rain'
    meteo_match_str(Snow_precip_index)='Snow'
    meteo_match_str(short_rad_in_index)='Global radiation'
    meteo_match_str(cloud_cover_index)='Cloud cover'
    meteo_match_str(road_temperature_obs_input_index)='Road surface temperature'
    meteo_match_str(road_wetness_obs_input_index)='Road wetness'
    meteo_match_str(long_rad_in_index)='Longwave radiation'
    meteo_match_str(short_rad_in_clearsky_index)='Global clearsky radiation'   
    meteo_match_str(T_sub_input_index)='T_sub'
    meteo_match_str(pressure_index)='Pressure'
    meteo_match_str(T_dewpoint_index)='T2m dewpoint'

    traffic_match_str(N_total_index)='N(total)'
    traffic_match_str(N_li_index)='N(li)'
    traffic_match_str(N_he_index)='N(he)'
    traffic_match_str(N_st_he_index)='N(st,he)'
    traffic_match_str(N_wi_he_index)='N(wi,he)'
    traffic_match_str(N_su_he_index)='N(su,he)'
    traffic_match_str(N_st_li_index)='N(st,li)'
    traffic_match_str(N_wi_li_index)='N(wi,li)'
    traffic_match_str(N_su_li_index)='N(su,li)'
    traffic_match_str(V_he_index)='V_veh(he)'
    traffic_match_str(V_li_index)='V_veh(li)'

    activity_match_str(M_sanding_index)='M_sanding'
    activity_match_str(M_salting_index)='M_salting'
    activity_match_str(t_ploughing_index)='Ploughing_road'
    activity_match_str(t_cleaning_index)='Cleaning_road'
    activity_match_str(g_road_wetting_index)='Wetting'
    activity_match_str(M_fugitive_index)='M_fugitive'
    activity_match_str(activity_year_index)='Year'
    activity_match_str(activity_month_index)='Month'
    activity_match_str(activity_day_index)='Day'
    activity_match_str(activity_hour_index)='Hour'
    activity_match_str(activity_minute_index)='Minute'
    activity_match_str(activity_roadID_index)='Road_ID'

    salt_match_str(na)='(na)'
    salt_match_str(mg)='(mg)'
    salt_match_str(cma)='(cma)'
    salt_match_str(ca)='(ca)'
    
    airquality_match_str(PM_obs_index(pm_10))='PM10_obs'
    airquality_match_str(PM_obs_index(pm_25))='PM25_obs'
    airquality_match_str(NOX_obs_index)='NOX_obs'
    airquality_match_str(PM_bg_index(pm_10))='PM10_background'
    airquality_match_str(PM_bg_index(pm_25))='PM25_background'
    airquality_match_str(NOX_bg_index)='NOX_background'
    airquality_match_str(PM_net_index(pm_10))='PM10_net'
    airquality_match_str(PM_net_index(pm_25))='PM25_net'
    airquality_match_str(NOX_net_index)='NOX_net'
    airquality_match_str(NOX_emis_index)='NOX_emis'
    airquality_match_str(EP_emis_index)='EP_emis'
    airquality_match_str(f_conc_index)='Disp_fac'
    
    if (operating_system.eq.1) then
        slash='\'
        delete_file_command='del /f'
    endif
    
    if (operating_system.eq.2) then
        slash='/'
        delete_file_command='rm -f'
    endif
    
    !Preset this here
    ploughing_min_thresh=ploughing_min_thresh_2_ref
    
    end subroutine set_constant_string_values

!==========================================================================
!   allocate_NORTRIP_arrays
!==========================================================================
    subroutine allocate_NORTRIP_arrays

    use NORTRIP_definitions
 
    implicit none
    
    !real memory_size
    
    !Allocates all the arrays used in NORTRIP calculations except the input data
    !Input data is allocated in the input data routine 'read_NORTRIP_inputdata'
    !-----------------------------------------------------------------------
    
    !Order is (date_type,time)
    !allocate (date_data(num_date_data,n_time))
    !Order is (variable_type,time,road)
    !allocate (traffic_data(num_traffic_data,n_time,0:n_roads))
    !allocate (meteo_data(num_meteo_data,n_time,0:n_roads))
    !allocate (airquality_data(num_airquality_data,n_time,0:n_roads))
    !allocate (activity_data(num_activity_data,n_time,0:n_roads))
    !Order is (source_type,size,time,track,road)
    allocate (M_road_data(num_source_all,num_size,n_time,num_track,0:n_roads))
    allocate (M_road_bin_data(num_source_all,num_size,ti_bin,num_track,ro_bin))
    !Order is (source_type,size,variable_type,time,track,road)
    allocate (M_road_bin_balance_data(num_source_all,num_size,num_dustbalance,ti_bin,num_track,ro_bin))
    allocate (M_road_balance_data(num_source_all,num_size,num_dustbalance,n_time,num_track,0:n_roads))
    !Order is (source_type,size,process_type,time,track,road)
    !allocate (C_bin_data(num_source_all,num_size,num_process,ti_bin,num_track,ro_bin))
    allocate (C_data(num_source_all,num_size,num_process,n_time,num_track,0:n_roads))
    allocate (E_road_data(num_source_all,num_size,num_process,n_time,num_track,0:n_roads))
    allocate (E_road_bin_data(num_source_all,num_size,num_process,ti_bin,num_track,ro_bin))
    !Order is (wearsource_type,time,track,road)
    allocate (WR_time_data(num_wear,n_time,num_track,0:n_roads))
    !Order is (salt_variable_type,salt_type,time,track,road)
    allocate (road_salt_data(num_saltdata,num_salt,n_time,num_track,0:n_roads))
    !Order is (variable_type,time,track,road)
    allocate (road_meteo_data(num_road_meteo,n_time,num_track,0:n_roads))
    !Order is (moisture_type,process_type,time,track,road)
    allocate (g_road_balance_data(num_moisture,num_moistbalance,n_time,num_track,0:n_roads))
    !Order is (moisture_type,time,track,road)
    allocate (g_road_data(num_moisture,n_time,num_track,0:n_roads))
    !Order is (source_type,time,track,road)
    allocate (f_q(num_source_all,n_time,num_track,0:n_roads))
    !Order is (time,track,road)
    allocate (f_q_obs(n_time,num_track,0:n_roads))

    !Radiation parameters
    allocate (azimuth_ang(n_time,0:n_roads))
    allocate (zenith_ang(n_time,0:n_roads))
    
    !write(*,'(a)')'Storage size of arrays in mega bytes'
    !write(*,'(a,f12.2)') 'M_road_data ',real(storage_size(M_road_data)*size(M_road_data)/8./1000000.)
    !write(*,'(a,f12.2)') 'M_road_bin_data ',real(storage_size(M_road_bin_data)*size(M_road_bin_data)/8./1000000.)
    !write(*,'(a,f12.2)') 'M_road_bin_balance_data ',real(storage_size(M_road_bin_balance_data)*size(M_road_bin_balance_data)/8./1000000.)
    !write(*,'(a,f12.2)') 'C_data ',real(storage_size(C_data)/8.)*real(size(C_data)/1000000.)
    !write(*,'(a,f12.2)') 'E_road_data ',real(storage_size(E_road_data)/8.)*real(size(E_road_data)/1000000.)
    !write(*,'(a,f12.2)') 'E_road_bin_data ',real(storage_size(E_road_bin_data)*size(E_road_bin_data)/8./1000000.)
    !write(*,'(a,f12.2)') 'WR_time_data ',real(storage_size(WR_time_data)*size(WR_time_data)/8./1000000.)
    !write(*,'(a,f12.2)') 'road_salt_data ',real(storage_size(road_salt_data)*size(road_salt_data)/8./1000000.)
    !write(*,'(a,f12.2)') 'road_meteo_data ',real(storage_size(road_meteo_data)*size(road_meteo_data)/8./1000000.)
    !write(*,'(a,f12.2)') 'g_road_balance_data ',real(storage_size(g_road_balance_data)*size(g_road_balance_data)/8./1000000.)
    !write(*,'(a,f12.2)') 'f_q ',real(storage_size(f_q)*size(f_q)/8./1000000.)
    !write(*,'(a,f12.2)') 'f_q_obs ',real(storage_size(f_q_obs)*size(f_q_obs)/8./1000000.)
    !write(*,'(a,f12.2)') 'traffic_data ',real(storage_size(traffic_data)*size(traffic_data)/8./1000000.)
    !write(*,'(a,f12.2)') 'meteo_data ',real(storage_size(meteo_data)*size(meteo_data)/8./1000000.)
    !write(*,'(a,f12.2)') 'airquality_data ',real(storage_size(airquality_data)*size(airquality_data)/8./1000000.)
    !write(*,'(a,f12.2)') 'activity_data ',real(storage_size(activity_data)*size(activity_data)/8./1000000.)
    !write(*,'(a,f12.2)') 'activity_input_data ',real(storage_size(activity_input_data)*size(activity_input_data)/8./1000000.)
    !write(*,'(a,f12.2)') 'M_road_init ',real(storage_size(M_road_init)*size(M_road_init)/8./1000000.)
    !write(*,'(a,f12.2)') 'g_road_init ',real(storage_size(g_road_init)*size(g_road_init)/8./1000000.)
    !write(*,'(a,f12.2)') 'Others,approx ',real(storage_size(slope_road)*size(slope_road)/8./1000000.)*40.
    !write(*,'(a,f12.2)') 'azimuth_ang ',real(storage_size(azimuth_ang)*size(azimuth_ang)/8./1000000.)
    !write(*,'(a,f12.2)') 'zenith_ang ',real(storage_size(zenith_ang)*size(zenith_ang)/8./1000000.)
       
   ! memory_size=real(storage_size(M_road_data)*size(M_road_data)/8./1000000.) + &
   !     real(storage_size(M_road_bin_data)*size(M_road_bin_data)/8./1000000.) + &
   !     real(storage_size(M_road_bin_balance_data)*size(M_road_bin_balance_data)/8./1000000.) + &
   !     real(storage_size(C_data)/8.)*real(size(C_data)/1000000.) + &
   !     real(storage_size(E_road_data)/8.)*real(size(E_road_data)/1000000.) + &
   !     real(storage_size(E_road_bin_data)*size(E_road_bin_data)/8./1000000.) + &
   !     real(storage_size(WR_time_data)*size(WR_time_data)/8./1000000.) + &
   !     real(storage_size(road_salt_data)*size(road_salt_data)/8./1000000.) + &
   !     real(storage_size(road_meteo_data)*size(road_meteo_data)/8./1000000.) + &
   !     real(storage_size(g_road_balance_data)*size(g_road_balance_data)/8./1000000.) + &
   !     real(storage_size(f_q)*size(f_q)/8./1000000.) + &
   !     real(storage_size(f_q_obs)*size(f_q_obs)/8./1000000.) + &
   !     real(storage_size(traffic_data)*size(traffic_data)/8./1000000.) + &
   !     real(storage_size(meteo_data)*size(meteo_data)/8./1000000.) + &
   !     real(storage_size(airquality_data)*size(airquality_data)/8./1000000.) + &
   !     real(storage_size(activity_data)*size(activity_data)/8./1000000.) + &
   !     real(storage_size(activity_input_data)*size(activity_input_data)/8./1000000.) + &
   !     real(storage_size(M_road_init)*size(M_road_init)/8./1000000.) + &
   !     real(storage_size(g_road_init)*size(g_road_init)/8./1000000.) + &
   !     real(storage_size(slope_road)*size(slope_road)/8./1000000.)*40. + &
   !     real(storage_size(azimuth_ang)*size(azimuth_ang)/8./1000000.) + &
   !     real(storage_size(zenith_ang)*size(zenith_ang)/8./1000000.)
    
    !Times 2 because RAM in 64 bit is actually double and long anyway
    !write(*,'(a,f12.3)')'Estimated total storage size of arrays in gigabytes: ',memory_size/1000.*2.

    !Auto road maintenace
    allocate (time_since_last_salting(0:n_roads))
    allocate (time_since_last_binding(0:n_roads))
    allocate (time_since_last_sanding(0:n_roads))
    allocate (time_since_last_cleaning(0:n_roads))
    allocate (time_since_last_ploughing(0:n_roads))

        
    !Initialise all arrays to 0
    M_road_data=0.0
    M_road_bin_data=0.0
    M_road_bin_balance_data=0.0
    M_road_balance_data=0.0
    C_data=0.0
    E_road_data=0.0
    E_road_bin_data=0.0
    WR_time_data=0.0
    road_salt_data=0.0
    road_meteo_data=0.0
    g_road_balance_data=0.0
    g_road_data=0.0
    f_q=0.0
    f_q_obs=0.0
    
    azimuth_ang=0.0
    zenith_ang=0.0

    
    end subroutine allocate_NORTRIP_arrays

!==========================================================================
!   allocate_NORTRIP_arrays
!==========================================================================
    subroutine allocate_NORTRIP_save_arrays !TODO: Maybe change name on this (define types or something?)
    !! Define types that contain information needed to save data in netcdf files. 

    use NORTRIP_definitions
    implicit none
    !TODO: Now the conditions for allocation of the data array member is hard coded to match the "save_in_*" property (E.g. (save_in_summary = .true., save_in_meteo = .true.) means that space is allocated if (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag)). The condition to allocate should probably be more flexible.  
    save_vars(save_T_surf_mod_index) = save_var(varname = "T_surf_mod",units = "Celsius", description = "Modeled surface temperature", long_name = "surface_temperature", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_T_surf_mod_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_T_surf_mod_index)%data_2d(n_save_links_netcdf,n_time))  

    save_vars(save_T_sub_mod_index) = save_var(varname = "T_sub_mod",units = "Celsius", description = "Modeled subsurface temperature",  long_name = "sub_surface_temperature_temperature", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_T_sub_mod_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_T_sub_mod_index)%data_2d(n_save_links_netcdf,n_time))  
    
    save_vars(save_T_freeze_mod_index) = save_var(varname = "T_freeze_mod",units = "Celsius", description = "Freezing point temperature",  long_name = "freezing_point_temperature", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_T_freeze_mod_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_T_freeze_mod_index)%data_2d(n_save_links_netcdf,n_time))  
    
    save_vars(save_T_surf_meteo_index) = save_var(varname = "T_surf_meteo",units = "Celsius", description = "Observed surface temperature, otherwise nodata value",  long_name = "surface_temperature", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_T_surf_meteo_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_T_surf_meteo_index)%data_2d(n_save_links_netcdf,n_time))  
    
    save_vars(save_T_air_index) = save_var(varname = "T_air",units = "Celsius", description = "Air temperature at 2 m above ground",  long_name = "air_temperature", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_T_air_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_T_air_index)%data_2d(n_save_links_netcdf,n_time))  

    save_vars(save_Td_air_index) = save_var(varname = "Td_air",units = "Celsius", description = "Dew point temperature at 2 m above ground, calc. from relative humidity (RH).",  long_name = "dew_point_temperature", save_in_meteo = .true., save_in_summary = .true.)
    if(.not.allocated(save_vars(save_Td_air_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_Td_air_index)%data_2d(n_save_links_netcdf,n_time))  
    
    save_vars(save_RH_air_index) = save_var(varname = "RH_air",units = "percent", description = "Relative humidity at 2 m above ground",  long_name = "relative_humidity", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_RH_air_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_RH_air_index)%data_2d(n_save_links_netcdf,n_time))  

    save_vars(save_Rain_index) = save_var(varname = "Rain",units = "mm", description = "Amount of liquid precipitation within the model time step",  long_name = "rainfall_amount", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_Rain_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_Rain_index)%data_2d(n_save_links_netcdf,n_time))  

    save_vars(save_Snow_index) = save_var(varname = "Snow",units = "mm", description = "Amount of solid precipitation within the model time step",  long_name = "snowfall_amount", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_Snow_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag ))allocate(save_vars(save_Snow_index)%data_2d(n_save_links_netcdf,n_time))  
    
    save_vars(save_Wind_FF_index) = save_var(varname = "Wind_FF",units = "m/s", description = "Wind speed at 10 m above ground",  long_name = "wind_speed", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_Wind_FF_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_Wind_FF_index)%data_2d(n_save_links_netcdf,n_time))  

    save_vars(save_Wind_DD_index) = save_var(varname = "Wind_DD",units = "degree", description = "Wind from direction at 10 m above ground",  long_name = "wind_from_direction", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_Wind_DD_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_Wind_DD_index)%data_2d(n_save_links_netcdf,n_time))  
    
    save_vars(save_SW_rad_cls_index) = save_var(varname = "SW_rad_cls",units = "W/m2", description = "Clear sky incoming short wave radiation",  long_name = "downwelling_shortwave_flux_in_air_assuming_clear_sky", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_SW_rad_cls_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_SW_rad_cls_index)%data_2d(n_save_links_netcdf,n_time))  

    save_vars(save_SW_rad_in_index) = save_var(varname = "SW_rad_in",units = "W/m2", description = "Incoming short wave radiation",  long_name = "downwelling_shortwave_flux_in_air", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_SW_rad_in_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_SW_rad_in_index)%data_2d(n_save_links_netcdf,n_time))  

    save_vars(save_SW_rad_net_index) = save_var(varname = "SW_rad_net",units = "W/m2", description = "Net short wave radiation",  long_name = "net_downward_shortwave_flux_in_air", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_SW_rad_net_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_SW_rad_net_index)%data_2d(n_save_links_netcdf,n_time))  

    save_vars(save_LW_rad_net_index) = save_var(varname = "LW_rad_net",units = "W/m2", description = "Net long wave radiation",  long_name = "net_downward_longwave_flux_in_air", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_LW_rad_net_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_LW_rad_net_index)%data_2d(n_save_links_netcdf,n_time))  

    save_vars(save_LW_rad_in_index) = save_var(varname = "LW_rad_in",units = "W/m2", description = "Incoming long wave radiation",  long_name = "downwelling_longwave_flux_in_air", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_LW_rad_in_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_LW_rad_in_index)%data_2d(n_save_links_netcdf,n_time))  

    save_vars(save_H_in_index) = save_var(varname = "H_in",units = "W/m2", description = "Surface sensible heat flux, positive downwards",  long_name = "surface_downward_sensible_heat_flux", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_H_in_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_H_in_index)%data_2d(n_save_links_netcdf,n_time)) 
    
    save_vars(save_L_in_index) = save_var(varname = "L_in",units = "W/m2", description = "Surface latent heat flux, positive downwards",  long_name = "surface_downward_latent_heat_flux", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_L_in_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_L_in_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_G_sub_index) = save_var(varname = "G_sub",units = "W/m2", description = "Sub-surface energy flux",  long_name = "sub_surface_energy_flux", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_G_sub_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_G_sub_index)%data_2d(n_save_links_netcdf,n_time)) 
    
    save_vars(save_G_net_index) = save_var(varname = "G_net",units = "W/m2", description = "Surface energy flux",  long_name = "surface_energy_flux", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_G_net_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_G_net_index)%data_2d(n_save_links_netcdf,n_time)) 
    
    save_vars(save_Energy_correction_index) = save_var(varname = "Energy_correction",units = "W/m2", description = "Energy correction term used in surface energy balance",  long_name = "surface_energy_flux", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_Energy_correction_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag .and. use_energy_correction_flag.eq.1)) allocate(save_vars(save_Energy_correction_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_Energy_difference_index) = save_var(varname = "Energy_difference",units = "W/m2", description = "Energy difference needed to match observed surface temperature",  long_name = "energy_difference_to_match_observed_surface_temperature", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_Energy_difference_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag .and. use_energy_correction_flag.eq.1)) allocate(save_vars(save_Energy_difference_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_W_surf_mod_index) = save_var(varname = "W_surf_mod",units = "mm", description = "Water mass on the road surface (water equivalent)",  long_name = "liquid_water_on_surface", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_W_surf_mod_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag)) allocate(save_vars(save_W_surf_mod_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_I_surf_mod_index) = save_var(varname = "I_surf_mod",units = "mm", description = "Ice mass on the road surface (water equivalent)",  long_name = "ice_on_surface_as_water_equivalent", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_I_surf_mod_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag)) allocate(save_vars(save_I_surf_mod_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_S_surf_mod_index) = save_var(varname = "S_surf_mod",units = "mm", description = "Snow mass on the road surface (water equivalent)",  long_name = "snow_on_surface_as_water_equivalent", save_in_summary = .true., save_in_meteo = .true.)
    if(.not.allocated(save_vars(save_S_surf_mod_index)%data_2d) .and. (NORTRIP_save_road_meteo_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_S_surf_mod_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_PM10_Emissions_tot_index) = save_var(varname = "PM10_Emissions_tot",units = "g/km/h", description = "Total non-exhaust emissions of PM10",  long_name = "pm10_emissions_total", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_tot_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_PM10_Emissions_tot_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_PM25_Emissions_tot_index) = save_var(varname = "PM25_Emissions_tot",units = "g/km/h", description = "Total non-exhaust emissions of PM2.5",  long_name = "pm25_emissions_total", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_PM25_Emissions_tot_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_PM25_Emissions_tot_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_PM10_Emissions_roadwear_index) = save_var(varname = "PM10_Emissions_roadwear",units = "g/km/h", description = "Emissions of PM10 from roadwear",  long_name = "pm10_emissions_roadwear", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_roadwear_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_PM10_Emissions_roadwear_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_PM10_Emissions_tyrewear_index) = save_var(varname = "PM10_Emissions_tyrewear",units = "g/km/h", description = "Emissions of PM10 from tyrewear",  long_name = "pm10_emissions_tyrewear", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_tyrewear_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_PM10_Emissions_tyrewear_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_PM10_Emissions_brakewear_index) = save_var(varname = "PM10_Emissions_brakewear",units = "g/km/h", description = "Emissions of PM10 from brakewear",  long_name = "pm10_emissions_brakewear", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_brakewear_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_PM10_Emissions_brakewear_index)%data_2d(n_save_links_netcdf,n_time)) 
    
    save_vars(save_PM10_Emissions_sand_index) = save_var(varname = "PM10_Emissions_sand",units = "g/km/h", description = "Emissions of PM10 from sanding",  long_name = "pm10_emissions_sanding", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_sand_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_PM10_Emissions_sand_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_PM10_Emissions_fugitive_index) = save_var(varname = "PM10_Emissions_fugitive",units = "g/km/h", description = "Fugitive emissions of PM10",  long_name = "pm10_emissions_fugitive", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_fugitive_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_PM10_Emissions_fugitive_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_PM10_Emissions_exhaust_index) = save_var(varname = "PM10_Emissions_exhaust",units = "g/km/h", description = "Emissions of PM10 from exhaust",  long_name = "pm10_emissions_exhaust", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_exhaust_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_PM10_Emissions_exhaust_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_PM10_Emissions_salt1_index) = save_var(varname = "PM10_Emissions_salt1",units = "g/km/h", description = "Emissions of PM10 from salt1",  long_name = "pm10_emissions_salt1", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_salt1_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_PM10_Emissions_salt1_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_PM10_Emissions_salt2_index) = save_var(varname = "PM10_Emissions_salt2",units = "g/km/h", description = "Emissions of PM10 from salt2",  long_name = "pm10_emissions_salt2", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_salt2_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_PM10_Emissions_salt2_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_PM10_Emissions_direct_index) = save_var(varname = "PM10_Emissions_direct",units = "g/km/h", description = "Direct emissions of PM10",  long_name = "pm10_emissions_direct", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_direct_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_PM10_Emissions_direct_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_PM10_Emissions_suspension_index) = save_var(varname = "PM10_Emissions_suspension",units = "g/km/h", description = "Emissions of PM10 from suspension",  long_name = "pm10_emissions_suspension", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_suspension_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_PM10_Emissions_suspension_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_PM10_Emissions_windblown_index) = save_var(varname = "PM10_Emissions_windblown",units = "g/km/h", description = "Windblown emissions of PM10",  long_name = "pm10_emissions_windblown", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_windblown_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_PM10_Emissions_windblown_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_NOX_Emissions_tot_index) = save_var(varname = "NOX_Emissions_tot",units = "g/km/h", description = "Total emissions of NOX",  long_name = "nox_emissions_total", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_NOX_Emissions_tot_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_NOX_Emissions_tot_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_Traffic_index) = save_var(varname = "Traffic",units = "veh", description = "Vehicles per hour",  long_name = "traffic", save_in_summary = .true., save_in_emissions = .true.,save_in_activity = .true.)
    if(.not.allocated(save_vars(save_Traffic_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_Traffic_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_HDV_index) = save_var(varname = "HDV",units = "veh", description = "Percentage of heavy duty vehicles",  long_name = "percentage_heavy_duty_vehicles", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_HDV_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_HDV_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_Studs_li_index) = save_var(varname = "Studs_li",units = "%", description = "Percentage of light vehicles with studded tyres",  long_name = "percentage_studs_light_vehicles", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_Studs_li_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_Studs_li_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_Studs_he_index) = save_var(varname = "Studs_he",units = "%", description = "Percentage of heavy vehicles with studded tyres",  long_name = "percentage_studs_heavy_vehicles", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_Studs_he_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_Studs_he_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_Speed_li_index) = save_var(varname = "Speed_li",units = "km/h", description = "Speed of light vehicles",  long_name = "speed_light_vehicles", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_Speed_li_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_Speed_li_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_Mass_dust_PM200_index) = save_var(varname = "Mass_dust_PM200",units = "g/m2", description = "Mass of non-suspendable dust (>200 micrometer) on road",  long_name = "mass_pm200_dust", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_Mass_dust_PM200_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_Mass_dust_PM200_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_Mass_dust_PM10_index) = save_var(varname = "Mass_dust_PM10",units = "g/m2", description = "Mass of PM10 particles on road",  long_name = "mass_pm10_dust", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_Mass_dust_PM10_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_Mass_dust_PM10_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_Mass_dust_PM25_index) = save_var(varname = "Mass_dust_PM25",units = "g/m2", description = "Mass of PM25 particles on road",  long_name = "mass_pm25_dust", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_Mass_dust_PM25_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_Mass_dust_PM25_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_Mass_sand_PM200_index) = save_var(varname = "Mass_sand_PM200",units = "g/m2", description = "Mass of non-suspendable sand (>200 micrometer) on road",  long_name = "mass_sand_pm200", save_in_summary = .true., save_in_emissions = .true.)
    if(.not.allocated(save_vars(save_Mass_sand_PM200_index)%data_2d) .and. (NORTRIP_save_road_emission_and_mass_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_Mass_sand_PM200_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_Salt1_a_index) = save_var(varname = "Salt1_a",units = "g/m2", description = "Total mass of NaCl salt applied in the time step",  long_name = "applied_salt1", save_in_summary = .true., save_in_activity = .true.)
    if(.not.allocated(save_vars(save_Salt1_a_index)%data_2d) .and. (NORTRIP_save_road_emission_activity_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_Salt1_a_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_Salt2_a_index) = save_var(varname = "Salt2_a",units = "g/m2", description = "Total mass of alternative salt applied in the time step",  long_name = "applied_salt2", save_in_summary = .true., save_in_activity = .true.) !TODO: In timestep or pr. hour?
    if(.not.allocated(save_vars(save_Salt2_a_index)%data_2d) .and. (NORTRIP_save_road_emission_activity_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_Salt2_a_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_Sand_a_index) = save_var(varname = "Sand_a",units = "g/m2", description = "Total mass of sand applied in the time step",  long_name = "applied_sand", save_in_summary = .true., save_in_activity = .true.)
    if(.not.allocated(save_vars(save_Sand_a_index)%data_2d) .and. (NORTRIP_save_road_emission_activity_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_Sand_a_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_Wetting_a_index) = save_var(varname = "Wetting_a",units = "mm", description = "Water added to the road during cleaning or salting",  long_name = "applied_water_on_road", save_in_summary = .true., save_in_activity = .true.) 
    if(.not.allocated(save_vars(save_Wetting_a_index)%data_2d) .and. (NORTRIP_save_road_emission_activity_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_Wetting_a_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_Ploughing_a_index) = save_var(varname = "Ploughing_a",units = "1", description = "Snow ploughing event in time step (0 to 1)",  long_name = "plowing_event", save_in_summary = .true., save_in_activity = .true.)
    if(.not.allocated(save_vars(save_Ploughing_a_index)%data_2d) .and. (NORTRIP_save_road_emission_activity_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_Ploughing_a_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_Cleaning_a_index) = save_var(varname = "Cleaning_a",units = "1", description = "Road cleaning event in time step (0 to 1). Value denote max. cleaning efficiency",  long_name = "cleaning_event", save_in_summary = .true., save_in_activity = .true.)
    if(.not.allocated(save_vars(save_Cleaning_a_index)%data_2d) .and. (NORTRIP_save_road_emission_activity_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_Cleaning_a_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_Mass_salt1_index) = save_var(varname = "Mass_salt1",units = "g/m2", description = "Mass of NaCl on the road",  long_name = "Mass_of_NaCl_on_road", save_in_summary = .true., save_in_activity = .true.)
    if(.not.allocated(save_vars(save_Mass_salt1_index)%data_2d) .and. (NORTRIP_save_road_emission_activity_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_Mass_salt1_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_Mass_salt2_index) = save_var(varname = "Mass_salt2",units = "g/m2", description = "Mass of alternative salt on the road",  long_name = "Mass_of_alternative_salt_on_road", save_in_summary = .true., save_in_activity = .true.)
    if(.not.allocated(save_vars(save_Mass_salt2_index)%data_2d) .and. (NORTRIP_save_road_emission_activity_data_flag .or. NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_Mass_salt2_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_f_q_index) = save_var(varname = "f_q",units = "1", description = "Surface retainment factor (0-1) based on the surface moisture. All is retained when value is zero.",  long_name = "surface_retainment_factor", save_in_summary = .true.)
    if(.not.allocated(save_vars(save_f_q_index)%data_2d) .and. ( NORTRIP_save_road_summary_data_flag )) allocate(save_vars(save_f_q_index)%data_2d(n_save_links_netcdf,n_time)) 

    save_vars(save_Speed_li_avg_index) = save_var(varname = "Speed_li_avg", units = "km/h", description = "Average light vehicle speed over the simulation period", long_name = "average_speed_light_vehicles", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_Speed_li_avg_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_Speed_li_avg_index)%data_1d(n_save_links_netcdf))    

    save_vars(save_NOX_Emissions_sum_index) = save_var(varname = "NOX_Emissions_sum", units = "g", description = "Grams of NOX emitted on the road over the period", long_name = "nox_emissions_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_NOX_Emissions_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_NOX_Emissions_sum_index)%data_1d(n_save_links_netcdf))     

    save_vars(save_PM10_Emissions_sum_index) = save_var(varname = "PM10_Emissions_sum", units = "g", description = "Grams of PM10 emitted over the period", long_name = "pm10_emissions_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_PM10_Emissions_sum_index)%data_1d(n_save_links_netcdf))   

    save_vars(save_PM25_Emissions_sum_index) = save_var(varname = "PM25_Emissions_sum", units = "g", description = "Grams of PM25 emitted over the period", long_name = "pm25_emissions_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_PM25_Emissions_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_PM25_Emissions_sum_index)%data_1d(n_save_links_netcdf))   

    save_vars(save_PM10_Emissions_road_sum_index) = save_var(varname = "PM10_Emissions_road_sum", units = "g", description = "Grams of emitted PM10 from roadwear over the period", long_name = "pm10_emissions_road_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_road_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_PM10_Emissions_road_sum_index)%data_1d(n_save_links_netcdf))    

    save_vars(save_PM10_Emissions_tyre_sum_index) = save_var(varname = "PM10_Emissions_tyre_sum", units = "g", description = "Grams of emitted PM10 from tyrewear over the period", long_name = "pm10_emissions_tyre_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_tyre_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_PM10_Emissions_tyre_sum_index)%data_1d(n_save_links_netcdf))    

    save_vars(save_PM10_Emissions_brake_sum_index) = save_var(varname = "PM10_Emissions_brake_sum", units = "g", description = "Grams of emitted PM10 from brakewear over the period", long_name = "pm10_emissions_brake_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_brake_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_PM10_Emissions_brake_sum_index)%data_1d(n_save_links_netcdf))    

    save_vars(save_PM10_Emissions_sand_sum_index) = save_var(varname = "PM10_Emissions_sand_sum", units = "g", description = "Grams of emitted PM10 from sand over the period", long_name = "pm10_emissions_sand_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_sand_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_PM10_Emissions_sand_sum_index)%data_1d(n_save_links_netcdf))    

    save_vars(save_PM10_Emissions_fugitive_sum_index) = save_var(varname = "PM10_Emissions_fugitive_sum", units = "g", description = "Grams of fugitive emissions of PM10 over the period", long_name = "pm10_emissions_fugitive_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_fugitive_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_PM10_Emissions_fugitive_sum_index)%data_1d(n_save_links_netcdf))

    save_vars(save_PM10_Emissions_exhaust_sum_index) = save_var(varname = "PM10_Emissions_exhaust_sum", units = "g", description = "Grams of exhaust emissions of PM10 over the period", long_name = "pm10_emissions_exhaust_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_exhaust_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_PM10_Emissions_exhaust_sum_index)%data_1d(n_save_links_netcdf))    
    
    save_vars(save_PM10_Emissions_salt1_sum_index) = save_var(varname = "PM10_Emissions_salt1_sum", units = "g", description = "Grams of NaCl in PM10 emitted over the period", long_name = "pm10_emissions_salt1_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_salt1_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_PM10_Emissions_salt1_sum_index)%data_1d(n_save_links_netcdf))    

    save_vars(save_PM10_Emissions_salt2_sum_index) = save_var(varname = "PM10_Emissions_salt2_sum", units = "g", description = "Grams of alternative salt in PM10 emitted over the period", long_name = "pm10_emissions_salt2_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_salt2_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_PM10_Emissions_salt2_sum_index)%data_1d(n_save_links_netcdf))    

    save_vars(save_PM10_Emissions_direct_sum_index) = save_var(varname = "PM10_Emissions_direct_sum", units = "g", description = "Grams of direct PM10 emissions over the period", long_name = "pm10_emissions_direct_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_direct_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_PM10_Emissions_direct_sum_index)%data_1d(n_save_links_netcdf))    

    save_vars(save_PM10_Emissions_suspension_sum_index) = save_var(varname = "PM10_Emissions_suspension_sum", units = "g", description = "Grams of PM10 emissions from suspension over the period", long_name = "pm10_emissions_suspension_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_suspension_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_PM10_Emissions_suspension_sum_index)%data_1d(n_save_links_netcdf))     

    save_vars(save_PM10_Emissions_windblown_sum_index) = save_var(varname = "PM10_Emissions_windblown_sum", units = "g", description = "Grams of PM10 windblown emissions over the period", long_name = "pm10_emissions_windblown_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_PM10_Emissions_windblown_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_PM10_Emissions_windblown_sum_index)%data_1d(n_save_links_netcdf)) 

    save_vars(save_Mass_salt1_sum_index) = save_var(varname = "Mass_salt1_sum", units = "g", description = "Grams of applied NaCl over the period", long_name = "mass_applied_salt1_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_Mass_salt1_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_Mass_salt1_sum_index)%data_1d(n_save_links_netcdf))  
    
    save_vars(save_Mass_salt2_sum_index) = save_var(varname = "Mass_salt2_sum", units = "g", description = "Grams of applied alternative salt over the period", long_name = "mass_applied_salt2_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_Mass_salt2_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_Mass_salt2_sum_index)%data_1d(n_save_links_netcdf))  

    save_vars(save_Mass_sand_PM200_sum_index) = save_var(varname = "Mass_sand_PM200_sum", units = "g", description = "Grams of applied sand of size PM200 over the period", long_name = "mass_sand_pm200_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_Mass_sand_PM200_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_Mass_sand_PM200_sum_index)%data_1d(n_save_links_netcdf))    

    save_vars(save_Mass_sand_PMall_sum_index) = save_var(varname = "Mass_sand_PMall_sum", units = "g", description = "Grams of applied sand of all sized over the period", long_name = "mass_sand_pmall_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_Mass_sand_PMall_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_Mass_sand_PMall_sum_index)%data_1d(n_save_links_netcdf))    

    save_vars(save_Mass_sand_PM200_sum_index) = save_var(varname = "Mass_sand_PM200_sum", units = "g", description = "Grams of applied sand of size PM200 over the period", long_name = "mass_sand_pm10_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_Mass_sand_PM200_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_Mass_sand_PM200_sum_index)%data_1d(n_save_links_netcdf))     

    save_vars(save_Mass_cleaning_PMall_sum_index) = save_var(varname = "Mass_cleaning_PMall_sum", units = "g", description = "", long_name = "mass_cleaning_pmall_sum", save_in_activity = .true.) 
    if(.not.allocated(save_vars(save_Mass_cleaning_PMall_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_Mass_cleaning_PMall_sum_index)%data_1d(n_save_links_netcdf))    

    save_vars(save_Mass_cleaning_PM200_sum_index) = save_var(varname = "Mass_cleaning_PM200_sum", units = "g", description = "", long_name = "mass_cleaning_pm200_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_Mass_cleaning_PM200_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_Mass_cleaning_PM200_sum_index)%data_1d(n_save_links_netcdf))    

    save_vars(save_Mass_cleaning_PM10_sum_index) = save_var(varname = "Mass_cleaning_PM10_sum", units = "g", description = "", long_name = "mass_cleaning_pm10_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_Mass_cleaning_PM10_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_Mass_cleaning_PM10_sum_index)%data_1d(n_save_links_netcdf))     

    save_vars(save_Mass_drain_PM10_sum_index) = save_var(varname = "Mass_drain_PM10_sum", units = "g", description = "", long_name = "mass_drainage_pm10_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_Mass_drain_PM10_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_Mass_drain_PM10_sum_index)%data_1d(n_save_links_netcdf))    

    save_vars(save_Mass_spray_PM10_sum_index) = save_var(varname = "Mass_spray_PM10_sum", units = "g", description = "", long_name = "mass_spray_pm10_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_Mass_spray_PM10_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_Mass_spray_PM10_sum_index)%data_1d(n_save_links_netcdf))    

    save_vars(save_Mass_ploughing_PM10_sum_index) = save_var(varname = "Mass_ploughing_PM10_sum", units = "g", description = "", long_name = "mass_ploughing_pm10_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_Mass_ploughing_PM10_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_Mass_ploughing_PM10_sum_index)%data_1d(n_save_links_netcdf))    

    save_vars(save_PM10_Wear_road_sum_index) = save_var(varname = "PM10_Wear_road_sum", units = "g", description = "", long_name = "pm10_wear_road_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_PM10_Wear_road_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_PM10_Wear_road_sum_index)%data_1d(n_save_links_netcdf))    

    save_vars(save_PM10_Wear_tyre_sum_index) = save_var(varname = "PM10_Wear_tyre_sum", units = "g", description = "", long_name = "pm10_wear_tyre_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_PM10_Wear_tyre_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_PM10_Wear_tyre_sum_index)%data_1d(n_save_links_netcdf))   

    save_vars(save_PM10_Wear_brake_sum_index) = save_var(varname = "PM10_Wear_brake_sum", units = "g", description = "", long_name = "pm10_wear_brake_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_PM10_Wear_brake_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_PM10_Wear_brake_sum_index)%data_1d(n_save_links_netcdf))   

    save_vars(save_Mass_dust_road_PM200_sum_index) = save_var(varname = "Mass_dust_road_PM200_sum", units = "g", description = "", long_name = "mass_dust_road_pm200_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_Mass_dust_road_PM200_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_Mass_dust_road_PM200_sum_index)%data_1d(n_save_links_netcdf))   

    save_vars(save_Mass_dust_road_PM10_sum_index) = save_var(varname = "Mass_dust_road_PM10_sum", units = "g", description = "", long_name = "mass_dust_road_pm10_sum", save_in_activity = .true.)
    if(.not.allocated(save_vars(save_Mass_dust_road_PM10_sum_index)%data_1d) .and. (NORTRIP_save_road_emission_activity_data_flag)) allocate(save_vars(save_Mass_dust_road_PM10_sum_index)%data_1d(n_save_links_netcdf))    

    save_vars(save_AirportName_index) = save_var(varname = "AirportName", units = "", description = "Name of the airport", long_name = "airport_name", save_in_summary = .true.)
    if(.not.allocated(save_vars(save_AirportName_index)%data_char_1d) .and. (NORTRIP_save_road_summary_data_flag) .and. calculation_type == "Avinor") allocate(save_vars(save_AirportName_index)%data_char_1d(n_save_links_netcdf)) 

    save_vars(save_AirportICAO_index) = save_var(varname = "AirportICAO", units = "", description = "ICAO code for airport", long_name = "airport_ICAO_code", save_in_summary = .true.)
    if(.not.allocated(save_vars(save_AirportICAO_index)%data_char_1d) .and. (NORTRIP_save_road_summary_data_flag) .and. calculation_type == "Avinor") allocate(save_vars(save_AirportICAO_index)%data_char_1d(n_save_links_netcdf)) 

    save_vars(save_RunwaySection_index) = save_var(varname = "RunwaySection", units = "", description = "Runway section", long_name = "runway_section", save_in_summary = .true.)
    if(.not.allocated(save_vars(save_RunwaySection_index)%data_char_1d) .and. (NORTRIP_save_road_summary_data_flag) .and. calculation_type == "Avinor") allocate(save_vars(save_RunwaySection_index)%data_char_1d(n_save_links_netcdf)) 

    save_vars(save_PhysicalRunway_index) = save_var(varname = "PhysicalRunway", units = "", description = "Identifies runway", long_name = "airport_name", save_in_summary = .true.)
    if(.not.allocated(save_vars(save_PhysicalRunway_index)%data_char_1d) .and. (NORTRIP_save_road_summary_data_flag) .and. calculation_type == "Avinor") allocate(save_vars(save_PhysicalRunway_index)%data_char_1d(n_save_links_netcdf)) 

    

    end subroutine allocate_NORTRIP_save_arrays
    
!==========================================================================
!   deallocate_NORTRIP_arrays
!==========================================================================
    subroutine deallocate_NORTRIP_arrays

    use NORTRIP_definitions
 
    implicit none
    
    !Allocates all the arrays used in NORTRIP calculations except the input data
    !Input data is allocated in the input data routine 'read_NORTRIP_inputdata'
    !-----------------------------------------------------------------------

    !Order is (date_type,time)
    if (allocated(date_data)) deallocate (date_data)
    if (allocated(date_str)) deallocate (date_str)
    !Order is (variable_type,time,road)
    if (allocated(traffic_data)) deallocate (traffic_data)
    if (allocated(meteo_data)) deallocate (meteo_data)
    if (allocated(airquality_data)) deallocate (airquality_data)
    if (allocated(activity_data)) deallocate (activity_data)
    if (allocated(activity_input_data)) deallocate (activity_input_data)
    !Order is (source_type,size,time,track,road)
    if (allocated(M_road_data)) deallocate (M_road_data)
    if (allocated(M_road_bin_data)) deallocate (M_road_bin_data)
    !Order is (source_type,size,variable_type,time,track,road)
    if (allocated(M_road_bin_balance_data)) deallocate (M_road_bin_balance_data)
    if (allocated(M_road_balance_data)) deallocate (M_road_balance_data)
    !Order is (source_type,size,process_type,time,track,road)
    !deallocate (C_bin_data)
    if (allocated(C_data)) deallocate (C_data)
    if (allocated(E_road_data)) deallocate (E_road_data)
    if (allocated(E_road_bin_data)) deallocate (E_road_bin_data)
    !Order is (wearsource_type,time,track,road)
    if (allocated(WR_time_data)) deallocate (WR_time_data)
    !Order is (salt_variable_type,salt_type,time,track,road)
    if (allocated(road_salt_data)) deallocate (road_salt_data)
    !Order is (variable_type,time,track,road)
    if (allocated(road_meteo_data)) deallocate (road_meteo_data)
    !Order is (moisture_type,process_type,time,track,road)
    if (allocated(g_road_balance_data)) deallocate (g_road_balance_data)
    !Order is (moisture_type,time,track,road)
    if (allocated(g_road_data)) deallocate (g_road_data)
    !Order is (source_type,time,track,road)
    if (allocated(f_q)) deallocate (f_q)
    !Order is (time,track,road)
    if (allocated(f_q_obs)) deallocate (f_q_obs)

    if (allocated(M_road_init)) deallocate (M_road_init)
    if (allocated(g_road_init)) deallocate (g_road_init)

    if (allocated(azimuth_ang)) deallocate (azimuth_ang)
    if (allocated(zenith_ang)) deallocate (zenith_ang)

    if (allocated(emis_grid)) deallocate (emis_grid)

    !if (allocated(save_vars)) deallocate (save_vars)
    if (allocated(save_1d_vars)) deallocate(save_1d_vars)
    
    end subroutine deallocate_NORTRIP_arrays