


######## %---- TOPCAT definition ----% ########

# .%%%%%%%%..%%%%%%%..%%%%%%%%...%%%%%%.....%%%....%%%%%%%%
# ....%%....%%.....%%.%%.....%%.%%....%%...%%.%%......%%...
# ....%%....%%.....%%.%%.....%%.%%........%%...%%.....%%...
# ....%%....%%.....%%.%%%%%%%%..%%.......%%.....%%....%%...
# ....%%....%%.....%%.%%........%%.......%%%%%%%%%....%%...
# ....%%....%%.....%%.%%........%%....%%.%%.....%%....%%...
# ....%%.....%%%%%%%..%%.........%%%%%%..%%.....%%....%%...


## Page 9 of TOPCAT protocol here: https://biolincc.nhlbi.nih.gov/media/studies/topcat/Protocols.pdf
## Does not yet use recommended methods for AF
# Parameters: e' med and lateral, E/A ratio, MV deceleration time


assign_diastolic_function_topcat <- 
  function(dat,
           pid="patientid",
           vst="VISIT",
           ep_lat="eprime_lat_val",
           ep_sept="eprime_med_val",
           earat="ea_ratio_val",
           dt="decel_time_val",
           verbose=F,
           complete_cases=T)
  {

    # TOPCAT: e' sept/lat/abnl; E/A ratio; MV decel time (ms)
  
  print(dat[,c(pid,
               vst,
               ep_lat,
               ep_sept,
               earat,
               dt)])  
    
  dat$eprime_abnl[dat[,ep_sept] < 8|dat[,ep_lat]<10] <- 1
  dat$eprime_abnl[dat[,ep_sept] >= 8|dat[,ep_lat] >= 10] <- 0



   if (complete_cases==T) {

    dat_complete <- dat[complete.cases(dat[,c("eprime_abnl",earat,dt)]),
               c(pid,vst,"eprime_abnl",earat,dt,ep_lat,ep_sept)]

  }

  

dat_complete$topcat_dd_num[dat_complete$eprime_abnl==0&!is.na(dat_complete$eprime_abnl)] <- 1

dat_complete$topcat_dd_num[dat_complete[,earat]<=0.8&
                             dat_complete$eprime_abnl==1&
      !is.na(dat_complete$eprime_abnl)&
      !is.na(dat_complete[,earat])] <- 2

dat_complete$topcat_dd_num[dat_complete[,earat]>0.8&
                             dat_complete[,earat]<=1.5&
                             dat_complete$eprime_abnl==1&
      !is.na(dat_complete$eprime_abnl)&
      !is.na(dat_complete[,earat])] <- 3

dat_complete$topcat_dd_num[dat_complete[,earat]>1.5&
                             dat_complete$eprime_abnl==1&
      !is.na(dat_complete$eprime_abnl)&
      !is.na(dat_complete[,earat])] <- 4

dat_complete$topcat_dd_num[dat_complete[,dt]<160&
                             dat_complete$eprime_abnl==1&
      is.na(dat_complete[,earat])&
              !is.na(dat_complete$eprime_abnl)&
              !is.na(dat_complete[,dt])] <- 4




dat_complete[dat_complete$topcat_dd_num==1&!is.na(dat_complete$topcat_dd_num),"topcat_dd_txt"] <- "Normal"
dat_complete[dat_complete$topcat_dd_num==2&!is.na(dat_complete$topcat_dd_num),"topcat_dd_txt"] <- "Grade I"
dat_complete[dat_complete$topcat_dd_num==3&!is.na(dat_complete$topcat_dd_num),"topcat_dd_txt"] <- "Grade II"
dat_complete[dat_complete$topcat_dd_num==4&!is.na(dat_complete$topcat_dd_num),"topcat_dd_txt"] <- "Grade III"

dat_complete$topcat_dd_txt <- factor(dat_complete$topcat_dd_txt,
                             levels=c("Normal",
                                      "Grade I",
                                      "Grade II",
                                      "Grade III"))


if (verbose == T) {
return(dat_complete[,c(pid,vst,earat,ep_lat,ep_sept,dt,
              "topcat_dd_num","topcat_dd_txt")])
    }

if (verbose == F) {
  return(dat_complete[,c(pid,vst, "topcat_dd_num","topcat_dd_txt")])
}

  
  }



######## %---- ASE 2016 definition ----% ########


# ....%%%.....%%%%%%..%%%%%%%%.....%%%%%%%....%%%%%......%%....%%%%%%%.
# ...%%.%%...%%....%%.%%..........%%.....%%..%%...%%...%%%%...%%.....%%
# ..%%...%%..%%.......%%.................%%.%%.....%%....%%...%%.......
# .%%.....%%..%%%%%%..%%%%%%.......%%%%%%%..%%.....%%....%%...%%%%%%%%.
# .%%%%%%%%%.......%%.%%..........%%........%%.....%%....%%...%%.....%%
# .%%.....%%.%%....%%.%%..........%%.........%%...%%.....%%...%%.....%%
# .%%.....%%..%%%%%%..%%%%%%%%....%%%%%%%%%...%%%%%....%%%%%%..%%%%%%%.


## Nagueh SF, Smiseth OA, Appleton CP, et al. Recommendations for the Evaluation of Left Ventricular Diastolic Function by Echocardiography:
## An Update from the American Society of Echocardiography and the European Association of Cardiovascular Imaging. J Am Soc Echocardiogr 2016;29:277–314.
# Parameters: E/e' septal vs.E/e' lateral vs. E/e' avg; E/A ratio; TR velocity (m/s); e' septal; e' lateral; E velocity; LA volume index



assign_diastolic_function_ase2016 <- 
  function(dat,                                             
           pid="patientid",
           vst="VISIT",
           ep_lat="eprime_lat_val",
           ep_sept="eprime_med_val",
           earat="ea_ratio_val",
           tr_vel = "tr_ms",
           e_vel="ewave_val",
           lavix="lav_ix_val",
           laa="laa_val",
           eep_sep = "eeprime_med_val",
           eep_lat="eeprime_lat_val",
           verbose=F,
           complete_cases=T)
{
    
    dat$eep_avg <- apply(dat[,c(eep_sep,eep_lat)],MARGIN = 1,FUN=mean)
    
    dat[!is.na(dat[,ep_lat])|!is.na(dat[,ep_lat]),"eprime_abnormal"] <- 0
    
    dat[dat[,ep_lat] < 10&!is.na(dat[,ep_lat]),"eprime_abnormal"] <- 1
    dat[dat[,ep_sept] < 7&!is.na(dat[,ep_sept]),"eprime_abnormal"] <- 1
    
    dat[!is.na(dat[,lavix])|!is.na(dat[,laa]),"la_abnl"] <- 0
    dat[dat[,laa]>20&!is.na(dat[,laa]),"la_abnl"] <- 1
    dat[dat[,lavix]>34&!is.na(dat[,lavix]),"la_abnl"] <- 1
   
    if (complete_cases==T) {
      dat <- dat[complete.cases(dat[,c("eprime_abnormal","la_abnl","eep_avg",earat,tr_vel,e_vel)]),]
    }
    
    ### ---- Normal vs. abnormal diastolic function --- ###

    dat[,'ase2016_crit1'] <- 0

    dat[dat[,"eep_avg"]>14&!is.na(dat[,"eep_avg"]),'ase2016_crit1'] <-
      dat[dat[,"eep_avg"]>14&!is.na(dat[,"eep_avg"]),'ase2016_crit1']+1

    dat[dat$eprime_abnormal==1&!is.na(dat$eprime_abnormal),'ase2016_crit1'] <-
      dat[dat$eprime_abnormal==1&!is.na(dat$eprime_abnormal),'ase2016_crit1']+1

    dat[dat[,tr_vel]>2.8&!is.na(dat[,tr_vel]),'ase2016_crit1'] <-
      dat[dat[,tr_vel]>2.8&!is.na(dat[,tr_vel]),'ase2016_crit1']+1

    dat[dat$la_abnl==1&!is.na(dat$la_abnl),'ase2016_crit1'] <-
      dat[dat$la_abnl==1&!is.na(dat$la_abnl),'ase2016_crit1']+1

    dat[,'crit1_done'] <- 0
    dat[!is.na(dat[,"eep_avg"]),"crit1_done"] <- dat[!is.na(dat[,"eep_avg"]),"crit1_done"] +1
    dat[!is.na(dat[,"eprime_abnormal"]),"crit1_done"] <- dat[!is.na(dat[,"eprime_abnormal"]),"crit1_done"] +1
    dat[!is.na(dat[,tr_vel]),"crit1_done"] <- dat[!is.na(dat[,tr_vel]),"crit1_done"] +1
    dat[!is.na(dat[,"la_abnl"]),"crit1_done"] <- dat[!is.na(dat[,"la_abnl"]),"crit1_done"] +1

        
    dat$crit1_pct <- dat$ase2016_crit1/dat$crit1_done
    

    ### ---- LAP criteria

    dat$ase2016_crit2 <- 0

    dat[dat[,"eep_avg"]>14&!is.na(dat[,"eep_avg"]),'ase2016_crit2'] <-  
      dat[dat[,"eep_avg"]>14&!is.na(dat[,"eep_avg"]),'ase2016_crit2']+1
    
    dat[dat[,tr_vel] > 2.8&!is.na(dat[,tr_vel]),'ase2016_crit2'] <- 
      dat[dat[,tr_vel] > 2.8&!is.na(dat[,tr_vel]),'ase2016_crit2']+1
    
    dat[dat[,"la_abnl"]==1&!is.na(dat[,"la_abnl"]), 'ase2016_crit2'] <- 
      dat[dat[,"la_abnl"]==1&!is.na(dat[,"la_abnl"]), 'ase2016_crit2']+1

    dat$crit2_done <- 0
    dat[!is.na(dat[,"eep_avg"]),"crit2_done"] <- dat[!is.na(dat[,"eep_avg"]),"crit2_done"] +1
    dat[!is.na(dat[,tr_vel]),"crit2_done"] <- dat[!is.na(dat[,tr_vel]),"crit2_done"] +1
    dat[!is.na(dat[,"la_abnl"]),"crit2_done"] <- dat[!is.na(dat[,"la_abnl"]),"crit2_done"]+1

    dat$crit2_pct <- dat$ase2016_crit2/dat$crit2_done
    
    
    ### ---- Mitral inflow criteria --- ###

    dat$mitral_inflow_type[dat[,earat]<=0.8&dat[,e_vel]<=50&dat[,"crit1_pct"]>=0.5&
                             !is.na(dat[,earat])&!is.na(dat[,e_vel])&!is.na(dat[,"crit1_pct"])] <- 1

    
    dat$mitral_inflow_type[dat[,earat]<=0.8&dat[,e_vel]>50&dat[,"crit1_pct"]>=0.5&
                             !is.na(dat[,earat])&!is.na(dat[,e_vel])&!is.na(dat[,"crit1_pct"])] <- 2

    
    

    dat$mitral_inflow_type[dat[,earat]>0.8&dat[,earat]<2&dat[,"crit1_pct"]>=0.5&
                             !is.na(dat[,earat])&!is.na(dat[,"crit1_pct"])] <- 2

    
    

    dat$mitral_inflow_type[dat[,earat]>2&dat[,"crit1_pct"]>=0.5&
                             !is.na(dat[,earat])&!is.na(dat[,"crit1_pct"])] <- 3



    ### --- Assign DD ---###

    dat$ase2016_dd_num[dat$crit1_pct < 0.5&!is.na(dat$crit1_pct)] <-  1
    dat$ase2016_dd_txt[dat$crit1_pct < 0.5&!is.na(dat$crit1_pct)] <-  "Normal"

    dat$ase2016_dd_num[dat$mitral_inflow_type==1&!is.na(dat$mitral_inflow_type)&
                         !is.na(dat$mitral_inflow_type)] <-  2
    dat$ase2016_dd_txt[dat$mitral_inflow_type==1&!is.na(dat$mitral_inflow_type)&
                         !is.na(dat$mitral_inflow_type)] <-  "Grade I"

    dat$ase2016_dd_num[dat$mitral_inflow_type==2&dat$crit2_pct < 0.5&
                         !is.na(dat$mitral_inflow_type)&!is.na(dat$crit2_pct)] <-  2
    dat$ase2016_dd_txt[dat$mitral_inflow_type==2&dat$crit2_pct < 0.5&
                         !is.na(dat$mitral_inflow_type)&!is.na(dat$crit2_pct)] <-  'Grade I'
    
    dat$ase2016_dd_num[dat$mitral_inflow_type==2&dat$crit2_pct >= 0.5&
                         !is.na(dat$mitral_inflow_type)&!is.na(dat$crit2_pct)] <-  3
    dat$ase2016_dd_txt[dat$mitral_inflow_type==2&dat$crit2_pct >= 0.5&
                         !is.na(dat$mitral_inflow_type)&!is.na(dat$crit2_pct)] <-  'Grade II'
    
    dat$ase2016_dd_num[dat$mitral_inflow_type==3&!is.na(dat$mitral_inflow_type)] <-  4
    dat$ase2016_dd_txt[dat$mitral_inflow_type==3&!is.na(dat$mitral_inflow_type)] <-  'Grade III'
    
    dat$ase2016_dd_txt <- factor(dat$ase2016_dd_txt,
                               levels=c("Normal",
                                        "Grade I",
                                        "Grade II",
                                        "Grade III"))
    
    
    # Parameters: E/e' septal vs.E/e' lateral vs. E/e' avg; E/A ratio; TR velocity (m/s); e' septal; e' lateral; E velocity; LA volume index

    if (verbose == T) {
      return(dat[,c(pid,
                    vst,
                    "eep_avg",
                    earat,
                    tr_vel,
                    "eprime_abnormal",
                    ep_lat,
                    ep_sept,
                    e_vel,
                    lavix,
                    laa,
                    "la_abnl",
                    "ase2016_crit1",
                    "crit1_done",
                    "crit1_pct",
                    "ase2016_crit2",
                    "crit2_done",
                    "crit2_pct",
                    "mitral_inflow_type", 
                    "ase2016_dd_num",
                    "ase2016_dd_txt")])
    }

    if (verbose == F) {
      return(dat[,c(pid, vst,"ase2016_dd_num","ase2016_dd_txt")])
    }

}



######## %---- Bursi definition ----% ########

# .%%%%%%%%..%%.....%%.%%%%%%%%...%%%%%%..%%%%
# .%%.....%%.%%.....%%.%%.....%%.%%....%%..%%.
# .%%.....%%.%%.....%%.%%.....%%.%%........%%.
# .%%%%%%%%..%%.....%%.%%%%%%%%...%%%%%%...%%.
# .%%.....%%.%%.....%%.%%...%%.........%%..%%.
# .%%.....%%.%%.....%%.%%....%%..%%....%%..%%.
# .%%%%%%%%...%%%%%%%..%%.....%%..%%%%%%..%%%%

# Bursi F, Weston SA, Redfield MM, et al. Systolic and diastolic heart failure in the community.JAMA;2006;296:2209–2216. 
# Parameters: E/A ratio; E/e' sept/lat/avg; MV decel time; LA volume index
# https://pubmed.ncbi.nlm.nih.gov/17090767/


assign_diastolic_function_bursi <- 
  function(dat,                                             
           pid="patientid",
           vst="VISIT",
           dt="decel_time_val",
           earat="ea_ratio_val",
           eep_sep = "eeprime_med_val",
           eep_lat="eeprime_lat_val",
           verbose=F,
           complete_cases=T)
{
    
    dat$eep_avg <- apply(dat[,c(eep_lat,eep_sep)],MARGIN = 1,na.rm=T,FUN=mean)
    
    if (complete_cases==T) {
      
      dat <- dat[complete.cases(dat[,c(pid,vst,"eep_avg",earat,dt)]),
                    c(pid,vst,"eep_avg",earat,dt)]
    }
  
    
    dat$bursi_dd_num[dat[,dt]>140&dat[,earat]>0.75&dat[,earat]<2&dat[,"eep_avg"]<10] <- 1
    dat$bursi_dd_txt[dat[,dt]>140&dat[,earat]>0.75&dat[,earat]<2&dat[,"eep_avg"]<10] <- "Normal"
    dat$bursi_dd_num[dat[,earat]<=0.75&dat[,"eep_avg"]<10] <- 2
    dat$bursi_dd_txt[dat[,earat]<=0.75&dat[,"eep_avg"]<10] <- "Grade I"
    dat$bursi_dd_num[dat[,dt]>140&dat[,earat]>0.75&dat[,earat]<2&dat[,"eep_avg"]>=10] <- 3
    dat$bursi_dd_txt[dat[,dt]>140&dat[,earat]>0.75&dat[,earat]<2&dat[,"eep_avg"]>=10] <- "Grade II"
    dat$bursi_dd_num[dat[,dt]<140&dat[,earat]>2&dat[,"eep_avg"]>=10] <- 4
    dat$bursi_dd_txt[dat[,dt]<140&dat[,earat]>2&dat[,"eep_avg"]>=10] <- "Grade III"
    
    dat$bursi_dd_txt <- factor(dat$bursi_dd_txt,
                                 levels=c("Normal",
                                          "Grade I",
                                          "Grade II",
                                          "Grade III"))
    
    
    if (verbose == T) {  
    return(dat[,c(pid,vst,dt,earat,eep_sep,eep_lat,
                     "bursi_dd_num","bursi_dd_txt")])
    }
    
    if (verbose == F) {  
      return(dat[,c(pid,vst,"bursi_dd_num","bursi_dd_txt")])
    }
    
}


######## %---- Olmsted definition ----% ########

# ..%%%%%%%..%%.......%%.....%%..%%%%%%..%%%%%%%%.%%%%%%%%.%%%%%%%%.
# .%%.....%%.%%.......%%%...%%%.%%....%%....%%....%%.......%%.....%%
# .%%.....%%.%%.......%%%%.%%%%.%%..........%%....%%.......%%.....%%
# .%%.....%%.%%.......%%.%%%.%%..%%%%%%.....%%....%%%%%%...%%.....%%
# .%%.....%%.%%.......%%.....%%.......%%....%%....%%.......%%.....%%
# .%%.....%%.%%.......%%.....%%.%%....%%....%%....%%.......%%.....%%
# ..%%%%%%%..%%%%%%%%.%%.....%%..%%%%%%.....%%....%%%%%%%%.%%%%%%%%.


# Redfield MM, Jacobsen SJ, Burnett JC, Mahoney DW, Bailey KR & Rodeheffer RJ 
# Burden of systolic and diastolic ventricular dysfunction in the community: Appreciating the scope of the heart failure epidemic. 
# Journal of the American Medical Association, 289(2), 194–202. https://doi.org/10.1001/jama.289.2.194
# Parameters: decel time, E/A ratio, E/e' medial and lateral

assign_diastolic_function_olmsted <- 
  function(dat,                                             
           pid="patientid",
           vst="VISIT",
           dt="decel_time_val",
           earat="ea_ratio_val",
           eep_sept = "eeprime_med_val",
           eep_lat="eeprime_lat_val",
           verbose=F,
           complete_cases=T)
  {
    dat$eep_avg <- apply(
                         dat[,c(eep_lat,eep_sept)],
                         MARGIN = 1,
                         na.rm=T,
                         FUN=max)
    
    dat$eep_avg[dat$eep_avg=="-Inf"] <- NA

    dat$red_missing <-
      is.na(dat[,earat])+
      is.na(dat[,"eep_avg"])+
      is.na(dat[,dt])
    
    if (complete_cases==T) {
      
    dat <- dat[complete.cases(dat[,c(pid,"eep_avg",earat,dt)]),
               c(pid,vst,"eep_avg",earat,dt)]
    }


        
    ###---------###
    

    dat$nodd_criteria_olmsted[!is.na(dat[,earat])&
                                !is.na(dat[,dt])&
                                !is.na(dat[,"eep_avg"])] <- 0
    
    dat$nodd_criteria_olmsted[dat[,earat]>0.75&dat[,earat]<1.5&dat[,dt]>140] <- 1

    dat$nodd_criteria_olmsted[dat[,"eep_avg"]<10] <-
      dat$nodd_criteria_olmsted[dat[,"eep_avg"]<10] + 1


    ###---------###

    dat$gradeI_criteria_olmsted[!is.na(dat[,earat])&!is.na(dat[,"eep_avg"])] <- 0
    dat$gradeI_criteria_olmsted[dat[,earat]<=0.75] <- 1

    dat$gradeI_criteria_olmsted[dat[,"eep_avg"]>=10] <-
      dat$gradeI_criteria_olmsted[dat[,"eep_avg"]>=10] + 1

    ###---------###

    dat$gradeII_criteria_olmsted[!is.na(dat[,earat])&
                                   !is.na(dat[,dt])&
                                   !is.na(dat[,"eep_avg"])] <- 0
    
    dat$gradeII_criteria_olmsted[dat[,earat]>0.75&
                                       dat[,earat]<1.5&
                                       dat[,dt]>140] <- 1

    dat$gradeII_criteria_olmsted[dat[,"eep_avg"]>=10] <-
      dat$gradeII_criteria_olmsted[dat[,"eep_avg"]>=10] + 1


    ###---------###

    dat$gradeIII_criteria_olmsted[!is.na(dat[,earat])&
                                    !is.na(dat[,dt])&
                                    !is.na(dat[,"eep_avg"])] <- 0

    dat$gradeIII_criteria_olmsted[dat[,earat] > 1.5|
                                        dat[,dt] < 140] <- 1

    dat$gradeIII_criteria_olmsted[dat[,"eep_avg"] >= 10] <-
      dat$gradeIII_criteria_olmsted[dat[,"eep_avg"] >= 10] + 1

    ###---------###

    dat$olmsted_dd_num[dat$nodd_criteria_olmsted >=1] <- 1
    dat$olmsted_dd_num[dat$gradeI_criteria_olmsted >=1] <- 2
    dat$olmsted_dd_num[dat$gradeII_criteria_olmsted >=2] <- 3
    dat$olmsted_dd_num[dat$gradeIII_criteria_olmsted >=2] <- 4

    dat$olmsted_dd_txt[dat$nodd_criteria_olmsted >=1] <- "Normal"
    dat$olmsted_dd_txt[dat$gradeI_criteria_olmsted >=1] <- "Grade I"
    dat$olmsted_dd_txt[dat$gradeII_criteria_olmsted >=2] <- "Grade II"
    dat$olmsted_dd_txt[dat$gradeIII_criteria_olmsted >=2] <- "Grade III"
    
    dat$olmsted_dd_txt <- factor(dat$olmsted_dd_txt,
                                 levels=c("Normal",
                                          "Grade I",
                                          "Grade II",
                                          "Grade III"))
  
    ###---------###

    # Parameters: decel time, E/A ratio, E/e' medial and latera

    if (verbose == T) {
    return(dat[,c(pid,vst,dt,earat,"eep_avg",
                  "nodd_criteria_olmsted",
                  "gradeI_criteria_olmsted",
                  "gradeII_criteria_olmsted",
                  "gradeIII_criteria_olmsted",
                  "olmsted_dd_num","olmsted_dd_txt")])
    }

    if (verbose == F) {
    return(dat[,c(pid,vst,"olmsted_dd_num","olmsted_dd_txt")])
    }
        
  }





######## %---- ASE 2009 definition ----% ########

# ....%%%.....%%%%%%..%%%%%%%%........%%%%%%%....%%%%%.....%%%%%....%%%%%%%.
# ...%%.%%...%%....%%.%%.............%%.....%%..%%...%%...%%...%%..%%.....%%
# ..%%...%%..%%.......%%....................%%.%%.....%%.%%.....%%.%%.....%%
# .%%.....%%..%%%%%%..%%%%%%..........%%%%%%%..%%.....%%.%%.....%%..%%%%%%%%
# .%%%%%%%%%.......%%.%%.............%%........%%.....%%.%%.....%%........%%
# .%%.....%%.%%....%%.%%.............%%.........%%...%%...%%...%%..%%.....%%
# .%%.....%%..%%%%%%..%%%%%%%%.......%%%%%%%%%...%%%%%.....%%%%%....%%%%%%%.


# Nagueh SF, Appleton CP, Gillebert TC, et al. Recommendations for the Evaluation of Left Ventricular Diastolic Function by Echocardiography. 
# Journal of the American Society of Echocardiography, 22, 107–133. https://doi.org/10.1016/j.echo.2008.11.023


assign_diastolic_function_ase2009 <- 
  function(dat,                                             
           pid="patientid",
           vst="VISIT",
           ep_lat="eprime_lat_val",
           ep_sept="eprime_med_val",
           dt="decel_time_val",
           earat="ea_ratio_val",
           lavix="lav_ix_val",
           laa="laa_val",
           eep_sep = "eeprime_med_val",
           eep_lat="eeprime_lat_val",
           verbose=F,
           complete_cases=T)
  {

    dat$eep_avg <- apply(dat[,c(eep_lat,eep_sep)],MARGIN = 1,na.rm=T,FUN=mean)
    
    dat[!is.na(dat[,ep_lat])|!is.na(dat[,ep_sept]),"eprime_abnormal"] <- 0
    dat[dat[,ep_lat]<10&!is.na(dat[,ep_lat]),"eprime_abnormal"] <- 1
    dat[dat[,ep_sept]<8&!is.na(dat[,ep_sept]),"eprime_abnormal"] <- 1
    
    dat[!is.na(dat[,lavix])|!is.na(dat[,laa]),"la_abnl"] <- 0
    dat[dat[,laa]>20&!is.na(dat[,laa]),"la_abnl"] <- 1
    dat[dat[,lavix]>34&!is.na(dat[,lavix]),"la_abnl"] <- 1

    if (complete_cases==T) {
      dat <- dat[complete.cases(dat[,c(pid,
                                       "eprime_abnormal",
                                       "la_abnl",
                                       earat,
                                       "eep_avg",
                                       dt)]),
                 c(pid,
                   vst,
                   "eprime_abnormal",
                   "la_abnl",
                   earat,
                   "eep_avg",
                   dt)]
    }
    
    
    ### --- Normal --- ###
    
    dat$ase2009_dd_num[dat$eprime_abnormal==0|dat$la_abnl==0&
                     !is.na(dat$eprime_abnormal)&!is.na(dat$la_abnl)] <- 1
    dat$ase2009_dd_txt[dat$eprime_abnormal==0|dat$la_abnl==0&
                         !is.na(dat$eprime_abnormal)&!is.na(dat$la_abnl)] <- "Normal"

    ### --- Grade I --- ###
    
    
    dat$ase2009_gradeI_crit[
      !is.na(dat$eprime_abnormal)&
        !is.na(dat[,earat])&
        !is.na("eep_avg")] <- 0
    
    dat$ase2009_gradeI_crit[dat[,earat]<0.8&dat$eprime_abnormal==1&
                              !is.na(dat[,earat])&!is.na(dat$eprime_abnormal)] <-
      dat$ase2009_gradeI_crit[dat[,earat]<0.8&dat$eprime_abnormal==1&
                                !is.na(dat[,earat])&!is.na(dat$eprime_abnormal)] + 1
    
    dat$ase2009_gradeI_crit[dat[,dt]>200&dat$eprime_abnormal==1&
                              !is.na(dat[,dt])&!is.na(dat$eprime_abnormal)] <-
      dat$ase2009_gradeI_crit[dat[,dt]>200&dat$eprime_abnormal==1&
                                !is.na(dat[,dt])&!is.na(dat$eprime_abnormal)] + 1
    
    
    dat$ase2009_gradeI_crit[dat[,"eep_avg"]<=8&dat$eprime_abnormal==1&
                              !is.na(dat[,"eep_avg"])&!is.na(dat$eprime_abnormal)] <-
      dat$ase2009_gradeI_crit[dat[,"eep_avg"]<=8&dat$eprime_abnormal==1&
                                !is.na(dat[,"eep_avg"])&!is.na(dat$eprime_abnormal)] + 1

    
    ### ---- Grade II ---- ###
    
    dat$ase2009_gradeII_crit[
      !is.na(dat$eprime_abnormal)&
        !is.na(dat[,earat])&
        !is.na("eep_avg")] <- 0
    
        dat$ase2009_gradeII_crit[dat[,earat]>=0.8&dat[,earat]<=1.5&dat$eprime_abnormal==1&
                                   !is.na(dat[,earat])&!is.na(dat$eprime_abnormal)] <-
      dat$ase2009_gradeII_crit[dat[,earat]>=0.8&dat[,earat]<=1.5&dat$eprime_abnormal==1&
                                 !is.na(dat[,earat])&!is.na(dat$eprime_abnormal)] + 1

            dat$ase2009_gradeII_crit[dat[,dt]>=160&dat[,dt]<=200&dat$eprime_abnormal==1&
                                       !is.na(dat[,dt])&!is.na(dat$eprime_abnormal)] <-
      dat$ase2009_gradeII_crit[dat[,dt]>=160&dat[,dt]<=200&dat$eprime_abnormal==1&
                                 !is.na(dat[,dt])&!is.na(dat$eprime_abnormal)] + 1
    
            dat$ase2009_gradeII_crit[dat[,"eep_avg"]>8&dat[,"eep_avg"]<13&dat$eprime_abnormal==1&
                                       !is.na(dat[,"eep_avg"])&!is.na(dat$eprime_abnormal)] <-
      dat$ase2009_gradeII_crit[dat[,"eep_avg"]>8&dat[,"eep_avg"]<13&dat$eprime_abnormal==1&
                                 !is.na(dat[,"eep_avg"])&!is.na(dat$eprime_abnormal)] + 1


    ### ---- Grade III ---- ###
            
                        
            dat$ase2009_gradeIII_crit[
              !is.na(dat$eprime_abnormal)&
                !is.na(dat[,earat])&
                !is.na("eep_avg")] <- 0
            
    
    dat$ase2009_gradeIII_crit[dat[,earat]>=2&dat$eprime_abnormal==1&
                                !is.na(dat[,earat])&!is.na(dat$eprime_abnormal)] <-
      dat$ase2009_gradeIII_crit[dat[,earat]>=2&dat$eprime_abnormal==1&
                                  !is.na(dat[,earat])&!is.na(dat$eprime_abnormal)] + 1
    
    dat$ase2009_gradeIII_crit[dat[,dt]<160&dat$eprime_abnormal==1&
                                !is.na(dat[,dt])&!is.na(dat$eprime_abnormal)] <-
      dat$ase2009_gradeIII_crit[dat[,dt]<160&dat$eprime_abnormal==1&
                                  !is.na(dat[,dt])&!is.na(dat$eprime_abnormal)] + 1
    
    dat$ase2009_gradeIII_crit[dat[,"eep_avg"]>=13&dat$eprime_abnormal==1&
                                !is.na(dat[,"eep_avg"])&!is.na(dat$eprime_abnormal)] <-
      dat$ase2009_gradeIII_crit[dat[,"eep_avg"]>=13&dat$eprime_abnormal==1&
                                  !is.na(dat[,"eep_avg"])&!is.na(dat$eprime_abnormal)] + 1
    


    dat$ase2009_dd_num[dat$ase2009_gradeI_crit>=2] <- 2
    dat$ase2009_dd_txt[dat$ase2009_gradeI_crit>=2] <- "Grade I"

    dat$ase2009_dd_num[dat$ase2009_gradeII_crit==2] <- 3
    dat$ase2009_dd_txt[dat$ase2009_gradeII_crit>=2] <- "Grade II"

    dat$ase2009_dd_num[dat$ase2009_gradeIII_crit==2] <- 4
    dat$ase2009_dd_txt[dat$ase2009_gradeIII_crit>=2] <- "Grade III"

    
    dat$ase2009_dd_txt <- factor(dat$ase2009_dd_txt,
                                levels=c("Normal",
                                         "Grade I",
                                         "Grade II",
                                         "Grade III"))
    

    if (verbose==T) {
    return(dat[,c(pid,
                  vst,
                  "eprime_abnormal",
                  "la_abnl",
                  # laa,
                  # lavix,
                  # earat,
                  # dt,
                  "eep_avg",
                  "ase2009_gradeI_crit",
                  "ase2009_gradeII_crit",
                  "ase2009_gradeIII_crit",
                  "ase2009_dd_num",
                  "ase2009_dd_txt")])
    }

    if (verbose==F) {
      return(dat[,c(pid,
                    vst,
                    "ase2009_dd_num",
                    "ase2009_dd_txt")])
    }
    
          
  }


######## %---- British Soc of Echo definition ----% ########

# .%%%%%%%%..%%%%%%%%..%%%%.%%%%%%%%.%%%%..%%%%%%..%%.....%%........%%%%%%...%%%%%%%...%%%%%%....
# .%%.....%%.%%.....%%..%%.....%%.....%%..%%....%%.%%.....%%.......%%....%%.%%.....%%.%%....%%...
# .%%.....%%.%%.....%%..%%.....%%.....%%..%%.......%%.....%%.......%%.......%%.....%%.%%.........
# .%%%%%%%%..%%%%%%%%...%%.....%%.....%%...%%%%%%..%%%%%%%%%........%%%%%%..%%.....%%.%%.........
# .%%.....%%.%%...%%....%%.....%%.....%%........%%.%%.....%%.............%%.%%.....%%.%%.........
# .%%.....%%.%%....%%...%%.....%%.....%%..%%....%%.%%.....%%.......%%....%%.%%.....%%.%%....%%...
# .%%%%%%%%..%%.....%%.%%%%....%%....%%%%..%%%%%%..%%.....%%........%%%%%%...%%%%%%%...%%%%%%....

# British Society of Echocardiography. (2013). A Guideline Protocol for the Echocardiographic assessment of Diastolic Dysfunction. 
# https://www.bsecho.org/common/Uploaded%20files/Education/Protocols%20and%20guidelines/Diastolic%20dysfunction.pdf

# Uses E, e', E/A, E/e', LA area, LA volume index, MV decel time, pulmonary vein flow ratio

assign_diastolic_function_bse <-
  function(dat,
           pid="patientid",
           vst="VISIT",
           age="age",
           ep_lat="eprime_lat_val",
           ep_sept="eprime_med_val",
           earat="ea_ratio_val",
           dt="decel_time_val",
           laa="laa_val",
           lav="lav_ix_val",
           pv="pvf_ratio",
           eep_sept = "eeprime_med_val",
           eep_lat="eeprime_lat_val",
           verbose=F,
           complete_cases=T) 
{

    dat[dat[,age]<=20&dat[,ep_lat]<20.6&!is.na(dat[,age])&!is.na(dat[,ep_lat]),"eprime_reduced"] <- 1
    dat[dat[,age]<=20&dat[,ep_sept]<14.9&!is.na(dat[,age])&!is.na(dat[,ep_sept]),"eprime_reduced"] <- 1
    
    dat[dat[,age]>20&dat[,age]<=40&dat[,ep_lat] < 19.8&!is.na(dat[,age])&!is.na(dat[,ep_lat]),"eprime_reduced"] <- 1
    dat[dat[,age]>20&dat[,age]<=40&dat[,ep_sept]< 15.5&!is.na(dat[,age])&!is.na(dat[,ep_sept]),"eprime_reduced"] <- 1

    dat[dat[,age]>41&dat[,age]<=60&dat[,ep_lat] < 16.1&!is.na(dat[,age])&!is.na(dat[,ep_lat]),"eprime_reduced"] <- 1
    dat[dat[,age]>41&dat[,age]<=60&dat[,ep_sept]< 12.2&!is.na(dat[,age])&!is.na(dat[,ep_sept]),"eprime_reduced"] <- 1
    
    dat[dat[,age]>61&dat[,ep_lat] < 12.9&!is.na(dat[,age])&!is.na(dat[,ep_lat]),"eprime_reduced"] <- 1
    dat[dat[,age]>61&dat[,ep_lat] < 10.4&!is.na(dat[,age])&!is.na(dat[,ep_lat]),"eprime_reduced"] <- 1

    dat$eep_max <- apply(dat[,c(eep_lat,eep_sept)],MARGIN = 1,FUN=max)
    
    print(names(dat))
    
    if (complete_cases==T) {
    dat <- dat[complete.cases(dat[,c(pid,
                                     "eprime_reduced",
                                     laa,
                                     lav,
                                     pv,
                                     earat,
                                     "eep_max",
                                     dt
                                     )]),
               c(pid,
                 vst,
                 "eprime_reduced",
                 laa,
                 lav,
                 pv,
                 earat,
                 "eep_max",
                 dt
                 )]
    }
    
    
### -- Normal secondary criteria -- ###    
    
    dat$nodd_secondary <- 0
    
    dat$nodd_secondary[dat[,laa]<20] <- 
      dat$nodd_secondary[dat[,laa]<20]+1
    
    dat$nodd_secondary[dat[,lav]<34] <- 
      dat$nodd_secondary[dat[,lav]<34]+1
    
    dat$nodd_secondary[dat[,pv]>1] <- 
      dat$nodd_secondary[dat[,pv]>1]+1
    
    dat$nodd_secondary[dat[,eep_max]<8] <- 
      dat$nodd_secondary[dat[,eep_max]<8]+1
    
    dat$bse_dd_num[dat[,earat]>=1&dat[,earat]<=2&
                     dat[,dt] >= 130&dat[,dt] <= 230&
                     dat[,"eprime_reduced"]==0&
                     dat[,"nodd_secondary"]>0] <- 1

    dat$bse_dd_txt[dat[,earat]>=1&dat[,earat]<=2&
          dat[,dt] >= 130&dat[,dt] <= 230&
          dat[,"eprime_reduced"]==0&
          dat[,"nodd_secondary"]>0] <- "Normal"

        
### -- Grade I secondary criteria -- ###    
    
    dat$dd1_secondary <- 0
    
    dat$dd1_secondary[dat[,eprime_reduced]==1] <- 
      dat$dd1_secondary[dat[,eprime_reduced]==1] +1
    
    dat$dd1_secondary[dat[,laa]>20] <- 
      dat$dd1_secondary[dat[,laa]>20]+1
    
    dat$dd1_secondary[dat[,lav]>34] <- 
      dat$dd1_secondary[dat[,lav]>34]+1
    
    dat$dd1_secondary[dat[,eep_max]<=8] <- 
      dat$dd1_secondary[dat[,eep_max]<=8]+1
    
    dat$dd1_secondary[dat$pv>=1] <- 
      dat$dd1_secondary[dat$pv>=1]+1

    dat$bse_dd_num[dat[,earat]<1&
                     dat[,dt] >= 230&
                     dat[,"dd1_secondary"]>0] <- 2
    
    dat$bse_dd_txt[dat[,earat]<1&
                     dat[,dt] >= 230&
                     dat[,"dd1_secondary"]>0] <- "Grade I"
    

### -- Grade II secondary criteria -- ###    
    
    dat$dd2_secondary <- 0
    
    dat$dd2_secondary[dat$laa>20] <- 
      dat$dd2_secondary[dat$laa>20]+1
    
    dat$dd2_secondary[dat$lav>34] <- 
      dat$dd2_secondary[dat$lav>34]+1
    
    dat$dd2_secondary[max(dat[,eep_lat],dat[,eep_sept]) >= 13] <- 
      dat$dd2_secondary[max(dat[,eep_lat],dat[,eep_sept]) >= 13]+1
    
    dat$dd2_secondary[dat$pv <1] <- 
      dat$dd2_secondary[dat$pv<1]+1
    
    dat$bse_dd_num[dat[,earat]>=1&dat[,earat]<=2&
                     dat[,dt] >= 130&dat[,dt] <= 230&
                     dat[,"dd2_secondary"]>0] <- 3
    
    dat$bse_dd_txt[dat[,earat]>=1&dat[,earat]<=2&
                     dat[,dt] >= 130&dat[,dt] <= 230&
                     dat[,"dd2_secondary"]>0] <- "Grade II"
    
  
### -- Grade III -- ###    
    
    dat$dd3_secondary <- 0
    
    dat$dd3_secondary[dat$eprime_reduced==1] <- 
      dat$dd3_secondary[dat$eprime_reduced==1] +1
    
    dat$dd3_secondary[dat$laa>20] <- 
      dat$dd3_secondary[dat$laa>20]+1
    
    dat$dd3_secondary[dat$lav>34] <- 
      dat$dd3_secondary[dat$lav>34]+1
    
    dat$dd3_secondary[max(dat[,eep_lat],dat[,eep_sept]) > 13] <- 
      dat$dd3_secondary[max(dat[,eep_lat],dat[,eep_sept]) > 13]+1
    
    dat$dd3_secondary[dat$pv <1] <- 
      dat$dd3_secondary[dat$pv<1]+1
    
    dat$bse_dd_num[dat[,earat]>2&dat[,dt]< 130&dat[,"dd3_secondary"]>0] <- 4
    dat$bse_dd_txt[dat[,earat]>2&dat[,dt]< 130&dat[,"dd3_secondary"]>0] <- "Grade III"
    
    
    dat$bse_dd_txt <- factor(dat$bse_dd_txt,
                                 levels=c("Normal",
                                          "Grade I",
                                          "Grade II",
                                          "Grade III"))
    
    
  #  E, e', E/A, E/e', LA area, LA volume index, MV decel time, pulmonary vein flow ratio
    
    if (verbose == T) {  
      return(dat[,c(pid,vst,earat,"eep_avg","ep_lat","ep_sept","eprime_reduced",dt,laa,lavix,pv,
                    "nodd_secondary","dd1_secondary","dd2_secondary","dd3_secondary",
                    "bse_dd_num","bse_dd_txt")])
    }
    
    if (verbose == F) {  
      return(dat[,c(pid,vst,"bse_dd_num","bse_dd_txt")])
    }
    
    
  }


assign_diastolic_function <- 
  function(dat,
           pid="patientid",
           vst="VISIT",
           age="age",
           ep_lat="eprime_lat_val",
           ep_sept="eprime_med_val",
           earat="ea_ratio_val",
           dt="decel_time_val",
           tr_vel = "tr_ms",
           e_vel="ewave_val",
           lavix="lav_ix_val",
           laa="laa_val",
           eep_sep = "eeprime_med_val",
           eep_lat="eeprime_lat_val",
           pv="pvf_ratio",
           def="TOPCAT",
           verbose=F,
           complete_cases=T) 

  {
    
    if (!def %in% c("TOPCAT","Olmsted","BSE","ASE2016","ASE2009","Bursi")) {
      stop('Invalid definition. Choose "TOPCAT","Olmsted","BSE","ASE2016","ASE2009","Bursi"')
    }
    
    if (def == "TOPCAT") {
      return(
        assign_diastolic_function_topcat(dat = dat,
                                         pid=pid,
                                         vst=vst,
                                         ep_lat=ep_lat,
                                         ep_sept=ep_sept,
                                         earat=earat,
                                         dt=dt,
                                         verbose=verbose,
                                         complete_cases=complete_cases))
    } else if (def == "ASE2016") {
      return(
        assign_diastolic_function_ase2016(dat=dat,                                             
                                          pid=pid,
                                          vst=vst,
                                          ep_lat=ep_lat,
                                          ep_sept=ep_sept,
                                          earat=earat,
                                          tr_vel = tr_vel,
                                          e_vel=e_vel,
                                          lavix=lavix,
                                          laa=laa,
                                          eep_sep = eep_sep,
                                          eep_lat= eep_lat,
                                          verbose=verbose,
                                          complete_cases=complete_cases)
      )
    } else if (def == "Bursi") {
      return(
        assign_diastolic_function_bursi(dat=dat,                                             
                                        pid=pid,
                                        vst=vst,
                                        ep_lat=ep_lat,
                                        ep_sept=ep_sept,
                                        earat=earat,
                                        tr_vel = tr_vel,
                                        e_vel=e_vel,
                                        lavix=lavix,
                                        laa=laa,
                                        eep_sep = eep_sep,
                                        eep_lat= eep_lat,
                                        verbose=verbose,
                                        complete_cases=complete_cases)
      )
    } else if (def == "Olmsted") {
      return(
        assign_diastolic_function_bursi(dat=dat,                                             
                                        pid=pid,
                                        vst=vst,
                                        dt=dt,
                                        earat=earat,
                                        eep_sep = eep_sep,
                                        eep_lat= eep_lat,
                                        verbose=verbose,
                                        complete_cases=complete_cases)
      )
    } else if (def == "ASE2009") {
      return(
        assign_diastolic_function_bursi(dat=dat,                                             
                                        pid=pid,
                                        vst=vst,
                                        ep_lat=ep_lat,
                                        ep_sept=ep_sept,
                                        dt=dt,
                                        earat=earat,
                                        lavix=lavix,
                                        laa=laa,
                                        eep_sep = eeo_sep,
                                        eep_lat= eep_lat,
                                        verbose=verbose,
                                        complete_cases=complete_cases)
      )
      
    } else if (def == "BSE") {
      return(
        assign_diastolic_function_bursi(dat=dat,                                             
                                        pid=pid,
                                        vst=vst,
                                        age=age,
                                        ep_lat = ep_lat,
                                        ep_sept = ep_sept,
                                        earat = earat,
                                        dt=dt,
                                        laa=laa,
                                        lav=lav,
                                        pv=pv,
                                        eep_sept = eep_sept,
                                        eep_lat= eep_lat,
                                        verbose=verbose,
                                        complete_cases=complete_cases) 
      )
      
    }
  }    



##### %-------- LV hypertrophy --------% #####

# .%%.......%%.....%%.......%%.....%%.%%....%%.%%%%%%%%..%%%%%%%%.%%%%%%%%..%%%%%%%%.%%%%%%%%...%%%%%%%..%%%%%%%%..%%....%%
# .%%.......%%.....%%.......%%.....%%..%%..%%..%%.....%%.%%.......%%.....%%....%%....%%.....%%.%%.....%%.%%.....%%..%%..%%.
# .%%.......%%.....%%.......%%.....%%...%%%%...%%.....%%.%%.......%%.....%%....%%....%%.....%%.%%.....%%.%%.....%%...%%%%..
# .%%.......%%.....%%.......%%%%%%%%%....%%....%%%%%%%%..%%%%%%...%%%%%%%%.....%%....%%%%%%%%..%%.....%%.%%%%%%%%.....%%...
# .%%........%%...%%........%%.....%%....%%....%%........%%.......%%...%%......%%....%%...%%...%%.....%%.%%...........%%...
# .%%.........%%.%%.........%%.....%%....%%....%%........%%.......%%....%%.....%%....%%....%%..%%.....%%.%%...........%%...
# .%%%%%%%%....%%%..........%%.....%%....%%....%%........%%%%%%%%.%%.....%%....%%....%%.....%%..%%%%%%%..%%...........%%...


calc_hypertrophy_type <- function(df, 
                                  id = "patientid", 
                                  sex="sex",
                                  male="1",
                                  female="2",
                                  lvedd_cm="lvedd",
                                  ivsd_cm="ivsd",
                                  lvpwtd_cm="lvpwtd",
                                  height_cm="height",
                                  weight_kg="weight") 
  
  {

  df$lvmass <-
  (0.8*
  1.04*
  (((df[,ivsd_cm]+
      df[,lvedd_cm]+
      df[,lvpwtd_cm])^3)-
     (df[,lvedd_cm]^3)))+
  0.6

  df$bsa <- 0.20247*((df[,weight_kg])^0.425)*((df[,height_cm]/100)^0.725)



  df$lvmass_ix <- df$lvmass/df$bsa

  df$rwt <- 2*df[,lvpwtd_cm]/df[,lvedd_cm]

  
  df$lvh_type[df[,sex]==male&df$lvmass_ix>115&df$rwt<=0.42&!is.na(df$lvmass_ix)&!is.na(df$rwt)] <- "Eccentric hypertrophy"
  df$lvh_type[df[,sex]==female&df$lvmass_ix>95&df$rwt<=0.42&!is.na(df$lvmass_ix)&!is.na(df$rwt)] <- "Eccentric hypertrophy"

  df$lvh_type[df[,sex]==male&df$lvmass_ix>115&df$rwt>0.42&!is.na(df$lvmass_ix)&!is.na(df$rwt)] <- "Concentric hypertrophy"
  df$lvh_type[df[,sex]==female&df$lvmass_ix>95&df$rwt>0.42&!is.na(df$lvmass_ix)&!is.na(df$rwt)] <- "Concentric hypertrophy"

  df$lvh_type[df[,sex]==male&df$lvmass_ix<=115&df$rwt<=0.42&!is.na(df$lvmass_ix)&!is.na(df$rwt)] <- "Normal geometry"
  df$lvh_type[df[,sex]==female&df$lvmass_ix<=95&df$rwt<=0.42&!is.na(df$lvmass_ix)&!is.na(df$rwt)] <- "Normal geometry"

  df$lvh_type[df[,sex]==male&df$lvmass_ix<=115&df$rwt>0.42&!is.na(df$lvmass_ix)&!is.na(df$rwt)] <- "Concentric remodeling"
  df$lvh_type[df[,sex]==female&df$lvmass_ix<=95&df$rwt>0.42&!is.na(df$lvmass_ix)&!is.na(df$rwt)] <- "Concentric remodeling"


  df[,c(lvedd_cm,ivsd_cm,lvpwtd_cm,height_cm,weight_kg,"lvmass_ix","rwt","lvh_type")]
  
  
  return(df)
}

######### %------ Left atrial volume ------% ##########

# .%%..........%%%..........%%.....%%..%%%%%%%..%%.......%%.....%%.%%.....%%.%%%%%%%%
# .%%.........%%.%%.........%%.....%%.%%.....%%.%%.......%%.....%%.%%%...%%%.%%......
# .%%........%%...%%........%%.....%%.%%.....%%.%%.......%%.....%%.%%%%.%%%%.%%......
# .%%.......%%.....%%.......%%.....%%.%%.....%%.%%.......%%.....%%.%%.%%%.%%.%%%%%%..
# .%%.......%%%%%%%%%........%%...%%..%%.....%%.%%.......%%.....%%.%%.....%%.%%......
# .%%.......%%.....%%.........%%.%%...%%.....%%.%%.......%%.....%%.%%.....%%.%%......
# .%%%%%%%%.%%.....%%..........%%%.....%%%%%%%..%%%%%%%%..%%%%%%%..%%.....%%.%%%%%%%%



##  This currently calculates using the LA 2P area-length volume method:
##      0.85*LAA-2chamber * LAA-4chamber)/min(LA superior-inferior dimension 4chamber/LA superior/inferior 2 chamber)

## Consider future additions of LA 1P area-length volume method:
##      0.85*LAA-4chamber^2)/LA superior-inferior dimension 4chamber

## Ellipsoid method
##      0.42 * (LA AP diameter/2)*(LA medial-lat diameter/2)*(LA superior-inferior diameter-4ch/2)


calc_lav <- function(dat,
                     laa_4c, 
                     laa_2c, 
                     lal_4c, 
                     lal_2c, 
                     height_cm, 
                     weight_kg)
  
{
  
  
  dat$lav_calc <- 0.85*
                  dat[,laa_4c]*
                  dat[,laa_2c]/
                  pmin(dat[,lal_4c], dat[,lal_2c],na.rm=T)
  
  dat$bsa <- 0.20247*
            ((dat[,weight_kg])^0.425)*
            ((dat[,height_cm]/100)^0.725)
  
  
  dat$lav_ix_calc <- dat$lav_calc/dat$bsa
  
  return(dat)
  
}


######### %------ Pulsatility metrics ------% ##########

# .%%%%%%%%..%%.....%%.%%........%%%%%%.....%%%....%%%%%%%%.%%%%.%%.......%%%%.%%%%%%%%.%%....%%
# .%%.....%%.%%.....%%.%%.......%%....%%...%%.%%......%%.....%%..%%........%%.....%%.....%%..%%.
# .%%.....%%.%%.....%%.%%.......%%........%%...%%.....%%.....%%..%%........%%.....%%......%%%%..
# .%%%%%%%%..%%.....%%.%%........%%%%%%..%%.....%%....%%.....%%..%%........%%.....%%.......%%...
# .%%........%%.....%%.%%.............%%.%%%%%%%%%....%%.....%%..%%........%%.....%%.......%%...
# .%%........%%.....%%.%%.......%%....%%.%%.....%%....%%.....%%..%%........%%.....%%.......%%...
# .%%.........%%%%%%%..%%%%%%%%..%%%%%%..%%.....%%....%%....%%%%.%%%%%%%%.%%%%....%%.......%%...

calc_pulsatility <- function(dat,
                             sbp=NA,
                             dbp=NA,
                             edv=NA,
                             esv=NA,
                             bsa=NA,
                             hr=NA,
                             lvot_d=NA,
                             vti=NA) {
 
  dat$pp <- dat[,sbp]-dat[,dbp]
  dat$map <- dat[,dbp]+0.3333*dat$pp
  

if (!is.na(edv)&!is.na(esv)) {
    
  dat$sv_pulsatile <- dat[,edv]-dat[,esv]
  dat$svi_pulsatile <- dat[,"sv_pulsatile"]/dat[,bsa]
  dat$co_pulsatile <- dat[,"sv_pulsatile"]*dat[,hr]/1000
  dat$ci_pulsatile <- dat$co_pulsatile/dat[,bsa]
  
  dat$arterial_elastance_ix_pulsatile <- 0.9*dat[,sbp]/dat$svi_pulsatile
  dat$syst_art_comp_pulsatile <- dat$svi_pulsatile/dat$pp
  dat$svr_pulsatile <- dat$map*80/dat$co_pulsatile
  dat$svr_index_pulsatile <- dat$map*80/dat$ci_pulsatile
}
  
  if (!is.na(lvot_d)&!is.na(vti)) {
    dat$lvot_area <- pi*((dat[,lvot_d])/2)^2
    dat$sv_lvot <- (dat$lvot_area * dat[,vti])
    dat$svi_lvot <- (dat$lvot_area * dat[,vti])/dat[,bsa]
    
    dat$co_lvot <- dat[,"sv_lvot"]*dat[,hr]/1000
    dat$ci_lvot <- dat$co_lvot/dat[,bsa]

    dat$arterial_elastance_ix_lvot <- 0.9*dat[,sbp]/dat$svi_lvot
    dat$syst_art_comp_lvot <- dat$svi_lvot/dat$pp
    dat$svr_lvot <- dat$map*80/dat$co_lvot
    dat$svr_index_lvot <- dat$map*80/dat$ci_lvot
  } 
  
 return(dat)
}



