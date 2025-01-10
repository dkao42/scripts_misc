assign_diastolic_function_bursi <- function (dat,pid="patientid",eep_avg="eeprime_avg_val", earat="ea_ratio_val",dt="decel_time_val", 
                                             eep_sep = "eeprime_med_val", eep_lat="eeprime_lat_val", outpt=c("num","char"),echo_flag = "echo_done") {

# Uses definition from Bursi F, Weston SA, Redfield MM, Jacobsen SJ, Pakhomov S, Nkomo VT, Meverden R a, Roger VL. Systolic and diastolic heart failure in the community. JAMA United States; 2006;296:2209–2216. 
  
  if (outpt=="num") {
    dd_nml <- 1
    dd_grade1 <- 2
    dd_grade2 <- 3
    dd_grade3 <- 4
  }
  
  else if (outpt=="char") {
    dd_nml <- "1. Normal"
    dd_grade1 <- "2. Grade I"
    dd_grade2 <- "3. Grade II"
    dd_grade3 <- "4. Grade III"
  }
    dat$bursi_dd[dat[,dt]>140&dat[,earat]>0.75&dat[,earat]<2&dat[,eep_avg]<10] <- dd_nml
    dat$bursi_dd[dat[,earat]<=0.75&dat[,earat]<2&dat[,eep_avg]<10] <- dd_grade1
    dat$bursi_dd[dat[,dt]>140&dat[,earat]>0.75&dat[,earat]<2&dat[,eep_avg]>=10] <- dd_grade2
    dat$bursi_dd[dat[,dt]<140&dat[,earat]>2&dat[,eep_avg]>=10] <- dd_grade3
  return(dat$bursi_dd)
}

##########################

assign_diastolic_function_ase2016 <- function (dat,pid="patientid",ep_lat="eprime_lat_val",ep_sept="eprime_med_val",eep_avg="eeprime_avg_val", earat="ea_ratio_val",e_vel="ewave_val",dt="decel_time_val",lavix="lav_ix_val",tr_vel="tr_val", eep_sep = "eeprime_med_val", eep_lat="eeprime_lat_val", 
                                       atrial_fib=c("T","F"),outpt=c("num","char"),echo_flag = "echo_done") {

  if (outpt=="num") {
    dd_nml <- 1
    dd_grade1 <- 2
    dd_grade2 <- 3
    dd_grade3 <- 4
  }
  
  else if (outpt=="char") {
    dd_nml <- "1. Normal"
    dd_grade1 <- "2. Grade I"
    dd_grade2 <- "3. Grade II"
    dd_grade3 <- "4. Grade III"
  }
    
  for (i in 1:nrow(dat)) {
    if (dat[i,echo_flag]==1&!is.na(dat[i,echo_flag])) {
      dat[i,'ase2016_crit1'] <- 0
      if (dat[i,eep_avg]>14&!is.na(dat[i,eep_avg])) {dat[i,'ase2016_crit1'] <- dat[i,'ase2016_crit1']+1}
      if ((dat[i,ep_sept]<7&(!is.na(dat[i,ep_sept]))|(dat[i,ep_lat]<10&!is.na(dat[i,ep_lat])))) {dat[i,'ase2016_crit1'] <- dat[i,'ase2016_crit1']+1}
      if (dat[i,tr_vel]>2.8&!is.na(dat[i,tr_vel])) {dat[i,'ase2016_crit1'] <- dat[i,'ase2016_crit1']+1} 
      if (dat[i,lavix]>34&!is.na(dat[i,lavix])) {dat[i,'ase2016_crit1'] <- dat[i,'ase2016_crit1']+1}
      if (dat[i,'ase2016_crit1'] >= 2) {
        if (dat[i,earat] <= 0.8&dat[i,e_vel] <= 50&!is.na(dat[i,earat])&!is.na(dat[i,e_vel])) {
          dat[i,'ase2016_dd'] <- dd_grade1
        }
        
        else if ((dat[i,earat] <= 0.8&dat[i,e_vel]> 50&!is.na(dat[i,earat])&!is.na(dat[i,e_vel]))|(dat[i,earat]>0.8&dat[i,earat]<2&!is.na(dat[i,earat]))) {
          dat[i,'ase2016_crit2'] <- 0
          if (dat[i,eep_avg]>14&!is.na(dat[i,eep_avg])) {dat[i,'ase2016_crit2'] <- dat[i,'ase2016_crit2']+1}
          if (dat[i,tr_vel] > 2.8&!is.na(dat[i,tr_vel])) {dat[i,'ase2016_crit2'] <- dat[i,'ase2016_crit2']+1}         
          if (dat[i,lavix] >34&!is.na(dat[i,lavix])) {dat[i,'ase2016_crit2'] <- dat[i,'ase2016_crit2']+1}
          if (dat[i,'ase2016_crit2']<2) { dat[i,'ase2016_dd'] <- dd_grade1}
          if (dat[i,'ase2016_crit2']>=2) { dat[i,'ase2016_dd'] <- dd_grade2}
        }
        
        else if (dat[i,earat] >= 2&!is.na(dat[i,earat])) {
          dat[i,'ase2016_dd'] <- dd_grade3
        }
      }
      else if (dat[i,'ase2016_crit1'] <2) {
        dat[i,'ase2016_dd'] <- dd_nml
      }
    }
  }
  return(dat[,c(eep_avg,e_vel,earat,tr_vel,lavix,"ase2016_crit1","ase2016_crit2","ase2016_dd")])
}     

