

### Chronic risk scores: 
# MAGGIC
# SHFM
# EMPHASIS-HF
# PREDICT-HF
# BCN BIO-HF
# BIOSTAT-HF
# HFSS
# EMPEROR-REDUCED

### Acute risk scores:
# ADHERE
# GWTG
# AHEAD
# ESCAPE (hospitalized inpatients at discharge)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%                                                                      %%%
####                                MAGGIC                              ####
#%%                                                                      %%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


maggic_score <- function (dat,
                          acearb="acearb",
                          age="age_val",
                          bb="betab",
                          bmi="bmi",
                          cr="cr_val",
                          dm="dm",
                          nyha="nyha",
                          hfdur="hf_dur_val",
                          lvef="ef_val",
                          sbp="sbp_val",
                          sex="sex",
                          cur_smoke="cur_smoke",
                          copd="copd",
                          Y=c("Yes",2),
                          N=c("No",1),
                          M=c("Male",1),
                          Fe=c("Female",2))
{
 
  ## Function assumes output from HDCP Criteriaset.csv file, currently Criteriaset_multistudy HF Collaboratory
  
  ## Pocock SJ, Ariti CA, McMurray JJ V, Maggioni A, et al. 
  ## Predicting survival in heart failure: a risk score based on 39 372 patients from 30 studies. 
  ## Eur Hear J. 2013;34:1404–1413.
  
  ## Returns NA if ANY missing values
  ## Units: HF duration = months; creatinine = mg/dL; age = years; sbp = mm Hg, lvef = %
  ## LVEF: <20 (+7); 20-24 (+6); 25-29 (+5); 30-34 (+3); 35-39 (+2); >40 (0)
  ## BMI: <15 (+6); 15-19 (+5); 20-24 (+3); 25-29 (+2); 30+ (0)
  ## Cr (mg/dL): <1.02 (0); 1.02-1.23 (+1); 1.23-1.46 (+2); 1.46-1.69 (+3); 1.69-1.91 (+4); 1.91-2.36 (+5); 2.36-2.82 (+6); >2.82 (+8)
  ## Cr (umol/L): <90 (0); 90-109 (+1); 110-129 (+2); 130-149 (+3); 150-169 (+4); 170-209 (+5); 210-249 (+6); >250 (+8)
  ## NYHA: I (0); II (+2); III (+6); IV (+8)
  ## Male (+1)
  ## Current smoker (+1)
  ## Diabetic (+3)
  ## COPD (+2)
  ## HF diagnosis < 18 months (+2)
  ## NOT on beta-blocker (+3)
  ## NOT on ACE/ARB (+1)
  ##
  ## Age x EF interaction
  ## Age < 55 (0)
  ## Age 55-59 AND: EF < 30 (+1); EF 30-39 (+2); EF>40 (+3)
  ## Age 60-64 AND: EF < 30 (+2); EF 30-39 (+4); EF>40 (+5)
  ## Age 65-69 AND: EF < 30 (+4); EF 30-39 (+6); EF>40 (+7)
  ## Age 70-74 AND: EF < 30 (+6); EF 30-39 (+8); EF>40 (+9)
  ## Age 75-79 AND: EF < 30 (+8); EF 30-39 (+10); EF>40 (+12)
  ## Age 80+ AND: EF < 30 (+10); EF 30-39 (+13); EF>40 (+15)
  ##
  ## SBP x EF interaction
  ## SBP < 110 AND: EF < 30 (+5); EF 30-39 (+3); EF>40 (+2)
  ## SBP 110-119 AND: EF < 30 (+4); EF 30-39 (+2); EF>40 (+1)
  ## SBP 120-129 AND: EF < 30 (+3); EF 30-39 (+1); EF>40 (+1)
  ## SBP 130-139 AND: EF < 30 (+2); EF 30-39 (+1); EF>40 (+0)
  ## SBP 140-149 AND: EF < 30 (+1); EF 30-39 (0); EF>40 (0)
  ## SBP 150+ (AND: EF < 30 (+0); EF 30-39 (+0); EF>40 (0)
  ## 
  ## This code will include adding 0 terms for completeness
  
  ## testna <- dat[i,c(acearb,age,bb,bmi,cr,dm,nyha,hfdur,lvef,sbp,sex,tob,copd)]
  ##  thisna_num <- table(is.na(testna))['TRUE']
  dat$mgc_score <- 0
  dat$mgc_score2 <- NA
  

  ## Create table of survival estimates based on risk score.
  
  maggic_probs <- data.frame(
    maggic_score=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50),
    maggic_1yr=c(0.015, 0.016, 0.018, 0.020, 0.022, 0.024, 0.027, 0.029, 0.032, 0.036, 0.039, 0.043, 0.048, 0.052, 0.058, 0.063, 0.070, 0.077, 0.084, 0.093, 0.102, 0.111, 0.122, 0.134, 0.147, 0.160, 0.175, 0.191, 0.209, 0.227, 0.248, 0.269, 0.292, 0.316, 0.342, 0.369, 0.398, 0.427, 0.458, 0.490, 0.523, 0.557, 0.591, 0.625, 0.659, 0.692, 0.725, 0.757, 0.787, 0.816, 0.842),
    maggic_3yr=c(0.039, 0.043, 0.048, 0.052, 0.058, 0.063, 0.070, 0.077, 0.084, 0.092, 0.102, 0.111, 0.122, 0.134, 0.146, 0.160, 0.175, 0.191, 0.209, 0.227, 0.247, 0.269, 0.292, 0.316, 0.342, 0.369, 0.397, 0.427, 0.458, 0.490, 0.523, 0.556, 0.590, 0.625, 0.658, 0.692, 0.725, 0.756, 0.787, 0.815, 0.842, 0.866, 0.889, 0.908, 0.926, 0.941, 0.953, 0.964, 0.973, 0.980, 0.985)
  )
  
  
  
   #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   ##                      Ejection fraction                      ##
   #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  ## LVEF: <20 (+7); 20-24 (+6); 25-29 (+5); 30-34 (+3); 35-39 (+2); >40 (0)
  
  dat$maggic_ef_score[dat[,lvef]<20&!is.na(dat[,lvef])] <- 7
  dat$maggic_ef_score[dat[,lvef]>=20&dat[,lvef]<25&!is.na(dat[,lvef])] <- 6
  dat$maggic_ef_score[dat[,lvef]>=25&dat[,lvef]<30&!is.na(dat[,lvef])] <- 5
  dat$maggic_ef_score[dat[,lvef]>=30&dat[,lvef]<35&!is.na(dat[,lvef])] <- 3
  dat$maggic_ef_score[dat[,lvef]>=35&dat[,lvef]<40&!is.na(dat[,lvef])] <- 2
  dat$maggic_ef_score[dat[,lvef]>=40&!is.na(dat[,lvef])] <- 0
  

   #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   ##                   Ejection fraction x Age                   ##
   #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  

  ## Age < 55 (0)
  dat$maggic_age_ef_score[!is.na(dat[,age])&dat[,age]<55] <-   0


  ## Age 55-59 AND: EF < 30 (+1); EF 30-39 (+2); EF>40 (+3)
  dat$maggic_age_ef_score[dat[,age]>=55&dat[,age]<60&
                     dat[,lvef]<30&
                     !is.na(dat[,lvef])&
                     !is.na(dat[,age])] <-   1

  dat$maggic_age_ef_score[dat[,age]>=55&dat[,age]<60&
                     dat[,lvef]>=30&dat[,lvef]<40&
                     !is.na(dat[,lvef])&
                     !is.na(dat[,age])] <-   2

  dat$maggic_age_ef_score[dat[,age]>=55&dat[,age]<60&
                     dat[,lvef]>=40&
                     !is.na(dat[,lvef])&
                     !is.na(dat[,age])] <-   3


  ## Age 60-64 AND: EF < 30 (+2); EF 30-39 (+4); EF>40 (+5)
  dat$maggic_age_ef_score[dat[,age]>=60&dat[,age]<65&
                     dat[,lvef]<30&
                     !is.na(dat[,lvef])&
                     !is.na(dat[,age])] <-   2

  dat$maggic_age_ef_score[dat[,age]>=60&dat[,age]<65&
                     dat[,lvef]>=30&dat[,lvef]<40&
                     !is.na(dat[,lvef])&
                     !is.na(dat[,age])] <-   4

  dat$maggic_age_ef_score[dat[,age]>=60&dat[,age]<65&
                     dat[,lvef]>=40&
                     !is.na(dat[,lvef])&
                     !is.na(dat[,age])] <-   5


  ## Age 65-69 AND: EF < 30 (+4); EF 30-39 (+6); EF>40 (+7)
  dat$maggic_age_ef_score[dat[,age]>=65&dat[,age]<70&
                     dat[,lvef]<30&
                     !is.na(dat[,lvef])&
                     !is.na(dat[,age])] <-   4

  dat$maggic_age_ef_score[dat[,age]>=65&dat[,age]<70&
                     dat[,lvef]>=30&dat[,lvef]<40&
                     !is.na(dat[,lvef])&
                     !is.na(dat[,age])] <-   6

  dat$maggic_age_ef_score[dat[,age]>=65&dat[,age]<70&
                     dat[,lvef]>=40&
                     !is.na(dat[,lvef])&
                     !is.na(dat[,age])] <-  7


  ## Age 70-74 AND: EF < 30 (+6); EF 30-39 (+8); EF>40 (+9)
  dat$maggic_age_ef_score[dat[,age]>=70&dat[,age]<75&
                     dat[,lvef]<30&
                     !is.na(dat[,lvef])&
                     !is.na(dat[,age])] <-   6

  dat$maggic_age_ef_score[dat[,age]>=70&dat[,age]<75&
                     dat[,lvef]>=30&dat[,lvef]<40&
                     !is.na(dat[,lvef])&
                     !is.na(dat[,age])] <-   8

  dat$maggic_age_ef_score[dat[,age]>=70&dat[,age]<75&
                     dat[,lvef]>=40&
                     !is.na(dat[,lvef])&
                     !is.na(dat[,age])] <-  9


  ## Age 75-79 AND: EF < 30 (+8); EF 30-39 (+10); EF>40 (+12)
  dat$maggic_age_ef_score[dat[,age]>=75&dat[,age]<80&
                     dat[,lvef]<30&
                     !is.na(dat[,lvef])&
                     !is.na(dat[,age])] <-   8

  dat$maggic_age_ef_score[dat[,age]>=75&dat[,age]<80&
                     dat[,lvef]>=30&dat[,lvef]<40&
                     !is.na(dat[,lvef])&
                     !is.na(dat[,age])] <-   10

  dat$maggic_age_ef_score[dat[,age]>=75&dat[,age]<80&
                     dat[,lvef]>=40&
                     !is.na(dat[,lvef])&
                     !is.na(dat[,age])] <-   12


  ## Age 80+ AND: EF < 30 (+10); EF 30-39 (+13); EF>40 (+15)
  dat$maggic_age_ef_score[dat[,age]>=80&
                     dat[,lvef]<30&
                     !is.na(dat[,lvef])&
                     !is.na(dat[,age])] <-   10

  dat$maggic_age_ef_score[dat[,age]>=80&
                     dat[,lvef]>=30&dat[,lvef]<40&
                     !is.na(dat[,lvef])&
                     !is.na(dat[,age])] <-   13

  dat$maggic_age_ef_score[dat[,age]>=80&
                     dat[,lvef]>=40&
                     !is.na(dat[,lvef])&
                     !is.na(dat[,age])] <-  15


   #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   ##         Systolic blood pressure x ejection fraction         ##
   #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ## SBP < 110 AND: EF < 30 (+5); EF 30-39 (+3); EF>40 (+2)
  dat$maggic_bp_ef_score[dat[,sbp]<110&
                    dat[,lvef]<30&
                    !is.na(dat[,lvef])&
                    !is.na(dat[,sbp])] <-   5

  dat$maggic_bp_ef_score[dat[,sbp]<110&
                    dat[,lvef]>=30&dat[,lvef]<40&
                    !is.na(dat[,lvef])&
                    !is.na(dat[,sbp])] <-   3

  dat$maggic_bp_ef_score[dat[,sbp]<110&
                    dat[,lvef]>=40&
                    !is.na(dat[,lvef])&
                    !is.na(dat[,sbp])] <-   2


  ## SBP 110-119 AND: EF < 30 (+4); EF 30-39 (+2); EF>40 (+1)
  dat$maggic_bp_ef_score[dat[,sbp]>=110&dat[,sbp]<120&
                    dat[,lvef]<30&
                    !is.na(dat[,lvef])&
                    !is.na(dat[,sbp])] <-   4

  dat$maggic_bp_ef_score[dat[,sbp]>=110&dat[,sbp]<120&
                    dat[,lvef]>=30&dat[,lvef]<40&
                    !is.na(dat[,lvef])&
                    !is.na(dat[,sbp])] <-  2

  dat$maggic_bp_ef_score[dat[,sbp]>=110&dat[,sbp]<120&
                    dat[,lvef]>=40&
                    !is.na(dat[,lvef])&
                    !is.na(dat[,sbp])] <-  1

  
  ## SBP 120-129 AND: EF < 30 (+3); EF 30-39 (+1); EF>40 (+1)
  dat$maggic_bp_ef_score[dat[,sbp]>=120&dat[,sbp]<130&
                    dat[,lvef]<30&
                    !is.na(dat[,lvef])&
                    !is.na(dat[,sbp])] <-   3

  dat$maggic_bp_ef_score[dat[,sbp]>=120&dat[,sbp]<130&
                    dat[,lvef]>=30&dat[,lvef]<40&
                    !is.na(dat[,lvef])&
                    !is.na(dat[,sbp])] <-  1


  dat$maggic_bp_ef_score[dat[,sbp]>=120&dat[,sbp]<130&
                    dat[,lvef]>=40&
                    !is.na(dat[,lvef])&
                    !is.na(dat[,sbp])] <-   1


  ## SBP 130-139 AND: EF < 30 (+2); EF 30-39 (+1); EF>40 (+0)
  dat$maggic_bp_ef_score[dat[,sbp]>=130&dat[,sbp]<140&
                    dat[,lvef]<30&
                    !is.na(dat[,lvef])&
                    !is.na(dat[,sbp])] <-  2

  dat$maggic_bp_ef_score[dat[,sbp]>=130&dat[,sbp]<140&
                    dat[,lvef]>=30&dat[,lvef]<40&
                    !is.na(dat[,lvef])&
                    !is.na(dat[,sbp])] <-  1

  dat$maggic_bp_ef_score[dat[,sbp]>=130&dat[,sbp]<140&
                    dat[,lvef]>=40&
                    !is.na(dat[,lvef])&
                    !is.na(dat[,sbp])] <-  0


  ## SBP 140-149 AND: EF < 30 (+1); EF 30-39 (0); EF>40 (0)
  
  dat$maggic_bp_ef_score[dat[,sbp]>=140&dat[,sbp]<150&
                    dat[,lvef]<30&
                    !is.na(dat[,lvef])&
                    !is.na(dat[,sbp])] <-   1

  dat$maggic_bp_ef_score[dat[,sbp]>=140&dat[,sbp]<150&
                    dat[,lvef]>=30&
                    !is.na(dat[,lvef])&
                    !is.na(dat[,sbp])] <-  0


  ## SBP 150+ (AND: EF < 30 (+0); EF 30-39 (+0); EF>40 (0)
  dat$maggic_bp_ef_score[dat[,sbp]>=150&!is.na(dat[,sbp])] <-   0





  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                       Body mass index                       %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  ### BMI: <15 (+6); 15-19 (+5); 20-24 (+3); 25-29 (+2); 30+ (0)
 
  dat$maggic_bmi_score[dat[,bmi]<15&!is.na(dat[,bmi])] <-   6

  dat$maggic_bmi_score[dat[,bmi]>=15&dat[,bmi]<20&!is.na(dat[,bmi])] <- 5

  dat$maggic_bmi_score[dat[,bmi]>=20&dat[,bmi]<25&!is.na(dat[,bmi])] <- 3

  dat$maggic_bmi_score[dat[,bmi]>=25&dat[,bmi]<30&!is.na(dat[,bmi])] <- 2

  dat$maggic_bmi_score[dat[,bmi]>=30&!is.na(dat[,bmi])] <-  0



  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                          Creatinine                          %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  ### Creatinine (mg/dL): <1.02 (0); 1.02-1.23 (+1); 1.23-1.46 (+2); 1.46-1.69 (+3); 1.69-1.91 (+4); 1.91-2.36 (+5); 2.36-2.82 (+6); >2.82 (+8)


  dat$maggic_cr_score[dat[,cr]<1.02&!is.na(dat[,cr])] <-   0

  dat$maggic_cr_score[dat[,cr]>=1.02&dat[,cr]<1.23&!is.na(dat[,cr])] <-   1

  dat$maggic_cr_score[dat[,cr]>=1.23&dat[,cr]<1.46&!is.na(dat[,cr])] <-   2

  dat$maggic_cr_score[dat[,cr]>=1.46&dat[,cr]<1.69&!is.na(dat[,cr])] <-   3

  dat$maggic_cr_score[dat[,cr]>=1.69&dat[,cr]<1.91&!is.na(dat[,cr])] <-   4

  dat$maggic_cr_score[dat[,cr]>=1.91&dat[,cr]<2.36&!is.na(dat[,cr])] <-   5

  dat$maggic_cr_score[dat[,cr]>=2.36&dat[,cr]<2.81&!is.na(dat[,cr])] <-   6

  dat$maggic_cr_score[dat[,cr]>=2.81&!is.na(dat[,cr])] <-   8



  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                             NYHA                             %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  ### NYHA: I (0); II (+2); III (+6); IV (+8)


  dat$maggic_nyha_score[dat[,nyha]==1&!is.na(dat[,nyha])] <- 0

  dat$maggic_nyha_score[dat[,nyha]==2&!is.na(dat[,nyha])] <- 2

  dat$maggic_nyha_score[dat[,nyha]==3&!is.na(dat[,nyha])] <- 6

  dat$maggic_nyha_score[dat[,nyha]==4&!is.na(dat[,nyha])] <- 8



  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                             Sex                             %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  ###  Male (+1)

  dat$maggic_sex_score[dat[,sex] %in% M&!is.na(dat[,sex])] <-   1
  dat$maggic_sex_score[dat[,sex] %in% Fe&!is.na(dat[,sex])] <-  0


  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                         Tobacco use                         %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  ### Current smoker (+1)

  dat$maggic_smoke_score[dat[,cur_smoke] %in% Y&!is.na(dat[,cur_smoke])] <-   1
  dat$maggic_smoke_score[dat[,cur_smoke] %in% N&!is.na(dat[,cur_smoke])] <-   0



  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                      Diabetes mellitus                      %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  ### Diabetic (+3)

  dat$maggic_dm_score[dat[,dm] %in% Y&!is.na(dat[,dm])] <-   3
  dat$maggic_dm_score[dat[,dm] %in% N&!is.na(dat[,dm])] <-   0



  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                             COPD                             %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  ### COPD (+2)

  dat$maggic_copd_score[dat[,copd] %in% Y&!is.na(dat[,copd])] <- 2
  dat$maggic_copd_score[dat[,copd] %in% N&!is.na(dat[,copd])] <- 0



  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                        Duration of HF                        %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  ### HF diagnosis < 18 months (+2)

  dat$maggic_hfdur_score[dat[,hfdur]>=18&!is.na(dat[,hfdur])] <- 2
  dat$maggic_hfdur_score[dat[,hfdur]<18&!is.na(dat[,hfdur])] <-  0


  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                         Beta-blocker                         %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  ### NOT on beta-blocker (+3)

  dat$maggic_bb_score[dat[,bb] %in% Y&!is.na(dat[,bb])] <-   0
  dat$maggic_bb_score[dat[,bb] %in% N&!is.na(dat[,bb])] <-   3



  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                           ACEI/ARB                           %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  ### NOT on ACE/ARB (+1)

  dat$maggic_acearb_score[dat[,acearb] %in% Y&!is.na(dat[,acearb])] <-   0
  dat$maggic_acearb_score[dat[,acearb] %in% N&!is.na(dat[,acearb])] <-   1

  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                       Calculate score                       %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  for (i in 1:nrow(dat)) {
    dat[i,"maggic_score"] <- sum(dat[i,"maggic_acearb_score"],
                              dat[i,"maggic_bb_score"],
                              dat[i,"maggic_hfdur_score"],
                              dat[i,"maggic_copd_score"],
                              dat[i,"maggic_dm_score"],
                              dat[i,"maggic_smoke_score"],
                              dat[i,"maggic_sex_score"],
                              dat[i,"maggic_nyha_score"],
                              dat[i,"maggic_cr_score"],
                              dat[i,"maggic_bmi_score"],
                              dat[i,"maggic_bp_ef_score"],
                              dat[i,"maggic_age_ef_score"],
                              dat[i,"maggic_ef_score"],
                              na.rm=T)

    dat[i,"maggic_score_nas"] <-  is.na(dat[i,acearb])+
                                  is.na(dat[i,age])+
                                  is.na(dat[i,bb])+
                                  is.na(dat[i,bmi])+
                                  is.na(dat[i,cr])+
                                  is.na(dat[i,dm])+
                                  is.na(dat[i,nyha])+
                                  is.na(dat[i,hfdur])+
                                  is.na(dat[i,lvef])+
                                  is.na(dat[i,sbp])+
                                  is.na(dat[i,sex])+
                                  is.na(dat[i,cur_smoke])+
                                  is.na(dat[i,copd])

}

  maggic_result <- merge(dat,maggic_probs,
                         by="maggic_score",
                         all.x=T,
                         sort=F)
  
  return(maggic_result[,c('patientid',
                       'study',
                       'maggic_score',
                       'maggic_1yr',
                       'maggic_3yr',
                       'maggic_score_nas')])
  
}  




##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###                                                                     ###
####                     SEATTLE HEART FAILURE MODEL                  #####
###                                                                     ###
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

shfm_score <- function (dat,age="age_val",
                        sex="sex",
                        nyha="nyha",
                        lvef="ef_val",
                        ischemic_cm="ischemic_cm",
                        sbp="sbp_val",
                        diuretic_dose="diur_dose",
                        na="na",
                        chol="chol",
                        hgb="hgb",
                        lymph="lymph",
                        uric="uric",
                        betab="betab",
                        acei="acei",
                        arb="arb",
                        k_diuretic="mra",
                        allo="allopurinol",
                        statin="statin",
                        crt="crt",
                        crtd="crtd",
                        icd="icd",
                        Y=c("Yes",2),
                        N=c("No",1),
                        M=c("Male",1),
                        Fe=c("Female",2),
                        yr_est=1)
{
  
  # Calculated using coefficients and example in primary publication:
  # Levy WC, Mozaffarian D, Linker DT, Sutradhar SC, Anker SD, Cropp AB, Anand IS, 
  # Maggioni A, Burton P, Sullivan MD, Pitt B, Poole-Wilson PA, Mann DL, Packer, M. 
  # The Seattle Heart Failure Model: Prediction of survival in heart failure. 
  # Circulation. 2006;113(11):1424–1433. 
  
  
    
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                             Age                             %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat$shfm_age[!is.na(dat[,age])] <- 
    dat[!is.na(dat[,age]),age]/10 * log(1.09)


  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                             Sex                             %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat$shfm_sex[!is.na(dat[,sex])&dat[,sex] %in% M] <- log(1.089)
  dat$shfm_sex[!is.na(dat[,sex])&dat[,sex] %in% F] <- 0
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                      Ejection fraction                      %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat$shfm_ef[!is.na(dat[,lvef])] <- 100/dat[!is.na(dat[,lvef]),lvef] * log(1.03)

  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                   Systolic blood pressure                   %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat$shfm_sbp[!is.na(dat[,sbp])&dat[,sbp]<160] <- dat[!is.na(dat[,sbp])&dat[,sbp]<160,sbp]/10 * log(0.877)
  dat$shfm_sbp[!is.na(dat[,sbp])&dat[,sbp]>=160] <- 0
  
    
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                      Ischemic etiology                      %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat$shfm_ischemic_cm[!is.na(dat[,ischemic_cm])&dat[,ischemic_cm] %in% Y] <- log(1.354)
  dat$shfm_ischemic_cm[!is.na(dat[,ischemic_cm])&dat[,ischemic_cm] %in% N] <- 0
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                             NYHA                             %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat$shfm_nyha[!is.na(dat[,nyha])] <- dat[!is.na(dat[,nyha]),nyha] * log(1.60)
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                        Sodium, mEq/L                        %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat$shfm_na[!is.na(dat[,na])&dat[,na]<138] <- (138-dat[!is.na(dat[,na])&dat[,na]<138,na]) * log(1.050)
  dat$shfm_na[!is.na(dat[,na])&dat[,na]>=138] <- 0
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                  Total cholesterol, mg/dL                   %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat$shfm_chol[!is.na(dat[,chol])] <- 100/dat[!is.na(dat[,chol]),chol] * log(2.206)

  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                       Hemoglobin, g/dL                       %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat$shfm_hgb[!is.na(dat[,hgb])&dat[,hgb]<=16] <- 
    (16-dat[!is.na(dat[,hgb])&dat[,hgb]<=16,hgb]) * log(1.124)
  
  dat$shfm_hgb[!is.na(dat[,hgb])&dat[,hgb]>16] <- 
    (dat[!is.na(dat[,hgb])&dat[,hgb]>16,hgb]-16) * log(1.336)
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                        % lymphocytes                        %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat$shfm_lymph[!is.na(dat[,lymph])&dat[,lymph]<47] <- (dat[!is.na(dat[,lymph])&dat[,lymph]<47,lymph])/5 * log(0.897)
  dat$shfm_lymph[!is.na(dat[,lymph])&dat[,lymph]>=47] <- 0
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                       Uric acid, mg/dL                       %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat$shfm_uric[!is.na(dat[,uric])&dat[,uric] > 3.4] <- (dat[!is.na(dat[,uric])&dat[,uric]>3.4,uric]) * log(1.064)
  dat$shfm_uric[!is.na(dat[,uric])&dat[,uric] <= 3.4] <- 0
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                         Beta-blocker                         %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat$shfm_betab[!is.na(dat[,betab])&dat[,betab] %in% Y] <- log(0.66)
  dat$shfm_betab[!is.na(dat[,betab])&dat[,betab] %in% N] <- 0
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                        ACE-inhibitor                        %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat$shfm_acei[!is.na(dat[,acei])&dat[,acei] %in% Y] <- log(0.77)
  dat$shfm_acei[!is.na(dat[,acei])&dat[,acei] %in% N] <- 0
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                 Angiotensin receptor blocker                 %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat$shfm_arb[!is.na(dat[,arb])&dat[,arb] %in% Y] <- log(0.85)
  dat$shfm_arb[!is.na(dat[,arb])&dat[,arb] %in% N] <- 0
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                         Allopurinol                         %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat$shfm_allo[!is.na(dat[,allo])&dat[,allo] %in% Y] <- log(1.571)
  dat$shfm_allo[!is.na(dat[,allo])&dat[,allo] %in% N] <- 0
  
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                K-sparing diuretic (using MRA)                %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat$shfm_kdiur[!is.na(dat[,k_diuretic])&dat[,k_diuretic] %in% Y] <- log(0.74)
  dat$shfm_kdiur[!is.na(dat[,k_diuretic])&dat[,k_diuretic] %in% N] <- 0
  
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                            Statin                            %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat$shfm_statin[!is.na(dat[,statin])&dat[,statin] %in% Y] <- log(0.63)
  dat$shfm_statin[!is.na(dat[,statin])&dat[,statin] %in% N] <- 0
  
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##       Diuretic dose mg/kg/day (furosemide equivalent)       %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat$shfm_diuretic_dose[!is.na(dat[,diuretic_dose])] <- dat[!is.na(dat[,diuretic_dose]),diuretic_dose]*log(1.178) 


    
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                             ICD                             %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat$shfm_icd[!is.na(dat[,icd])&
                 dat[,icd] %in% Y&
                 (is.na(dat[,crt])|
                    dat[,crt] %in% N)&
                 (dat[,crtd] %in% N|
                    is.na(dat[,crtd]))] <- log(0.73)

  dat$shfm_icd[!is.na(dat[,icd])&
                 dat[,icd] %in% N] <- 0
                 
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                            CRT-D                            %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat$shfm_crtd[(!is.na(dat[,crtd])&
                  dat[,crtd] %in% Y)|
                  (!is.na(dat[,crt])&
                     dat[,crt] %in% Y&
                  !is.na(dat[,icd])&
                  dat[,icd] %in% Y)] <- log(0.79)

  dat$shfm_crtd[(!is.na(dat[,crtd])&
                   dat[,crtd] %in% N)|
                  (!is.na(dat[,crt])&
                     dat[,crt] %in% N)|
                     (!is.na(dat[,icd])&
                     dat[,icd] %in% N)] <- 0
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                     Calculate SHFM score                     %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  for (i in 1:nrow(dat)) {

  dat[i,"shfm_score"] <-sum(dat[i,"shfm_age"],
                       dat[i,"shfm_sex"],
                       dat[i,"shfm_ef"],
                       dat[i,"shfm_sbp"],
                       dat[i,"shfm_ischemic_cm"],
                       dat[i,"shfm_nyha"],
                       dat[i,"shfm_na"],
                       dat[i,"shfm_hgb"],
                       dat[i,"shfm_chol"],
                       dat[i,"shfm_uric"],
                       dat[i,"shfm_acei"],
                       dat[i,"shfm_allo"],
                       dat[i,"shfm_arb"],
                       dat[i,"shfm_kdiur"],
                       dat[i,"shfm_statin"],
                       dat[i,"shfm_diuretic_dose"],
                       dat[i,"shfm_betab"],
                       dat[i,"shfm_lymph"],
                       dat[i,"shfm_icd"],
                       dat[i,"shfm_crtd"],
                       na.rm=T)
  
dat[i,"shfm_score_nas"] <- sum(is.na(dat[i,"shfm_age"]),
                          is.na(dat[i,"shfm_sex"]),
                          is.na(dat[i,"shfm_ef"]),
                          is.na(dat[i,"shfm_sbp"]),
                          is.na(dat[i,"shfm_ischemic_cm"]),
                          is.na(dat[i,"shfm_nyha"]),
                          is.na(dat[i,"shfm_na"]),
                          is.na(dat[i,"shfm_hgb"]),
                          is.na(dat[i,"shfm_chol"]),
                          is.na(dat[i,"shfm_uric"]),
                          is.na(dat[i,"shfm_acei"]),
                          is.na(dat[i,"shfm_allo"]),
                          is.na(dat[i,"shfm_arb"]),
                          is.na(dat[i,"shfm_kdiur"]),
                          is.na(dat[i,"shfm_statin"]),
                          is.na(dat[i,"shfm_diuretic_dose"]),
                          is.na(dat[i,"shfm_betab"]),
                          is.na(dat[i,"shfm_lymph"]),
                          is.na(dat[i,"shfm_icd"]),
                          is.na(dat[i,"shfm_crtd"]),
                          na.rm=T)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##    Calculate SHFM estimate for survival at specified year    %%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dat[i,"shfm_yrest"] <- round(exp((-0.0405*yr_est)*exp(dat[i,"shfm_score"])),3)

  }
  
  return(dat)
  
}



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###                                                                     ###
####                             EMPHASIS-HF                           ####
###                                                                     ###
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

emphasis_score <- function(dat, 
                           age_val="age_val",
                           sex="sex",
                           sbp="sbp_val",
                           gfr="crcl", 
                           dm="dm", 
                           prior_hfh = "prior_hfh",
                           hgb="hgb",
                           hct="hct",
                           mi="mi",
                           cabg="cabg",
                           bmi_val="bmi_val",
                           hr_val="hr_val") 
{
  
  ## Calculates EMPHASIS-HF score according to Collier TJ, Pocock SJ, McMurray JJV, 
  ## Collier TJ, Pocock SJ, McMurray JVV, Zannad F, Krum H, Veldhuisen DJ Van, Swedberg K, Shi H, Vincent J, Pitt B. 
  ## The impact of eplerenone at different levels of risk in patients with systolic heart failure 
  ## and mild symptoms: Insight from a novel risk score for prognosis derived from the EMPHASIS-HF trial. 
  ## Eur Heart J. 2013;34:2823–2829. 
  
  ## Returns full data frame + each component of emphasis score + total score.

  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                             Age                             %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat[dat[,age_val]<75&!is.na(dat[,age_val]),'emphasis_age'] <- 0
  dat[dat[,age_val]>=75&!is.na(dat[,age_val]),'emphasis_age'] <- 1
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                             Sex                             %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat[dat[,sex]  %in% c("2","Female")&!is.na(dat[,age_val]),'emphasis_sex'] <- 0
  dat[dat[,sex]  %in% c("1","Male")&!is.na(dat[,age_val]),'emphasis_sex']  <- 1
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                         Systolic BP                         %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat[dat[,sbp]<130&!is.na(dat[,sbp]),'emphasis_sbp'] <- 1
  dat[dat[,sbp]>=130&!is.na(dat[,sbp]),'emphasis_sbp'] <- 0
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                  Glomerular filtration rate                  %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat[dat[,gfr]<60&!is.na(dat[,gfr]),'emphasis_gfr'] <- 2
  dat[dat[,gfr]>60&dat[,gfr]<70&!is.na(dat[,gfr]),'emphasis_gfr'] <- 1
  dat[dat[,gfr]>=70&!is.na(dat[,gfr]),'emphasis_gfr'] <- 0
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                      Diabetes mellitus                      %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat[dat[,dm]  %in% c("2","Yes")&!is.na(dat[,dm]),'emphasis_dm'] <- 1
  dat[dat[,dm]  %in% c("1","No")&!is.na(dat[,dm]),'emphasis_dm']  <- 0
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##            Previous heart failure hospitalization            %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat[dat[,prior_hfh]  %in% c("2","Yes")&!is.na(dat[,prior_hfh]),'emphasis_hfh'] <- 1
  dat[dat[,prior_hfh]  %in% c("1","No")&!is.na(dat[,prior_hfh]),'emphasis_hfh']  <- 0
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                          Hemoglobin                          %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat[dat[,hgb]<11&!is.na(dat[,hgb]),'emphasis_hgb'] <- 2
  dat[dat[,hgb]>11&dat[,hgb]<13&!is.na(dat[,hgb]),'emphasis_hgb'] <- 1
  dat[dat[,hgb]>=13&!is.na(dat[,hgb]),'emphasis_hgb'] <- 0
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                          MI or CABG                          %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat[(dat[,mi] %in% c("2","Yes")&
         !is.na(dat[,mi]))|
        (dat[,cabg] %in% c("2","Yes")&
           !is.na(dat[,cabg])),
      'emphasis_micabg'] <- 1
  
    dat[dat[,mi] %in% N&
          dat[,cabg] %in% N&
          !is.na(dat[,mi])&
          !is.na(dat[,cabg]),
        'emphasis_micabg'] <- 0
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                       Body mass index                       %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat[dat[,bmi_val]<25&!is.na(dat[,bmi_val]),'emphasis_bmi'] <- 1
  dat[dat[,bmi_val]>=25&!is.na(dat[,bmi_val]),'emphasis_bmi'] <- 0
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                          Heart rate                          %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  dat[dat[,hr_val]<80&!is.na(dat[,hr_val]),'emphasis_hr'] <- 0
  dat[dat[,hr_val]>=80&!is.na(dat[,hr_val]),'emphasis_hr'] <- 1
  
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                 Calculate EMPHASIS-HF score                 %%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  for (i in 1:nrow(dat)) {
  dat[i,"emphasis_score"] <- sum(dat[i,"emphasis_age"],
                        dat[i,"emphasis_sex"], 
                        dat[i,"emphasis_sbp"], 
                        dat[i,"emphasis_gfr"], 
                        dat[i,"emphasis_dm"], 
                        dat[i,"emphasis_hfh"], 
                        dat[i,"emphasis_hgb"], 
                        dat[i,"emphasis_micabg"], 
                        dat[i,"emphasis_bmi"], 
                        dat[i,"emphasis_hr"],
                        na.rm=T)
  
  
  dat[i,"emphasis_score_nas"] <- sum(is.na(dat[i,"emphasis_age"]),
                                is.na(dat[i,"emphasis_sex"]), 
                                is.na(dat[i,"emphasis_sbp"]), 
                                is.na(dat[i,"emphasis_gfr"]), 
                                is.na(dat[i,"emphasis_dm"]), 
                                is.na(dat[i,"emphasis_hfh"]), 
                                is.na(dat[i,"emphasis_hgb"]), 
                                is.na(dat[i,"emphasis_micabg"]), 
                                is.na(dat[i,"emphasis_bmi"]), 
                                is.na(dat[i,"emphasis_hr"]),
                                na.rm=T)

  }
  
  # Mortality estimates:
  # Low risk (0-4): 13.4% in placebo, 11.4% in eplerenone, 12.4% combined
  # Med risk (5-6): 32.0% in placebo, 21.1% in eplerenone, 26.8% combined
  # Hi risk (7-12): 54.1% in placebo, 39.2% in eplerenone, 46.3% combined
  
  # Will return rates from placebo arm only
  
  dat$emphasis_cvm_hfh_2yr[dat$emphasis_score<=4] <- 0.134
  dat$emphasis_cvm_hfh_2yr[between(dat$emphasis_score,5,6)] <- 0.32
  dat$emphasis_cvm_hfh_2yr[dat$emphasis_score >= 7] <- 0.541
  
  return(dat)
  
  }
  

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###                                                                      ###
####                              PREDICT-HF                            ####
###                                                                      ###
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

predicthf_score <- function (dat,
                             age="age_val",
                             sex="sex",
                             race="race",
                             dm="dm",
                             nyha="nyha",
                             pci="pci",
                             sbp="sbp_val",
                             bmi="bmi_val",
                             region="region",
                             hr="hr",
                             lvef="ef_val",
                             ischemic="ischemic_cm",
                             hf_duration="hf_dur",
                             arni="arni",
                             na="sodium_val",
                             valv_dz = "valvdz",
                             betab="betab",
                             pad="pvd",
                             af="af",
                             bbb="bbb",
                             mi="mi",
                             tbili="tbili",   ## Add as phenotype
                             ast="ast",   ## Add as phenotype
                             mono="mono_pct", ## Add as phenotype
                             tg="tg",     ## Add as phenotype     
                             uric="uric",
                             alb="albumin",   ## Add as phenotype
                             k="potassium",   ## Add as phenotype
                             bnp="bnp_val",
                             ntbnp="ntbnp_val",
                             llt="statin", 
                             ckd="ckd",
                             prior_hfh="prior_hfh",
                             cl="chloride",   ## Add as phenotype
                             anc="anc",   ## Add as phenotype
                             hgb="hgb",   ## Add as phenotype
                             bun="bun_val",
                             ldl="ldl_val",   ## Add as phenotype
                             tchol="tchol",  ## Add as phenotype
                             alc="alc",   ## Add as phenotype
                             bnp_type="bnp",
                             latin_america=c(5, "5. Latin America"),
                             central_europe=c(2, "2. Central Europe"),
                             asia=c(4,"4. Asia"),
                             Y=c("Yes",2),
                             N=c("No",1),
                             M=c("Male",1),
                             Fe=c("Female",2),
                             white=c("1. White",1),
                             black=c("2. Black",2),
                             hispanic=c("3. Hispanic",3),
                             asian=c("4. Asian", 4),
                             other_race = c("6. Other", 6),
                             endpoint=c("cvmhfh",
                                        "cvm",
                                        "acm"),
                             max_missing_vals=0)

{

  ## Calculates risk of clinical endpoints in patients with LVEF < 40% as derived in 
  ## Simpson J, Jhund PS, Lund LH, et al. Prognostic Models Derived in PARADIGM-HF 
  ## and Validated in ATMOSPHERE and the Swedish Heart Failure Registry to Predict 
  ## Mortality and Morbidity in Chronic Heart Failure. JAMA Cardiology, 5(4), 432–441
  
  ## There are several models comprising 3 outcomes (CVM/HFH, CVM, ACM) with NTBNP, BNP, and no NP

  # Covariate means (for survival estimate): 
  # Age = 66.8
  # Sex = Male
  # Race = White
  # Region = Western Europe
  # HF diagnosis = 1-5 years
  # NYHA class = I/II
  # Prior admission for HF = No
  # Treatment with ARNI = No
  # Treatment with BB = No
  # Diabetes mellitus = No
  # Peripheral arterial disease = No
  # Myocardial infarction = No
  # Valvular heart disease = No
  # Bundle branch block = No
  # Prior PCI = No
  # LVEF = 29.5%
  # SBP = 120 mm Hg
  # Potassium = 4.5
  # BUN = 20.6 mg/dL
  # Uric acid = 6.89 mg/dL
  # Albumin = 4.2 g/dL
  # Total bilirubin = 0.63 mg/dL
  # Total cholesterol = 184.45 mg/dL
  # LDL = 98.61 mg/dL 
  # Hemoglobin = 13.94 g/dL
  # Absolute neutrophil count = 4.22
  # Absolute lymphocyte count = 0.72
  
  # Total survival score at means:
  # CVM/HFH model:
  # NT-BNP model: 2.553898
  # BNP model: 2.245007
  # No NP model: 2.260930
  
  # CVM model:
  # NT-BNP model: 3.660954
  # BNP model: 4.481415
  # No NP model: 3.198330
  
  # ACM model:
  # NT-BNP model: 4.050581
  # BNP model: 3.757179
  # No NP model: 3.620852
  
  
  # With covariate mean values taken from web calculator
  # No NP 
  # CVM:      1 yr 3.3%     2 yr 6.44%
  # CVM/HFH:  1 yr 7.97%    2 yr 14.26%
  # 
  
  # BNP (default 250)
  # CVM:      1 yr 3.35%     2 yr 6.57%
  # CVM/HFH:  1 yr 7.63%    2 yr 13.76%
  
  
  # NT-BNP (default 800)
  # CVM:      1 yr 2.84%     2 yr 5.54%
  # CVM/HFH:  1 yr 5.56%    2 yr 10.06%
  
  ## First, we will count the number of NAs for each model. 
  ## Models themselves will default to missing data = unity (i.e. no change in probability)
  ## Requirements/tolerance for missing data will be up to provider, though will default to 2.
  
  
## Now will calculate all scores by summing logs
## Technically, need a delta coefficient to estimate event rate at given time.
## As of May 9 2022, I do not hae those yet.
## The remainder of the equation should still be usable to differentiate risk.
## Issue is the point estimate, which may be off.
    

    ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ##                             Age                             %%
    ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    
    ##%%%%%%%%%%%%
    ##  NT-BNP  %%
    ##%%%%%%%%%%%%
    
    
    ####### Very surprisingly, age is not a predictor of CVM/HFH in their analysis if NT-proBNP is available. 
    
    #### CVM
  
  dat$predicthf_age_cvm_ntbnp[!is.na(dat[,age])&
                                dat[,age]>60] <-  dat[!is.na(dat[,age])&
                                                  dat[,age]>60,age]/10 * log(1.15)
  
  dat$predicthf_age_cvm_ntbnp[!is.na(dat[,age])&dat[,age]<=60] <- 0
  
    ## ACM  
        
      dat$predicthf_age_acm_ntbnp[!is.na(dat[,age])&
                                    dat[,age]>60] <-  dat[!is.na(dat[,age])&
                                                      dat[,age]>60,age]/10 * log(1.20)
     
       dat$predicthf_age_acm_ntbnp[!is.na(dat[,age])&dat[,age]<=60] <- 0

       
       ##%%%%%%%%%
       ##  BNP  %%
       ##%%%%%%%%%
       
       
    ## CVM

      dat$predicthf_age_cvm_bnp[!is.na(dat[,age])&dat[,age]>60] <-  dat[!is.na(dat[,age])&
                                                                    dat[,age]>60,age]/10 * log(1.18)
       
      dat$predicthf_age_cvm_bnp[!is.na(dat[,age])&dat[,age]<=60] <- 0
      
      
    ## ACM
  
      dat$predicthf_age_acm_bnp[!is.na(dat[,age])&dat[,age]>60] <-  dat[!is.na(dat[,age])&
                                                                    dat[,age]>60,age]/10 * log(1.23)

      dat$predicthf_age_acm_bnp[!is.na(dat[,age])&dat[,age]<=60] <- 0
  
      
            
            ##%%%%%%%%%%%%
            ##  No BNP  %%
            ##%%%%%%%%%%%%
            
            
    ## CVM/HFH
      
      dat$predicthf_age_cvmhfh_nobnp[!is.na(dat[,age])&dat[,age]>60] <-   dat[!is.na(dat[,age])&
                                                                          dat[,age]>60,age]/10 * log(1.15)
      
      dat$predicthf_age_cvmhfh_nobnp[!is.na(dat[,age])&dat[,age]<=60] <-  0

    ## CVM    
        
      dat$predicthf_age_cvm_nobnp[!is.na(dat[,age])&dat[,age]>60] <-    dat[!is.na(dat[,age])&
                                                                        dat[,age]>60,age]/10 * log(1.27)
      
      dat$predicthf_age_cvm_nobnp[!is.na(dat[,age])&dat[,age]<=60] <-   0

    ## ACM  
        
      dat$predicthf_age_acm_nobnp[!is.na(dat[,age])&dat[,age]>60] <-    dat[!is.na(dat[,age])&dat[,age]>60,age]/10 * log(1.33)
      
      dat$predicthf_age_acm_nobnp[!is.na(dat[,age])&dat[,age]<=60] <-   0
      
      
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                             Sex                             ##
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
    ## CVM/HFH
    
      dat$predicthf_sex_cvmhfh_ntbnp[!is.na(dat[,sex])&dat[,sex] %in% M] <- log(1.22)
      
      dat$predicthf_sex_cvmhfh_ntbnp[!is.na(dat[,sex])&dat[,sex] %in% Fe] <- 0
  
    ## CVM
    
      dat$predicthf_sex_cvm_ntbnp[!is.na(dat[,sex])&dat[,sex] %in% M] <-  log(1.37)
      
      dat$predicthf_sex_cvm_ntbnp[!is.na(dat[,sex])&dat[,sex] %in% Fe] <- 0

    ## ACM
  
      dat$predicthf_sex_acm_ntbnp[!is.na(dat[,sex])&dat[,sex] %in% M] <-  log(1.34)
      
      dat$predicthf_sex_acm_ntbnp[!is.na(dat[,sex])&dat[,sex] %in% Fe] <- 0

      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
    ## CVM/HFH  
  
      dat$predicthf_sex_cvmhfh_bnp[!is.na(dat[,sex])&dat[,sex] %in% M] <-   log(1.19)
      
      dat$predicthf_sex_cvmhfh_bnp[!is.na(dat[,sex])&dat[,sex] %in% Fe] <-  0

      
    ## CVM  
  
      dat$predicthf_sex_cvm_bnp[!is.na(dat[,sex])&dat[,sex] %in% M] <-    log(1.33)
      
      dat$predicthf_sex_cvm_bnp[!is.na(dat[,sex])&dat[,sex] %in% Fe] <-   0

      
    ## ACM  
  
      dat$predicthf_sex_acm_bnp[!is.na(dat[,sex])&dat[,sex] %in% M] <-    log(1.31)
      
      dat$predicthf_sex_acm_bnp[!is.na(dat[,sex])&dat[,sex] %in% Fe] <-   0
  

      ##%%%%%%%%%%%%
      ##  No BNP  %%
      ##%%%%%%%%%%%%
      
      
    ## CVM/HFH      
        
      dat$predicthf_sex_cvmhfh_nobnp[!is.na(dat[,sex])&dat[,sex] %in% M] <- log(1.26)
      
      dat$predicthf_sex_cvmhfh_nobnp[!is.na(dat[,sex])&dat[,sex] %in% Fe] <- 0
  
    ## CVM
      
      dat$predicthf_sex_cvm_nobnp[!is.na(dat[,sex])&
                                  dat[,sex] %in% M] <- log(1.41)
      
      dat$predicthf_sex_cvm_nobnp[!is.na(dat[,sex])&dat[,sex] %in% Fe] <- 0
  
    ## ACM
        
      dat$predicthf_sex_acm_nobnp[!is.na(dat[,sex])&dat[,sex] %in% M] <- log(1.43)
      
      dat$predicthf_sex_acm_nobnp[!is.na(dat[,sex])&dat[,sex] %in% Fe] <- 0
  

      
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  ##                             Race                             ##
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
    ## CVM/HFH
  
      dat$predicthf_race_cvmhfh_ntbnp[!is.na(dat[,race])&dat[,race] %in% black] <- log(1.67)
  
      dat$predicthf_race_cvmhfh_ntbnp[!is.na(dat[,race])&dat[,race] %in% asian] <- log(1.43)
  
      dat$predicthf_race_cvmhfh_ntbnp[!is.na(dat[,race])&
                                        dat[,race] %in% c(white,hispanic,other_race)] <- 0
  
    ## CVM 
  
      dat$predicthf_race_cvm_ntbnp[!is.na(dat[,race])&
                                        dat[,race] %in% asian] <- log(1.89)
  
      dat$predicthf_race_cvm_ntbnp[!is.na(dat[,race])&
                                        dat[,race] %in% c(white,black,hispanic,other_race)] <- 0

    ## ACM
  
      dat$predicthf_race_acm_ntbnp[!is.na(dat[,race])&
                                 dat[,race] %in% asian] <- log(1.42)
  
      dat$predicthf_race_acm_ntbnp[!is.na(dat[,race])&
                                 dat[,race] %in% c(white,black,hispanic,other_race)] <- 0
  
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
 
     ## CVM/HFH
      
        dat$predicthf_race_cvmhfh_bnp[!is.na(dat[,race])&
                                              dat[,race] %in% black] <- log(1.58)
        
        dat$predicthf_race_cvmhfh_bnp[!is.na(dat[,race])&
                                              dat[,race] %in% asian] <- log(1.39)
        
        dat$predicthf_race_cvmhfh_bnp[!is.na(dat[,race])&
                                              dat[,race] %in% c(white,hispanic,other_race)] <- 0
  
     ## CVM 
        
        dat$predicthf_race_cvmhfh_bnp[!is.na(dat[,race])&
                                            dat[,race] %in% black] <- log(1.31)
  
        dat$predicthf_race_cvm_bnp[!is.na(dat[,race])&
                                       dat[,race] %in% asian] <- log(1.85)
        
        dat$predicthf_race_cvm_bnp[!is.na(dat[,race])&
                                       dat[,race] %in% c(white,hispanic,other_race)] <- 0
  
     ## ACM
  
        dat$predicthf_race_acm_bnp[!is.na(dat[,race])&
                                       dat[,race] %in% asian] <- log(1.38)
        
        dat$predicthf_race_acm_bnp[!is.na(dat[,race])&
                                       dat[,race] %in% c(white,black,hispanic,other_race)] <- 0
  
  
        
        
        ##%%%%%%%%%%%%
        ##  No BNP  %%
        ##%%%%%%%%%%%%
        
        
        
     ## CVM/HFH
        
        dat$predicthf_race_cvmhfh_nobnp[!is.na(dat[,race])&
                                            dat[,race] %in% black] <- log(1.81)
        
        dat$predicthf_race_cvmhfh_nobnp[!is.na(dat[,race])&
                                            dat[,race] %in% asian] <- log(1.57)
        
        dat$predicthf_race_cvmhfh_nobnp[!is.na(dat[,race])&
                                            dat[,race] %in% c(white,hispanic,other_race)] <- 0
        
     ## CVM 
        
        dat$predicthf_race_cvmhfh_nobnp[!is.na(dat[,race])&
                                            dat[,race] %in% black] <- log(1.44)
        
        dat$predicthf_race_cvm_nobnp[!is.na(dat[,race])&
                                     dat[,race] %in% asian] <- log(1.82)
        
        dat$predicthf_race_cvm_nobnp[!is.na(dat[,race])&
                                     dat[,race] %in% c(white,hispanic,other_race)] <- 0
        
     ## ACM
        
        dat$predicthf_race_acm_nobnp[!is.na(dat[,race])&
                                     dat[,race] %in% asian] <- log(1.35)
        
        dat$predicthf_race_acm_nobnp[!is.na(dat[,race])&
                                     dat[,race] %in% c(white,black,hispanic,other_race)] <- 0
        

        
                
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                      Diabetes mellitus                      ##
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
        
        
        ##%%%%%%%%%%%%
        ##  NT-BNP  %%
        ##%%%%%%%%%%%%
        
        
    ## CVM/HFH
        
      dat$predicthf_dm_cvmhfh_ntbnp[!is.na(dat[,dm])&
                                             dat[,dm] %in% Y] <- log(1.36)
        
      dat$predicthf_dm_cvmhfh_ntbnp[!is.na(dat[,dm])&
                                             dat[,dm] %in% N] <- 0
        
    ## CVM
        
      dat$predicthf_dm_cvm_ntbnp[!is.na(dat[,dm])&
                                      dat[,dm] %in% Y] <- log(1.26)
        
      dat$predicthf_dm_cvm_ntbnp[!is.na(dat[,dm])&
                                      dat[,dm] %in% N] <- 0
        
    ## ACM
        
      dat$predicthf_dm_acm_ntbnp[!is.na(dat[,dm])&
                                      dat[,dm] %in% Y] <- log(1.26)
        
      dat$predicthf_dm_acm_ntbnp[!is.na(dat[,dm])&
                                      dat[,dm] %in% N] <- 0

      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
     ## CVM/HFH
      
      dat$predicthf_dm_cvmhfh_bnp[!is.na(dat[,dm])&
                                           dat[,dm] %in% Y] <- log(1.35)
      
      dat$predicthf_dm_cvmhfh_bnp[!is.na(dat[,dm])&
                                           dat[,dm] %in% N] <- 0
      
     ## CVM
      
      dat$predicthf_dm_cvm_bnp[!is.na(dat[,dm])&
                                    dat[,dm] %in% Y] <- log(1.26)
      
      dat$predicthf_dm_cvm_bnp[!is.na(dat[,dm])&
                                    dat[,dm] %in% N] <- 0
      
     ## ACM
      
      dat$predicthf_dm_acm_bnp[!is.na(dat[,dm])&
                                    dat[,dm] %in% Y] <- log(1.25)
      
      dat$predicthf_dm_acm_bnp[!is.na(dat[,dm])&
                                    dat[,dm] %in% N] <- 0
      
  
      
      
      ##%%%%%%%%%%%%
      ##  No BNP  %%
      ##%%%%%%%%%%%%
      
      
      
    ## CVM/HFH
      
      dat$predicthf_dm_cvmhfh_nobnp[!is.na(dat[,dm])&
                                           dat[,dm] %in% Y] <- log(1.37)
      
      dat$predicthf_dm_cvmhfh_nobnp[!is.na(dat[,dm])&
                                           dat[,dm] %in% N] <- 0
      
    ## CVM
      
      dat$predicthf_dm_cvm_nobnp[!is.na(dat[,dm])&
                                      dat[,dm] %in% Y] <- log(1.27)
        
      dat$predicthf_dm_cvm_nobnp[!is.na(dat[,dm])&
                                      dat[,dm] %in% N] <- 0
      
    ## ACM
      
      dat$predicthf_dm_acm_nobnp[!is.na(dat[,dm])&
                                      dat[,dm] %in% Y] <- log(1.29)
        
      dat$predicthf_dm_acm_nobnp[!is.na(dat[,dm])&
                                      dat[,dm] %in% N] <- 0
      
      

        
      
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  ##                             NYHA                             ##
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
    ## CVM/HFH
      
      dat$predicthf_nyha_cvmhfh_ntbnp[!is.na(dat[,nyha])&
                                           dat[,nyha] %in% c(1,2)] <- 0
      
      dat$predicthf_nyha_cvmhfh_ntbnp[!is.na(dat[,nyha])&        
                                           dat[,nyha] %in% c(3,4)] <- log(1.24)
      
    ## CVM
      
      dat$predicthf_nyha_cvm_ntbnp[!is.na(dat[,nyha])&
                                    dat[,nyha] %in% c(1,2)] <- 0
      
      dat$predicthf_nyha_cvm_ntbnp[!is.na(dat[nyha])&
                                    dat[,nyha] %in% c(3,4)] <- log(1.32)
      
    ## ACM
      
      dat$predicthf_nyha_acm_ntbnp[!is.na(dat[,nyha])&
                                    dat[,nyha] %in% c(1,2)] <- 0
      
      dat$predicthf_nyha_acm_ntbnp[!is.na(dat[,nyha])&        
                                    dat[,nyha] %in% c(3,4)] <- log(1.30)

      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
    ## CVM/HFH
      
      dat$predicthf_nyha_cvmhfh_bnp[!is.na(dat[,nyha])&
                                           dat[,nyha] %in% c(1,2)] <- 0
      
      dat$predicthf_nyha_cvmhfh_bnp[!is.na(dat[,nyha])&
                                           dat[,nyha] %in% c(3,4)] <- log(1.24)
      
    ## CVM
      
      dat$predicthf_nyha_cvm_bnp[!is.na(dat[,nyha])&
                                    dat[,nyha] %in% c(1,2)] <- 0
      
      dat$predicthf_nyha_cvm_bnp[!is.na(dat[,nyha])&
                                    dat[,nyha] %in% c(3,4)] <- log(1.33)
      
    ## ACM
      
      dat$predicthf_nyha_acm_bnp[!is.na(dat[,nyha])&        
                                    dat[,nyha] %in% c(1,2)] <- 0
      
      dat$predicthf_nyha_acm_bnp[!is.na(dat[,nyha])&
                                    dat[,nyha] %in% c(3,4)] <- log(1.30)
      
      

      
      
      ##%%%%%%%%%%%%
      ##  No BNP  %%
      ##%%%%%%%%%%%%
      
      
      
    ## CVM/HFH
      
      dat$predicthf_nyha_cvmhfh_nobnp[!is.na(dat[,nyha])&        
                                           dat[,nyha] %in% c(1,2)] <- 0
      
      dat$predicthf_nyha_cvmhfh_nobnp[!is.na(dat[,nyha])&
                                           dat[,nyha] %in% c(3,4)] <- log(1.34)
      
    ## CVM
      
      dat$predicthf_nyha_cvm_nobnp[!is.na(dat[,nyha])&        
                                    dat[,nyha] %in% c(1,2)] <- 0
      
      dat$predicthf_nyha_cvm_nobnp[!is.na(dat[,nyha])&
                                    dat[,nyha] %in% c(3,4)] <- log(1.55)
      
    ## ACM
      
      dat$predicthf_nyha_acm_nobnp[!is.na(dat[,nyha])&        
                                    dat[,nyha] %in% c(1,2)] <- 0
      
      dat$predicthf_nyha_acm_nobnp[!is.na(dat[,nyha])&
                                    dat[,nyha] %in% c(3,4)] <- log(1.40)
      

      
      
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  ##              Percutaneous coronary intervention              ##
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
      

      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
    ## CVM
      
      dat$predicthf_pci_cvm_ntbnp[!is.na(dat[,pci])&
                                    dat[,pci] %in% Y] <- 0
      
      dat$predicthf_pci_cvm_ntbnp[!is.na(dat[,pci])&
                                    dat[,pci] %in% N] <- log(1.21)
      
    ## ACM
      
      dat$predicthf_pci_acm_ntbnp[!is.na(dat[,pci])&
                                    dat[,pci] %in% Y] <- 0
      
      dat$predicthf_pci_acm_ntbnp[!is.na(dat[,pci])&
                                    dat[,pci] %in% N] <- log(1.29)
      
      
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      

    ## CVM
      
      dat$predicthf_pci_cvm_bnp[!is.na(dat[,pci])&
                                  dat[,pci] %in% Y] <- 0
      
      dat$predicthf_pci_cvm_bnp[!is.na(dat[,pci])&
                                  dat[,pci] %in% N] <- log(1.21)
      
    ## ACM
      
      dat$predicthf_pci_acm_bnp[!is.na(dat[,pci])&
                                  dat[,pci] %in% Y] <- 0
      
      dat$predicthf_pci_acm_bnp[!is.na(dat[,pci])&
                                  dat[,pci] %in% N] <- log(1.31)
      
      
      
      
      ##%%%%%%%%%%%%
      ##  No BNP  %%
      ##%%%%%%%%%%%%
      
      
    ## CVM
      
      dat$predicthf_pci_cvm_nobnp[!is.na(dat[,pci])&
                                    dat[,pci] %in% Y] <- 0
      
      dat$predicthf_pci_cvm_nobnp[!is.na(dat[,pci])&
                                    dat[,pci] %in% N] <- log(1.27)
      
    ## ACM
      
      dat$predicthf_pci_acm_nobnp[!is.na(dat[,pci])&
                                    dat[,pci] %in% Y] <- 0
      
      dat$predicthf_pci_acm_nobnp[!is.na(dat[,pci])&
                                    dat[,pci] %in% N] <- log(1.27)
      
      
      

      
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                         Systolic BP                         ##
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
    ## CVM
      
      dat$predicthf_sbp_cvm_ntbnp[!is.na(dat[,sbp])&dat[,sbp] < 120] <- log(1.10)*(120-dat[!is.na(dat[,sbp])&dat[,sbp] < 120,sbp])/10
      
      dat$predicthf_sbp_cvm_ntbnp[!is.na(dat[,sbp])&dat[,sbp] >= 120] <- 0 
      
    ## ACM
      
      dat$predicthf_sbp_acm_ntbnp[!is.na(dat[,sbp])&dat[,sbp] < 120] <- log(1.09)*(120-dat[!is.na(dat[,sbp])&dat[,sbp] < 120,sbp])/10
      
      dat$predicthf_sbp_acm_ntbnp[!is.na(dat[,sbp])&dat[,sbp] >= 120] <- 0 
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
    ## CVM
      
      dat$predicthf_sbp_cvm_bnp[!is.na(dat[,sbp])&dat[,sbp] < 120] <- log(1.10)*(120-dat[!is.na(dat[,sbp])&dat[,sbp] < 120,sbp])/10
      
      dat$predicthf_sbp_cvm_bnp[!is.na(dat[,sbp])&dat[,sbp] >= 120] <- 0 
      
    ## ACM
      
      dat$predicthf_sbp_acm_bnp[!is.na(dat[,sbp])&dat[,sbp] < 120] <- log(1.09)*(120-dat[!is.na(dat[,sbp])&dat[,sbp] < 120,sbp])/10
      
      dat$predicthf_sbp_acm_bnp[!is.na(dat[,sbp])&dat[,sbp] >= 120] <- 0 
      
      
      
      
      ##%%%%%%%%%%%%
      ##  No BNP  %%
      ##%%%%%%%%%%%%
      
      
    ## CVM/HFH
      
      dat$predicthf_sbp_cvmhfh_nobnp[!is.na(dat[,sbp])&dat[,sbp] < 120] <- log(1.10)*(120-dat[!is.na(dat[,sbp])&dat[,sbp] < 120,sbp])/10
      
      dat$predicthf_sbp_cvmhfh_nobnp[!is.na(dat[,sbp])&dat[,sbp] >= 120] <- 0 
    
    ## CVM
      
      dat$predicthf_sbp_cvm_nobnp[!is.na(dat[,sbp])&dat[,sbp] < 120] <- log(1.13)*(120-dat[!is.na(dat[,sbp])&dat[,sbp] < 120,sbp])/10
      
      dat$predicthf_sbp_cvm_nobnp[!is.na(dat[,sbp])&dat[,sbp] >= 120] <- 0 
      
    ## ACM
      
      dat$predicthf_sbp_acm_nobnp[!is.na(dat[,sbp])&dat[,sbp] < 120] <- log(1.13)*(120-dat[!is.na(dat[,sbp])&dat[,sbp] < 120,sbp])/10
      
      dat$predicthf_sbp_acm_nobnp[!is.na(dat[,sbp])&dat[,sbp] >= 120] <- 0 


      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##                       Body mass index                       ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## ACM
      
      dat$predicthf_bmi_acm_ntbnp[!is.na(dat[,bmi])&dat[,bmi] < 30] <- log(1.02)*(30-dat[!is.na(dat[,bmi])&dat[,bmi] < 30,bmi])
      
      dat$predicthf_bmi_acm_ntbnp[!is.na(dat[,bmi])&dat[,bmi] >= 30] <- 0
      
      
      
      
      ##%%%%%%%%%%%%
      ##  No BNP  %%
      ##%%%%%%%%%%%%
      
      ## CVM
      
      dat$predicthf_bmi_cvm_nobnp[!is.na(dat[,bmi])&dat[,bmi] < 30] <- log(1.03)*(30-dat[!is.na(dat[,bmi])&dat[,bmi] < 30,bmi])
      
      dat$predicthf_bmi_cvm_nobnp[!is.na(dat[,bmi])&dat[,bmi] >= 30] <- 0
      
      ## ACM
      
      dat$predicthf_bmi_acm_nobnp[!is.na(dat[,bmi])&dat[,bmi] < 30] <- log(1.03)*(30-dat[!is.na(dat[,bmi])&dat[,bmi] < 30,bmi])
      
      dat$predicthf_bmi_acm_nobnp[!is.na(dat[,bmi])&dat[,bmi] >= 30] <- 0
      
      
      

        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
        ##                            Region                            ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH

      dat$predicthf_region_cvmhfh_ntbnp[!is.na(dat[,region])&
                                          dat[,region] %in% central_europe] <- log(1.34)


      dat$predicthf_region_cvmhfh_ntbnp[!is.na(dat[,region])&
                                          dat[,region] %in% latin_america] <- log(1.50)

            dat$predicthf_region_cvmhfh_ntbnp[!is.na(dat[,region])&
                                          !dat[,region] %in% c(latin_america,central_europe)] <- 0

      ## CVM

      dat$predicthf_region_cvm_ntbnp[!is.na(dat[,region])&
                                          dat[,region] %in% (central_europe)] <- log(1.63)
      
      dat$predicthf_region_cvm_ntbnp[!is.na(dat[,region])&
                                          dat[,region] %in% latin_america] <- log(1.84)
            
      dat$predicthf_region_cvm_ntbnp[!is.na(dat[,region])&
                                          !dat[,region] %in% c(latin_america,central_europe)] <- 0
      
      ## ACM

      dat$predicthf_region_acm_ntbnp[!is.na(dat[,region])&
                                          dat[,region] %in% latin_america] <- log(1.69)

      dat$predicthf_region_acm_ntbnp[!is.na(dat[,region])&
                                       dat[,region] %in% central_europe] <- log(1.42)
      
      dat$predicthf_region_acm_ntbnp[!is.na(dat[,region])&
                                          !dat[,region] %in% c(central_europe, latin_america)] <- 0
      

      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_region_cvmhfh_bnp[!is.na(dat[,region])&
                                          dat[,region] %in% central_europe] <- log(1.33)
      
      dat$predicthf_region_cvmhfh_bnp[!is.na(dat[,region])&
                                          dat[,region] %in% asia] <- log(1.39)
      
      dat$predicthf_region_cvmhfh_bnp[!is.na(dat[,region])&
                                          dat[,region] %in% latin_america] <- log(1.52)
      
      dat$predicthf_region_cvmhfh_bnp[!is.na(dat[,region])&!
                                          dat[,region] %in% c(asia,latin_america,central_europe)] <- 0
      
      ## CVM
      
      dat$predicthf_region_cvm_bnp[!is.na(dat[,region])&
                                          dat[,region] %in% (central_europe)] <- log(1.62)
      
      dat$predicthf_region_cvm_bnp[!is.na(dat[,region])&
                                          dat[,region] %in% asia] <- log(1.85)
      
      dat$predicthf_region_cvm_bnp[!is.na(dat[,region])&
                                          dat[,region] %in% latin_america] <- log(1.84)
      
      dat$predicthf_region_cvm_bnp[!is.na(dat[,region])&!
                                          dat[,region] %in% c(asia,latin_america,central_europe)] <- 0
      
      ## ACM
      
      dat$predicthf_region_acm_bnp[!is.na(dat[,region])&
                                          dat[,region] %in% latin_america] <- log(1.68)
      
      dat$predicthf_region_acm_bnp[!is.na(dat[,region])&
                                     dat[,region] %in% central_europe] <- log(1.43)

            dat$predicthf_region_acm_bnp[!is.na(dat[,region])&
                                          !dat[,region] %in% c(central_europe,latin_america)] <- 0
      
            
            ##%%%%%%%%%%%%
            ##  No BNP  %%
            ##%%%%%%%%%%%%
            
            

      ## CVM/HFH

      dat$predicthf_region_cvmhfh_nobnp[!is.na(dat[,region])&
                                          dat[,region] %in% central_europe] <- log(1.23)

      dat$predicthf_region_cvmhfh_nobnp[!is.na(dat[,region])&
                                          dat[,region] %in% latin_america] <- log(1.25)

      dat$predicthf_region_cvmhfh_nobnp[!is.na(dat[,region])&!dat[,region] %in% c(central_europe,latin_america)] <- 0

      ## CVM

      dat$predicthf_region_cvm_nobnp[!is.na(dat[,region])&
                                          dat[,region] %in% central_europe] <- log(1.45)
      
      dat$predicthf_region_cvm_nobnp[!is.na(dat[,region])&
                                          dat[,region] %in% latin_america] <- log(1.85)
      
      dat$predicthf_region_cvm_nobnp[!is.na(dat[,region])&!dat[,region] %in% c(central_europe,latin_america)] <- 0
      
      ## ACM

      dat$predicthf_region_acm_nobnp[!is.na(dat[,region])&
                                       dat[,region] %in% central_europe] <- log(1.24)
      
      dat$predicthf_region_acm_nobnp[!is.na(dat[,region])&
                                       dat[,region] %in% latin_america] <- log(1.65)
      
      dat$predicthf_region_acm_nobnp[!is.na(dat[,region])&!dat[,region] %in% c(central_europe,latin_america)] <- 0
      

      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
        ##                          Heart rate                          ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
      
      
      ##%%%%%%%%%%%%
      ##  No BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_hr_cvmhfh_nobnp[!is.na(dat[,hr])] <- log(1.09)*dat[!is.na(dat[,hr]),hr]/10
      
      dat$predicthf_hr_cvmhfh_nobnp[!is.na(dat[,hr])] <- 0
      
      ## CVM
      
      dat$predicthf_hr_cvm_nobnp[!is.na(dat[,hr])] <- log(1.08)*dat[!is.na(dat[,hr]),hr]/10
      
      dat$predicthf_hr_cvm_nobnp[!is.na(dat[,hr])] <- 0
      
      ## ACM
      
      dat$predicthf_hr_acm_nobnp[!is.na(dat[,hr])] <- log(1.10)*dat[!is.na(dat[,hr]),hr]/10
      
      dat$predicthf_hr_acm_nobnp[!is.na(dat[,hr])] <- 0
      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##                      Ejection fraction                      ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_lvef_cvmhfh_ntbnp[!is.na(dat[,lvef])&dat[,lvef] < 40] <- log(1.09)*(40-dat[!is.na(dat[,lvef])&dat[,lvef] < 40,lvef])/5
      
      dat$predicthf_lvef_cvmhfh_ntbnp[!is.na(dat[,lvef])&dat[,lvef] >= 40] <- 0 
      
      
      ## CVM
      
      dat$predicthf_lvef_cvm_ntbnp[!is.na(dat[,lvef])&dat[,lvef] < 40]  <- log(1.10)*(40-dat[!is.na(dat[,lvef])&dat[,lvef] < 40,lvef])/5
      
      dat$predicthf_lvef_cvm_ntbnp[!is.na(dat[,lvef])&dat[,lvef] >= 40] <- 0 
      
      ## ACM
      
      dat$predicthf_lvef_acm_ntbnp[!is.na(dat[,lvef])&dat[,lvef] < 40]  <- log(1.08)*(40-dat[!is.na(dat[,lvef])&dat[,lvef] < 40,lvef])/5
      
      dat$predicthf_lvef_acm_ntbnp[!is.na(dat[,lvef])&dat[,lvef] >= 40] <- 0 
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_lvef_cvmhfh_bnp[!is.na(dat[,lvef])&dat[,lvef] < 40]  <- log(1.08)*(40-dat[!is.na(dat[,lvef])&dat[,lvef] < 40,lvef])/5
      
      dat$predicthf_lvef_cvmhfh_bnp[!is.na(dat[,lvef])&dat[,lvef] >= 40] <- 0 
      
      
      ## CVM
      
      dat$predicthf_lvef_cvm_bnp[!is.na(dat[,lvef])&dat[,lvef] < 40]  <- log(1.09)*(40-dat[!is.na(dat[,lvef])&dat[,lvef] < 40,lvef])/5
      
      dat$predicthf_lvef_cvm_bnp[!is.na(dat[,lvef])&dat[,lvef] >= 40] <- 0 
      
      ## ACM
      
      dat$predicthf_lvef_acm_bnp[!is.na(dat[,lvef])&dat[,lvef] < 40]  <- log(1.07)*(40-dat[!is.na(dat[,lvef])&dat[,lvef] < 40,lvef])/5
      
      dat$predicthf_lvef_acm_bnp[!is.na(dat[,lvef])&dat[,lvef] >= 40] <- 0 
      
      
      
      
      ##%%%%%%%%%%%%
      ##  No BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_lvef_cvmhfh_nobnp[!is.na(dat[,lvef])&dat[,lvef] < 40]  <- log(1.16)*(40-dat[!is.na(dat[,lvef])&dat[,lvef] < 40,lvef])/5
      
      dat$predicthf_lvef_cvmhfh_nobnp[!is.na(dat[,lvef])&dat[,lvef] >= 40] <- 0 
      
      ## CVM
      
      dat$predicthf_lvef_cvm_nobnp[!is.na(dat[,lvef])&dat[,lvef] < 40]  <- log(1.10)*(40-dat[!is.na(dat[,lvef])&dat[,lvef] < 40,lvef])/5
      
      dat$predicthf_lvef_cvm_nobnp[!is.na(dat[,lvef])&dat[,lvef] >= 40] <- 0 
      
      ## ACM
      
      dat$predicthf_lvef_acm_nobnp[!is.na(dat[,lvef])&dat[,lvef] < 40]  <- log(1.14)*(40-dat[!is.na(dat[,lvef])&dat[,lvef] < 40,lvef])/5
      
      dat$predicthf_lvef_acm_nobnp[!is.na(dat[,lvef])&dat[,lvef] >= 40] <- 0 
      
      
      
      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
        ##                    Heart failure duration                    ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_hf_duration_cvmhfh_ntbnp[!is.na(dat[,hf_duration])&dat[,hf_duration] > 5] <- log(1.50)
      
      dat$predicthf_hf_duration_cvmhfh_ntbnp[!is.na(dat[,hf_duration])&
                                               dat[,hf_duration] <=5&
                                               dat[,hf_duration] >=1] <- log(1.37)
      
      dat$predicthf_hf_duration_cvm_ntbnp[!is.na(dat[,hf_duration])&dat[,hf_duration] < 1] <- 0
      
      
      ## CVM
      
      dat$predicthf_hf_duration_cvm_ntbnp[!is.na(dat[,hf_duration])&dat[,hf_duration] > 5] <- log(1.35)
      
      dat$predicthf_hf_duration_cvm_ntbnp[!is.na(dat[,hf_duration])&
                                            dat[,hf_duration] <=5&
                                            dat[,hf_duration] >=1] <- log(1.25)
      
      dat$predicthf_hf_duration_cvm_ntbnp[!is.na(dat[,hf_duration])&dat[,hf_duration] < 1] <- 0
      
      ## ACM
      
      dat$predicthf_hf_duration_acm_ntbnp[!is.na(dat[,hf_duration])&dat[,hf_duration] > 5] <- log(1.35)
      
      dat$predicthf_hf_duration_acm_ntbnp[!is.na(dat[,hf_duration])&
                                            dat[,hf_duration] <=5&
                                            dat[,hf_duration] >=1] <- log(1.25)
      
      dat$predicthf_hf_duration_acm_ntbnp[!is.na(dat[,hf_duration])&dat[,hf_duration] < 1] <- 0
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      
      ## CVM/HFH
      
      dat$predicthf_hf_duration_cvmhfh_bnp[!is.na(dat[,hf_duration])&
                                             dat[,hf_duration] > 5] <- log(1.50)
      
      dat$predicthf_hf_duration_cvmhfh_bnp[!is.na(dat[,hf_duration])&
                                             dat[,hf_duration] <=5&
                                             dat[,hf_duration] >=1] <- log(1.37)
      
      dat$predicthf_hf_duration_cvmhfh_bnp[!is.na(dat[,hf_duration])&dat[,hf_duration] < 1] <- 0
      
      
      ## CVM
      
      dat$predicthf_hf_duration_cvm_bnp[!is.na(dat[,hf_duration])&
                                          dat[,hf_duration] > 5] <- log(1.35)
      
      dat$predicthf_hf_duration_cvm_bnp[!is.na(dat[,hf_duration])&
                                          dat[,hf_duration] <=5&
                                          dat[,hf_duration] >=1] <- log(1.21)
      
      dat$predicthf_hf_duration_cvm_bnp[!is.na(dat[,hf_duration])&dat[,hf_duration] < 1] <- 0
      
      
      ## ACM
      
      dat$predicthf_hf_duration_acm_bnp[!is.na(dat[,hf_duration])&
                                          dat[,hf_duration] > 5] <- log(1.35)
      
      dat$predicthf_hf_duration_acm_bnp[!is.na(dat[,hf_duration])&
                                          dat[,hf_duration] <=5&
                                          dat[,hf_duration] >=1] <- log(1.26)
      
      dat$predicthf_hf_duration_acm_bnp[!is.na(dat[,hf_duration])&dat[,hf_duration] < 1] <- 0
      
      
      
      
      ##%%%%%%%%%%%%
      ##  No BNP  %%
      ##%%%%%%%%%%%%
      
      
      
      ## CVM/HFH
      
      dat$predicthf_hf_duration_cvmhfh_nobnp[!is.na(dat[,hf_duration])&
                                               dat[,hf_duration] > 5] <- log(1.47)
      
      dat$predicthf_hf_duration_cvmhfh_nobnp[!is.na(dat[,hf_duration])&
                                               dat[,hf_duration] <=5&
                                               dat[,hf_duration] >=1] <- log(1.36)
      
      dat$predicthf_hf_duration_cvmhfh_nobnp[!is.na(dat[,hf_duration])&dat[,hf_duration] < 1] <- 0
      
      
      ## CVM
      
      dat$predicthf_hf_duration_cvm_nobnp[!is.na(dat[,hf_duration])&
                                            dat[,hf_duration] > 5] <- log(1.40)
      
      dat$predicthf_hf_duration_cvm_nobnp[!is.na(dat[,hf_duration])&
                                            dat[,hf_duration] <=5&
                                            dat[,hf_duration] >=1] <- log(1.25)
      
      dat$predicthf_hf_duration_cvm_nobnp[!is.na(dat[,hf_duration])&dat[,hf_duration] < 1] <- 0
      
      
      ## ACM
      
      dat$predicthf_hf_duration_acm_nobnp[!is.na(dat[,hf_duration])&
                                            dat[,hf_duration] > 5] <- log(1.36)
      
      dat$predicthf_hf_duration_acm_nobnp[!is.na(dat[,hf_duration])&
                                            dat[,hf_duration] <=5&
                                            dat[,hf_duration] >=1] <- log(1.26)
      
      dat$predicthf_hf_duration_acm_nobnp[!is.na(dat[,hf_duration])&dat[,hf_duration] < 1] <- 0
      
      
      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##                 Not on sacubitril/valsartan                 ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_arni_cvmhfh_ntbnp[!is.na(dat[,arni])&
                                        dat[,arni] %in% Y] <- 0
      
      dat$predicthf_arni_cvmhfh_ntbnp[!is.na(dat[,arni])&
                                        dat[,arni] %in% N] <- log(1.24)
      
      
      ## CVM
      
      dat$predicthf_arni_cvm_ntbnp[!is.na(dat[,arni])&
                                     dat[,arni] %in% Y] <- 0
      
      dat$predicthf_arni_cvm_ntbnp[!is.na(dat[,arni])&
                                     dat[,arni] %in% N] <- log(1.25)
      
      ## ACM
      
      dat$predicthf_arni_acm_ntbnp[!is.na(dat[,arni])&
                                     dat[,arni] %in% Y] <- 0
      
      dat$predicthf_arni_acm_ntbnp[!is.na(dat[,arni])&
                                     dat[,arni] %in% N] <- log(1.18)
      
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_arni_cvmhfh_bnp[!is.na(dat[,arni])&
                                      dat[,arni] %in% Y] <- 0
      
      dat$predicthf_arni_cvmhfh_bnp[!is.na(dat[,arni])&
                                      dat[,arni] %in% N] <- log(1.24)
      
      ## CVM
      
      dat$predicthf_arni_cvm_bnp[!is.na(dat[,arni])&
                                   dat[,arni] %in% Y] <- 0
      
      dat$predicthf_arni_cvm_bnp[!is.na(dat[,arni])&
                                   dat[,arni] %in% N] <- log(1.25)
      
      ## ACM
      
      dat$predicthf_arni_acm_bnp[!is.na(dat[,arni])&
                                   dat[,arni] %in% Y] <- 0
      
      dat$predicthf_arni_acm_bnp[!is.na(dat[,arni])&
                                   dat[,arni] %in% N] <- log(1.19)
      
      
      
      
      ##%%%%%%%%%%%%
      ##  No BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_arni_cvmhfh_nobnp[!is.na(dat[,arni])&
                                        dat[,arni] %in% Y] <- 0
      
      dat$predicthf_arni_cvmhfh_nobnp[!is.na(dat[,arni])&
                                        dat[,arni] %in% N] <- log(1.23)
      
      
      ## CVM
      
      dat$predicthf_arni_cvm_nobnp[!is.na(dat[,arni])&
                                     dat[,arni] %in% Y] <- 0
      
      dat$predicthf_arni_cvm_nobnp[!is.na(dat[,arni])&
                                     dat[,arni] %in% N] <- log(1.23)
      
      ## ACM
      
      dat$predicthf_arni_acm_nobnp[!is.na(dat[,arni])&
                                     dat[,arni] %in% Y] <- 0
      
      dat$predicthf_arni_acm_nobnp[!is.na(dat[,arni])&
                                     dat[,arni] %in% N] <- log(1.18)
      
      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
        ##                       Valvular disease                       ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_valv_dz_cvmhfh_ntbnp[!is.na(dat[,valv_dz])&
                                           dat[,valv_dz] %in% Y] <- log(1.33)
      
      dat$predicthf_valv_dz_cvmhfh_ntbnp[!is.na(dat[,valv_dz])&
                                           dat[,valv_dz] %in% N] <- 0
      
      
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_valv_dz_cvmhfh_bnp[!is.na(dat[,valv_dz])&
                                         dat[,valv_dz] %in% Y] <- log(1.35)
      
      dat$predicthf_valv_dz_cvmhfh_bnp[!is.na(dat[,valv_dz])&
                                         dat[,valv_dz] %in% N] <- 0
      
      
      
      
      ##%%%%%%%%%%%%
      ##  No BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_valv_dz_cvmhfh_nobnp[!is.na(dat[,valv_dz])&
                                           dat[,valv_dz] %in% Y] <- log(1.32)
      
      dat$predicthf_valv_dz_cvmhfh_nobnp[!is.na(dat[,valv_dz])&
                                           dat[,valv_dz] %in% N] <- 0
      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##                     Not on beta-blocker                     ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_betab_cvmhfh_ntbnp[!is.na(dat[,betab])&
                                         dat[,betab] %in% Y] <- 0
      
      dat$predicthf_betab_cvmhfh_ntbnp[!is.na(dat[,betab])&
                                         dat[,betab] %in% N] <- log(1.34)
      
      
      ## CVM
      
      dat$predicthf_betab_cvm_ntbnp[!is.na(dat[,betab])&
                                      dat[,betab] %in% Y] <- 0
      
      dat$predicthf_betab_cvm_ntbnp[!is.na(dat[,betab])&
                                      dat[,betab] %in% N] <- log(1.36)
      
      ## ACM
      
      dat$predicthf_betab_acm_ntbnp[!is.na(dat[,betab])&
                                      dat[,betab] %in% Y] <- 0
      
      dat$predicthf_betab_acm_ntbnp[!is.na(dat[,betab])&
                                      dat[,betab] %in% N] <- log(1.27)
      
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_betab_cvmhfh_bnp[!is.na(dat[,betab])&
                                       dat[,betab] %in% Y] <- 0
      
      dat$predicthf_betab_cvmhfh_bnp[!is.na(dat[,betab])&
                                       dat[,betab] %in% N] <- log(1.34)
      
      ## CVM
      
      dat$predicthf_betab_cvm_bnp[!is.na(dat[,betab])&
                                    dat[,betab] %in% Y] <- 0
      
      dat$predicthf_betab_cvm_bnp[!is.na(dat[,betab])&
                                    dat[,betab] %in% N] <- log(1.36)
      
      ## ACM
      
      dat$predicthf_betab_acm_bnp[!is.na(dat[,betab])&
                                    dat[,betab] %in% Y] <- 0
      
      dat$predicthf_betab_acm_bnp[!is.na(dat[,betab])&
                                    dat[,betab] %in% N] <- log(1.26)
      
      
      
      
      ##%%%%%%%%%%%%
      ##  No BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_betab_cvmhfh_nobnp[!is.na(dat[,betab])&
                                         dat[,betab] %in% Y] <- 0
      
      dat$predicthf_betab_cvmhfh_nobnp[!is.na(dat[,betab])&
                                         dat[,betab] %in% N] <- log(1.31)
      
      
      ## CVM
      
      dat$predicthf_betab_cvm_nobnp[!is.na(dat[,betab])&
                                      dat[,betab] %in% Y] <- 0
      
      dat$predicthf_betab_cvm_nobnp[!is.na(dat[,betab])&
                                      dat[,betab] %in% N] <- log(1.35)
      
      ## ACM
      
      dat$predicthf_betab_acm_nobnp[!is.na(dat[,betab])&
                                      dat[,betab] %in% Y] <- 0
      
      dat$predicthf_betab_acm_nobnp[!is.na(dat[,betab])&
                                      dat[,betab] %in% N] <- log(1.31)
      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##                  On lipid lowering therapy                  ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      
      
      
      ##%%%%%%%%%%%%
      ##  No BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_llt_cvmhfh_nobnp[!is.na(dat[,llt])&
                                       dat[,llt] %in% Y] <- log(1.16)
      
      dat$predicthf_llt_cvmhfh_nobnp[!is.na(dat[,llt])&
                                       dat[,llt] %in% N] <- 0
      
      ## ACM
      
      dat$predicthf_llt_acm_nobnp[!is.na(dat[,llt])&
                                    dat[,llt] %in% Y] <- log(1.16)
      
      dat$predicthf_llt_acm_nobnp[!is.na(dat[,llt])&
                                    dat[,llt] %in% N] <- 0
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##                 Peripheral arterial disease                 ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_pad_cvmhfh_ntbnp[!is.na(dat[,pad])&
                                       dat[,pad] %in% Y] <- log(1.28)
      
      dat$predicthf_pad_cvmhfh_ntbnp[!is.na(dat[,pad])&
                                       dat[,pad] %in% N] <- 0
      
      ## CVM
      
      dat$predicthf_pad_cvm_ntbnp[!is.na(dat[,pad])&
                                    dat[,pad] %in% Y] <- log(1.41)
      
      dat$predicthf_pad_cvm_ntbnp[!is.na(dat[,pad])&
                                    dat[,pad] %in% N] <- 0
      
      ## ACM
      
      dat$predicthf_pad_acm_ntbnp[!is.na(dat[,pad])&
                                    dat[,pad] %in% Y] <- log(1.36)
      
      dat$predicthf_pad_acm_ntbnp[!is.na(dat[,pad])&
                                    dat[,pad] %in% N] <- 0
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_pad_cvmhfh_bnp[!is.na(dat[,pad])&
                                     dat[,pad] %in% Y] <- log(1.26)
      
      dat$predicthf_pad_cvmhfh_bnp[!is.na(dat[,pad])&
                                     dat[,pad] %in% N] <- 0
      
      ## CVM
      
      dat$predicthf_pad_cvm_bnp[!is.na(dat[,pad])&
                                  dat[,pad] %in% Y] <- log(1.38)
      
      dat$predicthf_pad_cvm_bnp[!is.na(dat[,pad])&
                                  dat[,pad] %in% N] <- 0
      
      ## ACM
      
      dat$predicthf_pad_acm_bnp[!is.na(dat[,pad])&
                                  dat[,pad] %in% Y] <- log(1.34)
      
      dat$predicthf_pad_acm_bnp[!is.na(dat[,pad])&
                                  dat[,pad] %in% N] <- 0
      
      
      
      
      ##%%%%%%%%%%%%
      ##  No BNP  %%
      ##%%%%%%%%%%%%
      
      
      
      ## CVM/HFH
      
      dat$predicthf_pad_cvmhfh_nobnp[!is.na(dat[,pad])&
                                       dat[,pad] %in% Y] <- log(1.30)
      
      dat$predicthf_pad_cvmhfh_nobnp[!is.na(dat[,pad])&
                                       dat[,pad] %in% N] <- 0
      
      ## CVM
      
      dat$predicthf_pad_cvm_nobnp[!is.na(dat[,pad])&
                                    dat[,pad] %in% Y] <- log(1.43)
      
      dat$predicthf_pad_cvm_nobnp[!is.na(dat[,pad])&
                                    dat[,pad] %in% N] <- 0
      
      ## ACM
      
      dat$predicthf_pad_acm_nobnp[!is.na(dat[,pad])&
                                    dat[,pad] %in% Y] <- log(1.47)
      
      dat$predicthf_pad_acm_nobnp[!is.na(dat[,pad])&
                                    dat[,pad] %in% N] <- 0
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##                     Bundle branch block                     ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_bbb_cvmhfh_ntbnp[!is.na(dat[,bbb])&
                                       dat[,bbb] %in% Y] <- log(1.16)
      
      dat$predicthf_bbb_cvmhfh_ntbnp[!is.na(dat[,bbb])&
                                       dat[,bbb] %in% N] <- 0
      
      ## CVM
      
      dat$predicthf_bbb_cvm_ntbnp[!is.na(dat[,bbb])&
                                    dat[,bbb] %in% Y] <- log(1.20)
      
      dat$predicthf_bbb_cvm_ntbnp[!is.na(dat[,bbb])&
                                    dat[,bbb] %in% N] <- 0
      
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_bbb_cvmhfh_bnp[!is.na(dat[,bbb])&
                                     dat[,bbb] %in% Y] <- log(1.17)
      
      dat$predicthf_bbb_cvmhfh_bnp[!is.na(dat[,bbb])&
                                     dat[,bbb] %in% N] <- 0
      
      ## CVM
      
      dat$predicthf_bbb_cvm_bnp[!is.na(dat[,bbb])&
                                  dat[,bbb] %in% Y] <- log(1.20)
      
      dat$predicthf_bbb_cvm_bnp[!is.na(dat[,bbb])&
                                  dat[,bbb] %in% N] <- 0
      
      
      
      
      
      ##%%%%%%%%%%%%
      ##  No BNP  %%
      ##%%%%%%%%%%%%
      
      
      
      ## CVM/HFH
      
      dat$predicthf_bbb_cvmhfh_nobnp[!is.na(dat[,bbb])&
                                       dat[,bbb] %in% Y] <- log(1.21)
      
      dat$predicthf_bbb_cvmhfh_nobnp[!is.na(dat[,bbb])&
                                       dat[,bbb] %in% N] <- 0
      
      ## CVM
      
      dat$predicthf_bbb_cvm_nobnp[!is.na(dat[,bbb])&
                                    dat[,bbb] %in% Y] <- log(1.25)
      
      dat$predicthf_bbb_cvm_nobnp[!is.na(dat[,bbb])&
                                    dat[,bbb] %in% N] <- 0
      
      
      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##                 Prior myocardial infarction                 ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_mi_cvmhfh_ntbnp[!is.na(dat[,mi])&
                                      dat[,mi] %in% Y] <- log(1.12)
      
      dat$predicthf_mi_cvmhfh_ntbnp[!is.na(dat[,mi])&
                                      dat[,mi] %in% N] <- 0
      
      ## CVM
      
      dat$predicthf_mi_cvm_ntbnp[!is.na(dat[,mi])&
                                   dat[,mi] %in% Y] <- log(1.23)
      
      dat$predicthf_mi_cvm_ntbnp[!is.na(dat[,mi])&
                                   dat[,mi] %in% N] <- 0
      
      ## ACM
      
      dat$predicthf_mi_acm_ntbnp[!is.na(dat[,mi])&
                                   dat[,mi] %in% Y] <- log(1.15)
      
      dat$predicthf_mi_acm_ntbnp[!is.na(dat[,mi])&
                                   dat[,mi] %in% N] <- 0
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_mi_cvmhfh_bnp[!is.na(dat[,mi])&
                                    dat[,mi] %in% Y] <- log(1.06)
      
      dat$predicthf_mi_cvmhfh_bnp[!is.na(dat[,mi])&
                                    dat[,mi] %in% N] <- 0
      
      ## CVM
      
      dat$predicthf_mi_cvm_bnp[!is.na(dat[,mi])&
                                 dat[,mi] %in% Y] <- log(1.16)
      
      dat$predicthf_mi_cvm_bnp[!is.na(dat[,mi])&
                                 dat[,mi] %in% N] <- 0
      
      ## ACM
      
      dat$predicthf_mi_acm_bnp[!is.na(dat[,mi])&
                                 dat[,mi] %in% Y] <- log(1.10)
      
      dat$predicthf_mi_acm_bnp[!is.na(dat[,mi])&
                                 dat[,mi] %in% N] <- 0
      
      
      
      ##%%%%%%%%%%%%
      ##  No BNP  %%
      ##%%%%%%%%%%%%
      
      
      
      ## CVM/HFH
      
      dat$predicthf_mi_cvmhfh_nobnp[!is.na(dat[,mi])&
                                      dat[,mi] %in% Y] <- log(1.17)
      
      dat$predicthf_mi_cvmhfh_nobnp[!is.na(dat[,mi])&
                                      dat[,mi] %in% N] <- 0
      
      ## CVM
      
      dat$predicthf_mi_cvm_nobnp[!is.na(dat[,mi])&
                                   dat[,mi] %in% Y] <- log(1.18)
      
      dat$predicthf_mi_cvm_nobnp[!is.na(dat[,mi])&
                                   dat[,mi] %in% N] <- 0
      
      ## ACM
      
      dat$predicthf_mi_acm_nobnp[!is.na(dat[,mi])&
                                   dat[,mi] %in% Y] <- log(1.19)
      
      dat$predicthf_mi_acm_nobnp[!is.na(dat[,mi])&
                                   dat[,mi] %in% N] <- 0
      
      
      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##                     Atrial fibrillation                     ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      
      

      
      ##%%%%%%%%%%%%
      ##  No BNP  %%
      ##%%%%%%%%%%%%
      
      
      
      ## CVM/HFH
      
      dat$predicthf_af_cvmhfh_nobnp[!is.na(dat[,af])&
                                      dat[,af] %in% Y] <- log(1.17)
      
      dat$predicthf_af_cvmhfh_nobnp[!is.na(dat[,mi])&
                                      dat[,af] %in% N] <- 0
    
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
        ##                   Prior HF hospitalization                   ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
      
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_prior_hfh_cvmhfh_ntbnp[!is.na(dat[,prior_hfh])&
                                             dat[,prior_hfh] %in% Y] <- log(1.35)
      
      dat$predicthf_prior_hfh_cvmhfh_ntbnp[!is.na(dat[,prior_hfh])&
                                             dat[,prior_hfh] %in% N] <- 0
      
      ## Interaction prior_hfh x Latin America
      
      dat$predicthf_prior_hfh_latin_cvmhfh_ntbnp[!is.na(dat[,prior_hfh])&
                                                   dat[,prior_hfh] %in% Y&
                                                   !is.na(dat[,region])&
                                                   dat[,region] %in% (latin_america)] <- log(1.41)
      
      
      dat$predicthf_prior_hfh_latin_cvmhfh_ntbnp[!is.na(dat[,prior_hfh])&
                                                   dat[,prior_hfh] %in% N&
                                                   is.na(dat[,region])&
                                                   !dat[,region] %in% (latin_america)] <- 0
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_prior_hfh_cvmhfh_bnp[!is.na(dat[,prior_hfh])&
                                           dat[,prior_hfh] %in% Y] <- log(1.37)
      
      dat$predicthf_prior_hfh_cvmhfh_bnp[!is.na(dat[,prior_hfh])&
                                           dat[,prior_hfh] %in% N] <- 0
      
      ## Interaction prior_hfh x Latin America
      
      dat$predicthf_prior_hfh_latin_cvmhfh_bnp[!is.na(dat[,prior_hfh])&
                                                 dat[,prior_hfh] %in% Y&
                                                 !is.na(dat[,region])&
                                                 dat[,region] %in% (latin_america)] <- log(1.37)
      
      
      dat$predicthf_prior_hfh_latin_cvmhfh_bnp[!is.na(dat[,prior_hfh])&
                                                 dat[,prior_hfh] %in% N&
                                                 is.na(dat[,region])&
                                                 !dat[,region] %in% (latin_america)] <- 0
      
      
      
      
      ##%%%%%%%%%%%%
      ##  No BNP  %%
      ##%%%%%%%%%%%%
      
      
      
      ## CVM/HFH
      
      dat$predicthf_prior_hfh_cvmhfh_nobnp[!is.na(dat[,prior_hfh])&
                                             dat[,prior_hfh] %in% Y] <- log(1.33)
      
      dat$predicthf_prior_hfh_cvmhfh_nobnp[!is.na(dat[,prior_hfh])&
                                             dat[,prior_hfh] %in% N] <- 0
      
      ## CVM
      
      dat$predicthf_prior_hfh_cvm_nobnp[!is.na(dat[,prior_hfh])&
                                          dat[,prior_hfh] %in% Y] <- log(1.18)
      
      dat$predicthf_prior_hfh_cvm_nobnp[!is.na(dat[,prior_hfh])&
                                          dat[,prior_hfh] %in% N] <- 0
      
      ## ACM
      
      dat$predicthf_prior_hfh_acm_nobnp[!is.na(dat[,prior_hfh])&
                                          dat[,prior_hfh] %in% Y] <- log(1.18)
      
      dat$predicthf_prior_hfh_acm_nobnp[!is.na(dat[,prior_hfh])&
                                          dat[,prior_hfh] %in% N] <- 0
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##                             CKD                             ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      

      
      
      ##%%%%%%%%%%%%
      ##  No BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_ckd_cvmhfh_nobnp[!is.na(dat[,age])&dat[,ckd]==4] <- log(1.20) 
      
      dat$predicthf_ckd_cvmhfh_nobnp[!is.na(dat[,age])&dat[,ckd]<4] <- 0 
      

      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
        ##                    Total bilirubin (mg/dL)                   ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
      
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_tbili_cvmhfh_ntbnp[!is.na(dat[,tbili])&dat[,tbili] > 0.58] <- log(1.11)*(dat[!is.na(dat[,tbili])&dat[,tbili] > 0.58,tbili]-0.58)/0.29
      
      dat$predicthf_tbili_cvmhfh_ntbnp[!is.na(dat[,tbili])&dat[,tbili] <= 0.58] <- 0 
      
      
      ## CVM
      
      dat$predicthf_tbili_cvm_ntbnp[!is.na(dat[,tbili])&dat[,tbili] > 0.58] <- log(1.10)*(dat[!is.na(dat[,tbili])&dat[,tbili] > 0.58,tbili]-0.58)/0.29
      
      dat$predicthf_tbili_cvm_ntbnp[!is.na(dat[,tbili])&dat[,tbili] <= 0.58] <- 0 
      
      ## ACM
      
      dat$predicthf_tbili_acm_ntbnp[!is.na(dat[,tbili])&dat[,tbili] >0.58] <- log(1.08)*(dat[!is.na(dat[,tbili])&dat[,tbili] > 0.58,tbili]-0.58)/0.29
      
      dat$predicthf_tbili_acm_ntbnp[!is.na(dat[,tbili])&dat[,tbili] <= 0.58] <- 0 
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_tbili_cvmhfh_bnp[!is.na(dat[,tbili])&dat[,tbili] > 0.58] <- log(1.10)*(dat[!is.na(dat[,tbili])&dat[,tbili] > 0.58,tbili]-0.58)/0.29
      
      dat$predicthf_tbili_cvmhfh_bnp[!is.na(dat[,tbili])&dat[,tbili] >= 0.58] <- 0 
      
      
      ## CVM
      
      dat$predicthf_tbili_cvm_bnp[!is.na(dat[,tbili])&dat[,tbili] > 0.58] <- log(1.09)*(dat[!is.na(dat[,tbili])&dat[,tbili] > 0.58,tbili]-0.58)/0.29
      
      dat$predicthf_tbili_cvm_bnp[!is.na(dat[,tbili])&dat[,tbili] <= 0.58] <- 0 
      
      ## ACM
      
      dat$predicthf_tbili_acm_bnp[!is.na(dat[,tbili])&dat[,tbili] > 0.58] <- log(1.08)*(dat[!is.na(dat[,tbili])&dat[,tbili] > 0.58,tbili]-0.58)/0.229
      
      dat$predicthf_tbili_acm_bnp[!is.na(dat[,tbili])&dat[,tbili] <= 0.58] <- 0 
      
      
      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##              Aspartate aminotransferase (IU/L)              ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      
      ## ACM
      
      dat$predicthf_ast_acm_ntbnp[!is.na(dat[,ast])&dat[,ast] > 30] <- (dat[!is.na(dat[,ast])&dat[,ast] > 30,ast]-30)/10 *log(1.07)
      
      dat$predicthf_ast_acm_ntbnp[!is.na(dat[,ast])&dat[,ast] <= 30] <- 0 
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      ## ACM
      
      dat$predicthf_ast_acm_bnp[!is.na(dat[,ast])&dat[,ast] > 30] <- (dat[!is.na(dat[,ast])&dat[,ast] > 30,ast]-30)/10 * log(1.07)
      
      dat$predicthf_ast_acm_bnp[!is.na(dat[,ast])&dat[,ast] <= 30] <- 0 
      
      
      
      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##                         % monocytes                         ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## ACM
      
      dat$predicthf_mono_acm_ntbnp[!is.na(dat[,mono])&dat[,mono] > 7] <- (dat[!is.na(dat[,mono])&dat[,mono] > 7,mono]-7) * log(1.04)
      
      dat$predicthf_mono_acm_ntbnp[!is.na(dat[,mono])&dat[,mono] <= 7] <- 0 
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      ## ACM
      
      dat$predicthf_mono_acm_bnp[!is.na(dat[,mono])&dat[,mono] > 7] <- (dat[!is.na(dat[,mono])&dat[,mono] > 7,mono]-7) * log(1.04)
      
      dat$predicthf_mono_acm_bnp[!is.na(dat[,mono])&dat[,mono] <= 7] <- 0 
      
      
      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
        ##                     Triglycerides, mg/dL                 ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## ACM
      
      dat$predicthf_tg_acm_ntbnp[!is.na(dat[,tg])&dat[,tg] > 221.24] <- (dat[!is.na(dat[,tg])&dat[,tg] > 221.24,tg]-221.24)/88.5 * log(1.07)
      
      dat$predicthf_tg_acm_ntbnp[!is.na(dat[,tg])&dat[,tg] <= 221.24] <- 0 
      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
        ##                       Uric acid, mg/dL                   ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
      
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_uric_cvmhfh_ntbnp[!is.na(dat[,uric])&dat[,uric] > 6.72] <- (dat[!is.na(dat[,uric]),uric]-6.72)/0.84 * log(1.08)
      
      dat$predicthf_uric_cvmhfh_ntbnp[!is.na(dat[,uric])&dat[,uric] <= 6.72] <- 0 
      
      
      ## CVM
      
      dat$predicthf_uric_cvm_ntbnp[!is.na(dat[,uric])&dat[,uric] > 6.72] <- (dat[!is.na(dat[,uric])&dat[,uric] > 6.72,uric]-6.72)/0.84 * log(1.07)
      
      dat$predicthf_uric_cvm_ntbnp[!is.na(dat[,uric])&dat[,uric] <= 6.72] <- 0 
      
      ## ACM
      
      dat$predicthf_uric_acm_ntbnp[!is.na(dat[,uric])&dat[,uric] > 6.72] <- (dat[!is.na(dat[,uric])&dat[,uric] > 6.72,uric]-6.72)/0.84 * log(1.07)
      
      dat$predicthf_uric_acm_ntbnp[!is.na(dat[,uric])&dat[,uric] <= 6.72] <- 0 
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_uric_cvmhfh_bnp[!is.na(dat[,uric])&dat[,uric] > 6.72] <- (dat[!is.na(dat[,uric])&dat[,uric] > 6.72,uric]-6.72)/0.84 * log(1.08)
      
      dat$predicthf_uric_cvmhfh_bnp[!is.na(dat[,uric])&dat[,uric] <= 6.72] <- 0 
      
      
      ## CVM
      
      dat$predicthf_uric_cvm_bnp[!is.na(dat[,uric])&dat[,uric] > 6.72] <- (dat[!is.na(dat[,uric])&dat[,uric] > 6.72,uric]-6.72)/0.84 * log(1.07)
      
      dat$predicthf_uric_cvm_bnp[!is.na(dat[,uric])&dat[,uric] <= 6.72] <- 0 
      
      ## ACM
      
      dat$predicthf_uric_acm_bnp[!is.na(dat[,uric])&dat[,uric] > 6.72] <- (dat[!is.na(dat[,uric])&dat[,uric] > 6.72,uric]-6.72)/0.84 * log(1.07)
      
      dat$predicthf_uric_acm_bnp[!is.na(dat[,uric])&dat[,uric] <= 6.72] <- 0 
      
      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##                        Albumin, g/dL                    ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_alb_cvmhfh_ntbnp[!is.na(dat[,alb])&dat[,alb] < 4.2] <- (4.2-dat[!is.na(dat[,alb])&dat[,alb] < 4.2,alb])/0.1 * log(1.05)
      
      dat$predicthf_alb_cvmhfh_ntbnp[!is.na(dat[,alb])&dat[,alb] >= 4.2] <- 0 
      
      
      ## CVM
      
      dat$predicthf_alb_cvm_ntbnp[!is.na(dat[,alb])&dat[,alb] < 4.2] <- (4.2-dat[!is.na(dat[,alb])&dat[,alb] < 4.2,alb])/0.1 * log(1.05)
      
      dat$predicthf_alb_cvm_ntbnp[!is.na(dat[,alb])&dat[,alb] >= 4.2] <- 0 
      
      ## ACM
      
      dat$predicthf_alb_acm_ntbnp[!is.na(dat[,alb])&dat[,alb] < 4.2] <- (4.2-dat[!is.na(dat[,alb])&dat[,alb] < 4.2,alb])/0.1 * log(1.06)
      
      dat$predicthf_alb_acm_ntbnp[!is.na(dat[,alb])&dat[,alb] >= 4.2] <- 0 
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_alb_cvmhfh_bnp[!is.na(dat[,alb])&dat[,alb] < 4.2] <- (4.2-dat[!is.na(dat[,alb])&dat[,alb] < 4.2,alb])/0.1 * log(1.05)
      
      dat$predicthf_alb_cvmhfh_bnp[!is.na(dat[,alb])&dat[,alb] >= 4.2] <- 0 
      
      
      ## CVM
      
      dat$predicthf_alb_cvm_bnp[!is.na(dat[,alb])&dat[,alb] < 4.2] <- (4.2-dat[!is.na(dat[,alb])&dat[,alb] < 4.2,alb])/0.1 * log(1.05)
      
      dat$predicthf_alb_cvm_bnp[!is.na(dat[,alb])&dat[,alb] >= 4.2] <- 0 
      
      ## ACM
      
      dat$predicthf_alb_acm_bnp[!is.na(dat[,alb])&dat[,alb] < 4.2] <- (4.2-dat[!is.na(dat[,alb])&dat[,alb] < 4.2,alb])/0.1 * log(1.06)
      
      dat$predicthf_alb_acm_bnp[!is.na(dat[,alb])&dat[,alb] >= 4.2] <- 0 
      
      
      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##                      Potassium, mEq/dL                     ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_k_cvmhfh_ntbnp[!is.na(dat[,k])&dat[,k] < 4] <- (4-dat[!is.na(dat[,k])&dat[,k] < 4,k])/0.1 * log(1.07)
      
      dat$predicthf_k_cvmhfh_ntbnp[!is.na(dat[,k])&dat[,k] >= 4] <- 0 
      
      
      ## CVM
      
      dat$predicthf_k_cvm_ntbnp[!is.na(dat[,k])&dat[,k] < 4] <- (4-dat[!is.na(dat[,k])&dat[,k] < 4,k])/0.1 * log(1.09)
      
      dat$predicthf_k_cvm_ntbnp[!is.na(dat[,k])&dat[,k] >= 4] <- 0 
      
      ## ACM
      
      dat$predicthf_k_acm_ntbnp[!is.na(dat[,k])&dat[,k] < 4] <- (4-dat[!is.na(dat[,k])&dat[,k] < 4,k])/0.1 * log(1.05)
      
      dat$predicthf_k_acm_ntbnp[!is.na(dat[,k])&dat[,k] >= 4] <- 0 
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_k_cvmhfh_bnp[!is.na(dat[,k])&dat[,k] < 4] <- (4-dat[!is.na(dat[,k])&dat[,k] < 4,k])/0.1 * log(1.07)
      
      dat$predicthf_k_cvmhfh_bnp[!is.na(dat[,k])&dat[,k] >= 4] <- 0 
      
      
      ## CVM
      
      dat$predicthf_k_cvm_bnp[!is.na(dat[,k])&dat[,k] < 4] <- (4-dat[!is.na(dat[,k])&dat[,k] < 4,k])/0.1 * log(1.09)
      
      dat$predicthf_k_cvm_bnp[!is.na(dat[,k])&dat[,k] >= 4] <- 0 
      
      ## ACM
      
      dat$predicthf_k_acm_bnp[!is.na(dat[,k])&dat[,k] < 4] <- (4-dat[!is.na(dat[,k])&dat[,k] < 4,k])/0.1 * log(1.06)
      
      dat$predicthf_k_acm_bnp[!is.na(dat[,k])&dat[,k] >= 4] <- 0 
      
      
      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
        ##           B-type natriuretic peptide, picograms/mL           ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
      
      dat$bnp_cat[!is.na(dat[,bnp_val])&dat[,bnp_val]<100] <- 0
      dat$bnp_cat[!is.na(dat[,bnp_val])&dat[,bnp_val]>=100&dat[,bnp_val]<200] <- 1
      dat$bnp_cat[!is.na(dat[,bnp_val])&dat[,bnp_val]>=200&dat[,bnp_val]<300] <- 2
      dat$bnp_cat[!is.na(dat[,bnp_val])&dat[,bnp_val]>=300&dat[,bnp_val]<600] <- 3
      dat$bnp_cat[!is.na(dat[,bnp_val])&dat[,bnp_val]>=600] <- 4
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      
      ## CVM/HFH
      
      dat$predicthf_bnp_cat_cvmhfh_bnp <- dat[,"bnp_cat"] * log(1.29)

            
      ## CVM
      
      dat$predicthf_bnp_cat_cvm_bnp <- dat[,"bnp_cat"] * log(1.34)

      
      ## ACM
      
      dat$predicthf_bnp_cat_acm_bnp <- dat[,"bnp_cat"] * log(1.29)

      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##  N-terminal prohormone B-type natriuretic peptide, picograms/mL ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      dat$ntbnp_cat[!is.na(dat[,ntbnp_val])] <- 0
      dat$ntbnp_cat[!is.na(dat[,ntbnp_val])&dat[,ntbnp_val]<400] <- 1
      dat$ntbnp_cat[!is.na(dat[,ntbnp_val])&dat[,ntbnp_val]>=400&dat[,ntbnp_val]<800] <- 2
      dat$ntbnp_cat[!is.na(dat[,ntbnp_val])&dat[,ntbnp_val]>=800&dat[,ntbnp_val]<1600] <- 3
      dat$ntbnp_cat[!is.na(dat[,ntbnp_val])&dat[,ntbnp_val]>=1600&dat[,ntbnp_val]<3200] <- 4
      dat$ntbnp_cat[!is.na(dat[,ntbnp_val])&dat[,ntbnp_val]>=3200] <- 5
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_ntbnp_cat_cvmhfh_ntbnp <- dat[,"ntbnp_cat"] * log(1.34)


      ## CVM
      
      dat$predicthf_ntbnp_cat_cvm_ntbnp <- dat[,"ntbnp_cat"] * log(1.40)
      

      ## ACM
      
      dat$predicthf_ntbnp_cat_acm_ntbnp <- dat[,"ntbnp_cat"] * log(1.33)
      

      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##                       Chloride, mEq/L                       ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      
      ## ACM
      
      dat$predicthf_cl_acm_ntbnp[!is.na(dat[,cl])&dat[,cl] < 100] <- (100-dat[!is.na(dat[,cl])&dat[,cl] < 100,cl]) * log(1.06)
      
      dat$predicthf_cl_acm_ntbnp[!is.na(dat[,cl])&dat[,cl] >= 100] <- 0 
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      
      ## ACM
      
      dat$predicthf_cl_acm_bnp[!is.na(dat[,cl])&dat[,cl] < 100] <- (100-dat[!is.na(dat[,cl])&dat[,cl] < 100,cl]) * log(1.07)
      
      dat$predicthf_cl_acm_bnp[!is.na(dat[,cl])&dat[,cl] >= 100] <- 0 
      
      
      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##         Absolute neutrophil count, cells/microliter         ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_anc_cvmhfh_ntbnp[!is.na(dat[,anc])&dat[,anc] <= 6] <- dat[!is.na(dat[,anc])&dat[,anc] <= 6,anc] * log(1.07)
      
      dat$predicthf_anc_cvmhfh_ntbnp[!is.na(dat[,anc])&dat[,anc] > 6] <- 0 
      
      
      
      ## ACM
      
      dat$predicthf_anc_acm_ntbnp[!is.na(dat[,anc])&dat[,anc] <= 6] <- dat[!is.na(dat[,anc])&dat[,anc] <= 6,anc] * log(1.10)
      
      dat$predicthf_anc_acm_ntbnp[!is.na(dat[,anc])&dat[,anc] > 6] <- 0 
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_anc_cvmhfh_bnp[!is.na(dat[,anc])&dat[,anc] <= 6] <- dat[!is.na(dat[,anc])&dat[,anc] <= 6,anc] * log(1.07)
      
      dat$predicthf_anc_cvmhfh_bnp[!is.na(dat[,anc])&dat[,anc] > 6] <- 0 
      
      
      
      ## ACM
      
      dat$predicthf_anc_acm_bnp[!is.na(dat[,anc])&dat[,anc] <= 6] <- dat[!is.na(dat[,anc])&dat[,anc] <= 6,anc] * log(1.11)
      
      dat$predicthf_anc_acm_bnp[!is.na(dat[,anc])&dat[,anc] > 6] <- 0 
      
      
      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
        ##                       Hemoglobin, g/dL                   ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_hgb_cvmhfh_ntbnp[!is.na(dat[,hgb])&dat[,hgb] < 14.0] <- (14.0-dat[!is.na(dat[,hgb])&dat[,hgb] < 14.0,hgb]) * log(1.07)
      
      dat$predicthf_hgb_cvmhfh_ntbnp[!is.na(dat[,hgb])&dat[,hgb] >= 14.0] <- 0 
      
      
      ## CVM
      
      dat$predicthf_hgb_cvm_ntbnp[!is.na(dat[,hgb])&dat[,hgb] < 14.0] <- (14.0-dat[!is.na(dat[,hgb])&dat[,hgb] < 14.0,hgb]) * log(1.10)
      
      dat$predicthf_hgb_cvm_ntbnp[!is.na(dat[,hgb])&dat[,hgb] >= 14.0] <- 0 
      
      ## ACM
      
      dat$predicthf_hgb_acm_ntbnp[!is.na(dat[,hgb])&dat[,hgb] < 14.0] <- (14.0-dat[!is.na(dat[,hgb])&dat[,hgb] < 14.0,hgb]) * log(1.11)
      
      dat$predicthf_hgb_acm_ntbnp[!is.na(dat[,hgb])&dat[,hgb] >= 14.0] <- 0 
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_hgb_cvmhfh_bnp[!is.na(dat[,hgb])&dat[,hgb] < 14.0] <- (14.0-dat[!is.na(dat[,hgb])&dat[,hgb] < 14.0,hgb]) * log(1.09)
      
      dat$predicthf_hgb_cvmhfh_bnp[!is.na(dat[,hgb])&dat[,hgb] >= 14.0] <- 0 
      
      
      ## CVM
      
      dat$predicthf_hgb_cvm_bnp[!is.na(dat[,hgb])&dat[,hgb] < 14.0] <- (14.0-dat[!is.na(dat[,hgb])&dat[,hgb] < 14.0,hgb]) * log(1.11)
      
      dat$predicthf_hgb_cvm_bnp[!is.na(dat[,hgb])&dat[,hgb] >= 14.0] <- 0 
      
      ## ACM
      
      dat$predicthf_hgb_acm_bnp[!is.na(dat[,hgb])&dat[,hgb] < 14.0] <- (14.0-dat[!is.na(dat[,hgb])&dat[,hgb] < 14.0,hgb]) * log(1.11)
      
      dat$predicthf_hgb_acm_bnp[!is.na(dat[,hgb])&dat[,hgb] >= 14.0] <- 0 
      
      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
        ##                  Blood urea nitrogen, mg/dL                  ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
      
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_bun_cvmhfh_ntbnp[!is.na(dat[,bun_val])&dat[,bun_val] > 14.01] <- (dat[!is.na(dat[,bun_val])&dat[,bun_val] > 14.01,bun_val]-14.01)/2.8 * log(1.02)
      
      dat$predicthf_bun_cvmhfh_ntbnp[!is.na(dat[,bun_val])&dat[,bun_val] <= 14.01] <- 0 
      
      
      ## CVM
      
      dat$predicthf_bun_cvm_ntbnp[!is.na(dat[,bun_val])&dat[,bun_val] > 14.01] <- (dat[!is.na(dat[,bun_val])&dat[,bun_val] > 14.01,bun_val]-14.01)/2.8 * log(1.03)
      
      dat$predicthf_bun_cvm_ntbnp[!is.na(dat[,bun_val])&dat[,bun_val] <= 14.01] <- 0 
      
      ## ACM
      
      dat$predicthf_bun_acm_ntbnp[!is.na(dat[,bun_val])&dat[,bun_val] > 14.01] <- (dat[!is.na(dat[,bun_val]-14.01)&dat[,bun_val] > 14.01,bun_val])/2.8 * log(1.02)
      
      dat$predicthf_bun_acm_ntbnp[!is.na(dat[,bun_val])&dat[,bun_val] <= 14.01] <- 0 
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_bun_cvmhfh_bnp[!is.na(dat[,bun_val])&dat[,bun_val] > 14.01] <- (dat[!is.na(dat[,bun_val])&
                                                                                           dat[,bun_val] > 14.01,bun_val]-14.01)/2.8 * log(1.03)
      
      dat$predicthf_bun_cvmhfh_bnp[!is.na(dat[,bun_val])&dat[,bun_val] <= 14.01] <- 0 
      
      
      ## CVM
      
      dat$predicthf_bun_cvm_bnp[!is.na(dat[,bun_val])&dat[,bun_val] > 14.01] <- (dat[!is.na(dat[,bun_val])&
                                                                                           dat[,bun_val] > 14.01,bun_val]-14.01)/2.8 * log(1.03)
      
      dat$predicthf_bun_cvm_bnp[!is.na(dat[,bun_val])&dat[,bun_val] <= 14.01] <- 0 
      
      ## ACM
      
      dat$predicthf_bun_acm_bnp[!is.na(dat[,bun_val])&dat[,bun_val] > 14.01] <- (dat[!is.na(dat[,bun_val])&dat[,bun_val] > 14.01,bun_val]-14.01)/2.8 * log(1.03)
      
      dat$predicthf_bun_acm_bnp[!is.na(dat[,bun_val])&dat[,bun_val] <= 14.01] <- 0 
      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
        ##                Low density lipoprotein, mg/dL                ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
      
      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_ldl_cvmhfh_ntbnp[!is.na(dat[,ldl])&dat[,ldl] > 115.83] <- (dat[!is.na(dat[,ldl])&dat[,ldl] > 115.83,ldl]-115.83)/38.61 * log(1.15)
      
      dat$predicthf_ldl_cvmhfh_ntbnp[!is.na(dat[,ldl])&dat[,ldl] <= 115.83] <- 0 
      
      
      
      ## ACM
      
      dat$predicthf_ldl_acm_ntbnp[!is.na(dat[,ldl])&dat[,ldl] > 115.83] <- (dat[!is.na(dat[,ldl])&dat[,ldl] > 115.83,ldl]-115.83)/38.61 * log(1.19)
      
      dat$predicthf_ldl_acm_ntbnp[!is.na(dat[,ldl])&dat[,ldl] <= 115.83] <- 0 
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_ldl_cvmhfh_bnp[!is.na(dat[,ldl])&dat[,ldl] > 115.83] <- (dat[!is.na(dat[,ldl])&dat[,ldl] > 115.83,ldl]-115.83)/38.61 * log(1.15)
      
      dat$predicthf_ldl_cvmhfh_bnp[!is.na(dat[,ldl])&dat[,ldl] <= 115.83] <- 0 
      
      
      
      ## ACM
      
      dat$predicthf_ldl_acm_bnp[!is.na(dat[,ldl])&dat[,ldl] > 115.83] <- (dat[!is.na(dat[,ldl])&dat[,ldl] > 115.83,ldl]-115.83)/38.61 * log(1.18)
      
      dat$predicthf_ldl_acm_bnp[!is.na(dat[,ldl])&dat[,ldl] <= 115.83] <- 0 
      
      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
        ##                   Total cholesterol, mg/dL                   ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

      
      
      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%

      ## CVM
      
      dat$predicthf_tchol_cvm_ntbnp[!is.na(dat[,tchol])&dat[,tchol] > 115.83] <- (dat[!is.na(dat[,tchol])&dat[,tchol] > 115.83,tchol]-115.83)/38.61 * log(1.09)
      
      dat$predicthf_tchol_cvm_ntbnp[!is.na(dat[,tchol])&dat[,tchol] <= 115.83] <- 0 
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%

      
      ## CVM
      
      dat$predicthf_tchol_cvm_bnp[!is.na(dat[,tchol])&dat[,tchol] > 115.83] <- (dat[!is.na(dat[,tchol])&dat[,tchol] > 115.83,tchol]-115.83)/5 * log(1.10)
      
      dat$predicthf_tchol_cvm_bnp[!is.na(dat[,tchol])&dat[,tchol] <= 115.83] <- 0 
      
      
      
      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##         Absolute lymphocyte count, cells/microliter        ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      ##%%%%%%%%%%%%
      ##  NT-BNP  %%
      ##%%%%%%%%%%%%
      
      
      ## CVM/HFH
      
      dat$predicthf_alc_cvmhfh_ntbnp[!is.na(dat[,alc])&dat[,alc] < 2.5] <- (2.5-dat[!is.na(dat[,alc])&dat[,alc] < 2.5,alc]) * log(1.06)
      
      dat$predicthf_alc_cvmhfh_ntbnp[!is.na(dat[,alc])&dat[,alc] >= 2.5] <- 0 
      
      
      
      ##%%%%%%%%%
      ##  BNP  %%
      ##%%%%%%%%%
      
      ## CVM/HFH
      
      dat$predicthf_alc_cvmhfh_bnp[!is.na(dat[,alc])&dat[,alc] < 2.5] <- (2.5-dat[!is.na(dat[,alc])&dat[,alc] < 2.5,alc]) * log(1.12)
      
      dat$predicthf_alc_cvmhfh_bnp[!is.na(dat[,alc])&dat[,alc] >= 2.5] <- 0 

      

      
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##                 Calculate PREDICT-HF scores                 ##
        ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     
      for (i in 1:nrow(dat)) {
      
      ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ##  Calculate CVM/HFH score with NT-proBNP   %%
      ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      dat[i,"predicthf_cvmhfh_ntbnp"] <- 
        sum(dat[i,"predicthf_sex_cvmhfh_ntbnp"],
            dat[i,"predicthf_hf_duration_cvmhfh_ntbnp"],
            dat[i,"predicthf_race_cvmhfh_ntbnp"],
            dat[i,"predicthf_region_cvmhfh_ntbnp"],
            dat[i,"predicthf_dm_cvmhfh_ntbnp"],
            dat[i,"predicthf_prior_hfh_cvmhfh_ntbnp"],
            dat[i,"predicthf_lvef_cvmhfh_ntbnp"],
            dat[i,"predicthf_valv_dz_cvmhfh_ntbnp"],
            dat[i,"predicthf_bbb_cvmhfh_ntbnp"],
            dat[i,"predicthf_mi_cvmhfh_ntbnp"],
            dat[i,"predicthf_pad_cvmhfh_ntbnp"],
            dat[i,"predicthf_nyha_cvmhfh_ntbnp"],
            dat[i,"predicthf_betab_cvmhfh_ntbnp"],
            dat[i,"predicthf_arni_cvmhfh_ntbnp"],
            dat[i,"predicthf_tbili_cvmhfh_ntbnp"],
            dat[i,"predicthf_uric_cvmhfh_ntbnp"],
            dat[i,"predicthf_alb_cvmhfh_ntbnp"],
            dat[i,"predicthf_k_cvmhfh_ntbnp"],
            dat[i,"predicthf_anc_cvmhfh_ntbnp"],
            dat[i,"predicthf_alc_cvmhfh_ntbnp"],
            dat[i,"predicthf_hgb_cvmhfh_ntbnp"],
            dat[i,"predicthf_ldl_cvmhfh_ntbnp"],
            dat[i,"predicthf_bun_cvmhfh_ntbnp"],
            dat[i,"predicthf_ntbnp_cat_cvmhfh_ntbnp"],
            dat[i,"predicthf_prior_hfh_latin_cvmhfh_ntbnp"],
            na.rm=T)
      
      
      dat[i,"predicthf_cvmhfh_ntbnp_nas"] <- sum(is.na(dat[i,"predicthf_sex_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_hf_duration_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_race_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_region_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_dm_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_prior_hfh_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_lvef_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_valv_dz_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_bbb_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_mi_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_pad_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_nyha_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_betab_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_arni_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_tbili_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_uric_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_alb_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_k_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_anc_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_alc_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_hgb_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_ldl_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_bun_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_ntbnp_cat_cvmhfh_ntbnp"]),
                                                is.na(dat[i,"predicthf_prior_hfh_latin_cvmhfh_ntbnp"]))
      

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##  Calculate CVM score with NT-proBNP   %%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      dat[i,"predicthf_cvm_ntbnp"] <- sum(dat[i,"predicthf_age_cvm_ntbnp"],
                                     dat[i,"predicthf_sex_cvm_ntbnp"],
                                     dat[i,"predicthf_hf_duration_cvm_ntbnp"],
                                     dat[i,"predicthf_race_cvm_ntbnp"],
                                     dat[i,"predicthf_region_cvm_ntbnp"],
                                     dat[i,"predicthf_dm_cvm_ntbnp"],
                                     dat[i,"predicthf_lvef_cvm_ntbnp"],
                                     dat[i,"predicthf_bbb_cvm_ntbnp"],
                                     dat[i,"predicthf_mi_cvm_ntbnp"],
                                     dat[i,"predicthf_pci_cvm_ntbnp"],
                                     dat[i,"predicthf_pad_cvm_ntbnp"],
                                     dat[i,"predicthf_nyha_cvm_ntbnp"],
                                     dat[i,"predicthf_sbp_cvm_ntbnp"],
                                     dat[i,"predicthf_betab_cvm_ntbnp"],
                                     dat[i,"predicthf_arni_cvm_ntbnp"],
                                     dat[i,"predicthf_tbili_cvm_ntbnp"],
                                     dat[i,"predicthf_uric_cvm_ntbnp"],
                                     dat[i,"predicthf_alb_cvm_ntbnp"],
                                     dat[i,"predicthf_k_cvm_ntbnp"],
                                     dat[i,"predicthf_hgb_cvm_ntbnp"],
                                     dat[i,"predicthf_tchol_cvm_ntbnp"],
                                     dat[i,"predicthf_bun_cvm_ntbnp"],
                                     dat[i,"predicthf_ntbnp_cat_cvm_ntbnp"],
                                     na.rm=T)
      
      
      dat[i,"predicthf_cvm_ntbnp_nas"] <- sum(is.na(dat[i,"predicthf_age_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_sex_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_hf_duration_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_race_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_region_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_dm_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_lvef_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_bbb_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_mi_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_pci_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_pad_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_nyha_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_sbp_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_betab_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_arni_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_tbili_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_uric_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_alb_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_k_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_hgb_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_tchol_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_bun_cvm_ntbnp"]),
                                     is.na(dat[i,"predicthf_ntbnp_cat_cvm_ntbnp"]))
      

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##  Calculate ACM score with NT-proBNP   %%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      dat[i,"predicthf_acm_ntbnp"] <- sum(dat[i,"predicthf_age_acm_ntbnp"],
                                          dat[i,"predicthf_sex_acm_ntbnp"],
                                          dat[i,"predicthf_hf_duration_acm_ntbnp"],
                                          dat[i,"predicthf_race_acm_ntbnp"],
                                          dat[i,"predicthf_region_acm_ntbnp"],
                                          dat[i,"predicthf_dm_acm_ntbnp"],
                                          dat[i,"predicthf_lvef_acm_ntbnp"],
                                          dat[i,"predicthf_bmi_acm_ntbnp"],
                                          dat[i,"predicthf_mi_acm_ntbnp"],
                                          dat[i,"predicthf_pci_acm_ntbnp"],
                                          dat[i,"predicthf_pad_acm_ntbnp"],
                                          dat[i,"predicthf_nyha_acm_ntbnp"],
                                          dat[i,"predicthf_sbp_acm_ntbnp"],
                                          dat[i,"predicthf_betab_acm_ntbnp"],
                                          dat[i,"predicthf_arni_acm_ntbnp"],
                                          dat[i,"predicthf_tbili_acm_ntbnp"],
                                          dat[i,"predicthf_uric_acm_ntbnp"],
                                          dat[i,"predicthf_alb_acm_ntbnp"],
                                          dat[i,"predicthf_k_acm_ntbnp"],
                                          dat[i,"predicthf_cl_acm_ntbnp"],
                                          dat[i,"predicthf_anc_acm_ntbnp"],
                                          dat[i,"predicthf_mono_acm_ntbnp"],
                                          dat[i,"predicthf_hgb_acm_ntbnp"],
                                          dat[i,"predicthf_ldl_acm_ntbnp"],
                                          dat[i,"predicthf_tg_acm_ntbnp"],
                                          dat[i,"predicthf_bun_acm_ntbnp"],
                                          dat[i,"predicthf_ntbnp_cat_acm_ntbnp"],
                                          na.rm=T)
      
      
      dat[i,"predicthf_acm_ntbnp_nas"] <- sum(is.na(dat[i,"predicthf_age_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_sex_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_hf_duration_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_race_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_region_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_dm_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_lvef_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_bmi_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_mi_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_pci_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_pad_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_nyha_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_sbp_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_betab_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_arni_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_tbili_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_uric_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_alb_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_k_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_cl_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_anc_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_mono_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_hgb_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_ldl_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_tg_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_bun_acm_ntbnp"]),
                                              is.na(dat[i,"predicthf_ntbnp_cat_acm_ntbnp"]))
      

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##  Calculate CVM/HFH score with BNP   %%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      dat[i,"predicthf_cvmhfh_bnp"] <- sum(dat[i,"predicthf_dm_cvmhfh_bnp"],
                                            dat[i,"predicthf_hf_duration_cvmhfh_bnp"],
                                            dat[i,"predicthf_prior_hfh_cvmhfh_bnp"],
                                            dat[i,"predicthf_arni_cvmhfh_bnp"],
                                            dat[i,"predicthf_race_cvmhfh_bnp"],
                                            dat[i,"predicthf_region_cvmhfh_bnp"],
                                            dat[i,"predicthf_nyha_cvmhfh_bnp"],
                                            dat[i,"predicthf_lvef_cvmhfh_bnp"],
                                            dat[i,"predicthf_valv_dz_cvmhfh_bnp"],
                                            dat[i,"predicthf_betab_cvmhfh_bnp"],
                                            dat[i,"predicthf_pad_cvmhfh_bnp"],
                                            dat[i,"predicthf_bbb_cvmhfh_bnp"],
                                            dat[i,"predicthf_sex_cvmhfh_bnp"],
                                            dat[i,"predicthf_mi_cvmhfh_bnp"],
                                            dat[i,"predicthf_prior_hfh_latin_cvmhfh_bnp"],
                                            dat[i,"predicthf_tbili_cvmhfh_bnp"],
                                            dat[i,"predicthf_uric_cvmhfh_bnp"],
                                            dat[i,"predicthf_alb_cvmhfh_bnp"],
                                            dat[i,"predicthf_k_cvmhfh_bnp"],
                                            dat[i,"predicthf_anc_cvmhfh_bnp"],
                                            dat[i,"predicthf_hgb_cvmhfh_bnp"],
                                            dat[i,"predicthf_bun_cvmhfh_bnp"],
                                            dat[i,"predicthf_ldl_cvmhfh_bnp"],
                                            dat[i,"predicthf_alc_cvmhfh_bnp"],
                                            dat[i,"predicthf_bnp_cat_cvmhfh_bnp"],
                                            na.rm=T)
      
      
      dat[i,"predicthf_cvmhfh_bnp_nas"] <- sum(is.na(dat[i,"predicthf_dm_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_hf_duration_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_prior_hfh_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_arni_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_race_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_region_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_nyha_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_lvef_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_valv_dz_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_betab_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_pad_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_bbb_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_sex_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_mi_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_prior_hfh_latin_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_tbili_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_uric_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_alb_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_k_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_anc_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_hgb_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_bun_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_ldl_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_alc_cvmhfh_bnp"]),
                                               is.na(dat[i,"predicthf_bnp_cat_cvmhfh_bnp"]))
                                           
      
      
      ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ##  Calculate CVM score with BNP   %%
      ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      dat[i,"predicthf_cvm_bnp"] <- sum(dat[i,"predicthf_region_cvm_bnp"], 
                                        dat[i,"predicthf_race_cvm_bnp"],
                                        dat[i,"predicthf_nyha_cvm_bnp"], 
                                        dat[i,"predicthf_age_cvm_bnp"], 
                                        dat[i,"predicthf_arni_cvm_bnp"], 
                                        dat[i,"predicthf_dm_cvm_bnp"], 
                                        dat[i,"predicthf_hf_duration_cvm_bnp"], 
                                        dat[i,"predicthf_lvef_cvm_bnp"], 
                                        dat[i,"predicthf_pad_cvm_bnp"], 
                                        dat[i,"predicthf_sex_cvm_bnp"], 
                                        dat[i,"predicthf_betab_cvm_bnp"], 
                                        dat[i,"predicthf_bbb_cvm_bnp"], 
                                        dat[i,"predicthf_pci_cvm_bnp"], 
                                        dat[i,"predicthf_sbp_cvm_bnp"], 
                                        dat[i,"predicthf_mi_cvm_bnp"], 
                                        dat[i,"predicthf_tbili_cvm_bnp"], 
                                        dat[i,"predicthf_k_cvm_bnp"],
                                        dat[i,"predicthf_uric_cvm_bnp"], 
                                        dat[i,"predicthf_alb_cvm_bnp"],
                                        dat[i,"predicthf_hgb_cvm_bnp"], 
                                        dat[i,"predicthf_bun_cvm_bnp"],
                                        dat[i,"predicthf_tchol_cvm_bnp"], 
                                        dat[i,"predicthf_bnp_cat_cvm_bnp"],
                                        na.rm=T)
        
        
      dat[i,"predicthf_cvm_bnp_nas"] <- sum(is.na(dat[i,"predicthf_region_cvm_bnp"]), 
                                        is.na(dat[i,"predicthf_race_cvm_bnp"]),
                                        is.na(dat[i,"predicthf_nyha_cvm_bnp"]), 
                                        is.na(dat[i,"predicthf_age_cvm_bnp"]), 
                                        is.na(dat[i,"predicthf_arni_cvm_bnp"]), 
                                        is.na(dat[i,"predicthf_dm_cvm_bnp"]), 
                                        is.na(dat[i,"predicthf_hf_duration_cvm_bnp"]), 
                                        is.na(dat[i,"predicthf_lvef_cvm_bnp"]), 
                                        is.na(dat[i,"predicthf_pad_cvm_bnp"]), 
                                        is.na(dat[i,"predicthf_sex_cvm_bnp"]), 
                                        is.na(dat[i,"predicthf_betab_cvm_bnp"]), 
                                        is.na(dat[i,"predicthf_bbb_cvm_bnp"]), 
                                        is.na(dat[i,"predicthf_pci_cvm_bnp"]), 
                                        is.na(dat[i,"predicthf_sbp_cvm_bnp"]), 
                                        is.na(dat[i,"predicthf_mi_cvm_bnp"]), 
                                        is.na(dat[i,"predicthf_tbili_cvm_bnp"]), 
                                        is.na(dat[i,"predicthf_k_cvm_bnp"]),
                                        is.na(dat[i,"predicthf_uric_cvm_bnp"]), 
                                        is.na(dat[i,"predicthf_alb_cvm_bnp"]),
                                        is.na(dat[i,"predicthf_hgb_cvm_bnp"]), 
                                        is.na(dat[i,"predicthf_bun_cvm_bnp"]),
                                        is.na(dat[i,"predicthf_tchol_cvm_bnp"]), 
                                        is.na(dat[i,"predicthf_bnp_cat_cvm_bnp"]))
      
      
      ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ##  Calculate ACM score with BNP   %%
      ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      
      dat[i,"predicthf_acm_bnp"] <- sum(dat[i,"predicthf_region_acm_bnp"], 
                                        dat[i,"predicthf_age_acm_bnp"], 
                                        dat[i,"predicthf_nyha_acm_bnp"], 
                                        dat[i,"predicthf_hf_duration_acm_bnp"], 
                                        dat[i,"predicthf_dm_acm_bnp"], 
                                        dat[i,"predicthf_sex_acm_bnp"], 
                                        dat[i,"predicthf_pci_acm_bnp"], 
                                        dat[i,"predicthf_pad_acm_bnp"], 
                                        dat[i,"predicthf_race_acm_bnp"], 
                                        dat[i,"predicthf_arni_acm_bnp"], 
                                        dat[i,"predicthf_lvef_acm_bnp"], 
                                        dat[i,"predicthf_betab_acm_bnp"], 
                                        dat[i,"predicthf_sbp_acm_bnp"], 
                                        dat[i,"predicthf_mi_acm_bnp"], 
                                        dat[i,"predicthf_alb_acm_bnp"], 
                                        dat[i,"predicthf_anc_acm_bnp"], 
                                        dat[i,"predicthf_uric_acm_bnp"], 
                                        dat[i,"predicthf_cl_acm_bnp"], 
                                        dat[i,"predicthf_hgb_acm_bnp"], 
                                        dat[i,"predicthf_mono_acm_bnp"], 
                                        dat[i,"predicthf_tbili_acm_bnp"], 
                                        dat[i,"predicthf_ldl_acm_bnp"], 
                                        dat[i,"predicthf_k_acm_bnp"], 
                                        dat[i,"predicthf_bun_acm_bnp"], 
                                        dat[i,"predicthf_ast_acm_bnp"], 
                                        dat[i,"predicthf_bnp_cat_acm_bnp"],
                                        na.rm=T)
        
      
      
      dat[i,"predicthf_acm_bnp_nas"] <- sum(is.na(dat[i,"predicthf_region_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_age_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_nyha_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_hf_duration_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_dm_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_sex_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_pci_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_pad_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_race_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_arni_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_lvef_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_betab_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_sbp_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_mi_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_alb_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_anc_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_uric_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_cl_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_hgb_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_mono_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_tbili_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_ldl_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_k_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_bun_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_ast_acm_bnp"]), 
                                            is.na(dat[i,"predicthf_bnp_cat_acm_bnp"]))
      
      
      ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ##  Calculate CVM/HFH score with no BNP   %%
      ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      
      dat[i,"predicthf_cvmhfh_nobnp"] <- sum(dat[i,"predicthf_lvef_cvmhfh_nobnp"],
                                            dat[i,"predicthf_dm_cvmhfh_nobnp"],
                                            dat[i,"predicthf_race_cvmhfh_nobnp"],
                                            dat[i,"predicthf_hf_duration_cvmhfh_nobnp"],
                                            dat[i,"predicthf_prior_hfh_cvmhfh_nobnp"],
                                            dat[i,"predicthf_nyha_cvmhfh_nobnp"],
                                            dat[i,"predicthf_hr_cvmhfh_nobnp"],
                                            dat[i,"predicthf_arni_cvmhfh_nobnp"],
                                            dat[i,"predicthf_age_cvmhfh_nobnp"],
                                            dat[i,"predicthf_sex_cvmhfh_nobnp"],
                                            dat[i,"predicthf_bbb_cvmhfh_nobnp"],
                                            dat[i,"predicthf_pad_cvmhfh_nobnp"],
                                            dat[i,"predicthf_valv_dz_cvmhfh_nobnp"],
                                            dat[i,"predicthf_betab_cvmhfh_nobnp"],
                                            dat[i,"predicthf_region_cvmhfh_nobnp"],
                                            dat[i,"predicthf_ckd_cvmhfh_nobnp"],
                                            dat[i,"predicthf_mi_cvmhfh_nobnp"],
                                            dat[i,"predicthf_sbp_cvmhfh_nobnp"],
                                            dat[i,"predicthf_af_cvmhfh_nobnp"],
                                            dat[i,"predicthf_llt_cvmhfh_nobnp"],
                                            na.rm=T)
        

      dat[i,"predicthf_cvmhfh_nobnp_nas"] <- sum(is.na(dat[i,"predicthf_lvef_cvmhfh_nobnp"]),
                                             is.na(dat[i,"predicthf_dm_cvmhfh_nobnp"]),
                                             is.na(dat[i,"predicthf_race_cvmhfh_nobnp"]),
                                             is.na(dat[i,"predicthf_hf_duration_cvmhfh_nobnp"]),
                                             is.na(dat[i,"predicthf_prior_hfh_cvmhfh_nobnp"]),
                                             is.na(dat[i,"predicthf_nyha_cvmhfh_nobnp"]),
                                             is.na(dat[i,"predicthf_hr_cvmhfh_nobnp"]),
                                             is.na(dat[i,"predicthf_arni_cvmhfh_nobnp"]),
                                             is.na(dat[i,"predicthf_age_cvmhfh_nobnp"]),
                                             is.na(dat[i,"predicthf_sex_cvmhfh_nobnp"]),
                                             is.na(dat[i,"predicthf_bbb_cvmhfh_nobnp"]),
                                             is.na(dat[i,"predicthf_pad_cvmhfh_nobnp"]),
                                             is.na(dat[i,"predicthf_valv_dz_cvmhfh_nobnp"]),
                                             is.na(dat[i,"predicthf_betab_cvmhfh_nobnp"]),
                                             is.na(dat[i,"predicthf_region_cvmhfh_nobnp"]),
                                             is.na(dat[i,"predicthf_ckd_cvmhfh_nobnp"]),
                                             is.na(dat[i,"predicthf_mi_cvmhfh_nobnp"]),
                                             is.na(dat[i,"predicthf_sbp_cvmhfh_nobnp"]),
                                             is.na(dat[i,"predicthf_af_cvmhfh_nobnp"]),
                                             is.na(dat[i,"predicthf_llt_cvmhfh_nobnp"]))
      
      ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ##  Calculate CVM score with no BNP   %%
      ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      
      dat[i,"predicthf_cvm_nobnp"] <- sum(dat[i,"predicthf_nyha_cvm_nobnp"], 
                                          dat[i,"predicthf_lvef_cvm_nobnp"], 
                                          dat[i,"predicthf_race_cvm_nobnp"], 
                                          dat[i,"predicthf_age_cvm_nobnp"], 
                                          dat[i,"predicthf_sex_cvm_nobnp"], 
                                          dat[i,"predicthf_hf_duration_cvm_nobnp"], 
                                          dat[i,"predicthf_pad_cvm_nobnp"], 
                                          dat[i,"predicthf_dm_cvm_nobnp"], 
                                          dat[i,"predicthf_arni_cvm_nobnp"], 
                                          dat[i,"predicthf_bbb_cvm_nobnp"], 
                                          dat[i,"predicthf_hr_cvm_nobnp"], 
                                          dat[i,"predicthf_bmi_cvm_nobnp"], 
                                          dat[i,"predicthf_betab_cvm_nobnp"], 
                                          dat[i,"predicthf_sbp_cvm_nobnp"], 
                                          dat[i,"predicthf_pci_cvm_nobnp"], 
                                          dat[i,"predicthf_prior_hfh_cvm_nobnp"], 
                                          dat[i,"predicthf_mi_cvm_nobnp"],
                                          na.rm=T)
        

      
      dat[i,"predicthf_cvm_nobnp_nas"] <- sum(is.na(dat[i,"predicthf_nyha_cvm_nobnp"]), 
                                          is.na(dat[i,"predicthf_lvef_cvm_nobnp"]), 
                                          is.na(dat[i,"predicthf_race_cvm_nobnp"]), 
                                          is.na(dat[i,"predicthf_age_cvm_nobnp"]), 
                                          is.na(dat[i,"predicthf_sex_cvm_nobnp"]), 
                                          is.na(dat[i,"predicthf_hf_duration_cvm_nobnp"]), 
                                          is.na(dat[i,"predicthf_pad_cvm_nobnp"]), 
                                          is.na(dat[i,"predicthf_dm_cvm_nobnp"]), 
                                          is.na(dat[i,"predicthf_arni_cvm_nobnp"]), 
                                          is.na(dat[i,"predicthf_bbb_cvm_nobnp"]), 
                                          is.na(dat[i,"predicthf_hr_cvm_nobnp"]), 
                                          is.na(dat[i,"predicthf_bmi_cvm_nobnp"]), 
                                          is.na(dat[i,"predicthf_betab_cvm_nobnp"]), 
                                          is.na(dat[i,"predicthf_sbp_cvm_nobnp"]), 
                                          is.na(dat[i,"predicthf_pci_cvm_nobnp"]), 
                                          is.na(dat[i,"predicthf_prior_hfh_cvm_nobnp"]), 
                                          is.na(dat[i,"predicthf_mi_cvm_nobnp"]))
      
      
      ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ##  Calculate ACM score with no BNP   %%
      ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      dat[i,"predicthf_acm_nobnp"] <- sum(dat[i,"predicthf_age_acm_nobnp"],
                                          dat[i,"predicthf_lvef_acm_nobnp"],
                                          dat[i,"predicthf_nyha_acm_nobnp"],
                                          dat[i,"predicthf_sex_acm_nobnp"], 
                                          dat[i,"predicthf_pad_acm_nobnp"],
                                          dat[i,"predicthf_dm_acm_nobnp"],
                                          dat[i,"predicthf_hr_acm_nobnp"],
                                          dat[i,"predicthf_bmi_acm_nobnp"],
                                          dat[i,"predicthf_race_acm_nobnp"],
                                          dat[i,"predicthf_hf_duration_acm_nobnp"],
                                          dat[i,"predicthf_bmi_acm_nobnp"],
                                          dat[i,"predicthf_sbp_acm_nobnp"],
                                          dat[i,"predicthf_pci_acm_nobnp"],
                                          dat[i,"predicthf_arni_acm_nobnp"],
                                          dat[i,"predicthf_prior_hfh_acm_nobnp"],
                                          dat[i,"predicthf_betab_acm_nobnp"],
                                          dat[i,"predicthf_mi_acm_nobnp"],
                                          dat[i,"predicthf_llt_acm_nobnp"],
                                          na.rm=T)

      dat[i,"predicthf_acm_nobnp_nas"] <- sum(is.na(dat[i,"predicthf_age_acm_nobnp"]),
                                          is.na(dat[i,"predicthf_lvef_acm_nobnp"]),
                                          is.na(dat[i,"predicthf_nyha_acm_nobnp"]),
                                          is.na(dat[i,"predicthf_sex_acm_nobnp"]), 
                                          is.na(dat[i,"predicthf_pad_acm_nobnp"]),
                                          is.na(dat[i,"predicthf_dm_acm_nobnp"]),
                                          is.na(dat[i,"predicthf_hr_acm_nobnp"]),
                                          is.na(dat[i,"predicthf_bmi_acm_nobnp"]),
                                          is.na(dat[i,"predicthf_race_acm_nobnp"]),
                                          is.na(dat[i,"predicthf_hf_duration_acm_nobnp"]),
                                          is.na(dat[i,"predicthf_bmi_acm_nobnp"]),
                                          is.na(dat[i,"predicthf_sbp_acm_nobnp"]),
                                          is.na(dat[i,"predicthf_pci_acm_nobnp"]),
                                          is.na(dat[i,"predicthf_arni_acm_nobnp"]),
                                          is.na(dat[i,"predicthf_prior_hfh_acm_nobnp"]),
                                          is.na(dat[i,"predicthf_betab_acm_nobnp"]),
                                          is.na(dat[i,"predicthf_mi_acm_nobnp"]),
                                          is.na(dat[i,"predicthf_llt_acm_nobnp"]))
      }      

      
dat$predicthf_cvmhfh_ntbnp[dat$predicthf_cvmhfh_ntbnp_nas > max_missing_vals] <- NA       
dat$predicthf_cvm_ntbnp[dat$predicthf_cvm_ntbnp_nas > max_missing_vals] <- NA       
dat$predicthf_acm_ntbnp[dat$predicthf_acm_ntbnp_nas > max_missing_vals] <- NA       

dat$predicthf_cvmhfh_bnp[dat$predicthf_cvmhfh_bnp_nas > max_missing_vals] <- NA       
dat$predicthf_cvm_bnp[dat$predicthf_cvm_bnp_nas > max_missing_vals] <- NA       
dat$predicthf_acm_bnp[dat$predicthf_acm_bnp_nas > max_missing_vals] <- NA       

dat$predicthf_cvmhfh_nobnp[dat$predicthf_cvmhfh_nobnp_nas > max_missing_vals] <- NA       
dat$predicthf_cvm_nobnp[dat$predicthf_cvm_nobnp_nas > max_missing_vals] <- NA       
dat$predicthf_acm_nobnp[dat$predicthf_acm_nobnp_nas > max_missing_vals] <- NA       

this_score <- paste("predicthf_",endpoint,"_",bnp_type,sep="")      
      
return(dat[,this_score])      

}            
                      

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###                                                                     ###
####                           BCN BIO-HF SCORE                        ####
###                                                                     ###
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bcn_biohf_score <- function(dat, 
                           age="age_val",
                           sex="sex",
                           nyha="nyha",
                           lvef="ef_val",
                           gfr="crcl", 
                           hgb="hgb",
                           na="na",
                           diuretic_dose="diuretic_dose",
                           statin="statin",
                           acearb="acearb",
                           betab="betab",
                           ntbnp="ntbnp_val",
                           maxnas=0) 
{
  
# Lupón J, de Antonio M, Vila J, Peñafiel J,
# Galán A, Zamora E, Urrutia A, and Bayes-Genis A 
# Development of a novel heart failure risk tool: the Barcelona Bio-Heart Failure Risk
# Calculator (BCN Bio-HF calculator). PloS One. 2014;9(1):e85466

# This function only includes clinical and clincial + NTBNP due to inconsistent availability of Hs-cTnT and ST2
# Calculates clinical only by default.  Can specify NTBNP model (model 2) if desired

# Survival at covariate means (~reference for HRs), 1 yr = 0.942; 2 year = 0.875; 3 year = 0.802
# Sum product = -8.922 (at covariate)
# Calc:   Survival at x years = survival at covariate means for x years^exp(sum of coeffs-sum product)
# Example: Survival at 3 years = 0.802^exp(score-(-8.922))

# See 'bcn_coeffs.csv' for all 8 models
  
#### ***** Clinical only ***** #####

# 0.04054   * age
# -0.44658  if female
# 0.63749   if NYHA III/IV
# -0.37702  LVEF > 45%
# -0.06251  * sodium mEq/L
# -0.01001  * GFR ml/min/m2
# -0.12002  * hemoglobin (g/dL)
# 0.22825   if furosemide dose ≤ 40 or torsemide ≤ 10 mg daily
# 0.56390   if furosemide dose > 40 or torsemide > 10
# -0.46052  if on statin
# -0.40481  if on ACE/ARB
# -0.58605  if on BB

  dat$bcn_age <- dat[!is.na(dat[,age]),age]*0.04054

  dat$bcn_sex[!is.na(dat[,sex])&dat[,sex] %in% F] <- -0.44658
  dat$bcn_sex[!is.na(dat[,sex])&dat[,sex] %in% M] <- 0
  
  dat$bcn_nyha[!is.na(dat[,nyha])&dat[,nyha] %in% c(3,4)] <- 0.63749
  dat$bcn_nyha[!is.na(dat[,nyha])&dat[,nyha] %in% c(1,2)] <- 0
  
  dat$bcn_lvef[!is.na(dat[,lvef])&dat[,lvef] < 45] <- -0.37702
  dat$bcn_lvef[!is.na(dat[,lvef])&dat[,lvef] >= 45] <- 0
  
  dat$bcn_sodium <- dat[!is.na(dat[,na]),na] * -0.01001

  dat$bcn_gfr <- dat[!is.na(dat[,gfr]),gfr] * -0.01001

  dat$bcn_hgb <- dat[!is.na(dat[,hgb]),hgb] * -0.12002

  dat$bcn_diuretic[!is.na(dat[,diuretic_dose])&dat[,diuretic_dose] == 0] <- 0
  dat$bcn_diuretic[!is.na(dat[,diuretic_dose])&dat[,diuretic_dose] <= 40] <- 0.22825
  dat$bcn_diuretic[!is.na(dat[,diuretic_dose])&dat[,diuretic_dose] > 40] <- 0.56390

  dat$bcn_statin[!is.na(dat[,statin])&dat[,statin] %in% Y] <- -0.46052
  dat$bcn_statin[!is.na(dat[,statin])&dat[,statin] %in% N] <- 0
  
  dat$bcn_acearb[!is.na(dat[,acearb])&dat[,acearb] %in% Y] <- -0.40481
  dat$bcn_acearb[!is.na(dat[,acearb])&dat[,acearb] %in% N] <- 0
  
  dat$bcn_betab[!is.na(dat[,betab])&dat[,betab] %in% Y] <- -0.58605
  dat$bcn_betab[!is.na(dat[,betab])&dat[,betab] %in% N] <- 0

  
  for (i in 1:nrow(dat)) {
  
  dat[i,"bcn_score"] <- sum(dat[i,"bcn_age"],
                       dat[i,"bcn_sex"],
                       dat[i,"bcn_nyha"],
                       dat[i,"bcn_lvef"],
                       dat[i,"bcn_sodium"],
                       dat[i,"bcn_gfr"],
                       dat[i,"bcn_hgb"],
                       dat[i,"bcn_diuretic"],
                       dat[i,"bcn_statin"],
                       dat[i,"bcn_acearb"],
                       dat[i,"bcn_betab"],
                       na.rm=T)  

  
  dat[i,"bcn_score_nas"] <- sum(is.na(dat[i,"bcn_age"]),
                       is.na(dat[i,"bcn_sex"]),
                       is.na(dat[i,"bcn_nyha"]),
                       is.na(dat[i,"bcn_lvef"]),
                       is.na(dat[i,"bcn_sodium"]),
                       is.na(dat[i,"bcn_gfr"]),
                       is.na(dat[i,"bcn_hgb"]),
                       is.na(dat[i,"bcn_diuretic"]),
                       is.na(dat[i,"bcn_statin"]),
                       is.na(dat[i,"bcn_acearb"]),
                       is.na(dat[i,"bcn_betab"]),
                       na.rm=T)  
  }
  

        
#### ***** Clinical + NTBNP ***** ####

# 0.04037   * age (yrs)
# -0.55148  if female
# 0.55370   NYHA III/IV
# -0.05737  * sodium mEq/L
# -0.11560  * hemoglobin (g/dL)
# 0.27624   if furosemide dose ≤ 40 or torsemide ≤ 10 mg daily
# 0.64405   if furosemide dose > 40 or torsemide > 10
# -0.37624  if on statin
# -0.58492  if on BB
# 0.29549 * log(NTproBNP)  

  dat$bcn_ntbnp_age <- dat[!is.na(dat[,age]),age] * 0.04037

  dat$bcn_ntbnp_sex[!is.na(dat[,sex])&dat[,sex] %in% Fe] <- -0.55148
  dat$bcn_ntbnp_sex[!is.na(dat[,sex])&dat[,sex] %in% M] <- 0
  
  dat$bcn_ntbnp_nyha[!is.na(dat[,nyha])&dat[,nyha] %in% c(3,4)] <- 0.53370
  dat$bcn_ntbnp_nyha[!is.na(dat[,nyha])&dat[,nyha] %in% c(1,2)] <- 0
  
  dat$bcn_ntbnp_sodium <- dat[!is.na(dat[,na]),na]*(-0.05737)

  dat$bcn_ntbnp_hgb <- dat[!is.na(dat[,hgb]),hgb]*(-0.11560)

  dat$bcn_ntbnp_diuretic[!is.na(dat[,diuretic_dose])&dat[,diuretic_dose] == 0] <- 0
  dat$bcn_ntbnp_diuretic[!is.na(dat[,diuretic_dose])&dat[,diuretic_dose] <= 40] <- 0.27624
  dat$bcn_ntbnp_diuretic[!is.na(dat[,diuretic_dose])&dat[,diuretic_dose] > 40] <- 0.64405
  
  dat$bcn_ntbnp_statin[!is.na(dat[,statin])&dat[,statin] %in% Y] <- -0.37624
  dat$bcn_ntbnp_statin[!is.na(dat[,statin])&dat[,statin] %in% N] <- 0
  
  dat$bcn_ntbnp_betab[!is.na(dat[,betab])&dat[,betab] %in% Y] <- -0.58492
  dat$bcn_ntbnp_betab[!is.na(dat[,betab])&dat[,betab] %in% N] <- 0
  
  dat$bcn_ntbnp_log_ntbnp <- log(dat[!is.na(dat[,ntbnp]),ntbnp]) * 0.29549

  
  for (i in 1:nrow(dat)) {
  
  dat[i,"bcn_ntbnp_score"] <- sum(dat[i,"bcn_ntbnp_age"],
                             dat[i,"bcn_ntbnp_sex"],
                             dat[i,"bcn_ntbnp_nyha"],
                             dat[i,"bcn_ntbnp_sodium"],
                             dat[i,"bcn_ntbnp_gfr"],
                             dat[i,"bcn_ntbnp_hgb"],
                             dat[i,"bcn_ntbnp_diuretic"],
                             dat[i,"bcn_ntbnp_statin"],
                             dat[i,"bcn_ntbnp_betab"],
                             dat[i,"bcn_ntbnp_log_ntbnp"],
                             na.rm=T)  
  
  dat[i,"bcn_ntbnp_score_nas"] <- sum(is.na(dat[i,"bcn_ntbnp_age"]),
                                is.na(dat[i,"bcn_ntbnp_sex"]),
                                is.na(dat[i,"bcn_ntbnp_nyha"]),
                                is.na(dat[i,"bcn_ntbnp_lvef"]),
                                is.na(dat[i,"bcn_ntbnp_sodium"]),
                                is.na(dat[i,"bcn_ntbnp_gfr"]),
                                is.na(dat[i,"bcn_ntbnp_hgb"]),
                                is.na(dat[i,"bcn_ntbnp_diuretic"]),
                                is.na(dat[i,"bcn_ntbnp_statin"]),
                                is.na(dat[i,"bcn_ntbnp_acearb"]),
                                is.na(dat[i,"bcn_ntbnp_betab"]),
                                is.na(dat[i,"bcn_ntbnp_log_ntbnp"]))  
  
  }
  
  dat$bcn_score[dat$bcn_score_nas > max_missing_vals] <- NA
  dat$bcn_ntbnp_score[dat$bcn_ntbnp_score_nas>max_missing_vals] <- NA
  
  return(dat[,c("bcn_score","bcn_ntbnp_score")])


  }



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###                                                                     ###
####                              BIOSTAT-CHF                          ####
###                                                                     ###
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

biostatchf_score <- function(dat, 
                             age="age_val",
                             bun="bun_val",
                             hdl="hdl_val",
                             gfr="crcl", 
                             hgb="hgb",
                             na="na",
                             sbp="sbp_val",
                             betab="betab",
                             prior_hfh="prior_hfh",
                             ntbnp="ntbnp_val",
                             edema="edema",
                             max_missing_vals=0) 
{

#  Voors AA, Ouwerkerk W, Zannad F, van Veldhuisen DJ, Samani NJ, Ponikowski P,
#  Ng LL, Metra M, ter Maaten JM, Lang CC, Hillege HL, van der Harst P,
#  Filippatos G, Dickstein K, Cleland JG, Anker SD,  Zwinderman, AH. Development
#  and validation of multivariable models to predict mortality and
#  hospitalization in patients with heart failure. European Journal of Heart
#  Failure. 2017;19(5):627–634.

# NT-proBNP > 4000 pg/mL (mean 6934)
# BUN > 30.8 mg/dL (mean 39.2)
# HDL < 40.6 mg/dL (mean 42.54)
# Age > 70 years (mean 55)
# Sodium < 140 mmol/L (mean 139)
# Haemoglobin <12 g/dL (mean 13)
# eGFR (CKD-EPI formula) < 40 mL/min (62)
# SBP < 140 mmHg (124)
# No Beta-blockers (No)
# Prior HFH in past year (No)
# Peripheral edema (No)
  
# Sum mean
    
#### ***** All-cause mortality ***** #####
  
dat$biostat_acm_age[!is.na(dat[,age])&dat[,age]>70] <- 1   
dat$biostat_acm_age[!is.na(dat[,age])&dat[,age]<=70] <- 0   

dat$biostat_acm_bun[!is.na(dat[,bun])&dat[,bun]>30.8] <- 1   
dat$biostat_acm_bun[!is.na(dat[,bun])&dat[,bun]<=30.8] <- 0   

dat$biostat_acm_hgb[!is.na(dat[,hgb])&dat[,hgb]<12] <- 1   
dat$biostat_acm_hgb[!is.na(dat[,hgb])&dat[,hgb]>=12] <- 0   

dat$biostat_acm_ntbnp[!is.na(dat[,ntbnp])&dat[,ntbnp]>4000] <- 1   
dat$biostat_acm_ntbnp[!is.na(dat[,ntbnp])&dat[,ntbnp]<=4000] <- 0   

dat$biostat_acm_betab[!is.na(dat[,betab])&dat[,betab] %in% N] <- 1   
dat$biostat_acm_betab[!is.na(dat[,betab])&dat[,betab] %in% Y] <- 0   



#### ***** HF hospitalization ***** ####
  
dat$biostat_hfhosp_age[!is.na(dat[,age])&dat[,age]>70] <- 1   
dat$biostat_hfhosp_age[!is.na(dat[,age])&dat[,age]<=70] <- 0   

dat$biostat_hfhosp_sbp[!is.na(dat[,sbp])&dat[,sbp]<140] <- 1   
dat$biostat_hfhosp_sbp[!is.na(dat[,sbp])&dat[,sbp]>=140] <- 0   

dat$biostat_hfhosp_gfr[!is.na(dat[,gfr])&dat[,gfr]<40] <- 1   
dat$biostat_hfhosp_gfr[!is.na(dat[,gfr])&dat[,gfr]>=40] <- 0   

dat$biostat_hfhosp_prior_hfh[!is.na(dat[,prior_hfh])&dat[,prior_hfh] %in% Y] <- 1   
dat$biostat_hfhosp_prior_hfh[!is.na(dat[,prior_hfh])&dat[,prior_hfh] %in% N] <- 0   

dat$biostat_hfhosp_edema[!is.na(dat[,edema])&dat[,edema] %in% Y] <- 1   
dat$biostat_hfhosp_edema[!is.na(dat[,edema])&dat[,edema] %in% N] <- 0   




#### ***** ACM + HF hospitalization ***** ####

dat$biostat_acm_hfhosp_age[!is.na(dat[,age])&dat[,age]>70] <- 1   
dat$biostat_acm_hfhosp_age[!is.na(dat[,age])&dat[,age]<=70] <- 0   

dat$biostat_acm_hfhosp_hdl[!is.na(dat[,hdl])&dat[,hdl]<40.6] <- 1   
dat$biostat_acm_hfhosp_hdl[!is.na(dat[,hdl])&dat[,hdl]>=40.6] <- 0   

dat$biostat_acm_hfhosp_na[!is.na(dat[,na])&dat[,na]<140] <- 1   
dat$biostat_acm_hfhosp_na[!is.na(dat[,na])&dat[,na]>=140] <- 0   

dat$biostat_acm_hfhosp_hgb[!is.na(dat[,hgb])&dat[,hgb]<12] <- 1   
dat$biostat_acm_hfhosp_hgb[!is.na(dat[,hgb])&dat[,hgb]>=12] <- 0   

dat$biostat_acm_hfhosp_sbp[!is.na(dat[,sbp])&dat[,sbp]<140] <- 1   
dat$biostat_acm_hfhosp_sbp[!is.na(dat[,sbp])&dat[,sbp]>=140] <- 0   

dat$biostat_acm_hfhosp_betab[!is.na(dat[,betab])&dat[,betab] %in% N] <- 1   
dat$biostat_acm_hfhosp_betab[!is.na(dat[,betab])&dat[,betab] %in% Y] <- 0   

dat$biostat_acm_hfhosp_prior_hfh[!is.na(dat[,prior_hfh])&dat[,prior_hfh] %in% Y] <- 1   
dat$biostat_acm_hfhosp_prior_hfh[!is.na(dat[,prior_hfh])&dat[,prior_hfh] %in% N] <- 0   

dat$biostat_acm_hfhosp_edema[!is.na(dat[,edema])&dat[,edema] %in% Y] <- 1   
dat$biostat_acm_hfhosp_edema[!is.na(dat[,edema])&dat[,edema] %in% N] <- 0   

dat$biostat_acm_hfhosp_ntbnp[!is.na(dat[,ntbnp])&dat[,ntbnp]>4000] <- 1   
dat$biostat_acm_hfhosp_ntbnp[!is.na(dat[,ntbnp])&dat[,ntbnp]<=4000] <- 0   


###### ******Combined model for ACM/HFH****** ######

# Ferreira JP, Metra M, Mordi I, Gregson J, ter Maaten, JM, 
# Tromp J, Anker SD, Dickstein K, Hillege HL, Ng LL, 
# van Veldhuisen DJ, Lang CC, Voors AA, Zannad F 
# Heart failure in the outpatient versus inpatient setting: findings from the
# BIOSTAT-CHF study. European Journal of Heart Failure, 2019;21(1):112–120.


dat$biostat_combined_age[!is.na(dat[,age])&dat[,age]>75] <- 1   
dat$biostat_combined_age[!is.na(dat[,age])&dat[,age]<=75] <- 0   

dat$biostat_combined_bun[!is.na(dat[,bun])&dat[,bun]>22.4] <- 1   
dat$biostat_combined_bun[!is.na(dat[,bun])&dat[,bun]<=30.8] <- 0   

dat$biostat_combined_hgb[!is.na(dat[,hgb])&dat[,hgb]<12] <- 1   
dat$biostat_combined_hgb[!is.na(dat[,hgb])&dat[,hgb]>=12] <- 0   

dat$biostat_combined_ntbnp[!is.na(dat[,ntbnp])&dat[,ntbnp] > 7000] <- 3   
dat$biostat_combined_ntbnp[!is.na(dat[,ntbnp])&between(dat[,ntbnp],3000,7000)] <- 2   
dat$biostat_combined_ntbnp[!is.na(dat[,ntbnp])&dat[,ntbnp] <= 3000] <- 0   

dat$biostat_combined_betab[!is.na(dat[,betab])&dat[,betab] %in% N] <- 1   
dat$biostat_combined_betab[!is.na(dat[,betab])&dat[,betab] %in% Y] <- 0   

dat$biostat_combined_sbp[!is.na(dat[,sbp])&dat[,sbp]<=140] <- 1   
dat$biostat_combined_sbp[!is.na(dat[,sbp])&dat[,sbp]>140] <- 0   

dat$biostat_combined_gfr[!is.na(dat[,gfr])&dat[,gfr]<45] <- 1   
dat$biostat_combined_gfr[!is.na(dat[,gfr])&dat[,gfr]>=45] <- 0   

dat$biostat_combined_prior_hfh[!is.na(dat[,prior_hfh])&dat[,prior_hfh] %in% Y] <- 1   
dat$biostat_combined_prior_hfh[!is.na(dat[,prior_hfh])&dat[,prior_hfh] %in% N] <- 0   

dat$biostat_combined_edema[!is.na(dat[,edema])&dat[,edema] %in% Y] <- 1   
dat$biostat_combined_edema[!is.na(dat[,edema])&dat[,edema] %in% N] <- 0   

dat$biostat_combined_hdl[!is.na(dat[,hdl])&dat[,hdl] < 38.67] <- 1   
dat$biostat_combined_hdl[!is.na(dat[,hdl])&dat[,hdl] >= 38.67] <- 0   

dat$biostat_combined_na[!is.na(dat[,na])&dat[,na]<135] <- 1   
dat$biostat_combined_na[!is.na(dat[,na])&dat[,na]>=135] <- 0   







for (i in 1:nrow(dat)) {

dat[i,"biostat_acm_score"] <- sum(dat[i,"biostat_acm_age"],
                             dat[i,"biostat_acm_bun"],
                             dat[i,"biostat_acm_hgb"],
                             dat[i,"biostat_acm_ntbnp"],
                             dat[i,"biostat_acm_betab"],
                             na.rm=T)

dat[i,"biostat_acm_score_nas"] <- sum(is.na(dat[i,"biostat_acm_age"]),
                                 is.na(dat[i,"biostat_acm_bun"]),
                                 is.na(dat[i,"biostat_acm_hgb"]),
                                 is.na(dat[i,"biostat_acm_ntbnp"]),
                                 is.na(dat[i,"biostat_acm_betab"]))





dat[i,"biostat_hfhosp_score"] <- sum(dat[i,"biostat_hfhosp_age"],
                                dat[i,"biostat_hfhosp_sbp"],
                                dat[i,"biostat_hfhosp_gfr"],
                                dat[i,"biostat_hfhosp_prior_hfh"],
                                dat[i,"biostat_hfhosp_edema"],
                                na.rm=T)

dat[i,"biostat_hfhosp_score_nas"] <- sum(is.na(dat[i,"biostat_hfhosp_age"]),
                                is.na(dat[i,"biostat_hfhosp_sbp"]),
                                is.na(dat[i,"biostat_hfhosp_gfr"]),
                                is.na(dat[i,"biostat_hfhosp_prior_hfh"]),
                                is.na(dat[i,"biostat_hfhosp_edema"]))



dat[i,"biostat_acm_hfhosp_score"] <- sum(dat[i,"biostat_acm_hfhosp_age"],
                                    dat[i,"biostat_acm_hfhosp_hdl"],
                                    dat[i,"biostat_acm_hfhosp_na"],
                                    dat[i,"biostat_acm_hfhosp_hgb"],
                                    dat[i,"biostat_acm_hfhosp_sbp"],
                                    dat[i,"biostat_acm_hfhosp_betab"],
                                    dat[i,"biostat_acm_hfhosp_prior_hfh"],
                                    dat[i,"biostat_acm_hfhosp_edema"],
                                    dat[i,"biostat_acm_hfhosp_ntbnp"],
                                    na.rm=T)

dat[i,"biostat_acm_hfhosp_score_nas"] <- sum(is.na(dat[i,"biostat_acm_hfhosp_age"]),
                                        is.na(dat[i,"biostat_acm_hfhosp_hdl"]),
                                        is.na(dat[i,"biostat_acm_hfhosp_na"]),
                                        is.na(dat[i,"biostat_acm_hfhosp_hgb"]),
                                        is.na(dat[i,"biostat_acm_hfhosp_sbp"]),
                                        is.na(dat[i,"biostat_acm_hfhosp_betab"]),
                                        is.na(dat[i,"biostat_acm_hfhosp_prior_hfh"]),
                                        is.na(dat[i,"biostat_acm_hfhosp_edema"]),
                                        is.na(dat[i,"biostat_acm_hfhosp_ntbnp"]))





dat[i,"biostat_combined_score"] <- sum(dat[i,"biostat_combined_age"],   
                                        dat[i,"biostat_combined_bun"],   
                                        dat[i,"biostat_combined_hgb"],   
                                        dat[i,"biostat_combined_ntbnp"],   
                                        dat[i,"biostat_combined_betab"],   
                                        dat[i,"biostat_combined_sbp"],   
                                        dat[i,"biostat_combined_gfr"],   
                                        dat[i,"biostat_combined_prior_hfh"],
                                        dat[i,"biostat_combined_edema"],
                                        dat[i,"biostat_combined_hdl"],
                                        dat[i,"biostat_combined_na"],
                                        na.rm=T)


dat[i,"biostat_combined_score_nas"] <- sum(is.na(dat[i,"biostat_combined_age"]),   
                                       is.na(dat[i,"biostat_combined_bun"]),   
                                       is.na(dat[i,"biostat_combined_hgb"]),   
                                       is.na(dat[i,"biostat_combined_ntbnp"]),   
                                       is.na(dat[i,"biostat_combined_betab"]),   
                                       is.na(dat[i,"biostat_combined_sbp"]),   
                                       is.na(dat[i,"biostat_combined_gfr"]),   
                                       is.na(dat[i,"biostat_combined_prior_hfh"]),
                                       is.na(dat[i,"biostat_combined_edema"]),
                                       is.na(dat[i,"biostat_combined_hdl"]),
                                       is.na(dat[i,"biostat_combined_na"]))



}

dat$biostat_acm_score[dat$biostat_acm_score_nas > max_missing_vals] <- NA
dat$biostat_hfhosp_score[dat$biostat_hfhosp_score_nas > max_missing_vals] <- NA
dat$biostat_acm_hfhosp_score[dat$biostat_acm_hfhosp_score_nas > max_missing_vals] <- NA
dat$biostat_combined_score[dat$biostat_combined_score_nas > max_missing_vals] <- NA

return(dat)

# return(dat[,c("biostat_acm_score",
#               "biostat_hfhosp_score",
#               "biostat_acm_hfhosp_score",
#               "biostat_combined_score")])

}


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###                                                                     ###
####                                 HFSS                              ####
###                                                                     ###
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


hfss_score <- function(dat, 
                       hr="hr_val",
                       lvef="ef_val",
                       sbp="sbp_val",
                       dbp="dbp_val",
                       vo2="vo2", 
                       ischemic_cm="ischemic_cm",
                       na="na",
                       bbb="bbb",
                       pcwp="pcwp",
                       max_missing_vals=2) 
{
  
dat$map <- dat$dbp + (dat$sbp-dat$dbp)/3
  
print(dat$map)

# The  model  derivation  sample  contained  data  collected  from  268 ambulatory
# patients  aged  <70  years  with  LVEF  ≤40%  referred  to HUP for evaluation of 
# severe heart failure and/or cardiac transplant evaluation  between  July  1986  and  
# December  1991.  The  model validation  sample  consisted  of  data  from  199  ambulatory
# patients aged  ≤70  years  with  LVEF  ≤40%  who  were  referred  to  CPMC for cardiac  transplant
# evaluation  between  July  1993  and  July  1995.  

# Aaronson KD, Schwartz JS, Chen TM, Wong KL, Goin JE, Mancini DM. 
# Development and prospective validation of a clinical index to 
# predict survival in ambulatory patients referred for cardiac
# transplant evaluation. Circulation. 1997;95(12):2660–7.

# Noninvasive model:
# 0.0216 * (resting heart rate in beats per minute) + 
# - 0.0464 * (left ventricular ejection fraction in %) + 
# - 0.0255 * (mean blood pressure in millimeters of mercury) + 
# - 0.0546 * (peak consumption of oxygen with maximal exertion in ml/kg/min) + 
# - 0.0470 * (serum sodium in mmol/L)
# + 0.6931 if the patient has ischemic cardiomyopathy
# + 0.6083 is subtracted from the score if the patient has IVCD 

# Score ≥ 8.1 = 89% 1 year survival
# Score 7.2-8.1 = 72% 1 year survival
# Score < 7.2 = 60% 1 year survival

# 1 year mortality in derivation set = 76%
# 2 year mortality in derivation set = 63%

dat$hfss_hr[!is.na(dat[,hr])] <- dat[!is.na(dat[,hr]),hr] * -0.0216

dat$hfss_lvef[!is.na(dat[,lvef])] <- dat[!is.na(dat[,lvef]),lvef] * 0.0464

dat$hfss_map[!is.na(dat[,"map"])] <- dat[!is.na(dat[,"map"]),"map"] * 0.0255

dat$hfss_vo2[!is.na(dat[,vo2])] <- dat[!is.na(dat[,vo2]),vo2] * 0.0546

dat$hfss_na[!is.na(dat[,na])] <- dat[!is.na(dat[,na]),na] * 0.0470

dat$hfss_ischemic_cm[!is.na(dat[,ischemic_cm])&dat[,ischemic_cm] %in% Y] <- -0.6931
dat$hfss_ischemic_cm[!is.na(dat[,ischemic_cm])&dat[,ischemic_cm] %in% N] <- 0

dat$hfss_bbb[!is.na(dat[,bbb])&dat[,bbb] %in% Y] <- -0.6083
dat$hfss_bbb[!is.na(dat[,bbb])&dat[,bbb] %in% N] <- 0


# Invasive model:
# 0.0218 * (resting heart rate in beats per minute) + 
# - 0.0396 * (left ventricular ejection fraction in %) + 
# - 0.0289 * (mean blood pressure in millimeters of mercury) + 
# - 0.0621 * (peak consumption of oxygen with maximal exertion in ml/kg/min) + 
# - 0.0462 * (serum sodium in mmol/L)
# + 0.0285 * (mean PCWP)
# + 0.5654 if the patient has ischemic cardiomyopathy
# + 0.5931 is subtracted from the score if the patient has IVCD 

dat$hfss_invasive_hr[!is.na(dat[,hr])] <- dat[!is.na(dat[,hr]),hr] * -0.0218
dat$hfss_invasive_lvef[!is.na(dat[,lvef])] <- dat[!is.na(dat[,lvef]),lvef] * 0.0396
dat$hfss_invasive_map[!is.na(dat[,"map"])] <- dat[!is.na(dat[,"map"]),"map"] * 0.0289
dat$hfss_invasive_vo2[!is.na(dat[,vo2])] <- dat[!is.na(dat[,vo2]),vo2] * 0.0621
dat$hfss_invasive_na[!is.na(dat[,na])] <- dat[!is.na(dat[,na]),na] * 0.0462
dat$hfss_invasive_pcwp[!is.na(dat[,pcwp])] <- dat[!is.na(dat[,pcwp]),pcwp] * 0.0285

dat$hfss_invasive_ischemic_cm[!is.na(dat[,ischemic_cm])&dat[,ischemic_cm] %in% Y] <- -0.5654
dat$hfss_invasive_ischemic_cm[!is.na(dat[,ischemic_cm])&dat[,ischemic_cm] %in% N] <- 0

dat$hfss_invasive_bbb[!is.na(dat[,bbb])&dat[,bbb] %in% Y] <- -0.5931
dat$hfss_invasive_bbb[!is.na(dat[,bbb])&dat[,bbb] %in% N] <- 0




for (i in 1:nrow(dat)) {
  
  dat[i,"hfss_score"] <- sum(dat[i,"hfss_hr"],
                             dat[i,"hfss_lvef"],
                             dat[i,"hfss_map"],
                             dat[i,"hfss_vo2"],
                             dat[i,"hfss_na"],
                             dat[i,"hfss_ischemic_cm"],
                             dat[i,"hfss_bbb"],
                             na.rm=T)
    
  dat[i,"hfss_score_nas"] <- sum(is.na(dat[i,"hfss_hr"]),
                                 is.na(dat[i,"hfss_lvef"]),
                                 is.na(dat[i,"hfss_map"]),
                                 is.na(dat[i,"hfss_vo2"]),
                                 is.na(dat[i,"hfss_na"]),
                                 is.na(dat[i,"hfss_ischemic_cm"]),
                                 is.na(dat[i,"hfss_bbb"]))
  
  
  dat[i,"hfss_invasive_score"] <- sum(dat[i,"hfss_invasive_hr"],
                                      dat[i,"hfss_invasive_lvef"],
                                      dat[i,"hfss_invasive_map"],
                                      dat[i,"hfss_invasive_vo2"],
                                      dat[i,"hfss_invasive_na"],
                                      dat[i,"hfss_invasive_pcwp"],
                                      dat[i,"hfss_invasive_ischemic_cm"],
                                      dat[i,"hfss_invasive_bbb"],
                                      na.rm=T)
  
  dat[i,"hfss_invasive_score_nas"] <- sum(is.na(dat[i,"hfss_invasive_hr"]),
                                          is.na(dat[i,"hfss_invasive_lvef"]),
                                          is.na(dat[i,"hfss_invasive_map"]),
                                          is.na(dat[i,"hfss_invasive_vo2"]),
                                          is.na(dat[i,"hfss_invasive_na"]),
                                          is.na(dat[i,"hfss_invasive_pcwp"]),
                                          is.na(dat[i,"hfss_invasive_ischemic_cm"]),
                                          is.na(dat[i,"hfss_invasive_bbb"]))
  
}

dat$hfss_score[dat$hfss_score_nas > max_missing_vals] <- NA
dat$hfss_invasive_score[dat$hfss_invasive_score_nas > max_missing_vals] <- NA


dat$hfss_score_cat[dat$hfss_score < 7.2] <- "High risk"
dat$hfss_score_cat[between(dat$hfss_score,7.2,8.09999)] <- "Medium risk"
dat$hfss_score_cat[dat$hfss_score >= 8.1] <- "Low risk"

return(dat)

}



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###                                                                     ###
####                                ADHERE                             ####
###                                                                     ###
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


adhere_score <- function(dat, 
                       age="age_val", 
                       bun="bun_val",
                       sbp="sbp_val",
                       hr="hr_val",
                       cr="creatinine",
                       max_missing_vals=2) 
{
  
# Acute HF risk score

# 0.0212 * (blood urea nitrogen) – 
# 0.0192 * (systolic blood pressure) – 
# 0.0131 * (heart rate) – 
# 0.0288 * (age) – 4.72

  dat$adhere_bun[!is.na(dat[,bun])] <- dat[!is.na(dat[,bun]),bun]* 0.0212
  dat$adhere_sbp[!is.na(dat[,sbp])] <- dat[!is.na(dat[,bun]),sbp]* 0.0192
  dat$adhere_hr[!is.na(dat[,hr])] <- dat[!is.na(dat[,hr]),hr]* 0.0131
  dat$adhere_age[!is.na(dat[,age])] <- dat[!is.na(dat[,age]),age]* 0.0288-4.72
  
  for (i in 1:nrow(dat)) {
    
    dat[i,"adhere_score"] <- sum(dat[i,"adhere_bun"],
                                 dat[i,"adhere_sbp"],
                                 dat[i,"adhere_hr"],
                                 dat[i,"adhere_age"],
                                 na.rm=T)

        dat[i,"adhere_score_nas"] <- sum(is.na(dat[i,"adhere_bun"]),
                                         is.na(dat[i,"adhere_sbp"]),
                                         is.na(dat[i,"adhere_hr"]),
                                         is.na(dat[i,"adhere_age"]))
    
      }
  
  
  dat$adhere_cart_cat[!is.na(dat[,bun])&dat[,bun] < 43&
                        !is.na(dat[,sbp])&dat[,sbp] >= 115] <- "Low risk"
  
  dat$adhere_cart_cat[!is.na(dat[,bun])&dat[,bun] < 43&
                        !is.na(dat[,sbp])&dat[,sbp] < 115] <- "Medium risk"
  
  dat$adhere_cart_cat[!is.na(dat[,bun])&dat[,bun] >= 43&
                        !is.na(dat[,sbp])&dat[,sbp] >= 115] <- "Medium risk"
  
  dat$adhere_cart_cat[!is.na(dat[,bun])&dat[,bun] >= 43&
                        !is.na(dat[,sbp])&dat[,sbp] < 115&
                        !is.na(dat[,cr])&dat[,cr] < 2.75] <- "Medium risk"
  
  dat$adhere_cart_cat[!is.na(dat[,bun])&dat[,bun] >= 43&
                        !is.na(dat[,sbp])&dat[,sbp] < 115&
                        !is.na(dat[,cr])&dat[,cr] >= 2.75] <- "High risk"
  
  return(dat[,c("adhere_score","adhere_cart_cat")])
  
}


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###                                                                     ###
####                                 GWTG                              ####
###                                                                     ###
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


gwtg_ahf_score <- function(dat,
                           id="patientid",
                           sbp="sbp_val",
                           bun="bun_val",
                           na="na",
                           age="age_val",
                           hr="hr_val",
                           race="race",
                           black=c(2,"2. Black"),
                           copd="copd",
                           Y=c(2,"Yes"),
                           max_missing_vals=0)
{
  
  # Acute HF score
  
  # Peterson PN, Rumsfeld JS, Liang L, Albert NM,
  # Hernandez AF, Peterson ED, Fonarow GC, Masoudi FA, 
  # The Get With the Guidelines-Heart Failure Program. (2010). A validated risk
  # score for in-hospital mortality in patients with heart failure from the
  # American Heart Association get with the guidelines program. Circulation: 
  # Cardiovascular Quality and Outcomes, 2010;3(1):25–32.
  
  dat$gwtg_sbp_score[between(dat[,sbp],50,59)] <- 28
  dat$gwtg_sbp_score[between(dat[,sbp],60,69)] <- 26
  dat$gwtg_sbp_score[between(dat[,sbp],70,79)] <- 24
  dat$gwtg_sbp_score[between(dat[,sbp],80,89)] <- 23
  dat$gwtg_sbp_score[between(dat[,sbp],90,99)] <- 21
  dat$gwtg_sbp_score[between(dat[,sbp],100,109)] <- 19
  dat$gwtg_sbp_score[between(dat[,sbp],110,119)] <- 17
  dat$gwtg_sbp_score[between(dat[,sbp],120,129)] <- 15
  dat$gwtg_sbp_score[between(dat[,sbp],130,139)] <- 13
  dat$gwtg_sbp_score[between(dat[,sbp],140,149)] <- 11
  dat$gwtg_sbp_score[between(dat[,sbp],150,159)] <- 9
  dat$gwtg_sbp_score[between(dat[,sbp],160,169)] <- 8
  dat$gwtg_sbp_score[between(dat[,sbp],170,179)] <- 6
  dat$gwtg_sbp_score[between(dat[,sbp],180,189)] <- 4
  dat$gwtg_sbp_score[between(dat[,sbp],190,199)] <- 2
  dat$gwtg_sbp_score[between(dat[,sbp],200,999)] <- 0
  
  dat$gwtg_bun_score[between(dat[,bun],0,9)] <- 0  
  dat$gwtg_bun_score[between(dat[,bun],10,19)] <- 2  
  dat$gwtg_bun_score[between(dat[,bun],20,29)] <- 4  
  dat$gwtg_bun_score[between(dat[,bun],30,39)] <- 6  
  dat$gwtg_bun_score[between(dat[,bun],40,49)] <- 8  
  dat$gwtg_bun_score[between(dat[,bun],50,59)] <- 9  
  dat$gwtg_bun_score[between(dat[,bun],60,69)] <- 11  
  dat$gwtg_bun_score[between(dat[,bun],70,79)] <- 13  
  dat$gwtg_bun_score[between(dat[,bun],80,89)] <- 15  
  dat$gwtg_bun_score[between(dat[,bun],90,99)] <- 17  
  dat$gwtg_bun_score[between(dat[,bun],100,109)] <- 19  
  dat$gwtg_bun_score[between(dat[,bun],110,119)] <- 21  
  dat$gwtg_bun_score[between(dat[,bun],120,129)] <- 23  
  dat$gwtg_bun_score[between(dat[,bun],130,139)] <- 25  
  dat$gwtg_bun_score[between(dat[,bun],140,149)] <- 27  
  dat$gwtg_bun_score[between(dat[,bun],150,999)] <- 28  
  
  dat$gwtg_na_score[between(dat[,na],0,130)] <- 4
  dat$gwtg_na_score[between(dat[,na],131,133)] <- 3
  dat$gwtg_na_score[between(dat[,na],134,136)] <- 2
  dat$gwtg_na_score[between(dat[,na],137,138)] <- 1
  dat$gwtg_na_score[between(dat[,na],139,999)] <- 0
  
  dat$gwtg_age_score[between(dat[,age],0,19)] <- 0
  dat$gwtg_age_score[between(dat[,age],20,29)] <- 3
  dat$gwtg_age_score[between(dat[,age],30,39)] <- 6
  dat$gwtg_age_score[between(dat[,age],40,49)] <- 8
  dat$gwtg_age_score[between(dat[,age],50,59)] <- 11
  dat$gwtg_age_score[between(dat[,age],60,69)] <- 14
  dat$gwtg_age_score[between(dat[,age],70,79)] <- 17
  dat$gwtg_age_score[between(dat[,age],80,89)] <- 19
  dat$gwtg_age_score[between(dat[,age],90,99)] <- 22
  dat$gwtg_age_score[between(dat[,age],100,109)] <- 25
  dat$gwtg_age_score[between(dat[,age],110,999)] <- 28
  
  dat$gwtg_hr_score[between(dat[,hr],0,79)] <- 0
  dat$gwtg_hr_score[between(dat[,hr],80,84)] <- 1
  dat$gwtg_hr_score[between(dat[,hr],85,89)] <- 3
  dat$gwtg_hr_score[between(dat[,hr],90,94)] <- 4
  dat$gwtg_hr_score[between(dat[,hr],95,99)] <- 5
  dat$gwtg_hr_score[between(dat[,hr],100,104)] <- 6
  dat$gwtg_hr_score[between(dat[,hr],105,999)] <- 8

  dat$gwtg_race_score[!is.na(dat[,race])&!dat[,race] %in% black] <- 3
  dat$gwtg_race_score[!is.na(dat[,race])&dat[,race] %in% black] <- 0
  
  dat$gwtg_copd_score[!is.na(dat[,copd])&dat[,copd] %in% Y] <- 2
  dat$gwtg_copd_score[!is.na(dat[,copd])&!dat[,copd] %in% Y] <- 0
  
  dat$gwtg_score <- dat$gwtg_sbp_score+
                    dat$gwtg_bun_score+
                    dat$gwtg_na_score+
                    dat$gwtg_age_score+
                    dat$gwtg_hr_score+
                    dat$gwtg_race_score+
                    dat$gwtg_copd_score
  
  dat$gwtg_score_nas <- is.na(dat[,sbp])+
                        is.na(dat[,bun])+
                        is.na(dat[,na])+
                        is.na(dat[,age])+
                        is.na(dat[,hr])+
                        is.na(dat[,race])+
                        is.na(dat[,copd])
  
  dat$gwtg_score[dat$gwtg_score_nas > max_missing_vals] <- NA
  
  return(dat[,c("patientid","gwtg_score","gwtg_score_nas")])
  
}




##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###                                                                     ###
####                                AHEAD                              ####
###                                                                     ###
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ahead_score <- function(dat,
                        id="patientid",
                        af="af",
                        hgb="hgb",
                        age="age",
                        cr="creatinine",
                        dm="dm",
                        Y=c(2,"Yes"),
                        N=c(1,"No"))
{
  
ahead_mortality <- data.frame(ahead_score=c(0, 1, 2, 3, 4, 5),
                              ahead_mortality_12mo=c(0.864, 0.766, 0.680, 0.589, 0.523, 0.418),
                              ahead_mortality_36mo=c(0.785, 0.639, 0.512, 0.398, 0.298, 0.190),
                              ahead_mortality_60mo=c(0.732, 0.527, 0.381, 0.253, 0.185, 0.083),
                              ahead_mortality_90mo=c(0.649, 0.427, 0.265, 0.152, 0.120, 0.083))
  
# Spinar J, Jarkovsky J, Spinarova L, Mebazaa A,
# Gayat E, Vitovec J, Linhart A, Widimsky P, Miklik R, Zeman K,
# Belohlavek J, Malek F, Felsoci M, Kettner J, Ostadal P, Cihalik C,
# Vaclavik J, Taborsky M, Dusek L, Parenica, J. AHEAD score -
# Long-term risk classification in acute heart failure. International Journal
# of Cardiology. 2016;202:21–26.

# 1 point for each:
# A - atrial fibrillation
# H - haemoglobin < 13.0 g/dl for men and 12.0 g/dl for women (anaemia)
# E - elderly (age > 70 years), 
# A - abnormal renal parameters (creatinine > 1.47 mg/dL)
# D - diabetes mellitus

# Score   Month 12    Month 36    Month 60    Month 90
# 0       86.4%       78.5%       73.2%       64.9%
# 1       76.6%       63.9%       52.7%       42.7%
# 2       68.0%       51.2%       38.1%       26.5%
# 3       58.9%       39.8%       25.3%       15.2%
# 4       52.3%       29.8%       18.5%       12.0%
# 5       41.8%       19.0%       8.3%        8.3%

  dat$ahead_af[is.na(dat[,af])&dat[,af] %in% Y] <- 1
  dat$ahead_af[is.na(dat[,af])&dat[,af] %in% N] <- 0
  
  dat$ahead_hgb[is.na(dat[,hgb])&dat[,hgb] %in% Y] <- 1
  dat$ahead_hgb[is.na(dat[,hgb])&dat[,hgb] %in% N] <- 0

  dat$ahead_age[is.na(dat[,age])&dat[,age] > 70] <- 1
  dat$ahead_age[is.na(dat[,age])&dat[,age] <= 70] <- 0
  
  dat$ahead_cr[is.na(dat[,cr])&dat[,cr] > 1.47] <- 1
  dat$ahead_cr[is.na(dat[,cr])&dat[,cr] <= 1.47] <- 0

  dat$ahead_dm[is.na(dat[,dm])&dat[,dm] %in% Y] <- 1
  dat$ahead_dm[is.na(dat[,dm])&dat[,dm] %in% N] <- 0
  
  dat$ahead_score <- sum(ahead_af,
                       ahead_hgb,
                       ahead_age,
                       ahead_rf,
                       ahead_dm,
                       na.rm=T)
  
  dat <- merge(dat,ahead_score,by=("ahead_score"),all.x=T)

}

###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###                                                                      %%%
####                           EMPEROR-REDUCED                          ####
###                                                                      %%%
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

emperor_reduced_score <- function(dat,
                           id="patientid",
                           sbp="sbp_val",
                           bun="bun_val",
                           na="na",
                           age="age_val",
                           hr="hr_val",
                           race="race",
                           black=c(2,"2. Black"),
                           copd="copd",
                           Y=c(2,"Yes"))
{
  

# 1 - 
# 0.9977^
# exp (0.49 * log NT-proBNP +
# 0.41 * log hs-cTnT +
# 0.44 * recent HHF1 +
# 0.61 * recent HHF2 +
# 0.38 * time since diagnosis1 +
# 0.58 * time since dignosis2 -
# 0.12 * SBP/10 +
# 0.33 * NYHA +
# 0.28 * peripheral oedema +
# 0.10 * heart rate/10 + 
# -0.29 * empagliflozin)
#
# ‘recent HHF1’ and ‘recent HHF2’ indicate whether most recent HHF was within 3–6 or <3 months
# ‘time since diagnosis1’ and ‘time since diagnosis2’ = most recent HF diagnosis was 1-5 years or ≥ 5 years 
# NYHA is an indicator variable for whether the patient’s NYHA class is III or IV.

}






###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###                                                                     %%%
####                    ESCAPE AHF RISK SCORE                          ####
###                                                                     %%%
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


escape_score <- function(dat,
                         id = "patientid",
                         bun = "bun_val",
                         na = "na",
                         sixmw = "sixmw_dist_m",
                         age = "age_val",
                         cpr = "cpr",
                         diuretic_dose = "diuretic_dose",
                         betab = "betab",
                         bnp = "bnp_val",
                         Y = c(2, "Yes"))
  

{
 
#  O’Connor CM, Hasselblad V, Mehta RH, Tasissa, G, Califf RM, Fiuzat M, 
#  Rogers JG, Leier CV  Stevenson LW Triage After Hospitalization with Advanced Heart
#  Failure: The ESCAPE Risk Model and Discharge Score Journal of the
#  American College of Cardiology 2010;55(9):872–878

  
  #  Estimates 6 month mortality using discharge values
  
  #  Criteria (at discharge)	
  #  Age > 70	= 1
  #  BUN > 40	= 1
  #  BUN > 90*	= 1 (total for BUN > 90 = 2)
  #  6-minute walk < 300 feet	= 1
  #  Sodium <130 mEq/L = 1
  #  CPR/mechanical ventilation (yes/no) = 2
  #  Diuretic dose >240 at discharge (yes/no) = 1
  #  No beta-blocker at discharge = 1
  #  Discharge BNP >500 = 1
  #  Discharge BNP >1300 = 3   
  
  #  SCORE	Observed mortality	Est. prob. of dying (no BNP)	Est. prob. of dying (with BNP)
  #  0	    0.077	              0.053	                        0.033
  #  1	    0.104	              0.103	                        0.065
  #  2	    0.167	              0.189	                        0.123
  #  3	    0.264	              0.322	                        0.223
  #  4	    0.448	              0.492	                        0.368
  #  5	    0.800	              0.664 	                      0.543
  #  6	    0.750	              0.801	                        0.708
  #  7	    1.000	              0.891	                        0.831
  #  >/=8		1.000	              0.943	                        0.909
  
  escape_mortality <- data.frame(escape_score=c(0,1,2,3,4,5,6,7,8,9,10,11,12),
                                 escape_mortality_6mo =c(0.077,0.104,0.167,0.264,0.448,0.800,0.750,1,1,1,1,1,1))
  
  
  dat$escape_score_age[!is.na(dat[,age])&dat[,age] > 70] <- 1
  dat$escape_score_age[!is.na(dat[,age])&dat[,age] <= 70] <- 0
  
  dat$escape_score_bun[!is.na(dat[,bun])&dat[,bun]>90] <- 2
  dat$escape_score_bun[!is.na(dat[,bun])&between(dat[,bun],40,90)] <- 1
  dat$escape_score_bun[!is.na(dat[,bun])&dat[,bun] < 40] <- 0
  
  dat$escape_score_6mw[!is.na(dat[,sixmw])&dat[,sixmw] < 300] <- 1
  dat$escape_score_6mw[!is.na(dat[,sixmw])&dat[,sixmw] >= 300] <- 0
  
  dat$escape_score_na[!is.na(dat[,na])&dat[,na] < 130] <- 1
  dat$escape_score_na[!is.na(dat[,na])&dat[,na] >= 130] <- 0
  
  dat$escape_score_cpr[!is.na(dat[,cpr])&dat[,cpr] %in% Y] <- 2
  dat$escape_score_cpr[!is.na(dat[,cpr])&dat[,cpr] %in% N] <- 0
  
  dat$escape_score_diuretic_dose[!is.na(dat[,diuretic_dose])&dat[,diuretic_dose] >= 240] <- 1
  dat$escape_score_diuretic_dose[!is.na(dat[,diuretic_dose])&dat[,diuretic_dose] < 240] <- 0
  
  dat$escape_score_betab[!is.na(dat[,betab])&dat[,betab] %in% N] <- 1
  dat$escape_score_betab[!is.na(dat[,betab])&dat[,betab] %in% Y] <- 0

  dat$escape_score_bnp[!is.na(dat[,bnp])&dat[,bnp]>1300] <- 3
  dat$escape_score_bnp[!is.na(dat[,bnp])&between(dat[,bnp],500,1300)] <- 1
  dat$escape_score_bnp[!is.na(dat[,bnp])&dat[,bnp] < 500] <- 0
  
  dat$escape_score <- sum(dat$escape_score_age,
                          dat$escape_score_bun,
                          dat$escape_score_6mw,
                          dat$escape_score_na,
                          dat$escape_score_cpr,
                          dat$escape_score_diuretic_dose,
                          dat$escape_score_betab,
                          dat$escape_score_bnp,
                          na.rm=T)
  
  dat$escape_score_nas <- sum(is.na(dat$escape_score_age),
                              is.na(dat$escape_score_bun),
                              is.na(dat$escape_score_6mw),
                              is.na(dat$escape_score_na),
                              is.na(dat$escape_score_cpr),
                              is.na(dat$escape_score_diuretic_dose),
                              is.na(dat$escape_score_betab),
                              is.na(dat$escape_score_bnp))
  
  dat <- merge(dat,escape_mortality,by=c("escape_score"))
  
  return(dat[,c("escape_score",
                "escape_mortality_6mo",
                "escape_score_nas")])

  }




###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###                                                                      %%%
####     EMERGENCY HEART FAILURE MORTALITY RISK GRADE (EHMRG)           ####
###                                                                      %%%
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


ehmrg_score <- function(dat,
                        age = "age_val",
                        ems = "ems",
                        sbp = "sbp_val",
                        hr="hr",
                        o2 = "o2_sat_val",
                        cr = "creatinine",
                        potassium = "potassium",
                        tn = "tn",
                        cancer = "cancer",
                        metolazone = "metolazone",
                        Y = c(2, "Yes"),
                        N = c(1, "No"))

{
  
# Lee, DS, Stitt A, Austin PC, Stukel TA, Schull MJ, Chong A, Newton GE, Lee JS,
# Tu JV. Prediction of Heart Failure Mortality in Emergent Care. Annals of
# Internal Medicine. 2012; 156(11):767

# 12,500 patients from 86 Canadian hospitals who visited ED for HF
# Not appropriate for dialysis-dependent position
  
# Age                   years             2 * age
# Transported by EMS    If "Y"            +60
# SBP                   mm Hg (1st val)   -SBP
# Heart rate            bpm (1st val)     + heart rate (val) [80-120]
# Oxy. sat.             % (lowest)        -2 * oxygen saturation (%) [< 92%]
# Creatinine            mg/dL             20 * creatinine
# Potassium             4.0 to 4.5 mEq/L `0
#                       >4.5 mmol/L       +30
#                       <3.9 mmol/L       +5
# Troponin              >ULN              +60
# Active cancer         If "yes"          +45
# Metolazone at home    If "yes"         +60
# Adjustment factor                       +12  To allow for median 0 score (?)
  
  
  dat$ehmrg_age[!is.na(dat[,age])] <- dat[!is.na(dat[,age]),age]*2
  
  dat$ehmrg_ems[!is.na(dat[,ems])&dat[,ems] %in% Y] <- 60
  dat$ehmrg_ems[!is.na(dat[,ems])&dat[,ems] %in% N] <- 0
  
  dat$ehmrg_sbp[!is.na(dat[,sbp])&dat[,sbp]<160] <- 0-dat[!is.na(dat[,sbp])&dat[,sbp]<160,sbp]
  dat$ehmrg_sbp[!is.na(dat[,sbp])&dat[,sbp] >= 160] <- -320
  
  dat$ehmrg_lowo2[!is.na(dat[,o2])&dat[,o2] < 92] <- -2 * dat[!is.na(dat[,o2])&dat[,o2] < 92,o2] 
  dat$ehmrg_lowo2[!is.na(dat[,o2])&dat[,o2] >= 92] <- -184
  
  dat$ehmrg_cr[!is.na(dat[,cr])] <- 20* dat[!is.na(dat[,cr]),cr]  

  dat$ehmrg_hr[!is.na(dat[,hr])&dat[,hr]>=80] <- dat[!is.na(dat[,hr])&dat[,hr]>=80,hr]  
  dat$ehmrg_hr[!is.na(dat[,hr])&dat[,hr]<80] <- 80
  
  
  dat$ehmrg_potassium[!is.na(dat[,potassium])&dat[,potassium]>4.5] <- 30
  dat$ehmrg_potassium[!is.na(dat[,potassium])&between(dat[,potassium],4,4.5)] <- 0
  dat$ehmrg_potassium[!is.na(dat[,potassium])&dat[,potassium] < 3.9] <- 5
  
  dat$ehmrg_trop[!is.na(dat[,tn])&dat[,tn] %in% Y] <- 60
  dat$ehmrg_trop[!is.na(dat[,tn])&dat[,tn] %in% N] <- 0
  
  dat$ehmrg_cancer[!is.na(dat$cancer)&dat$cancer %in% Y] <- 45
  dat$ehmrg_cancer[!is.na(dat$cancer)&dat$cancer %in% N] <-  0
  
  dat$ehmrg_metolazone[!is.na(dat[,metolazone])&dat[,metolazone] %in% Y] <- 60
  dat$ehmrg_metolazone[!is.na(dat[,metolazone])&dat[,metolazone] %in% N] <- 0
  
  for (i in 1:nrow(dat)) {
  
  dat[i,"ehmrg_score"] <- sum(dat[i,"ehmrg_age"],
                         dat[i,"ehmrg_ems"],
                         dat[i,"ehmrg_sbp"],
                         dat[i,"ehmrg_hr"],
                         dat[i,"ehmrg_lowo2"],
                         dat[i,"ehmrg_cr"],
                         dat[i,"ehmrg_potassium"],
                         dat[i,"ehmrg_trop"],
                         dat[i,"ehmrg_cancer"],
                         dat[i,"ehmrg_metolazone"],
                         12,
                         na.rm=T)

  dat[i,"ehmrg_score_nas"] <- sum(is.na(dat[i,"ehmrg_age"]),
                             is.na(dat[i,"ehmrg_ems"]),
                             is.na(dat[i,"ehmrg_sbp"]),
                             is.na(dat[i,"ehmrg_lowo2"]),
                             is.na(dat[i,"ehmrg_hr"]),
                             is.na(dat[i,"ehmrg_cr"]),
                             is.na(dat[i,"ehmrg_potassium"]),
                             is.na(dat[i,"ehmrg_trop"]),
                             is.na(dat[i,"ehmrg_cancer"]),
                             is.na(dat[i,"ehmrg_metolazone"]))
  
  
  # Risk categories - 7 day mortality:
  # Quintile 1 (≤ -49.1): 0.3 %
  # Quintile 2 (-49.0 to -15.9): 0.3 %
  # Quintile 3 (-15.8 to 17.9): 0.7 %
  # Quintile 4 (18.0 to 56.6): 1.9 %
  # Quintile 5a (56.6 to 89.3): 3.5 %
  # Quintile 5b (≥ 89.4): 8.5 %
  
  dat$ehmrg_mortality_7d[dat$ehmrg_score < -15.9] <- 0.003
  dat$ehmrg_mortality_7d[between(dat$ehmrg_score,-15.8,17.9)] <- 0.007
  dat$ehmrg_mortality_7d[between(dat$ehmrg_score,-18.0,56.5)] <- 0.019
  dat$ehmrg_mortality_7d[between(dat$ehmrg_score, 56.6,89.3)] <- 0.035
  dat$ehmrg_mortality_7d[dat$ehmrg_score >= 89.4] <- 0.085
  
  }
  
  return(dat[,c("ehmrg_score","ehmrg_score_nas","ehmrg_mortality_7d")])
  
}



## Skipping Bathel Index and MEESSI-AHF due to unique clinical variables.
## Leaving specifications for later.

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##                        Barthel Index                        %%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

barthel_index <- function(dat,
                          bowel='barthel_bowels',
                          bl)

# Bowels
# 0 = Incontinent (or needs to be given enema)
# 1 = occasional accident (once/week)
# 2 = continent

# Bladder:
# 0 = incontinent, or catheterized and unable to manage
# 1 = occasional accident (max. once per 24 hours)
# 2 = continent (for over 7 days)

# Grooming
# 0 = needs help with personal care
# 1 = independent face/hair/teeth/shaving (implements provided)

# Toilet use
# 0 = dependent
# 1 = needs some help, but can do something alone
# 2 = independent (on and off, dressing, wiping)

# Feeding
# 0 = unable
# 1 = needs help cutting, spreading butter, etc.
# 2 = independent (food provided within reach)

# Transfer
# 0 = unable – no sitting balance
# 1 = major help (one or two people, physical), can sit
# 2 = minor help (verbal or physical)
# 3 = independent

# Mobility
# 0 = immobile
# 1 = wheelchair independent, including corners, etc.
# 2 = walks with help of one person (verbal or physical)
# 3 = independent (but may use any aid)

# Dressing
# 0 = dependent
# 1 = needs help, but can do about half unaided
# 2 = independent (including buttons, zips, laces etc)

# Stairs
# 0 = unable
# 1 = needs help (verbal, physical, carrying aid)
# 2 = independent up and down

# Bathing
# 0 = dependent
# 1 = independent (or in shower)





###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###                                                                                                      %%%
####  MULTIPLE ESTIMATION OF RISK BASED ON THE EMERGENCY DEPARTMENT SPANISH SCORE IN PATIENTS WITH AHF  ####
###                                            (MEESSI-AHF)                                              %%%
###                                                                                                      %%%
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

meessi_score <- function(dat,
                        id = "patientid",
                        age = "age_val",
                        barthel = "barthel",
                        sbp = "sbp_val",
                        o2 = "o2_sat_val",
                        cr = "creatinine",
                        potassium = "potassium",
                        tn = "tn",
                        nyha = "nyha",
                        rr = "resp_rate",
                        ams = "ams",
                        weak = "weakness",
                        cool = "cool",
                        uo = "urine_output",
                        perf = "perfusion",
                        Y = c(2, "Yes"))

{
  
  

    
return(dat)

# Barthel index score at admission (HR)
# Score                 HR                    Coefficient
    # ≥75               1.00 (reference)      0
    # 50–74             1.52 (1.07–2.16)      0.4187103
    # 25–49             2.34 (1.61–3.38)      0.8501509      
    # <25               3.99 (2.69–5.92)      1.383791

# Systolic blood pressure (mm Hg)
    # ≥155              1.00 (reference)      0
    # 140–154           1.52 (1.08–2.15)      0.4187103
    # 125–139           2.06 (1.48–2.86)      0.722706
    # 110–124           2.56 (1.85–3.56)      0.9400073
    # 95–109            2.52 (1.67–3.78)      0.9242589
    # <95               3.03 (1.82–5.06)      1.108563

# Age (yr)
    # <75               1.00 (reference)      0
    # 75–79             1.59 (1.08–2.33)      0.463734
    # 80–84             1.74 (1.22–2.49)      0.5538851
    # 85–89             1.72 (1.21–2.45)      0.5423243
    # ≥90               2.62 (1.79–3.83)      0.9631743

# NT-proBNP level (ng/L)
    # <8000             1.00 (reference)      0
    # 8000–15,999       1.64 (1.08–2.49)      0.4946962
    # 16,000–23,999     2.04 (1.25–3.34)      0.7129498
    # ≥24,000 ng/L      2.59 (1.68–3.99)      0.9516579

# Potassium level (mmol/L)
    # <3.5              1.48 (0.95–2.30)      0.3920421
    # 3.5–4.9           1.00 (reference)      0
    # 5.0–5.5           1.35 (0.98–1.87)      0.3001046
    # >5.5              2.09 (1.48–2.94)      0.7371641

# Elevated troponin     1.75 (1.32–2.30)      0.5596158

# NYHA class IV         1.63 (1.28–2.09)      0.48858

# Respiratory rate (breaths/min)
    # <25               1.00 (reference)      0
    # 25–29             1.35 (0.96–1.88)      0.3001046
    # ≥30               1.69 (1.23–2.32)      0.5247285

# Low-output sx         1.48 (1.15–1.90)      0.3920421

# Oxygen saturation (%)
    # 95–100            1.00 (reference)      0
    # 90–94             1.19 (0.90–1.56)      0.1739533
    # 85–89             1.34 (0.97–1.86)      0.2926696
    # <85               1.67 (1.18–2.36)      0.5128236

# Acute Cor Synd        2.02 (1.25–3.27)      0.7030975

# LVH on ECG            1.59 (1.05–2.40)      0.463734

# Creatinine level
    # <1.5 mg/dL        1.00 (reference)      0
    # 1.5–2.4 mg/dL     1.27 (0.99–1.64)      0.2390169
    # >2.4 mg/dL        1.46 (1.00–2.13)      0.3784364

}




