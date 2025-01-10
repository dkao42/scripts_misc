
hrr <- function(dat, age_field='age_val', resthr_field='hr_bl') {
  ## Calculates heart rate reserve using patient age and resting heart rate
  dat$max_hr <- 220-dat[,age_field]
  dat$hr_reserve <- dat[,'max_hr'] - dat[,resthr_field]
  return(dat[,c('hr_reserve')])
}

tors_to_furo <- 2
bum_to_furo <- 40
ethacrynic_to_furo <- 0.8

diur_calc <- function (x) {
  a <- x[1]
  b <- x[2] * tors_to_furo
  c <- x[3] * bum_to_furo
  d <- x[4] * ethacrynic_to_furo
  tot <- sum(a,b,c,d,na.rm=T)  
  return(tot)
}


calc_map <- function (diabp, sysbp, hr=NULL) 
{
  if (is.null(hr)) {
    map <- round((2*diabp+sysbp)/3,0)
  }
  
  if (!is.null(hr)) {
  map <- round(diabp + 0.01 * exp(4.14 - 40.74/hr) * (sysbp - diabp),0)
  }
  return(map)
}

score_mlhf <- function(dat,swelling = "mlhfq_swell", 
                         day_rest="mlhf_rest",
                         stairs = "mlhf_walk", 
                         housework="mlhf_housework",
                         go_places="mlhf_goplaces",
                         sleep = "mlhf_sleep",
                         friendfam = "mlhf_friendfam",
                         earn_living = "mlhf_earn_living",
                         hobbies = "mlhf_hobbies",
                         sex = "mlhf_sex",
                         food = "mlhf_food",
                         sob = "mlhf_sob",
                         fatigue = "mlhf_fatigue",
                         hospital = "mlhf_hospital",
                         cost = "mlhf_money",
                         sideeffect = "mlhf_side_effects",
                         burden = "mlhf_burden",
                         nocontrol = "mlhf_nocontrol",
                         worry="mlhf_worry",
                         concentrate="mlhf_concentrate",
                         depressed="mlhf_depressed", 
                         total=c("N")) {
  
  ## Takes data frame with inidivual responses to MLWHF questions and returns total score and physical + emotional subscores
  for (i in 1:nrow(dat)){

    dat[i,'mlhf_physical'] <- dat[i,friendfam]+
    dat[i,day_rest]+
    dat[i,sob]+
    dat[i,go_places]+
    dat[i,sleep]+
    dat[i,fatigue]+
    dat[i,stairs]+
    dat[i,housework]
  
  dat[i,'mlhf_emotional'] <- dat[i,burden]+
    dat[i,nocontrol]+
    dat[i,worry]+
    dat[i,depressed]+
    dat[i,concentrate]
  
  if (total=="Y"){
    dat[i,'mlhf_total'] <- 
      dat[i,swelling]+
      dat[i,day_rest]+
      dat[i,stairs]+
      dat[i,housework]+
      dat[i,go_places]+
      dat[i,sleep]+
      dat[i,friendfam]+
      dat[i,earn_living]+
      dat[i,hobbies]+
      dat[i,sex]+
      dat[i,food]+
      dat[i,sob]+
      dat[i,fatigue]+
      dat[i,hospital]+
      dat[i,cost]+
      dat[i,sideeffect]+
      dat[i,burden]+
      dat[i,nocontrol]+
      dat[i,worry]+
      dat[i,concentrate]+
      dat[i,depressed]
  }
  }
  return(dat)
}

convert_kccq_to_mlwhf <- function(dat, kccq_var) {

## Takes data frame and converts overall KCCQ summary score to MLWHF total score based on linear relationship between two from NEAT-HFpEF
## Formula = -0.853*kccq_overall_summary + 93.85
  dat[,'mlwhf_total'] <- round(-0.94025*dat[,kccq_var]+95.75665,0)
  return(dat[,'mlwhf_total'])
}

convert_mlwhf_to_kccq <- function(dat, mlwhf_var) {
  
  ## Takes data frame and converts overall MLWHF summary score to KCCQ total score based on linear relationship between two from NEAT-HFpEF
  ## Formula = -0.853*kccq_overall_summary + 93.85
  dat[,'kccq_overallsumm'] <- round(-0.94025*dat[,mlwhf_var]+95.75665,0)
  return(dat[,'kccq_overallsumm'])
}

score_kccq <- function(dat,q1a="kccq_q1a",q1b="kccq_q1b",q1c="kccq_q1c",q1d="kccq_q1d",q1e="kccq_q1e",q1f="kccq_q1f",q2="kccq_q2",q3="kccq_q3",q4="kccq_q4",q5="kccq_q5",q6="kccq_q6",q7="kccq_q7",
                       q8="kccq_q8",q9="kccq_q9",q10="kccq_q10",q11="kccq_q11",q12="kccq_q12",q13="kccq_q13",q14="kccq_q14",q15a="kccq_q15a",q15b="kccq_q15b",q15c="kccq_q15c",q15d="kccq_q15d",
                       scores=c("overall_summ","clinical_summ","social_lim","qol","self_eff","total_sym","sym_burden","sym_freq","sym_stab","phys_lim")) {

  ## Physical limitation score
  
    # The Physical Limitation score corresponds to questions 1a through 1f. Responses to questions 1a through 1f should be coded numerically as follows:
    # 
    # 1 = Extremely Limited 
    # 2 = Quite a bit Limited 
    # 3 = Moderately Limited 
    # 4 = Slightly Limited
    # 5 = Not at all Limited
    # 6 = Limited for other reasons or did not do the activity
    # 
    #   If the responses to questions 1a through 1f are not values 1, 2, 3, 4 or 5 then the response is set to missing. 
    #   Note that a response of 6 (Limited for other reasons or did not do the activity) is treated as a missing value. 
    #   If at least three responses to questions 1a-1f are not missing, then the physical limitation score is computed by calculating the mean response and standardizing the result as follows:
    #   
    #     Physical Limitation = 100*(Mean Response – 1)/4

  phys_lim_questions <- c(q1a,q1b,q1c,q1d,q1e,q1f)
  sym_freq_questions <- c(q3,q5,q7,q9)
  for (i in 1:nrow(dat)) {
  phys_lim_present <- 0
  phys_lim_interm <- 0
    for (j in 1:length(phys_lim_questions)) {
      thisq <- phys_lim_questions[j]
      if (!dat[i,thisq] %in% c(1,2,3,4,5)) {
        dat[i,thisq] <- NA
      } else if (dat[i,thisq] %in% c(1,2,3,4,5)) {
        phys_lim_present <- phys_lim_present + 1
        phys_lim_interm <- phys_lim_interm + dat[i,thisq]
      }
      
    }
  
  if (phys_lim_present >= 3) {
    mean_response <- phys_lim_interm/phys_lim_present
    dat[i,'kccq_phys_lim'] <- 100*(mean_response-1)/4
  }
  

  ## Symptom stability score
  
    # The Symptom Stability score corresponds to question 2. Responses to question 2 should be coded numerically as follows:
    #   
    #   1 = Much Worse
    #   2 = Slightly Worse 
    #   3 = Not  Changed 
    #   4 = Slightly Better 
    #   5 = Much Better
    #   6 = I’ve had no symptoms over the last 2 weeks
    #   
    #   If the response is 6 (no symptoms over last 2 weeks) then set the response to 3 (not changed). 
    #   If question 2 is not missing then the symptom stability score is computed by standardizing the result as follows:
    #     
    #     Symptom Stability = 100*(Response – 1)/4
  
  if (!is.na(dat[i,q2])) {  
  this_q2 <- dat[i,q2]
  if (this_q2==6) {this_q2 <- 3}
  dat[i,'kccq_symp_stab'] <- 100*(this_q2-1)/4
  }

  
  ## Symptom frequency score
  
    # The Symptom Frequency score corresponds to questions 3, 5, 7 and 9. The responses should be coded sequentially (1, 2, 3…) in order of increasing health status as follows:
    #   
    # Question 3
    #   1 = Every Morning
    #   2 = 3 or more times per week, but not every day 
    #   3 = 1-2 times a week
    #   4 = Less than once a week
    #   5 = Never over the past 2 weeks
    # 
    # Questions 5 and 7 
    #   1 = All of the time
    #   2 = Several times per day 
    #   3 = At least once a day
    #   4 = 3 or more times per week, but not every day 
    #   5 = 1-2 times per week
    #   6 = Less than once a week
    #   7 = Never over the past 2 weeks
    # 
    # Question 9
    #   1 = Every night
    #   2 = 3 or more times a week, but not every day 
    #   3 = 1-2 times a week
    #   4 = Less than once a week
    #   5 = Never over the past 2 weeks
    # 
    #   If two or more responses are missing then symptom frequency cannot be computed and will be missing. 
    #   Otherwise, the symptom frequency is computed by calculating the mean of the standardized responses and multiplying by 100 as follows:
    #   
    #   Symptom Frequency = 100*Mean((Q3 – 1)/4, (Q5 – 1)/6, (Q7 – 1)/6, (Q9 – 1)/4)
  
  sym_freq_present <- 0
  sym_freq_interm <- 0
  for (k in 1:length(sym_freq_questions)) {
    thisq <- sym_freq_questions[k]
    if (!dat[i,thisq] %in% c(1,2,3,4,5,6,7)) {
      dat[i,thisq] <- NA
    } else if (dat[i,thisq] %in% c(1,2,3,4,5,6,7)) {
      sym_freq_present <- sym_freq_present + 1
    }

  }
  if (sym_freq_present >= 2) {
    q3_norm <- (dat[i,q3]-1)/4
    q5_norm <- (dat[i,q5]-1)/6
    q7_norm <- (dat[i,q7]-1)/6
    q9_norm <- (dat[i,q9]-1)/4
    dat[i,'kccq_sym_freq'] <- 100*(mean(c(q3_norm,q5_norm,q7_norm,q9_norm),na.rm=T))
  }

  
  ## Symptom burden score
  
    # The Symptom Burden score corresponds to questions 4, 6 and 8. The responses should be coded numerically as follows:
    #   
    #   1 = Extremely Bothersome 
    #   2 = Quite a bit Bothersome 
    #   3 = Moderately Bothersome 
    #   4 = Slightly Bothersome
    #   5 = Not at all Bothersome
    #   6 = I’ve had no swelling (fatigue, shortness of breath)
    #   
    #   If a response is 6 (none) then set the response to 5 (not at all). 
    #   If at least one response is present then symptom burden score is computed by calculating the mean response and standardizing the result as follows:
    #     
    #     Symptom Burden = 100*(Mean Response – 1)/4
    
  
  this_q4 <- dat[i,q4]
  if (this_q4==6&!is.na(this_q4)) {this_q4 <- 5}
  this_q6 <- dat[i,q6]
  if (this_q6==6&!is.na(this_q6)) {this_q6 <- 5}
  this_q8 <- dat[i,q8]
  if (this_q8==6&!is.na(this_q8)) {this_q8 <- 5}
  
  if (!is.na(this_q4)|!is.na(this_q6)|!is.na(this_q8)) {
  dat[i,"kccq_sym_burd"] <-100*(mean(c(this_q4,this_q6,this_q8),na.rm=T)-1)/4
  }
  
  
  ## Total symptom score
  
      #  total_sym = mean(sym_freq,sym_burden)
  
   dat[i,"kccq_total_sym"] <- mean(c(dat[i,"kccq_sym_burd"],dat[i,"kccq_sym_freq"]),na.rm=T)
  # 
  # ## Self-efficacy score
  # 
  #   # The Self-Efficacy score corresponds to questions 10 and 11. 
  #   #   Responses to questions 10 and 11 should be coded sequentially (1, 2, 3, 4, 5) in order of increasing health status, with 1 denoting the response associated with the lowest health status. 
  #   #   If at least one question response is present then the self-efficacy score may be computed by standardizing the mean response as follows:
  #   #   
  #   #     Self-Efficacy = 100*(Mean Response – 1)/4
  #   
   if (!is.na(dat[i,q10]|!is.na(dat[i,q11]))) {
   dat[i,"kccq_self_eff"] <- 100*(mean(c(dat[i,q10],dat[i,q11]))-1)/4 
   }
  
  ## Quality of life score
  
    # The Quality of Life score corresponds to questions 12, 13 and 14. 
    #   Responses to questions 12, 13 and 14 should be coded sequentially (1, 2, 3, 4, 5) in order of increasing health status, with 1 denoting the response associated with the lowest health status. 
    #   If at least one question response is present then the quality of life score may be computed by standardizing the mean response as follows:
    #     
    #     Quality of Life = 100*(Mean Response – 1)/4
    
  if (!is.na(dat[i,q12])|!is.na(dat[i,q13])|!is.na(dat[i,q14])) {
    dat[i,"kccq_qol"] <- 100*(mean(c(dat[i,q12],dat[i,q13],dat[i,q14]))-1)/4
  }
  
  ## Social limitation score
  
    # The Social Limitation score corresponds to questions 15a through 15d. These responses should be coded numerically as follows:
    #   
    #   1 = Severely  Limited 
    #   2 = Limited Quite a bit
    #   3 = Moderately Limited 
    #   4 = Slightly Limited
    #   5 = Did Not Limit at All
    #   6 = Does not apply or did not do for other reasons
    #   
    #   If the responses to questions 15a through 15d are not values 1, 2, 3, 4 or 5 then the response is set to missing. 
    #   Note that a response of 6 is treated as a missing value. 
    #   If at least two question responses are present then the social limitation score may be computed by standardizing the mean response as follows:
    #     
    #     Social Limitation = 100*(Mean Response – 1)/4
    
   this_q15a <- dat[i,q15a]
   this_q15b <- dat[i,q15b]
   this_q15c <- dat[i,q15c]
   this_q15d <- dat[i,q15d]
   
   if (!this_q15a %in% c(1,2,3,4,5)) {this_q15a <- NA}
   if (!this_q15b %in% c(1,2,3,4,5)) {this_q15b <- NA}
   if (!this_q15c %in% c(1,2,3,4,5)) {this_q15c <- NA}
   if (!this_q15d %in% c(1,2,3,4,5)) {this_q15d <- NA}
  
   soc_count <- 0
   if (!is.na(this_q15a)) {soc_count <- soc_count+1}
   if (!is.na(this_q15b)) {soc_count <- soc_count+1}
   if (!is.na(this_q15c)) {soc_count <- soc_count+1}
   if (!is.na(this_q15d)) {soc_count <- soc_count+1}
   
   if (soc_count >=2) {
     dat[i,'kccq_soc_lim'] <- 100*(mean(c(this_q15a,this_q15b,this_q15c,this_q15d),na.rm=T)-1)/4
   }
    
  ## Clinical summary score
  
    #     kccq_clin_summ = mean(kccq_phys_lim,kccq_total_sym)
  
   dat[i,"kccq_clin_sum"] <- mean(c(dat[i,"kccq_phys_lim"],dat[i,"kccq_total_sym"]),na.rm=T)
   
  ## Overall summary score
  
    #     kccq_overall_summ = mean(kccq_phys_lim,kccq_total_sym,qol,kccq_social_lim)
 
     dat[i,"kccq_overall_sum"] <- mean(c(dat[i,"kccq_phys_lim"],dat[i,"kccq_total_sym"],dat[i,"kccq_qol"],dat[i,"kccq_soc_lim"]),na.rm=T)
   
  
  
    }
  return(dat)
}




calc_MDRD4 <- function(dat,cr,age="AGE",sex="SEX",race="RACE",male=1,black=2) {
  ##  Assumes 
  return(round(175*
                 as.numeric(dat[,cr])^(-1.154)*
                 as.numeric(dat[,age])^(-0.203)*
                 ifelse(dat[,sex]==male,1,0.742)*
                 ifelse(dat[,race]==black,1.212,1),1))
  
}


calc_bsa <- function(dat,weight_kg="weight_kg", height_cm="height_cm") {
  return(0.20247*
           (dat[,weight_kg]^0.425)*
           ((dat[,height_cm]/100)^0.725))
}

calc_bmi <- function(dat,weight="weight_kg",height="height_cm", metric=T)  {
  if (metric==F) {
    dat$weight_kg <- dat[,weight]/2.2
    dat$height_cm <- dat[,height]*2.54
  }
  if (metric==T) {
    dat$weight_kg <- dat[,weight]
    dat$height_cm <- dat[,height]
  }
  dat$bmi <- dat$weight_kg/(dat$height_cm/100)^2
  return(dat$bmi)
}






### SF-12

# sf_vars <- c("sfgenhl",  # SF12
#              "sfactmod", # SF12 
#              "sfactsts", # SF12
#              "sfphcutw",  
#              "sfphaccl", (#SF12 - duplicate sfemaccl)
#              "sfphlimt", #SF12
#              "sfpbdiff", 
#              "sfpainin", #SF12
#              "sfemcutw",
#              "sfemaccl", #SF12
#              "sfemslop", #SF12
#              "sfextent", 
#              "sfflpep",  #SF12
#              "sfflnerv",
#              "sffldown", #SF12
#              "sfflcalm", #SF12
#              "sfflnrgy", #SF12
#              "sffblue", 
#              "sfflworn",
#              "sfflhapy",
#              "sffltird",
#              "sfsocint") #SF12



calc_framingham_pai <- function(dat,
                                slp_hrs="sleep", 
                                sed_hrs="sedentary",
                                slgt_hrs="slight",
                                mod_hrs="moderate",
                                hvy_hrs="heavy") {
  pai <- 
    dat[,slp_hrs] + 
    dat[,sed_hrs]*1.1 + 
    dat[,slgt_hrs]*1.5 + 
    dat[,mod_hrs]*2.4 + 
    dat[,hvy_hrs]*5
  return(pai)
}




calc_mtlpaq_simple <- function(walk_pleas,  walk_pleas_2wks=0,
                              chores,       chores_2wks=0,
                              mowing,       mowing_2wks=0,
                              raking,       raking_2wks=0,
                              garden,       garden_2wks=0,
                              hiking,       hiking_2wks=0,
                              jogging,      jogging_2wks=0,
                              biking,       biking_2wks=0,
                              cycling,      cycling_2wks=0,
                              dancing,      dancing_2wks=0,
                              aerobics,     aerobics_2wks=0,
                              bowling,      bowling_2wks=0,
                              golf,         golf_2wks=0,
                              tennis_sing,  tennis_sing_2wks=0,
                              tennis_doub,  tennis_doub_wwks=0,
                              racquet,      racquest_2wks=0,
                              exercise,     exercise_2wks=0,
                              swimming,     swimming_2wks=0,
                              other_txt,    other_2wks=0,
                              other2,      other2_txt=0,   other2_2wks=0)
{
  
}


calc_cpet_predict <- function(age,sex=NA,active=F, referral=T, bsa) {

  # Gulati M, Black HR, Shaw LJ, Arnsdorf MF, Merz CNB, Lauer MS, Marwick TH, Pandey DK, Wicklund RH, Thisted RA. 
  # The prognostic value of a nomogram for exercise capacity in women. New England Journal of Medicine, 2005;353(5):468–475. 
  # https://doi.org/10.1056/NEJMOA044154  
  
  # Morris CK, Myers J, Froelicher VF, Kawaguchi T, Ueshima K, Hideg, A. Nomogram based on metabolic equivalents and age for assessing aerobic exercise capacity in men. 
  # Journal of the American College of Cardiology. 1993;22(1):175–182. https://doi.org/10.1016/0735-1097(93)90832-L
  
  if (referral==T)  {
    if (active==T&!is.na(active)) {mets_jacc <- 18.7-0.15*age}
    if (active==F&!is.na(active)) {mets_jacc <- 18.0-0.15*age}
    if (is.na(active)) {mets_jacc <- 16.6-0.16*age}
    if (is.na(active)) {mets_jacc <- 16.6-0.16*age}
  
    apmhr_jacc <- 196-0.9*age
  }
  
  if (referral==F)  {
    if (active==T&!is.na(active)) {mets_jacc <- 16.4-0.13*age}
    if (active==F&!is.na(active)) {mets_jacc <- 11.9-0.07*age}
    if (is.na(active)&is.na(sex)) {mets_jacc <- 14.7-0.11*age}
    if (is.na(active)&sex=="M") {mets_jacc <- 14.7-0.11*age}
    if (is.na(active)&sex=="F") {mets_jacc <- 14.7-0.13*age}
    
    apmhr_jacc <- 200-0.72*age
    
  #   Hollenberg M, Tager IB. Oxygen uptake efficiency slope: An index of exercise performance and cardiopulmonary reserve requiring only submaximal exercise. 
  #   Journal of the American College of Cardiology 2000;36(1):194–201. https://doi.org/10.1016/S0735-1097(00)00691-4
    
  }
  
  if (sex=="M") {oues <- 1320 - (26.7*age) + (1394*bsa)}
  if (sex=="F") {oues <- 1175 - (15.8*age) + (841*bsa)}
  
  return(paste("APMHR:",apmhr_jacc,"  METS:",mets_jacc,"  OUES:",oues))
  
}

