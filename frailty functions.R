
# Social frailty: ESSI, Brim, CHS social scores, HALFt, ISEL, ESSI, BSNI, Makizak, Tilburg, Lubben SNS
# Physical frailty: FRAIL, Fried, Rockwood, Tilburg, FIFE, Katz


################### ******** Fried ******** ##############################

# .&&&&&&&&.&&&&&&&&..&&&&.&&&&&&&&.&&&&&&&&.
# .&&.......&&.....&&..&&..&&.......&&.....&&
# .&&.......&&.....&&..&&..&&.......&&.....&&
# .&&&&&&...&&&&&&&&...&&..&&&&&&...&&.....&&
# .&&.......&&...&&....&&..&&.......&&.....&&
# .&&.......&&....&&...&&..&&.......&&.....&&
# .&&.......&&.....&&.&&&&.&&&&&&&&.&&&&&&&&.

  # Fried LP, Tangen CM, Walston J, et al. 
  # Frailty in Older Adults: Evidence for a Phenotype. 
  # Journal of Gerontology: MEDICAL SCIENCES, 2001;56(3):M146-56 
  # https://academic.oup.com/biomedgerontology/article/56/3/M146/545770
  
  # 5 parameters, 1 point each:
  # 1. Shrinking (unintentional wt loss > 10 lbs in past year)
  # 2. Weakness (grip strength in lowest 20th percentile)
  # 3. Exhaustion (self-report, usually worst of CESD "everything was an effort" or "hard to get going" questions)
  # 4. Slowness - gait speed (walking time/15 feet in lowest 20th percentile adjusted by sex height)
  # 5. Low activity (kcals/week in lowest 20th percentile - males: 383 kcal/week, females: 270 kcal/wk)
  
  # Cut-off values taken from Appendix of above primary reference
  # These may be adjusted from non-CHS data based on data availability
  # Were possible, published harmonization schemes should be used
  
  
score_Fried <- function(dat,
                        sex,
                        height_cm,
                        gait,
                        grip,
                        bmi,
                        female=c(2,"Female"),
                        male=c(1,"Male"),
                        effort,
                        getgo,
                        kcal,
                        wtloss)
{

  ##### Slowness (1 = slow, 0 = fast)

  #### Men:
  # Height ≤ 173 cm (68.1 in):  ≥ 7 sec/15 ft (≤ 0.653 m/sec)    
  # Height >173 cm (68.1 in):   ≥ 6 sec/15 ft (≤ 0.762 m/sec)	
  
  dat[!is.na(dat[,gait]),"fried_gait_score"] <- 0 
  
  dat[dat[,sex]==1&
    dat[,height_cm]>173&
    dat[,walktime] >= 6,"fried_gait_score"] <- 1
  
  dat[dat[,sex]==1&
        dat[,height_cm]<=173&
        dat[,walktime] >= 7,"fried_gait_score"] <- 1
  

  ### Women
  # Height ≤ 159 cm (62.6 in):  ≥ 7 sec/15 ft (≤ 0.653 m/sec)
  # Height >159 cm (62.6 in):   ≥ 6 sec/15 ft (≤ 0.762 m/sec)
  
  dat[dat[,sex]==0&
        dat[,height_cm]>159&
        dat[,walktime] >= 6,"fried_gait_score"] <- 1
  
  dat[dat[,sex]==0&
        dat[,height_cm]<=159&
        dat[,walktime] >= 7,"fried_gait_score"] <- 1
  
  ########### BMI-adjusted grip (kg)
  
  ##### Men
  # BMI ≤ 24.0:     ≤ 29 kg
  # BMI 24.1-26.0:  ≤ 30 kg
  # BMI 26.1-28.0:  ≤ 30 kg
  # BMI > 28.0:     ≤ 32 kg
  
  ##### Women
  # BMI ≤ 23.0:     ≤ 17
  # BMI 23.1-26.0:  ≤ 17.3 kg
  # BMI 26.1-29.0:  ≤ 18 kg
  # BMI > 29.0:     ≤ 21 kg
  
  dat[!is.na(dat[,grip])] <- 0 
  
  # Men
  
  dat[dat[,bmi]<=24&
        dat[,grip]<= 29&
        dat[,sex==1],"fried_grip_score"] <- 1 

  dat[dat[,bmi]>24&
        dat[,bmi]<=26&
        dat[,grip]<= 30&
        dat[,sex==1],"fried_grip_score"] <- 1 
  
  dat[dat[,bmi]>26&
        dat[,bmi]<=28&
        dat[,grip]<= 30&
        dat[,sex==1],"fried_grip_score"] <- 1 
  
  dat[dat[,bmi]>28&
        dat[,grip]<= 32&
        dat[,sex==1],"fried_grip_score"] <- 1 
  
  # Women

  dat[dat[,bmi]<=23&
        dat[,grip]<= 17&
        dat[,sex==0],"fried_grip_score"] <- 1 
  
  dat[dat[,bmi]>23&
        dat[,bmi]<=26&
        dat[,grip]<= 17.3&
        dat[,sex==0],"fried_grip_score"] <- 1 
  
  dat[dat[,bmi]>26&
        dat[,bmi]<=29&
        dat[,grip]<= 18&
        dat[,sex==0],"fried_grip_score"] <- 1 
  
  dat[dat[,bmi]>29&
        dat[,grip]<= 21&
        dat[,sex==0],"fried_grip_score"] <- 1 
  
  dat[,"Fried_score"] <- 
    dat[,"fried_gait_score"]+
    dat[,"fried_grip_score"]+
    dat[,fatigue]+
    dat[,kcal]
    dat[,wtloss]
  
 dat$Fried_rank[dat$Fried_score==0] <- 1
 dat$Fried_cat[dat$Fried_score==0] <- "Robust"

 dat$Fried_rank[dat$Fried_score %in% c(1,2)] <- 2
 dat$Fried_cat[dat$Fried_score %in% c(1,2)] <- "Pre-frail"
 
 dat$Fried_rank[dat$Fried_score %in% c(3,4,5)] <- 3
 dat$Fried_cat[dat$Fried_score %in% c(3,4,5)] <- "Frail"
 
 return(dat)
  
}

################### ******** FRAIL ******** ################################


# .&&&&&&&&.&&&&&&&&.....&&&....&&&&.&&......
# .&&.......&&.....&&...&&.&&....&&..&&......
# .&&.......&&.....&&..&&...&&...&&..&&......
# .&&&&&&...&&&&&&&&..&&.....&&..&&..&&......
# .&&.......&&...&&...&&&&&&&&&..&&..&&......
# .&&.......&&....&&..&&.....&&..&&..&&......
# .&&.......&&.....&&.&&.....&&.&&&&.&&&&&&&&

# van Kan, G. A., Rolland, Y. M., Morley, J. E., & Vellas, B. (2008). 
# Frailty: Toward a Clinical Definition.J Am Med Dir Assoc. 2008;9(2):71–72. 
# https://doi.org/10.1016/j.jamda.2007.11.005  

# Uses harmonized values from ADAPT-HF pipeline
# Scores for 0-4 conditions, score = 0, for ≥ 5, score=1

## Fatigue - uses harmonized value
## Resistance - uses harmonized value
## Ambulate - uses harmonized values
## Illness score counts presence of HTN, DM, cancer, chronic lung disease, MI, CHD, angina, asthma, arthritis, stroke, and ckd
## Weight loss - uses harmnonized value

score_FRAIL <- function(dat,
                        dm_frail,
                        htn_frail,
                        copd_frail,
                        asthma_frail,
                        djd_frail,
                        ckd_frail,
                        chf_frail,
                        chd_frail,
                        stroke_frail,
                        cancer_frail) 
{

# Compile chronic illness

dat[,"conditions_frail"] <- 
  dat[,dm_frail]+
  dat[,htn_frail]+
  dat[,copd_frail]+
  dat[,asthma_frail]+
  dat[,djd_frail]+
  dat[,ckd_frail]+
  dat[,chf_frail]+
  dat[,chd_frail]+
  dat[,stroke_frail]+
  dat[,cancer_frail]

dat[,dat[,conditions_frail <= 4],"illness_frail"] <- 0
dat[,dat[,conditions_frail > 4],"illness_frail"] <- 1

## Calculate FRAIL score

dat[,"FRAIL_score"] <- 
  dat[,fatigue_frail]+
  dat[,resistance_frail]+
  dat[,ambulate_frail]+
  dat[,"illness_frail"]+
  dat[,wtloss_frail]

dat$FRAIL_rank[dat$FRAIL_score==0] <- 1
dat$FRAIL_cat[dat$FRAIL_score==0] <- "Robust"

dat$FRAIL_rank[dat$FRAIL_score %in% c(1,2)] <- 2
dat$FRAIL_cat[dat$FRAIL_score %in% c(1,2)] <- "Pre-frail"

dat$FRAIL_rank[dat$FRAIL_score %in% c(3,4,5)] <- 3
dat$FRAIL_cat[dat$FRAIL_score %in% c(3,4,5)] <- "Frail"

return(dat)

}



################### ******** Tilburg ******** ################################

# .&&&&&&&&.&&&&.&&.......&&&&&&&&..&&.....&&.&&&&&&&&...&&&&&&..
# ....&&.....&&..&&.......&&.....&&.&&.....&&.&&.....&&.&&....&&.
# ....&&.....&&..&&.......&&.....&&.&&.....&&.&&.....&&.&&.......
# ....&&.....&&..&&.......&&&&&&&&..&&.....&&.&&&&&&&&..&&...&&&&
# ....&&.....&&..&&.......&&.....&&.&&.....&&.&&...&&...&&....&&.
# ....&&.....&&..&&.......&&.....&&.&&.....&&.&&....&&..&&....&&.
# ....&&....&&&&.&&&&&&&&.&&&&&&&&...&&&&&&&..&&.....&&..&&&&&&..

# Gobbens, RJJ, van Assen MALM, Luijkx KG, Wijnen-Sponselee MT, Schols JMGA.  
# The Tilburg Frailty Indicator: Psychometric Properties. 
# Journal of the American Medical Directors Association 2010;11(5), 344–355
# https://doi.org/10.1016/J.JAMDA.2009.11.003

## Physical components
# 1) Feel Physically healthy?:                  No = 1/Yes = 0
# 2) Unintentional weight loss (6 kg/6 months or 3kg/1 month: No = 0/Yes = 1)
# Experience problems in daily life due to      No = 0/yes = 1
#     3) Difficulty walking 
#     4) Difficulty maintaining balance
#     5) Poor hearing
#     6) Poor vision
#     7) Lack of hand strength
#     8) Physical tiredness

## Psychological components
# 9)  Memory problems?                          No/sometimes = 0; yes=1
# 10) Felt down in last month?                  No = 0; sometimes/yes = 1
# 11) Felt nervous or anxious in last month?    No = 0, sometimes/yes = 1
# 12) Able to cope with problems well?          No = 1/Yes = 0

## Social components
# 13) Live alone?                               No = 0/Yes = 1
# 14) Miss having people around you?            No = 0; sometimes/yes=1
# 15) Receive enough support from other people? No = 1/Yes = 0

score_Tilburg <- function(dat,
                          physically_healthy,
                          walking,
                          balance,
                          hearing,
                          vision,
                          grip_report,
                          tiredness,
                          memory,
                          felt_down,
                          anxious,
                          coping,
                          lives_alone,
                          miss_people,
                          receive_support) 
{
  
  dat$Tilburg_score <-
    dat[,physically_healthy]+
    dat[,walking]+
    dat[,balance]+
    dat[,hearing]+
    dat[,vision]+
    dat[,grip_report]+
    dat[,tiredness]+
    dat[,memory]+
    dat[,felt_down]+
    dat[,anxious]+
    dat[,coping]+
    dat[,lives_alone]+
    dat[,miss_people]+
    dat[,receive_support]

  dat[dat$Tilburg_score %in% c(0,1),c("Tilburg_rank","Tilburg_cat")] <- c(1,"Robust")
  dat[dat$Tilburg_score %in% c(2,3,4),c("Tilburg_rank","Tilburg_cat")] <- c(2,"Pre-frail")
  dat[dat$Tilburg_score >= 5,c("Tilburg_rank","Tilburg_cat")] <- c(3,"Frail")
  
  dat$Tilburg_physical_score <-
    dat[,physically_healthy]+
    dat[,walking]+
    dat[,balance]+
    dat[,hearing]+
    dat[,vision]+
    dat[,grip_report]+
    dat[,tiredness]

  dat$Tilburg_psych_score <-
    dat[,memory]+
    dat[,felt_down]+
    dat[,anxious]+
    dat[,coping]

  dat$Tilburg_social_score <-
    dat[,lives_alone]+
    dat[,miss_people]+
    dat[,receive_support]
  
  return(dat)
}



############ ******** Berkman Social Network Index ******** ##################


# .&&&&&&&&...&&&&&&..&&....&&.&&&&
# .&&.....&&.&&....&&.&&&...&&..&&.
# .&&.....&&.&&.......&&&&..&&..&&.
# .&&&&&&&&...&&&&&&..&&.&&.&&..&&.
# .&&.....&&.......&&.&&..&&&&..&&.
# .&&.....&&.&&....&&.&&...&&&..&&.
# .&&&&&&&&...&&&&&&..&&....&&.&&&&

# Berkman, LF. Social networks, host resistance, and mortality, a follow-up study of Alameda County residents.
# Berkman, LF, Syme SL. Social networks, host resistance, and mortality: a nine-year follow-up study of Alameda County residents. 
# Am J Epidem. 1979;109(2), 186–204. https://doi.org/10.1093/OXFORDJOURNALS.AJE.A112674

# 1) Num close friends to talk to: 0 -> 0pts; 1-2 -> 1pts; 3-5 -> 2; 6-9 -> 3pts; ≥10 -> 4pts
# *** 2) Num close friends seen ≥ 1/mo: 0 -> 0pts; 1-2 -> 1pts; 3-5 -> 2; 6-9 -> 3pts; ≥10 -> 4pts
# 3) Num relatives to talk to: 0 -> 0pts; 1-2 -> 1pts; 3-5 -> 2; 6-9 -> 3pts; ≥10 -> 4pts
# *** 4) Num relatives seen ≥ 1/mo: 0 -> 0pts; 1-2 -> 1pts; 3-5 -> 2; 6-9 -> 3pts; ≥10 -> 4pts
# 5) Group participation (e.g. church, social, volunteer): No -> 0pts, Yes = 1pt
# 6) Frequency of religious service: < 1/year -> 0pts; 1-2/year -> 1pt; > 2/year + < 1/month -> 2pts
#   1-2/month -> 3pts; 3-4x/month -> 4 pts, > 1/week -> 5 pts
# 7) Medicare or Medicaid coverage: No -> 0pts; Yes = 1pt
# 8) Any health insurance: No -> 0pt; Yes = 1pt
# 9) Someone to talk to: Never -> 0pt; A little = 1pt; Sometimes = 2pts; Most of the time -> 3pts; Always = 4pts
# 10) Someone to give advice: Never -> 0pt; A little = 1pt; Sometimes = 2pts; Most of the time -> 3pts; Always = 4pts
# 11) Someone to show love and affection: Never -> 0pt; A little = 1pt; Sometimes = 2pts; Most of the time -> 3pts; Always = 4pts
# 12) Someone to provide emotional support: Never -> 0pt; A little = 1pt; Sometimes = 2pts; Most of the time -> 3pts; Always = 4pts

# Reference: https://sakai.unc.edu/access/content/user/vschoenb/Public%20Library/People/BySurname/S/Victor%20Schoenbach/Research/Social%20Networks%20-%20archival/Appendix-BerkmanSocialNetworkIndex-BSNI-1983.docx  
# Reference: https://www.proquest.com/docview/288073491

score_BSNI <- 
  function(dat,
           ptid = "patientid",
           cf = "bsnq_num_friends",
           cfs = "bsnq_friends_per_month",
           rels = "bsnq_num_relatives",
           rel_seen = "bsnq_rels_per_month",
           gm = "bsnq_group_act",
           church = "bsnq_church_freq",
           marital = "bsnq_marital",
           adviser = "bsnq_adviser",
           listen="bsnq_listener",
           love="bsnq_love_affection",
           support = "bsnq_emot_support",
           confidant = "bsnq_confidant") {
      
  ## Calculate overall BSNI
    
    dat[,'bsnq_total_11'] <-
      dat[,cf]+
      dat[,cfs]+
      dat[,rels]+
      dat[,rel_seen]+
      dat[,gm]+
      dat[,church]+
      dat[,listen]+
      dat[,adviser]+
      dat[,love]+
      dat[,support]+
      dat[,confidant]
      
 
  ## Simplest BSNQ index
    
    dat$bsnq_cfr4[(dat[,cf]+dat[,rels]) <= 1] <- 0
    dat$bsnq_cfr4[(dat[,cf]+dat[,rels]) > 1] <- 1

    dat$bsnq_church_member[dat$bsnq_church_freq<3] <- 0
    dat$bsnq_church_member[dat$bsnq_church_freq>=3] <- 1
    
    dat$bsn_index4 <-
      dat[,marital]+
      dat[,gm]+
      dat$bsnq_cfr4+
      dat$bsnq_church_member
        
  ## Subscores
  
    dat$bsnq_church_member[dat$bsnq_church_freq<3] <- 0
    dat$bsnq_church_member[dat$bsnq_church_freq>=3] <- 1
    
    dat[dat[,cf]==0&!is.na(dat[,cf]),"cf_midpoint"] <- 0
    dat[dat[,cf]==1&!is.na(dat[,cf]),"cf_midpoint"] <- 1.5
    dat[dat[,cf]==2&!is.na(dat[,cf]),"cf_midpoint"] <- 4
    dat[dat[,cf]==3&!is.na(dat[,cf]),"cf_midpoint"] <- 7.5
    dat[dat[,cf]==4&!is.na(dat[,cf]),"cf_midpoint"] <- 10
    
    dat[dat[,rels]==0&!is.na(dat[,rels]),"rel_midpoint"] <- 0
    dat[dat[,rels]==1&!is.na(dat[,rels]),"rel_midpoint"] <- 1.5
    dat[dat[,rels]==2&!is.na(dat[,rels]),"rel_midpoint"] <- 4
    dat[dat[,rels]==3&!is.na(dat[,rels]),"rel_midpoint"] <- 7.5
    dat[dat[,rels]==4&!is.na(dat[,rels]),"rel_midpoint"] <- 10
    
    dat$rel_friends_sum <- dat$cf_midpoint + dat$rel_midpoint 
    dat$rel_friends[dat$rel_friends_sum <= 4] <- 1
    dat$rel_friends[dat$rel_friends_sum > 4&dat$rel_friends_sum < 10 ] <- 2
    dat$rel_friends[dat$rel_friends_sum >= 10&dat$rel_friends_sum < 16 ] <- 3
    dat$rel_friends[dat$rel_friends_sum >= 16] <- 4
    
    dat[dat[,cfs]==0&!is.na(dat[,cfs]),"cf_seen_midpoint"] <- 0
    dat[dat[,cfs]==1&!is.na(dat[,cfs]),"cf_seen_midpoint"] <- 1.5
    dat[dat[,cfs]==2&!is.na(dat[,cfs]),"cf_seen_midpoint"] <- 4
    dat[dat[,cfs]==3&!is.na(dat[,cfs]),"cf_seen_midpoint"] <- 7.5
    dat[dat[,cfs]==4&!is.na(dat[,cfs]),"cf_seen_midpoint"] <- 10
    
    dat[dat[,rel_seen]==0&!is.na(dat[,rel_seen]),"rel_seen_midpoint"] <- 0
    dat[dat[,rel_seen]==1&!is.na(dat[,rel_seen]),"rel_seen_midpoint"] <- 1.5
    dat[dat[,rel_seen]==2&!is.na(dat[,rel_seen]),"rel_seen_midpoint"] <- 4
    dat[dat[,rel_seen]==3&!is.na(dat[,rel_seen]),"rel_seen_midpoint"] <- 7.5
    dat[dat[,rel_seen]==4&!is.na(dat[,rel_seen]),"rel_seen_midpoint"] <- 10
    
    dat$rel_friends_seen <- 
      dat$cf_seen_midpoint + 
      dat$rel_seen_midpoint

## Calculate sociability score    

    dat$bsnq_sociability[dat$rel_friends==1] <- 1
    dat$bsnq_sociability[dat$rel_friends %in% c(2,3)] <- 2
    dat$bsnq_sociability[dat$rel_friends==4&dat$rel_friends_seen <=5] <- 2
    dat$bsnq_sociability[dat$rel_friends==4&dat$rel_friends_seen >= 6] <- 3

## Calculate intimates index    
        
    dat$bsnq_intimates[dat[,marital] %in% c(1,"Yes")] <-  
      dat$bsnq_sociability[dat[,marital] %in% c(1,"Yes")]
    
    dat$bsnq_intimates[dat[,marital] %in% c(0,"No")&
                    dat$bsnq_sociability %in% c(1,2)] <- 1 
    
    dat$bsnq_intimates[dat[,marital] %in% c(0,"No")&dat$bsnq_sociability==3] <- 2 
    
    dat[,"bsn_index"] <- 
      4*dat[,"bsnq_intimates"] +
      2*dat[,"bsnq_church_member"] +
      dat[,gm]-3 
    
  return(dat)
    
}

################### ******** Makizako ******** ################################

# .&&.....&&....&&&....&&....&&.&&&&.&&&&&&&&....&&&....&&....&&..&&&&&&&.
# .&&&...&&&...&&.&&...&&...&&...&&.......&&....&&.&&...&&...&&..&&.....&&
# .&&&&.&&&&..&&...&&..&&..&&....&&......&&....&&...&&..&&..&&...&&.....&&
# .&&.&&&.&&.&&.....&&.&&&&&.....&&.....&&....&&.....&&.&&&&&....&&.....&&
# .&&.....&&.&&&&&&&&&.&&..&&....&&....&&.....&&&&&&&&&.&&..&&...&&.....&&
# .&&.....&&.&&.....&&.&&...&&...&&...&&......&&.....&&.&&...&&..&&.....&&
# .&&.....&&.&&.....&&.&&....&&.&&&&.&&&&&&&&.&&.....&&.&&....&&..&&&&&&&.

# Makizako, H., Shimada, H., Tsutsumimoto, K., Lee, S., Doi, T., Nakakubo, S., Hotta, R., & Suzuki, T. (2015). 
# Social Frailty in Community-Dwelling Older Adults as a Risk Factor for Disability. 
# Journal of the American Medical Directors Association, 16(11), 1003.e7-1003.e11. 
# https://doi.org/10.1016/J.JAMDA.2015.08.023

# Jujo K, Kagiyama, N, Saito, K, et al. 
# Impact of social frailty in hospitalized elderly patients with heart failure: A fragile-hf registry subanalysis. 
# Journal of the American Heart Association, 10(17), 19954. 
# https://doi.org/10.1161/JAHA.120.019954

# 1) Going out less frequently in past year       (No = 0/Yes = 1)
# 2) Not visiting friends                         (No = 0/Yes = 1)
# 3) Not talking with someone every day           (No = 0/Yes = 1)
# 4) Not feeling helpful towards friends/family   (No = 0/Yes = 1)
# 5) Living alone                                 (No = 0/Yes = 1)

# Interpretation
# ≥ 3 = frail

score_Makizako <- function(dat,
                           going_out,
                           not_visiting,
                           not_talking,
                           not_helpful,
                           lives_alone) 
{
  
dat$Makizako_score <-
  dat[,going_out]+
  dat[,not_visiting]+
  dat[,not_taking]+
  dat[,not_helpful]+
  dat[,lives_alone]

  dat[dat$Makizako_score==0,c("Makizako_rank","Makizako_rank")] <- 
    c(1,"Robust")

  dat[dat$Makizako_score %in% c(1,2),c("Makizako_rank","Makizako_rank")] <- 
    c(2,"Pre-frail")
  
  dat[dat$Makizako_score >= 3,c("Makizako_rank","Makizako_rank")] <- 
    c(3,"Frail")
  
  return(dat)
  
}

################### ******** HALFT ******** ################################


# .&&.....&&....&&&....&&.......&&&&&&&&.&&&&&&&&
# .&&.....&&...&&.&&...&&.......&&..........&&...
# .&&.....&&..&&...&&..&&.......&&..........&&...
# .&&&&&&&&&.&&.....&&.&&.......&&&&&&......&&...
# .&&.....&&.&&&&&&&&&.&&.......&&..........&&...
# .&&.....&&.&&.....&&.&&.......&&..........&&...
# .&&.....&&.&&.....&&.&&&&&&&&.&&..........&&...

# Ma L, Sun F, Tang Z. 
# Social frailty is associated with physical functioning, cognition, and depression, and predicts mortality. 
# J Nutr Health Aging.2018;22(8):989–995.
# https://pubmed.ncbi.nlm.nih.gov/30272104/

# In past 12 months:
#   1) Helped friends/family members in need      (No = 1/Yes = 0)
#   2) Participated in social/leisure activities  (No = 1/Yes = 0)
#   3) Had enough income to live on               (No = 1/Yes = 0)
#   4) In past week, have you felt lonely?        (No = 0/Yes = 1)
#   5) Do you have someone to talk to daily?      (No = 0/Yes = 1)

# Interpretation 
#   0 = normal social function
#   1-2 = pre-social frailty
#   ≥ 3 = social frailty

score_HALFt <- function(dat,
                        help_others,
                        participate_activities,
                        enough_income,
                        lonely,
                        someone_to_talk_to) 
{
  
  dat$HALFt_score <- 
    dat[,help_others]+
    dat[,participate_activities]+
    dat[,enough_income]+
    dat[,lonely]+
    dat[,someone_to_talk_to]
  
  dat[dat$HALFt_score==0, c("HALFt_rank","HALFt_score")] <- c(1,"Robust")
  dat[dat$HALFt_score %in% c(1,2), c("HALFt_rank","HALFt_score")] <- c(2,"Pre-frail")
  dat[dat$HALFt_score >= 3, c("HALFt_rank","HALFt_score")] <- c(3,"Frail")
  
  return(dat)
  
}

################### ******** Rockwood ******** ################################

# .&&&&&&&&...&&&&&&&...&&&&&&..&&....&&.&&......&&..&&&&&&&...&&&&&&&..&&&&&&&&.
# .&&.....&&.&&.....&&.&&....&&.&&...&&..&&..&&..&&.&&.....&&.&&.....&&.&&.....&&
# .&&.....&&.&&.....&&.&&.......&&..&&...&&..&&..&&.&&.....&&.&&.....&&.&&.....&&
# .&&&&&&&&..&&.....&&.&&.......&&&&&....&&..&&..&&.&&.....&&.&&.....&&.&&.....&&
# .&&...&&...&&.....&&.&&.......&&..&&...&&..&&..&&.&&.....&&.&&.....&&.&&.....&&
# .&&....&&..&&.....&&.&&....&&.&&...&&..&&..&&..&&.&&.....&&.&&.....&&.&&.....&&
# .&&.....&&..&&&&&&&...&&&&&&..&&....&&..&&&..&&&...&&&&&&&...&&&&&&&..&&&&&&&&.



# Jones, D. M., Song, X., & Rockwood, K. (2004). 
# Operationalizing a frailty index from a standardized comprehensive geriatric assessment. 
# Journal of the American Geriatrics Society, 52(11), 1929–1933. 
# https://doi.org/10.1111/J.1532-5415.2004.52521.X


score_Rockwood <- function(dat,
                           heavy_housework,
                           half_mile,
                           up_down_stairs,
                           effort,
                           getgo,
                           dress,
                           bathe,
                           transfer,
                           toilet,
                           limit_mod_act,
                           limit_several_flights,
                           physical_accomplish_less,
                           physical_limit_work_otheract,
                           general_health,
                           mood_accomplish_less,
                           mood_limit_work_otheract,
                           pain_limit_work,
                           calm_peaceful,
                           lots_of_energy,
                           felt_downhearted,
                           phys_emot_limit_social_act,
                           htn,
                           lung,
                           dm,
                           cad,
                           ckd,
                           osteoporosis_fx,
                           chf,
                           stroke_tia,
                           cancer,
                           arthritis,
                           dementia,
                           bmi,
                           wtloss,
                           grip,
                           gait,
                           snf)
  {

  
  # 1) Do heavy work around the house, like shoveling snow or washing windows, walls, or floors w/o help?
  # 2) Walk half a mile w/o help?
  # 3) Walk up and down 1 flight of stairs w/o help?
  # 4) During past week, everything was an effort? (Rarely=0; Sometime/occasional=0.5, most/always=1)
  # 5) During past week, I could not get going? (Rarely=0; Sometime/occasional=0.5, most/always=1)
  
  # Katz ADL - can you do these independently? 
  #   Katz score 0 = 0pts (independent); 1-2 = 0.5 pts (any assistance); 3 = 1 point (dependent)
  #   6) Dressing
  #   7) Bathing
  #   8) Transferring
  #   9) Toileting
  
  # Health status and function
  # Does your health limit you in these activities? How much? (A lot = 1pts; a little = 0.5pts; No = 0pts)
  #   10) Moderate activities (moving a table, pushing vacuum cleaner, bowling, playing golf)
  #   11) Climbing several flights of stairs
  
  # During past 4 weeks, have you had difficulty with these as a result of physical health?
  #   12) Accomplished less than you would like (No = 0/Yes = 1)
  #   13) Type of work or other activities (No = 0/Yes = 1)
  
  # 14) Rate general health (Poor = 1; Fair = 0.75; Good = 0.5; Very good = 0.25; excellent = 0)
  
  # Mood
  # During past 4 weeks, difficulty with these due to emotional problems (e.g. depressed/anxious)?
  #   15) Accomplished less than you would like (No = 0/Yes = 1)
  #   16) Type of work or other activities (No = 0/Yes = 1)
  
  # 17) During past 4 weeks, how much did pain interfere with normal work 
  # (Not at all = 0, a little = 0.25, moderate = 0.5, quite a bit=0.75, extremely = 1)
  
  # How much of the time during past 4 weeks...
  # - All or most of the time = 0
  # - A good bit of the time =0.25
  # - Some of the time = 0.5
  # - A little of the time = 0.75
  # - none of the time = 1
  #   18) Felt calm and peaceful
  #   19) Have a lot of energy
  #   20) Felt downhearted and blue
  #   21) Physical/emotional problems interfered with social activities
    
  # Comorbidities
  #   22) Hypertension (>140/90 OR history OR on meds = 1pt)
  #   23) Lung disease (yes to any = 1pt; maybe to any = 0.5pts, no to all = 0pts)
  #       * asthma
  #       * copd
  #       * chronic bronchitis
  #       * emphysema
  #       * pulmonary fibrosis
  #   24) DM (fasting blood flucose > 125 mg/dL OR oral meds OR insulin use = 1 pt)
  #   25) CAD (MI or coronary insufficiency = 1pt, angina or possible cor insufficiency = 0.5)
  #   26) CKD 
  #         * Stage 1/2 OR eGFR > 60 = 0pt
  #         * Stage III or eGFR 30-60 = 0.5pts
  #         * Stage 4/5 or eGFR < 30 = 1 pt)
  #   27) Oseoporosis related fractur (Yes = 1)
  #   28) CHF (Yes = 1; Maybe = 0.5,; No = 0pts)
  #   29) Stroke/TIA (Yes = 1; Maybe = 0.5,; No = 0pts)
  #   30) Cancer (Yes = 1; Maybe = 0.5,; No = 0pts)
  #   31) Arthritis (yes to any = 1pt; maybe to any = 0.5pts, no to all = 0pts)
  #     * RA, DJD, gout, connective tissue disorder
  
  # Cognition
  #   32) Dementia by self-report, validated events or MMSE
  #       * Diagnosed, been told they by a doctor, MMSE < 18 = 1pt
  #       * Self-report: told 'maybe demetia'; MMSE 18-23 = 0.5pt
  #       * No diagnosis, no self-report, MMSE ≥ 24 = 0pt
  
  # Measurements (score 1 pt for each if unable to complete due to physical limits)
  #   33) BMI (< 18.5 or > 30 = 1 pt)
  #   34) Weight loss (> 10 lbs in last year = 1pt)
  #   35) Reduced grip strength (3 trials, use highest value)
  #     * Men: 
  #       - BMI < 24 & GS ≤ 29kg = 1pt
  #       - BMI 24.1-28 & GS ≤ 30 kg = 1pt
  #       - BMI > 28 & GS ≤ 32 = 1pt
  #     * Women:
  #       - BMI < 23 & GS ≤ 17kg = 1pt
  #       - BMI 23.1-26 & GS ≤ 17.3 kg = 1pt
  #       - BMI 26.1-29 & GS ≤ 18 kg = 1pt
  #       - BMI > 29 & GS ≤ 21 = 1pt
  #   36) Gait speed over 4 m (<0.8 m/s = 1; >=0.8 m/s = 0)
  #
  #   37) Have you been admitted to a nursing home.SNF in past year (Yes = 1)
  
  dat$Rockwood_total <-
    dat[,heavy_housework]+
    dat[,half_mile]+
    dat[,up_down_stairs]+
    dat[,effort]+
    dat[,getgo]+
    dat[,dress]+
    dat[,bathe]+
    dat[,transfer]+
    dat[,toilet]+
    dat[,limit_mod_act]+
    dat[,limit_several_flights]+
    dat[,physical_accomplish_less]+
    dat[,physical_limit_work_otheract]+
    dat[,general_health]+
    dat[,mood_accomplish_less]+
    dat[,mood_limit_work_otheract]+
    dat[,pain_limit_work]+
    dat[,calm_peaceful]+
    dat[,lots_of_energy]+
    dat[,felt_downhearted]+
    dat[,phys_emot_limit_social_act]+
    dat[,htn]+
    dat[,lung]+
    dat[,dm]+
    dat[,cad]+
    dat[,ckd]+
    dat[,osteoporosis_fx]+
    dat[,chf]+
    dat[,stroke_tia]+
    dat[,cancer]+
    dat[,arthritis]+
    dat[,dementia]+
    dat[,bmi]+
    dat[,wtloss]+
    dat[,grip]+
    dat[,gait]+
    dat[,snf]
  
    return(dat)
  
  }



################### ******** Barthel ******** ################################


# .&&&&&&&&.....&&&....&&&&&&&&..&&&&&&&&.&&.....&&.&&&&&&&&.&&......
# .&&.....&&...&&.&&...&&.....&&....&&....&&.....&&.&&.......&&......
# .&&.....&&..&&...&&..&&.....&&....&&....&&.....&&.&&.......&&......
# .&&&&&&&&..&&.....&&.&&&&&&&&.....&&....&&&&&&&&&.&&&&&&...&&......
# .&&.....&&.&&&&&&&&&.&&...&&......&&....&&.....&&.&&.......&&......
# .&&.....&&.&&.....&&.&&....&&.....&&....&&.....&&.&&.......&&......
# .&&&&&&&&..&&.....&&.&&.....&&....&&....&&.....&&.&&&&&&&&.&&&&&&&&

# Mahoney FI, Barthel DW. Barthel index. Maryland state medical journal. 1965

# Note that higher score = MORE INDEPENDENT/LESS FRAIL unlike others
# 1) Feeding 
#   * Unable = 0 pts
#   * With help (cutting food;preparing) = 5pts
#   * Independent = 10pts

# 2) Transfer from wheelchair to bed and back incl. sitting 
#   * Unable = 0 pts
#   * Can sit up but needs assistance to to be lifted out = 5 pts
#   * Minimal help with some step or needs prompts for safety = 10 pts
#   * Independent = 15 pts

# 3) Personal hygiene
#   * Can wash hands/face, comb hair, shave, brush teeth = 5 pts
#   * Unable to wash hands/face, comb hair, shave, brush teeth = 0 pts

# 4) Getting on/off toilet
#   * Able to to go to the bathroom without help = 10 pts
#   * Able to to go to the bathroom with help = 5 pts
#   * Unable to to go to the bathroom unassisted = 0 pts

# 5) Bathing self
#   * Able to use bath tub, shower = 5 pts
#   * Unable to use bath tub, shower = 0 pts


# 6) Walking on a level service
#   * Unable to walk 50 yards = 0pts
#   * Propel and maneuver wheelchair > 50 yards = 5 pts
#   * Can walk > 50 yards with assistance = 10 pts
#   * Can walk > 50 yards without assistance = 15pts

# 7) Ascend/descend stairs
#   * Cannot ascend/descend stairs at all = 0
#   * Needs someone to help = 5 pts
#   * Can get up and down flight of stairs with no supervision.= 10 pts

# 8) Dress/undress
#   * Cannot dress/undress (does < 50% of work) = 0pts
#   * Needs help from someone but does > 50% = 5 pts
#   * Does without help = 10 pts

# 9) Bowel continence
#   * Has no accidents, can use suppository or enema if necessary without help = 10pts
#   * Occasional accidents or needs help to use suppositories = 5 pts
#   * No control/max assistance = 0 pts

# 10) Bladder continence
#   * Can control or can manage supportive measures without assistance = 10 pts
#   * Occasional accidents or nees help with external device = 5 pts
#   * No control = 0 pts


score_Barthel <- function(dat,
                          feed,
                          transfer,
                          hygiene,
                          toilet,
                          bath,
                          walk_level,
                          up_down_stairs,
                          dress_undress,
                          bowel_incontinence,
                          urine_incontinence) 
{
   
  dat[,"barthel_score"] <-
    dat[,feed]+
    dat[,transfer]+
    dat[,hygiene]+
    dat[,toilet]+
    dat[,bath]+
    dat[,walk_level]+
    dat[,up_down_stairs]+
    dat[,dress_undress]+
    dat[,bowel_incontinence]+
    dat[,urine_incontinence]
}
  

################### ******** FIFE ******** ################################


# .&&&&&&&&.&&&&.&&&&&&&&.&&&&&&&&
# .&&........&&..&&.......&&......
# .&&........&&..&&.......&&......
# .&&&&&&....&&..&&&&&&...&&&&&&..
# .&&........&&..&&.......&&......
# .&&........&&..&&.......&&......
# .&&.......&&&&.&&.......&&&&&&&&

#  Tocchi, C., Dixon, J., Naylor, M., Sangchoon, J., & McCorkle, R. (2014). Development of a frailty index
# measure for older adults: The Frailty Index for Elders. Journal of Nursing Measurement, 22(2), 223-240.

# 1 point per item for 'yes':
# 1) Do you need help getting in/out of bed?
# 2) Do you need hep with washing or bathing?
# 3) Unintentional 10 lb weight gain or loss in last 6 months?
# 4) Tooth/mouth problems that make it hard to eat?
# 5) Poor appetite & quickly feel full?
# 6) Physical health or emotional problems interfere w/ social activities?
# 7) Would say health is fair or poor?
# 8) Easily get tired?
# 9) Hospitalized in past 3 months?
# 10) Gone to ED for health problem in last 3 months?

# Total scoring:
#   0 = no frailty
#   1-3 = frailty risk
#   ≥ 4 = frail


score_FIFE <- function(dat,
                       bed,
                       bathing,
                       wtloss,
                       teeth,
                       poor_appetite,
                       health_social_act,
                       health,
                       tired,
                       hospital_3mo,
                       ed_3mo) {
  
  dat$FIFE_score <-
  dat[,bed]+
    dat[,bathing]+
    dat[,wtloss]+
    dat[,teeth]+
    dat[,poor_appetite]+
    dat[,health_social_act]+
    dat[,health]+
    dat[,tired]+
    dat[,hospital_3mo]+
    dat[,ed_3mo]

  dat$FIFE_rank[dat$FIFE_score==0] <- 1
  dat$FIFE_rank[dat$FIFE_score %in% c(1,2,3)] <- 2 
  dat$FIFE_rank[dat$FIFE_score >= 4] <- 3
  
  dat$FIFE_cat[dat$FIFE_score==0] <- "Robust"
  dat$FIFE_cat[dat$FIFE_score %in% c(1,2,3)] <- "Pre-frail" 
  dat$FIFE_cat[dat$FIFE_score >= 4] <- "Frail"
  
  return(dat)
  
}

################### ******** ISEL-SF ******** ################################


# .&&&&..&&&&&&..&&&&&&&&.&&................&&&&&&..&&&&&&&&
# ..&&..&&....&&.&&.......&&...............&&....&&.&&......
# ..&&..&&.......&&.......&&...............&&.......&&......
# ..&&...&&&&&&..&&&&&&...&&.......&&&&&&&..&&&&&&..&&&&&&..
# ..&&........&&.&&.......&&.....................&&.&&......
# ..&&..&&....&&.&&.......&&...............&&....&&.&&......
# .&&&&..&&&&&&..&&&&&&&&.&&&&&&&&..........&&&&&&..&&......

# All scores: 1: Definitely false   2: Probably false   3: Probably true    4. Definitely true

# 1. If I wanted to go on a trip for a day (for example, to the country or mountains), I would have a hard time finding someone to go with me.
# 2. I feel that there is no one I can share my most private worries and fears with.
# 3. If I were sick, I could easily find someone to help me with my daily chores.
# 4. There is someone I can turn to for advice about handling problems with my family.
# 5. If I decide one afternoon that I would like to go to a movie that evening, I could easily find someone to go with me.
# 6. When I need suggestions on how to deal with a personal problem, I know someone I can turn to.
# 7. I don't often get invited to do things with others.
# 8. If I had to go out of town for a few weeks, it would be difficult to find someone who would look after my house or apartment (the plants, pets, garden, etc.).
# 9. If I wanted to have lunch with someone, I could easily find someone to join me.
# 10. If I was stranded 10 miles from home, there is someone I could call who could come and get me.
# 11. If a family crisis arose, it would be difficult to find someone who could give me good advice about how to handle it.
# 12. If I needed some help in moving to a new house or apartment, I would have a hard time finding someone to help me.

# Scoring:
# Items 1, 2, 7, 8, 11, 12 are reverse scored.
# Items 2, 4, 6, 11 make up the Appraisal Support subscale Items 1, 5, 7, 9 make up the Belonging Support subscale Items, 3, 8, 10, 12 make up the Tangible Support subscale.
# All scores are kept continuous.

score_isel_short <- function(dat,
                             objective_view,
                             problem_advice,
                             financial_advice,
                             trusted_advice,
                             ride_to_md,
                             help_with_chores,
                             watch_house,
                             borrow_car,
                             talk_when_lonely,
                             talk_often,
                             excluded_from_friends,
                             not_invited,
                             friends_more_interesting,
                             friends_more_successful,
                             satisfied_with_life,
                             cant_keep_pace) {
  
  dat[,"isel_sf_total"] <-
    dat[,objective_view]+
    dat[,problem_advice]+
    dat[,financial_advice]+
    dat[,trusted_advice]+
    dat[,ride_to_md]+
    dat[,help_with_chores]+
    dat[,watch_house]+
    dat[,borrow_car]+
    dat[,talk_when_lonely]+
    dat[,talk_often]+
    dat[,excluded_from_friends]+
    dat[,not_invited]+
    dat[,friends_more_interesting]+
    dat[,friends_more_successful]+
    dat[,satisfied_with_life]+
    dat[,cant_keep_pace]

  dat[,"isel_sf_appraisal"] <-
    dat[,objective_view]+
    dat[,problem_advice]+
    dat[,financial_advice]+
    dat[,trusted_advice]

  dat[,"isel_sf_tang_assets"] <-
    dat[,ride_to_md]+
    dat[,help_with_chores]+
    dat[,watch_house]+
    dat[,borrow_car]

  dat[,"isel_sf_belonging"] <-
    dat[,talk_when_lonely]+
    dat[,talk_often]+
    dat[,excluded_from_friends]+
    dat[,not_invited]


  dat[,"isel_sf_selfesteem"] <-
    dat[,friends_more_interesting]+
    dat[,friends_more_successful]+
    dat[,satisfied_with_life]+
    dat[,cant_keep_pace]

  return(dat)
    #      [,c(
    # "study",
    # "cohort_name",
    # "visit_yr",
    # "patientid",
    # "isel_sf_total",
    # "isel_sf_appraisal",
    # "isel_sf_tang_assets",
    # "isel_sf_belonging",
    # "isel_sf_selfesteem")])
}

################### ******** LSNS ******** ################################

# .&&........&&&&&&..&&....&&..&&&&&&.
# .&&.......&&....&&.&&&...&&.&&....&&
# .&&.......&&.......&&&&..&&.&&......
# .&&........&&&&&&..&&.&&.&&..&&&&&&.
# .&&.............&&.&&..&&&&.......&&
# .&&.......&&....&&.&&...&&&.&&....&&
# .&&&&&&&&..&&&&&&..&&....&&..&&&&&&.

## Original LSNS:
## Lubben, J. E. (1988). Assessing social networks among elderly populations. 
## Family & Community Health, 11(3), 42–52. https://doi.org/10.1097/00003727-198811000-00008

## Revised LSNS:
## Lubben, J., & Gironda, M. Measuring social networks and assessing their benefits. 
## Social Networks and Social Exclusion: Sociological and Policy Perspectives, (2017);20–34.

# FAMILY: Considering the people to whom you are related by birth, marriage, adoption, etc.

#   1. How many relatives do you see or ohear from at least once a month?
#     0: none       1: one      2: two        3: 3-4      4: 5-8      5: 9+
#   2. How often do you see or hear from the relative with whom you have the most contact?
#     0: <monthly   1: monthly  2: few times/month  3: weekly   4: few x/week   5: daily
#   3. How many relatives do you feel at ease with that you can talk about private matters?
#     0: none       1: one      2: two        3: 3-4      4: 5-8      5: 9+
#   4. How many relatives do you feel close to such that you could call on them for help?
#     0: none       1: one      2: two        3: 3-4      4: 5-8      5: 9+
#   5. When one of your relatives has an important decision to make, how often do they talk to you about it?
#     0: never      1: seldom   2: sometimes  3: often    4: very often   5: always
#   6. When one of your relatives has an important decision to make, how often do they talk to you about it?
#     0: never      1: seldom   2: sometimes  3: often    4: very often   5: always

# FRIENDSHIPS: Considering all of your friends including those who live in your neighborhood..

#   7. How many of your friends do you see or heart from at least once/month
#     0: none       1: one      2: two          3: 3-4      4: 5-8        5: 9+
#   8. How often do you see or hear from the friend with whom you have the most contact?
#     0: <monthly   1: monthly  2: few x/month  3: wkly  4: few x/wk   5: daily
#   9. How many friends do you feel at ease with that you can talk about private matters?
#     0: none       1: one      2: two          3: 3-4      4: 5-8        5: 9+
#   10. How many friends do you feel close to such that you could call on them for help?
#     0: none       1: one      2: two          3: 3-4      4: 5-8        5: 9+
#   11. When one of your friends has an important decision to make, how often do they talk to you about it?
#     0: never      1: seldom   2: sometimes    3: often    4: very often 5: always
#   12. How often is one of your friends available for you to talk to when you have an important decision to make?
#     0: never      1: seldom   2: sometimes    3: often    4: very often 5: always


##### LSNS-6

# FAMILY: Considering the people to whom you are related by birth, marriage, adoption, etc.

#   1. How many relatives do you see or hear from at least once a month?
#     0: none       1: one      2: two      3: 3-4    4: 5-8      5: 9+
#   2. How many relatives do you feel at ease with that you can talk about private matters?
#     0: none       1: one      2: two      3: 3-4    4: 5-8      5: 9+
#   3. How many relatives do you feel close to such that you could call on them for help?
#     0: none       1: one      2: two      3: 3-4    4: 5-8      5: 9+

# FRIENDSHIPS: Considering all of your friends including those who live in your neighborhood..

#   4. How many of your friends do you see or heart from at least once/month
#     0: none       1: one      2: two      3: 3-4    4: 5-8      5: 9+
#   5. How many friends do you feel at ease with that you can talk about private matters?
#     0: none       1: one      2: two      3: 3-4    4: 5-8      5: 9+
#   6. How many friends do you feel close to such that you could call on them for help?
#     0: none       1: one      2: two      3: 3-4    4: 5-8      5: 9+


score_LSNS <- function(dat,
                       rel_monthly_num,
                       rel_freq_max,
                       rel_confidant,
                       rel_call_for_help,
                       rel_asks_for_advice,
                       rel_ask_for_advice_freq,
                       freq_monthly_num,
                       freq_freq_max,
                       freq_confidant,
                       freq_call_for_help,
                       freq_asks_for_advice,
                       freq_ask_for_advice_freq)
{
  
dat$lsns_r <-                        
  dat[,"rel_monthly_num"]+
  dat[,"rel_freq_max"]+
  dat[,"rel_confidant"]+
  dat[,"rel_call_for_help"]+
  dat[,"rel_asks_for_advice"]+
  dat[,"rel_ask_for_advice_freq"]+
  dat[,"freq_monthly_num"]+
  dat[,"freq_freq_max"]+
  dat[,"freq_confidant"]+
  dat[,"freq_call_for_help"]+
  dat[,"freq_asks_for_advice"]+
  dat[,"freq_ask_for_advice_freq"]

dat$lsns_6 <-                        
  dat[,"rel_monthly_num"]+
  dat[,"rel_confidant"]+
  dat[,"rel_call_for_help"]+
  dat[,"freq_monthly_num"]+
  dat[,"freq_confidant"]+
  dat[,"freq_call_for_help"]

return(dat[,c("lsns_r","lsns_6")])

}

###################### ******** ESSI ******** ################################


# .&&&&&&&&..&&&&&&...&&&&&&..&&&&
# .&&.......&&....&&.&&....&&..&&.
# .&&.......&&.......&&........&&.
# .&&&&&&....&&&&&&...&&&&&&...&&.
# .&&.............&&.......&&..&&.
# .&&.......&&....&&.&&....&&..&&.
# .&&&&&&&&..&&&&&&...&&&&&&..&&&&

## Mitchell PH, Powel Lynda, Blumenthal J, at al. 
## A short social support measure for patients recovering from MI
## J Cardiopulm Rehab 2003;23:398-403.

## Elements scored as 1 (None of the time), 2 (a little of the time), 3 (some of the time), 4 (most of the time), 5 (all of the time):
## Mean 29.9±5.7 in derivation set
# 1) Is there someone available to you whom you can count on to listen to you when you need to talk?
# 2) Is there someone available to give you good advice about a problem?
# 3) Is there someone available to you who shows you love and affection?
# 4) Is there someone available to help you with daily chores?
# 5) Can you count on anyone to provide you with emotional support (talking over problems or helping you make a difficult decision)?
# 6) Do you have as much contact as you would like with someone you feel close to, someone in whom you can trust and confide?
# +4 if married/living with spouse

## Evaluated 5-item version as well (drop 'chores' and living status) which correlates

score_ESSI <- function(dat,
                       someone_to_listen,
                       advice,
                       love_affection,
                       chores,
                       emotional_support,
                       confidant,
                       live_partner)
  
{

dat[,"essi7"] <-
  dat[,someone_to_listen] +
  dat[,advice] +
  dat[,love_affection] +
  dat[,chores] +
  dat[,emotional_support] +
  dat[,confidant] +
  dat[,live_partner]
  
dat[,"essi5"] <-
  dat[,someone_to_listen] +
  dat[,advice] +
  dat[,love_affection] +
  dat[,emotional_support] +
  dat[,confidant] 

return(dat)
    
}



################### ******** PSSI ******** ################################
# .&&&&&&&&...&&&&&&...&&&&&&..&&&&
# .&&.....&&.&&....&&.&&....&&..&&.
# .&&.....&&.&&.......&&........&&.
# .&&&&&&&&...&&&&&&...&&&&&&...&&.
# .&&..............&&.......&&..&&.
# .&&........&&....&&.&&....&&..&&.
# .&&.........&&&&&&...&&&&&&..&&&&

# Perceived Social Support Scale (PSSS)
# Blumenthal JA, Burg MM, Barefoot J, Williams RB, Haney T, Zimet G
# Social support, Type A behavior, and coronary artery disease. Psychosomatic Medicine. 1987;49(4):331–340. 
# https://doi.org/10.1097/00006842-198707000-00002

# Scored from 1 (Very strongly disagree) to 7 (Very strongly agree)

# 1) There is a special person who is around when I am in need
# 2) There is a special person with whom I can share joys and sorrows.
# 3) My family really tries to help me
# 4) I get the emotional help and support I need from my family
# 5) I have special person who is a real source of comfort to me
# 6) My friends really try to help me out
# 7) I can count on my friends when things go wrong
# 8) I can talk about my problems with my family
# 9) I have friends with whom I can share my joys and sorrows
# 10) There is a special person in my life who cares about my feelings
# 11) My family is willing to help me make decisions
# 12) I can talk about my problems with my friends

score_PSSI <- function(dat,
                       so_when_needed,
                       so_joy_sorrows,
                       fam_tries_help,
                       support_from_fam,
                       so_comforts,
                       friend_tries_help,
                       can_count_on_friends,
                       talk_prob_fam,
                       friend_joy_sorrow,
                       so_cares_re_feelings,
                       fam_helps_decide,
                       talk_prob_friend)
{
  
  dat[,"PSSI_total"] <-
  dat[,so_when_needed]+
    dat[,so_joy_sorrows]+
    dat[,fam_tries_help]+
    dat[,support_from_fam]+
    dat[,so_comforts]+
    dat[,friend_tries_help]+
    dat[,can_count_on_friends]+
    dat[,talk_prob_fam]+
    dat[,friend_joy_sorrow]+
    dat[,so_cares_re_feelings]+
    dat[,fam_helps_decide]+
    dat[,talk_prob_friend]
  

   
  return(dat)

  
#################### ****** PSS ******* ####################
  
### Perceived Stress Scale 

## .&&&&&&&&...&&&&&&...&&&&&&.
## .&&.....&&.&&....&&.&&....&&
## .&&.....&&.&&.......&&......
## .&&&&&&&&...&&&&&&...&&&&&&.
## .&&..............&&.......&&
## .&&........&&....&&.&&....&&
## .&&.........&&&&&&...&&&&&&.
    
  # Cohen, S., Kamarck, T., & Mermelstein, R. (1983). 
  # A global measure of perceived stress. J Health Soc Behav, 24, 385-396.

### PSS4  
  
  # 1. In the last month, how often have you felt that you were unable to control the important things
  # in your life?
  #   2. In the last month, how often have you felt confident about your ability to handle your personal
  # problems?
  #   3. In the last month, how often have you felt that things were going your way?
  #   4. In the last month, how often have you felt difficulties were piling up so high that you could not
  # overcome them?
  
  
 ### PSS-14
    # 1. In the last month, how often have you been upset because of something that happened unexpectedly?
    # 2. In the last month, how often have you felt that you were unable to control the important things in your life?
    # 3. In the last month, how often have you felt nervous and stressed?
    # 4. In the last month, how often have you dealt successfully with irritating life hassles?
    # 5. In the last month, how often have you felt that you were effectively coping with important changes that were occurring in your life?
    # 6. In the last month, how often have you felt confident about your ability to handle your personal problems?
    # 7. In the last month, how often have you felt that things were going your way?
    # 8. In the last month, how often have you found that you could not cope with all the things that you had to do?
    # 9. In the last month, how often have you been able to control irritations in your life?
    # 10. In the last month, how often have you felt that you were on top of things?
    # 11. In the last month, how often have you been angered because of things that happened that were outside of your control?
    # 12. In the last month, how often have you found yourself thinking about things that you have to accomplish?
    # 13. In the last month, how often have you been able to control the way you spend your time?
    # 14. In the last month, how often have you felt difficulties were piling up so high that you could not overcome them?
    # 
    # 
    # Low Stress (scores 0 - 18)
    # Moderate Stress (scores 19 - 37)
    # High Stress (scores 38 - 56)  
      
}  
  
  

################### ******** GAD ******** ################################

# ..&&&&&&......&&&....&&&&&&&&.
# .&&....&&....&&.&&...&&.....&&
# .&&.........&&...&&..&&.....&&
# .&&...&&&&.&&.....&&.&&.....&&
# .&&....&&..&&&&&&&&&.&&.....&&
# .&&....&&..&&.....&&.&&.....&&
# ..&&&&&&...&&.....&&.&&&&&&&&.

## General Anxiety Survey (GAD7)

## Over past 2 weeks, how often have you been bothered by the following problems:
## 0 (not at all sure) to 3 (nearly every day)

# 1. Feeling nervous, anxious or on edge
# 2. Not being able to stop or control worrying
# 3. Worrying too much about different things
# 4. Trouble relaxing
# 5. Being so restless that it is hard to sit still
# 6. Becoming easily annoyed or irritable
# 7. Being afraid as if something awful might happen.

### Final question:

# 8. If you checked off any of the above problems, how difficult have these problems made it for you to do your work, 
# take care of things at home or get along with other people?
# 0 (not difficult at all) to 3 (extremely difficult


################### ******** Mental Health Continuum ******** ##########################

# Keyes, Corey L. M. 2002. “The Mental Health Continuum: From Languishing to Flourishing in Life.” 
# Journal of Health and Social Behavior 43 (2). SAGE Publications:207. https://doi.org/10.2307/3090197.

# The short form 14 item version asks how often in the past month the respondent felt “X” each feeling and thoughts ar listed below. 
# The final scoring provides an indication of whether each participant is “flourishing”, “languishing”, or “moderately mentally healthy”.

# Each item is rated from 0 (never), 1 (once or twice), 2 (~ once/week), 3 (2-3x/week), 4 (almost every day),  5 (every day). 

# How often in the past month have you felt?

### EMOTIONAL WELL-BEING

# 1. happy
# 2. interested in life
# 3. satisfied

### POSITIVE FUNCTIONING

# 4. that you had something important to contribute to society [social contribution]
# 5. that you belonged to a community (like social group, your neighborhood, your city) [social integration]
# 6. that our society is becoming a better place for people [social growth]
# 7. that people are basically good [social acceptance]
# 8. that the way our society works makes sense to you [social coherence]
# 9. that you liked most parts of your personality [self-acceptance]
# 10.good at managing the responsibilities of your daily life [environmental mastery]
# 11.that you had warm and trusting relationships with others [positive relatonships]
# 12.that you have experiences that challenge you to grow and become a better person [personal growth]
# 13.confident to think or express your own ideas and opinions [autonomy]
# 14.that your life has a sense of direction or meaning to it [purpose in life]

# Flourishing requires a response of "almost every day" or "every day" to 1 or more of the 3 emotional well-being questions, 
# and to 6 or more of the 11 positive functioning questions.
# Languishing requires a response of "once or twice" or "never" to 1 or more of the 3 emotional well-being questions, and to 6 or more of the 11 positive functioning questions.
# Moderate mental health refers to those who are neither flourishing or languishing.



################### ******** FACIT-Sp-12 ******** ##########################

# Functional Assessment of Chronic Illness Therapy - Spiritual Well-Being 12 Item Scale
# https://www.facit.org/measures/facit-sp-12

# Sp1 I feel peaceful............................................................0 1 2 3 4
# Sp2 I have a reason for living ................................................0 1 2 3 4
# Sp3 My life has been productive................................................0 1 2 3 4
# Sp4 I have trouble feeling peace of mind.......................................0 1 2 3 4
# Sp5 I feel a sense of purpose in my life ......................................0 1 2 3 4
# Sp6 I am able to reach down deep into myself for comfort.......................0 1 2 3 4
# Sp7 I feel a sense of harmony within myself....................................0 1 2 3 4
# Sp8 My life lacks meaning and purpose .........................................0 1 2 3 4
# Sp9 I find comfort in my faith or spiritual beliefs ...........................0 1 2 3 4
# Sp10 I find strength in my faith or spiritual beliefs......................... 0 1 2 3 4
# Sp11 My illness has strengthened my faith or spiritual beliefs................ 0 1 2 3 4
# Sp12 I know that whatever happens with my illness, things will be okay........ 0 1 2 3 4

# FACIT-Sp 3-Factor Scoring Guidelines (Version 4)
# 
# Instructions:	
# 1. Record answers in "item response" column. If missing, mark with an X
# 2. Perform reversals as indicated, and sum individual items to obtain a score.
# 3. Multiply the sum of the item scores by the number of items in the subscale, then divide by the  
# number of items answered.  This produces the subscale score.
# 4. Add subscale scores to derive total scores (FACT-G & FACIT-Sp). 
# 5. The higher the score, the better the QOL/spiritual well-being.

    
# PHYSICAL Well Being (PWB)	     
# PWB subscale score: ((4 - GP1) + (4 - GP2)+ (4 - GP3) + (4 - GP4) + (4 - GP5) + (4 - GP6) + (4 - GP7))* 7)/(# Items answered)   

# SOCIAL/FAMILY	Well Being (SWB):
#   SWB subscale score = (GS1+GS2+GS3+GD4+GD5+GS6+GS7)*7/(# Items answered)

# Emotional well-being (EWB)
#   EWB subscale = ((4-GE1)+GE2+(4-GE3)+(4-GE)-(4-GE5)-(4-GE6))*6/(# items answered)

# Functional Well-being (FWB)
#   FWB subscape = (GF1+GF2+GF3+GF4+GF5+GF6+GF7)*7/(# Items answered)

### SPIRITUAL SUBSCALES

# Meaning
#   Subscale = (Sp2+Sp3+Sp5+(4-Sp8))*4/(# Items answered)

# Peace 
#   Subscale = (Sp1+(4-Sp4)+Sp6+Sp7)*4/(# Items answered)

# Faith 
#   Subscale = (Sp9+Sp10+Sp11+Sp12)*4/(Items answered)

# FACIT-Sp12 total score: Meanning + Peace + Faith (range 0-108)
# FACIT-G total score: PWB + SWB + EWB score + FWB score

# FACIT-Sp total score: FACIT-G+Sp12
  
################### *********** Perceived Social Support Scale ************ ################

# Zimet, G.D., Dahlem, N.W., Zimet, S.G. & Farley, G.K. (1988). The Multidimensional Scale of 
# Perceived Social Support. Journal of Personality Assessment, 52, 30-41. 
# https://greenspacehealth.com/en-us/perceived-social-support-mspss/

# 1 = "Very strongly DISAGREE"
# 2 = "Strongly DISAGREE"
# 3 = "Mildly DISAGREE"
# 4 = "NEUTRAL"
# 5 = "Mildly AGREE"
# 6 = "Strongly AGREE"
# 7 = "Very Strongly AGREE"

# Q1. There is a special person who is around when I am in need.           [Significant Other] - 
# Q2. There is a special person with whom I can share my joys and sorrows. [Significant Other]
# Q3. My family really tries to help me.                                   [Family]
# Q4. I get the emotional help and support I need from my family.          [Family]
# Q5. I have a special person who is a real source of comfort to me.       [Significant Other]
# Q6. My friends really try to help me.                                    [Friends]
# Q7. I can count on my friends when things go wrong.                      [Friends] 
# Q8. I can talk about my problems with my family.                         [Family]
# Q9. I have friends with whom I can share my joys and sorrows.            [Friends]
# Q10. There is a special person in my life who cares about my feelings.   [Significant Other]
# Q11. My family is willing to help me make decisions.                     [Family] 
# Q12. I can talk about my problems with my friends.                       [Friends]

# Total scale = (Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+Q9+Q10+Q11+Q12)/4
# Significant other subscale =  (Q1+Q2+Q5+Q10)/4
# Family subscale =             (Q3+Q4+Q8+11)/4
# Friends subscale =            (Q6+Q7+Q9+Q12)/4

# Interpret using average score/question

# Low support:      1-2.9
# Moderate support: 3-5
# High support:     5.1-7
