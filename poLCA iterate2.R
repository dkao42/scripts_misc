
# from DK

library(reshape2)
library(poLCA)
library(plotrix)
library(depmixS4)

poLCA_iterate <- function (f,dat,maxclass=10,reps=10,na.remove=FALSE) {
  
  ## Assumes f is a formula expression passed formatted for poLCA.
  
  lca_error <- matrix(nrow=maxclass-1,ncol=6,dimnames=list(c(2:maxclass),c('AIC','BIC','X2','G2','cAIC','aBIC')))
  for (i in 2:maxclass) {
    this_iter <- poLCA(f,dat,nclass=i,nrep=reps,na.rm=na.remove,verbose=FALSE)
    write(this_iter$aic)
    lca_error[as.character(i),'AIC'] <- this_iter$aic
    lca_error[as.character(i),'BIC'] <- this_iter$bic
    lca_error[as.character(i),'X2'] <- this_iter$Chisq
    lca_error[as.character(i),'G2'] <- this_iter$Gsq
    lca_error[as.character(i),'cAIC'] <- -2*this_iter$llik+this_iter$npar*log(this_iter$N)+1
    lca_error[as.character(i),'aBIC'] <- -2*this_iter$llik+this_iter$npar*log((this_iter$N+2)/24)
    cat("Completed",i,"class analysis.\n")
  }                      
  print(lca_error)
  par(mfrow=c(3,2))
  plot(x=2:maxclass,y=lca_error[as.character(2:maxclass),'AIC'],col='blue',xlab='Latent classes', 
       type='b',pch=1,ylab='AIC',main='AIC')
  plot(x=2:maxclass,y=lca_error[as.character(2:maxclass),'BIC'],col='blue',xlab='Latent classes', 
       type='b',pch=1,ylab='BIC',main='BIC')
  plot(x=2:maxclass,y=lca_error[as.character(2:maxclass),'X2'],col='blue',xlab='Latent classes', 
             type='b',pch=1,ylab='X2',main='X2')
  plot(x=2:maxclass,y=lca_error[as.character(2:maxclass),'G2'],col='blue',xlab='Latent classes', 
       type='b',pch=1,ylab='G2',main='G2')
  plot(x=2:maxclass,y=lca_error[as.character(2:maxclass),'cAIC'],col='blue',xlab='Latent classes', 
       type='b',pch=1,ylab='cAIC',main='cAIC')
  plot(x=2:maxclass,y=lca_error[as.character(2:maxclass),'aBIC'],col='blue',xlab='Latent classes', 
       type='b',pch=1,ylab='aBIC',main='aBIC')
  return(lca_error)
}

## Function to export a table that will support LC application in SQL using 'lca_application.sql'

write_poLCA_coeff_tab <- function(df,pl,mname) {
  library(reshape2)
  problist <- names(df$probs)
  full_list <- data.frame(latent_class = NA, brief_name = NA, variable=NA,value=NA,stringsAsFactors=F)
  thispop <- as.data.frame(cbind(df$P))
  colnames(thispop)[1] <- 'share'
  for (i in 1:length(problist)) {
    thisslot <- df['probs'][[1]]
    thismatrix <- thisslot[[i]]
    for (j in 1:ncol(thismatrix)) {
      colnames(thismatrix)[j] <- as.character(j)
    }
    thisdf <- as.data.frame(thismatrix)
    thisdf$brief_name=problist[i]
    for (k in 1:nrow(thisdf)) {
      thisdf[k,'latent_class'] <- paste("Class",LETTERS[k])
      thispop[k,'latent_class'] <- paste("Class",LETTERS[k])
    }
    full_list <- rbind(full_list,melt(thisdf,id.vars=c('latent_class','brief_name')))
  }
  full_list <- full_list[2:nrow(full_list),] 
  names(full_list)[3] <- 'var.value'
  names(full_list)[4] <- 'coeff'
  full_list$model_name <- mname
  thispop$model_name <- mname
  full_list <- merge(full_list,pl[,c('brief_name','phenotype')],by='brief_name',all.x=T)
  return(full_list)
  write.csv(full_list,paste(mname,'_lca_coeff.csv',sep=''),row.names = F)
  write.csv(thispop,paste(mname,'_popshare.csv',sep=''),row.names = F)
}

## Function to return modal class probability using poLCA model (df1) and data used to make model (dat)

poLCA_find_modal_prob <- function (df1,dat) {
  df <- cbind(df1$posterior,cbind(df1$predclass))
  endcol <- ncol(df)
  modal_prob <-vector()
  colnames(df)[endcol] <- 'modal_class'
  for (h in 1:(endcol-1)){
    colnames(df)[h] <- paste("Class",h,sep="_")
  }
  for (i in 1:nrow(df)) {
    thismodalclass <- df[i,endcol]
    modal_prob <- c(modal_prob,df[i,thismodalclass])  
  }
  df <- cbind(df,cbind(modal_prob))
  x <- cbind(dat,df)
  return(x)
}

plot_LCA_outcomes <- function(time,cens,cens_val=1,lca_class,mname,outc_name,xlo=0,xhi=60,ylo=0,yhi=1) {
  if (dir.exists(mname)==F) {dir.create(mname)}
  pdf(paste(mname,"/LCA Model ",modelname," ",outc_name," according to class",".pdf",sep=""))
  plot(survfit(Surv(as.numeric(time)/30,cens==cens_val)~as.factor(lca_class),conf.type="none"),col=c(1:14),xlim=c(xlo,xhi),ylim=c(ylo,yhi))
  title(main=paste('Primary outcome according to LCA model:',modelname),xlab='Time since enrollment (months)',ylab='Proportion event free')
  dev.off()
}

### Shows formulae for abic, caic

##results <- data.frame(Modell=c("Modell 1"),
##                      log_likelihood=lc1$llik,
##                      df = lc1$resid.df,
##                    BIC=lc1$bic,
##                    ABIC=  (-2*lc2$llik) + ((log((lc2$N + 2)/24)) * lc2$npar),
##                    CAIC = (-2*lc1$llik) + lc1$npar * (1 + log(lc1$N)), 
##                    likelihood_ratio=lc1$Gsq)


depmixS4_iterate <- function (f,typ,dat,maxclass=10,reps=10,rspstrt=15) {
  
  ## f assumes list of manifest variables, e.g. list(age_cat~1,sex~1,race~1,cad~1,af~1,dm~1)
  ## typ assumes list of corresponding distribution types, e.g. list(multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity"))
  depmixS4_error <- matrix(nrow=maxclass-1,ncol=4,dimnames=list(c(2:maxclass),c('AIC','BIC','cAIC','aBIC')))
  for (i in 2:maxclass) {
    this_rspstart <- rspstrt * (i)
    this_iter <-   fit(mix(response=f,
                       data=dat,
                       nstates=i,
                       family=typ,
                       respstart=runif(this_rspstart)))
    ss <- logLik(this_iter)[1]
    depmixS4_error[as.character(i),'AIC'] <- AIC(this_iter)
    depmixS4_error[as.character(i),'BIC'] <- BIC(this_iter)
    depmixS4_error[as.character(i),'cAIC'] <- -2*(ss)+npar(this_iter)*log(nobs(this_iter))+1
    depmixS4_error[as.character(i),'aBIC'] <- -2*(ss)+npar(this_iter)*log((nobs(this_iter)+2)/24)
    cat("Completed",i,"class analysis.\n")
  }                      
  par(mfrow=c(3,2))
  plot(x=2:maxclass,y=depmixS4_error[as.character(2:maxclass),'AIC'],col='blue',xlab='Latent classes', 
       type='b',pch=1,ylab='AIC',main='AIC')
  plot(x=2:maxclass,y=depmixS4_error[as.character(2:maxclass),'BIC'],col='blue',xlab='Latent classes', 
       type='b',pch=1,ylab='BIC',main='BIC')
  plot(x=2:maxclass,y=depmixS4_error[as.character(2:maxclass),'cAIC'],col='blue',xlab='Latent classes', 
       type='b',pch=1,ylab='cAIC',main='cAIC')
  plot(x=2:maxclass,y=depmixS4_error[as.character(2:maxclass),'aBIC'],col='blue',xlab='Latent classes', 
       type='b',pch=1,ylab='aBIC',main='aBIC')
  return(depmixS4_error)
}




# loop over all cells in data frame()
# In new dataframe, replace variable value with lca coefficient corresponding to value.  
# Need separate frame for each LC or loop across latent classes and store together 
# Replace all NAs in new data frame with 1 (identify)
# Product (prod()) across rows to create intermediate probability for each subject for each class (terminal row)
# Combine intermediate probs into single dataframe (intermediate probs, posterior prob, final prob)
# Merge in posterior probability to new column from lcamodel$posterior
# 
# Output matrix with all class probabilities, modal probability, class corresponding to modal probability.

# df <- hdcp_data_ep[,c('patientid','study',thisrun_variables)]
# clses <- unique(lca_coeff$latent_class)
# df_intermediate <- as.data.frame(matrix(nrow=nrow(df),ncol=ncol(df)))
# colnames(df2) <- colnames(df)
# df2[,c('patientid','study')] <- df[,c('patientid','study')]
# 
# for (m in clses) {
#  for (i in 1:nrow(df)){
#   for (j in thisrun_variables) {
#     if (!is.na(df[i,j]==lca_coeff$var.value)) {
#     this_coeff_val <- lca_coeff$coeff[lca_coeff$brief_name==j&
#                                         df[i,j]==lca_coeff$var.value&
#                                         lca_coeff$latent_class==m]
#     df2[i,j] <- this_coeff_val
#     }
#     else if (is.na(df[i,j])) {
#       df2[i,j] <- 1
#     }
#   }
#  }
# }


apply_lca_model <- function (df,apply_model,lca_coeff,prepost='post') {

### This function will apply a poLCA-generated model to any dataset (df)
### Requires a poLCA model (apply_model) and a melted coefficient table produced by write_poLCA_coeff_tab (lca_coeff)
### Generates 2 tables, 1 using variable values only (df_final_pre) and 1 adding population share (df_final_post)
### The poLCA function uses the modal posterior class probability to classify, so by default, function returns df_final_post
### lca_coeff is a table produced by above function "write_poLCA_coeff_tab" applied to a poLCA model
### Assumes that all coefficients in model are present in df
### Will classify all rows in df, even with missing data

### Extract variables and create df subset of just those columns
    
these_vars <- lca_coeff$brief_name
df <- df[,c('patientid','study',these_vars)]

### Extract unique 

clses <- unique(lca_coeff$latent_class)

### Create melted table from df subset.  This makes matching values to coefficients much easierl

df_melt <- melt(df,id.vars=c('patientid','study'))
df_melt_merge <- merge(df_melt,
                       lca_coeff,
                       by.x=c('variable',
                              'value'),
                       by.y=c('brief_name',
                              'var.value'),
                       all.x=T)

### Extract population shares from 

df_popshare <- apply_model$P
names(df_popshare) <- paste("Class",
                            LETTERS[1:length(df_popshare)])

### Create first intermediate table of pre-probability coefficients corresponding to variables

df_sum <- df[,c('patientid',
               'study')]

### Multiple all variable (product) coefficients for each class

for (pp in clses) {
  temp_coeffs <- dcast(subset(df_melt_merge,
                              latent_class==pp),
                       patientid+study ~ variable,
                       value.var="coeff")
  
  temp_coeffs[is.na(temp_coeffs)] <- 1
  for (qq in 1:nrow(temp_coeffs)) {
    temp_coeffs[qq,pp] <- prod(temp_coeffs[qq,these_vars])
  }
  df_sum <- merge(df_sum,
                  temp_coeffs[,
                              c('patientid',
                                'study',
                                pp)])
}

### Use pre- coefficients * class-specific population share table

df_sum2 <- df_sum[c('patientid',
                    'study')]

for (pp in clses) {
  df_sum2[,pp] <- df_sum[,pp] * 
    df_popshare[pp]
}


### Calculate sums of all Class probabilities for each individual (i.e. Class A + Class B + Class C....) 

for (ss in 1:nrow(df_sum)) {
  df_sum[ss,'pre_sum'] <- sum(df_sum[ss,clses])
  df_sum2[ss,'post_sum'] <- sum(df_sum2[ss,clses])
}

### Create final tables which contain patientid,study,pre- and post-probabilities of each class, modal (highest) probability and corresponding modal class
### Checked against 8000 individuals with classifications using poLCA model with 100% accuracy

df_final_pre <- df_sum[,c('patientid','study')]
df_final_post <- df_sum[,c('patientid','study')]

for (ss in clses) {
  for (rr in 1:nrow(df_sum)) {
    df_final_pre[rr,ss] <- df_sum[rr,ss]/df_sum[rr,'pre_sum']
    df_final_post[rr,ss] <- df_sum2[rr,ss]/df_sum2[rr,'post_sum']
  }
}

for (ee in 1:nrow(df_sum)) {
  df_final_pre[ee,'modal'] <- max(df_final_pre[ee,clses])
  df_final_post[ee,'modal'] <- max(df_final_post[ee,clses])
}

### Extract name of Class with modal probability

for (tt in 1:nrow(df_final_pre)){
  for (uu in clses) {
    if (df_final_pre[tt,uu]==df_final_pre[tt,'modal']) {
      df_final_pre[tt,'modal_class'] <- uu
    }
    if (df_final_post[tt,uu]==df_final_post[tt,'modal']) {
      df_final_post[tt,'modal_class'] <- uu
    }
  }
}

### Return pre- vs. posterior probabilities
### Defaults to posterior.

if (prepost == 'post') {
  return(df_final_post)
}
if (prepost == 'pre') {
  return(df_final_pre)
}

}