library(survival)
library(ggforestplot)
library(sqldf)
library(plot.matrix)
library(ggforce)


################################################################################################################

plot_study_covar_survival <- function (alldata, 
                                       covars, 
                                       covarlabels, 
                                       studies, 
                                       outcome_status,
                                       outcome_days,
                                       outcome_label,
                                       clrs=c('black',
                                              'blue',
                                              'red',
                                              'orange',
                                              'violet',
                                              'darkgreen',
                                              'grey'))
## This function will create a series of PDFs in the working directory containing KM curves of the specified outcome which display 
## a) relative survival/rates of outcome for each study overall and b) separate plots comparing values of each of the 'covars', e.g. sex, race, etc. for each study 
## The function will automatically generate PDFs using the 'covar labels' and 'outcome label' in the name of the PDF.  e.g., covar might = "DM" where covarlabel = "Diabetes mellitus" 
## Plots will end up in the current working directory
{
  for (h in 1:length(covars)) {
    print(covars[h])
    # pdf(paste(outcome_label,
    #           " according to ", 
    #           covarlabels[h],
    #           " all studies.pdf",sep=""))
    plot(
      survfit(
        Surv(
          alldata[,outcome_days]/30,
          alldata[,outcome_status])~
          alldata[,covars[h]],
        conf.type="none"),
      col=clrs,
      xlim=c(0,42),
      ylim=c(0.5,1))
    
    title(main=paste(outcome_label,
                     ' according to',
                     covarlabels[h],
                     '\n'),
          xlab='Time since enrollment (months)',
          ylab='Proportion event free')
    # dev.off()
    for (i in 1:length(studies)) {
      print(studies[i])
      # pdf(paste(outcome_label," according to ", covarlabels[h]," in ",studies[i],".pdf",sep=""))
      plot(survfit(Surv(alldata[alldata$study==studies[i],outcome_days]/30,alldata[alldata$study==studies[i],outcome_status])~alldata[alldata$study==studies[i],covars[h]],conf.type="none"),lty=c(1),col=clrs,xlim=c(0,42),ylim=c(0.5,1))
      title(main=paste(outcome_label,' according to',covarlabels[h],'\n',studies[i]),xlab='Time since enrollment (months)',ylab='Proportion event free')
      # dev.off()
    }
  }
}  


################################################################################################################




plot_available_variables <- function(var_list,
                                     stds=c('BEST',
                                            'TOPCAT',
                                            'RELAX',
                                            'HF-ACTION',
                                            'IPRESERVE',
                                            'DIG',
                                            'SOLVD Treat',
                                            'SOLVD Prevent',
                                            'SCD-HeFT',
                                            'MOCHA',
                                            'NEAT-HFpEF',
                                            'STICH',
                                            'EXACT',
                                            'GUIDE-IT',
                                            'DOSE',
                                            'CARRESS',
                                            'ESCAPE',
                                            'ROSE',
                                            'RELAX'),
                                     num_thresh=8,
                                     plot_name='Var availability')
{

## This function will produce a grid plot for variables that appear in each study of a list of studies.
## The input 'var_list' is a data frame containing unique pairs of study-brief_name.  This is then transformed into a matrix of study x brief name where 1 = variable is present in that study
## It will also count the number of studies a given variable appears in and only display those above a certain number threshhold if (set by 'num_thresh' parameter)


  all_studies <- unique(var_list$study)
  all_fields <- unique(var_list$brief_name)

  avail_field <- matrix(nrow=length(all_fields),
                        ncol=length(all_studies),
                        dimnames=list(c(all_fields),
                                      c(all_studies)))

  query_studies <- data.frame(study=stds,stringsAsFactors = F)

  for (i in 1:nrow(var_list)) {
    thisstudy <- var_list[i,'study']
    thisfield <- var_list[i,'brief_name']
    avail_field[thisfield,thisstudy] <- 1
  }

  var_rank <- sqldf('select brief_name,count(distinct study) as num from var_list where study in (select study from query_studies) group by brief_name order by num desc')

  pdf(paste(plot_name,'.pdf',sep=''))
  plot(avail_field[var_rank$brief_name[var_rank$num>=num_thresh],
                   query_studies$study],
       border=NA,
       key=NULL,
       las=2,
       xlab=NA,
       ylab=NA,
       main="Available variables",
       cex.axis=0.5)
  dev.off()
}





multistudy_forest_ggplot_log_logit <- function (alldata, 
                                          covars, 
                                          covarlabels, 
                                          studies, 
                                          outcome_status,
                                          outcome_label, 
                                          fpfldr = "",
                                          in_line=T,
                                          outpt=F)
  
{
  wd <- getwd()
  for (i in 1:length(covars)) {
    thiscovar <- covars[i]
    thislabel <- covarlabels[i]
    print(outcome_label)
    print(thislabel)
    mtx1 <- matrix(
      nrow=length(studies),
      ncol=4,
      dimnames=list(c(studies),
                    c("lo",
                      "or",
                      "hi",
                      "p")))
    mtx2 <- matrix(
      nrow=length(studies),
      ncol=3,
      dimnames=list(c(studies),
                    c("coef",
                      "se",
                      "p")))
    
    for (s in 1:length(studies)) {
      thisstudy <- studies[s]
      thisglm <- glm(alldata[alldata[,'study']==thisstudy,outcome_status]~
                       alldata[alldata[,'study']==thisstudy,thiscovar],
                     family=binomial(link="logit"))
      sumglm <- as.data.frame(summary(thisglm)$coefficients)
      coefglm <- as.data.frame(confint(thisglm))
      sumglm <- cbind(sumglm,coefglm)

      mtx1[thisstudy,"or"] <-round(exp(sumglm[2,'Estimate']),digits=2)
      mtx1[thisstudy,"lo"] <- round(exp(sumglm[2,'2.5 %']),digits=2)
      mtx1[thisstudy,"hi"] <- round(exp(sumglm[2,'97.5 %']),digits=2)
      mtx1[thisstudy,"p"] <- round(sumglm[2,'Pr(>|z|)'],digits=4)


      mtx2[thisstudy,"coef"] <- sumglm[2,'Estimate']
      mtx2[thisstudy,"se"] <- sumglm[2,'Std. Error']
      mtx2[thisstudy,"p"] <- round(sumcox$coefficients[,5],digits=4)
    }

    mtx2 <- as.data.frame(mtx2)
    mtx2$study_name <- rownames(mtx2)

    print(mtx1)
    print(mtx2)

    p <- print(ggforestplot::forestplot(
      df=mtx2,
      name=study_name,
      estimate=coef,
      logodds=T,
      xlab="Hazard ratio",
      pvalue=p,
      xtickbreaks=c(0.25,0.5,0.75,1,1.5,2),
      xlim=c(0.25,2.5)
    ) + geom_point(size=5,shape=18) 
    )
    
    
    if (in_line==T) {print(p)}

    if (outpt==T) {
      pdf(paste(wd,"/",fpfldr,"/",outcome_label," according to ", thislabel,".pdf",sep=""),width=w_pdf)
      print(p)
      dev.off()
    }
  }
}





multistudy_forest_ggplot_log_sig <- function (alldata, 
                                            covar, 
                                            covarlabel,
                                            map,
                                            varlist,
                                            studies, 
                                            outcome_status,
                                            outcome_days,
                                            outcome_label, 
                                            fpfldr = "",
                                            in_line=T,
                                            outpt=F,
                                            point_shape=18,
                                            point_size=5,
                                            xtckbrks=c(0.25,0.5,1,2,4),
                                            xlm=c(0.25,4.2),
                                            sig.only=T,
                                            hr_table=T,
                                            tbl_hdr_theme=c("default",
                                                          "blank",
                                                          "classic",
                                                          "minimal",
                                                          "light",
                                                          "lBlack", 
                                                          "lBlue", 
                                                          "lRed", 
                                                          "lGreen",
                                                          "lViolet",
                                                          "lCyan",
                                                          "lOrange",
                                                          "lBlackWhite",
                                                          "lBlueWhite",
                                                          "lRedWhite",
                                                          "lGreenWhite",
                                                          "lVioletWhite", 
                                                          "lCyanWhite", 
                                                          "lOrangeWhite", 
                                                          "mBlack", 
                                                          "mBlue", 
                                                          "mRed", 
                                                          "mGreen", 
                                                          "mViolet", 
                                                          "mCyan", 
                                                          "mOrange", 
                                                          "mBlackWhite", 
                                                          "mBlueWhite", 
                                                          "mRedWhite", 
                                                          "mGreenWhite", 
                                                          "mVioletWhite", 
                                                          "mCyanWhite", 
                                                          "mOrangeWhite" ))

{
  mtxlength <- 0
  for (i in studies) {
    studyrows <- (length(unique(alldata[alldata$study==i,covar]))-1)
    mtxlength <- mtxlength + studyrows
  }


  mtx1 <- matrix(
    nrow=mtxlength,
    ncol=7)
  colnames(mtx1)=
    c("lo",
      "hr",
      "hi",
      "p",
      "study",
      "brief_name",
      "val")
  
  mtx2 <- matrix(
    nrow=mtxlength,
    ncol=6)
  colnames(mtx2)=c("coef",
                   "se",
                   "p",
                   "study",
                   "brief_name",
                   "val")
  mtxrows <- 0
  
  
  for (s in studies) {
    num_values <- length(unique(alldata[alldata$study==s,covar]))
    thiscox <- coxph(
      Surv(
        alldata[alldata$study==s,outcome_days]/30,
        alldata[alldata$study==s,outcome_status])~
        as.factor(alldata[alldata$study==s,covar]))
    sumcox <- summary(thiscox)
    
    for (v in 1:(num_values-1)) {
      mtxrows <- mtxrows+1
      mtx1[mtxrows,"hr"] <- round(sumcox$conf.int[v,1],digits=2)
      mtx1[mtxrows,"lo"] <- round(sumcox$conf.int[v,3],digits=2)
      mtx1[mtxrows,"hi"] <- round(sumcox$conf.int[v,4],digits=2)
      mtx1[mtxrows,"p"] <-  sumcox$coefficients[v,"Pr(>|z|)"]
      mtx1[mtxrows,"study"] <- s
      mtx1[mtxrows,"brief_name"] <- covar
      mtx1[mtxrows,"val"] <- thiscox$xlevels[[1]][v+1]
      
      mtx2[mtxrows,"coef"] <- sumcox$coefficients[v,1]
      mtx2[mtxrows,"se"] <- sumcox$coefficients[v,3]
      mtx2[mtxrows,"p"] <- round(sumcox$coefficients[v,5],digits=4)
      mtx2[mtxrows,"study"] <-s
      mtx2[mtxrows,"brief_name"] <- covar
      mtx2[mtxrows,"val"] <- thiscox$xlevels[[1]][v+1]
    }
  }
  
  mtx1 <- as.data.frame(mtx1)
  mtx2 <- as.data.frame(mtx2)
  
  mtx1[,'Hazard ratio'] <- paste(mtx1$hr,' (',mtx1$lo,'-',mtx1$hi,')',sep="")
  mtx1$pval <-  format.pval(as.numeric(mtx1$p),digits=3,eps=0.001,nsmall=3)
  
  mtx1$sig <- ""
  mtx1$sig[mtx1$p < 0.05] <- "*"
  mtx1$sig[mtx1$p < 0.01] <- paste(mtx1$sig[mtx1$p < 0.01],"*")
  mtx1$sig[mtx1$p < 0.001] <- paste(mtx1$sig[mtx1$p < 0.001],"*")
  
  crit <- sqldf("select distinct study as study, phenotype,rank, string_value from map")
  crit <- merge(crit,varlist[,c('phenotype','brief_name')],by.x=c("phenotype"),by.y="phenotype")
  
  
  mtx1 <- merge(mtx1,crit, by.x=c("study","brief_name","val"),by.y=c("study","brief_name","rank"))
  mtx2 <- merge(mtx2,crit, by.x=c("study","brief_name","val"),by.y=c("study","brief_name","rank"))
  
  mtx2$coef <- as.numeric(mtx2$coef)
  mtx2$se <- as.numeric(mtx2$se)
  mtx2$p <- as.numeric(mtx2$p)
  
  mtx1$string_value <- gsub("([0-9])\\.\\s+","",mtx1$string_value)
  mtx2$string_value <- gsub("([0-9])\\.\\s+","",mtx2$string_value)
  
  mtx2$string_value2 <- paste(mtx2$string_value,"\nHR",mtx1[,"Hazard ratio"])
  
  colnames(mtx1)[colnames(mtx1)=='string_value'] <- "Value"
  
  if (sig.only==T) {
    mtx1 <- subset(mtx1,p<0.05)
    mtx2 <- subset(mtx2,p<0.05)
  }

  if (hr_table==T&nrow(mtx1[as.numeric(mtx1[,"p"])<0.05,]>0)) {
  p <- ggarrange(
    ggforestplot::forestplot(
      df=mtx2,
      name=string_value,
      estimate=coef,
      logodds=T,
      xlab="Hazard ratio",
      pvalue=p,
      title=paste(outcome_label,"by",covarlabel),
      xtickbreaks=xtckbrks,
      xlim=xlm) +
      geom_point(size=point_size,
                 shape=point_shape) +
      theme(strip.text.x = element_text(size = 12))+
      ggforce::facet_col(
        facets = ~study,
        scales="free",
        space="free"),
    ggtexttable(mtx1[,c('study',
                        'phenotype',
                        'Value',
                        'Hazard ratio',
                        'pval',
                        'sig')],
                rows=NULL,
                theme=ttheme(
                  base_style=tbl_hdr_theme,
                  tbody.style=tbody_style(hjust=0.5,
                                          size=12)
                )
                
    ),
    ncol=1,
    nrow=2,
    heights=c(4,2))
  
  return(p)
  }
  
  if (hr_table==F&nrow(mtx1[as.numeric(mtx1[,"p"])<0.05,]>0)) {
    p <- ggforestplot::forestplot(
        df=mtx2,
        name=string_value,
        estimate=coef,
        logodds=T,
        xlab="Hazard ratio",
        pvalue=p,
        title=paste(outcome_label,"by",covarlabel),
        xtickbreaks=xtckbrks,
        xlim=xlm) +
        geom_point(size=point_size,
                   shape=point_shape) +
        theme(strip.text.x = element_text(size = 12))+
        ggforce::facet_col(
          facets = ~study,
          scales="free",
          space="free")
    return(p)
  }
  
  # if (nrow(mtx1[as.numeric(mtx1[,"p"])<0.05,])==0) {
  #   HTML(paste("No significant associations between",
  #                    outcome_label,
  #                    "and",covarlabel))
  # }
  
  cat("\n")
  
}



##################################################################################################




ggsurvplot_facet2 <- function(pval.size = 5, ...)
{
  newcall <- bquote(
    p <- p + geom_text(data = pvals.df, aes(x = pval.x, y = pval.y, 
                                            label = pval.txt), size = .(pval.size), hjust = 0)
  )
  
  body(ggsurvplot_facet)[[20]][[3]][[8]] <- newcall
  ggsurvplot_facet(...)
}



##-----------------------------------------------------------------




#######################################################################





multistudy_metaanalysis_sum <- function (alldata, 
                                     covar, 
                                     covarlabel,
                                     map,
                                     varlist,
                                     studies, 
                                     outcome_status,
                                     outcome_days,
                                     rtn='log')

{
  
  mtxlength <- 0
  for (i in studies) {
    studyrows <- (length(unique(alldata[alldata$study==i,covar]))-1)
    mtxlength <- mtxlength + studyrows
  }
  
  
  mtx1 <- matrix(
    nrow=mtxlength,
    ncol=7)
  colnames(mtx1)=
    c("lo",
      "hr",
      "hi",
      "p",
      "study",
      "brief_name",
      "val")
  
  mtx2 <- matrix(
    nrow=mtxlength,
    ncol=6)
  colnames(mtx2)=c("coef",
                   "se",
                   "p",
                   "study",
                   "brief_name",
                   "val")
  
  mtxrows <- 0
  
  
  for (s in studies) {
    num_values <- length(unique(alldata[alldata$study==s,covar]))
    thiscox <- coxph(
      Surv(
        alldata[alldata$study==s,outcome_days]/30,
        alldata[alldata$study==s,outcome_status])~
        as.factor(alldata[alldata$study==s,covar]))
    sumcox <- summary(thiscox)
    
    for (v in 1:(num_values-1)) {
      mtxrows <- mtxrows+1
      mtx1[mtxrows,"hr"] <- round(sumcox$conf.int[v,1],digits=2)
      mtx1[mtxrows,"lo"] <- round(sumcox$conf.int[v,3],digits=2)
      mtx1[mtxrows,"hi"] <- round(sumcox$conf.int[v,4],digits=2)
      mtx1[mtxrows,"p"] <-  sumcox$coefficients[v,"Pr(>|z|)"]
      mtx1[mtxrows,"study"] <- s
      mtx1[mtxrows,"brief_name"] <- covar
      mtx1[mtxrows,"val"] <- thiscox$xlevels[[1]][v+1]
      
      mtx2[mtxrows,"coef"] <- sumcox$coefficients[v,1]
      mtx2[mtxrows,"se"] <- sumcox$coefficients[v,3]
      mtx2[mtxrows,"p"] <- round(sumcox$coefficients[v,5],digits=4)
      mtx2[mtxrows,"study"] <-s
      mtx2[mtxrows,"brief_name"] <- covar
      mtx2[mtxrows,"val"] <- thiscox$xlevels[[1]][v+1]
    }
  }
  
  mtx1 <- as.data.frame(mtx1)
  mtx2 <- as.data.frame(mtx2)
  
  mtx1[,'Hazard ratio'] <- paste(mtx1$hr,' (',mtx1$lo,'-',mtx1$hi,')',sep="")
  mtx1$pval <-  format.pval(as.numeric(mtx1$p),digits=3,eps=0.001,nsmall=3)
  
  mtx1$sig <- ""
  mtx1$sig[mtx1$p < 0.05] <- "*"
  mtx1$sig[mtx1$p < 0.01] <- paste(mtx1$sig[mtx1$p < 0.01],"*")
  mtx1$sig[mtx1$p < 0.001] <- paste(mtx1$sig[mtx1$p < 0.001],"*")
  
  crit <- sqldf("select distinct study as study, phenotype,rank, string_value from map")
  crit <- merge(crit,varlist[,c('phenotype','brief_name')],by.x=c("phenotype"),by.y="phenotype")
  
  
  mtx1 <- merge(mtx1,crit, by.x=c("study","brief_name","val"),by.y=c("study","brief_name","rank"))
  mtx2 <- merge(mtx2,crit, by.x=c("study","brief_name","val"),by.y=c("study","brief_name","rank"))
  
  mtx2$coef <- as.numeric(mtx2$coef)
  mtx2$se <- as.numeric(mtx2$se)
  mtx2$p <- as.numeric(mtx2$p)
  
  mtx1$string_value <- gsub("([0-9])\\.\\s+","",mtx1$string_value)
  mtx2$string_value <- gsub("([0-9])\\.\\s+","",mtx2$string_value)
  
  
  mtx2$string_value2 <- paste(mtx2$string_value,"\nHR",mtx1[,"Hazard ratio"])
  
  colnames(mtx1)[colnames(mtx1)=='string_value'] <- "Value"
  
  
if (rtn=="log") {
  return(mtx2)
}

if (rtn=="hazard") {
    return(mtx1)
  }
  
    
}



