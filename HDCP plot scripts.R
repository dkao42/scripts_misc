library(survival)
library(rmeta)
library(sqldf)
library(plot.matrix)


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

multistudy_forest_plot <- function (alldata, 
                                    covars, 
                                    covarlabels, 
                                    studies, 
                                    outcome_status,
                                    outcome_days,
                                    outcome_label,
                                    plotlo=0.5,
                                    plothi=1.5,
                                    thiscolor="black",
                                    bxsize=0.75,
                                    fpfldr = "FP plots",
                                    in_line=T,
                                    outpt=T,
                                    w_fp,
                                    w_pdf)

  ## This function will take a HDCP-type data set, an outcome and time-to-event, a set of studies to analyze, and a set of covariabes
## It will produce PDFs with forest plots for each 'covar' showing the hazard of the specified outcome associated with that covar FOR EACH STUDY
## I.E.  the forest plot will have one row for each study, where the hazard ratio shown is the relatively likelihood of the outcome associated with each value of var (e.g. drug v placebo)
## The function will automatically generate PDFs using the 'covar labels' and 'outcome label' in the name of the PDF.  e.g., covar might = "DM" where covarlabel = "Diabetes mellitus" 
## Plots will end up in a subdirector - default is "FP plots"
  
## If you want to look at e.g. treatment response in a subgroup of studies, set covar = treatment group variable and subset data to e.g. subset(data,sex="Male").  This will produce an FP with treatment response in males in all studies.
## If you want to create FPs for treatment response for all values of a variable (e.g. White, black, Asian, other), write a "for" loop iteratively subsetting the data set.
  
  {
  wd <- getwd()
  for (i in 1:length(covars)) {
    thiscovar <- covars[i]
    print(thiscovar)
    print("test")
    mtx <- matrix(nrow=length(studies),ncol=3,dimnames=list(c(studies),c("lo","hr","hi")))
    for (s in 1:length(studies)) {
      thisstudy <- studies[s]
      thiscox <- coxph(
        Surv(
          alldata[alldata$study==thisstudy,outcome_days]/30,
          alldata[alldata$study==thisstudy,outcome_status])~
          alldata[alldata$study==thisstudy,thiscovar])
      
      sumcox <- summary(thiscox)
      mtx[thisstudy,"hr"] <- sumcox$conf.int[,1]
      mtx[thisstudy,"lo"] <- sumcox$conf.int[,3]
      mtx[thisstudy,"hi"] <- sumcox$conf.int[,4]
      print(cat("\n\n\nVariable:",thiscovar,"  Study: ",thisstudy,"\n"))
      print(clean_cox_hronly(
        coxph(
          Surv(
            alldata[alldata$study==thisstudy,outcome_days]/30,
            alldata[alldata$study==thisstudy,outcome_status])~
            alldata[alldata$study==thisstudy,thiscovar])))
    }
    
    print(mtx)
    
    if (in_line==T) {
      forestplot(
        matrix(
          c(rownames(mtx))),
        mean=mtx[,"hr"],
        lower=mtx[,"lo"],
        upper=mtx[,"hi"],
        boxsize=bxsize,
        zero=1,
        clip=c(plotlo,plothi),
        xticks=c(0.5,0.75,1.0,1.25,1.5),
        col=meta.colors(
          box=thiscolor,
          lines= thiscolor,
          zero= thiscolor,
          text= thiscolor),
        graphwidth=unit(w_fp,"inches"))
    }

    if (outpt==T) {
    pdf(paste(wd,"/",fpfldr,"/",outcome_label," according to ", thiscovar,".pdf",sep=""),w_pdf=4)
    forestplot(
      matrix(
        c(rownames(mtx))),
      mean=mtx[,"hr"],
      lower=mtx[,"lo"],
      upper=mtx[,"hi"],
      boxsize=bxsize,
      zero=1,
      clip=c(plotlo,plothi),
      xticks=c(0.5,0.75,1.0,1.25,1.5),
      col=meta.colors(
        box=thiscolor,
        lines= thiscolor,
        zero= thiscolor,
        text= thiscolor),
      graphwidth=unit(w_fp,"inches"))
    dev.off()
    }
    }
}


################################################################################################################

multistudy_forest_plot_log <- function (alldata, 
                                        covars, 
                                        covarlabels, 
                                        studies, 
                                        outcome_status,
                                        outcome_days,
                                        outcome_label, 
                                        zro=log2(1),
                                        plotlo=log2(0.5),
                                        plothi=log2(8),
                                        thiscolor="black",
                                        bxsize=0.75,
                                        fpfldr = "FP plots",
                                        tcks=log2(c(0.5,1,2,4,8)),
                                        in_line=T,
                                        outpt=T,
                                        w_pdf=4,
                                        w_fp=2.5)

  ## This function will take a HDCP-type data set, an outcome and time-to-event, a set of studies to analyze, and a set of covariabes
  ## It will produce PDFs with forest plots for each 'covar' showing the hazard of the specified outcome associated with that covar FOR EACH STUDY
  ## I.E.  the forest plot will have one row for each study, where the hazard ratio shown is the relatively likelihood of the outcome associated with each value of var (e.g. drug v placebo)
  ## Same as above, just with log hazard ratio scale. 
  
  ## If you want to look at e.g. treatment response in a subgroup of studies, set covar = treatment group variable and subset data to e.g. subset(data,sex="Male").  This will produce an FP with treatment response in males in all studies.
  ## If you want to create FPs for treatment response for all values of a variable (e.g. White, black, Asian, other), write a "for" loop iteratively subsetting the data set.
  
{
  wd <- getwd()
  for (i in 1:length(covars)) {
    thiscovar <- covars[i]
    thislabel <- covarlabels[i]
    print(outcome_label)
    print(thislabel)
    mtx <- matrix(
      nrow=length(studies),
      ncol=4,
      dimnames=list(c(studies),
                    c("lo",
                      "hr",
                      "hi",
                      "p")))
    
    for (s in 1:length(studies)) {
      thisstudy <- studies[s]
      thiscox <- coxph(
        Surv(
          alldata[alldata$study==thisstudy,outcome_days]/30,
          alldata[alldata$study==thisstudy,outcome_status])~
          alldata[alldata$study==thisstudy,thiscovar])
      sumcox <- summary(thiscox)
      ## print(sumcox)
      mtx[thisstudy,"hr"] <- round(sumcox$conf.int[,1],digits=2)
      mtx[thisstudy,"lo"] <- round(sumcox$conf.int[,3],digits=2)
      mtx[thisstudy,"hi"] <- round(sumcox$conf.int[,4],digits=2)
      mtx[thisstudy,"p"] <- round(sumcox$logtest[3],digits=4)
      ## print(cat("\n\n\nVariable:",thiscovar,"  Study: ",thisstudy,"\n"))
      ## print(clean_cox_hronly(coxph(Surv(alldata[alldata$study==thisstudy,outcome_days]/30,aallldata[alldata$study==thisstudy,outcome_status])~alldata[alldata$study==thisstudy,thiscovar])))
    }
    
    print(mtx)

    if (in_line==T) {
      forestplot(
        matrix(
          c(rownames(mtx))),
        mean=log2(mtx[,"hr"]),
        lower=log2(mtx[,"lo"]),
        upper=log2(mtx[,"hi"]),
        boxsize=bxsize,
        zero=zro,
        clip=c(plotlo,plothi),
        xticks=tcks,
        col=meta.colors(
          box=thiscolor,
          lines= thiscolor,
          zero= thiscolor,
          text= thiscolor),
        graphwidth=unit(w_fp,"inches"))
    }
    
    if (outpt==T) {
    pdf(paste(wd,"/",fpfldr,"/",outcome_label," according to ", thislabel,".pdf",sep=""),width=w_pdf,height=h_pdf)
    forestplot(
      matrix(
        c(rownames(mtx))),
      mean=log2(mtx[,"hr"]),
      lower=log2(mtx[,"lo"]),
      upper=log2(mtx[,"hi"]),
      boxsize=bxsize,
      zero=zro,
      clip=c(plotlo,plothi),
      xticks=tcks,
      col=meta.colors(
        box=thiscolor,
        lines= thiscolor,
        zero= thiscolor,
        text= thiscolor),
      graphwidth=unit(w_fp,"inches"))
    dev.off()
    }
  }
}
######################################################################################


#####################################################################################



################################################################################################################

multistudy_forest_plot_logit_or_log <- function (alldata, 
                                        covars, 
                                        covarlabels, 
                                        studies, 
                                        outcome_status,
                                        outcome_label, 
                                        zro=log2(1),
                                        plotlo=log2(0.5),
                                        plothi=log2(8),
                                        thiscolor="black",
                                        bxsize=0.75,
                                        fpfldr = "FP plots",
                                        tcks=log2(c(0.5,1,2,4,8)),
                                        in_line=T,
                                        outpt=T)

## This function will take a HDCP-type data set, an outcome, a set of studies to analyze, and a set of covariabes
## It will produce PDFs with forest plots for each 'covar' showing the hazard of the specified outcome associated with that covar FOR EACH STUDY
## I.E.  the forest plot will have one row for each study, where the hazard ratio shown is the relatively likelihood of the outcome associated with each value of var (e.g. drug v placebo)
## Same as above, just with log hazard ratio scale. 

## If you want to look at e.g. treatment response in a subgroup of studies, set covar = treatment group variable and subset data to e.g. subset(data,sex="Male").  This will produce an FP with treatment response in males in all studies.
## If you want to create FPs for treatment response for all values of a variable (e.g. White, black, Asian, other), write a "for" loop iteratively subsetting the data set.

{
  wd <- getwd()
  for (i in 1:length(covars)) {
    thiscovar <- covars[i]
    thislabel <- covarlabels[i]
    print(outcome_label)
    print(thislabel)
    mtx <- matrix(
      nrow=length(studies),
      ncol=4,
      dimnames=list(c(studies),
                    c("lo",
                      "or",
                      "hi",
                      "p")))
    
    for (s in 1:length(studies)) {
      thisstudy <- studies[s]
      thisglm <- glm(alldata[alldata[,'study']==thisstudy,outcome_status]~
                       alldata[alldata[,'study']==thisstudy,thiscovar],
                     family=binomial(link="logit"))
      sumglm <- summary(thisglm)$coefficients
      coefglm <- as.data.frame(confint(thisglm))
      sumglm <- cbind(sumglm,coefglm)    
      
      mtx[thisstudy,"or"] <- round(exp(sumglm[2,'Estimate']),digits=2)
      mtx[thisstudy,"lo"] <- round(exp(sumglm[2,'2.5 %']),digits=2)
      mtx[thisstudy,"hi"] <- round(exp(sumglm[2,'97.5 %']),digits=2)
      mtx[thisstudy,"p"] <- round(sumglm[2,'Pr(>|z|)'],digits=4)
    }
    
    print(mtx)
    
    if (in_line==T) {
      forestplot(
        matrix(
          c(rownames(mtx))),
        mean=log2(mtx[,"or"]),
        lower=log2(mtx[,"lo"]),
        upper=log2(mtx[,"hi"]),
        boxsize=bxsize,
        zero=zro,
        clip=c(plotlo,plothi),
        xticks=tcks,
        col=meta.colors(
          box=thiscolor,
          lines= thiscolor,
          zero= thiscolor,
          text= thiscolor),
        graphwidth=unit(2,"inches"))
    }
    
    if (outpt==T) {
      pdf(paste(wd,"/",fpfldr,"/",outcome_label," according to ", thislabel,".pdf",sep=""))
      forestplot(
        matrix(
          c(rownames(mtx))),
        mean=log2(mtx[,"or"]),
        lower=log2(mtx[,"lo"]),
        upper=log2(mtx[,"hi"]),
        boxsize=bxsize,
        zero=zro,
        clip=c(plotlo,plothi),
        xticks=tcks,
        col=meta.colors(
          box=thiscolor,
          lines= thiscolor,
          zero= thiscolor,
          text= thiscolor),
        graphwidth=unit(2,"inches"))
      dev.off()
    }
  }
}



################################################################################################################
# 
# plot_available_variables <- function(var_list, 
#                                      stds=c('BEST',
#                                             'TOPCAT',
#                                             'RELAX',
#                                             'HF-ACTION',
#                                             'IPRESERVE',
#                                             'DIG',
#                                             'SOLVD Treat',
#                                             'SOLVD Prevent',
#                                             'SCD-HeFT',
#                                             'MOCHA',
#                                             'NEAT-HFpEF',
#                                             'STICH',
#                                             'EXACT',
#                                             'GUIDE-IT',
#                                             'DOSE',
#                                             'CARRESS',
#                                             'ESCAPE',
#                                             'ROSE',
#                                             'RELAX'),
#                                      num_thresh=8,
#                                      plot_name='Var availability')
# {
#   
# ## This function will produce a grid plot for variables that appear in each study of a list of studies.
# ## The input 'var_list' is a data frame containing unique pairs of study-brief_name.  This is then transformed into a matrix of study x brief name where 1 = variable is present in that study
# ## It will also count the number of studies a given variable appears in and only display those above a certain number threshhold if (set by 'num_thresh' parameter)
#   
#   
#   all_studies <- unique(var_list$study)
#   all_fields <- unique(var_list$brief_name)
#   
#   avail_field <- matrix(nrow=length(all_fields),
#                         ncol=length(all_studies),
#                         dimnames=list(c(all_fields),
#                                       c(all_studies)))
#   
#   query_studies <- data.frame(study=stds,stringsAsFactors = F)
#   
#   for (i in 1:nrow(var_list)) {
#     thisstudy <- var_list[i,'study']
#     thisfield <- var_list[i,'brief_name']
#     avail_field[thisfield,thisstudy] <- 1
#   }
#   
#   var_rank <- sqldf('select brief_name,count(distinct study) as num from var_list where study in (select study from query_studies) group by brief_name order by num desc')
#   
#   pdf(paste(plot_name,'.pdf',sep=''))
#   plot(avail_field[var_rank$brief_name[var_rank$num>=num_thresh],
#                    query_studies$study],
#        border=NA,
#        key=NULL,
#        las=2,
#        xlab=NA,
#        ylab=NA,
#        main="Available variables",
#        cex.axis=0.5)
#   dev.off()
# }
# 




multistudy_forest_ggplot_log <- function (alldata, 
                                        covars, 
                                        covarlabels, 
                                        studies, 
                                        outcome_status,
                                        outcome_days,
                                        outcome_label, 
                                        fpfldr = "",
                                        in_line=T,
                                        outpt=F,
                                        point_shape=18,
                                        point_size=5)

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
                      "hr",
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
      thiscox <- coxph(
        Surv(
          alldata[alldata$study==thisstudy,outcome_days]/30,
          alldata[alldata$study==thisstudy,outcome_status])~
          alldata[alldata$study==thisstudy,thiscovar])
      
      sumcox <- summary(thiscox)
      
      mtx1[thisstudy,"hr"] <- round(sumcox$conf.int[,1],digits=2)
      mtx1[thisstudy,"lo"] <- round(sumcox$conf.int[,3],digits=2)
      mtx1[thisstudy,"hi"] <- round(sumcox$conf.int[,4],digits=2)
      mtx1[thisstudy,"p"] <- round(sumcox$logtest[3],digits=4)
      mtx2[thisstudy,"coef"] <- sumcox$coefficients[,1]
      mtx2[thisstudy,"se"] <- sumcox$coefficients[,3]
      mtx2[thisstudy,"p"] <- round(sumcox$coefficients[,5],digits=4)
    }
    
    mtx2 <- as.data.frame(mtx2)
    mtx2$study_name <- rownames(mtx2)

    print(mtx1)
    
    print(ggforestplot::forestplot(
        df=mtx2,
        name=study_name,
        estimate=coef,
        logodds=T,
        xlab="Hazard ratio",
        pvalue=p,
        title=paste(outcome_label,"by",thislabel),
        xtickbreaks=c(0.25,0.5,0.75,1,1.5,2),
        xlim=c(0.25,2.5)
        ) + geom_point(size=point_size,shape=point_shape)
    )

    
    if (outpt==T) {
      pdf(paste(wd,"/",fpfldr,"/",outcome_label," according to ", thislabel,".pdf",sep=""),width=w_pdf)
      ggforestplot::forestplot(
        df=mtx2,
        name=study_name,
        estimate=coef,
        logodds=T,
        xlab="Hazard ratio",
        pvalue=p,
        xtickbreaks=c(0.25,0.5,0.75,1,1.5,2),
        xlim=c(0.25,2.5)
      )  + geom_point(size=point_size,shape=point_shape)
      dev.off()
    }
  }
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

    if (in_line==T) {
      print(ggforestplot::forestplot(
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
    }

    if (outpt==T) {
      pdf(paste(wd,"/",fpfldr,"/",outcome_label," according to ", thislabel,".pdf",sep=""),width=w_pdf)
      ggforestplot::forestplot(
        df=mtx2,
        name=study_name,
        estimate=coef,
        logodds=T,
        xlab="Hazard ratio",
        pvalue=p,
        xtickbreaks=c(0.25,0.5,0.75,1,1.5,2),
        xlim=c(0.25,2.5),
        colour=outcome,
        environment=environment()
      )
      dev.off()
    }
  }
}






multistudy_forest_cox_log_hr_table <- function (alldata, 
                                          covars, 
                                          covarlabels, 
                                          studies, 
                                          outcome_status,
                                          outcome_days,
                                          outcome_label, 
                                          fpfldr = "",
                                          in_line=T,
                                          outpt=F,
                                          point_shape=18,
                                          point_size=5)

{
  wd <- getwd()
  for (i in 1:length(covars)) {
    thiscovar <- covars[i]
    thislabel <- covarlabels[i]
    
    
    mtxrows <- 0
    
    for (i in studies) {
      studyrows <- (length(unique(alldata[alldata$study==i,thiscovar]))-1)
      mtxrows <- mtxrows + studyrows
      
      print(paste(i,"Matrix rows:",studyrows))
    }
    print(outcome_label)
    print(thislabel)
    mtx1 <- matrix(
      nrow=length(studies),
      ncol=4,
      dimnames=list(c(studies),
                    c("lo",
                      "hr",
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
      thiscox <- coxph(
        Surv(
          alldata[alldata$study==thisstudy,outcome_days]/30,
          alldata[alldata$study==thisstudy,outcome_status])~
          alldata[alldata$study==thisstudy,thiscovar])
      
      sumcox <- summary(thiscox)
      
      mtx1[thisstudy,"hr"] <- round(sumcox$conf.int[,1],digits=2)
      mtx1[thisstudy,"lo"] <- round(sumcox$conf.int[,3],digits=2)
      mtx1[thisstudy,"hi"] <- round(sumcox$conf.int[,4],digits=2)
      mtx1[thisstudy,"p"] <- round(sumcox$logtest[3],digits=4)
      

    }
  }
  print(mtx1)
}





multistudy_forest_ggplot_log_2 <- function (alldata, 
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
                                        h_pdf=10,
                                        w_pdf=7)

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
      # mtx1[mtxrows,"hr"] <- round(sumcox$conf.int[v,1],digits=2)
      # mtx1[mtxrows,"lo"] <- round(sumcox$conf.int[v,3],digits=2)
      # mtx1[mtxrows,"hi"] <- round(sumcox$conf.int[v,4],digits=2)
      # mtx1[mtxrows,"p"] <- round(sumcox$coefficients[v,"Pr(>|z|)"],digits=4)
      # mtx1[mtxrows,"study"] <- s
      # mtx1[mtxrows,"brief_name"] <- covar
      # mtx1[mtxrows,"val"] <- thiscox$xlevels[[1]][v+1]

      mtx2[mtxrows,"coef"] <- sumcox$coefficients[v,1]
      mtx2[mtxrows,"se"] <- sumcox$coefficients[v,3]
      mtx2[mtxrows,"p"] <- round(sumcox$coefficients[v,5],digits=4)
      mtx2[mtxrows,"study"] <-s
      mtx2[mtxrows,"brief_name"] <- covar
      mtx2[mtxrows,"val"] <- thiscox$xlevels[[1]][v+1]
      }
    }


      # mtx1 <- as.data.frame(mtx1)
      mtx2 <- as.data.frame(mtx2)
      
      # mtx1[,'Hazard ratio'] <- paste(mtx1$hr,' (',mtx1$lo,'-',mtx1$hi,')',sep="")

      crit <- sqldf("select distinct study as study, phenotype,Indicates_rank, string_value from map")
      crit <- merge(crit,varlist[,c('phenotype','brief_name')],by.x=c("Phenotype"),by.y="phenotype")


      mtx1 <- merge(mtx1,crit, by.x=c("study","brief_name","val"),by.y=c("study","brief_name","Indicates_rank"))
      mtx2 <- merge(mtx2,crit, by.x=c("study","brief_name","val"),by.y=c("study","brief_name","Indicates_rank"))

      # print(mtx1[,c('study','Phenotype','string_value','Hazard ratio','p')])

      # print(mtx2)
      
      mtx2$coef <- as.numeric(mtx2$coef)
      mtx2$se <- as.numeric(mtx2$se)
      mtx2$p <- as.numeric(mtx2$p)

  # mtx1$string_value <-gsub("([0-9])\\.\\s+","",mtx1$string_value)      
      
    print(ggforestplot::forestplot(
        df=mtx2,
        name=val,
        estimate=coef,
        logodds=T,
        xlab="Hazard ratio",
        pvalue=p,
        title=paste(outcome_label,"by",covarlabel),
        xtickbreaks=c(0.25,0.5,1,2,4),
        xlim=c(0.25,4.2)
        ) +
      geom_point(size=point_size,shape=point_shape)+
      theme(axis.text.x=element_text(size=0))+
      ggforce::facet_col(
        facets = ~study,
        scales="free",
        space="fixed",
        strip.position="left")
    )

    if (outpt==T) {
      pdf(paste(wd,"/",fpfldr,"/",outcome_label," according to ", thislabel,".pdf",sep=""),width=w_pdf,height=h_pdf)
      ggforestplot::forestplot(
        df=mtx2,
        name=study_name,
        estimate=coef,
        logodds=T,
        xlab="Hazard ratio",
        pvalue=p,
        xtickbreaks=c(0.25,0.5,1,2,4),
        xlim=c(0.25,4.2)
      )  + geom_point(size=point_size,shape=point_shape)
      dev.off()
    }

  

}


multistudy_forest_ggplot_log_2_table <- function (alldata, 
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
                                            point_size=5)

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
      mtx1[mtxrows,"p"] <- round(sumcox$coefficients[v,"Pr(>|z|)"],digits=4)
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
  
  crit <- sqldf("select distinct study as study, phenotype,Indicates_rank, string_value from map")
  crit <- merge(crit,varlist[,c('phenotype','brief_name')],by.x=c("Phenotype"),by.y="phenotype")
  
  
  mtx1 <- merge(mtx1,crit, by.x=c("study","brief_name","val"),by.y=c("study","brief_name","Indicates_rank"))
  mtx2 <- merge(mtx2,crit, by.x=c("study","brief_name","val"),by.y=c("study","brief_name","Indicates_rank"))
  
  
  mtx2$coef <- as.numeric(mtx2$coef)
  mtx2$se <- as.numeric(mtx2$se)
  mtx2$p <- as.numeric(mtx2$p)

  print(mtx1[,c('study','Phenotype','string_value','Hazard ratio','p')])
  
    
}



