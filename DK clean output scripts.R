library(rmeta)
library(survival)
library(speedglm)
library(stringr)
source("~/Dropbox/R scripts/sem.R")

##########################################################################################

glmer_ci_dk <- function (model1,tail,ordig=2,pdig=3)
{
  ## Takes either glm or glmer objects and outputs a table of odds ratios and p-values
  ## ordig = odds ratio significant digits, pdig = p-value significant digits
  or <- round(exp(coef(summary(model1))[,1]),ordig)
  lower <- round(exp(coef(summary(model1))[,1] + qnorm(.025)*coef(summary(model1))[,2]),ordig)
  upper <- round(exp(coef(summary(model1))[,1] + qnorm(.975)*coef(summary(model1))[,2]),ordig)
  p <- round(coef(summary(model1))[,c(4)],pdig)
  cbind(or,lower, upper, p)
}

##########################################################################################

glmer_ci_dk_fp <- function(model,bsize=0.5,zro=1,thiscolor='black',lo=0.1,hi=3,tcks=c(0.1,0.5,1,2,3)) {
  
## Takes a glm or glmer object to make a forest plot of all relevant odds ratios.
forestplot(matrix(c(rownames(glmer_ci_dk(model))[2:nrow(glmer_ci_dk(model))])),glmer_ci_dk(model)[2:nrow(glmer_ci_dk(model)),1],glmer_ci_dk(model)[2:nrow(glmer_ci_dk(model)),2],glmer_ci_dk(model)[2:nrow(glmer_ci_dk(model)),3],
             boxsize=bsize,zero=zro,col=meta.colors(box=thiscolor,lines= thiscolor,zero= thiscolor,text= thiscolor),clip=c(lo,hi),xticks=tcks,xlab='Odds Ratio')
}

##########################################################################################

cox_dk_fp <- function(model,bsize=0.5,zro=1,thiscolor='black',lo=0.1,hi=3,tcks=c(0.1,0.5,1,2,3),lbl=NA,ttle) {
  thiscox <- summary(model)$conf.int
  if (length(lbl)>1) {
    labels <- matrix(lbl,ncol=1) 
  } else if (length(lbl)==1) {
    labels <- matrix(c(rownames(thiscox)))
  }
  forestplot(labels,mean=(thiscox[,1]),lower=thiscox[,3],upper=thiscox[,4],
             boxsize=bsize,zero=zro,col=meta.colors(box=thiscolor,lines= thiscolor,zero= thiscolor,text= thiscolor),clip=c(lo,hi),xticks=tcks,xlab='Hazard Ratio')
  title(ttle)
}

##########################################################################################

cox_dk_fp_log <- function(model,bsize=0.5,zro=log2(1),thiscolor='black',lo=log2(0.5),hi=log2(8),tcks=log2(c(0.5,1,2,4,8)),lbl=NA,ttle) {
  thiscox <- summary(model)$conf.int
  if (length(lbl)>1) {
    labels <- matrix(lbl,ncol=1) 
  } else if (length(lbl)==1) {
    labels <- matrix(c(rownames(thiscox)))
  }
  forestplot(labels,mean=log2(thiscox[,1]),lower=log2(thiscox[,3]),upper=log2(thiscox[,4]),
             boxsize=bsize,zero=zro,col=meta.colors(box=thiscolor,lines= thiscolor,zero= thiscolor,text= thiscolor),clip=c(lo,hi),xticks=tcks,xlab='Hazard Ratio')
  title(ttle)
}

##########################################################################################

glmer_ci_dk_fp_log <- function(model,bsize=0.5,zro=log2(1),thiscolor='black',lo=log2(0.5),hi=log2(8),tcks=log2(c(0.5,1,2,4,8))) {
  forestplot(matrix(c(rownames(glmer_ci_dk(model))[2:nrow(glmer_ci_dk(model))])),log2(glmer_ci_dk(model)[2:nrow(glmer_ci_dk(model)),1]),log2(glmer_ci_dk(model)[2:nrow(glmer_ci_dk(model)),2]),log2(glmer_ci_dk(model)[2:nrow(glmer_ci_dk(model)),3]),
             boxsize=bsize,zero=zro,col=meta.colors(box=thiscolor,lines= thiscolor,zero= thiscolor,text= thiscolor),clip=c(lo,hi),xticks=tcks,xlab='Odds Ratio')
}

##########################################################################################

clean_cox <- function(x,hrdig=2,pdig=3) {
    ### Uses a coxph object and outputs HR and p-values as well as tests for PH assumption via cox.zph
    hrs <- round(summary(x)$conf.int[,c(1,3,4)],hrdig)
    ps <-  round(summary(x)$coefficients[,5],pdig)
    if (is.vector(hrs)) {
      tbl <- c(hrs,ps)
      cat(names(x$coefficients),"\tHR: ",tbl[[1]], " [",tbl[[2]],"-",tbl[[3]],"]"," p.value=",tbl[[4]],sep="")
      }
    if (is.matrix(hrs)) {
      tbl <- cbind(hrs,ps)
      colnames(tbl) <- c("HR","lowCI","hiCI","p.value")
      print(tbl)
    }
    cat('\n\n','Log likelihood test:','\n')
    print(summary(x)$logtest)
    cat('ZPH test (Check proportional hazards assumption):','\n')
    print(cox.zph(x))
    cat('---------------------------')
}

##########################################################################################

clean_cox_hronly <- function(x,hrdig=2,pdig=3,print_coef="Y",spcer=T) {
    ### Uses a coxph object (x) and just outputs HR and p-values (no PH test)
    hrs <- round(summary(x)$conf.int[,c(1,3,4)],hrdig)
    ps <-  round(summary(x)$coefficients[,5],pdig)
    thiscoef <- ""
    if (is.vector(hrs)) {
        tbl <- c(hrs,ps)
        if (print_coef=="Y") {
            thiscoef <- cat(names(x$coefficients),"\n",sep="")
        }
        cat(thiscoef,"HR: ",tbl[[1]], " [",tbl[[2]],"-",tbl[[3]],"]"," p.value=",tbl[[4]],sep="")
    }
    if (is.matrix(hrs)) {
        tbl <- cbind(hrs,ps)
        colnames(tbl) <- c("HR","lowCI","hiCI","p.value")
        print(tbl)
    }
    if (spcer==T) {cat('\n---------------------------\n')}
    cat('\n')
}

##########################################################################################

clean_logit <- function(x,ordig=2,pdig=3) {
  ## Assumes a glm binomial object and prints a nicely formatted OR
  thisresult <- paste(names(x$coefficients),": OR ",round(exp(x$coefficients),ordig)," 95% CI [",
        round(exp(confint(x)[,1]),ordig),"-",round(exp(confint(x)[,2]),ordig),"], p=",round(summary(x)$coefficients[,4],pdig),sep="")
  print(thisresult,quote=F)
}

##########################################################################################

clean_speedglm <- function(x,ordig=2,pdig=3){
  ## Assumes x is a speedglm binomial object and prints a nicely formatted OR table.
  thisglm_out <- summary(x)$coefficients
  thisglm_out$OR <- round(exp(thisglm_out$Estimate),ordig)
  thisglm_out <- cbind(thisglm_out,round(exp(confint(x)),ordig))
  thisglm_out$pvalue <- round(as.numeric(as.character(summary(x)$coefficients[,4])),pdig)
  print(thisglm_out[,c("OR","2.5 %","97.5 %","pvalue")])
}

##########################################################################################

speedglm_ci_dk_fp <- function(x,bsize=0.5,zro=1,thiscolor='black',lo=0.1,hi=3,tcks=c(0.1,0.5,1,2,3)) {
  ## Assumes x is a speedglm object and creates a FP of ORs
  model <- clean_speedglm(x)
  forestplot(matrix(c(rownames(model)[2:nrow(model)])),model[2:nrow(model),1],model[2:nrow(model),2],model[2:nrow(model),3],
             boxsize=bsize,zero=zro,col=meta.colors(box=thiscolor,lines= thiscolor,zero= thiscolor,text= thiscolor),clip=c(lo,hi),xticks=tcks,xlab='Odds Ratio')
}

##########################################################################################

speedglm_ci_dk_fp_log <- function(x,bsize=0.5,zro=log2(1),thiscolor='black',low=0.5,high=8,tks=c(0.5,1,2,4,8)) {
  ## Assumes x is a speedglm object and creates a FP of ORs with log axis
  model <- clean_speedglm(x)
  tcks <- log2(tks)
  lo <- log2(low)
  hi <- log2(high)
  forestplot(matrix(c(rownames(model)[2:nrow(model)])),log2(model[2:nrow(model),1]),log2(model[2:nrow(model),2]),log2(model[2:nrow(model),3]),
             boxsize=bsize,zero=zro,col=meta.colors(box=thiscolor,lines= thiscolor,zero= thiscolor,text= thiscolor),clip=c(lo,hi),xticks=tcks,xlab='Odds Ratio')
}

##########################################################################################

speedglm_ci_dk_fp_log10 <- function(x,bsize=0.5,zro=log10(1),thiscolor='black',low=1,high=1000,tks=c(0.1,1,10,100,1000)) {
  ## Assumes x is a speedglm object and creates a FP of ORs with log axis
  model <- clean_speedglm(x)
  tcks <- log10(tks)
  lo <- log10(low)
  hi <- log10(high)
  forestplot(matrix(c(rownames(model)[2:nrow(model)])),log10(model[2:nrow(model),1]),log10(model[2:nrow(model),2]),log10(model[2:nrow(model),3]),
             boxsize=bsize,zero=zro,col=meta.colors(box=thiscolor,lines= thiscolor,zero= thiscolor,text= thiscolor),clip=c(lo,hi),xticks=tcks,xlab='Odds Ratio')
}

##########################################################################################

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)

  ## Summarizes data.
  ## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
  ##   data: a data frame.
  ##   measurevar: the name of a column that contains the variable to be summariezed
  ##   groupvars: a vector containing names of columns that contain grouping variables
  ##   na.rm: a boolean that indicates whether to ignore NA's
  ##   conf.interval: the percent range of the confidence interval (default is 95%)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

##########################################################################################

swap_count_pct_Hmisc <- function(h) {

      # g <- str_split(h,"\\(")
      # g_num <- gsub("\\)","",trimws(g[[1]][2]))
      # g_pct <- trimws(g[[1]][1])
      # return(paste(g_num," (",g_pct,")",sep=""))
  
  
  if (grepl("\\(",h)) {
    g <- str_split(h,"\\(")
    g_num <- gsub("\\)","",trimws(g[[1]][2]))
    g_pct <- trimws(g[[1]][1])
    return(paste(g_num," (",g_pct,")",sep=""))
  }
  else if (!grepl("\\(",h)) {
    return(h)
  }
}


##########################################################################################

pretty_Hsummary_datatable <- function(y,rowlkp,cap="",plength=10) {

  
n <- gsub("\\+\\/\\-\\s+", "±", trimws(y))
n <- gsub("\\+\\/\\-", "±", trimws(y))
n <- gsub("\\s+", " ", n)
n <- gsub("\\(\\s+", "\\(", n)
n <- gsub("±\\s+", "±", n)
colnames(n) <- n[1,]
colnames(n) <- gsub("([0-9])\\.\\s+","",colnames(n))
rownames(n) <- gsub("([0-9])\\.\\s+","",rownames(n))

n <-  apply(n,MARGIN=c(1,2),function(x) swap_count_pct_Hmisc(x))


for (i in 1:nrow(rowlkp)) {
  this_row_brief <- paste("^",rowlkp[i,'out_column'],sep="")
  this_abbrev <- rowlkp[i,'out_abbreviation']
  rownames(n) <- gsub(this_row_brief,this_abbrev,rownames(n))
}
rownames(n) <- gsub("\\s+:\\s+Yes","",rownames(n))
rownames(n) <- gsub("_val","",rownames(n))

maxwidth <- max(nchar(rownames(n)))

rr <- rownames(n)

for (i in 1:length(rr)) {
  this_row <- rr[i]
  if (grepl("\\s\\s\\s\\s",this_row)) {
    this_row <- trimws(this_row,"left")
    # this_row <- str_pad(this_row,width=10,side="left","_")
    rr[i] <- this_row
  }
}

rownames(n) <- rr  

# for (i in 1:nrow(hdcp_outcome_map)) {
#   this_row_brief <- hdcp_outcome_map[i,'out_column']
#   this_abbrev <- hdcp_outcome_map[i,'out_abbreviation']
#   print(this_row_brief)
#   rownames(n)[rownames(n)==this_row_brief] <- this_abbrev
# }


  datatable(n[2:nrow(n),],
            extensions=c('Buttons','FixedColumns','KeyTable','ColReorder'),
            caption=cap,
            options= list(
              dom='Blfrtip',
              paging=T,
              extensions="Buttons",
              colnames=colnames(n),
              order=F,
              columnDefs = list(
                list(className = 'dt-center', targets = "_all"),
                list(width = '200px', targets = 0)),
              buttons = c('copy','csv', 'excel'),
              pageLength=plength,
              keys=T,
              scrollCollapse=F,
#             scrollY="100vh",
              searchHighlight=T,
              scrollX=T,
              fixedColumns=list(
                leftColumns=1),
              # fixedHeader=T,
              rowReorder=F,
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}")
              )
            )  
}
  
### Takes a print(Hmisc:summary.formula) statement and vector of rownames and displays as a DT datatable with 
### horizontal and vertical scrolling


################################################################################################



cleanup_Hsummary_rawlabels <- function(y,cap="") {
  
  n <- gsub("\\+\\/\\-\\s+", "±", trimws(y))
  n <- gsub("\\+\\/\\-", "±", trimws(y))
  n <- gsub("\\s+", " ", n)
  n <- gsub("\\(\\s+", "\\(", n)
  n <- gsub("±\\s+", "±", n)
  colnames(n) <- n[1,]
  colnames(n) <- gsub("([0-9])\\.\\s+","",colnames(n))
  rownames(n) <- gsub("([0-9])\\.\\s+","",rownames(n))
  
  n <-  apply(n,MARGIN=c(1,2),function(x) swap_count_pct_Hmisc(x))
  
  rownames(n) <- gsub("\\s+:\\s+Yes","",rownames(n))
  rownames(n) <- gsub("_val","",rownames(n))
    maxwidth <- max(nchar(rownames(n)))
  
  rr <- rownames(n)
  
  for (i in 1:length(rr)) {
    this_row <- rr[i]
    if (grepl("\\s\\s\\s\\s",this_row)) {
      this_row <- trimws(this_row,"left")
      rr[i] <- this_row
    }
  }
  
  rownames(n) <- rr  
  return(n)
}


##########################################################################################

cleanup_Hsummary <- function(y,rowlkp,cap="") {
  
  n <- gsub("\\+\\/\\-\\s+", "±", trimws(y))
  n <- gsub("\\+\\/\\-", "±", trimws(y))
  n <- gsub("\\s+", " ", n)
  n <- gsub("\\(\\s+", "\\(", n)
  n <- gsub("±\\s+", "±", n)
  colnames(n) <- n[1,]
  colnames(n) <- gsub("([0-9])\\.\\s+","",colnames(n))
  rownames(n) <- gsub("([0-9])\\.\\s+","",rownames(n))
  
  n <-  apply(n,MARGIN=c(1,2),function(x) swap_count_pct_Hmisc(x))
  
  for (i in 1:nrow(rowlkp)) {
    this_row_brief <- paste("^",rowlkp[i,'out_column'],sep="")
    this_abbrev <- rowlkp[i,'out_abbreviation']
    rownames(n) <- gsub(this_row_brief,this_abbrev,rownames(n))
  }
  rownames(n) <- gsub("\\s+:\\s+Yes","",rownames(n))
  rownames(n) <- gsub("_val","",rownames(n))
  
  maxwidth <- max(nchar(rownames(n)))
  
  rr <- rownames(n)
  
  for (i in 1:length(rr)) {
    this_row <- rr[i]
    if (grepl("\\s\\s\\s\\s",this_row)) {
      this_row <- trimws(this_row,"left")
      rr[i] <- this_row
    }
  }
  
  rownames(n) <- rr  
  return(n)
}




############################################################################################################3

fix_wrap_kable <- function(kbl) {
  
  kbl <- kbl %>% 
    str_remove(paste0("\\\\caption[{].+[}]\\n")) %>% # Removes the first caption
    str_replace("\\\\end[{]tabular[}]",
                paste0("\\\\end{tabular}\n\\\\caption{",attributes(kbl)$kable_meta$caption %>% # Adds the new caption using the string supplied in the kable 'caption' argument.
                         # The '{0,100}' is just a hack around look behind needing a set length, so the function will not work if the table label is longer 
                         str_replace("(?<=[(]\\\\#.{0,100})[)]","}") %>% # Fixes and issue with pandoc syntax being mixed with LaTeX 
                         str_replace("\\label[{]|[(]\\\\#","\\\\\\\\label{"),"}\n")) %>% # Adds an appropriate amount of backslashes before "\label"
    set_attributes(attributes(kbl)) # Ensures that the returned object is still a kable
  
  attributes(kbl)$kable_meta$caption <- NA # Removes the caption from the metadata
  
  return(kbl)
}

####

cleancox_kable <- function(bbb,header_color='black',rownms=NA) {
  
  
c <- as.data.frame(round(summary(bbb)$conf.int[,c(1,3,4)],2))

if (ncol(c)==1) 
  {c <- t(c)}

c <- as.data.frame(c)


p <- as.data.frame(summary(bbb)$coefficients)

c$hr <- as.data.frame(paste(sprintf("%.2f",c[,1]),
              " [",
              sprintf("%.2f",c[,2]),
              "-",
              sprintf("%.2f",c[,3]),
              "]",
              sep=""))
p$pval <- format.pval(p[,5],eps=0.001,nsmall=3,digits=3)

c$pval <- p$pval

if (length(rownms)==1) {
if (is.na(rownms)) {
  rownms <- names(bbb$coefficients)
}
}
rownames(c) <- rownms


knitr::kable(c[,c(4,5)],
             col.names=c("Hazard ratio","p-value"),
             align='c',
             row.names=T,
             format="html",
             table.attr="style='width:100%;'") %>%
  kable_styling(bootstrap_options = "hover") %>%
  row_spec(0,background=col2hex(header_color),color="white")
}


###########################################################################

extract_pct <- function(a) {
  as.numeric(substr(a,start=str_locate(a,"\\(")+1,stop=str_locate(a,"%\\)")-1))
}


