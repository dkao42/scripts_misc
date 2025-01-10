
plot_LCA_outcomes <- function(time,cens,cens_val=1,lca_class,mname) {
  if (dir.exists(mname)==F) {dir.create(mname)}
  pdf(paste(mname,"/LCA Model ",modelname," ",outcome," according to class",".pdf",sep=""))
  plot(survfit(Surv(as.numeric(time)/30,cens==cens_val)~as.factor(lca_class),conf.type="none"),col=c(1:14),xlim=c(0,42),ylim=c(0.5,1))
  title(main=paste('Primary outcome according to LCA model:',modelname),xlab='Time since enrollment (months)',ylab='Proportion event free')
  dev.off()
}