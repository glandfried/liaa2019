oldpar <- par(no.readonly = TRUE)
oldwd <- getwd()
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
nombre.R <-  sys.frame(1)$ofile
require(tools)
nombre <- print(file_path_sans_ext(nombre.R))
pdf(paste0(nombre,".pdf"))
setwd(this.dir)
##################################################


observado <-  c(0,1,1,1,0,0,0,0,0,0)
ana <- abs(observado-0.6)
berta <-  c(0,observado[-length(observado)])

truePositiveRate_ana <- c(0) #VP/P
falsePositiveRate_ana <- c(0) #FN/N
truePositiveRate_berta <- c(0) #VP/P
falsePositiveRate_berta <- c(0) #FN/N
positivos <- observado==1
negativos <- !positivos
N <- sum(negativos)
P <- sum(positivos)
grilla <- seq(0,1,by=0.1)
for(i in rev(grilla) ){#i=0.7
  
  predicciones_ana <- ana>=i
  predicciones_berta <- berta>=i
  
  VP_ana <-  sum(positivos&predicciones_ana)
  truePositiveRate_ana <- c(truePositiveRate_ana,VP_ana/P)
  VP_berta <- sum(positivos&predicciones_berta)
  truePositiveRate_berta <- c(truePositiveRate_berta,VP_berta/P)
  
  FP_ana <- sum(!positivos&predicciones_ana)
  falsePositiveRate_ana <- c(falsePositiveRate_ana, FP_ana/N)
  FP_berta <- sum(!positivos&predicciones_berta)
  falsePositiveRate_berta <- c(falsePositiveRate_berta, FP_berta/N)
}
plot(falsePositiveRate_ana,truePositiveRate_ana,type="l",lty=2,lwd=2
     ,axes=F,ylab="",xlab="")
lines(falsePositiveRate_berta,truePositiveRate_berta,lty=4,lwd=2)
lines(c(0,1),c(0,1),lty=3)

axis(side=2, labels=NA,cex.axis=0.6,tck=0.015)
axis(side=1, labels=NA,cex.axis=0.6,tck=0.015)
axis(lwd=0,side=1, cex.axis=1.25,line=-0.85,tck=0.015)
axis(lwd=0,side=2, cex.axis=1.25,line=-0.85,tck=0.015)
mtext(text ="True Positive Rate" ,side =2,line=2,cex=1.75)
mtext(text ="False Positive Rate" ,side =1,line=2,cex=1.75)

legend(0.05,0.95, c("Ana","Berta"),
       lty=c(2,4), lwd=2, cex=1.25, box.lty=0, bg="transparent")

######
dev.off()
system(paste("pdfcrop -m '0 0 0 0'",paste0(nombre,".pdf") ,paste0(nombre,".pdf")))
setwd(oldwd)
par(oldpar, new=F)
