

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


library(xtable)


observado <-  c(0,1,1,1,0,0,0,0,0,0)
ana <- abs(observado-0.6)
berta <-  c(0,observado[-length(observado)])


metricas <- matrix(NA,nrow=9,ncol=3)
colnames(metricas) <- c("Ana", "Berta","Gana")
rownames(metricas) <- c("Mean squared error",
                        "Mean absolute error",
                        "Hit rate",
                        "Accuracy",
                        "Precsion",
                        "Recall",
                        "F1 score", 
                        "ROC area",
                        "Evidence"
                        )

mse <- function(observado,predicho){
  return(sum((observado-predicho)^2))
}

mae <- function(observado,predicho){
  return(sum(abs(observado-predicho)))
}

hitRate <- function(observado,predicho){
  n <- length(observado)
  return((1/n)*sum(1-abs(observado-predicho)))
}

accuracy <- function(observado,predicho,umbral=0.5){
  n <- length(observado)
  lluvia <- predicho > umbral
  TN <- sum(observado==0 & !lluvia)
  TP <- sum(observado==1 & lluvia)
  return((TP+TN)/n)
}

precision <- function(observado,predicho,umbral=0.5){
  n <- length(observado)
  lluvia <- predicho > umbral
  FP <- sum(observado[lluvia]==0 & lluvia[lluvia])
  TP <- sum(observado[lluvia]==1 & lluvia[lluvia])
  res <- round(TP/(TP + FP),2)
  return(res)
}

recall <-  function(observado,predicho,umbral=0.5){
  n <- length(observado)
  lluvia <- predicho > umbral
  FN <- sum(observado[!lluvia]==0 & !lluvia[!lluvia])
  TP <- sum(observado[lluvia]==1 & lluvia[lluvia])
  res <- round(TP/(TP + FN),2)
  return(res)  
}

f_score <- function(observado,predicho,umbral=0.5,beta=1){
  r <- recall(observado,predicho)
  p <- recall(observado,predicho)
  res <- (1+beta^2)* (  (r*p)/(r+p*(beta^2))  )
  return(round(res,2))  
}


roc_area <- function(observado,predicho){
  truePositiveRate_ana <- c(0) #VP/P
  falsePositiveRate_ana <- c(0) #FN/N
  positivos <- observado==1
  negativos <- !positivos
  N <- sum(negativos)
  P <- sum(positivos)
  grilla <- seq(0,1,by=0.1)
  for(i in rev(grilla) ){#i=0.7
    
    predicciones_ana <- predicho>=i
    
    VP_ana <-  sum(positivos&predicciones_ana)
    truePositiveRate_ana <- c(truePositiveRate_ana,VP_ana/P)
    
    FP_ana <- sum(!positivos&predicciones_ana)
    falsePositiveRate_ana <- c(falsePositiveRate_ana, FP_ana/N)
    
  }
  
  base <- c()
  altura <- c()
  for(i in seq(length(falsePositiveRate_ana))){
    noEsta <- sum(base == falsePositiveRate_ana[i])>0 &  sum(altura == truePositiveRate_ana[i])>0
    if (!noEsta){
      base <- c(base,falsePositiveRate_ana[i])
      altura <- c(altura,truePositiveRate_ana[i]) 
    }
  }  
  base <- c(0,base[-1] - base[-length(base)])
  altura_anterior <- c(0,altura[-length(altura)]) 
  
  area <- sum(altura_anterior*base + ((altura-altura_anterior)*base)/2)
  return(round(area,2))
}

evidence <- function(observado,predicho){
  return(prod(abs(1 - observado-predicho)))
}

metricas["Mean squared error",] <- c(mse(observado,ana),mse(observado,berta),"Berta")
metricas["Mean absolute error",] <- c(mae(observado,ana),mae(observado,berta),"Berta")
metricas["Hit rate",] <- c(hitRate(observado,ana),hitRate(observado,berta),"Berta")
metricas["Accuracy",] <- c(accuracy(observado,ana),accuracy(observado,berta),"Berta")
metricas["Precsion",] <- c(precision(observado,ana),precision(observado,berta),"Berta")
metricas["Recall",] <- c(recall(observado,ana),precision(observado,berta),"Berta")
metricas["F1 score",] <- c(f_score(observado,ana),f_score(observado,berta),"Berta")
metricas["ROC area",] <- c(roc_area(observado,ana),roc_area(observado,berta),"Berta")
metricas["Evidence",] <- c(evidence(observado,ana),evidence(observado,berta),"Ana")



xtable(metricas)

######
dev.off()
system(paste("pdfcrop -m '0 0 0 0'",paste0(nombre,".pdf") ,paste0(nombre,".pdf")))
setwd(oldwd)
par(oldpar, new=F)
