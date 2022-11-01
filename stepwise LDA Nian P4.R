require(MASS) # required for LDA procedure
require(klaR) # required for stepwise refinement of LDA
require(rgr) # required for additive-logratio (alr) multivariate transformation
#
# create a data subset for the robust LDA by choosing relevant columns from default data
# not including variables with lots of na, replacing fe.oes with fe.ms as they are 
#    well correlated (pearsons r 0.99) and fe.oes has many na values
data2<-as.matrix(na.omit(cbind(nx.clr[c("Ba.ms", "Cd.ms", "Co.ms", "Cr.ms", "Cu.ms", "Fe.ms",
                                        "Mn.ms", "Ni.oes", "Pb.ms", "Rb.ms", "Sr.ms", 
                                        "Th.ms", "Ti.ms", "U.ms",  "V.ms",  "Zn.ms", "REE.ms")])))
data2f <- na.omit(cbind(nx.clr[c("Type.S3","Redox","Site","Depth.cm","Ba.ms", "Cd.ms", "Co.ms", 
                                 "Cr.ms", "Cu.ms", "Fe.ms","Mn.ms", "Ni.oes","Pb.ms", 
                                 "Rb.ms","Sr.ms","Th.ms", "Ti.ms","U.ms","V.ms","Zn.ms","REE.ms")]))
data2.alr <- alr(data2, j=4)
colnames(data2.alr) <- c("Ba", "Cd", "Co", "Cu", "Fe", "Mn", "Ni","Pb", "Rb", 
                         "Sr","Th", "Ti", "U",  "V",  "Zn", "REE")
# colnames(data2.alr) <- substr(colnames(data2.alr),1,str_locate(colnames(data2.alr),"s")[,1]-1)
data2f.alr <- cbind(data2f[,1:4],data2.alr[,1:ncol(data2.alr)])
summary(data2f.alr)
str(data2f.alr)
# specify default data object
attach(data2f.alr)
#
stepwise.lda <- stepclass(formula = Type.S3~Ba+ Cd+ Co+ Cu+ Fe+ Mn+ Ni+ Pb+ Rb+  
            Sr+ Th+ Ti+ U+ V+ Zn+ REE, data=data2f.alr, method="lda", improvement=0.002, 
            direction="backward", criterion="AC")
summary(stepwise.lda)
#
directs <- c("backward","forward","both")
minimp <- c(0.001,0.002,0.003,0.005,0.01,0.02,0.03,0.05)
crits=c("CR","AC","AS","CF","CFvec")
cat("Sort order, Direction of steps",",", "Improvement tolerance",",","Improvement Criterion",",",
    "Final predictor variables",",","Criterion value\n")
for (k in 1:3) {
  for (j in 1:8) {
    for (i in 1:5){
    # cat("\n",j,i,"Stepwise LDA, min. improvement tolerance =",minimp[j], ", Direction = both,
    #   Criterion = ", crits[i],"\n")
    stepwise.lda <- stepclass(formula = Type.S3~Ba+ Cd+ Co+ Cu+ Fe+ Mn+ Ni+ Pb+ Rb+
                              Sr+ Th+ Ti+ U+ V+ Zn+ REE, data=data2f.alr, method="lda",
                              improvement=minimp[j], direction=directs[k], criterion=crits[i])
    # print(stepwise.lda)
    finvars <- as.character(stepwise.lda$formula[3]) 
    finperf <- as.numeric(stepwise.lda$process[nrow(stepwise.lda$process),4])
    perfmeas <- stepwise.lda$performance.measure
    cat(((k*100)+((j*10)+i)),", forward-backward",",",minimp[j],",",perfmeas,",",finvars,",",finperf,"\n")
    }
  }
}  
stepwise.lda$call
stepwise.lda$method
stepwise.lda$model
stepwise.lda$performance.measure
stepwise.lda$process
stepwise.lda$result.pm
stepwise.lda$runtime
stepwise.lda$start.variables
#
# Details [for criterion=c("CR","AC","AS","CF","CFvec")]
# The correctness rate is the estimator for the correctness of a classification rule (1-error rate).
# The accuracy is based on the euclidean distances between (scaled) membership vectors and the vectors representing the true class corner. These distances are standardized so that a measure of 1 is achieved if all vectors lie in the correct corners and 0 if they all lie in the center.
# Analougously, the ability to seperate is based on the distances between (scaled) membership vectors and the vector representing the corresponding assigned class corner.
# The confidence is the mean of the membership values of the assigned classes.
# 
# Value
# A list with elements:
#   
# CR	Correctness Rate
# AC	Accuracy
# AS	Ability to Seperate
# CF	Confidence
# CFvec	Confidence for each (true) class
