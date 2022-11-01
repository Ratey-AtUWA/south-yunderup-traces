# data object data2f.alr has non-predictor variables/factors in [,1:4]
windowsFonts(nar = windowsFont("Arial Narrow"))
windowsFonts() # check available fonts
require(MASS)
########## PREPARE DATA ##########
require(rgr) # required for several (including robust) multivariate procedures
require(stringr) # to manipulate variable names for prettier graphs
#
# create a data subset for the robust pca by choosing relevant columns from default data
# not including variables with lots of na, replacing fe.oes with fe.ms as they are 
#    well correlated (pearsons r 0.99) and fe.oes has many na values
data2<-as.matrix(na.omit(cbind(nx.clr[c("Ba.ms", "Cr.ms",
                                        "Ni.oes", "Sr.ms", 
                                        "Th.ms", "Ti.ms", "U.ms")])))
data2f <- na.omit(cbind(nx.clr[c("Type.S3","Redox","Site","Depth.cm","Ba.ms", 
                                 "Cr.ms", "Ni.oes", 
                                 "Sr.ms","Th.ms", "Ti.ms","U.ms")]))
data2.alr <- alr(data2, j=4)
colnames(data2.alr) <- c("Ba", "Ni", 
                         "Sr","Th", "Ti", "U")
# colnames(data2.alr) <- substr(colnames(data2.alr),1,str_locate(colnames(data2.alr),"s")[,1]-1)
data2f.alr <- cbind(data2f[,1:4],data2.alr[,1:ncol(data2.alr)])
summary(data2f.alr)
str(data2f.alr)
# specify default data object
#
attach(data2f.alr)
########## RUN LDA ##########
lda.nx.alr <- lda(formula = Type.S3~Ba+ Ni+   
                      Sr+ Th+ Ti+ U, data=data2f.alr, prior=c(1,1,1)/3)
prop = 100*lda.nx.alr$svd^2/sum(lda.nx.alr$svd^2)
# ls(lda.nx.alr)
########## CALCULATE 'OBSERVATION SCORES' FOR TRAINING DATA ##########
lda.nx.alr.values <- predict(lda.nx.alr, data2f.alr[c("Ba", "Ni",  
                                                          "Sr","Th", "Ti", "U")], na.rm=T)
########## CALCULATE PREDICTIONS FOR NEW DATA ##########
# lda.nx.alr.pred <- predict(lda.nx.alr, data2f.alr.pred[c("As","Ba","Ca","Cd","Ce","Co","Cr","Cu","Fe",
#   "Gd","K","La","Mg","Mn","Na","Ni","P","Pb","S","Sr","Ti","V","Y","Zn")], na.rm=T)
#
########## PLOT SEPARATIONS ##########
# par(mfrow=c(2,1),lend=2,ljoin=1,font.lab=2,cex.main=2, cex.lab=2, cex.axis=1.6)
ldahist(data = lda.nx.alr.values$x[,1], g=data2f.alr$Type.S3)
# ldahist(data = lda.nx.alr.values$x[,2], g=data2f.alr$Type.S3)
#
########## NUMERICAL DATA ##########
print(lda.nx.alr$scaling,digits=4) # COEFFICIENTS
#
########### SET OF 2 LDA PLOTS ##########
# dev.new(width=13.6, height=7.0)
par(mfrow=c(1,2),mar=c(4,4,2,1),lend=2,ljoin=1,font.lab=2,cex.main=.7, mgp=c(2.2,0.8,0), family="sans")
########## PLOT VARIABLE WEIGHTINGS ##########
plot(lda.nx.alr$scaling[,1],lda.nx.alr$scaling[,2],col="skyblue4",pch=18,lwd=1,cex=1.4,
     cex.lab=1.4, cex.axis=1.4, xlab="Linear Discriminant [1]", 
     ylab="Linear Discriminant [2]", xlim=c(-4,4), ylim=c(-4,4), 
     main="(a) Variable Coefficients, data = data2f.alr, Ref. element Cr", family="nar") #  
mtext(text="  (a)", cex=1.6, side=3, line=-2,adj=0.03, family="sans")
abline(v=0,col="grey",lty=2)
abline(h=0,col="grey",lty=2)
palette(colors())
text(lda.nx.alr$scaling[,1],lda.nx.alr$scaling[,2],labels=rownames(lda.nx.alr$scaling),pos=4, 
     cex=1., family="nar", col=seq(53,500,by=13))
legend("bottomright",legend=c("Variables"), bty="o", pch=c(18), col=c("skyblue4"), cex=1., 
       pt.cex=1.4, inset=0.01, box.col="grey90", bg="white", box.lwd=2, y.intersp=0.7)
#
########## PLOT TRAINING DATA OBSERVATION SCORES ##########
palette(c("firebrick","#006699","saddlebrown"))
plot(lda.nx.alr.values$x[,1], lda.nx.alr.values$x[,2], 
     col=c(1,2,3)[lda.nx.alr.values$class],
     pch=c(1,15,0)[lda.nx.alr.values$class], lwd=c(2,1,2)[lda.nx.alr.values$class],
     cex=c(2,1.8,1.7)[lda.nx.alr.values$class],cex.axis=1.4, cex.lab=1.4, 
     xlab="Linear Discriminant [1]", ylab="Linear Discriminant [2]",
     main="(b) Coordinates of observations, data = data2f.alr, Ref. element Cr", family="nar")
mtext(text="  (b)", cex=1.6, side=3, line=-2,adj=0.03, family="sans")
abline(v=0,col="grey",lty=2)
abline(h=0,col="grey",lty=2)
legend("bottom",legend=levels(data2f.alr$Type.S3), col=c(1,2,3), pch=c(1,15,0),
       pt.lwd=c(2,1,2), title=expression(italic("Sample type")), bty="o", box.col="grey90", 
       box.lwd=2, inset=0.01, pt.cex=c(1.2,1.1,1), cex=0.8, 
       text.col=c(1,2,3), title.col="black", y.intersp=0.99, bg="white")
{cat("Discriminant 1 explains",signif(prop[1],3),"% of between groups variance\n")
  cat("Discriminant 2 explains",signif(prop[2],3),"% of between groups variance\n")}
########## NUMERICAL DATA ##########
print(lda.nx.alr$scaling,digits=4) # COEFFICIENTS
#
# palette('default')
#