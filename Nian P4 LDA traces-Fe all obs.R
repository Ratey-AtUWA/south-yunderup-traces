# data object data2f.alr has non-predictor variables/factors in [,1:7]
windowsFonts(nar = windowsFont("Arial Narrow"))
windowsFonts() # check available fonts
require(MASS)
########## PREPARE DATA ##########
require(rgr) # required for several (including robust) multivariate procedures
require(stringr) # to manipulate variable names for prettier graphs
#
# create a data subset for the robust PCA by choosing relevant columns from default data
data2<-as.matrix(na.omit(cbind(nx.clr[c("As.oes","Ba.ms", "Cd.ms", "Co.ms", "Cr.ms", "Cu.ms", "Fe.oes",
                                        "Mn.ms", "Mo.ms", "Ni.oes","P.oes", "Pb.ms", "Rb.ms", "S.oes", 
                                        "Sr.ms","Th.ms", "Ti.ms", "U.ms",  "V.ms",  "Zn.ms", "REE.ms")])))
data2f <- na.omit(cbind(nx.clr[c("Type.S3","Redox","Site","Depth.cm","As.oes","Ba.ms", "Cd.ms", "Co.ms", 
                                 "Cr.ms", "Cu.ms", "Fe.oes","Mn.ms", "Mo.ms", "Ni.oes","P.oes", "Pb.ms", 
                                 "Rb.ms","S.oes","Sr.ms","Th.ms", "Ti.ms","U.ms","V.ms","Zn.ms","REE.ms")]))
data2.alr <- alr(data2, j=7)
colnames(data2.alr) <- c("As","Ba", "Cd", "Co", "Cr", "Cu", "Mn", "Mo", "Ni","P", "Pb", "Rb", "S", 
                         "Sr","Th", "Ti", "U",  "V",  "Zn", "REE")
# colnames(data2.alr) <- substr(colnames(data2.alr),1,str_locate(colnames(data2.alr),"s")[,1]-1)
data2f.alr <- cbind(data2f[,1:4],data2.alr[,1:ncol(data2.alr)])
summary(data2f.alr)
str(data2f.alr)
# specify default data object
#
attach(data2f.alr)
########## RUN LDA ##########
lda.nx.alr <- lda(formula = Type.S3~As+Ba+ Cd+ Co+ Cr+ Cu+ 
                    Mn+ Mo+ Ni+P+ Pb+ Rb+ S+ 
                    Sr+Th+ Ti+ U+  V+  Zn+ REE, data=data2f.alr, prior=c(1,1,1)/3)
prop = 100*lda.nx.alr$svd^2/sum(lda.nx.alr$svd^2)
{cat("Discriminant 1 explains",signif(prop[1],3),"% of between groups variance\n")
  cat("Discriminant 2 explains",signif(prop[2],3),"% of between groups variance\n")
  cat("Discriminant 3 explains",signif(prop[3],3),"% of between groups variance\n")}
# ls(lda.nx.alr)
########## CALCULATE 'OBSERVATION SCORES' FOR TRAINING DATA ##########
lda.nx.alr.values <- predict(lda.nx.alr, data2f.alr[c("As","Ba", "Cd", "Co", "Cr", "Cu", "Mn", "Mo", "Ni","P", "Pb", "Rb", "S", 
                                                          "Sr","Th", "Ti", "U",  "V",  "Zn", "REE")], na.rm=T)
########## CALCULATE PREDICTIONS FOR NEW DATA ##########
# lda.nx.alr.pred <- predict(lda.nx.alr, data2f.alr.pred[c("As","Ba","Ca","Cd","Ce","Co","Cr","Cu","Fe",
#   "Gd","K","La","Mg","Mn","Na","Ni","P","Pb","S","Sr","Ti","V","Y","Zn")], na.rm=T)
#
########## PLOT SEPARATIONS ##########
par(mfrow=c(3,1),lend=2,ljoin=1,font.lab=2,cex.main=2, cex.lab=2, cex.axis=1.6)
ldahist(data = lda.nx.alr.values$x[,1], g=data2f.alr$Type.S3)
# ldahist(data = lda.nx.alr.pred$x[,2], g=data2f.alr$Type.S3)
#
########## NUMERICAL DATA ##########
print(lda.nx.alr$scaling,digits=3) # COEFFICIENTS
#
########### SET OF 2 LDA PLOTS ##########
dev.new(width=13.6, height=7.0)
palette(c("purple2","red3","green4","dodgerblue2"))
par(mfrow=c(1,2),mar=c(4,4,2,1),lend=2,ljoin=1,font.lab=2,cex.main=1.25, mgp=c(2.2,0.8,0), family="sans")
########## PLOT VARIABLE WEIGHTINGS ##########
plot(lda.nx.alr$scaling[,1],lda.nx.alr$scaling[,2],col="skyblue4",pch=18,lwd=1,cex=1.4,cex.lab=1.4,
     cex.axis=1.4, xlab="Linear Discriminant [1]", ylab="Linear Discriminant [2]",
     xlim=c(-9,15), main="(a) Variable Coefficients, data = data2f.alr, Ref. element Fe", family="sans") #  ylim=c(-2,6.5),
mtext(text="  (a)", cex=1.6, side=3, line=-2,adj=0.03, family="sans")
abline(v=0,col="grey",lty=2)
abline(h=0,col="grey",lty=2)
text(lda.nx.alr$scaling[,1],lda.nx.alr$scaling[,2],labels=rownames(lda.nx.alr$scaling),pos=4, cex=1.4, family="nar")
legend("bottomright",legend=c("Variables"), bty="o", pch=c(18), col=c("skyblue4"), cex=1.4, pt.cex=1.4,
       inset=0.01, box.col="grey90", bg="white", box.lwd=2, y.intersp=0.7)
#
########## PLOT TRAINING DATA OBSERVATION SCORES ##########
plot(lda.nx.alr.values$x[,1], lda.nx.alr.values$x[,2], 
     col=c(1,2,3,4)[lda.nx.alr.values$class],
     pch=c(0,15,1,3)[lda.nx.alr.values$class], lwd=c(2,1,2,2)[lda.nx.alr.values$class],
     cex=c(1.8,1.8,2,1.8)[lda.nx.alr.values$class],cex.axis=1.4, cex.lab=1.4, 
     xlab="Linear Discriminant [1]", ylab="Linear Discriminant [2]",
     main="(b) Coordinates of observations, data = data2f.alr, Ref. element Fe", family="sans")
mtext(text="  (b)", cex=1.6, side=3, line=-2,adj=0.03, family="sans")
abline(v=0,col="grey",lty=2)
abline(h=0,col="grey",lty=2)
legend("bottomleft",legend=levels(data2f.alr$Type.S3),
       col=c(1,2,3,4), pch=c(0,15,1,3),pt.lwd=c(2,1,2,2),title=expression(italic("Sample type")),
       bty="o", box.col="grey90", box.lwd=2, inset=0.01, pt.cex=c(1.8,1.8,2,1.8), cex=1.4, 
       text.col=c(1,2,3,4), title.col="black", y.intersp=0.99, bg="white")
#
#
#
#
#
########## NEW DATA PREDICTED OBSERVATION SCORES ##########
plot(lda.nx.alr.pred$x[,1], lda.nx.alr.pred$x[,2], col=c(2,"#006666","grey60")[lda.nx.alr.values$class], pch=c(0,2,1)[lda.nx.alr.values$class],
     lwd=c(2,2,2)[lda.nx.alr.values$class],cex=c(1.8,1.8,2)[lda.nx.alr.values$class],cex.axis=1.4, cex.lab=1.4,
     xlab="Linear Discriminant [1]", ylab="Linear Discriminant [2]",
     main="(c) Predictions (symbols) vs. observations (text) for new obs.")
abline(v=0,col="grey",lty=2)
abline(h=0,col="grey",lty=2)
text(lda.nx.alr.pred$x[,1], lda.nx.alr.pred$x[,2], labels=substr(lda.nx.alr.pred$class,1,3),
     col=c(2,"blue4","grey60")[lda.nx.alr.pred$class], offset=0.25, pos=1, cex=1.1) # pos=1, 
legend("bottomright",legend=c("Sediment (Sed)","Soil (Soi)","Street dust (Str)"),
       col=c(2,"#006666","grey60"),pch=c(0,2,1), pt.lwd=c(2,2,2),title=expression(italic("Sample type")),
       bty="o", box.col="grey90", box.lwd=3,inset=0.02, pt.cex=c(1.8,1.8,2), cex=1.25,
       text.col=c(2,"blue4","grey40"), title.col="black")
#
########## 'DEFAULT' PLOT FOR LDA OBJECT ##########
# plot(lda.nx.alr, abbrev=T)
# plot(lda.nx.alr, col=c(1,2,3,4)[lda.nx.alr.values$class], abbrev=T)
# plot(lda.nx.alr, abbrev=F, cex=1., cex.lab=1.4, cex.axis=1.4, xlim=c(-5,5), ylim=c(-5,5), 
xlab="Linear Discriminant [1]", ylab="Linear Discriminant [2]", main="Observation scores",
col=c(1,2,3,4)[lda.nx.alr.values$class])
