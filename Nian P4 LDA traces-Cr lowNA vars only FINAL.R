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
summary(nx.clr[c("Ba.ms", "Cr.ms", "Pb.ms", "Sr.ms", 
                 "Th.ms", "Ti.ms", "U.ms", "REE.ms")])
data2<-as.matrix(na.omit(cbind(nx.clr[c("Ba.ms", "Cr.ms", "Pb.ms", "Sr.ms", 
                                        "Th.ms", "Ti.ms", "U.ms", "REE.ms")])))
data2f <- na.omit(cbind(nx.clr[c("Type.S3","Redox","Site","Depth.cm","Ba.ms", 
          "Cr.ms", "Pb.ms", "Sr.ms", "Th.ms", "Ti.ms", "U.ms", "REE.ms")]))
data2.alr <- alr(data2, j=2)
colnames(data2.alr) <- c("Ba", "Pb", "Sr","Th", "Ti", "U", "REE")
# colnames(data2.alr) <- substr(colnames(data2.alr),1,str_locate(colnames(data2.alr),"s")[,1]-1)
data2f.alr <- cbind(data2f[,1:4],data2.alr[,1:ncol(data2.alr)])
# summary(data2f.alr)
numSummary(data2f.alr[,5:ncol(data2f.alr)])
str(data2f.alr)
# specify default data object
#
attach(data2f.alr)
########## RUN LDA ##########
lda.nx.alr <- lda(formula = Type.S3~Ba+ Pb+ Sr+ Th+ Ti+ U+ REE, data=data2f.alr, prior=c(1,1,1)/3)
prop = 100*lda.nx.alr$svd^2/sum(lda.nx.alr$svd^2)
{cat("Discriminant 1 explains",signif(prop[1],3),"% of between groups variance\n")
  cat("Discriminant 2 explains",signif(prop[2],3),"% of between groups variance\n")}
# ls(lda.nx.alr)
########## CALCULATE 'OBSERVATION SCORES' FOR TRAINING DATA ##########
lda.nx.alr.values <- predict(lda.nx.alr, 
                             data2f.alr[c("Ba", "Pb", "Sr","Th", "Ti", "U", "REE")], na.rm=T)
########## CALCULATE PREDICTIONS FOR NEW DATA ##########
# lda.nx.alr.pred <- predict(lda.nx.alr, data2f.alr.pred[c("As","Ba","Ca","Cd","Ce","Co","Cr","Cu","Fe",
#   "Gd","K","La","Mg","Mn","Na","Ni","P","Pb","S","Sr","Ti","V","Y","Zn")], na.rm=T)
#
########## PLOT SEPARATIONS ##########
# par(mfrow=c(3,1),lend=2,ljoin=1,font.lab=2,cex.main=2, cex.lab=2, cex.axis=1.6)
ldahist(data = lda.nx.alr.values$x[,1], g=data2f.alr$Type.S3)
# ldahist(data = lda.nx.alr.values$x[,2], g=data2f.alr$Type.S3)
#
########## NUMERICAL DATA ##########
print(lda.nx.alr$scaling,digits=3) # COEFFICIENTS
#
########### SET OF 2 LDA PLOTS ##########
# dev.new(width=13.6, height=7.0)
palette(c("firebrick","#006699","saddlebrown"))
par(mfrow=c(1,2),mar=c(4,4,2,1),lend=2,ljoin=1,font.lab=2,cex.main=.7, mgp=c(2.2,0.8,0), family="sans")
########## PLOT VARIABLE WEIGHTINGS ##########
plot(lda.nx.alr$scaling[,1],lda.nx.alr$scaling[,2],col="skyblue4",pch=18,lwd=1,cex=1.4,
     cex.lab=1.4, cex.axis=1.4, xlab="Linear Discriminant [1]", 
     ylab="Linear Discriminant [2]", xlim=c(-3,3), ylim=c(-3,3), 
     main="(a) Variable Coefficients, data = data2f.alr, Ref. element Cr", family="nar") #  ylim=c(-2,6.5),
mtext(text="  (a)", cex=1.6, side=3, line=-2,adj=0.03, family="sans")
abline(v=0,col="grey",lty=2)
abline(h=0,col="grey",lty=2)
text(lda.nx.alr$scaling[,1],lda.nx.alr$scaling[,2],labels=rownames(lda.nx.alr$scaling),pos=4, 
     cex=1.4, family="nar")
legend("bottomright",legend=c("Variables"), bty="o", pch=c(18), col=c("skyblue4"), cex=1., 
       pt.cex=1.4, inset=0.01, box.col="grey90", bg="white", box.lwd=2, y.intersp=0.7)
#
########## PLOT TRAINING DATA OBSERVATION SCORES ##########
plot(lda.nx.alr.values$x[,1], lda.nx.alr.values$x[,2], 
     col=c(1,2,3)[lda.nx.alr.values$class],
     pch=c(1,15,0)[lda.nx.alr.values$class], lwd=c(2,1,2)[lda.nx.alr.values$class],
     cex=c(2,1.8,1.7)[lda.nx.alr.values$class],cex.axis=1.4, cex.lab=1.4, 
     xlab="Linear Discriminant [1]", ylab="Linear Discriminant [2]",
     main="(b) Coordinates of observations, data = data2f.alr, Ref. element Cr", family="nar")
mtext(text="  (b)", cex=1.6, side=3, line=-2,adj=0.03, family="sans")
abline(v=0,col="grey",lty=2)
abline(h=0,col="grey",lty=2)
legend("topright",legend=levels(data2f.alr$Type.S3), col=c(1,2,3), pch=c(1,15,0),
       pt.lwd=c(2,1,2), title=expression(italic("Sample type")), bty="o", box.col="grey90", 
       box.lwd=2, inset=0.01, pt.cex=c(2,1.8,1.7), cex=1., 
       text.col=c(1,2,3), title.col="black", y.intersp=0.99, bg="white")
palette('default')
#
par(mfrow=c(1,1), cex.lab=1)
sp(lda.nx.alr.values$x[,2]~lda.nx.alr.values$x[,1]|lda.nx.alr.values$class, spread=F, smooth=F, reg.line=F,
   ellipse=T, levels=c(0.95), xlim=c(-6,6), ylim=c(-4,8), legend.coords="topright", cex=1.4, cex.lab=1.5, 
   cex.axis=1.4, grid=F, pch=c(1,15,0), legend.title="Sample Type")
#