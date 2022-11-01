# nx.clr <- 
#   read.csv("C:/Users/00028958/Documents/Postgraduate/2013.PhD Xu Nian/XuNian data//Paper_4/Trace elements-OES-MS-pH.clr.csv")
windowsFonts(nar = windowsFont("Ubuntu Condensed"))
#
#### run pca on prepared, clr-transformed data ####
nx.clr.noNA <- na.omit(cbind(nx.clr[c("Type.S3","As","Ba","Cd","Co","Cr","Cu","Fe",
                                      "Mn","Mo","Ni","P","Pb","Rb","S","Sr","Th","Ti",
                                      "U","V","Zn","REE")]))
PC.nx.clr<-prcomp(~As +Ba +Cd +Co +Cr +Cu +Fe +Mn
  +Mo +Ni +P +Pb +Rb +S +Sr +Th +Ti +U +V +Zn +REE, 
  scale=T, data=nx.clr.noNA) # 
ls(PC.nx.clr)
plot(PC.nx.clr)
biplot(PC.nx.clr, choices=c(1,2), col=c("black","grey50"),
       cex=c(1,0.8), font.lab=2, cex.lab=1.5, arrow.len = 0.1)
PC.nx.clr$x[,1:5]
PC.nx.clr$rotation[,1:5]
summary(PC.nx.clr)
PC.nx.clr$sdev[1:8]^2 # first eight (8) eigenvalues
#
# ________________________________________________________________ ____________ ________________
#
# win.metafile(height=7, width=7, file="PCA[1-3].nx.clr.20170828.wmf")
par(mfrow=c(1,1), mar=c(4,4,4,4), oma=c(0,0,0,0), mgp=c(2.4,0.7,0), lend=2, ljoin=1, font.lab=2)
#
biplot(PC.nx.clr, choices=c(1,3), col=c("transparent","black"), cex=c(0.2,1.0),
       cex.lab=1.5,cex.axis=1.4, expand=1., xlab="Component Loadings, PC1 (55.5% of variance)",
       ylab="Component Loadings, PC3 (7.3% of variance)", xlim=c(-0.25,0.25), ylim=c(-0.18,0.18), family="nar") # , font.lab=2
abline(v=0, lty=2, col="grey85")
abline(h=0, lty=2, col="grey85")
mtext(side=3, line=2.4, text="Observation scores, PC1",font=2, family='nar', cex=1.5)
mtext(side=4, line=2.4, text="Observation scores, PC3",font=2, family='nar', cex=1.5)
sf0 <- 0.9
points(PC.nx.clr$x[,1]*sf0, PC.nx.clr$x[,3]*sf0, pch=c(17,1,10)[nx.clr.noNA$Type.S3],
       col=c("firebrick","#226422","darkgoldenrod")[nx.clr.noNA$Type.S3], cex=1.5, 
       lwd=c(2,3,2)[nx.clr.noNA$Type.S3])
legend("topright", ncol=1, legend=levels(nx.clr.noNA$Type.S3),
       pch=c(17,1,10), col=c("firebrick","#226422","darkgoldenrod"), cex=1., pt.cex=1.5, 
       pt.lwd=c(2,3,2), bty="o", box.col="grey", inset=0.01, y.intersp=0.9, 
       title=expression(bold("Type"))) ### 
rm(sf0)
box()
dev.off() # IF USING RSTUDIO # USE options(device="RStudioGD") # THEN getOption('device') # IF USING RSTUDIO #
# ________________________________________________________________ ____________ ________________
#
biplot(PC.nx.clr, choices=c(1,3), col=c("transparent","black"), cex=c(0.2,1.0),
       cex.lab=1.5,cex.axis=1.4, expand=1., xlab="PC1 (55.5% of variance)",
       ylab="PC3 (7.3% of variance)", xlim=c(-0.3,0.3), ylim=c(-0.15,0.15), family="nar") # , font.lab=2
# abline(v=0, lty=2, col="grey85")
# abline(h=0, lty=2, col="grey85")
sf0 <- 1
points(PC.nx.clr$x[,1]*sf0,PC.nx.clr$x[,3]*sf0,pch=c(17,1,10)[nx.clr$Type.S3],
       col=c("firebrick","#226422","darkgoldenrod")[nx.clr$Type.S3], cex=1.5, lwd=c(2,2)[nx.clr$Type.S3])
legend("bottomleft", ncol=1, legend=levels(nx.clr$Type.S3),
       pch=c(17,1,10), col=c("firebrick","#226422","darkgoldenrod"), cex=1., pt.cex=1.5, pt.lwd=c(2,2,2),
       bty="o", box.col="grey", inset=0.01, y.intersp=0.95, title=expression(bold("Type"))) ### 
rm(sf0)
box()
# ________________________________________________________________ ____________ ________________
#
biplot(PC.nx.clr, choices=c(2,3), col=c("transparent","black"), cex=c(0.2,1.0),
       cex.lab=1.5,cex.axis=1.4, expand=1., xlab="PC2 (11.9% of variance)",
       ylab="PC3 (7.3% of variance)", xlim=c(-0.32,0.3), ylim=c(-0.3,0.3), family="nar") # , font.lab=2
# abline(v=0, lty=2, col="grey85")
# abline(h=0, lty=2, col="grey85")
sf0 <- 1
points(PC.nx.clr$x[,2]*sf0,PC.nx.clr$x[,3]*sf0,pch=c(17,1,10)[nx.clr$Type.S3],
       col=c("firebrick","#226422","darkgoldenrod")[nx.clr$Type.S3], cex=1.5, lwd=c(2,2)[nx.clr$Type.S3])
legend("topleft", ncol=1, legend=levels(nx.clr$Type.S3),
       pch=c(17,1,10), col=c("firebrick","#226422","darkgoldenrod"), cex=1., pt.cex=1.5, pt.lwd=c(2,2,2),
       bty="o", box.col="grey", inset=0.01, y.intersp=0.95, title=expression(bold("Type"))) ### 
rm(sf0)
box()
# ________________________________________________________________ ____________ ________________
#
# par(mfrow=c(1,1), mar=c(5,5,2,2), oma=c(0,0,0,0), lend=2, ljoin=1, font.lab=2)
biplot(PC.nx.clr, choices=c(2,4), col=c("transparent","black"), cex=c(0.2,1.0),
  cex.lab=1.5,cex.axis=1.4, expand=1., xlab="PC3 (7.3% of variance)",
  ylab="PC4 (5.7% of variance)", family="nar") # , font.lab=2, xlim=c(-0.25,0.25), ylim=c(-0.25,0.25)
# abline(v=0, lty=1, col="grey85")
# abline(h=0, lty=1, col="grey85")
sf0 <- 1.0
points(PC.nx.clr$x[,2]*sf0,PC.nx.clr$x[,4]*sf0,pch=c(17,1,10)[nx.clr$Type.S3],
       col=c("firebrick","#226422","darkgoldenrod")[nx.clr$Type.S3], cex=1.5, lwd=c(2,2)[nx.clr$Type.S3])
legend("bottomright", ncol=1, legend=levels(nx.clr$Type.S3), pch=c(17,1,10), 
       col=c("firebrick","#226422","darkgoldenrod"), cex=1., pt.cex=1.5, pt.lwd=c(2,2,2),
       bty="o", box.col="grey", inset=0.01, y.intersp=0.95, title=expression(bold("Type"))) ### 
rm(sf0)
box()
# ________________________________________________________________ ____________ ________________
#
# par(mfrow=c(1,1), mar=c(5,5,2,2), oma=c(0,0,0,0), lend=2, ljoin=1, font.lab=2)
biplot(PC.nx.clr, choices=c(3,4), col=c("transparent","black"), cex=c(0.2,1.0),
       cex.lab=1.5,cex.axis=1.4, expand=1., xlab="PC3 (7.3% of variance)",
       ylab="PC4 (5.7% of variance)", family="nar") # , font.lab=2, xlim=c(-0.25,0.25), ylim=c(-0.25,0.25)
# abline(v=0, lty=1, col="grey85")
# abline(h=0, lty=1, col="grey85")
sf0 <- 1.0
points(PC.nx.clr$x[,3]*sf0,PC.nx.clr$x[,4]*sf0,pch=c(17,1,10)[nx.clr$Type.S3],
       col=c("firebrick","#226422","darkgoldenrod")[nx.clr$Type.S3], cex=1.5, lwd=c(2,2,2)[nx.clr$Type.S3])
legend("topright", ncol=1, legend=levels(nx.clr$Type.S3),
       pch=c(17,1,10), col=c("firebrick","#226422","darkgoldenrod"), cex=1., pt.cex=1.5, pt.lwd=c(2,2,2),
       bty="o", box.col="grey", inset=0.01, y.intersp=0.95, title=expression(bold("Type"))) ### 
rm(sf0)
box()
dev.off() # IF USING RSTUDIO # USE options(device="RStudioGD") # THEN getOption('device') # IF USING RSTUDIO #
#
# ________________________________________________________________
#### run lda on principal components ####
require(MASS)
pc2ld <- as.data.frame(cbind(nx.clr.noNA$Type.S3,PC.nx.clr$x))
names(pc2ld)[1] <- "Type.S3"
lda.PCA.nx.clr <- lda(formula = Type.S3~PC1+ PC2+ PC3+ PC4+ PC5, 
                       data=pc2ld, prior=rep(1,nlevels(nx.clr$Type.S3))/nlevels(nx.clr$Type.S3))
# + PC6+ PC7+ PC8+ PC9+ PC10+ PC11+ PC12+ PC13+ PC14+ PC15+ PC16+ PC17+ PC18+ PC19+ PC20+ PC21
prop = 100*lda.PCA.nx.clr$svd^2/sum(lda.PCA.nx.clr$svd^2)
{cat("Discriminant 1 explains",signif(prop[1],3),"% of between groups variance\n")
  cat("Discriminant 2 explains",signif(prop[2],3),"% of between groups variance\n")}
#
# ____________________________________________________________________
#### run stepwise lda on principal components ####
require(klaR)
# options [for criterion=c("CR","AC","AS","CF","CFvec")]
stepwise.lda <- stepclass(formula = Type.S3~PC1+ PC2+ PC3+ PC4+ PC5, data=pc2ld, 
                          prior=rep(1,nlevels(nx.clr$Type.S3))/nlevels(nx.clr$Type.S3), 
                          method="lda", improvement=0.001, direction="both", criterion="CR")
summary(stepwise.lda)

########## CALCULATE 'OBSERVATION SCORES' FOR TRAINING DATA ##########
lda.PCA.nx.clr.values <- predict(lda.PCA.nx.clr, 
                             pc2ld[c("PC1", "PC2", "PC3","PC4", "PC5")], na.rm=T)
#
########### SET OF 2 LDA PLOTS ##########
# dev.new(width=13.6, height=7.0)
palette(c("firebrick","#006699","saddlebrown"))
par(mfrow=c(1,2),mar=c(4,4,2,1),lend=2,ljoin=1,font.lab=2,cex.main=.7, mgp=c(2.2,0.8,0), family="sans")
########## PLOT VARIABLE WEIGHTINGS #
plot(lda.PCA.nx.clr$scaling[,1],lda.PCA.nx.clr$scaling[,2],col="skyblue4",pch=18,lwd=1,cex=1.4,
     cex.lab=1.4, cex.axis=1.4, xlab="Linear Discriminant [1]", 
     ylab="Linear Discriminant [2]", xlim=c(-2,2), ylim=c(-2,2), 
     main="(a) Variable Coefficients, data = data2f.alr, Ref. element Cr", family="nar") #  ylim=c(-2,6.5),
mtext(text="  (a)", cex=1.6, side=3, line=-2,adj=0.03, family="sans")
abline(v=0,col="grey",lty=2)
abline(h=0,col="grey",lty=2)
text(lda.PCA.nx.clr$scaling[,1],lda.PCA.nx.clr$scaling[,2],labels=rownames(lda.PCA.nx.clr$scaling),pos=4, 
     cex=1.4, family="nar")
legend("bottomright",legend=c("Variables"), bty="o", pch=c(18), col=c("skyblue4"), cex=1., 
       pt.cex=1.4, inset=0.01, box.col="grey90", bg="white", box.lwd=2, y.intersp=0.7)
#
########## PLOT TRAINING DATA OBSERVATION SCORES #
plot(lda.PCA.nx.clr.values$x[,1], lda.PCA.nx.clr.values$x[,2], 
     col=c(1,2,3)[lda.PCA.nx.clr.values$class],
     pch=c(1,15,0)[lda.PCA.nx.clr.values$class], lwd=c(2,1,2)[lda.PCA.nx.clr.values$class],
     cex=c(2,1.8,1.7)[lda.PCA.nx.clr.values$class],cex.axis=1.4, cex.lab=1.4, 
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
sp(lda.PCA.nx.clr.values$x[,2]~lda.PCA.nx.clr.values$x[,1]|lda.PCA.nx.clr.values$class, spread=F, smooth=F, reg.line=F,
   ellipse=T, levels=c(0.95), xlim=c(-6,6), ylim=c(-4,8), legend.coords="topright", cex=1.4, cex.lab=1.5, 
   cex.axis=1.4, grid=F, pch=c(1,15,0), legend.title="Sample Type")
#
# END
