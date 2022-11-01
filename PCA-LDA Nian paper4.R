obs.PC.nx.clr <- as.data.frame(rep(NA,nrow(PC.nx.clr$x)))
newcols <- as.character(seq(1,ncol(PC.nx.clr$x), by=1))
for(i in 1:ncol(PC.nx.clr$x)){
  newcols[i] <- paste0("PC",i)
  obs.PC.nx.clr[newcols[i]] <- PC.nx.clr$x[,i]
}
names(jk.clr.soil)
summary(jk.clr.soil[newcols])
rm(newcols)
rm(i)
#
########## RUN LDA ##########
require(MASS)
lda.PCA.jk.soil <- lda(formula = Profile~PC1+ PC2+ PC3+ PC4+ PC5+ PC6+ PC7+ PC8+ PC9+ PC10+ PC11+ 
                         PC12+ PC13+ PC14+ PC15+ PC16+ PC17+ PC18+ PC19+ PC20+ PC21+ PC22, 
                       data=jk.clr.soil, prior=c(1,1,1,1,1,1,1)/7)
prop = 100*lda.PCA.jk.soil$svd^2/sum(lda.PCA.jk.soil$svd^2)
{cat("Discriminant 1 explains",signif(prop[1],3),"% of between groups variance\n")
  cat("Discriminant 2 explains",signif(prop[2],3),"% of between groups variance\n")
  cat("Discriminant 3 explains",signif(prop[3],3),"% of between groups variance\n")}
# ls(lda.PCA.jk.soil)
########## CALCULATE 'OBSERVATION SCORES' FOR TRAINING DATA ##########
lda.PCA.jk.soil.values <- predict(lda.PCA.jk.soil, jk.clr.soil[c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", 
                                                                 "PC7", "PC8", "PC9", "PC10", "PC11", "PC12", "PC13", "PC14", "PC15",      
                                                                 "PC16", "PC17", "PC18", "PC19", "PC20", "PC21", "PC22")], na.rm=T)
########## PLOT SEPARATIONS ##########
par(mfrow=c(1,1),lend=2,ljoin=1,font.lab=2,cex.main=2, cex.lab=2, cex.axis=1.6)
ldahist(data = lda.PCA.jk.soil.values$x[,1], g=lda.PCA.jk.soil.values$class)
# ldahist(data = lda.PCA.jk.soil.pred$x[,1], g=data2q.clr$Profile)
#
########## NUMERICAL DATA ##########
print(lda.PCA.jk.soil$scaling,digits=3) # COEFFICIENTS
#
########### SET OF 2 LDA PLOTS ##########
dev.new(width=13.6, height=7.0)
# win.metafile(width=6,height=12,file="JK_LDA_clr[1-2].emf")
palette(c("black","purple2","red3","sienna","olivedrab4","green4","dodgerblue2"))
par(mfrow=c(1,2),mar=c(4,4,2,1),lend=2,ljoin=1,font.lab=2,cex.main=1.25, mgp=c(2.2,0.8,0), family="sans")
########## PLOT VARIABLE WEIGHTINGS ##########
plot(lda.PCA.jk.soil$scaling[,1],lda.PCA.jk.soil$scaling[,2],col="skyblue4",pch=18,lwd=1,cex=1.4,cex.lab=1.4,
     cex.axis=1.4, xlab="Linear Discriminant [1]", ylab="Linear Discriminant [2]",
     main="(a) Variable Coefficients, data = data2q.clr", family="sans", xlim=c(-60,55)) #  , ylim=c(-4.5,5.2)
mtext(text="  (a)", cex=1.6, side=3, line=-2,adj=0.45, family="sans", font=2)
abline(v=0,col="grey",lty=2)
abline(h=0,col="grey",lty=2)
text(lda.PCA.jk.soil$scaling[,1],lda.PCA.jk.soil$scaling[,2],labels=rownames(lda.PCA.jk.soil$scaling),pos=4, cex=1.4, family="nar")
legend("bottomleft",legend=c("Variables"), bty="o", pch=c(18), col=c("skyblue4"), cex=1.4, pt.cex=1.4,
       inset=0.01, box.col="grey90", bg="white", box.lwd=2, y.intersp=0.7)
#
########## PLOT TRAINING DATA OBSERVATION SCORES ##########
plot(lda.PCA.jk.soil.values$x[,1], lda.PCA.jk.soil.values$x[,2], 
     col=c(1,2,3,4,5,6,7)[lda.PCA.jk.soil.values$class],
     pch=c(0,15,1,19,2,17,3)[lda.PCA.jk.soil.values$class], lwd=c(2,1,2,2,2,2,2)[lda.PCA.jk.soil.values$class],
     cex=c(1.8,1.8,2,1.8,1.8,1.8,1.8)[lda.PCA.jk.soil.values$class],cex.axis=1.4, cex.lab=1.4, 
     xlab="Linear Discriminant [1]", ylab="Linear Discriminant [2]",
     main="(b) Coordinates of observations, data = data2q.clr", family="sans")
mtext(text="  (b)", cex=1.6, side=3, line=-2, adj=0.55, family="sans", font=2)
abline(v=0,col="grey",lty=2)
abline(h=0,col="grey",lty=2)
legend("topleft",legend=levels(data2q.clr$Profile), ncol=3,
       col=c(1,2,3,4,5,6,7), pch=c(0,15,1,19,2,17,3),pt.lwd=c(2,1,2,1,2,1,2),title=expression(italic("Profile")),
       bty="o", box.col="grey90", box.lwd=2, inset=0.01, pt.cex=c(1.8,1.8,2,1.8,1.8,1.8,1.8), cex=1.4, 
       text.col=c(1,2,3,4,5,6,7), title.col="black", y.intersp=0.99, bg="white")
dev.off()
#
# END