# SEND THIS TO NIAN
# exploratory data analysis nian paper 4 trace elements
require(car)
require(RcmdrMisc)
windowsFonts(nar = windowsFont("Arial Narrow"))
windowsFonts() # check available fonts
# pearson and spearman correlation matrices ####
# pearson
rcorr.adjust(nx.clr[44:64], type = "pearson", use="pairwise.complete.obs")
#
# spearman
rcorr.adjust(nx.clr[,c(16,17,18,19,21,23,26,27,29,30,31,32,34,35,36,37,38,39,40,41,43)], 
             type = "spearman", use="pairwise.complete.obs")
# log-transformations of variables ####
nx.clr$As.log <- log10(nx.clr$As.oes)
nx.clr$Ba.log <- log10(nx.clr$As.ms)
nx.clr$Ba.log <- log10(nx.clr$Ba.ms)
nx.clr$Cd.log <- log10(nx.clr$Cd.ms)
nx.clr$Co.log <- log10(nx.clr$Co.ms)
nx.clr$Cr.log <- log10(nx.clr$Cr.ms)
nx.clr$Cu.log <- log10(nx.clr$Cu.ms)
nx.clr$Fe.log <- log10(nx.clr$Fe.oes)
nx.clr$Mn.log <- log10(nx.clr$Mn.ms)
nx.clr$Ni.log <- log10(nx.clr$Ni.oes)
nx.clr$P.log <- log10(nx.clr$P.oes)
nx.clr$Pb.log <- log10(nx.clr$Pb.ms)
nx.clr$S.log <- log10(nx.clr$S.oes)
nx.clr$Sr.log <- log10(nx.clr$Sr.ms)
nx.clr$Th.log <- log10(nx.clr$Th.ms)
nx.clr$Ti.log <- log10(nx.clr$Ti.ms)
nx.clr$U.log <- log10(nx.clr$U.ms)
nx.clr$V.log <- log10(nx.clr$V.ms)
nx.clr$Zn.log <- log10(nx.clr$Zn.ms)
nx.clr$REE.log <- log10(nx.clr$REE.ms)
names(nx.clr)
summary(nx.clr)
# Scatter plot matrix on clr-transformed elements ####
require(car)
dev.new()
spm(~As+Ba+Cd+Co+Cr+Cu+Fe+Mn+Mo+Ni+P+Pb+S+Sr+Th+Ti+U+V+Zn+REE | Type, data=nx.clr, smooth=F, span=F, 
    col=c("sienna", "darkcyan",2), pch=c(16,2,3), cex.lab=1.5)
# <em>Next</em>: scatter plot matrix on <strong>SELECTED</strong> <em>centred-logratio transformed</em> trace element 
# concentrations
spm(~Cr+Fe+P+Pb+Sr+Th+Ti+V | Type.S3, data=nx.clr, diagonal="boxplot", smooth=F, span=F, 
    col=c("orangered2","#0080B0","saddlebrown"), pch=c(1,15,0), cex.lab=1.5, legend.pos="left")
# <em>Next</em>: scatter plot matrix on <strong>common contaminant elements, Fe, P, and S</strong> 
#   using <em>clr-transformed</em> concentrations
spm(~As+Cd+Cr+Cu+Mn+Mo+Ni+Pb+Zn+Fe+P+S | Type.S3, data=nx.clr, diagonal="boxplot", smooth=F, span=F, 
    col=c("orangered2","#0088BB","saddlebrown"), pch=c(1,15,0), cex.lab=1.5, legend.pos="left")
# <em>Next</em>: scatter plot matrix: <strong>Focus on REEs</strong> 
#   using <em>clr-transformed</em> concentrations
spm(~Cd+Cr+Cu+Pb+Rb+Sr+Th+V+Zn+REE+Fe+P+S | Type.S3, data=nx.clr, diagonal="boxplot", smooth=F, span=F, 
    col=c("orangered2","#0088BB","saddlebrown"), pch=c(1,15,0), cex.lab=1.5, legend.pos="bottom")
# Scatter plot matrix on log-transformed elements ####
spm(~As.log+Ba.log+Cd.log+Co.log+Cr.log+Cu.log+Fe.log+Mn.log+P.log+Pb.log+S.log+Sr.log+Th.log+
      Ti.log+U.log+V.log+Zn.log+REE.log | Type, data=nx.clr, smooth=F, span=F, col=c("sienna","blue3",2),
    pch=c(16,2,3), cex.lab=1.5)
dev.off() # IF USING RSTUDIO # USE options(device="RStudioGD") # THEN getOption('device') # IF USING RSTUDIO #
plot(nx.clr$Fe.oes, nx.clr$REE.ms, log="xy")
#
# scatter plots of total ree vs predictors, closed compared w CLR-transformed ####
# dev.new()
par(mfrow=c(2,5), mar=c(4,0,1.2,0), oma=c(0,4,0,1), mgp=c(2.2,0.7,0.0), lend=2, ljoin=1, 
    font.lab=2, cex.axis=1.2, cex.lab=1.3, cex=1.2, lwd=2)
palette(c("black","sienna","darkolivegreen","grey92"))
# closed
plot(nx.clr$Fe.oes, nx.clr$REE.ms, log="xy", pch=c(2,15)[nx.clr$Type], col=c(2,3)[nx.clr$Type],
     xlab=expression(bold(paste(Fe[closed]," (mg/kg)"))), lwd=2)
mtext(side=2, line=2., expression(bold(paste(Sigma,REE[closed]," (mg/kg)"))), cex=1.4)
plot(nx.clr$P.oes, nx.clr$REE.ms, log="xy", yaxt="n", pch=c(2,15)[nx.clr$Type], col=c(2,3)[nx.clr$Type],
     xlab=expression(bold(paste(P[closed]," (mg/kg)"))), lwd=2)
plot(nx.clr$S.oes, nx.clr$REE.ms, log="xy", yaxt="n", pch=c(2,15)[nx.clr$Type], col=c(2,3)[nx.clr$Type],
     xlab=expression(bold(paste(S[closed]," (mg/kg)"))), lwd=2,
     ylab=expression(bold(paste(Sigma,REE[closed]," (mg/kg)"))))
plot(nx.clr$S.oes, nx.clr$REE.ms, , type="n", ann=F, xaxt="n", yaxt="n", bty="n")
legend("left",legend=levels(nx.clr$Type), pch=c(2,15), col=c(2,3), bty="n", inset=0.025, cex=1.2,
       pt.lwd=2, box.col=4, box.lwd=2)
plot(nx.clr$Fe.oes, nx.clr$P.oes, log="xy", pch=c(2,15)[nx.clr$Type], col=c(2,3)[nx.clr$Type], lwd=2,
     xlab=expression(bold(paste(Fe[closed]," (mg/kg)"))), ylab=expression(bold(paste(P[closed]," (mg/kg)"))))
mtext(side=2, line=2., expression(bold(paste(Sigma,P[closed]," (mg/kg)"))), cex=1.4)
# clr
plot(nx.clr$Fe, nx.clr$REE, pch=c(2,15)[nx.clr$Type], col=c(2,3)[nx.clr$Type],
     xlab=expression(bold(Fe[clr-transformed])), lwd=2)
mtext(side=2, line=2., expression(bold(paste(Sigma,REE[clr-transformed]))), cex=1.4)
plot(nx.clr$P, nx.clr$REE, yaxt="n", pch=c(2,15)[nx.clr$Type], col=c(2,3)[nx.clr$Type],
     xlab=expression(bold(P[clr-transformed])), lwd=2)
plot(nx.clr$S, nx.clr$REE, yaxt="n", pch=c(2,15)[nx.clr$Type], col=c(2,3)[nx.clr$Type],
     xlab=expression(bold(S[clr-transformed])), lwd=2, ylab=expression(bold(paste(Sigma,REE[clr-transformed]))))
plot(nx.clr$S, nx.clr$REE, type="n", ann=F, xaxt="n", yaxt="n", bty="n")
legend("topleft",legend=levels(nx.clr$Type), pch=c(2,15), col=c(2,3), bty="o", inset=0.025, cex=1.2,
       pt.lwd=2, box.col=4, box.lwd=2)
plot(nx.clr$Fe, nx.clr$P, pch=c(2,15)[nx.clr$Type], col=c(2,3)[nx.clr$Type],
     xlab=expression(bold(Fe[clr-transformed])), ylab=expression(bold(P[clr-transformed])), lwd=2)
mtext(side=2, line=2., expression(bold(P[clr-transformed])), cex=1.4)
palette('default')
dev.off() # IF USING RSTUDIO # USE options(device="RStudioGD") # THEN getOption('device') # IF USING RSTUDIO #
#
# boxplots by sample type ####
palette(c("black","#DDFFDD","#FFDDBB","#FFFFBB"))
par(mfrow=c(3,6), mar=c(3.,3.,.5,1), mgp=c(1.3,0.4,0.0), font.lab=2, cex.axis=1, cex.lab=1, family='nar')
Boxplot(As.log~Type.S3, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.0, id.n=0, names=c("DS","Sed","S3"), col=c(2,3,4))
Boxplot(Ba.log~Type.S3, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.0, id.n=0, names=c("DS","Sed","S3"), col=c(2,3,4))
Boxplot(Cd.log~Type.S3, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.0, id.n=0, names=c("DS","Sed","S3"), col=c(2,3,4))
Boxplot(Co.log~Type.S3, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.0, id.n=0, names=c("DS","Sed","S3"), col=c(2,3,4))
Boxplot(Cr.log~Type.S3, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.0, id.n=0, names=c("DS","Sed","S3"), col=c(2,3,4))
Boxplot(Cu.log~Type.S3, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.0, id.n=0, names=c("DS","Sed","S3"), col=c(2,3,4))
Boxplot(Fe.log~Type.S3, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.0, id.n=0, names=c("DS","Sed","S3"), col=c(2,3,4))
Boxplot(Mn.log~Type.S3, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.0, id.n=0, names=c("DS","Sed","S3"), col=c(2,3,4))
Boxplot(Ni.log~Type.S3, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.0, id.n=0, names=c("DS","Sed","S3"), col=c(2,3,4))
Boxplot(P.log~Type.S3, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.0, id.n=0, names=c("DS","Sed","S3"), col=c(2,3,4))
Boxplot(Pb.log~Type.S3, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.0, id.n=0, names=c("DS","Sed","S3"), col=c(2,3,4))
Boxplot(Sr.log~Type.S3, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.0, id.n=0, names=c("DS","Sed","S3"), col=c(2,3,4))
Boxplot(Th.log~Type.S3, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.0, id.n=0, names=c("DS","Sed","S3"), col=c(2,3,4))
Boxplot(Ti.log~Type.S3, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.0, id.n=0, names=c("DS","Sed","S3"), col=c(2,3,4))
Boxplot(U.log~Type.S3, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.0, id.n=0, names=c("DS","Sed","S3"), col=c(2,3,4))
Boxplot(V.log~Type.S3, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.0, id.n=0, names=c("DS","Sed","S3"), col=c(2,3,4))
Boxplot(Zn.log~Type.S3, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.0, id.n=0, names=c("DS","Sed","S3"), col=c(2,3,4))
Boxplot(REE.log~Type.S3, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.0, id.n=0, names=c("DS","Sed","S3"), col=c(2,3,4))
#
# Kruskal-Wallis non-parametric mean comparisons by sample type groups ####
require(car)
require(PMCMR)
names.of.cols <- names(nx.clr)
{cat("Kruskal-Wallis tests by sample type groups\n")
  cat("=============================================\n")
  cat("Variable",",","Kruskal-Wallis Chi-squared",",","p-value",",","Pairwise p-values","\n")}
for ( i in 15:43){
  kwt0 <- kruskal.test(nx.clr[,i]~Type.S3, data=nx.clr) # powerTransform(nx.clr[,i])
  pkt0 <- posthoc.kruskal.nemenyi.test(x=nx.clr[,i], g=nx.clr$Type.S3, dist="Tukey")
  cat(names.of.cols[i], ",", signif(kwt0$statistic,4), ",", signif(kwt0$p.value,4),",",
      pkt0$p.value[1:4],"\n")
}
#
# remove temporary objects
rm(names.of.cols)
rm(kwt0)
rm(i)
#
# boxplots by redox status ####
par(mfrow=c(3,6), mar=c(4.5,4.5,1,1), mgp=c(2.2,0.7,0.0), font.lab=2)
Boxplot(As.log~Redox, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.5)
Boxplot(Ba.log~Redox, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.5)
Boxplot(Cd.log~Redox, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.5)
Boxplot(Co.log~Redox, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.5)
Boxplot(Cr.log~Redox, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.5)
Boxplot(Cu.log~Redox, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.5)
Boxplot(Fe.log~Redox, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.5)
Boxplot(Mn.log~Redox, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.5)
Boxplot(Ni.log~Redox, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.5)
Boxplot(P.log~Redox, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.5)
Boxplot(Pb.log~Redox, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.5)
Boxplot(Sr.log~Redox, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.5)
Boxplot(Th.log~Redox, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.5)
Boxplot(Ti.log~Redox, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.5)
Boxplot(U.log~Redox, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.5)
Boxplot(V.log~Redox, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.5)
Boxplot(Zn.log~Redox, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.5)
Boxplot(REE.log~Redox, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.5)
#
# Kruskal-Wallis non-parametric mean comparisons by Redox groups ####
require(car)
names.of.cols <- names(nx.clr)
{cat("Kruskal-Wallis tests by Redox groups\n")
  cat("=============================================\n")
  cat("Variable",",","Kruskal-Wallis Chi-squared",",","p-value","\n")}
for ( i in 15:43){
  kwt0 <- kruskal.test(nx.clr[,i]~Redox, data=nx.clr) # powerTransform(nx.clr[,i])
  cat(names.of.cols[i], ",", signif(kwt0$statistic,4), ",", signif(kwt0$p.value,4),"\n")
}
#
# remove temporary objects
rm(names.of.cols)
rm(kwt0)
rm(i)
#
# boxplots by Site [set 1] ####
require(car)
dev.new()
windowsFonts(nar = windowsFont("Ubuntu Condensed"))
palette(c("black","#DDFFDD","#FFDDBB","#FFFFBB"))
par(mfrow=c(3,3), mar=c(4.5,4.5,1,1), mgp=c(2.2,0.7,0.0), font.lab=2, family='nar')
Boxplot(As.log~Site, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.8,
        col=c(2,2,4,2,2,3,3,3,3,3,3,3))
Boxplot(Ba.log~Site, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.8,
        col=c(2,2,4,2,2,3,3,3,3,3,3,3))
Boxplot(Cd.log~Site, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.8,
        col=c(2,2,4,2,2,3,3,3,3,3,3,3))
Boxplot(Co.log~Site, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.8,
        col=c(2,2,4,2,2,3,3,3,3,3,3,3))
Boxplot(Cr.log~Site, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.8,
        col=c(2,2,4,2,2,3,3,3,3,3,3,3))
Boxplot(Cu.log~Site, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.8,
        col=c(2,2,4,2,2,3,3,3,3,3,3,3))
Boxplot(Fe.log~Site, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.8,
        col=c(2,2,4,2,2,3,3,3,3,3,3,3))
Boxplot(Mn.log~Site, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.8,
        col=c(2,2,4,2,2,3,3,3,3,3,3,3))
Boxplot(Ni.log~Site, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.8,
        col=c(2,2,4,2,2,3,3,3,3,3,3,3))
palette('default')
dev.off() # IF USING RSTUDIO # USE options(device="RStudioGD") # THEN getOption('device') # IF USING RSTUDIO #
#
# boxplots by Site [set 2] ####
require(car)
dev.new()
windowsFonts(nar = windowsFont("Ubuntu Condensed"))
palette(c("black","#DDFFDD","#FFDDBB","#FFFFBB"))
par(mfrow=c(3,3), mar=c(4.5,4.5,1,1), mgp=c(2.2,0.7,0.0), font.lab=2, family='nar')
Boxplot(P.log~Site, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.8,
        col=c(2,2,4,2,2,3,3,3,3,3,3,3))
Boxplot(Pb.log~Site, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.8,
        col=c(2,2,4,2,2,3,3,3,3,3,3,3))
Boxplot(Sr.log~Site, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.8,
        col=c(2,2,4,2,2,3,3,3,3,3,3,3))
Boxplot(Th.log~Site, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.8,
        col=c(2,2,4,2,2,3,3,3,3,3,3,3))
Boxplot(Ti.log~Site, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.8,
        col=c(2,2,4,2,2,3,3,3,3,3,3,3))
Boxplot(U.log~Site, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.8,
        col=c(2,2,4,2,2,3,3,3,3,3,3,3))
Boxplot(V.log~Site, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.8,
        col=c(2,2,4,2,2,3,3,3,3,3,3,3))
Boxplot(Zn.log~Site, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.8,
        col=c(2,2,4,2,2,3,3,3,3,3,3,3))
Boxplot(REE.log~Site, data=nx.clr, notch=TRUE, varwidth=TRUE, cex=1.5, cex.axis=1.4, cex.lab=1.8,
        col=c(2,2,4,2,2,3,3,3,3,3,3,3))
palette('default')
dev.off() # IF USING RSTUDIO # USE options(device="RStudioGD") # THEN getOption('device') # IF USING RSTUDIO #
#
# 
