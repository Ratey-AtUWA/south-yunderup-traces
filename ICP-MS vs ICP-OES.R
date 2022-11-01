# plot relationships of icp-oes to icp-ms concentrations ####
attach(nx.clr)
par(mfrow=c(2,4), mar=c(4,4,2,1), mgp=c(2.2,0.7,0.0), lend=2, ljoin=1, font.lab=2)
plot(As.ms,As.oes, cex=1.4, cex.axis=1.4, cex.lab=1.5)
abline(0,1,col="red")
text(par('usr')[2]*0.8,par('usr')[4]*0.1,labels="1:1 line", col="red", cex=1.4)
#
plot(Co.ms,Co.oes, cex=1.4, cex.axis=1.4, cex.lab=1.5)
abline(0,1,col="red")
text(par('usr')[2]*0.8,par('usr')[4]*0.1,labels="1:1 line", col="red", cex=1.4)
#
plot(Cr.ms,Cr.oes, cex=1.4, cex.axis=1.4, cex.lab=1.5)
abline(0,1,col="red")
text(par('usr')[2]*0.8,par('usr')[4]*0.1,labels="1:1 line", col="red", cex=1.4)
#
plot(Cu.ms,Cu.oes, cex=1.4, cex.axis=1.4, cex.lab=1.5)
abline(0,1,col="red")
text(par('usr')[2]*0.8,par('usr')[4]*0.1,labels="1:1 line", col="red", cex=1.4)
#
plot(Fe.ms,Fe.oes, cex=1.4, cex.axis=1.4, cex.lab=1.5)
abline(0,1,col="red")
text(par('usr')[2]*0.8,par('usr')[4]*0.1,labels="1:1 line", col="red", cex=1.4)
#
plot(Mn.ms,Mn.oes, cex=1.4, cex.axis=1.4, cex.lab=1.5)
abline(0,1,col="red")
text(par('usr')[2]*0.8,par('usr')[4]*0.1,labels="1:1 line", col="red", cex=1.4)
#
plot(Pb.ms,Pb.oes, cex=1.4, cex.axis=1.4, cex.lab=1.5)
abline(0,1,col="red")
text(par('usr')[2]*0.8,par('usr')[4]*0.1,labels="1:1 line", col="red", cex=1.4)
#
plot(Zn.ms,Zn.oes, cex=1.4, cex.axis=1.4, cex.lab=1.5)
abline(0,1,col="red")
text(par('usr')[2]*0.8,par('usr')[4]*0.1,labels="1:1 line", col="red", cex=1.4)
#
