par(mfrow=c(5,12), mar=c(0,0,0,0), oma=c(2,5,3.5,1), mgp=c(1.8,0.4,0), cex=1.0, col=1, tcl=-0.3)
concs <- names(nx.clr2)
cores <- levels(nx.clr2$Site)
# NROW(levels(nx.clr$Site))
## [1] 12
for (j in 1:5) {
  for (i in 16:27) {
    plot(nx.clr2$Depth.cm~nx.clr2[,i], type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
       xlim=c(min(nx.clr2[,i], na.rm=T),max(nx.clr2[,i], na.rm=T)), subset=nx.clr2$Site==levels(nx.clr2$Site)[j])
    if (j < 1.5) {
      axis(3, cex.axis=0.85, xaxs="i")
      mtext(side=3,line=2.2,font=2, text=concs[i], cex=1.0)
      }
    if (i < 16.5) {
      axis(2)
      mtext(side=2,line=2.2,font=2, text=paste0("Core = ", levels(nx.clr2$Site)[j],"\nDepth (cm)"), cex=1.2)
    }
  }
}
#
rm(i)
rm(j)
rm(concs)
rm(cores)
# ========================= Core 1 =========================
plot(nx.clr$Depth.cm~nx.clr$pH, type="b", ylim=c(30,0), xaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$pH, na.rm=T),max(nx.clr$pH, na.rm=T)), subset=nx.clr$Site=="S 1")
mtext(side=2,line=2.2,font=2, text="Core 1\nDepth (cm)", cex=1.2)
axis(3)
mtext(side=3,line=2.2,font=2, text="pH", cex=1.0)
#
plot(nx.clr$Depth.cm~nx.clr$Eh.mV, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Eh.mV, na.rm=T),max(nx.clr$Eh.mV, na.rm=T)), subset=nx.clr$Site=="S 1")
axis(3, at=c(-50,0,50,100))
mtext(side=3,line=1.8,font=2, cex=1.0, text=expression(paste(bold("E"[h],"(mV)"))))
#
plot(nx.clr$Depth.cm~nx.clr$AVS.umolg, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$TotalS.oes.umolg, na.rm=T),max(nx.clr$TotalS.oes.umolg, na.rm=T)), subset=nx.clr$Site=="S 1")
points(nx.clr$Depth.cm~nx.clr$TotalS.oes.umolg, type="o", pch=17, col="goldenrod3", 
       subset=nx.clr$Site=="S 1")
axis(3)
mtext(side=3,line=2.,font=2, cex=1.0, text="Sulfur (\u00B5mol/g)")
legend("topright", legend=c("AVS", "Total S"), pch=c(16,17), col=c("black","goldenrod3"), cex=0.9, 
       pt.cex=1, bty="o", x.intersp=0.5, y.intersp=0.9, inset=0.03, box.col="grey92")
#
plot(nx.clr$Depth.cm~nx.clr$As.oes, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$As.oes, na.rm=T),max(nx.clr$As.oes, na.rm=T)), subset=nx.clr$Site=="S 1")
axis(3)
mtext(side=3,line=2.,font=2, cex=1.0, text="As (mg/kg)")
#
plot(nx.clr$Depth.cm~nx.clr$Cd.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cd.ms, na.rm=T),max(nx.clr$Cd.ms, na.rm=T)), subset=nx.clr$Site=="S 1")
axis(3)
mtext(side=3,line=2.,font=2, cex=1.0, text="Cd (mg/kg)")
#
plot(nx.clr$Depth.cm~nx.clr$Cu.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cu.ms, na.rm=T),max(nx.clr$Cu.ms, na.rm=T)), subset=nx.clr$Site=="S 1")
axis(3)
mtext(side=3,line=2.,font=2, cex=1.0, text="Cu (mg/kg)")
#
plot(nx.clr$Depth.cm~nx.clr$Fe.oes, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Fe.oes, na.rm=T),max(nx.clr$Fe.oes, na.rm=T)), subset=nx.clr$Site=="S 1")
axis(3)
mtext(side=3,line=2.,font=2, cex=1.0, text="Fe (mg/kg)")
#
plot(nx.clr$Depth.cm~nx.clr$P.oes, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$P.oes, na.rm=T),max(nx.clr$P.oes, na.rm=T)), subset=nx.clr$Site=="S 1")
axis(3)
mtext(side=3,line=2.,font=2, cex=1.0, text="P (mg/kg)")
#
plot(nx.clr$Depth.cm~nx.clr$Pb.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Pb.ms, na.rm=T),max(nx.clr$Pb.ms, na.rm=T)), subset=nx.clr$Site=="S 1")
axis(3)
mtext(side=3,line=2.,font=2, cex=1.0, text="Pb (mg/kg)")
#
plot(nx.clr$Depth.cm~nx.clr$Ti.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Ti.ms, na.rm=T),max(nx.clr$Ti.ms, na.rm=T)), subset=nx.clr$Site=="S 1")
axis(3)
mtext(side=3,line=2.,font=2, cex=1.0, text="Ti (mg/kg)")
#
plot(nx.clr$Depth.cm~nx.clr$Zn.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Zn.ms, na.rm=T),max(nx.clr$Zn.ms, na.rm=T)), subset=nx.clr$Site=="S 1")
axis(3)
mtext(side=3,line=2.,font=2, cex=1.0, text="Zn (mg/kg)")
#
plot(nx.clr$Depth.cm~nx.clr$REE.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$REE.ms, na.rm=T),max(nx.clr$REE.ms, na.rm=T)), subset=nx.clr$Site=="S 1")
axis(3)
mtext(side=3,line=2.,font=2, cex=1.0, text="\u2211REE (mg/kg)")
#
# ========================= Core 2 =========================
plot(nx.clr$Depth.cm~nx.clr$pH, type="b", ylim=c(30,0), xaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$pH, na.rm=T),max(nx.clr$pH, na.rm=T)), subset=nx.clr$Site=="S 2")
mtext(side=2,line=2.2,font=2, text="Core 2\nDepth (cm)", cex=1.2)
#
plot(nx.clr$Depth.cm~nx.clr$Eh.mV, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Eh.mV, na.rm=T),max(nx.clr$Eh.mV, na.rm=T)), subset=nx.clr$Site=="S 2")
#
plot(nx.clr$Depth.cm~nx.clr$AVS.umolg, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$TotalS.oes.umolg, na.rm=T),max(nx.clr$TotalS.oes.umolg, na.rm=T)), subset=nx.clr$Site=="S 2")
points(nx.clr$Depth.cm~nx.clr$TotalS.oes.umolg, type="o", pch=17, col="goldenrod3", 
       subset=nx.clr$Site=="S 2")
#
plot(nx.clr$Depth.cm~nx.clr$As.oes, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$As.oes, na.rm=T),max(nx.clr$As.oes, na.rm=T)), subset=nx.clr$Site=="S 2")
#
plot(nx.clr$Depth.cm~nx.clr$Cd.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cd.ms, na.rm=T),max(nx.clr$Cd.ms, na.rm=T)), subset=nx.clr$Site=="S 2")
#
plot(nx.clr$Depth.cm~nx.clr$Cu.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cu.ms, na.rm=T),max(nx.clr$Cu.ms, na.rm=T)), subset=nx.clr$Site=="S 2")
#
plot(nx.clr$Depth.cm~nx.clr$Fe.oes, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Fe.oes, na.rm=T),max(nx.clr$Fe.oes, na.rm=T)), subset=nx.clr$Site=="S 2")
#
plot(nx.clr$Depth.cm~nx.clr$P.oes, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$P.oes, na.rm=T),max(nx.clr$P.oes, na.rm=T)), subset=nx.clr$Site=="S 2")
#
plot(nx.clr$Depth.cm~nx.clr$Pb.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Pb.ms, na.rm=T),max(nx.clr$Pb.ms, na.rm=T)), subset=nx.clr$Site=="S 2")
#
plot(nx.clr$Depth.cm~nx.clr$Ti.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Ti.ms, na.rm=T),max(nx.clr$Ti.ms, na.rm=T)), subset=nx.clr$Site=="S 2")
#
plot(nx.clr$Depth.cm~nx.clr$Zn.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Zn.ms, na.rm=T),max(nx.clr$Zn.ms, na.rm=T)), subset=nx.clr$Site=="S 2")
#
plot(nx.clr$Depth.cm~nx.clr$REE.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$REE.ms, na.rm=T),max(nx.clr$REE.ms, na.rm=T)), subset=nx.clr$Site=="S 2")
#
# ========================= Core 3 =========================
plot(nx.clr$Depth.cm~nx.clr$pH, type="b", ylim=c(30,0), xaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$pH, na.rm=T),max(nx.clr$pH, na.rm=T)), subset=nx.clr$Site=="S 3")
mtext(side=2,line=2.2,font=2, text="Core 3\nDepth (cm)", cex=1.2)
#
plot(nx.clr$Depth.cm~nx.clr$Eh.mV, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Eh.mV, na.rm=T),max(nx.clr$Eh.mV, na.rm=T)), subset=nx.clr$Site=="S 3")
#
plot(nx.clr$Depth.cm~nx.clr$AVS.umolg, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$TotalS.oes.umolg, na.rm=T),max(nx.clr$TotalS.oes.umolg, na.rm=T)), subset=nx.clr$Site=="S 3")
points(nx.clr$Depth.cm~nx.clr$TotalS.oes.umolg, type="o", pch=17, col="goldenrod3", 
       subset=nx.clr$Site=="S 3")
#
plot(nx.clr$Depth.cm~nx.clr$As.oes, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$As.oes, na.rm=T),max(nx.clr$As.oes, na.rm=T)), subset=nx.clr$Site=="S 3")
#
plot(nx.clr$Depth.cm~nx.clr$Cd.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cd.ms, na.rm=T),max(nx.clr$Cd.ms, na.rm=T)), subset=nx.clr$Site=="S 3")
#
plot(nx.clr$Depth.cm~nx.clr$Cu.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cu.ms, na.rm=T),max(nx.clr$Cu.ms, na.rm=T)), subset=nx.clr$Site=="S 3")
#
plot(nx.clr$Depth.cm~nx.clr$Fe.oes, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Fe.oes, na.rm=T),max(nx.clr$Fe.oes, na.rm=T)), subset=nx.clr$Site=="S 3")
#
plot(nx.clr$Depth.cm~nx.clr$P.oes, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$P.oes, na.rm=T),max(nx.clr$P.oes, na.rm=T)), subset=nx.clr$Site=="S 3")
#
plot(nx.clr$Depth.cm~nx.clr$Pb.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Pb.ms, na.rm=T),max(nx.clr$Pb.ms, na.rm=T)), subset=nx.clr$Site=="S 3")
#
plot(nx.clr$Depth.cm~nx.clr$Ti.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Ti.ms, na.rm=T),max(nx.clr$Ti.ms, na.rm=T)), subset=nx.clr$Site=="S 3")
#
plot(nx.clr$Depth.cm~nx.clr$Zn.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Zn.ms, na.rm=T),max(nx.clr$Zn.ms, na.rm=T)), subset=nx.clr$Site=="S 3")
#
plot(nx.clr$Depth.cm~nx.clr$REE.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$REE.ms, na.rm=T),max(nx.clr$REE.ms, na.rm=T)), subset=nx.clr$Site=="S 3")
#
# ========================= Core 4 =========================
plot(nx.clr$Depth.cm~nx.clr$pH, type="b", ylim=c(30,0), xaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$pH, na.rm=T),max(nx.clr$pH, na.rm=T)), subset=nx.clr$Site=="S 4")
mtext(side=2,line=2.2,font=2, text="Core 4\nDepth (cm)", cex=1.2)
#
plot(nx.clr$Depth.cm~nx.clr$Eh.mV, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Eh.mV, na.rm=T),max(nx.clr$Eh.mV, na.rm=T)), subset=nx.clr$Site=="S 4")
#
plot(nx.clr$Depth.cm~nx.clr$AVS.umolg, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$TotalS.oes.umolg, na.rm=T),max(nx.clr$TotalS.oes.umolg, na.rm=T)), subset=nx.clr$Site=="S 4")
points(nx.clr$Depth.cm~nx.clr$TotalS.oes.umolg, type="o", pch=17, col="goldenrod3", 
       subset=nx.clr$Site=="S 4")
#
plot(nx.clr$Depth.cm~nx.clr$As.oes, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$As.oes, na.rm=T),max(nx.clr$As.oes, na.rm=T)), subset=nx.clr$Site=="S 4")
#
plot(nx.clr$Depth.cm~nx.clr$Cd.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cd.ms, na.rm=T),max(nx.clr$Cd.ms, na.rm=T)), subset=nx.clr$Site=="S 4")
#
plot(nx.clr$Depth.cm~nx.clr$Cu.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cu.ms, na.rm=T),max(nx.clr$Cu.ms, na.rm=T)), subset=nx.clr$Site=="S 4")
#
plot(nx.clr$Depth.cm~nx.clr$Fe.oes, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Fe.oes, na.rm=T),max(nx.clr$Fe.oes, na.rm=T)), subset=nx.clr$Site=="S 4")
#
plot(nx.clr$Depth.cm~nx.clr$P.oes, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$P.oes, na.rm=T),max(nx.clr$P.oes, na.rm=T)), subset=nx.clr$Site=="S 4")
#
plot(nx.clr$Depth.cm~nx.clr$Pb.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Pb.ms, na.rm=T),max(nx.clr$Pb.ms, na.rm=T)), subset=nx.clr$Site=="S 4")
#
plot(nx.clr$Depth.cm~nx.clr$Ti.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Ti.ms, na.rm=T),max(nx.clr$Ti.ms, na.rm=T)), subset=nx.clr$Site=="S 4")
#
plot(nx.clr$Depth.cm~nx.clr$Zn.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Zn.ms, na.rm=T),max(nx.clr$Zn.ms, na.rm=T)), subset=nx.clr$Site=="S 4")
#
plot(nx.clr$Depth.cm~nx.clr$REE.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$REE.ms, na.rm=T),max(nx.clr$REE.ms, na.rm=T)), subset=nx.clr$Site=="S 4")
#
# ========================= Core 5 =========================
plot(nx.clr$Depth.cm~nx.clr$pH, type="b", ylim=c(30,0), xaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$pH, na.rm=T),max(nx.clr$pH, na.rm=T)), subset=nx.clr$Site=="S 5")
mtext(side=2,line=2.2,font=2, text="Core 5\nDepth (cm)", cex=1.2)
#
plot(nx.clr$Depth.cm~nx.clr$Eh.mV, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Eh.mV, na.rm=T),max(nx.clr$Eh.mV, na.rm=T)), subset=nx.clr$Site=="S 5")
#
plot(nx.clr$Depth.cm~nx.clr$AVS.umolg, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$TotalS.oes.umolg, na.rm=T),max(nx.clr$TotalS.oes.umolg, na.rm=T)), subset=nx.clr$Site=="S 5")
points(nx.clr$Depth.cm~nx.clr$TotalS.oes.umolg, type="o", pch=17, col="goldenrod3", 
       subset=nx.clr$Site=="S 5")
#
plot(nx.clr$Depth.cm~nx.clr$As.oes, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$As.oes, na.rm=T),max(nx.clr$As.oes, na.rm=T)), subset=nx.clr$Site=="S 5")
#
plot(nx.clr$Depth.cm~nx.clr$Cd.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cd.ms, na.rm=T),max(nx.clr$Cd.ms, na.rm=T)), subset=nx.clr$Site=="S 5")
#
plot(nx.clr$Depth.cm~nx.clr$Cu.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cu.ms, na.rm=T),max(nx.clr$Cu.ms, na.rm=T)), subset=nx.clr$Site=="S 5")
#
plot(nx.clr$Depth.cm~nx.clr$Fe.oes, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Fe.oes, na.rm=T),max(nx.clr$Fe.oes, na.rm=T)), subset=nx.clr$Site=="S 5")
#
plot(nx.clr$Depth.cm~nx.clr$P.oes, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$P.oes, na.rm=T),max(nx.clr$P.oes, na.rm=T)), subset=nx.clr$Site=="S 5")
#
plot(nx.clr$Depth.cm~nx.clr$Pb.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Pb.ms, na.rm=T),max(nx.clr$Pb.ms, na.rm=T)), subset=nx.clr$Site=="S 5")
#
plot(nx.clr$Depth.cm~nx.clr$Ti.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Ti.ms, na.rm=T),max(nx.clr$Ti.ms, na.rm=T)), subset=nx.clr$Site=="S 5")
#
plot(nx.clr$Depth.cm~nx.clr$Zn.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Zn.ms, na.rm=T),max(nx.clr$Zn.ms, na.rm=T)), subset=nx.clr$Site=="S 5")
#
plot(nx.clr$Depth.cm~nx.clr$REE.ms, type="b", ylim=c(30,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$REE.ms, na.rm=T),max(nx.clr$REE.ms, na.rm=T)), subset=nx.clr$Site=="S 5")
#

