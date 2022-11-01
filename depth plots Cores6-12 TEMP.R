dev.new()
par(mfrow=c(7,12), mar=c(0,0,0,0), oma=c(2,5,3.5,1), mgp=c(1.8,0.4,0), cex=1.0, col=1, tcl=-0.3)
# ========================= Core 6 =========================
plot(nx.clr$Depth.cm~nx.clr$pH, type="o", ylim=c(130,0), xaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$pH, na.rm=T),max(nx.clr$pH, na.rm=T)), subset=nx.clr$Site=="S 6")
mtext(side=2,line=2.2,font=2, text="Core 6\nDepth (cm)", cex=1.2)
axis(3)
mtext(side=3,line=2.2,font=2, text="pH", cex=1.0)
#
plot(nx.clr$Depth.cm~nx.clr$Eh.mV, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Eh.mV, na.rm=T),max(nx.clr$Eh.mV, na.rm=T)), subset=nx.clr$Site=="S 6")
axis(3, at=c(-50,0,50,100))
mtext(side=3,line=1.8,font=2, cex=1.0, text=expression(paste(bold("E"[h],"(mV)"))))
#
plot(nx.clr$Depth.cm~nx.clr$AVS.umolg, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$AVS.umolg, na.rm=T),max(nx.clr$AVS.umolg, na.rm=T)), subset=nx.clr$Site=="S 6")
points(nx.clr$Depth.cm~nx.clr$TotalS.oes.umolg, type="o", pch=17, col="goldenrod3", 
       subset=nx.clr$Site=="S 6")
axis(3)
mtext(side=3,line=2.,font=2, cex=1.0, text="Sulfur (\u00B5mol/g)")
legend("topright", legend=c("AVS", "Total S"), pch=c(16,17), col=c("black","goldenrod3"), cex=0.9, 
       pt.cex=1, bty="o", x.intersp=0.5, y.intersp=0.9, inset=0.03, box.col="grey92")
#
plot(nx.clr$Depth.cm~nx.clr$As.oes, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$As.oes, na.rm=T),max(nx.clr$As.oes, na.rm=T)), subset=nx.clr$Site=="S 6")
axis(3)
mtext(side=3,line=2.,font=2, cex=1.0, text="As (mg/kg)")
#
plot(nx.clr$Depth.cm~nx.clr$Cd.ms, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cd.ms, na.rm=T),max(nx.clr$Cd.ms, na.rm=T)), subset=nx.clr$Site=="S 6")
axis(3)
mtext(side=3,line=2.,font=2, cex=1.0, text="Cd (mg/kg)")
#
plot(nx.clr$Depth.cm~nx.clr$Cu.ms, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cu.ms, na.rm=T),max(nx.clr$Cu.ms, na.rm=T)), subset=nx.clr$Site=="S 6")
axis(3)
mtext(side=3,line=2.,font=2, cex=1.0, text="Cu (mg/kg)")
#
plot(nx.clr$Depth.cm~nx.clr$Fe.oes, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Fe.oes, na.rm=T),max(nx.clr$Fe.oes, na.rm=T)), subset=nx.clr$Site=="S 6")
axis(3)
mtext(side=3,line=2.,font=2, cex=1.0, text="Fe (mg/kg)")
#
plot(nx.clr$Depth.cm~nx.clr$P.oes, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$P.oes, na.rm=T),max(nx.clr$P.oes, na.rm=T)), subset=nx.clr$Site=="S 6")
axis(3)
mtext(side=3,line=2.,font=2, cex=1.0, text="P (mg/kg)")
#
plot(nx.clr$Depth.cm~nx.clr$Pb.ms, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Pb.ms, na.rm=T),max(nx.clr$Pb.ms, na.rm=T)), subset=nx.clr$Site=="S 6")
axis(3)
mtext(side=3,line=2.,font=2, cex=1.0, text="Pb (mg/kg)")
#
plot(nx.clr$Depth.cm~nx.clr$Ti.ms, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Ti.ms, na.rm=T),max(nx.clr$Ti.ms, na.rm=T)), subset=nx.clr$Site=="S 6")
axis(3)
mtext(side=3,line=2.,font=2, cex=1.0, text="Ti (mg/kg)")
#
plot(nx.clr$Depth.cm~nx.clr$Zn.ms, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Zn.ms, na.rm=T),max(nx.clr$Zn.ms, na.rm=T)), subset=nx.clr$Site=="S 6")
axis(3)
mtext(side=3,line=2.,font=2, cex=1.0, text="Zn (mg/kg)")
#
plot(nx.clr$Depth.cm~nx.clr$REE.ms, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$REE.ms, na.rm=T),max(nx.clr$REE.ms, na.rm=T)), subset=nx.clr$Site=="S 6")
axis(3)
mtext(side=3,line=2.,font=2, cex=1.0, text="\u2211REE (mg/kg)")
#
# ========================= Core 7 =========================
plot(nx.clr$Depth.cm~nx.clr$pH, type="o", ylim=c(130,0), xaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$pH, na.rm=T),max(nx.clr$pH, na.rm=T)), subset=nx.clr$Site=="S 7")
mtext(side=2,line=2.2,font=2, text="Core 7\nDepth (cm)", cex=1.2)
#
plot(nx.clr$Depth.cm~nx.clr$Eh.mV, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Eh.mV, na.rm=T),max(nx.clr$Eh.mV, na.rm=T)), subset=nx.clr$Site=="S 7")
#
plot(nx.clr$Depth.cm~nx.clr$AVS.umolg, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$AVS.umolg, na.rm=T),max(nx.clr$AVS.umolg, na.rm=T)), subset=nx.clr$Site=="S 7")
points(nx.clr$Depth.cm~nx.clr$TotalS.oes.umolg, type="o", pch=17, col="goldenrod3", 
       subset=nx.clr$Site=="S 7")
#
plot(nx.clr$Depth.cm~nx.clr$As.oes, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$As.oes, na.rm=T),max(nx.clr$As.oes, na.rm=T)), subset=nx.clr$Site=="S 7")
#
plot(nx.clr$Depth.cm~nx.clr$Cd.ms, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cd.ms, na.rm=T),max(nx.clr$Cd.ms, na.rm=T)), subset=nx.clr$Site=="S 7")
#
plot(nx.clr$Depth.cm~nx.clr$Cu.ms, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cu.ms, na.rm=T),max(nx.clr$Cu.ms, na.rm=T)), subset=nx.clr$Site=="S 7")
#
plot(nx.clr$Depth.cm~nx.clr$Fe.oes, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Fe.oes, na.rm=T),max(nx.clr$Fe.oes, na.rm=T)), subset=nx.clr$Site=="S 7")
#
plot(nx.clr$Depth.cm~nx.clr$P.oes, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$P.oes, na.rm=T),max(nx.clr$P.oes, na.rm=T)), subset=nx.clr$Site=="S 7")
#
plot(nx.clr$Depth.cm~nx.clr$Pb.ms, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Pb.ms, na.rm=T),max(nx.clr$Pb.ms, na.rm=T)), subset=nx.clr$Site=="S 7")
#
plot(nx.clr$Depth.cm~nx.clr$Ti.ms, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Ti.ms, na.rm=T),max(nx.clr$Ti.ms, na.rm=T)), subset=nx.clr$Site=="S 7")
#
plot(nx.clr$Depth.cm~nx.clr$Zn.ms, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Zn.ms, na.rm=T),max(nx.clr$Zn.ms, na.rm=T)), subset=nx.clr$Site=="S 7")
#
plot(nx.clr$Depth.cm~nx.clr$REE.ms, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$REE.ms, na.rm=T),max(nx.clr$REE.ms, na.rm=T)), subset=nx.clr$Site=="S 7")
#
# ========================= Core 8 =========================
plot(nx.clr$Depth.cm~nx.clr$pH, type="o", ylim=c(130,0), xaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$pH, na.rm=T),max(nx.clr$pH, na.rm=T)), subset=nx.clr$Site=="S 8")
mtext(side=2,line=2.2,font=2, text="Core 8\nDepth (cm)", cex=1.2)
#
plot(nx.clr$Depth.cm~nx.clr$Eh.mV, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Eh.mV, na.rm=T),max(nx.clr$Eh.mV, na.rm=T)), subset=nx.clr$Site=="S 8")
#
plot(nx.clr$Depth.cm~nx.clr$AVS.umolg, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$AVS.umolg, na.rm=T),max(nx.clr$AVS.umolg, na.rm=T)), subset=nx.clr$Site=="S 8")
points(nx.clr$Depth.cm~nx.clr$TotalS.oes.umolg, type="o", pch=17, col="goldenrod3", 
       subset=nx.clr$Site=="S 8")
#
plot(nx.clr$Depth.cm~nx.clr$As.oes, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$As.oes, na.rm=T),max(nx.clr$As.oes, na.rm=T)), subset=nx.clr$Site=="S 8")
#
plot(nx.clr$Depth.cm~nx.clr$Cd.ms, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cd.ms, na.rm=T),max(nx.clr$Cd.ms, na.rm=T)), subset=nx.clr$Site=="S 8")
#
plot(nx.clr$Depth.cm~nx.clr$Cu.ms, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cu.ms, na.rm=T),max(nx.clr$Cu.ms, na.rm=T)), subset=nx.clr$Site=="S 8")
#
plot(nx.clr$Depth.cm~nx.clr$Fe.oes, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Fe.oes, na.rm=T),max(nx.clr$Fe.oes, na.rm=T)), subset=nx.clr$Site=="S 8")
#
plot(nx.clr$Depth.cm~nx.clr$P.oes, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$P.oes, na.rm=T),max(nx.clr$P.oes, na.rm=T)), subset=nx.clr$Site=="S 8")
#
plot(nx.clr$Depth.cm~nx.clr$Pb.ms, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Pb.ms, na.rm=T),max(nx.clr$Pb.ms, na.rm=T)), subset=nx.clr$Site=="S 8")
#
plot(nx.clr$Depth.cm~nx.clr$Ti.ms, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Ti.ms, na.rm=T),max(nx.clr$Ti.ms, na.rm=T)), subset=nx.clr$Site=="S 8")
#
plot(nx.clr$Depth.cm~nx.clr$Zn.ms, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Zn.ms, na.rm=T),max(nx.clr$Zn.ms, na.rm=T)), subset=nx.clr$Site=="S 8")
#
plot(nx.clr$Depth.cm~nx.clr$REE.ms, type="o", ylim=c(130,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$REE.ms, na.rm=T),max(nx.clr$REE.ms, na.rm=T)), subset=nx.clr$Site=="S 8")
#
# ========================= Core 9 =========================
plot(nx.clr$Depth.cm~nx.clr$pH, type="o", ylim=c(75,0), xaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$pH, na.rm=T),max(nx.clr$pH, na.rm=T)), subset=nx.clr$Site=="S 9")
mtext(side=2,line=2.2,font=2, text="Core 9\nDepth (cm)", cex=1.2)
#
plot(nx.clr$Depth.cm~nx.clr$Eh.mV, type="o", ylim=c(75,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Eh.mV, na.rm=T),max(nx.clr$Eh.mV, na.rm=T)), subset=nx.clr$Site=="S 9")
#
plot(nx.clr$Depth.cm~nx.clr$AVS.umolg, type="o", ylim=c(75,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$AVS.umolg, na.rm=T),max(nx.clr$AVS.umolg, na.rm=T)), subset=nx.clr$Site=="S 9")
points(nx.clr$Depth.cm~nx.clr$TotalS.oes.umolg, type="o", pch=17, col="goldenrod3", 
       subset=nx.clr$Site=="S 9")
#
plot(nx.clr$Depth.cm~nx.clr$As.oes, type="o", ylim=c(75,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$As.oes, na.rm=T),max(nx.clr$As.oes, na.rm=T)), subset=nx.clr$Site=="S 9")
#
plot(nx.clr$Depth.cm~nx.clr$Cd.ms, type="o", ylim=c(75,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cd.ms, na.rm=T),max(nx.clr$Cd.ms, na.rm=T)), subset=nx.clr$Site=="S 9")
#
plot(nx.clr$Depth.cm~nx.clr$Cu.ms, type="o", ylim=c(75,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cu.ms, na.rm=T),max(nx.clr$Cu.ms, na.rm=T)), subset=nx.clr$Site=="S 9")
#
plot(nx.clr$Depth.cm~nx.clr$Fe.oes, type="o", ylim=c(75,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Fe.oes, na.rm=T),max(nx.clr$Fe.oes, na.rm=T)), subset=nx.clr$Site=="S 9")
#
plot(nx.clr$Depth.cm~nx.clr$P.oes, type="o", ylim=c(75,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$P.oes, na.rm=T),max(nx.clr$P.oes, na.rm=T)), subset=nx.clr$Site=="S 9")
#
plot(nx.clr$Depth.cm~nx.clr$Pb.ms, type="o", ylim=c(75,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Pb.ms, na.rm=T),max(nx.clr$Pb.ms, na.rm=T)), subset=nx.clr$Site=="S 9")
#
plot(nx.clr$Depth.cm~nx.clr$Ti.ms, type="o", ylim=c(75,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Ti.ms, na.rm=T),max(nx.clr$Ti.ms, na.rm=T)), subset=nx.clr$Site=="S 9")
#
plot(nx.clr$Depth.cm~nx.clr$Zn.ms, type="o", ylim=c(75,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Zn.ms, na.rm=T),max(nx.clr$Zn.ms, na.rm=T)), subset=nx.clr$Site=="S 9")
#
plot(nx.clr$Depth.cm~nx.clr$REE.ms, type="o", ylim=c(75,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$REE.ms, na.rm=T),max(nx.clr$REE.ms, na.rm=T)), subset=nx.clr$Site=="S 9")
#
# ========================= Core 10 =========================
plot(nx.clr$Depth.cm~nx.clr$pH, type="o", ylim=c(40,0), xaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$pH, na.rm=T),max(nx.clr$pH, na.rm=T)), subset=nx.clr$Site=="S 10")
mtext(side=2,line=2.2,font=2, text="Core 10\nDepth (cm)", cex=1.2)
#
plot(nx.clr$Depth.cm~nx.clr$Eh.mV, type="o", ylim=c(40,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Eh.mV, na.rm=T),max(nx.clr$Eh.mV, na.rm=T)), subset=nx.clr$Site=="S 10")
#
plot(nx.clr$Depth.cm~nx.clr$AVS.umolg, type="o", ylim=c(40,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$AVS.umolg, na.rm=T),max(nx.clr$AVS.umolg, na.rm=T)), subset=nx.clr$Site=="S 10")
points(nx.clr$Depth.cm~nx.clr$TotalS.oes.umolg, type="o", pch=17, col="goldenrod3", 
       subset=nx.clr$Site=="S 10")
#
plot(nx.clr$Depth.cm~nx.clr$As.oes, type="o", ylim=c(40,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$As.oes, na.rm=T),max(nx.clr$As.oes, na.rm=T)), subset=nx.clr$Site=="S 10")
#
plot(nx.clr$Depth.cm~nx.clr$Cd.ms, type="o", ylim=c(40,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cd.ms, na.rm=T),max(nx.clr$Cd.ms, na.rm=T)), subset=nx.clr$Site=="S 10")
#
plot(nx.clr$Depth.cm~nx.clr$Cu.ms, type="o", ylim=c(40,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cu.ms, na.rm=T),max(nx.clr$Cu.ms, na.rm=T)), subset=nx.clr$Site=="S 10")
#
plot(nx.clr$Depth.cm~nx.clr$Fe.oes, type="o", ylim=c(40,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Fe.oes, na.rm=T),max(nx.clr$Fe.oes, na.rm=T)), subset=nx.clr$Site=="S 10")
#
plot(nx.clr$Depth.cm~nx.clr$P.oes, type="o", ylim=c(40,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$P.oes, na.rm=T),max(nx.clr$P.oes, na.rm=T)), subset=nx.clr$Site=="S 10")
#
plot(nx.clr$Depth.cm~nx.clr$Pb.ms, type="o", ylim=c(40,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Pb.ms, na.rm=T),max(nx.clr$Pb.ms, na.rm=T)), subset=nx.clr$Site=="S 10")
#
plot(nx.clr$Depth.cm~nx.clr$Ti.ms, type="o", ylim=c(40,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Ti.ms, na.rm=T),max(nx.clr$Ti.ms, na.rm=T)), subset=nx.clr$Site=="S 10")
#
plot(nx.clr$Depth.cm~nx.clr$Zn.ms, type="o", ylim=c(40,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Zn.ms, na.rm=T),max(nx.clr$Zn.ms, na.rm=T)), subset=nx.clr$Site=="S 10")
#
plot(nx.clr$Depth.cm~nx.clr$REE.ms, type="o", ylim=c(40,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$REE.ms, na.rm=T),max(nx.clr$REE.ms, na.rm=T)), subset=nx.clr$Site=="S 10")
#
# ========================= Core 11 =========================
plot(nx.clr$Depth.cm~nx.clr$pH, type="o", ylim=c(60,0), xaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$pH, na.rm=T),max(nx.clr$pH, na.rm=T)), subset=nx.clr$Site=="S 11")
mtext(side=2,line=2.2,font=2, text="Core 11\nDepth (cm)", cex=1.2)
#
plot(nx.clr$Depth.cm~nx.clr$Eh.mV, type="o", ylim=c(60,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Eh.mV, na.rm=T),max(nx.clr$Eh.mV, na.rm=T)), subset=nx.clr$Site=="S 11")
#
plot(nx.clr$Depth.cm~nx.clr$AVS.umolg, type="o", ylim=c(60,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$AVS.umolg, na.rm=T),max(nx.clr$AVS.umolg, na.rm=T)), subset=nx.clr$Site=="S 11")
points(nx.clr$Depth.cm~nx.clr$TotalS.oes.umolg, type="o", pch=17, col="goldenrod3", 
       subset=nx.clr$Site=="S 11")
#
plot(nx.clr$Depth.cm~nx.clr$As.oes, type="o", ylim=c(60,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$As.oes, na.rm=T),max(nx.clr$As.oes, na.rm=T)), subset=nx.clr$Site=="S 11")
#
plot(nx.clr$Depth.cm~nx.clr$Cd.ms, type="o", ylim=c(60,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cd.ms, na.rm=T),max(nx.clr$Cd.ms, na.rm=T)), subset=nx.clr$Site=="S 11")
#
plot(nx.clr$Depth.cm~nx.clr$Cu.ms, type="o", ylim=c(60,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cu.ms, na.rm=T),max(nx.clr$Cu.ms, na.rm=T)), subset=nx.clr$Site=="S 11")
#
plot(nx.clr$Depth.cm~nx.clr$Fe.oes, type="o", ylim=c(60,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Fe.oes, na.rm=T),max(nx.clr$Fe.oes, na.rm=T)), subset=nx.clr$Site=="S 11")
#
plot(nx.clr$Depth.cm~nx.clr$P.oes, type="o", ylim=c(60,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$P.oes, na.rm=T),max(nx.clr$P.oes, na.rm=T)), subset=nx.clr$Site=="S 11")
#
plot(nx.clr$Depth.cm~nx.clr$Pb.ms, type="o", ylim=c(60,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Pb.ms, na.rm=T),max(nx.clr$Pb.ms, na.rm=T)), subset=nx.clr$Site=="S 11")
#
plot(nx.clr$Depth.cm~nx.clr$Ti.ms, type="o", ylim=c(60,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Ti.ms, na.rm=T),max(nx.clr$Ti.ms, na.rm=T)), subset=nx.clr$Site=="S 11")
#
plot(nx.clr$Depth.cm~nx.clr$Zn.ms, type="o", ylim=c(60,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Zn.ms, na.rm=T),max(nx.clr$Zn.ms, na.rm=T)), subset=nx.clr$Site=="S 11")
#
plot(nx.clr$Depth.cm~nx.clr$REE.ms, type="o", ylim=c(60,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$REE.ms, na.rm=T),max(nx.clr$REE.ms, na.rm=T)), subset=nx.clr$Site=="S 11")
#
# ========================= Core 12 =========================
plot(nx.clr$Depth.cm~nx.clr$pH, type="o", ylim=c(45,0), xaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$pH, na.rm=T),max(nx.clr$pH, na.rm=T)), subset=nx.clr$Site=="S 12")
mtext(side=2,line=2.2,font=2, text="Core 12\nDepth (cm)", cex=1.2)
#
plot(nx.clr$Depth.cm~nx.clr$Eh.mV, type="o", ylim=c(45,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Eh.mV, na.rm=T),max(nx.clr$Eh.mV, na.rm=T)), subset=nx.clr$Site=="S 12")
#
plot(nx.clr$Depth.cm~nx.clr$AVS.umolg, type="o", ylim=c(45,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$AVS.umolg, na.rm=T),max(nx.clr$AVS.umolg, na.rm=T)), subset=nx.clr$Site=="S 12")
points(nx.clr$Depth.cm~nx.clr$TotalS.oes.umolg, type="o", pch=17, col="goldenrod3", 
       subset=nx.clr$Site=="S 12")
#
plot(nx.clr$Depth.cm~nx.clr$As.oes, type="o", ylim=c(45,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$As.oes, na.rm=T),max(nx.clr$As.oes, na.rm=T)), subset=nx.clr$Site=="S 12")
#
plot(nx.clr$Depth.cm~nx.clr$Cd.ms, type="o", ylim=c(45,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cd.ms, na.rm=T),max(nx.clr$Cd.ms, na.rm=T)), subset=nx.clr$Site=="S 12")
#
plot(nx.clr$Depth.cm~nx.clr$Cu.ms, type="o", ylim=c(45,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Cu.ms, na.rm=T),max(nx.clr$Cu.ms, na.rm=T)), subset=nx.clr$Site=="S 12")
#
plot(nx.clr$Depth.cm~nx.clr$Fe.oes, type="o", ylim=c(45,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Fe.oes, na.rm=T),max(nx.clr$Fe.oes, na.rm=T)), subset=nx.clr$Site=="S 12")
#
plot(nx.clr$Depth.cm~nx.clr$P.oes, type="o", ylim=c(45,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$P.oes, na.rm=T),max(nx.clr$P.oes, na.rm=T)), subset=nx.clr$Site=="S 12")
#
plot(nx.clr$Depth.cm~nx.clr$Pb.ms, type="o", ylim=c(45,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Pb.ms, na.rm=T),max(nx.clr$Pb.ms, na.rm=T)), subset=nx.clr$Site=="S 12")
#
plot(nx.clr$Depth.cm~nx.clr$Ti.ms, type="o", ylim=c(45,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Ti.ms, na.rm=T),max(nx.clr$Ti.ms, na.rm=T)), subset=nx.clr$Site=="S 12")
#
plot(nx.clr$Depth.cm~nx.clr$Zn.ms, type="o", ylim=c(45,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$Zn.ms, na.rm=T),max(nx.clr$Zn.ms, na.rm=T)), subset=nx.clr$Site=="S 12")
#
plot(nx.clr$Depth.cm~nx.clr$REE.ms, type="o", ylim=c(45,0), xaxt="n", yaxt="n", ann=F, pch=16, 
     xlim=c(min(nx.clr$REE.ms, na.rm=T),max(nx.clr$REE.ms, na.rm=T)), subset=nx.clr$Site=="S 12")
#
