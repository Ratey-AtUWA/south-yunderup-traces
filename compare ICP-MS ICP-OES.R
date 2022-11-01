names(nx)

# [1] "Type"      "Site"      "Depth.cm"     "Distance..m"    
# [5] "pH"      "Eh.mV"      "EC.mScm"     "AVS.umolg"    
# [9] "TotalS.oes.umolg"   "Porewater.FeII.mgL"  
# [11] "Porewater.total.Fe.II.mgL" "Porewater.FeIII.mgL"  
# [13] "Pore.water.sulfate.mgL" 
# [14] "As.oes"     "Co.oes"     "Cr.oes"     
# [17] "Cu.oes"     "Fe.oes"     "Mn.oes"     "Ni.oes"     
# [21] "P.oes"      "Pb.oes"     "S.oes"      "Zn.oes"     
# [25] "Ti.ms"      "V.ms"      "Cr.ms"      "Mn.ms"     
# [29] "Fe.ms"      "Co.ms"      "Cu.ms"      "Zn.ms"     
# [33] "As.ms"      "Rb.ms"      "Sr.ms"      "Mo.ms"     
# [37] "Cd.ms"      "Ba.ms"      "Pb.ms"      "Bi.ms"     
# [41] "Th.ms"      "U.ms"

par(mfrow = c(3,3), mar = c(3,3,1,1), mgp = c(1.6,0.3,0), 
    tcl = 0.2, font.lab = 2, cex.lab = 1.2, cex = 1., 
    lend = "square", ljoin = "mitre")
with(nx, plot(As.ms ~ As.oes)) ; abline(0,1, col = "red2")
with(nx, plot(Co.ms ~ Co.oes)) ; abline(0,1, col = "red2")
with(nx, plot(Cr.ms ~ Cr.oes)) ; abline(0,1, col = "red2")
with(nx, plot(Cu.ms ~ Cu.oes)) ; abline(0,1, col = "red2")
plot(c(0,1),c(0,1), ann = F, xaxt="n", yaxt="n", type="n", bty="n")
legend("center", bty = "n", cex = 1.4, 
       legend = c("Data","1:1 line"), 
       col = c(1,2), text.col = c(1,2), 
       pch = c(1,NA), lty = c(NA,1))
with(nx, plot(Fe.ms ~ Fe.oes)) ; abline(0,1, col = "red2")
with(nx, plot(Mn.ms ~ Mn.oes)) ; abline(0,1, col = "red2")
with(nx, plot(Pb.ms ~ Pb.oes)) ; abline(0,1, col = "red2")
with(nx, plot(Zn.ms ~ Zn.oes)) ; abline(0,1, col = "red2")
