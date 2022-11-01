# SCRIPT BY ANDREW RATE (C) UNIVERSITY OF WESTERN AUSTRALIA 2017-
require(car)
names.of.cols <- names(nx.clr)
{cat("Shapiro-Wilk normality tests\n")
  cat("=============================\n")
  cat("Variable",",","W untransformed",",","p untransformed",",","W log-transformed",",",
      "p log-transformed",",","W power transformed",",","p power transformed",",","Power term","\n")}
for ( i in 23:42){
  pt1 <- powerTransform(nx.clr[,i])
  # nx.clr[paste0(names.of.cols[i],".log")]<-log10(nx.clr[i])
  # nx.clr[paste0(names.of.cols[i],".pow")] <- as.numeric((nx.clr[i])^as.vector(pt1$lambda))
  sw0 <- shapiro.test(nx.clr[,i])
  sw.pow <- shapiro.test((nx.clr[,i])^as.vector(pt1$lambda))
  sw.log <- shapiro.test(log10(nx.clr[,i]))
  cat(names.of.cols[i], ",", signif(sw0$statistic,4), ",", signif(sw0$p.value,4), ",",
      signif(sw.log$statistic,4), ",", signif(sw.log$p.value,4), ",", signif(sw.pow$statistic,4), ",",
      signif(sw.pow$p.value,4),",",signif(as.vector(pt1$lambda),4),"\n")
}
#
# remove temporary objects
rm(names.of.cols)
rm(pt1)
rm(sw0)
rm(sw.pow)
rm(sw.log)
rm(i)
# END