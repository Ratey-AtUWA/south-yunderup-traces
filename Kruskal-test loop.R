# SCRIPT BY ANDREW RATE (C) UNIVERSITY OF WESTERN AUSTRALIA 2017-
require(car)
names.of.cols <- names(nx.clr)
{cat("Kruskal-Wallis tests by Redox groups\n")
  cat("=============================================\n")
  cat("Variable",",","Kruskal-Wallis \u03C7\u00B2",",","p-value","\n")}
for ( i in 15:43){
  kwt0 <- kruskal.test(nx.clr[,i]~Redox, data=nx.clr) # powerTransform(nx.clr[,i])
  cat(names.of.cols[i], ",", signif(kwt0$statistic,4), ",", signif(kwt0$p.value,4),"\n")
}
#
# remove temporary objects
rm(names.of.cols)
rm(kwt0)
rm(i)
# END