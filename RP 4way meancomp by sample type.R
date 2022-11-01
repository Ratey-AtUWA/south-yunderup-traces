require(car)
require(PMCMR)
options(warn=-1)
names.of.cols <- names(RP.clr.sort)
{cat("Kruskal-Wallis tests by sample type groups (",levels(RP.clr.sort$Type.adj),
    ") with pairwise multiple comparisons of mean rank sums\n")
cat("==========================================================================================\n")
cat("Variable",",","Kruskal-Wallis Chi-squared",",","Overall p-value",",", 
    comps0[1],",", comps0[2],",", comps0[3],",", comps0[4],",", comps0[5],",", comps0[6],",",
    levels(RP.clr.sort$Type.adj)[1], ",", levels(RP.clr.sort$Type.adj)[2], ",",
    levels(RP.clr.sort$Type.adj)[3],",",levels(RP.clr.sort$Type.adj)[4],",","Overall mean\n")}
for ( i in 14:51){
  kwt0 <- kruskal.test(RP.clr.sort[,i]~Type.adj, data=RP.clr.sort) # powerTransform(RP.clr.sort[,i])
  pkt0 <- posthoc.kruskal.nemenyi.test(x=RP.clr.sort[,i], g=RP.clr.sort$Type.adj, dist="Tukey")
  comps0 <- names(get.pvalues(pkt0))
  pvals0 <- as.numeric(get.pvalues(pkt0))
  means0 <- tapply(RP.clr.sort[,i], RP.clr.sort$Type.adj, mean, na.rm=T)
  cat(names.of.cols[i], ",", signif(kwt0$statistic,4), ",", signif(kwt0$p.value,4), ",", 
      pvals0[1],",", pvals0[2],",", pvals0[3],",", pvals0[4],",", pvals0[5],",", pvals0[6],",",
      as.numeric(means0[1]),",", as.numeric(means0[2]),",", as.numeric(means0[3]),",", as.numeric(means0[4]),",",
      mean(RP.clr.sort[,i], na.rm=T),"\n")
}
# remove temporary objects
rm(names.of.cols)
rm(pkt0)
rm(kwt0)
rm(comps0)
rm(pvals0)
rm(i)
#
# names(RP.clr.sort)
