#adjusting p values with Benjamini Hochberg method
pvals = c(0.6314, .0000, 0.02303, .0000)
BH = p.adjust(pvals, "BH")
res = cbind(pvals, BH = round(BH, 3))
