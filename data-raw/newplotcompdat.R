
library(codeutils)
library(hplot)






compdata=x;sau="sauX";horizline=c(5,10);console=TRUE;outdir=""
barcol="red";bordercol="black";ylabel="Age Structure"
numcol <- ncol(x)
if (numcol > 20) x1 <- x[,1:20]

x1 <- x[,21:numcol]


plotcompdata2(compdata=x1,sau="sauX",horizline=c(5,10),console=TRUE)



if ((!console) & (nchar(tabname) > 0)) {

}


