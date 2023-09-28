



#define libraries and R options
library(codeutils) # this replaces hutils
library(hplot) 
library(makehtml)
library(sizemod)
library(mvtnorm)
library(TasHS)   # to use  ploteHSphase
options("show.signif.stars"=FALSE,"stringsAsFactors"=FALSE,
        "max.print"=50000,"width"=240)
#define rundir. Set postfix so can cycle through sub-directories in block11
dropbdir <- getDBdir()
prefixdir <- pathtopath(dropbdir,"A_CodeUse/sizemoduse/block11/")
postfix <- "C15_7_8"   #   _2phase
controlfile <- "controlLBM.csv"

rundir <- pathtopath(prefixdir,postfix)
# FIRST OPTIMIZATION ---------------------------------------------------
# read in ctrl, all data, and the pin file

outinit <- initiateModel(rundir=rundir,ctrlfile=controlfile,
                         indices=1,mincount=120,backup="n")


fish <- outinit$fish



plotprep(width=13,height=4)
parset(margin=c(0.3,0.3,0.1,0.1),outmargin=c(0,1,0,0))
maxy <- getmax(fish[,"catch"])
barplot(fish[,"catch"],axis.lty=1,ylim=c(0,maxy),col="red",border="green",
        panel.first=grid())
mtext(text="Catch (t)",side=2,line=-0.2,outer=TRUE,cex=1.2)



sizecomp <- outinit$sizecomp
sizecomp[,1:5]













plotcompdata(compdata=sizecomp,sau="sau11",horizline=c(5,13),console=TRUE,
               bordercol="red")

 comps <- rnorm(400,mean=10,sd=1)
 x <- matrix(comps,nrow=20,ncol=20,dimnames=list(1:20,letters[1:20]))
 plotcompdata(compdata=x,sau="Here_and_There",horizline=c(5,10),console=TRUE)


plotprep(width=9,height=4.5)
parset()
# parset(outmargin=c(1,3,1,3),margin=c(0.2,0.2,0,0))
# matfor <- matrix(c(1:20),1,20,byrow=TRUE)
# layout(matfor,heights=rep(1,20),TRUE)
barplot(sizecomp[,1],axes=FALSE,col="red",space=0,axis.lty=1.0,
        cex.names=1.0)
mtext(label[1],side=2,outer=FALSE,cex=1)
abline(v=c(5,13),lwd=3,col=c("green","blue"))


















