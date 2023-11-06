



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


# The Game ----------------------------------
plotprep(width=7, height=10)
makecanvas()
makeoblong(5,95,0,100,lwd=2,col="black")
for (i in seq(0,75,25)){
  makeoblong(5,25,i,i+25,lwd=1,col="grey")
  makeoblong(25,95,i,i+25,lwd=1,col="grey")
}
mtext("offshore",side=3,adj=0.60,outer=TRUE,cex=1.0,line=-2,)
mtext("onshore",side=3,adj=0.15,outer=TRUE,cex=1.0,line=-2,)
mtext("Lat 1",side=4,adj=0.85,outer=TRUE,cex=1.0,line=-3)
mtext("Lat 2",side=4,adj=0.625,outer=TRUE,cex=1.0,line=-3)
mtext("Lat 3",side=4,adj=0.375,outer=TRUE,cex=1.0,line=-3)
mtext("Lat 4",side=4,adj=0.15,outer=TRUE,cex=1.0,line=-3)
mtext("North",side=2,adj=0.85,outer=TRUE,cex=1.0,line=-3)
mtext("North",side=2,adj=0.625,outer=TRUE,cex=1.0,line=-3)
mtext("South",side=2,adj=0.375,outer=TRUE,cex=1.0,line=-3)
mtext("South",side=2,adj=0.15,outer=TRUE,cex=1.0,line=-3)
makeoblong(7,23,2,98,lwd=2,col="red")
text(15,50,"Recreational Fishery (Fleet 1)",srt=90,cex=2.0)
makeoblong(27,93,2,98,lwd=4,col="blue")
text(75,50,"Non-State Fishery (Fleet 3)",srt=90,cex=2.0,col="blue")
makeoblong(29,91,4,96,lwd=4,col="green")
lines(c(29,91),c(50,50),lwd=2,col="green")
text(45,75,"State Fishery (Fleet 2N)",srt=90,cex=2.0,col="green")
text(45,25,"State Fishery (Fleet 2S)",srt=90,cex=2.0,col="green")
mtext("larger",side=1,adj=0.60,outer=TRUE,cex=1.0,line=-2)
mtext("older",side=1,adj=0.60,outer=TRUE,cex=1.0,line=-1)
mtext("smaller",side=1,adj=0.15,outer=TRUE,cex=1.0,line=-2)
mtext("younger",side=1,adj=0.15,outer=TRUE,cex=1.0,line=-1)




# diagrams within hpolot---------------------------------------

library(codeutils)
library(hplot)


plotprep(width=10,height=10)

makecanvas()

putcircle(origx = 50, origy = 50, radius = 10, col = NA, lwd = 3, fill = RGB("red",127))
putcircle(origx = 30, origy = 30, radius = 25, col = NA, lwd = 3, fill = RGB("yellow",127))

putoblong(x1=50,x2=80,y1=10,y2=30,col=NA,fill=RGB("blue",63),lwd=1)

puttriangle(c(40,60,80),c(40,60,40),col=NA,fill=RGB("red",50),lwd=1)
puttriangle(c(40,60,80),c(40,70,40),col=NA,fill=RGB("red",50),lwd=1)
puttriangle(c(40,60,80),c(40,80,40),col=NA,fill=RGB("red",50),lwd=1)
puttriangle(c(40,60,80),c(40,90,40),col=NA,fill=RGB("red",50),lwd=1)
puttriangle(c(40,60,80),c(40,100,40),col=NA,fill=RGB("red",50),lwd=1)


makecanvas()
for (i in 1:20) {
  putoblong(x1=(40+2*i),x2=(55+1.2*i),y1=(40+2*i),y2=(65+2*i),col="black")
}











