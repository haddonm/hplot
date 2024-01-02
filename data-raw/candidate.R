



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




# diagrams within hplot---------------------------------------

library(codeutils)
library(hplot)


plotprep(width=10,height=10)

makecanvas()

putcircle(origx = 50, origy = 50, radius = 10, col = NA, lwd = 3, 
          fill = RGB("red",127))
putcircle(origx = 30, origy = 30, radius = 25, col = NA, lwd = 3, 
          fill = RGB("yellow",127))

putoblong(x1=50,x2=20,y1=10,y2=30,col=NA,fill=RGB("blue",63),lwd=1)

puttriangle(c(40,60,80),c(40,60,40),col=NA,fill=RGB("red",50),lwd=1)
puttriangle(c(40,60,80),c(40,70,40),col=NA,fill=RGB("red",50),lwd=1)
puttriangle(c(40,60,80),c(40,80,40),col=NA,fill=RGB("red",50),lwd=1)
puttriangle(c(40,60,80),c(40,90,40),col=NA,fill=RGB("red",50),lwd=1)
puttriangle(c(40,60,80),c(40,100,40),col=NA,fill=RGB("red",50),lwd=1)


makecanvas()
for (i in 1:20) {
  putoblong(x1=(40+2*i),x2=(55+1.2*i),y1=(40+2*i),y2=(65+2*i),col="black")
}




# Flow Charts -----------------------------------



updatecanvas <- function(canvas,form,coords) {
    canvas$objects <- canvas$objects + 1
    canvas$form <- c(canvas$form,form)
    canvas$coords[[canvas$objects]] <- coords
  return(canvas)
} # end of updatecanvas

puttextbox <- function(xy,width,height,canvas,txt="",cex=1.5,...) {
# xy=c(50,90);width=26;height=12;txt="do_MSE";cex=1.5;canvas=canvas  
  x1= xy[1]-width/2;    x2 <- xy[1]+width/2
  y1 <- xy[2]-height/2; y2 <- xy[2]+height/2
  coords <- c(xy=xy,xL=x1,xR=x2,yB=y1,yT=y2)
  putoblong(x1,x2,y1,y2,col="black")
  text(xy[1],xy[2],txt,cex=cex,...)
  canvas <- updatecanvas(canvas,form="txtbox",coords=coords)
  return(canvas)
} # end of puttextbox



joinobjs <- function(canvas,obj1,obj2,connect,lwd=2,code=2,
                     angle=20,length=0.1) {
# canvas=canvas;obj1=5;obj2=4;connect="downright";lwd=3;code=2;angle=20;length=0.1  
  njoin <- canvas$njoin
  update <- TRUE
  if (njoin > 0) {
     for (i in 1:njoin) {
       if ((obj1 == canvas$join[[i]][1]) & (obj2 == canvas$join[[i]][2]))
         update=FALSE
     }
  }
  cord1 <- canvas$coords[[obj1]]
  cord2 <- canvas$coords[[obj2]]
  if (connect == "topdown") {
    begin <- c(cord1["xy1"],cord1["yB"])
    end <- c(cord2["xy1"],cord2["yT"])
  }
  if (connect == "leftright") {
    begin <- c(cord1["xL"],cord1["xy2"])
    end <- c(cord2["xR"],cord2["xy2"])
  }
  if (connect == "downright") {
    lines(c(cord1["xy1"],cord1["xy1"]),c(cord1["yB"],cord2["xy2"]),lwd=lwd)
    begin <- c(cord1["xy1"],cord2["xy2"])
    end <- c(cord2["xL"],cord2["xy2"])
  }
  arrows(begin[1],begin[2],end[1],end[2],length=length,angle=angle,
         code=2,lwd=lwd)
  if (update) {
    canvas$njoin <- canvas$njoin + 1
    label <- c("obj1","obj2","x1","y1","x2","y2")
    out <- c(obj1,obj2,begin,end); names(out) <- label
    if (connect %in% c("downright","downleft")) {
      out <- c(out,cord1["xy1"],cord1["yB"],cord1["xy1"],cord2["xy2"])
      names(out) <- c(label,"x3","y3","x4","y4")
    }
    canvas$join[[canvas$njoin]] <- out
  }
  return(canvas)
} # end of join objects


labelobjects <- function(canvas) {
  nobj <- canvas$objects
  for (i in 1:nobj) { # i=3
    cord <- canvas$coords[[i]]
    form <- canvas$form[i]
    if (form == "txtbox") {
      x1 <- cord["xL"] + (cord["xy1"] - cord["xL"])/2
      y1 <- cord["yT"]
      text(x1,y1,i,pos=3,cex=1.5)
    }
    if (form == "decision") {
      x1 <- cord["xL"] + (cord["xy1"] - cord["xL"])/2
      y1 <- cord["xy2"] + (cord["yT"] - cord["xy2"])/2
      text(x1,y1,i,pos=3,cex=1.5)
    }
  }
} # end of labelobjects
labelobjects(canvas)


putdecision <- function(xy,width,height,canvas,txt="",cex=1.0,...) {
   x4 <- c(xy[1],xy[1]+width/2,xy[1],xy[1]-width/2,xy[1])
   y4 <- c(xy[2]+height/2,xy[2],xy[2]-height/2,xy[2],xy[2]+height/2)
   coords <- c(xy,xy[1]-width/2,xy[1]+width/2,xy[2]-height/2,xy[2]+height/2) 
   names(coords)=c("xy1","xy2","xL","xR","yB","yT")
   lines(x4,y4,lwd=3)
   text(xy[1],xy[2],txt,cex=cex,...)
   text(xy[1],xy[2]-height/2,"N",pos=3,cex=cex)
   text(xy[1]-width/2,xy[2],"Y",pos=3,cex=cex) 
   canvas <- updatecanvas(canvas,form="decision",coords=coords)
   return(invisible(canvas))
} # end of putdecision

  


library(codeutils)
library(hplot)

plotprep(width=10,height=10)
canvas <- makecanvas()
#abline(v=c(0,50,100),lty=2,col="grey")

canvas <- puttextbox(xy=c(50,91),width=26,height=6,canvas=canvas,
                     txt="do_MSE",cex=1.5,font=7)
canvas <- puttextbox(xy=c(50,79),width=26,height=6,canvas=canvas,
                     txt="makequilzone",cex=1.5,font=7)
canvas <- putdecision(xy=c(50,65),width=20,height=12,canvas=canvas,
            txt="Any initdepl < 1.0",cex=1.3,font=7) 
canvas <- puttextbox(xy=c(50,50),width=26,height=6,canvas=canvas,
                     txt="dohistoricC",cex=1.5,font=7)
canvas <- puttextbox(xy=c(25,65),width=15,height=6,canvas=canvas,
                     txt="depleteSAU",cex=1.5,font=7)
canvas <- puttextbox(xy=c(50,40),width=26,height=6,canvas=canvas,
                     txt="Plots and Tables",cex=1.5,font=7)
canvas <- puttextbox(xy=c(50,30),width=26,height=6,canvas=canvas,
                     txt="prepareprojection",cex=1.5,font=7)
canvas <- puttextbox(xy=c(50,20),width=26,height=6,canvas=canvas,
                     txt="doprojections",cex=1.5,font=7)

canvas <- joinobjs(canvas=canvas,obj1=1,obj2=2,connect="topdown",lwd=3)
canvas <- joinobjs(canvas=canvas,obj1=2,obj2=3,connect="topdown",lwd=3)
canvas <- joinobjs(canvas=canvas,obj1=3,obj2=5,connect="leftright",lwd=3)
canvas <- joinobjs(canvas=canvas,obj1=3,obj2=4,connect="topdown",lwd=3)
canvas <- joinobjs(canvas=canvas,obj1=5,obj2=4,connect="downright",lwd=3)
canvas <- joinobjs(canvas=canvas,obj1=4,obj2=6,connect="topdown",lwd=3)
canvas <- joinobjs(canvas=canvas,obj1=6,obj2=7,connect="topdown",lwd=3)
canvas <- joinobjs(canvas=canvas,obj1=7,obj2=8,connect="topdown",lwd=3)








labelobjects(canvas)



 data(sps)
 cbv <- tapply(sps$catch_kg,list(sps$Vessel,sps$Year),sum,na.rm=TRUE)/1000
 dim(cbv)
 early <- rowSums(cbv[,1:6],na.rm=TRUE)
 late <- rowSums(cbv[,7:12],na.rm=TRUE)
 cbv1 <- cbv[order(late,-early),]
 plotprep(width=10,height=8)
 yearBubble(cbv1,ylabel="Catch by Trawl",vline=2006.5,diam=0.2,txt=c(3,4,5,5))
 
 cbv1[,9] <- NA
 yearBubble(cbv1,ylabel="Catch by Trawl",vline=2006.5,diam=0.2,txt=c(3,4,5,5))
 



















