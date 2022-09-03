









library(hplot)
library(codeutils)

yrs <- 2000:2020
nyrs <- length(yrs)
reps <- 100
av <- 5:(nyrs+4)
dat <- matrix(0,nrow=reps,ncol=nyrs,dimnames=list(1:reps,yrs))
for (i in 1:nyrs) dat[,i] <- rnorm(reps,mean=av[i],sd=2)
qs <- apply(dat,2,quants)




plotprep(width=8, height=5.5,newdev=FALSE) 
parset()
maxy <- getmax(dat)
plot(yrs,dat[1,],type="p",pch=16,cex=0.2,col=1,ylim=c(0,maxy),panel.first=grid())
for (i in 1:reps) points(yrs,dat[i,],pch=16,col=1,cex=0.2)
polygon(poldat,border=NULL,col=rgb(1,0,0,0.1))






























