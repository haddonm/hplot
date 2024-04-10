

#' @title addlnorm estimates a log-normal distribution from output of hist.
#'
#' @description  addlnorm estimates a log-normal distribution from output of
#'    a histogram of a data set.
#'
#' @param inhist is the output from a call to 'hist' (see examples)
#' @param xdata is the data that is being plotted in the histogram.
#' @param inc defaults to a value of 0.01; is the fine grain increment used to
#'    define the normal curve. The histogram will be coarse grained relative to
#'    this.
#'
#' @return a 4 x N matrix of x and y values to be used to plot the fitted normal
#'    probability density function.Combined with estiamtes of mean(log(indata))
#'    and log(sd(indata))
#' @export addlnorm
#'
#' @examples
#' egdata <- rlnorm(200,meanlog=0.075,sdlog=0.5)
#' outh <- hist(egdata,main="",col=2,breaks=seq(0,8,0.2))
#' ans <- addlnorm(outh,egdata)
#' lines(ans[,"x"],ans[,"y"],lwd=2,col=4)
addlnorm <- function(inhist,xdata,inc=0.01) {
  lower <- inhist$breaks[1]
  upper <- tail(inhist$breaks,1)
  cw <- inhist$breaks[2]-inhist$breaks[1]
  x <- seq(lower,upper, inc) #+ (cw/2)
  avCE <- mean(log(xdata),na.rm=TRUE)
  sdCE <- sd(log(xdata),na.rm=TRUE)
  N <- length(xdata)
  ans <- cbind(x,(N*cw) * stats::dlnorm(x,avCE,sdCE),avCE,sdCE)
  colnames(ans) <- c("x","y","avCE","sdCE")
  return(ans)
} # end of addlnorm

#' @title addnorm adds a normal distribution to a histogram of a data set.
#'
#' @description  addnorm adds a normal distribution to a histogram of a data
#'    set. This is generally to be used to illustrate whether log-transformation
#'    normalizes a set of catch or cpue data.
#'
#' @param inhist is the output from a call to 'hist' (see examples)
#' @param xdata is the data that is being plotted in the histogram.
#' @param inc defaults to a value of 0.01; is the fine grain increment used to
#'    define the normal curve. The histogram will be coarse grained relative to
#'    this.
#'
#' @return a list with a vector of 'x' values and a vector of 'y' values (to be
#'    used to plot the fitted normal probability density function), and a vector
#'    of four called 'stats' containing the mean, standard deviation, number of
#'    observations and median of the input data
#'    
#' @export addnorm
#' @examples
#' x <- rnorm(1000,mean=5,sd=1)
#' dev.new(height=6,width=4,noRStudioGD = TRUE)
#' par(mfrow= c(1,1),mai=c(0.5,0.5,0.3,0.05))
#' par(cex=0.85, mgp=c(1.5,0.35,0), font.axis=7)
#' outH <- hist(x,breaks=25,col=3,main="")
#' nline <- addnorm(outH,x)
#' lines(nline$x,nline$y,lwd=3,col=2)
#' print(nline$stats)
addnorm <- function(inhist,xdata,inc=0.01) {
  lower <- inhist$breaks[1]
  upper <- tail(inhist$breaks,1)
  cw <- inhist$breaks[2]-inhist$breaks[1]
  x <- seq(lower,upper, inc) #+ (cw/2)
  avCE <- mean(xdata,na.rm=TRUE)
  sdCE <- sd(xdata,na.rm=TRUE)
  N <- length(xdata)
  med <- median(xdata,na.rm=TRUE)
  ans <- list(x=x,y=(N*cw)*dnorm(x,avCE,sdCE),
              stats=c(mean=avCE,stdev=sdCE,N=N,median=med))
  return(ans)
} # end of addnorm

#' @title cart2pol converts cartesian coordinates into the polar angle
#' 
#' @description cart2pol as a step in converting cartesian coordinates into
#'     polar coordinates this calculates the angle, in degrees, from x y
#'     values
#'
#' @param x either a vector of two values of a matrix of pairs of values
#'
#' @return a single angle of vector of angles
#' @export
#'
#' @examples
#'   cart2pol(c(3,3))  # should be 45
#'   dat <- matrix(c(3,4,5,7),nrow=2,ncol=2,byrow=TRUE)
#'   print(dat)
#'   cart2pol(dat)     # should be 36.8699 twice.
cart2pol <- function(x){
  if (is.vector(x)) angle <- 180 * (atan2(x[1],x[2])) / pi
  if (is.matrix(x)) angle <- 180 * (atan2(x[,1],x[,2])) / pi
  return(angle=angle)
} # end of cart2pol

#' @title categoryplot generates a bubble plot of the contents of a matrix
#' 
#' @description categoryplot generates a bubble plot of the contents of a matrix
#'     in an effort to visualize 2-D trends in the data. It must have the numeric
#'     year variable in the columns. So if using table or tapply to generate the
#'     matrix put year last in the list of variables. 
#'
#' @param x the matrix of values to be plotted
#' @param xlab the label for the x-axis, default=''
#' @param ylab the label for the y-axis, default=''
#' @param mult the multiplier for the values. Should be selected so that the 
#'     circles produce a visual representation of the variation in the data
#' @param gridx should grey grid-lines be added for each year. default=FALSE
#' @param addtotal should the sum of the year columns be printed at the top of
#'     the diagram. default=FALSE. If TRUE it prints the column totals 
#'     sequentially addlines-2 lines and then addlines-1 lines above the circles
#' @param addlines if addtotal is TRUE then a number of lines are added at the 
#'     top of the plot to contain the column totals. This argument determines 
#'     the number of extra lines. default=3, but if only a few then a smaller 
#'     number would be more appropriate. If addtotal = FALSE, then addlines is 
#'     ignored
#' @param textcex the totals at the top of the plot can be too large with the
#'     default=1 so change the value here if required.
#'
#' @return nothing but it does generate a plot
#' @export
#'
#' @examples
#' xmat <- matrix(rnorm(25,5,2),nrow=5,ncol=5,dimnames=list(1:5,1:5))
#' categoryplot(xmat,mult=0.03,ylab="Random Numbers",addtotal=TRUE,addline=2)
categoryplot <- function(x,xlab="",ylab="",mult=0.1,gridx=FALSE,addtotal=FALSE,
                         addlines=3,textcex=1) {  
  xlabel <- colnames(x)
  nx <- length(xlabel)
  years <- as.numeric(xlabel) # assumes columns are years
  ylabel <- rownames(x) # make no assumption about rows. can be categorical
  ny <- length(ylabel)
  yvar <- seq(1,ny,1)
  upy <- ny+1
  if (addtotal) upy <- ny+addlines
  yrtot <- colSums(x,na.rm=TRUE)
  countyr <- apply(x,2,countgtzero)
  xval <- x
  rownames(xval) <- yvar
  values <- expandmatrix(xval)
  plotprep(width=7, height=6, newdev=FALSE)
  parset(cex=0.85)
  plot(values[,1],values[,2],type="n",xlab=xlab,ylab=ylab,ylim=c(0,upy),
       yaxs="i",yaxt="n",xaxt="n",xaxs="r")
  axis(side=1,at=xlabel,labels=xlabel)
  axis(side=2,at=yvar,labels=ylabel)
  if (gridx) 
    for (i in 1:nx) abline(v=xlabel[i],lwd=1,lty=3,col="grey")
  for (i in 1:nx) # i = 1
    symbols(rep(xlabel[i],ny),1:ny,circles=(mult*x[,i]),inches=FALSE,add=TRUE,
            bg=rgb(1, 0, 0, 0.5), fg = "black")
  if (addtotal) {
    incstep <- trunc(addlines/2)
    if (incstep > 2) incstep <- incstep - 1 
    for (yr in 1:nx) {
      oddeven <- yr %% 2
      if (oddeven == 0) text(years[yr],(ny+incstep),round(yrtot[yr],1),cex=textcex)
      else  text(years[yr],(ny+incstep*2),round(yrtot[yr],1),cex=textcex)
    }
  }
  return(invisible(list(yrtotal=yrtot,yrcount=countyr)))
} # end of categoryplot


#' @title countgtzero used in apply to count the number >0 in a vector
#'
#' @description countgtzero used in apply to count number >0 in a vector
#' @param invect vector of values
#' @return A single value of number of values > 0
#' @export countgtzero
#' @examples
#'   set.seed(12346)
#'   x <- trunc(runif(10)*10)
#'   x
#'   countgtzero(x)  # should be 9
countgtzero <- function(invect) {
  pick <- which(invect > 0)
  return(length(pick))
}

#' @title diagrams provides the syntax of functions for making diagrams
#' 
#' @description diagrams provides the syntax of functions for making diagrams
#'
#' @return nothing but it write syntax for diagram functions to the console
#' @export
#'
#' @examples
#' diagrams()
diagrams <- function() {
  cat('circle(origx = 50, origy = 50, radius = 10, col = 1, lwd = 1) \n')
  cat('makecanvas(xstart = 0, xfinish = 100, ystart = 0, yfinish = 100) \n')
  cat('makerect(left, xinc, top, yinc, linecol = "grey", lwd = 1) \n')
  cat('makevx(init, inc) \n')
  cat('makevy(init, inc) \n')
  cat('plotoblong(x0, x1, y0, y1, border = 1, col = 0, lwd = 1)  \n')
} # end of diagrams

#' @title expandmatrix reshapes a matrix of values into a 3 column data.frame
#' 
#' @description expandmatrix takes an oblong matrix of values and expands it
#'     into a three column data.frame of row, column, value. This is then easier 
#'     to plot as a scattergram or is used within categoryplot. It expects to 
#'     have the year values in the columns = xvalues
#'
#' @param x a matrix of values 
#'
#' @return a 3-column matrix of (rows x cols) rows from the input matrix
#' @export
#'
#' @examples
#' x <- matrix(rnorm(25,5,1),nrow=5,ncol=5,dimnames=list(1:5,1:5))
#' res <- expandmatrix(x)
#' res
expandmatrix <- function(x) { #  x=t(numyr)
  ylabel <- as.numeric(rownames(x))
  xlabel <- as.numeric(colnames(x))
  nx <- length(xlabel)
  ny <- length(ylabel)
  res <- as.data.frame(matrix(0,nrow=(nx*ny),ncol=3))
  count <- 0
  for (i in 1:nx) {
    for (j in 1:ny) {
      count <- count + 1
      res[count,] <- c(xlabel[i],ylabel[j],x[j,i])
    }
  }
  rownames(res) <- paste0(res[,1],"_",res[,2])
  colnames(res) <- c("rows","cols","value")
  return(res)
} # end of expandmatrix


#' @title getmin generates the lower bound for a plot
#'
#' @description getmin generates lower bound for a plot where it is unknown
#'     whether the minimum is less than zero of not. If less than 0 then
#'     multiplying by the default mult of 1.05 works well but if the outcome
#'     if > 0 then the multiplier needs to be adjusted appropriately so 
#'     the minimum is slightly lower than the minimum of the data
#'
#' @param x the vector of data to be tested for its minimum
#' @param mult the multiplier for both ends, defaults to 1.05 (=0.95 if >0)
#'
#' @return a suitable lower bound for a plot if required
#' @export
#'
#' @examples
#' vect <- rnorm(10,mean=0,sd=2)
#' sort(vect)
#' getmin(vect,mult=1.0)
getmin <- function(x,mult=1.05) {
  ymin <- min(x,na.rm=TRUE)
  if (ymin < 0) {
    ymin <- ymin * mult
  } else {
    ymin <- ymin * (2 - mult)
  }
  return(ymin)
} # end of getmin

#' @title getmax generates the upper bound for a plot
#'
#' @description getmax generates upper bound for a plot where it is unknown
#'     whether the maximum is greater than zero of not. If > 0 then
#'     multiplying by the default mult of 1.05 works well but if the outcome
#'     if < 0 then the multiplier needs to be adjusted appropriately so the 
#'     maximum is slightly higher than the maximum of the data
#'
#' @param x the vector of data to be tested for its maximum
#' @param mult the multiplier for both ends, defaults to 1.05 (=0.95 if < 0)
#'
#' @return a suitable upper bound for a plot if required
#' @export
#'
#' @examples
#'  vect <- rnorm(10,mean=0,sd=2)
#'  sort(vect,decreasing=TRUE)
#'  getmax(vect,mult=1.0)
#'  vect <- rnorm(10,mean = -5,sd = 1.5)
#'  sort(vect,decreasing=TRUE)
#'  getmax(vect,mult=1.0)
getmax <- function(x,mult=1.05) {
  ymax <- max(x,na.rm=TRUE)
  if (ymax > 0) {
    ymax <- ymax * mult
  } else {
    ymax <- ymax * (2 - mult)
  }
  return(ymax)
} # end of getmax


#' @title histyear plots a histogram of a given variable for each year available
#'
#' @description histyear plots a histogram of a given variable for each year
#'     available
#'
#' @param x the data.frame of data with at least a 'Year' and pickvar present
#' @param xlimit the xaxis bounds for all histograms, defaults to c(NA,NA,NA),
#'     the values would be as used in seq(xlimit[1],xlimit[2],xlimit[3]). If the
#'     default is used then the 0 and 0.98 quantiles of the variable are used as
#'     the bounds with 25 bins
#' @param pickvar which variable to plot each year default = 'cpue'
#' @param years which variable name identifies the yaer column, default='year'
#' @param varlabel what label to use on x-axis, default = 'CPUE'
#' @param vline an optional vertical line to aid interpretation. If it is
#'     numeric it will be added to each plot
#' @param plots how many plots to generate, default = c(5,5)
#' @param normadd should a normal distribution be added to each plot. 
#'     default=TRUE
#' @param left on which side of each plot should the year and number of records 
#'     be placed left=TRUE is the default. left=FALSE will place text on right
#'
#' @return invisibly, a matrix of the year, mean value, stdev, and N number of
#'     observations. It also plots a histogram for each year and fits a
#'     normal distribution to each one.
#' @export
#'
#' @examples
#' \dontrun{
#' print("still to be developed")
#' # pickvar="x100nethr";years="year";varlabel="log(CPUE)";vline=NA;plots=plotnum;
#' # normadd=TRUE;left=FALSE;xlimit=c(0,250,10)
#' }
histyear <- function(x,xlimit=c(NA,NA,NA),
                     pickvar="cpue",years="year",varlabel="CPUE",
                     vline=NA,plots=c(5,5),normadd=TRUE,left=TRUE) {
  yrs <- sort(unique(x[,years]))
  nyr <- length(yrs)
  columns <- c("Year","maxcount","Mean","StDev","N","Median","Min","Max")
  results <- matrix(0,nrow=nyr,ncol=length(columns),dimnames=list(yrs,columns))
  par(mfcol=plots,mai=c(0.25,0.25,0.05,0.05),oma=c(1.2,1.0,0.0,0.0))
  par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
  if (left) adj=0 else adj=1
  if (is.na(xlimit[1])) {
    xlimit[1:2] <- quantile(x[,pickvar],probs=c(0,0.99))
    bins <- seq(xlimit[1],xlimit[2],length=25)
  } else { bins <- seq(xlimit[1],xlimit[2],xlimit[3]) }
  pickX <- which((x[,pickvar] >= xlimit[1]) &
                   (x[,pickvar] <= xlimit[2]))
  x2 <- droplevels(x[pickX,])
  for (yr in 1:nyr) {
    pick <- which(x2[,years] == yrs[yr])
    outh <- hist(x2[pick,pickvar],breaks=bins,col=2,main="",xlab="",ylab="")
    mtext(paste0("  ",yrs[yr]),side=3,outer=F,line=-2,font=7,cex=0.9,adj=adj)
    mtext(paste0("  ",length(pick)),side=3,outer=F,line=-3,font=7,cex=0.9,adj=adj)
    if (is.numeric(vline)) abline(v=vline,col=4,lwd=2)
    if (normadd) {
      pickmax <- which.max(outh$counts)
      ans <- addnorm(outh,x[pick,pickvar])
      lines(ans$x,ans$y,col=3,lwd=2)
      results[yr,] <- c(yrs[yr],outh$mids[pickmax],ans$stats,
                        range(x2[pick,pickvar],na.rm=TRUE))
    }
  }
  mtext("Frequency",side=2,outer=T,line=0.0,font=7,cex=1.0)
  mtext(varlabel,side=1,outer=T,line=0.0,font=7,cex=1.0)
  return(invisible(results))
} # end of histyear

#' @title inthist a replacement for the hist and boxplot functions
#'
#' @description inthist it is common to want to generate a list of counts as
#'     integers from an input vector and then plot then as columns of those
#'     counts. Alternatively, it is common to have a two-column matrix of 
#'     values and counts or totals where one wants to plot columns of those
#'     counts or totals against those values. inhist allows one to enter either 
#'     a vector of integers to be counted and plotted OR a matrix of values in 
#'     column 1 and counts or totals in column 2. The option of rounding 
#'     non-integers is available.
#'
#' @param x a vector of integers to be counted and plotted OR a matrix of
#'     values in column 1 and counts or totals in column 2
#' @param col the colour of the fill; defaults to black = 1, set this to 0
#'     for an empty bar, but then give a value for border
#' @param border the colour of the outline of each bar defaults to col
#' @param width denotes the width of each bar; defaults to 0.9, should be >0
#'     and <= 1
#' @param xlab the label for the x axis; defaults to ""
#' @param ylab the label for the y axis; defaults to ""
#' @param main the title for the individual plot; defaults to ""
#' @param lwd the line width of the border; defaults to 1
#' @param xmin sets the lower bound for x-axis; used to match plots, defaults to 
#'     NA whereupon the minimum of values is used
#' @param xmax sets the upper bound for x axis; used with multiple plots, 
#'     defaults to NA whereupon the maximum of values is used
#' @param ymax enables external control of the maximum y value; mainly of
#'     use when plotting multiple plots together.
#' @param plotout plot the histogram or not? Defaults to TRUE
#' @param prop plot the proportions rather than the counts, default=FALSE
#' @param inc sets the xaxis increment; used to customize the axis;
#'     defaults to 1.
#' @param xaxis set to FALSE to define the xaxis outside of inthist;
#'     defaults to TRUE
#' @param roundoff if values are not integers should they be rounded off to
#'     become integers? default=TRUE. Obviously only useful when inputting a
#'     matrix.
#' @param ... available to pass extra plot arguments, such as 
#'     panel.first=grid(), or whatever to the internal plot call
#'     
#' @return a matrix of values and counts with the proportions of counts and 
#'     values is returned invisibly
#' @export
#' 
#' @examples
#'   x <- trunc(runif(1000)*10) + 1
#'   inthist(x,col="grey",border=3,width=0.75,xlabel="Random Uniform",
#'           ylabel="Frequency")
#'   x <- as.matrix(cbind(c(1,2,3,4,5,6,7,8),trunc(runif(8,1,20))))
#'   inthist(x,col="grey",border=3,width=0.75,xlabel="integers",
#'           ylabel="Frequency")
inthist <- function(x,col=1,border=NULL,width=0.9,xlab="",ylab="",
                    main="",lwd=1,xmin=NA,xmax=NA,ymax=NA,plotout=TRUE,
                    prop=FALSE,inc=1,xaxis=TRUE,roundoff=TRUE,...) {
  #  x=ebtipy;col=2;border=3;width=0.9;xlab="";ylab="";main="";lwd=1;xmin=NA
  #  xmax=NA;ymax=NA;plotout=TRUE;prop=FALSE;inc=1;xaxis=TRUE;roundoff=TRUE
  if (class(x)[1] == "matrix") {
    counts <- x[,2]
    values <- x[,1]
  } else {
    counts <- table(x)
    if (length(counts) == 0) stop("No data provided \n\n")
    values <- as.numeric(names(counts))
  }
  if ((sum(!(abs(values - round(values)) < .Machine$double.eps^0.5)) > 0) &
      (roundoff)) {
    warning("Using 'inthist' with non-integers; Values now rounded \n")
    values <- round(values,0)
  }
  if ((width <= 0) | (width > 1)) {
    warning("width values should be >0 and <= 1")
    width <- 1
  }
  counts <- as.numeric(counts)
  nct <- length(counts)
  propor <- counts/sum(counts,na.rm=TRUE)
  if (is.na(xmin)) xmin <- min(values,na.rm=TRUE)
  if (is.na(xmax)) xmax <- max(values,na.rm=TRUE)
  if (prop) {
    outplot <- propor
  } else {
    outplot <- counts
  }
  if (is.na(ymax)) {
    if (nchar(main) > 0) {
      ymax <- max(outplot,na.rm=TRUE) * 1.15
    } else {
      ymax <- max(outplot,na.rm=TRUE) * 1.05
    }
  }
  if (plotout) {
    plot(values,outplot,type="n",
         xlim=c((xmin-(width*0.75)),(xmax+(width*0.75))),
         xaxs="r",ylim=c(0,ymax),yaxs="i",xlab="",ylab="",xaxt="n",...)
    if (xaxis) axis(side=1,at=seq(xmin,xmax,inc),labels=seq(xmin,xmax,inc))
    if (length(counts) > 0) {
      for (i in 1:nct) {  # i <- 1
        x1 <- values[i] - (width/2)
        x2 <- values[i] + (width/2)
        x <- c(x1,x1,x2,x2,x1)
        y <- c(0,outplot[i],outplot[i],0,0)
        if (is.null(border)) border <- col
        polygon(x,y,col=col,border=border,lwd=lwd)
      }
      title(ylab=list(ylab, cex=1.0, font=7),
            xlab=list(xlab, cex=1.0, font=7))
      if (nchar(main) > 0) mtext(main,side=3,line=-1.0,outer=FALSE,cex=0.9)
    }
  } # end of if-plotout
  if (length(counts) > 0) {
    answer <- cbind(values,counts,propor);
    rownames(answer) <- values
    colnames(answer) <- c("values","counts","propcounts")
  } else { answer <- NA  }
  class(answer) <- c("matrix","inthist")
  return(invisible(answer))
}  # end of inthist

#' @title linept adds a line and a series of points to a plot
#' 
#' @description linept adds both a line and a series of points to a plot but
#'     without the gaps introduced in the line when using type='b' within the
#'     base R lines function. This is simply a format issue as I do not like 
#'     those gaps
#'
#' @param x the x series of points
#' @param y the corresponding y series of points
#' @param lwd the line width, default=1
#' @param pch the character used, default = 16 (a large dot)
#' @param ... and other graphics arguments typically used with either lines or
#'     points
#'
#' @return nothing but it does add a pointed line to a plot
#' @export
#'
#' @examples
#' print("wait on example data")
linept <- function(x,y,lwd=1,pch=16,...) {
  lines(x,y,lwd=lwd,...)
  points(x,y,pch=pch,...)
}

#' @title makecanvas sets up a plotting area ready for the flowchart
#'
#' @description makecanvas sets up a plotting areas ready for a flowchart
#'     made up of shapes, circles, polygons, rectangles, text, and arrows
#'
#' @param xstart x-origin value defaults = 0
#' @param xfinish maximum of x axis defaults = 100
#' @param ystart y-origin value default = 0
#' @param yfinish y-axis maximum default = 100
#' @param addbox should a box be drawn around the canvas, default = FALSE
#'
#' @return plots an empty graph ready for polygons and text, returns a 
#'     list defining the objects on the canvas
#' @export
#'
#' @examples
#' \dontrun{
#'   canvas=makecanvas(ystart=50,yfinish=93.5)
#'   polygon(makevx(2,27),makevy(90,6),col=0,lwd=1,border=1)
#' }
makecanvas <- function(xstart=0,xfinish=100,ystart=0,yfinish=100,addbox=FALSE) {
  width <- length(xstart:xfinish)
  height <- length(ystart:yfinish)
  par(mfrow=c(1,1),mai=c(0.1,0.1,0.1,0.1),oma=c(0.0,0,0.0,0.0))
  par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
  plot(seq(xstart,xfinish,length=101),seq(ystart,yfinish,length=101),
       type="n",xaxt="n",yaxt="n",xlab="",ylab="", bty="n")
  if (addbox) makerect(left=xstart,right=xfinish-xstart,
                       bottom=yfinish - (yfinish-ystart),top=yfinish)
  canvas <- list(xyaxes=c(width,height),objects=0,form=NULL,
                 coords=list(),njoin=0,join=list())
  return(invisible(canvas))
} # end of makecanvas

#' @title makevx make an x values vector for plotting oblongs and triangles
#'
#' @description makevx takes either a vector of 3 values or two single values.
#'     If it receives 2 single values then the left x value of a rectangle and 
#'     the right x value defines a vector describing the four vertices of the 
#'     rectangle bottomleft, bottomright, topright,topleftleft, bottomleft. 
#'     When matched with makevy generates the descriptor for a complete 
#'     rectangle or oblong is returned. If it receives a single vector of 3
#'     values it returns that vector with a repeat of the first value appended
#'     so that it is ready to plot a triangle if paired with makevy.
#'
#' @param x1 x-value for the left-hand edge of a rectangle, or a vector of 3
#'     if defining a triangle
#' @param x2 x value for the the right-hand edge, or defaults to NULL if 
#'     defining a triangle
#'
#' @return a vector of x-values
#' @export
#'
#' @examples
#' \dontrun{
#'  plot(0:100,seq(0,100),type="n",xaxt="n",yaxt="n",
#'  xlab="",ylab="", bty="n")
#'  lines(makevx(20,80),makevy(20,80),col=1,lwd=2)
#' }
makevx <- function(x1,x2=NULL) {
  if (length(x1) == 1) {
    return(c(x1,x2,x2,x1,x1))
  }
  if (length(x1) == 3) {
    return(c(x1,x1[1]))
  }
} # end of makevx

#' @title makevy make a y values vector for either an oblong or a triangle
#'
#' @description makevx takes either a vector of 3 values or two single values.
#'     If it receives 2 single values then the left x value of a rectangle and 
#'     the right x value defines a vector describing the four vertices of the 
#'     rectangle bottomleft, bottomright, topright, topleft, bottomleft. When 
#'     matched with makevx generates the descriptor for a complete rectangle or
#'     oblong is returned. If it receives a single vector of 3 values it returns 
#'     that vector with a repeat of the first value appended so that it is ready 
#'     to plot a triangle if paired with makevx.
#'
#' @param y1 y-value for the bottom edge of a rectangle, or a vector of 3 x values
#'     for vertices if defining a triangle.
#' @param y2 y-value for the top edge of an oblong, or defaults to NULL if 
#'     defining a triangle
#'
#' @return a vector of y-values
#' @export
#'
#' @examples
#' \dontrun{
#'  canvas(ystart=50,yfinish=93.5)
#'  polygon(makevx(2,27),makevy(90,6),col=0,lwd=1,border=1)
#' }
makevy <- function(y1,y2) {
  if (length(y1) == 1) {
    return(c(y1,y1,y2,y2,y1))
  }
  if (length(y1) == 3) {
    return(c(y1,y1[1]))
  }
} # end of makevy

#' @title makepolygon simplifies the creation of a polygon from xy data
#' 
#' @description makepolygon simplifies the generation of the outline data for
#'     plotting a polygon when one has a pair of lines on a graph that one wants
#'     to use as the bounds of a polygon. For example, given time-series of 
#'     biomass trajectories across years, one may wish to impose, say, the 90th
#'     quantiles to illustrate how the trends and variation changes through time.
#'     One can use single coloured lines o depict such bounds or, alternatively,
#'     one can use a polygon filled with an rgb transparent colour to more 
#'     clearly show the outcome. This would be especially useful when trying to 
#'     compare different sets of time-series. The overlap and differences would
#'     become more clear visually.
#'
#' @param y1 the y-axis values for one of the time-series
#' @param y2 the y-axis values for the other series
#' @param x1 the x-axis values (often years) relating to the y1 values
#' @param x2 the x-axis values (often years) relating to the y2 values. It is
#'     expected that often x2 will be identical to x1 and so the default value
#'     for x2 = NULL, in which case it is set - x1 inside the function
#'
#' @return a two column matrix that can act as the input to the polygon function
#' @export
#'
#' @examples
#' yrs <- 2000:2020
#' nyrs <- length(yrs)
#' reps <- 100
#' av <- 5:(nyrs+4)
#' dat <- matrix(0,nrow=reps,ncol=nyrs,dimnames=list(1:reps,yrs))
#' for (i in 1:nyrs) dat[,i] <- rnorm(reps,mean=av[i],sd=2)
#' qs <- apply(dat,2,quantile)
#' poldat <- makepolygon(y1=qs[2,],y2=qs[4,],x1=yrs)
#' plot(yrs,dat[1,],type="p",pch=16,cex=0.2,col=1,ylim=c(0,30),
#' panel.first=grid(),xlab="years",ylab="data")
#' for (i in 1:reps) points(yrs,dat[i,],pch=16,col=1,cex=0.2)
#' polygon(poldat,border=NULL,col=rgb(1,0,0,0.1))
makepolygon <- function(y1,y2,x1,x2=NULL) {
  if ((is.null(x2))) x2 <- x1
  if ((length(x1) != length(y1)) | (length(x2) != length(y2))) 
    stop("The number of x-axis values MUST equal the number of y-axis values in makepolygon \n")
  seqord <- 1:length(x2)
  rx2 <- x2[order(seqord,decreasing=TRUE)]
  seqord <- 1:length(y2)
  ry2 <- y2[order(seqord,decreasing=TRUE)]  
  x <- c(x1,rx2)
  y <- c(y1,ry2)
  return(cbind(x,y))
} # end of makepolygon

#' @title makerect draws a rectangle once a plot is available
#'
#' @description makerect draws a rectangle after canvas has been called
#'
#' @param left defines lefthand edge of rectangle
#' @param right defines right-hand edge of rect
#' @param bottom defines bottom edge of rectangle angle
#' @param top defines top edge of rectangle
#' @param linecol colour of line. default="grey"
#' @param lwd the width of the line, default=1
#' @param console should the centerx and centery bt sent ot console, 
#'     default=FALSE
#'
#' @return a vector denoting the center (x,y) of the rectangle
#' @export
#'
#' @examples
#' \dontrun{
#'    canvas(ystart=50,yfinish=93.5)
#'    makerect(left=2,xinc=27,top=90,yinc=6)
#' }
makerect <- function(left,right,bottom,top,linecol="grey",lwd=1,console=FALSE) {
  polygon(makevx(left,right),makevy(top,bottom),col=NULL,
          lwd=lwd,border=linecol)
  centerx <- (left + (right-left)/2)
  centery <- (top - (top- bottom)/2)
  if (console) return(c(centerx,centery))
}

#' @title pickbound selects an optimum number of rows and cols for a plot
#' 
#' @description pickbound enables the automatic selection of a pre-determined
#'     optimum combination of plot rows and columns to suit a number of plots
#'     up to 36. So, given a number of plots from 1 to 36 this returns a numeric 
#'     dimer containing the number of rows and columns needed for par statement.
#'     If pickbound is set to a value > 36 a warning will be given and it will
#'     revert to 36, which I assume will ruin you plot.
#'
#' @param n the number of plots to be included in a combined plot
#'
#' @return a vector of two with the number of rows and columns for a plot
#' @export
#'
#' @examples
#' pickbound(5)
#' pickbound(8)
#' pickbound(33)
pickbound <- function(n) {
  bounds <- matrix(c(1,1,1,2,2,1,3,2,2,4,2,2,5,3,2,6,3,2,7,4,2,8,4,2,9,3,3,10,3,4,
                     11,3,4,12,3,4,13,5,3,14,5,3,15,5,3,16,4,4,17,5,4,18,5,4,19,5,4,
                     20,5,4,21,5,5,22,5,5,23,5,5,24,5,5,25,5,5,26,5,6,27,5,6,28,5,6,
                     29,5,6,30,5,6,31,8,4,32,8,4,33,6,6,34,6,6,35,7,5,36,6,6),
                   nrow=36,ncol=3,byrow=TRUE,
                   dimnames=list(1:36,c("plots","rows","cols")))
  if (n > 36) {
    tmp <- ceiling(sqrt(n))
    out <- c(tmp,tmp)
    if ((tmp * (tmp-1)) >= n) out <- c(tmp,(tmp-1)) 
  } else {
    out <- c(bounds[n,2],bounds[n,3])
  }
  return(out)
} # end of pickbound


#' @title newplot simple floating window setup a plot
#'
#' @description newplot is a bare-bones setup routine to generate a plot in
#'     RStudio using a floating window. If you want to alter the default par
#'     settings then you can use either setplot() to get suitable syntax or,
#'     more simply, use parsyn() which only gives a template for the par 
#'     syntax. DEPRECATED, USE plotprep() and parset() INSTEAD.
#' @param width defaults to 6 inches = 15.24cm - width of plot
#' @param height defaults to 3.6 inches = 9.144cm - height of plot
#' @param newdev reuse a previously defined graphics device or make a new 
#'     one, defaults to TRUE
#' @return Checks for and sets up a graphics device and sets the default 
#'     plotting par values. This changes the current plotting options!
#' @export
#' @examples
#' \dontrun{
#'  x <- rnorm(1000,mean=0,sd=1.0)
#'  plotprep()
#'  hist(x,breaks=30,main="",col=2)
#' }
newplot <- function(width=6,height=3.6,newdev=TRUE) {
  if  ((names(dev.cur()) != "null device") & (newdev)) 
    suppressWarnings(dev.off())
  if (names(dev.cur()) %in% c("null device","RStudioGD"))
    dev.new(width=width,height=height,noRStudioGD = TRUE)
  par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0))
  par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
} # end of new_plot


#' @title panel.hist a function to modify the pairs function
#' 
#' @description panel.hist is a function lifted directly from the help
#'     for the 'pairs' function. It enables histograms of each variable
#'     to be included in the diagonal plots insude the pairs plot. It is
#'     only included here as a convenience.
#'
#' @param x a data.frame or matrix of variables that will be plotted
#'     against each other in a pairs plot
#' @param ... the other parameters for the function
#'
#' @return nothing but it does add histograms to the diagonals
#' @export
#'
#' @examples
#' print("wait on data sets")
panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "pale green", ...)
} # end of panel.hist

#' @title parset alters the current base graphics par settings
#'
#' @description parset alters the current base graphics par settings
#'     to suit a single standard plot. It is merely here to simplify
#'     and speed the coding for exploratory base graphics. The font
#'     and its size default to 0.85 and font 7 (Times bold). The
#'     default values can be seen by typing parset with no brackets in
#'     the console. If a different
#'     set of par values are needed then the function parsyn() can be
#'     used to act as a prompt for the correct syntax. The output to
#'     the console can be copied to your script and modified to suit.
#'
#' @param plots vector of number of rows and columns, defaults to c(1,1)
#' @param cex the size of the font used, defaults to 0.85
#' @param font the font used, defaults to 7 which is Times Bold, 6 is
#'     Times, 1 is Sans and 2 is Sans Bold.
#' @param outmargin default=c(0,0,0,0) and defines the outer margin used by
#'     mtext
#' @param margin default=c(0.45,0.45,0.05,0.05), which avoids whitespace 
#'     but leaves plenty of room for titles
#' @param byrow should plots be made by row (mfrow; byrow=TRUE, the default),
#'     of by column (mfcol; byrow=FALSE)
#' @param ... the generic ellipsis allowing for the includion of other graphics
#'     arguments such as xaxs="n", etc.
#'
#' @return nothing but it changes the base graphics par settings
#' @export
#'
#' @examples
#' \dontrun{
#' parset()
#' parsyn()
#' }
parset <- function(plots=c(1,1),cex=0.75,font=7,outmargin=c(0,0,0,0),
                   margin=c(0.45,0.45,0.05,0.05),byrow=TRUE,...) {
  if (byrow) {
    par(mfrow=plots,mai=margin,oma=outmargin)
  } else {
    par(mfcol=plots,mai=margin,oma=outmargin)
  }
  par(cex=cex, mgp=c(1.35,0.35,0), font.axis=font,font=font,
      font.lab=font,...)
} # end of parset

#' @title parsyn types standard syntax for the par command to the console
#'
#' @description parsyn types the standard syntax for the par command to the
#'     console so it can be copied and pasted into your own code.
#'
#' @return it writes two lines of R code to the console
#' @export
#'
#' @examples
#' \dontrun{
#' parsyn()
#' }
parsyn <- function() {
  cat("par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0)) \n")
  cat("par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)  \n")
}

#' @title plot1 a simple way to plot an xy line plot
#'
#' @description plot1 provides a quick way to plot out a single xy
#'     line plot. It can be used with plotprep to generate a plot
#'     outside of Rstudio or by itself to generate one within Rstudio.
#'     It uses a standard par setup and permits custom labels, font,
#'     and font size (cex). It checks the spread of y and if a ymax is
#'     not given in the parameters finds the ymax and checks to see if
#'     y goes negative in which case it uses getmin, so the
#'     y-axis is set to 0 - ymax or ymin - ymax
#'
#' @param x The single vector of x data
#' @param y the single vector of y data. If more are required they can
#'     be added spearately after calling plot1.
#' @param xlab the label fot the x-axis, defaults to empty
#' @param ylab the label fot the y-axis, defaults to empty
#' @param type the type of plot "l" is for line, the default, "p" is
#'     points. If you want both plot a line and add points afterwards.
#' @param usefont which font to use, defaults to 7 which is Times bold
#' @param cex the size of the fonts used. defaults to 0.85
#' @param maxy defaults to 0, if a value is given then that value is used rather 
#'     than estimating from the input y using getmax
#' @param defpar if TRUE then plot1 will declare a par statement. If false 
#'     it will expect one outside the function. In this way plot1 can be
#'     used when plotting multiple graphs, perhaps as mfrow=c(2,2)
#' @param ... room for other graphics commands like col, pch, and lwd
#'
#' @return nothing but it does plot a graph and changes the par setting
#' @export
#'
#' @examples
#' x <- rnorm(20,mean=5,sd=1)
#' plot1(x,x,xlab="x-values",ylab="yvalues")
plot1 <- function(x,y,xlab="",ylab="",type="l",usefont=7,cex=0.75,
                  maxy=0,defpar=TRUE,...){
  if (defpar) {
    par(mfrow = c(1,1), mai = c(0.45,0.45,0.1,0.05),oma = c(0,0,0,0))
    par(cex = cex, mgp = c(1.35, 0.35, 0), font.axis = usefont,
        font = usefont, font.lab = usefont)
  }
  if (maxy > 0) ymax <- maxy  else ymax <- getmax(y)
  if (min(y,na.rm=TRUE) < 0.0) ymin <- getmin(y) else ymin <- 0.0
  plot(x,y,type=type,ylim=c(ymin,ymax),yaxs="i",
       ylab=ylab,xlab=xlab,cex=cex,panel.first=grid(),...)
} # end of plot1

#' @title plotcompdata makes a series of barplots to compare composition data 
#' 
#' @description plotcompdata generates a horizontal array of up to 20 barplots
#'     and is used to represent time-series of age- or size-composition data in
#'     a manner that makes interannual comparisons simple. Currently designed 
#'     for use with makehtml 
#'
#' @param compdata matrix of sizes or ages by years, row and column names should
#'    be numbers, as in ages or sizeclasses for rows, and years for columns.
#' @param sau name of spatial assessment unit or origin of samples
#' @param ylabel what outer name to be used for the Y-axis, default=''
#' @param console should the graph be plotted to the console or saved as a file.
#'     default=TRUE ie it goes to the console, if set to FALSE it goes to 
#'     paste0(rundir,/) 
#' @param outdir if saving a png file this is the directory to which a filename
#'     is added, default=""
#' @param barcol what colour should the bars be, default = red 
#' @param bordercol what colour should the bar borders be? default = black
#' @param horizline indices of the sizes or ages against which to draw reference
#'     lines. default=NULL. To draw 1 line then eg line = 5, if 2 eg = c(5, 13)
#'
#' @return invisibly returns a list of the filename and caption
#' @export
#'
#' @examples
#' \dontrun{
#'   x <- matrix(rnorm(600,mean=20,sd=3),nrow=20,ncol=30,
#'               dimnames=list(1:20,1991:2020))
#'   ylabel="Random Composition"
#'   numcol <- ncol(x)
#'   if (numcol > 20) x1 <- x[,1:20]
#'   plotcompdata(compdata=x1,sau="sauX",ylabel="Random Composition Data",
#'                console=TRUE,horizline=c(5,10))
#' }              
plotcompdata <- function(compdata,sau,ylabel="",console=TRUE,outdir="",
                         barcol="red",bordercol="black",horizline=NULL) {
  compcl <- as.numeric(rownames(compdata))  # expects size or age classes
  label <- as.numeric(colnames(compdata))   # expects years
  addyrs <- paste0(label[1],"_",label[length(label)])
  Nsamp <- ncol(compdata)
  sampsize <- round(colSums(compdata),1)  
  filen <- ""
  if (!console) {
    filen <- paste0(outdir,"/horizontal_compdata_for_",sau,"_",addyrs,".png")
  }
  caption <- paste0("Observed size-composition data for ",sau)
  if (length(horizline) == 1) linecol <- "blue"
  if (length(horizline) == 2) linecol <- c("green","blue")
  plotprep(width=12,height=4,newdev=TRUE,filename=filen,cex=0.9,
           verbose=FALSE)
  parset(outmargin=c(1,2,1,0.2),margin=c(0.2,0.2,0,0))
  matfor <- matrix(c(1:20),1,20,byrow=TRUE)
  layout(matfor,heights=rep(1,20),TRUE)
  barplot(compdata[,1],horiz=TRUE,axes=FALSE,col=barcol,border=bordercol,
          space=0,axis.lty=1.0,cex.names=1.0)
  mtext(label[1],side=1,outer=FALSE,cex=1,line=-0.75)
  mtext(sampsize[1],side=3,outer=FALSE,line=-1,cex=1)
  if (length(horizline) > 0) abline(h=horizline,lwd=3,col=linecol)
  for (i in 2:Nsamp) {
    barplot(compdata[,i],horiz=TRUE,axes=FALSE,axisnames=FALSE,col=barcol,
            border=bordercol,space=0)
    mtext(label[i],side=1,outer=FALSE,line=-0.75,cex=1)
    mtext(sampsize[i],side=3,outer=FALSE,line=-1,cex=1)
    if (length(horizline) > 0) abline(h=horizline,lwd=3,col=linecol)
  }
  mtext(text=ylabel,side=2,outer=TRUE,cex=1.1,line=0.2)
  if (!console) dev.off()
  return(invisible(list(filename=filen,caption=caption)))
} # end of plotcompdata

#' @title plotnull generates an empty plot when one is needed
#'
#' @description plotnull, there are often circumstances, for example, when
#'     plotting up results from each year and each SAU, where there will be
#'     combinations of year and SAU that have no data, but to avoid a problem
#'     with the plotting it is necessary to generate an empty plot. By using
#'     x=0:100 and y= 0:100 this is equivalent to a canvas of 0:100,0:100
#'
#' @param msg a message to be printed in the middle of the empty plot.
#'
#' @return nothing but it does generate a plot
#' @export
#'
#' @examples
#' plotnull("An empty plot")
plotnull <- function(msg="") {
  plot(0:100,0:100,type="n",xaxt="n",yaxt="n",xlab="",ylab="",frame.plot=FALSE)
  if (nchar(msg) > 0)
    text(x=5,y=5,msg,cex=1.0,font=7)
} # end of plotnull

#' @title plotoblong generates an oblong from x0,x1,y0,y1
#' 
#' @description plotoblong generates an oblong from x0,x1,y0,y1
#'
#' @param x0 x-axis left
#' @param x1 x-axis right
#' @param y0 yaxis bottom
#' @param y1 yaxis top
#' @param border colour of the border, default=black=1
#' @param col colour of fill, default = 0 =  empty
#' @param lwd width of the line,default=1
#'
#' @return nothing but it plots a polygon
#' @export
#'
#' @examples
#' \dontrun{
#'   canvas()
#'   plotoblong(1,50,1,50,lwd=3,linecol=4)
#' }
plotoblong <- function(x0,x1,y0,y1,border=1,col=0,lwd=1) {
  x <- c(x0,x0,x1,x1,x0); y <- c(y0,y1,y1,y0,y0)
  polygon(x,y,lwd=lwd,border=border,col=col)
}

#' @title plotprep sets up a window and the par values for a single plot
#'
#' @description plotprep sets up a window and the par values for a single 
#'     plot. It checks to see if a graphics device is open and opens a new 
#'     one if not. This is simply a utility function to save typing the 
#'     standard syntax. Some of the defaults can be changed. Typing the name
#'     without the brackets will provide a template for modification. If 
#'     'windows' is called repeatedly this will generate a new active graphics 
#'     device each time leaving the older ones inactive but present. For quick 
#'     exploratory plots this behaviour is not wanted, hence the check if 
#'     an active device exists already or not.
#'
#' @param width defaults to 6 inches = 15.24cm - width of plot
#' @param height defaults to 3 inches = 7.62cm - height of plot
#' @param usefont default is 7 (bold Times) 1 sans serif, 2 sans serif bold
#' @param cex default is 0.85, the font size font used for text in the plots
#' @param newdev reuse a previously defined graphics device or make new one;
#'     defaults to FALSE
#' @param filename defaults to "" = do not save to a filename. If a
#'     filename is input the last three characters will be checked and if
#'     they are not png then .png will be added.
#' @param resol resolution of the png file, if defined, default=300
#' @param verbose set this to FALSE to turn off the reminder to include 
#'     a graphics.off() command after the plot. Default=TRUE
#' 
#' @return Checks for and sets up a graphics device and sets the default 
#'     plotting par values. This changes the current plotting options!
#' @export
#' 
#' @examples
#'  x <- rnorm(1000,mean=0,sd=1.0)
#'  plotprep()
#'  hist(x,breaks=30,main="",col=2)
plotprep <- function(width=6,height=3.6,usefont=7,cex=0.85,
                     newdev=FALSE,filename="",resol=300,verbose=TRUE) {
  if  ((names(dev.cur()) != "null device") &
       (newdev)) suppressWarnings(dev.off())
  lenfile <- nchar(filename)
  if (lenfile > 3) {
    end <- substr(filename,(lenfile-3),lenfile)
    if (end != ".png") filename <- paste0(filename,".png")
    png(filename=filename,width=width,height=height,units="in",res=resol)
  } else {
    if (names(dev.cur()) %in% c("null device","RStudioGD"))
      dev.new(width=width,height=height,noRStudioGD = TRUE)
  }
  oldpar <- par(no.readonly=TRUE)
  par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0.0,0.0,0.0))
  par(cex=cex, mgp=c(1.35,0.35,0), font.axis=usefont,font=usefont,
      font.lab=usefont)
  if ((lenfile > 0) & (verbose))
    cat("\n Remember to place 'dev.off()' after plot \n")
  return(invisible(oldpar))
} # end of plotprep

#' @title plotxyy plots two vectors of numbers against single x-axis
#' 
#' @description plotxyy plots two plots on the single graph so that
#'     they share the x-axis. The first series is plotted on the left
#'     vertical axis and the second on the right-hand axis.
#'
#' @param x1 the x values for the first variable. Always use the variable that 
#'     has the widest x-axis values as the first variable to ensure all values
#'     are plotted.
#' @param x2 the x values for the second variable. 
#' @param y1 the left-hand axis values
#' @param y2 the right-hand axis values
#' @param xlab the x label, default=""
#' @param ylab1 the left-hand y label, default=""
#' @param ylab2 the right-hand y label, default=""
#' @param cex the size of font on the axes, default=0.85
#' @param fnt the font used on axes, default=7 (bold times)
#' @param colour a vector of two values for the colour of each line,
#'     default=c(1,2)  black and red
#' @param defpar should the internal 'par' statement be used = defpar=TRUE, or
#'     the default=FALSE, which means the plot 'par' will be defined outside the
#'     plot.
#'
#' @return nothing but it plots a graph
#' @export
#'
#' @examples
#' x <- 1:20
#' yval1 <- rnorm(20,mean=5,sd=1)
#' yval2 <- rnorm(20,mean=10,sd=1)
#' plotxyy(x1=x,y1=yval1,y2=yval2,ylab1="mean=5",ylab2="mean=10")
plotxyy <- function(x1,x2=x1,y1,y2,xlab="",ylab1="",ylab2="",cex=0.85,fnt=7,
                    colour=c(1,2),defpar=FALSE) {
  if (defpar) {
    par(mfrow=c(1,1),mai=c(0.5,0.45,0.15,0.05),oma=c(0.0,0.75,0.0,3.0)) 
    par(cex=cex, mgp=c(1.35,0.35,0), font.axis=fnt,font=fnt,font.lab=fnt) 
  }
  maxy <- getmax(y1)
  plot(x1,y1,type="l",lwd=2,col=colour[1],ylim=c(0,maxy),yaxs="i",
       ylab="",xlab="")
  mtext(ylab1, side=2, line=1.5)
  mtext(xlab, side=1, line=1.25)
  par(new=TRUE)
  maxy2 <- getmax(y2)
  plot(x2,y2,type="l",lwd=2,col=colour[2],ylim=c(0,maxy2),axes=FALSE,
       xlab="",ylab="",yaxs="i")
  mtext(ylab2, side=4, line=1.5)
  axis(4)
  grid(ny=0)
} # end of plotxyy

#' @title pltregcol colours the plot region of a base R plot using panel.first
#' 
#' @description pltregcol is used to fill in the plot area with a neutral grey 
#'     colour to clarify the use of some of the fainter colours, especially
#'     when using faint transparent colours. The solution came from a
#'     stackoverflow discussion. It is used as the argument for the panel.first
#'     function.
#'
#' @param col what grey colour to use to infill, default = 'grey90'
#' @param gridcol what colour to use for the grid added to the plot, default=
#'     'darkgrey'
#'
#' @return nothing but it does infill the plot region of a plot
#' @export
#'
#' @examples
#' plot(x=2,y=2,panel.first=pltregcol())
pltregcol <- function(col="grey90",gridcol="darkgrey") {
  points(0,0,pch=16,cex=1e6,col=col)
  grid(col=gridcol,lty="dotted")
} # end of pltregcol

#' @title pol2cart polar to cartesian coordinates
#' 
#' @description pol2cart translate polar coordinates of angles (as degrees)
#'     and a distance = radius, into cartesian coordinates of x and y. The
#'     option of using arbitrary origin coordinates is included
#'
#' @param angle the angle in degrees, either a single number of a vector
#' @param dist the length of the line or radius, a single number
#' @param xorig the final xorigin
#' @param yorig the final yorigin
#'
#' @return a matrix of 1 or more rows depending on length of angle
#' @export
#'
#' @examples
#' \dontrun{
#'   ans <- pol2cart(angle=seq(0,360,15),dist=20,xorig=30,yorig=30)
#'   print(ans)
#' }
pol2cart <- function(angle,dist,xorig=0,yorig=0){
  #  angle=45:50; dist=10; xorig=0; yorig=0
  numang <- length(angle)
  coord <- matrix(0,nrow=numang,ncol=2,dimnames=list(1:numang,c("x","y")))
  angler <- angle*pi/180
  for (i in 1:numang) {
    coord[i,] <- c(xorig + dist * sin(angler[i]),
                   yorig + dist * cos(angler[i]))  
  }
  return(coord) #output the new x and y coordinates
} # end of pol2cart

#' @title putcircle draws a circle with a given origin and radius
#' 
#' @description putcircle provides the means of drawing a circle of a given
#'     radius and origin within a diagram ready for the addition of text.
#'
#' @param origx the final x origin
#' @param origy the final y origin
#' @param radius the radius of the circle
#' @param col the col of the circle
#' @param lwd the line width of the circle
#' @param fill should the circle be filled with colour? default=NA = empty
#' @param ... to allow for other graphics commands such as lty=2, etc
#'
#' @return the matrix of x and y values invisibly  
#' @export
#'
#' @examples
#'   makecanvas()
#'   putcircle(origx=35,origy=70,radius=30,lwd=2,col=1)
#'   putcircle(origx=65,origy=60,radius=30,lwd=2,col=2)
#'   putcircle(origx=45,origy=40,radius=30,lwd=2,col=4)
putcircle <- function(origx=50,origy=50,radius=10,col=1,lwd=1,fill=NA,...) {
  xy <- pol2cart(angle=seq(0,360,0.1),dist=radius,xorig=origx,yorig=origy)
  polygon(xy[,"x"],xy[,"y"],border=col,col=fill,lwd=lwd,...)
  #  lines(ans[,"x"],ans[,"y"],lwd=lwd,col=col)
  return(invisible(xy))
} # end of circle



#' @title putoblong draws a rectangle once a canvas is available
#'
#' @description putoblong draws a rectangle after makecanvas has been called
#'
#' @param x1 defines lefthand edge of rectangle
#' @param x2 defines right-hand edge or rectangle
#' @param y1 defines bottom edge of rectangle
#' @param y2 defines top edge of rectangle
#' @param col colour of line. default="black"
#' @param fill colour with which to fill the oblong, default=NA = empty
#' @param lwd the width of the line, default=1
#' @param ... to allow for other graphics commands such as lty=2, etc
#'
#' @return an invisible matrix of x,y series defining the polygon
#' @export
#'
#' @examples
#' \dontrun{
#'    makecanvas(xstart=0,xfinish=100,ystart=0,yfinish=100)
#'    putoblong(x1=20,x2=80,y1=25,y2=75,col="red",lwd=2)
#' }
putoblong <- function(x1,x2,y1,y2,col="black",fill=NA,lwd=1,...) {
  xs <- makevx(x1,x2)
  ys <- makevy(y1,y2)
  polygon(x=xs,y=ys,border=col,lwd=lwd,col=fill,...)
  centerx <- (x2 - x1)/2
  centery <- (y2 - y1)/2
  return(invisible(cbind(xs,ys)))
} # end of putoblong

#' @title puttriangle draws a triangle once a canvas is available
#' 
#' @description puttriangle draws a triangle once a canvas is available. It
#'     requires two0 vectors of three values defining the three vertices of the 
#'     triangle.
#'
#' @param xs a vector of 3 values defining the x-axis values for the vertices
#' @param ys a vector of 3 values defining the y-axis values for the vertices
#' @param col the colour of theborder of the triangle
#' @param fill the col of the fill of the triangle, default=NA = no fill
#' @param lwd the line width if the border is coloured
#' @param ... to allow for other graphics commands such as lty=2, etc
#'
#' @return an invisible matrix of x,y series defining the polygon
#' @export
#'
#' @examples
#' makecanvas()
#' puttriangle(xs=c(40,60,80),ys=c(40,60,40),col=NA,fill=RGB("red",alpha=127))
puttriangle <- function(xs,ys,col="black",fill=NA,lwd=1,...) {
  xv <- makevx(xs)
  yv <- makevy(ys)
  polygon(xv,yv,border=col,col=fill,lwd=lwd,...)
  return(xy=cbind(xv,yv))
} # end of puttriangle

#' @title pythag calculates Pythagorus' theorum on a vector of two values
#' 
#' @description pythag Pythagorus' theorum states that the length of the
#'     hypotheneuse between two lines at right angels to each other (that
#'     is in cartesian coordinates) is the sqrt of the sum of their squares.
#'
#' @param x a vector of two numbers or a matrix of pairs of numbers
#'
#' @return a single number or a vector depending on input
#' @export
#'
#' @examples
#' \dontrun{
#'  pythag(c(3,4))  # should be 5
#'  dat <- matrix(c(3,4,5,7),nrow=2,ncol=2,byrow=TRUE)
#'  print(dat)
#'  pythag(dat)     # should be 5 and 10
#' }
pythag <- function(x) {  # x = ans
  if (is.vector(x)) ans <- sqrt((x[1]^2 + x[2]^2))
  if (is.matrix(x)) ans <- sqrt((x[,1]^2 + x[,2]^2))
  return(ans) 
}

#' @title RGB a wrapper for col2rgb and rgb with maxColorValue=255
#'
#' @description RGB is a wrapper that simplifies the use of the rgb function
#'     used to generate transparent colours in plots. The basic palette of
#'     colours used can be altered with the palette() function, and see
#'     palette.pals() for a list of the palettes available by default. Each of
#'     those defines 8 colours and col is either a named colour or a number
#'     from 1 - 8. The outcome is a vector of three values the first being the
#'     value from 0-255 for red, then green, then blue. The intensity of the
#'     colour used is given by alpha, again 0-255. The use of 255 rather than 1
#'     for the maximum value is in lione with standard usage of rgb colours.
#'     This function is not vectorized so only a single number at a time can be
#'     selected.
#'
#' @param col a single value as either a number from 1-8 or a named colour, for
#'     example 'pink'
#' @param alpha the intensity of colour as an integer from 0-255, where 255 is
#'     a solid colour
#'
#' @return a vector of length 3 containing integer values from 0-255 in order
#'     of red, green and blue as an rgb code
#' @export
#'
#' @examples
#' RGB("pink",alpha=255)
#' RGB(2,alpha=127)
RGB <- function(col,alpha=127) {
  vals <- col2rgb(col=col)
  return(rgb(vals[1],vals[2],vals[3],alpha=alpha,maxColorValue = 255))
} # end of RGB

#' @title saucompdata plots horizontal barcharts of size-composition data
#' 
#' @description saucompdata generates horizontal plots of all observed size-
#'     composition data. If there are more than 20 years of data then it will 
#'     generate two pots of up to 20 years each, separating the plots using
#'     the plotnumber argument in the file name. This function uses plotcompdata
#'
#' @param allcomp A 3-D array of observed data on size-classes x years x sau 
#' @param glb the globals objects. specifically glb$nSAU and glb$saunames are
#'     required as a list.
#' @param horizline indices of the sizes or ages agsint which to draw reference
#'     lines. default=NULL. IF draw 1 then eg line = 5, if 2 eg = c(5, 13)
#' @param console should the graph be plotted to the console or saved as a file.
#'     default=TRUE ie it goes to the console, if set to FALSE it goes to 
#'     paste0(rundir,/) 
#' @param rundir if saving a png file this is the directory to which a filename
#'     is added
#' @param barcol what colour should the bars be, default = red 
#' @param bordercol what colour should the bar borders be? default = black
#' @param ylabel what outer name to be used for the Y-axis, default=''
#' @param tabname what web tab name to use. Only useful when using
#'
#' @return nothing but it does generate plots for ech sau
#' @export
#'
#' @examples
#' print("wait on data")
saucompdata <- function(allcomp, glb, horizline = NULL,console = TRUE,
                        rundir = "",barcol = "red",bordercol = "black",
                        ylabel = "",tabname = "") {
  nsau <- glb$nSAU
  saunames <- glb$saunames
  for (sau in 1:nsau) {  # sau=4
    usecomp <- allcomp[,,sau]
    numcol <- ncol(usecomp)
    if (ncol(usecomp) <= 20) {
      ans <- plotcompdata(compdata=usecomp,sau=saunames[sau],ylabel=ylabel,
                   console=console,outdir=rundir,barcol=barcol,
                   bordercol=bordercol,horizline=horizline)
      addplot(ans$filename,rundir=rundir,category=tabname,ans$caption)
    } else {
      ans <- plotcompdata(compdata=usecomp[,(numcol-19):numcol],sau=saunames[sau],
                          ylabel=ylabel,console=console,outdir=rundir,
                          barcol=barcol,bordercol=bordercol,horizline=horizline)
      addplot(ans$filename,rundir=rundir,category=tabname,ans$caption)
      ans <- plotcompdata(compdata=usecomp[,1:(numcol-20)],sau=saunames[sau],
                          ylabel=ylabel,console=console,outdir=rundir,
                          barcol=barcol,bordercol=bordercol,horizline=horizline)
      addplot(ans$filename,rundir=rundir,category=tabname,ans$caption)
    }
  }
} # end of saucompdata

#' @title setplot provides an example plot with defaults for a standard plot
#'
#' @description Provides an example plot with defaults for a standard plot
#'   includes details of how to generate tiff, pdf, and png versions,
#'   mtext and legends. Currently no parameters, but the function
#'   is open to development for customization of the example plot.
#' @return prints lines of R that will define a standard plot and can be 
#'     copied into an R script.
#' @export setplot
#' @examples
#' setplot()
setplot <- function() {
  cat('#if (names(dev.cur()) %in% c("null device","RStudioGD")) \n')
  cat('#    dev.new(width=width,height=height,noRStudioGD = TRUE) \n')
  cat('#graphfile <- "name.tiff" OR "name.pdf" OR name.png  \n')
  cat('#if (file.exists(graphfile)) file.remove(graphfile)  \n')
  cat('#tiff(file=graphfile,width=150,height=150,units="mm",res=300,
      compression=c("lzw")) OR  \n')
  cat('#pdf(file=graphfile,onefile=T,width=8,height=6,family="Times") OR \n')
  cat('#png(filename=graphfile,width=150,height=100,units="mm",res=300,
      family="Times") \n')
  cat('\n')
  cat('par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0)) \n')
  cat('par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7) \n')
  cat('ymax <- max(y,na.rm=T) * 1.05 \n')
  cat('plot(x,y,type="l",xlab="",ylab="",col=1,ylim=c(0,ymax),yaxs="i",
      lwd=2) \n')
  cat('title(ylab=list("ylabel", cex=1.0, font=7),  \n')
  cat('      xlab=list("xlabel", cex=1.0, font=7)) \n')
  cat('\n')
  cat('legend(0,0.45,c("True Mean","Precise","Imprecise"),col=c(4,1,2),
      lwd=3,bty="n",cex=1.0) \n')
  cat('mtext("label",side=2,outer=T,line=0.0,font=7,cex=1.0) \n')
  cat('\n')
  cat('#dev.off() \n')
  cat('#graphics.off() \n')
} # end of set_plot


#' @title uphist a histogram with an upper limit on the x-axis
#' 
#' @description uphist is merely a wrapper around the base hist
#'     function, which adds the ability to limit the upper value on
#'     the x-axis. With fisheries data it is surprisingly common to 
#'     have data that has a very few extreme values that can obscure
#'     a standard plot of the data. The data are only truncated 
#'     within the uphist function so any other analyses will be on all 
#'     available data. If a maximum value is selected which 
#'     accidently eliminates all available data the script stops with
#'     an appropriate warning. If a value is selected which fails to 
#'     eliminate any data then all data are used.
#'
#' @param x the vector of values to be plotted as a histogram
#' @param maxval the maximum value to be retained in the plotted data
#' @param ... all the other arguments used by the base hist function
#'
#' @return nothing, but it does plot a histogram
#' @export
#'
#' @examples
#'   plotprep(width=7, height=9)
#'   parset(plots=c(3,1))
#'   x <- rlnorm(5000, meanlog=2, sdlog=1)
#'   hist(x,breaks=30,main="",xlab="log-normal values")
#'   uphist(x,breaks=30,main="",xlab="log-normal values",maxval=100)
#'   uphist(x,breaks=30,main="",xlab="log-normal values",maxval=1000)
uphist <- function(x,maxval=NA,...) {
  if (is.numeric(maxval)) {
    pick <- which(x > maxval)
    if (length(pick) > 0) x <- x[-pick]
  }
  if (length(x) > 0){
    hist(x,...)
  } else {
    stop("maxval in uphist too small and no data remaining. \n")
  }
} # end of uphist


#' @title xyplotyear generates n year xy plots from a data.frame
#' 
#' @description xyplotyear meets a common need that occurs when we have xy data
#'     from multiple years and want to plot them we can use xyplotyear. The 
#'     y-label for each plot is the year of data. The numeric label at the top 
#'     of each plot includes the number of observations, the gradient of the
#'     regression, if included, and the sum of the yvar for each year. The same
#'     y-axis scale is used for each plot.
#'
#' @param x the data.frame containing the data
#' @param yvar the character name of y-axis column in the data.frame x 
#' @param xvar the character name of x-axis column in the data.frame x 
#' @param year the name of the year variable, default="year"
#' @param plotnum a vector of rows and cols for the plots, default=c(1,1). This
#'     assumes that the columns are filled first using mfcol
#' @param xlim the range of the xvar to be plotted, default=c(0,12); If c(NA,NA)
#'     the xlim is set to the range of the input xvar
#' @param addline should a linear regression be fitted and added to each plot
#'     default=TRUE
#' @param origin should the regression pass through the origin, default=FALSE 
#' @param xlab the  generic label for the x-axis, default='', if left empty the 
#'     xvar name will be used
#' @param ylab the generic label for the y-axis, default='', if left empty the 
#'     yvar name will be used
#' @param maxy is available if you wish to vary the maximum y-axis value. The 
#'     default=NA, which means it will use getmax x 1.15 to find a maximum 
#' 
#'
#' @return currently nothing but it does plot a graph
#' @export
#'
#' @examples
#' print("wait on internal data")
xyplotyear <- function(x,yvar="",xvar="",year="year",plotnum=c(1,1),
                       xlim=c(0,12),addline=TRUE,origin=FALSE,
                       xlab="",ylab="",maxy=NA) {
  if (is.na(maxy)) {
    ymax <- getmax(x[,yvar],mult=1.15)
  } else {
    ymax <- maxy
  }
  parset(plots=plotnum,margin=c(0.2,0.35,0.05,0.05),outmargin=c(1.5,1.5,0,0), 
         cex=0.7,byrow=FALSE)
  yrs <- sort(unique(x[,year]))
  nyr <- length(yrs)
  if (is.na(xlim[1])) xlim <- range(x[,xvar])
  for (i in 1:nyr) {
    pickY <- which(x[,year] == yrs[i])
    N <- length(pickY)
    x2 <- droplevels(x[pickY,])
    plot(x2[,xvar],x2[,yvar],type="p",pch=1,xlab="",ylab=yrs[i],
         xlim=xlim,ylim=c(0,ymax))
    label <- paste0(N," ",round(sum(x2[,yvar],na.rm=TRUE)/1000,2))
    if (addline) {
      if (origin) {
        model <- lm(x2[,yvar] ~ x2[,xvar] - 1)  
        grad <- round(coef(model),2)
      } else {
        model <- lm(x2[,yvar] ~ x2[,xvar]) 
        grad <- round(coef(model)[2],2)
      }
      abline(model,lwd=2,col=2)
      label <- paste0(N," _ ",grad," _ ",
                      round(sum(x2[,yvar],na.rm=TRUE)/1000,2))
    }
    text(0,0.95*ymax,label,cex=0.8,pos=4)
  }
  if (nchar(xlab) == 0) xlab <- xvar
  if(nchar(ylab) == 0) ylab <- yvar
  mtext(xlab,side=1,line=0,cex=1.0,outer=TRUE)
  mtext(ylab,side=2,line=0,cex=1.0,outer=TRUE)
} # end of xyplotyear


#' @title yearBubble Generates a bubbleplot of x against Year.
#'
#' @description yearBubble Generates a bubbleplot of x against Year. If the 
#'     sum of x in year i == 0 then nothing is plotted but the graph is still
#'     generated.
#'
#' @param x a matrix of variable * Year; although it needn't be year
#' @param xlabel defaults to nothing but allows a custom x-axis label
#' @param ylabel defaults to nothing but allows a custom y-axis label
#' @param diam defaults to 0.1, is a scaling factor to adjust bubble size
#' @param vline defaults to NA but allows vertical ablines to higlight regions
#' @param txt defaults are lines to vessel numbers, catches, catches, maximumY
#' @param Fyear defaults to FALSE, if TRUE generates a fishing year x-axis
#' @param xaxis defaults to TRUE, allows for a custom x-axis if desired by
#'     using something like axis(1,at=years,labels=years).
#' @param yaxis defaults to TRUE, allows for a custom y-axis if desired by
#'     using something like axis(side=2,at=years,labels=years).
#' @param hline defaults to FALSE
#' @param nozero defaults to FALSE, if TRUE replaces all zeros with NA so they
#'     do not appear in the plot
#'
#' @return invisible, vectors of catch and vessels by year, and radii matrix
#' @export yearBubble
#' @examples
#' \dontrun{
#'  data(sps)
#'  cbv <- tapply(sps$catch_kg,list(sps$Vessel,sps$Year),sum,na.rm=TRUE)/1000
#'  dim(cbv)
#'  early <- rowSums(cbv[,1:6],na.rm=TRUE)
#'  late <- rowSums(cbv[,7:14],na.rm=TRUE)
#'  cbv1 <- cbv[order(late,-early),]
#'  plotprep(width=7,height=6)
#'  yearBubble(cbv1,ylabel="Catch by Trawl",vline=2006.5,diam=0.2)
#' }
#' # x=cbv1;xlabel="";ylabel="";diam=0.1;vline=NA;txt=c(3,4,5,5)
#' # Fyear=FALSE;xaxis=TRUE;yaxis=TRUE;hline=FALSE;nozero=FALSE
yearBubble <- function(x,xlabel="",ylabel="",diam=0.1,vline=NA,txt=c(4,6,9,11),
                       Fyear=FALSE,xaxis=TRUE,yaxis=TRUE,hline=FALSE,nozero=FALSE) {
  nyrs <- dim(x)[2]
  if (Fyear) {
    tyrs <- colnames(x)  # assumes a yyyy/yyyy format
    if (nchar(tyrs[1]) != 9) warning("Wrong fishing year format for yearBubble \n")
    years <- as.numeric(substr(tyrs,1,4))
  } else { years <- as.numeric(colnames(x)) # assumes columns are years
  }
  nves <- length(rownames(x))
  yvar <- seq(1,nves,1)
  if (nozero) {
    pick <- which(x == 0)
    x[pick] <- NA
  }
  radii <- sqrt(x)
  catch <- colSums(x,na.rm=TRUE)   # total annual catches  
  biggest <- max(radii,na.rm=TRUE)
  numves <- apply(x,2,function(x1) length(which(x1 > 0))) # num vess x year
  answer <- list(catch,numves,radii) # generate output
  names(answer) <- c("Catch","Vessels","Radii")
  xspace <- 0.3
  if (nchar(xlabel) > 0) xspace <- 0.45
  par(mfrow= c(1,1))
  par(mai=c(xspace,0.45,0.1,0.1), oma=c(0.0,0.0,0.0,0.0))
  par(cex=0.85, mgp=c(1.5,0.3,0), font.axis=7,font=7)
  xt <- "s"
  yt <- "s"
  if (!xaxis) xt <- "n"
  if (!yaxis) yt <- "n"
  plot(years,years,type="n",xlab="",ylab="",ylim=c(0,(nves+txt[4])),yaxs="r",
       yaxt=yt,xaxt=xt,xaxs="r")
  if (hline) abline(h=yvar,col="grey")
  for (i in 1:nyrs) { # i = 9
    yr <- years[i]
    odd.even<-i%%2
    if (odd.even == 0) { 
      text(yr,nves+txt[3],round(catch[i],0),cex=0.65,font=7)
      } else { 
      text(yr,nves+txt[2],round(catch[i],0),cex=0.65,font=7)
    }
    text(yr,nves+txt[1],numves[i],cex=0.8,font=7)
    if (sum(radii[,i],na.rm=TRUE) > 0) {
      mult <- max(radii[,i],na.rm=TRUE)/biggest
      symbols(rep(yr,nves),yvar,circles=radii[,i],inches=diam*mult,
              bg=rgb(1, 0, 0, 0.5), fg = "black",xlab="",ylab="",add=TRUE)
    }
  }
  if (length(vline) > 0) abline(v=c(vline),col="grey")
  title(ylab=list(ylabel, cex=1.0, col=1, font=7))
  return(invisible(answer))
} # end of YearBubble


