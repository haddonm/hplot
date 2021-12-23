


# plotutils ----------------------------------------------------

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
#' @param xlabel the label for the x axis; defaults to ""
#' @param ylabel the label for the y axis; defaults to ""
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
inthist <- function(x,col=1,border=NULL,width=0.9,xlabel="",ylabel="",
                    main="",lwd=1,xmin=NA,xmax=NA,ymax=NA,plotout=TRUE,
                    prop=FALSE,inc=1,xaxis=TRUE,roundoff=TRUE,...) {
  #  x=ebtipy;col=2;border=3;width=0.9;xlabel="";ylabel="";main="";lwd=1;xmin=NA
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
      title(ylab=list(ylabel, cex=1.0, font=7),
            xlab=list(xlabel, cex=1.0, font=7))
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

#' @title pickbound selects an optimum number of rows and cols for a plot
#' 
#' @description pickbound enables the automatic selection of a pre-determined
#'     optimum combination of plot rows and columns to suit a number of plots
#'     up to 30. So, given a number of plots from 1 to 30 this returns a numeric 
#'     dimer containing the number of rows and columns needed for par statement
#'
#' @param n the number of plots to be included in a combined plot
#'
#' @return a vector of two with the number of rows and columns for a plot
#' @export
#'
#' @examples
#' pickbound(5)
#' pickbound(8)
pickbound <- function(n) {
  bounds <- matrix(c(1,1,1,2,2,1,3,2,2,4,2,2,5,3,2,6,3,2,7,4,2,8,4,2,9,3,3,10,3,4,
                     11,3,4,12,3,4,13,5,3,14,5,3,15,5,3,16,4,4,17,5,4,18,5,4,19,5,4,
                     20,5,4,21,5,5,22,5,5,23,5,5,24,5,5,25,5,5,26,5,6,27,5,6,28,5,6,
                     29,5,6,30,5,6),nrow=30,ncol=3,byrow=TRUE)
  out <- c(bounds[n,2],bounds[n,3])
  return(out)
} # end of pickbound



#' @title newplot simple floating window setup a plot
#'
#' @description newplot is a bare-bones setup routine to generate a plot in
#'     RStudio using a floating window. If you want to alter the default par
#'     settings then you can use either setplot() to get suitable syntax or,
#'     more simply, use parsyn() which only gives a template for the par 
#'     syntax
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
#' \dontrun{
#' x <- rnorm(20,mean=5,sd=1)
#' plot1(x,x,xlabel="x-values",ylabel="yvalues")
#' }
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

#' @title plotnull generates an empty plot when one is needed
#'
#' @description plotnull there are often circumstances, for example, when
#'     plotting up results from each year and each SAU, where there will be
#'     combinations of year and SAU that have no data, but to avoid a problem
#'     with the plotting it is necessary to generate an empty plot.
#'
#' @param msg a message to be printed in the middle of the empty plot.
#'
#' @return nothing but it does generate a plot
#' @export
#'
#' @examples
#' plotnull("An empty plot")
plotnull <- function(msg="") {
  plot(1:10,1:10,type="n",xaxt="n",yaxt="n",xlab="",ylab="")
  if (nchar(msg) > 0)
    text(x=5,y=5,msg,cex=1.0,font=7)
} # end of plotnull

#' @title plotprep sets up a window and the par values for a single plot
#'
#' @description plotprep sets up a window and the par values for a single 
#'     plot. It checks to see if a graphics device is open and opens a new 
#'     one if not. This is simply a utility function to save typing the 
#'     standard syntax. Some of the defaults can be changed. Typing the name
#'     without () will provide a template for modification. If 'windows' is 
#'     called repeatedly this will generate a new active graphics device 
#'     each time leaving the older ones inactive but present. For quick 
#'     exploratory plots this behaviour is not wanted, hence the check if 
#'     an active device exists already or not.
#'
#' @param width defaults to 6 inches = 15.24cm - width of plot
#' @param height defaults to 3 inches = 7.62cm - height of plot
#' @param usefont default is 7 (bold Times) 1 sans serif, 2 sans serif bold
#' @param cex default is 0.85, the font size font used for text in the plots
#' @param newdev reuse a previously defined graphics device or make new one;
#'     defaults to TRUE
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
#' @examples
#' \dontrun{
#'  x <- rnorm(1000,mean=0,sd=1.0)
#'  plotprep()
#'  hist(x,breaks=30,main="",col=2)
#' }
plotprep <- function(width=6,height=3.6,usefont=7,cex=0.85,
                     newdev=TRUE,filename="",resol=300,verbose=TRUE) {
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
#' @param x the x values
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
#' \dontrun{
#' x <- 1:20
#' yval1 <- rnorm(20,mean=5,sd=1)
#' yval2 <- rnorm(20,mean=10,sd=1)
#' plotxyy(x,yval1,yval2)
#' }
plotxyy <- function(x,y1,y2,xlab="",ylab1="",ylab2="",cex=0.85,fnt=7,
                    colour=c(1,2),defpar=FALSE) {
  if (defpar) {
    par(mfrow=c(1,1),mai=c(0.5,0.45,0.15,0.05),oma=c(0.0,0.75,0.0,3.0)) 
    par(cex=cex, mgp=c(1.35,0.35,0), font.axis=fnt,font=fnt,font.lab=fnt) 
  }
  maxy <- getmax(y1)
  plot(x,y1,type="l",lwd=2,col=colour[1],ylim=c(0,maxy),yaxs="i",
       ylab="",xlab="")
  mtext(ylab1, side=2, line=1.5)
  mtext(xlab, side=1, line=1.25)
  par(new=TRUE)
  maxy2 <- getmax(y2)
  plot(x,y2,type="l",lwd=2,col=colour[2],ylim=c(0,maxy2),axes=FALSE,
       xlab="",ylab="",yaxs="i")
  mtext(ylab2, side=4, line=1.5)
  axis(4)
  grid(ny=0)
} # end of plotxyy

#' @title setplot provides an example plot with defaults for a standard plot
#'
#' @description Provides an example plot with defaults for a standard plot
#'   includes details of how to gnerate tiff, pdf, and png versions,
#'   mtext and legends. Currently no parameters, but the function
#'   is open to development for customization of the example plot.
#' @return prints lines of R that will define a standard plot and can be 
#'     copied into an R script.
#' @export setplot
#' @examples
#' \dontrun{
#' setplot()
#' }
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
#' \dontrun{
#'   x <- rlnorm(5000, meanlog=2, sdlog=1)
#'   hist(x,breaks=30,main="",xlab="log-normal values")
#'   uphist(x,breaks=30,main="",xlab="log-normal values",maxval=100)
#'   uphist(x,breaks=30,main="",xlab="log-normal values",maxval=1000)
#' }
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

