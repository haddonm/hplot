





library(codeutils)
library(hplot)
plotprep(width=6, height=5,newdev=TRUE,filename="",verbose=FALSE)
parset()
rad <- seq(0, 15 * pi, length.out = 500)  # Angle in radians                               # Radius grows with the angle
x <- rad * cos(rad)                        # x-coordinates
y <- rad * sin(rad)                        # y-coordinates
rad2 <- seq(0, 15 * pi, length.out = 500)  # Angle in radians                               # Radius grows with the angle
x2 <- rad2 * sin(rad2)                        # x-coordinates
y2 <- rad2 * cos(rad2)                        # y-coordinates
miny <- getmin(c(y,y2)); minx <- getmin(c(x,x2))
maxy <- getmax(c(y,y2)); maxx <- getmax(c(x,x2))
plot(x, y,type = "l",col = "white",lwd = 4,xaxt="n",yaxt="n",
     xlab = "", ylab = "", main = "",ylim=c(miny,maxy),xlim=c(minx,maxx))
low <- -29;  high <- 31
lines(c(low,high),c(low,high),lwd=4,col=3)
lines(c(x[500],x2[500]),c(y[500],y2[500]),lwd=4,col=3)
lines(x,y,lwd=4,col="red")
lines(x2,y2,lwd=4,col="blue")


gethypot <- function(x,y,x0=0,y0=0) {
  return(sqrt((x0 - x) * (x0 - x) + (y0 - y) * (y0 - y)))
}

N <-  2000
plotprep(width=8, height=7,newdev=FALSE,filename="",verbose=FALSE)
parset()
rad <- seq(0, 5 * pi, length.out = N)  # Angle and radius in radians 
mult <- seq(0.5,1.2,length.out = N)
x1 <- mult*rad * cos(rad + (pi/3))            # red
y1 <- mult*rad * sin(rad + (pi/3)) 
x2 <- mult*rad * cos(rad + (pi))   # blue
y2 <- mult*rad * sin(rad + (pi))
x3 <- mult*rad * cos(rad + (5*pi/3)) # green   
y3 <- mult*rad * sin(rad + (5*pi/3))
maxrad <- gethypot(x=x1[N],y=y1[N])
miny <- getmin(c(y1,y2,y3,-maxrad)); minx <- getmin(c(x1,x2,x3,-maxrad))
maxy <- getmax(c(y1,y2,y3,maxrad)); maxx <- getmax(c(x1,x2,x3,maxrad))
plot(x1, y1,type = "l",col = "red",lwd = 4,xaxt="n",yaxt="n",
     xlab = "", ylab = "", main = "",ylim=c(miny,maxy),xlim=c(minx,maxx))
lines(x2,y2,col="red",lwd=4)
lines(x3,y3,col="red",lwd=4)
points(0,0,pch=16,cex=2,col="red")
points(0,0,pch=16,cex=1,col="white")
r <- gethypot(x=x1[N],y=y1[N])   
theta <- seq(0, 2 * pi, length.out = 4*N)
x <- r * cos(theta)
y <- r * sin(theta)
lines(x,y,lwd=3,col="black")




gethypot(x=x3[500],y=y3[500])  
gethypot(x=x2[500],y=y2[500])    
gethypot(x=x1[500],y=y1[500])    

r <- gethypot(x=x1[500],y=y1[500])   
theta <- seq(0, 2 * pi, length.out = 500)
x <- r * cos(theta)
y <- r * sin(theta)
lines(x,y,lwd=2,col="black")
# Plot the circle
plot(x, y, type = "l", asp = 1, xlab = "X", ylab = "Y", main = paste("Circle with Radius =", r))
abline(h = 0, v = 0, col = "gray", lty = 2)  # Optional: Add reference lines



drawcircle <- function(r,lwd=2,col=1,line=FALSE) {
  theta <- seq(0, 2 * pi, length.out = 500)
  x <- r * cos(theta)
  y <- r * sin(theta)
  if (col != 0) {
    if (line) {
      lines(x,y,lwd=lwd,col=col)
    } else {
      plot(x, y, type = "l", lwd=lwd, asp = 1, xlab = "", ylab = "", main = "",
           xaxt="n",yaxt="n")
      abline(h = 0, v = 0, col = "gray", lty = 2)  # Optional: Add reference lines
    }
  }
  return(invisible(cbind(x,y)))
} # end of drawcircle

plotprep(width=6,height=6)
parset(margin=c(0.05,0.05,0.05,0.05))
drawcircle(r=17,lwd=5)
drawcircle(r=11,lwd=4,line=TRUE)
drawcircle(r=7,lwd=3,line=TRUE)
drawcircle(r=5,lwd=2,line=TRUE)
xy <- drawcircle(r=3,lwd=1,col=0,line=TRUE)
polygon(xy[,1],xy[,2],col=RGB(col="red",alpha=127))
# Optional: Add circular "frame" to emphasize Celtic style

symbols(0, 0, circles = gethypot(x=x2[500],y=y2[500]), 
        add = TRUE, inches = FALSE, lwd = 2)





