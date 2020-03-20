
#' @title addcontours eases adding contours to an xy plot of points
#' 
#' @description addcontours is used to add contours to a dense plot of 
#'     xy points such as might be generated when conducting an analysis 
#'     of the the uncertainty associated with a stock assessment, or 
#'     other analysis using a bootstrap, a Bayesian MCMC, or even using 
#'     asymptotic errors and sampling from a multi-variate normal. 
#'     addcoutours first uses the kde2d function from the MASS package 
#'     to translate the density of points into 2-D kernal densities, and 
#'     then searches through the resulting densities for those points 
#'     that would identify approximate contours. Finally it calls the 
#'     contour function to add the identified approximate contours to 
#'     the xy plot.
#'
#' @param xval the vector of x-axis values, one half of the data pairs
#' @param yval the vector of y-axis values, the other half of the data
#' @param xrange the range of x-axis data included in the graph
#' @param yrange the range of y-axis data included in the graph
#' @param ngrid the number of subdivisions by which to split the data 
#'     along each axis; defaults to 100
#' @param contval the contour values, defaults to those containing 50 
#'     and 90 percent i.e. c(0.5, 0.9) 
#' @param lwd the width of the contour lines, defaults=1
#' @param col the col of the contour lines, default=1
#'
#' @return nothing but it does add contours to a plot of points
#' @export
#'
#' @examples
#'  library(mvtnorm)
#'  library(MASS)
#'  data(abdat)
#'  param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05)) 
#'  bestmod <- nlm(f=negLL,p=param,funk=simpspm,indat=abdat, 
#'                 hessian=TRUE,logobs=log(abdat$cpue),
#'                 typsize=magnitude(param),iterlim=1000)
#'  optpar <- bestmod$estimate
#'  vcov <- solve(bestmod$hessian)      # solve inverts matrices
#'  columns <- c("r","K","Binit","sigma")
#'  N <- 1000  # the contours improve as N increases; try 5000
#'  mvnpar <- matrix(exp(rmvnorm(N,mean=optpar,sigma=vcov)),
#'                   nrow=N,ncol=4,dimnames=list(1:N,columns))
#'  xv <- mvnpar[,"K"]
#'  yv <- mvnpar[,"r"]
#'  # plotprep(width=6,height=5,newdev = FALSE)
#'  plot(xv,yv,type="p") # use default 0.5 and 0.9 contours.
#'  addcontours(xv,yv,range(xv),range(yv),lwd=2,col=2)
#'  points(mean(xv),mean(yv),pch=16,cex=1.5,col=2)
addcontours <- function(xval,yval,xrange,yrange,ngrid=100,
                        contval=c(0.5,0.90),lwd=1,col=1) {
  z<-kde2d(xval,yval,n=ngrid,lims=c(range(xrange),range(yrange)))
  zdat <- z$z      # get the kde values
  tot <- sum(zdat)  
  target <- 1-contval  # contour values
  ntarg <- length(target)   # number contours
  contourvalues <- numeric(ntarg) 
  steps <- 10
  for (targ in 1:ntarg) {  # for each contour  targ=1
    finish <- max(zdat)  
    begin <- finish/(2*steps)
    tmpval <- numeric(steps)
    count <- 0
    repeat {
      count <- count + 1
      for (i in 1:steps) {
        test <- seq(begin,finish,length=steps)
        pick <- which(zdat < test[i])
        samp <- sum(zdat[pick])
        tmpval[i] <- (samp/tot)-target[targ]
      }
      pickx <- which(tmpval > 0.0)
      if (count > 10) warning("Searching Loop reached 10; caution!")
      if ((tmpval[pickx[1]] < 0.01) | (count > 10)) break
      begin <- test[pickx[1]-1]
      finish <- test[pickx[1]]
    }
    contourvalues[targ] <- test[pickx[1]]
  }
  contour(z,drawlabels=FALSE,levels=c(contourvalues),add=TRUE,col=col,
          lwd=lwd,xlim=range(xrange,finite=TRUE),
          ylim=range(yrange,finite=TRUE))
} # end of addcontour


#' @title addnorm adds a normal distribution to a histogram of a data set.
#'
#' @description  addnorm adds a normal distribution to a histogram of a 
#'     data set. This is generally to be used to illustrate whether 
#'     log-transformation normalizes a set of catch or cpue data.
#' @param inhist is the output from a call to 'hist' (see examples)
#' @param xdata is the data that is being plotted in the histogram.
#' @param inc defaults to a value of 0.01; is the fine grain increment 
#'     used to define the normal curve. The histogram breaks should 
#'     be coarse grained relative to inc.
#' @return a list with a vector of 'x' values and a vector of 'y' values 
#'     (to be used to plot the fitted normal probability density function), 
#'     and a vector called 'stats' containing the mean and 
#'     standard deviation of the input data
#' @export
#' @examples
#'  oldpar <- par(no.readonly=TRUE)
#'  x <- rnorm(1000,mean=5,sd=1)
#'  #plotprep(height=6,width=4,newdev=FALSE)
#'  par(mfrow= c(1,1),mai=c(0.5,0.5,0.3,0.05))
#'  par(cex=0.75, mgp=c(1.5,0.35,0), font.axis=7)
#'  outH <- hist(x,breaks=25,col=3,main="")
#'  nline <- addnorm(outH,x)
#'  lines(nline$x,nline$y,lwd=3,col=2)
#'  print(nline$stats)
#'  par(oldpar)
addnorm <- function(inhist,xdata,inc=0.01) {
  lower <- inhist$breaks[1]
  upper <- tail(inhist$breaks,1)
  cw <- inhist$breaks[2]-inhist$breaks[1]
  x <- seq(lower,upper, inc) #+ (cw/2)
  avCE <- mean(xdata,na.rm=TRUE)
  sdCE <- sd(xdata,na.rm=TRUE)
  N <- length(xdata)
  ans <- list(x=x,y=(N*cw)*dnorm(x,avCE,sdCE),stats=c(avCE,sdCE,N))
  return(ans)
} # end of addnorm

#' @title addlnorm estimates a log-normal distribution from output of hist
#'
#' @description  addlnorm estimates a log-normal distribution from the
#'     output of a histogram of a data set.
#' @param inhist is the output from a call to 'hist' (see examples)
#' @param xdata is the data that is being plotted in the histogram.
#' @param inc defaults to a value of 0.01; is the fine grain increment 
#'     used to define the normal curve. The histogram breaks should be
#'     coarse grained relative to this.
#' @return a 4 x N matrix of x and y values to be used to plot the fitted 
#'     normal probability density function.Combined with estimates of 
#'     mean(log(indata)) and log(sd(indata))
#' @export
#'
#' @examples
#'  oldpar <- par(no.readonly=TRUE)
#'  egdata <- rlnorm(200,meanlog=0.075,sdlog=0.5)
#'  outh <- hist(egdata,main="",col=2,breaks=seq(0,8,0.2))
#'  ans <- addlnorm(outh,egdata)
#'  lines(ans[,"x"],ans[,"y"],lwd=2,col=4) 
#'  par(oldpar)
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

#' @title inthist a replacement for the hist function for use with integers
#'
#' @description inthist a replacement for the hist function for use with
#'    integers because the ordinary function fails to count them correctly. 
#'    The function is designed for integers and if it is given real numbers
#'     it will issue a warning and then round all values before plotting.
#' @param x the vector of integers to be counted and plotted OR a matrix 
#'     of values in column 1 and counts in column 2
#' @param col the colour of the fill; defaults to black = 1, set this to 0
#'    for an empty bar, but then give a value for border
#' @param border the colour of the outline of each bar defaults to 1
#' @param width denotes the width of each bar; defaults to 1, should be >0
#'    and usually <= 1. A warning will be issued outside this range. If
#'    < 0 then it will be reset to 1.0
#' @param xlabel the label for the x axis; defaults to ""
#' @param ylabel the label for the y axis; defaults to ""
#' @param main the title for the individual plot; defaults to ""
#' @param lwd the line width of the border; defaults to 1
#' @param xmin sets the lower bound for x-axis; used to match plots
#' @param xmax sets the upper bound for x axis; used with multiple plots
#' @param ymax enables external control of the maximum y value; mainly of
#'    use when plotting multiple plots together.
#' @param plotout plot the histogram or not? Defaults to TRUE
#' @param prop plot the proportions rather than the counts
#' @param inc sets the xaxis increment; used to customize the axis;
#'    defaults to 1.
#' @param xaxis set to FALSE to define the xaxis outside of inthist;
#'    defaults to TRUE
#'    
#' @return a matrix of values and counts is returned invisibly
#' @export
#' 
#' @examples
#'  oldpar <- par(no.readonly=TRUE)
#'  x <- trunc(runif(1000)*10) + 1
#'  #plotprep(width=6,height=4)
#'  inthist(x,col="grey",border=3,width=0.75,xlabel="Random Uniform",
#'          ylabel="Frequency")
#'  abline(h=100)
#'  par(oldpar)
inthist <- function(x,col=1,border=1,width=1,xlabel="",ylabel="",
                    main="",lwd=1,xmin=NA,xmax=NA,ymax=NA,plotout=TRUE,
                    prop=FALSE,inc=1,xaxis=TRUE) {
  if (class(x) == "matrix") {
    counts <- x[,2]
    values <- x[,1]
  } else {
    counts <- table(x)
    if (length(counts) == 0) stop("No data provided \n\n")
    values <- as.numeric(names(counts))
  }
  if (sum(!(abs(values - round(values)) < .Machine$double.eps^0.5)) > 0) {
    warning("Attempting to use 'inthist' with non-integers; 
            Values now rounded \n")
    values <- round(values,0)
  }
  if (width <= 0) {
    warning("width values must be >0 - reset = 1.0")
    width <- 1
  }
  counts <- as.numeric(counts)
  nct <- length(counts)
  propor <- counts/sum(counts,na.rm=TRUE)
  if (is.na(xmin)) xmin <- min(values)
  if (is.na(xmax)) xmax <- max(values)
  if (prop) {
    outplot <- propor
  } else {
    outplot <- counts
  }
  if (is.na(ymax)) {
    if (nchar(main) > 0) {
      ymax <- max(outplot) * 1.15
    } else {
      ymax <- max(outplot) * 1.05
    }
  }
  if (plotout) {
    plot(values,outplot,type="n",xlim=c((xmin-(width*0.75)),
         (xmax+(width*0.75))),xaxs="r",ylim=c(0,ymax),yaxs="i",xlab="",
         ylab="",xaxt="n", panel.first = grid())
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
    colnames(answer) <- c("values","counts","proportion")
  } else { answer <- NA  }
  class(answer) <- "inthist"
  return(invisible(answer))
}  # end of inthist


#' @title panel.cor is a version of function given in the pairs help
#' 
#' @description panel.cor is a panel function modified from that 
#'     described in the help file for the pairs function from the 
#'     graphics package. This has been customized both to show that 
#'     one can make such customizations, and to enable this one to be
#'     used to calculate the correlations between the variables 
#'     included in a pairs plot.
#'
#' @param x the first variable - provided by pairs
#' @param y the second variable, provided by pairs, see examples
#' @param digits how many digits to use on the pairs plot for the 
#'     correlations
#' @param ... any other graphics parameters to be passed to pairs.
#'
#' @return this prints the correlations in a square of the pairs plot
#' @export
#'
#' @examples
#'   dat <- matrix(rnorm(900,mean=5,sd=0.5),nrow=300,ncol=3)
#'   pairs(dat[,1:3],lower.panel=panel.smooth,  # all should be
#'         upper.panel=panel.cor,gap=0.25,lwd=2) #low correlations
panel.cor <- function(x, y, digits = 3, ...) {
  usr <- par("usr"); on.exit(par(usr)) #store par values
  par(usr = c(0, 1, 0, 1))
  r <- (cor(x, y))
  text(0.5, 0.5, round(r,digits), cex = 1.5)
}

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
#' @param cex the size of the font used, defaults to 0.75
#' @param font the font used, defaults to 7 which is Times Bold, 6 is
#'     Times, 1 is Sans and 2 is Sans Bold.
#' @param outmargin defines whether to leave extra space on the bottom, 
#'     left, top, or right hand sides of the plot. Used when plots 
#'     != c(1,1). Allows room for mtexting
#' @param margin defines the space allowed for labels on axes. Again,
#'     likely needs to change is having more than one plot
#'
#' @return nothing but it changes the base graphics par settings. The 
#'     original par values are returned invisibly if user wishes to reset.
#' @export
#'
#' @examples
#'  x <- rnorm(100,mean=5,sd=0.5)
#'  y <- rnorm(100,mean=5,sd=0.5)
#'  oldpar <- parset(plots=c(1,2))
#'  plot1(x,y,defpar=FALSE)
#'  plot1(y,x,defpar=FALSE)
#'  par(oldpar)
parset <- function(plots=c(1,1),cex=0.75,font=7,outmargin=c(0,0,0,0),
                   margin=c(0.45,0.45,0.05,0.05)) {
  oldpar <- par(no.readonly=TRUE)
  par(mfrow=plots,mai=margin,oma=outmargin)
  par(cex=cex, mgp=c(1.35,0.35,0), font.axis=font,font=font,
      font.lab=font)
  return(invisible(oldpar))
} # end of parset


#' @title parsyn types the standard par command syntax to the console
#'
#' @description parsyn prints the standard par command syntax to the 
#'     console so it can be copied and pasted into your own code and 
#'     modified as suits your needs. It is simply a memory aid.
#'
#' @return nothing but it writes two lines of R code to the console
#' @export
#'
#' @examples
#' \dontrun{
#'   parsyn()
#' }
parsyn <- function() {
  cat("par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0,0,0,0)) \n")
  cat("par(cex=0.75,mgp=c(1.35,0.35,0),font.axis=7,font=7,font.lab=7) \n")
}

#' @title plot1 a quick way to plot an xy line or point plot
#'
#' @description plot1 provides a quick way to plot out a single xy
#'     line or point plot. It can be used with plotprep to generate a 
#'     plot outside of Rstudio or by itself to generate one within 
#'     Rstudio. It uses a standard par setup and permits custom labels, 
#'     font, and font size (cex). It checks the spread of y and if a 
#'     ymax is not given in the parameters finds the ymax and checks 
#'     to see if y goes negative in which case it uses getmin, so the
#'     y-axis is set to 0 - ymax or ymin - ymax
#'
#' @param x The single vector of x data
#' @param y the single vector of y data. If more are required they can
#'     be added separately after calling plot1.
#' @param xlab the label for the x-axis, defaults to empty
#' @param ylab the label for the y-axis, defaults to empty
#' @param type the type of plot "l" is for line, the default, "p" is
#'     points. If you want both plot a line and add points afterwards.
#' @param usefont which font to use, defaults to 7 which is Times bold
#' @param cex the size of the fonts used. defaults to 0.75
#' @param maxy defaults to 0, which does nothing. If a value is given
#'     then this value is used rather than estimating from the input y
#' @param defpar if TRUE then plot1 will declare a par statement. If false 
#'     it will expect one outside the function. In this way plot1 can be
#'     used when plotting multiple graphs, perhaps as mfrow=c(2,2)
#' @param ...  required to allow funk to access its other parameters 
#'     without having to explicitly declare them in plot1, these include
#'     col, default = black, pch, if the type = "p", lwd, etc.
#'
#' @return plots a graph and sets the default plotting par values. 
#'     This changes the current plotting options! The original par 
#'     values are returned invisibly if the user wishes to reset.
#' @export
#'
#' @examples
#'  x <- rnorm(20,mean=5,sd=1)
#'  oldpar <- plot1(x,x,xlab="x-values",ylab="yvalues")
#'  points(x,x,pch=16,cex=1.5)
#'  par(oldpar)
plot1 <- function(x,y,xlab="",ylab="",type="l",usefont=7,cex=0.75,
                  maxy=0,defpar=TRUE,...){
  oldpar <- par(no.readonly=TRUE)
  if (defpar) {
    par(mfrow = c(1,1), mai = c(0.45,0.45,0.1,0.05),oma = c(0,0,0,0))
    par(cex = cex, mgp = c(1.35, 0.35, 0), font.axis = usefont,
        font = usefont, font.lab = usefont)
  }
  if (maxy > 0) ymax <- maxy  else ymax <- getmax(y)
  if (min(y,na.rm=TRUE) < 0.0) ymin <- getmin(y) else ymin <- 0.0
  plot(x,y,type=type,ylim=c(ymin,ymax),yaxs="i",
       ylab=ylab,xlab=xlab,cex=cex,panel.first=grid(),...)
  return(invisible(oldpar))
} # end of plot1

#' @title plot.dynpop an S3 method for plotting dynpop objects
#' 
#' @description plot.dynpop an S3 method for plotting dynpop objects
#'     which are generated by the function discretelogistic. It 
#'     generates a plot of the numbers through time, that is year x Nt,
#'     and a second plotthat is a phase plot of Nt+1 vs Nt, to better
#'     illustrate the dynamics and simplify the search for equilibria.
#'     The input is designed to be the invisibly produced output from
#'     the MQMF function discretelogistic, but as long as there is at
#'     least a matrix of year, Nt, and Nt+1 with a class of dynpop, 
#'     then a call to plot should generate the two graphs.
#'
#' @param x a dynpop matrix containing at least year, nt, and nt1 as
#'     columns, as returned invisibly from discretelogistic.
#' @param y a second y value added to the first plot if present
#' @param main defines text to print across top of plot with the
#'     intention of including the parameter values, default="" 
#' @param cex the size of the fonts used, default=0.9
#' @param font the font used default=7 (bold serif) 1 = non-serif,
#'     2 = bold non-serif, 6 = serif
#' @param ... ready for extra graphical parameters
#'
#' @return it returns the dynpop matrix invisibly
#' @export
#'
#' @examples
#' \dontrun{
#'   r <- 2.5; K <- 1000.0; N0 <- 50; Ct <- 0.0; yrs <- 50; p <- 1
#'   pop <- discretelogistic(r=r,K=K,N0=N0,Ct=Ct,Yrs=yrs,p=p)
#'   plot(pop,main=paste0("r=",r,"  K=",K,"  Ct=",Ct, "  N0=",N0,
#'                        "  p=",p),cex=0.85,font=7)
#' }
plot.dynpop <- function(x, y=NULL,main="",cex=0.9,font=7, ...) { 
  colnames(x) <- tolower(colnames(x))
  ymax <- getmax(x[,"nt"])
  xl <- c(0,ymax)
  yrs <- length(x[,"year"])
  oldpar <- par(no.readonly=TRUE)
  on.exit(par(oldpar))
  par(mfrow=c(1,2),mai=c(0.45,0.45,0.05,0.1),oma=c(0.0,0,2.0,0.0))
  par(cex=cex, mgp=c(1.35,0.35,0), font.axis=font,font=font,
      font.lab=font)
  plot(x[,"year"],x[,"nt"],type="l",col=1,lwd=1,ylim=c(0,ymax),yaxs="i",
       panel.first = grid(),xlab="Time",ylab="Population Size")
  if (length(y) > 0) {
    lines(x[,"year"],y,lwd=1,col=3)
  }
  mtext("Population Dynamics",side=3,line=0.0,cex=cex,font=font)
  plot(x[1:yrs,"nt"],x[1:yrs,"nt1"],type="p",pch=1,lwd=1.0,
       cex=cex,yaxs="i",xlim=c(0,ymax),ylim=c(0,ymax),col="darkgrey",
       panel.first=grid(),xlab="Population Nt",ylab="Population Nt+1")
  begin <- trunc(yrs * 0.8)      # final 20%
  lines(xl,xl,lwd=2,col="grey")
  points(x[begin:yrs,"nt"],x[begin:yrs,"nt1"],pch=18,col=1,
         cex=(cex*1.2))
  mtext("Phase Plot",side=3,line=0.0,cex=cex,font=font,outer=FALSE)
  mtext(main,side=3,line=1.0,cex=cex,font=font,outer=TRUE)
  invisible(x)
} # end of S3 method plot.dlpop

#' @title plotprep: sets up a window and the par values for plotting
#'
#' @description plotprep: sets up a window and changes the par values 
#'     for plots. This is simply a utility function to save typing the
#'     standard syntax. Some of the defaults can be changed. Typing 
#'     the name without () will provide a template for modification. 
#'     If different par values are wanted then just include a par 
#'     statement after plotprep(). Calling plotprep saves the current 
#'     par settings and returns them invisibly. So to recover the 
#'     original par settings after oldpar <- plotprep(), and you have 
#'     completed your plot, you can include a par(oldpar) to recover 
#'     your original settings.
#'   
#' @param width defaults to 6 inches = 15.24cm - width of plot
#' @param height defaults to 3.6 inches = 9.14cm - height of plot
#' @param usefont default=7 (bold Times) 1=sans serif, 2=sans serif bold
#' @param cex default=0.75, size of font used for text within the plots
#' @param newdev reuse a previously defined graphics device or make a 
#'     new one; default=FALSE
#' @param filename default="" ie do not save to a filename. If filename 
#'     is defined it makes that file as a png file with resolution resol
#' @param resol resolution of the png file, if defined, default=300
#' @param verbose set this to TRUE to turn on the reminder to 
#'     include a graphics.off() command after the plot. Default=FALSE
#' 
#' @return sets up a graphics device, if needed, and resets the default 
#'     plotting par values. This changes the current plotting options! 
#'     The original par values are returned invisibly.
#' @export
#' 
#' @examples
#'  x <- rnorm(1000,mean=0,sd=1.0)
#'  plotprep(newdev=TRUE)
#'  hist(x,breaks=30,main="",col=2)
#'  oldpar <- plotprep(width=6,height=5,newdev=FALSE)
#'  par(mfrow = c(2,1)) # can run parset() or change the par settings
#'  hist(x,breaks=20,main="",col=2)
#'  hist(x,breaks=30,main="",col=3)
#'  par(oldpar)
plotprep <- function(width=6,height=3.6,usefont=7,cex=0.75,
                     newdev=FALSE,filename="",resol=300,
                     verbose=FALSE) {
  if  ((names(dev.cur()) != "null device") & (newdev)) 
      suppressWarnings(dev.off())
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
  par(mfrow = c(1,1),mai=c(0.45,0.45,0.1,0.05), oma=c(0,0,0,0))
  par(cex=cex,mgp=c(1.35,0.35,0),font.axis=usefont,font=usefont, 
      font.lab=usefont)
  if ((lenfile > 0) & (verbose))
      cat("\n Remember to place 'graphics.off()' after plot \n")
  return(invisible(oldpar))
} # end of plot_prep

#' @title plotprofile simplifies plotting single likelihood profiles
#' 
#' @description plotprofile simplifies plotting out the likelihood 
#'     profiles of single parameters or variables. It is necessary to 
#'     pass the function the output from the profile calculations, 
#'     identifying the variable name against which to plot the 
#'     likelihood. Identifying the name of the -ve log-likelihood 
#'     column. Facilities are provided for defining the x and y axis 
#'     labels. We need to use the function which.closest because we
#'     use a sequence of parameter values so an exact match would be
#'     highly unlikely.
#'
#' @param prof the results from the likelihood profile calculations. 
#'     This matrix should include, as a minimum, the fixed variable 
#'     of interest and the matching -ve log-likelihood in named 
#'     columns.
#' @param var the name of the variable of interest to identify the 
#'     column in prof in which to find the vector of fixed values 
#'     given.
#' @param digit this is a vector of three that determine by how much 
#'     the round function limits the values printed of the 95% and 
#'     mean at the top of the plot.
#' @param xlabel the x-axis label, defaults to the name of the var
#' @param ylabel the y-axis label, defaults to -ve Log-Likelihood
#' @param like identifies the name of the column containing the -ve 
#'     log-likelihood
#' @param defpar logical, should the par values be assumed or defined, 
#'     defaults to TRUE, so only one plot will be produced and any old
#'     par values will be restored afterwards. If part of a multiple 
#'     plot define the formatting before calling and set this
#'     to FALSE. This will lead to the oldpar being returned invisibly
#'     so that eventually the par settings can be reset.
#' @param ... used for any other graphic parameters used.     
#'
#' @return nothing but this does generate a plot.
#' @export
#'
#' @examples
#'  data(abdat)    #usually use  ~100 steps in rval, perhaps use
#'  rval <- seq(0.325,0.45,0.02)   # seq(0.325,0.45,0.001)
#'  ntrial <- length(rval)
#'  columns <- c("r","K","Binit","sigma","-veLL")
#'  result <- matrix(0,nrow=ntrial,ncol=length(columns),
#'                   dimnames=list(rval,columns))
#'  bestest <- c(r= 0.32,K=11000,Binit=4000,sigma=0.05) 
#'  for (i in 1:ntrial) {  #i <- 1
#'    param <- log(c(rval[i],bestest[2:4])) 
#'    parinit <- param    
#'    bestmodP <- nlm(f=negLLP,p=param,funk=simpspm,initpar=parinit,
#'                   indat=abdat,logobs=log(abdat$cpue),notfixed=c(2:4),
#'                   typsize=magnitude(param),iterlim=1000)
#'    bestest <- exp(bestmodP$estimate)
#'    result[i,] <- c(bestest,bestmodP$minimum)
#'  }
#'  plotprofile(result,var="r",defpar=TRUE,lwd=2)
plotprofile <- function(prof,var,digit=c(3,3,3),xlabel=var,
                         ylabel="-ve Log-Likelihood",like="-veLL",
                         defpar=TRUE,...) {
  oldpar <- par(no.readonly=TRUE)
  if (defpar) on.exit(par(oldpar))
  plot1(prof[,var],prof[,like],xlab=xlabel,ylab=ylabel,
        defpar=defpar,...)
  ntrial <- dim(prof)[1]
  minimLL <- min(prof[,like],na.rm=TRUE)
  upper <- (minimLL+1.92)
  abline(h=c(minimLL,upper),col=2)
  mid <- which.closest(minimLL,prof[,like])
  left <- which.closest(upper,prof[1:mid,like])
  right <- which.closest(upper,prof[mid:ntrial,like])
  abline(v=c(prof[c(left,mid,(mid+right-1)),var]),lwd=c(2,1,2),col=3)
  label <- paste0("Mean +/- 95%CI = ",round(prof[left,var],digit[1]),"   ",
                  round(prof[mid,var],digit[2]),"    ",
                  round(prof[(mid+right-1),var],digit[3]))
  mtext(label,side=3,outer=FALSE,line=-1.1,cex=0.9,font=7)
  if (!defpar) return(invisible(oldpar))
} # end of plotprofile

#' @title setpalette is a shortcut for altering the palette to R4
#' 
#' @description setpalette is a shortcut for changing the 
#'     default color palette to the proposed new R version 4.0.0 
#'     version before it comes out. The new palette was described in a
#'     blog post at developer.r-project.org and provides less 
#'     garish and a more visible set of default colours that can
#'     be called using the numbers 1 - 8. An important point is 
#'     that this alters the default colours for all sessions
#'     until a restart of R. Using something similar you can 
#'     define your own preferred palettes should you wish to.     
#'     
#' @param x either "default", "R3", or "R4", with R4 as the 
#'     default value. Use "default" or "R3" to revert back to the
#'     standard R version 3. values.
#'
#' @return nothing but it does alter the base colour palette
#' @export
#'
#' @examples
#'    setpalette("R3")
#'    plot(1:8,rep(0.25,8),type="p",pch=16,cex=5,col=c(1:8))
#'    text(1,0.2,"Default R3.0.0 - some garish or pale",cex=1.5,
#'         font=7,pos=4)
#'    setpalette("R4")
#'    points(1:8,rep(0.3,8),pch=16,cex=5,col=c(1:8)) #toprow
#'    text(1,0.325,"Default R4.0.0 - more balanced",cex=1.5,
#'         font=7,pos=4)
setpalette <- function(x="R4") { # x="R4"
  choice <- c("default","R3","R4")
  if (x %in% choice) {
    if ((x == "R3") | (x == "default")) {
      palette("default")
    }
    if (x == "R4") {
      palette(c("#000000", "#DF536B", "#61D04F", "#2297E6",
                "#28E2E5", "#CD0BBC", "#EEC21F", "#9E9E9E"))
    }
  } else {
    cat("Currently options are default, R3, or R4 \n")
  }
} # end of setpalette

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
#'   x <- rlnorm(5000, meanlog=2, sdlog=1)
#'   hist(x,breaks=30,main="",xlab="log-normal values")
#'   uphist(x,breaks=30,main="",xlab="log-normal values",maxval=100)
#'   uphist(x,breaks=30,main="",xlab="log-normal values",maxval=1000)
uphist <- function(x,maxval=NA,...) {
  oldpar <- par(no.readonly=TRUE)
  on.exit(par(oldpar))
  if (is.numeric(maxval)) {
    pick <- which(x > maxval)
    if (length(pick) > 0) x <- x[-pick]
  }
  if (length(x) > 0){
    hist(x,...)
  } else {
    stop("maxval in uphist too small and no data remaining. \n")
  }
} #end of uphist
