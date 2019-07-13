
#' @title addcontours facilitates adding ontours to an xy plot of points
#' 
#' @description addcontours is used to add contours to a dense plot of xy
#'     points such as might be generated when conducting an analysis of the
#'     the uncertainty associated with a stock assessment, or other 
#'     analysis using a bootstrap, a Bayesian MCMC, or even using 
#'     asymptotic errors and sampling from a muti-variate normal. 
#'     addcoutours first uses the kde2d function from the MASS package to
#'     translate the density of points into 2-D kernal densities, and then 
#'     searches through the resulting densities for those points that would
#'     identify approximate contours. Finally it calls the contour function
#'     to add the identified contours to the xy plot.
#'
#' @param xval the vector of x-axis values, one halfopf the data pairs
#' @param yval the vector of y-axis values, the other half of plotted data
#' @param xrange the range of x-axis data included in the graph
#' @param yrange the range of y-axis data included in the graph
#' @param ngrid the number of subdivisions by hich to split the data along
#'     each axis; defaults to 100
#' @param contval the contour values, defaults to those containing 50 and
#'     90 percent i.e. c(0.5, 0.9) 
#' @param lwd the width of the contour lines, defaults=1
#' @param col the col of the contour lines, default=1
#'
#' @return nothing but it does add contours to a plot of points
#' @export
#'
#' @examples
#' \dontrun{
#' # library(mvtnorm)
#' # library(MASS)
#'  data(abdat)
#'  fish <- abdat$fish
#'  param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05)) 
#'  bestmod <- nlm(f=negLL,p=param,funk=simpspm,indat=fish, hessian=TRUE,
#'             logobs=log(fish$cpue),typsize=magnitude(param),iterlim=1000)
#'  optpar <- bestmod$estimate
#'  vcov <- solve(bestmod$hessian)      # solve inverts matrices
#'  columns <- c("r","K","Binit","sigma")
#'  N <- 1000  # the contours improve as N increases; try 5000
#'  mvnpar <- matrix(exp(rmvnorm(N,mean=optpar,sigma=vcov)),
#'                   nrow=N,ncol=4,dimnames=list(1:N,columns))
#'  xv <- mvnpar[,"K"]
#'  yv <- mvnpar[,"r"]
#'  plotprep(width=6,height=5,newdev = FALSE)
#'  plot(xv,yv,type="p")
#'  addcontours(xv,yv,range(xv),range(yv),lwd=2,col=2)
#'  points(mean(xv),mean(yv),pch=16,cex=1.5,col=2)
#' }
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


#' @title addnorm - adds a normal distribution to a histogram of a data set.
#'
#' @description  addnorm - adds a normal distribution to a histogram of a 
#'     data set. This is generally to be used to illustrate whether 
#'     log-transformation normalizes a set of catch or cpue data.
#' @param inhist - is the output from a call to 'hist' (see examples)
#' @param xdata -  is the data that is being plotted in the histogram.
#' @param inc - defaults to a value of 0.01; is the fine grain increment 
#'     used to define the normal curve. The histogram will be coarse 
#'     grained relative to this.
#' @return a list with a vector of 'x' values and a vector of 'y' values 
#'     (to be used to plot the fitted normal probability density function), 
#'     and a vector used two called 'stats' containing the mean and sandard 
#'     deviation of the input data
#' @export
#' @examples
#' \dontrun{
#'  x <- rnorm(1000,mean=5,sd=1)
#'  dev.new(height=6,width=4,noRStudioGD = TRUE)
#'  par(mfrow= c(1,1),mai=c(0.5,0.5,0.3,0.05))
#'  par(cex=0.75, mgp=c(1.5,0.35,0), font.axis=7)
#'  outH <- hist(x,breaks=25,col=3,main="")
#'  nline <- addnorm(outH,x)
#'  lines(nline$x,nline$y,lwd=3,col=2)
#'  print(nline$stats)
#' }
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

#' @title addlnorm - estimates a log-normal distribution from output of hist
#'
#' @description  addlnorm - estiamtes a log-normal distribution from output 
#'     of a histogram of a data set.
#' @param inhist - is the output from a call to 'hist' (see examples)
#' @param xdata -  is the data that is being plotted in the histogram.
#' @param inc - defaults to a value of 0.01; is the fine grain increment 
#'     used to define the normal curve. The histogram will be coarse grained 
#'     relative to this.
#' @return a 4 x N matrix of x and y values to be used to plot the fitted 
#'     normal probability density function.Combined with estiamtes of 
#'     mean(log(indata)) and log(sd(indata))
#' @export addlnorm
#'
#' @examples
#' \dontrun{
#'  egdata <- rlnorm(200,meanlog=0.075,sdlog=0.5)
#'  outh <- hist(egdata,main="",col=2,breaks=seq(0,8,0.2))
#'  ans <- addlnorm(outh,egdata)
#'  lines(ans[,"x"],ans[,"y"],lwd=2,col=4) 
#' }
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
#' @param x - the vector of integers to be counted and plotted OR a matrix 
#'     of values in column 1 and counts in column 2
#' @param col - the colour of the fill; defaults to black = 1, set this to 0
#'    for an empty bar, but then give a value for border
#' @param border - the colour of the outline of each bar defaults to col
#' @param width - denotes the width of each bar; defaults to 1, should be >0
#'    and usually <= 1. A warning will beissued outside this range. If
#'    < 0 then it will be reset to 1.0
#' @param xlabel - the label for the x axis; defaults to ""
#' @param ylabel - the label for the y axis; defaults to ""
#' @param main - the title for the individual plot; defaults to ""
#' @param lwd - the line width of the border; defaults to 1
#' @param xmin - sets the lower bound for x-axis; used to match plots
#' @param xmax - sets the upper bound for x axis; used with multiple plots
#' @param ymax - enables external control of the maximum y value; mainly of
#'    use when plotting multiple plots together.
#' @param plotout - plot the histogram or not? Defaults to TRUE
#' @param prop - plot the proportions rather than the counts
#' @param inc - sets the xaxis increment; used to customize the axis;
#'    defaults to 1.
#' @param xaxis - set to FALSE to define the xaxis outside of inthist;
#'    defaults to TRUE
#' @return a matrix of values and counts is returned invisibly
#' @export inthist
#' @examples
#' \dontrun{
#'  x <- trunc(runif(1000)*10) + 1
#'  plotprep(width=6,height=4,plots=c(1,1))
#'  inthist(x,col="grey",border=3,width=0.75,xlabel="Random Uniform",
#'          ylabel="Frequency")
#' }
inthist <- function(x,col=1,border=NULL,width=1,xlabel="",ylabel="",
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


#' @title newplot simple floating window setup a plot
#'
#' @description newplot is a bare-bones setup routine to generate a plot in
#'     RStudio using a floating window. If you want to alter the default par
#'     settings then you can use either setplot to get suitable syntax or,
#'     more simply, use parsyn which only give a template for the par syntax
#' @param width defaults to 6 inches = 15.24cm - width of plot
#' @param height defaults to 3.6 inches = 9.144cm - height of plot
#' @param newdev reuse a previously defined graphics device or make a new 
#'     one; defaults to TRUE
#' @return Checks for and sets up a graphics device and sets the default 
#'     plotting par values. This changes the current plotting options!
#' @export
#' @examples
#' \dontrun{
#'  x <- rnorm(1000,mean=0,sd=1.0)
#'  plotprep()
#'  hist(x,breaks=30,main="",col=2)
#' }
newplot <- function(width=5,height=3.15,newdev=TRUE) {
  if  ((names(dev.cur()) != "null device") & (newdev)) 
    suppressWarnings(dev.off())
  if (names(dev.cur()) %in% c("null device","RStudioGD"))
    dev.new(width=width,height=height,noRStudioGD = TRUE)
  par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0))
  par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
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
#' @param cex the size of the font used, defaults to 0.75
#' @param font the font used, defaults to 7 which is Times Bold, 6 is
#'     Times, 1 is Sans and 2 is Sans Bold.
#'
#' @return nothing but it changes teh base graphics par settings
#' @export
#'
#' @examples
#' \dontrun{
#'  parset()
#'  parsyn()
#' }
parset <- function(plots=c(1,1),cex=0.75,font=7) {
  par(mfrow=plots,mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0))
  par(cex=cex, mgp=c(1.35,0.35,0), font.axis=font,font=font,
      font.lab=font)
} # end of parset

#' @title parsyn types the standard par command syntax  to the console
#'
#' @description parsyn types the standard par command syntax to the console
#'      so it can be copied and pasted into your own code and modified.
#'
#' @return it writes two lines of R code to the console
#' @export
#'
#' @examples
#' \dontrun{
#'  parsyn()
#' }
parsyn <- function() {
  cat("par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0,0,0,0)) \n")
  cat("par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7) \n")
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
#' @param xlabel the label fot the x-axis, defaults to empty
#' @param ylabel the label fot the y-axis, defaults to empty
#' @param type the type of plot "l" is for line, the default, "p" is
#'     points. If you want both plot a line and add points afterwards.
#' @param usefont which font to use, defaults to 7 which is Times bold
#' @param cex the size of the fonts used. defaults to 0.75
#' @param maxy defaults to 0, which does nothing. If a value is given
#'     then this value is used rather than estimating from the input y
#' @param defpar if TRUE then plot1 will declare a par statement. If false 
#'     it will expect one outside the function. In this way plot1 can be
#'     used when plotting multiple graphs, perhaps as mfrow=c(2,2)
#' @param inpch the input character type if using type="p", default=16
#' @param incol the colour to use for the line or points, default = black
#'
#' @return nothing but it does plot a graph and changes the par setting
#' @export
#'
#' @examples
#' \dontrun{
#'  x <- rnorm(20,mean=5,sd=1)
#'  plot1(x,x,xlabel="x-values",ylabel="yvalues")
#' }
plot1 <- function(x,y,xlabel="",ylabel="",type="l",usefont=7,cex=0.75,
                  maxy=0,defpar=TRUE,inpch=16,incol=1){
  if (defpar) {
    par(mfrow = c(1,1), mai = c(0.45,0.45,0.1,0.05),oma = c(0,0,0,0))
    par(cex = cex, mgp = c(1.35, 0.35, 0), font.axis = usefont,
        font = usefont, font.lab = usefont)
  }
  if (maxy > 0) ymax <- maxy  else ymax <- getmax(y)
  if (min(y,na.rm=TRUE) < 0.0) ymin <- getmin(y) else ymin <- 0.0
  addline <- FALSE
  plot(x,y,type=type,pch=inpch,lwd=2,col=incol,ylim=c(ymin,ymax),yaxs="i",
       ylab=ylabel,xlab=xlabel,cex=cex,panel.first=grid())
} # end of plot1


#' @title plotprep: sets up a window and the par values for plotting
#'
#' @description plotprep: sets up a window and the par values for plots.
#'    This is simply a utility function to save typing the standard syntax.
#'    Some of the defaults can be changed. Typing the name without () will
#'    provide a template for modification. If different par values are 
#'    wanted then just include a par statement after plotprep()
#'   
#' @param width defaults to 6 inches = 15.24cm - width of plot
#' @param height defaults to 3 inches = 7.62cm - height of plot
#' @param usefont default=7 (bold Times) 1=sans serif, 2=sans serif bold
#' @param cex default=0.75, the size of font used for text within the plots
#' @param newdev reuse a previously defined graphics device or make a new 
#'     one; default=TRUE
#' @param filename default="" ie do not save to a filename. If a filename 
#'     is defined it creates that file as a png file with resolution resol
#' @param resol resolution of the png file if one is defined, default=300
#' 
#' @return sets up a graphics device, if needed and sets the default 
#'     plotting par values. This changes the current plotting options! 
#' @export plotprep
#' 
#' @examples
#' \dontrun{
#'  x <- rnorm(1000,mean=0,sd=1.0)
#'  plotprep()
#'  hist(x,breaks=30,main="",col=2)
#'  plotprep(width=6,height=5)
#'  par(mfrow = c(2,1))
#'  hist(x,breaks=20,main="",col=2)
#'  hist(x,breaks=30,main="",col=3)
#' }
plotprep <- function(width=6,height=3.6,usefont=7,cex=0.75,
                     newdev=TRUE,filename="",resol=300) {
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
  par(mfrow = c(1,1),mai=c(0.45,0.45,0.1,0.05), oma=c(0,0,0,0))
  par(cex=cex,mgp=c(1.35,0.35,0),font.axis=usefont,font=usefont, 
      font.lab=usefont)
  if (lenfile > 0) 
      cat("\n Remember to place 'graphics.off()' after plot \n")
} # end of plot_prep



#' @title plotprofile simplifies plotting single likelihood profiles
#' 
#' @description plotprofile simplifies plotting out the likelihood 
#'     profiles of single parameters or variables. It is necessary to 
#'     pass the function the output from the profile calculations, 
#'     identifying the variable name against which to plot the 
#'     likelihood. Identifying the name of the -ve log-likelihood 
#'     column. Facilities are provided for defining the x and y axis 
#'     labels
#'
#' @param prof the results from te elikelihood profile calculations. 
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
#'     defaults to TRUE, so only one plot will be produced. If part of 
#'     a multiple plot define the formatting before calling and set this
#'     to FALSE
#'
#' @return nothing but this does generate a plot.
#' @export
#'
#' @examples
#' \dontrun{
#'  data(abdat)
#'  fish <- abdat$fish
#'  rval <- seq(0.325,0.45,0.001)
#'  ntrial <- length(rval)
#'  columns <- c("r","K","Binit","sigma","-veLL")
#'  result <- matrix(0,nrow=ntrial,ncol=length(columns),
#'                   dimnames=list(rval,columns))
#'  bestest <- c(r= 0.32,K=11000,Binit=4000,sigma=0.05) 
#'  for (i in 1:ntrial) {  #i <- 1
#'    param <- log(c(rval[i],bestest[2:4])) 
#'    parinit <- param    
#'    bestmodP <- nlm(f=negLLP,p=param,funk=simpspmP,initpar=parinit,
#'                   indat=fish,logobs=log(fish$cpue),notfixed=c(2:4),
#'                   typsize=magnitude(param),iterlim=1000)
#'    bestest <- exp(bestmodP$estimate)
#'    result[i,] <- c(bestest,bestmodP$minimum)
#'  }
#'  plotprofile(result,var="r",defpar=TRUE)
#' }
plotprofile <- function(prof,var,digit=c(3,3,3),xlabel=var,
                         ylabel="-ve Log-Likelihood",like="-veLL",
                         defpar=TRUE) {
  plot1(prof[,var],prof[,like],xlabel=xlabel,ylabel=ylabel,defpar=defpar)
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
} # end of plotpreofile