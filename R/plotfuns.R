

#' @title addnorm - adds a normal distribution to a histogram of a data set.
#'
#' @description  addnorm - adds a normal distribution to a histogram of a data
#'    set. This is generally to be used to illustrate whether log-transformation
#'    normalizes a set of catch or cpue data.
#' @param inhist - is the output from a call to 'hist' (see examples)
#' @param xdata -  is the data that is being plotted in the histogram.
#' @param inc - defaults to a value of 0.01; is the fine grain increment used to
#'    define the normal curve. The histogram will be coarse grained relative to
#'    this.
#' @return a list with a vector of 'x' values and a vector of 'y' values (to be
#'    used to plot the fitted normal probability density function), and a vector
#'    used two called 'stats' containing the mean and sandard deviation of the
#'    input data
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
  ans <- list(x=x,y=(N*cw)*dnorm(x,avCE,sdCE),stats=c(avCE,sdCE,N))
  return(ans)
} # end of addnorm

#' @title addlnorm - estimates a log-normal distribution from output of hist.
#'
#' @description  addlnorm - estiamtes a log-normal distribution from output of
#'    a histogram of a data set.
#' @param inhist - is the output from a call to 'hist' (see examples)
#' @param xdata -  is the data that is being plotted in the histogram.
#' @param inc - defaults to a value of 0.01; is the fine grain increment used to
#'    define the normal curve. The histogram will be coarse grained relative to
#'    this.
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

#' @title inthist - a replacement for the hist function for use with integers
#'
#' @description inthist - a replacement for the hist function for use with
#'    integers because the ordinary function fails to count them correctly at
#'    the end. The function is designed for integers and if it is given real
#'    numbers it will issue a warning and then round all values before plotting.
#' @param x - the vector of integers to be counted and plotted OR a matrix of
#'     values in column 1 and counts in column 2
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
#' x <- trunc(runif(1000)*10) + 1
#' plotprep(width=6,height=4,plots=c(1,1))
#' inthist(x,col="grey",border=3,width=0.75,xlabel="Random Uniform",
#'         ylabel="Frequency")
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
    warning("Attempting to use 'inthist' with non-integers; Values now rounded \n")
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
    plot(values,outplot,type="n",xlim=c((xmin-(width*0.75)),(xmax+(width*0.75))),
         xaxs="r",ylim=c(0,ymax),yaxs="i",xlab="",ylab="",xaxt="n",
         panel.first = grid())
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
#'     settings then you can use either setplot() to get suitable syntax or,
#'     more simply, use parsyn() which only give a template for the par syntax
#' @param width defaults to 6 inches = 15.24cm - width of plot
#' @param height defaults to 3.6 inches = 9.144cm - height of plot
#' @param newdev reuse a previously defined graphics device or make a new one;
#'    defaults to TRUE
#' @return Checks for and sets up a graphics device and sets the default plotting
#'   par values. This changes the current plotting options!
#' @export
#' @examples
#' x <- rnorm(1000,mean=0,sd=1.0)
#' plotprep()
#' hist(x,breaks=30,main="",col=2)
newplot <- function(width=6,height=3.6,newdev=TRUE) {
  if  ((names(dev.cur()) != "null device") & (newdev)) suppressWarnings(dev.off())
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
#'
#' @return nothing but it changes teh base graphics par settings
#' @export
#'
#' @examples
#' \dontrun{
#' parset()
#' parsyn()
#' }
parset <- function(plots=c(1,1),cex=0.85,font=7) {
  par(mfrow=plots,mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0))
  par(cex=cex, mgp=c(1.35,0.35,0), font.axis=font,font=font,
      font.lab=font)
} # end of parset

#' @title parsyn types the standard syntax for the par command to the console
#'
#' @description parsyn types the standard syntax for the par command to the
#'     console so it can be copied and pasted into your own code and modified.
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
#' @param xlabel the label fot the x-axis, defaults to empty
#' @param ylabel the label fot the y-axis, defaults to empty
#' @param type the type of plot "l" is for line, the default, "p" is
#'     points. If you want both plot a line and add points afterwards.
#' @param usefont which font to use, defaults to 7 which is Times bold
#' @param cex the size of the fonts used. defaults to 0.85
#' @param maxy defaults to 0, which does nothing. If a value is given
#'     then this value is used rather than estimating from the input y
#' @param defpar if TRUE then plot1 will declare a par statement. If false it
#'     will expect one outside the function. In this way plot1 can be
#'     used when plotting multiple graphs, perhaps as mfrow=c(2,2)
#'
#' @return nothing but it does plot a graph and changes the par setting
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rnorm(20,mean=5,sd=1)
#' plot1(x,x,xlabel="x-values",ylabel="yvalues")
#' }
plot1 <- function(x,y,xlabel="",ylabel="",type="l",usefont=7,cex=0.85,
                  maxy=0,defpar=TRUE){
  if (defpar) {
    par(mfrow = c(1,1), mai = c(0.45,0.45,0.1,0.05),oma = c(0,0,0,0))
    par(cex = cex, mgp = c(1.35, 0.35, 0), font.axis = usefont,
        font = usefont, font.lab = usefont)
  }
  if (maxy > 0) ymax <- maxy  else ymax <- getmax(y)
  if (min(y,na.rm=TRUE) < 0.0) ymin <- getmin(y) else ymin <- 0.0
  addline <- FALSE
  plot(x,y,type=type,pch=16,lwd=2,ylim=c(ymin,ymax),yaxs="i",
       ylab=ylabel,xlab=xlabel,cex=cex,panel.first=grid())
} # end of plot1


#' @title plotprep: sets up a window and the par values for plotting
#'
#' @description plotprep: sets up a window and the par values for plots.
#'   This is simply a utility function to save typing the standard syntax.
#'   Some of the defaults can be changed. Typing the name without () will
#'   provide a template for modification. 
#' @param width defaults to 6 inches = 15.24cm - width of plot
#' @param height defaults to 3 inches = 7.62cm - height of plot
#' @param plots defaults to c(1,1), but arranges multiple plots. If used it may
#'    be necessary to print out this code and adjust the mai and oma variables
#' @param usefont default is 7 (bold Times); 1 = sans serif, 2 = sans serif bold
#' @param cex default is 0.85, the size of font used for text within the plots
#' @param xmtext default is TRUE; if plots is not c(1,1) this alters the mai and
#'    oma variables for the x-axis to allow for mtexting and avoid the x title
#' @param ymtext default is TRUE; if plots is not c(1,1) this alters the mai and
#'    oma variables for the y-axis to allow for mtexting and avoid the y title
#' @param newdev reuse a previously defined graphics device or make a new one;
#'    defaults to TRUE
#' @param rows defaults to TRUE, determines whether to use mfrow or mfcol
#' @param filename defaults to "" = do not save to a filename. If a filename is
#' @return Checks for and sets up a graphics device and sets the default plotting
#'   par values. This changes the current plotting options!
#' @export plotprep
#' @examples
#' x <- rnorm(1000,mean=0,sd=1.0)
#' plotprep()
#' hist(x,breaks=30,main="",col=2)
#' plotprep(width=6,height=5,plots=c(2,1))
#' hist(x,breaks=20,main="",col=2)
#' hist(x,breaks=30,main="",col=3)
plotprep <- function(width=6,height=3.6,plots=c(1,1),usefont=7,cex=0.85,
                     xmtext=TRUE,ymtext=TRUE,
                     newdev=TRUE,rows=TRUE,filename="") {
  if  ((names(dev.cur()) != "null device") & (newdev)) suppressWarnings(dev.off())
  lenfile <- nchar(filename)
  if (lenfile > 3) {
    end <- substr(filename,(lenfile-3),lenfile)
    if (end != ".png") filename <- paste0(filename,".png")
    png(filename=filename,width=width,height=height,units="in",res=300)
  } else {
    if (names(dev.cur()) %in% c("null device","RStudioGD"))
      dev.new(width=width,height=height,noRStudioGD = TRUE)
  }
  firstmai <- 0.45; secondmai <- 0.45; thirdmai <- 0.1
  firstoma <- 0.0; secondoma <- 0.0; thirdoma <- 0.0
  if (sum(plots) != 2) {
    if (xmtext) {
      firstmai <- 0.25
      thirdmai <- 0.05
      firstoma <- 1.0
      thirdoma <- 0.1
    }
    if (ymtext) {
      secondmai <- 0.25
      secondoma <- 1.0
    }
  }
  maival <- c(firstmai,secondmai,thirdmai,0.05)
  omaval <- c(firstoma,secondoma,thirdoma,0.0)
  if (rows) {
    par(mfrow = plots,mai=maival,oma=omaval)
  } else {
    par(mfcol = plots,mai=maival,oma=omaval)
  }
  par(cex=cex, mgp=c(1.35,0.35,0), font.axis=usefont,font=usefont,font.lab=usefont)
  if (lenfile > 0) cat("\n Remember to place 'graphics.off()' after the plot \n")
} # end of plot_prep



#' @title plotprofile1 simplifies plotting single likelihood profiles
#' 
#' @description plotprofile1 simplifies plotting out the likelihood 
#'     profiles of single parameters or variables. It is necessary to pass
#'     the function the output from the profile calculations, identifying
#'     the variable name against which to plot the likelihood. Identifying 
#'     the name of the -ve log-likelihood column. Facilities are provided
#'     for defining the x and y axis labels
#'
#' @param prof the results from teh likelihood profile calculations. This 
#'     matrix should include, as a minimum, the fixed variable of interest 
#'     and the matching -ve log-likelihood in named columns.
#' @param var the name of the variable of interest to identify the column
#'     in prof in which to find the vector of fixed values given.
#' @param digit this is a vector of three that determine by how much the
#'     round function limits the values printed of the 95% and mean at the 
#'     top of the plot.
#' @param xlabel the x-axis label, defaults to the name of the var
#' @param ylabel the y-axis label, defaults to -ve Log-Likelihood
#' @param like identifies the name of the column containing the -ve log-
#'     likelihood
#'
#' @return nothing but this does generate a plot.
#' @export
#'
#' @examples
#' \dontrun{
#' data(abdat)
#' fish <- abdat$fish
#' simpspmP <- function(pars, indat,initpar,notfixed=c(1:length(pars)),
#'                      schaefer=TRUE,depleted=TRUE,
#'                      year="year",cats="catch",index="cpue") { 
#'   nyrs <- length(indat[,year])
#'   biom <- numeric(nyrs+1)
#'   catch <- indat[,cats]
#'   param <- initpar
#'   param[notfixed] <- pars[notfixed] 
#'   ep <- exp(param) 
#'   biom[1] <- ep[2]  
#'   if (depleted) biom[1] <- ep[3] 
#'   if(schaefer) p <- 1 else p <- 1e-8
#'   for (yr in 1:nyrs) { 
#'     Bt <- biom[yr]  
#'     biom[yr+1] <- max(Bt + ((ep[1]/p)*Bt*(1-(Bt/ep[2])^p)-catch[yr]),10)
#'   }
#'   qval <- exp(mean(log(indat[,"cpue"]/biom[1:nyrs])))
#'   return(log(biom[1:nyrs] * qval))  
#' } 
#' negLLP <- function(pars, funk, indat, logobs, initpar=pars,
#'                    notfixed=c(1:length(pars))) {
#'   logpred <- funk(pars,indat,initpar,notfixed)
#'   LL <- -sum(dnorm(logobs,logpred,exp(tail(pars,1)),log=T))
#'   return(LL)
#' }
#' rval <- seq(0.325,0.45,0.001)
#' ntrial <- length(rval)
#' columns <- c("r","K","Binit","sigma","-veLL")
#' result <- matrix(0,nrow=ntrial,ncol=length(columns),
#'                  dimnames=list(rval,columns))
#' bestest <- c(r= 0.32,K=11000,Binit=4000,sigma=0.05) 
#' for (i in 1:ntrial) {  #i <- 1
#'   param <- log(c(rval[i],bestest[2:4])) 
#'   parinit <- param    
#'   bestmodP <- nlm(f=negLLP,p=param,funk=simpspmP,initpar=parinit,
#'                   indat=fish,logobs=log(fish$cpue),notfixed=c(2:4),
#'                   typsize=magnitude(param),iterlim=1000)
#'   bestest <- exp(bestmodP$estimate)
#'   result[i,] <- c(bestest,bestmodP$minimum)
#' }
#' minLL <- min(result[,"-veLL"])
#' head(result,20)  # now plot -veLL agsinst r
#' }
plotprofile1 <- function(prof,var,digit=c(3,3,3),xlabel=getname(var),
                         ylabel="-ve Log-Likelihood",like="-veLL") {
  plot1(prof[,var],prof[,like],xlabel=xlabel,
        ylabel=ylabel)
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
  mtext(label,side=3,outer=FALSE,line=-1.1,cex=1.0,font=7)
} # end of plotpreofile1