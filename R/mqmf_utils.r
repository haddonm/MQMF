


#' @title countones used in apply to count the number of ones in a vector
#'
#' @description countones used in apply to count the number of ones in a vector
#' @param invect vector of values
#' @return A single value of zero or the number of ones
#' @export countones
#' @examples
#' \dontrun{
#' x <- matrix(trunc(runif(20)*10),nrow=4,ncol=5)
#' print(x)
#' apply(x,1,countones)
#' }
countones <- function(invect) {
   pick <- which(invect == 1)
   return(length(pick))
}

#' @title countzeros used in apply to count the number of zeros in a vector
#'
#' @description countzeros used in apply to count the number of zeros in a vector
#' @param invect vector of values
#' @return A single value of zero or the number of zeros
#' @export countzeros
#' @examples
#' \dontrun{
#' x <- matrix(trunc(runif(20)*10),nrow=4,ncol=5)
#' print(x)
#' apply(x,1,countzeros)
#' }
countzeros <- function(invect) {
   pick <- which(invect == 0.0)
   return(length(pick))
}

#' @title countgtzero used in apply to count how many numbers are
#'     greater than zero in a vector
#'
#' @description countgtzero used in apply to count how many numbers are
#'     greater than zero in a vector
#' @param invect vector of values
#' @return A single integer counting how many numbers are > 0
#' @export
#' @examples
#' \dontrun{
#' x <- matrix(trunc(runif(20)*10),nrow=4,ncol=5)
#' print(x)
#' apply(x,1,countgtzero)
#' }
countgtzero <- function(invect) {
   pick <- which(invect > 0)
   return(length(pick))
}

#' @title countNAs used in apply to count the number of NAs in a vector
#'
#' @description countNAs used in apply to count the number of NAs in a vector
#' @param invect vector of values
#' @return A single value of zero or the number of NAs
#' @export countNAs
#' @examples
#' \dontrun{
#' x <- matrix(trunc(runif(20)*10),nrow=4,ncol=5)
#' diag(x) <- NA
#' print(x)
#' apply(x,1,countNAs)
#' }
countNAs <- function(invect) {
   pick <- which(is.na(invect))
   return(length(pick))
}

#' @title countgtOne used in apply to count the number > 1 in a vector
#'
#' @description countgtOne used in apply to count the number > 1 in a vector
#' @param invect vector of values
#' @return A single value of zero or the number of NAs
#' @export countgtOne
#' @examples
#' \dontrun{
#' x <- matrix(trunc(runif(20)*10),nrow=4,ncol=5)
#' print(x)
#' apply(x,1,countgtone)
#' }
countgtOne <- function(invect) {
   pick1 <- which(invect > 1.0)
   return(length(pick1)/length(invect))
}

#' @title facttonum converts a vector of numeric factors into numbers
#'
#' @description facttonum converts a vector of numeric factors into numbers.
#'     If the factors are not numeric then the outcome will be a series of NA.
#'     It is up to you to apply this function only to numeric factors. A warning
#'     will be thrown if the resulting output vector contains NAs
#'
#' @param invect the vector of numeric factors to be converted back to numbers
#'
#' @return an output vector of numbers instead of the input factors
#' @export
#'
#' @examples
#' \dontrun{
#'  DepCat <- as.factor(rep(seq(100,600,100),2)); DepCat
#'  5 * DepCat[3]
#'  as.numeric(levels(DepCat))  # #only converts the levels not the replicates
#'  DepCat <- facttonum(DepCat)
#'  5 * DepCat[3]
#'  x <- factor(letters)
#'  facttonum(x)
#' }
facttonum <- function(invect){
  if (class(invect) == "factor") {
    outvect <- suppressWarnings(as.numeric(levels(invect))[invect])
  }
  if (class(invect) == "numeric") outvect <- invect
  if (any(is.na(outvect)))
    warning("NAs produced, your input vector may have non-numbers present \n")
  return(outvect)
} # end of facttonum

#' @title freqMean calculates the mean and stdev of count data
#'
#' @description freqMean calculates the mean and stdev of count data
#'     it requires both the values and their associated counts and
#'     return a vector of two numbers.
#'
#' @param values the values for which there are counts
#' @param infreqs the counts for each of the values empty cells can be
#'     either 0 or NA
#'
#' @return a vector containing the mean and st.dev.
#' @export
#'
#' @examples
#' \dontrun{
#' vals <- c(1,2,3,4,5)
#' counts <- c(3,NA,7,4,2)
#' freqMean(vals,counts)  # should give 3.125 and 1.258306
#' }
freqMean <- function(values,infreqs) {
   N <- length(values)
   if (N != length(infreqs)) {
      cat("vectors have different lengths \n")
      ans <- c(NA,NA)
      names(ans) <- c("mean","stdev")
   } else {
      nobs <- sum(infreqs,na.rm=T)
      sumX <- sum(values * infreqs,na.rm=T)
      av <- sumX/nobs
      if (nobs > 1) {
         sumX2 <- sum(values * values * infreqs,na.rm=T)
         stdev <- sqrt((sumX2 - (sumX * sumX)/nobs)/(nobs-1))
      } else { stdev <- NA
      }
      ans <- c(av,stdev)
      names(ans) <- c("mean","stdev")
   }
   return(ans)
} # end of freq_Mean

#' @title getmin generates the lower bound for a plot
#'
#' @description getmin generates a lower bound for a plot where it is unknown
#'     whether the minumum is less than zero of not. If less than 0 then
#'     multiplying by the default mult of 1.05 works well but if the outcome if
#'     > 0 then the multiplier needs to be adjusted appropriately so the minimum
#'     is slightly lower than the minimum of the data
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
#' @description getmax generates an upper bound for a plot where it is unknown
#'     whether the maximum is greater than zero of not. If > 0 then
#'     multiplying by the default mult of 1.05 works well but if the outcome if
#'     < 0 then the multiplier needs to be adjusted appropriately so the maximum
#'     is slightly higher than the maximum of the data
#'
#' @param x the vector of data to be tested for its maximum
#' @param mult the multiplier for both ends, defaults to 1.05 (=0.95 if < 0)
#'
#' @return a suitable upper bound for a plot if required
#' @export
#'
#' @examples
#' vect <- rnorm(10,mean=0,sd=2)
#' sort(vect,decreasing=TRUE)
#' getmax(vect,mult=1.0)
#' vect <- rnorm(10,mean = -5,sd = 1.5)
#' sort(vect,decreasing=TRUE)
#' getmax(vect,mult=1.0)
getmax <- function(x,mult=1.05) {
   ymax <- max(x,na.rm=TRUE)
   if (ymax > 0) {
      ymax <- ymax * mult
   } else {
      ymax <- ymax * (2 - mult)
   }
   return(ymax)
} # end of getmax

#' @title getname returns the name of a variable as character
#'
#' @description getname runs 'deparse(substitute(x))' to get the
#'     name of the input variable. Saves remembering the syntax
#'
#' @param x any variable whose name is wanted as a character string
#'
#' @return a character string with the name of input variable
#' @export
#'
#' @examples
#' a_variable <- c(1,2,3,4,5,6,7,8)
#' getname(a_variable)
getname <- function(x) {
   return((deparse(substitute(x))))
}

#' @title getseed generates a random number seed
#' 
#' @description getseed generates a seed for use within set.seed. 
#'     It produces up to a 6 digit integer from the Sys.time. This
#'     Initially, at the start of a session there is no seed; a new one 
#'     is created from the current time and the process ID when one is 
#'     first required. Here, in getseed, we do not use the process ID so 
#'     the process is not identical but this at least allows the 
#'     set.seed value to be stored should the need to repeat a set of 
#'     simulations arise. The process generates up to a six digit number
#'     it then randomly reorders those digits and that becomes the seed.
#'     That way, if you were to call getseed in quick succession the
#'     seeds generated should differ even when they are generated close
#'     together in time.
#'
#' @return  a 6 digits integer
#' @export
#'
#' @examples
#' \dontrun{
#' seed <- getseed()
#' set.seed(seed)
#' rnorm(5)
#' set.seed(seed)
#' rnorm(5)
#' }
getseed <- function() {
  begin <- as.integer(Sys.time())
  pickseed <- as.character(begin %% 1e6)
  nc <- nchar(pickseed)
  pseed <- unlist(strsplit(pickseed,split=character(0)))
  pseed <- sample(pseed,nc)
  newseed <- NULL
  for (i in 1:nc) newseed <- paste0(newseed,pseed[i])
  newseed <- as.numeric(newseed)
  return(newseed)
}

#' @title getsingle extracts a single number from an input line of characters
#'
#' @description getsingle splits up a text line and translates the first non-
#'     empty character string into a number.
#'
#' @param inline the line of text, usually taken after using readLines
#' @param sep the separator used to divide the numbers from descriptive text.
#'     defaults to a comma.
#'
#' @return a single number
#' @export
#'
#' @examples
#' \dontrun{
#' x <- "12.3 , this is a number"
#' y <- "21.3 # 22.3 # here are two numbers"
#' getsingle(x)
#' getsingle(y,sep="#")
#' }
getsingle <- function(inline,sep=",") {  # inline=dat[41]
  tmp <- unlist(strsplit(inline,sep))
  tmp <- gsub(" ","",tmp)
  tmp <- tmp[nchar(tmp) > 0]
  return(as.numeric(tmp[1]))
}

#' @title gettime calculates time in seconds passed each day
#' 
#' @description gettime is a function designed to facilitate the measurement
#'     of time between intervals within R software that are expected to
#'     take a maximum of hours. It calculates the time as seconds elapsed 
#'     from the start of each day. As long as the timing of events does not
#'     pass from one day to the next accurate results will be generated.
#'
#' @return the time in seconds from the start of a day
#' @export
#'
#' @examples
#' \dontrun{
#'   begin <- gettime()
#'   for (i in 1:1e6) sqrt(i)
#'   finish <- gettime()
#'   print(finish - begin)
#' }
gettime <- function() {
  tim <- unlist(as.POSIXlt(Sys.time()))
  hr <- as.numeric(tim["hour"])*3600
  min <- as.numeric(tim["min"])*60
  sec <- as.numeric(tim["sec"])
  return(hr+min+sec)
} # end of gettime

#' @title getvector  extracts a vector of numbers from a line of characters
#'
#' @description getvector when reading in a csv file using readLines,
#'     getvector extarcts a line of numbers from a specified line within
#'     the readLine object.This function works out how many numbers there
#'     are. If you wish to add a comment at the end of a vector of numbers
#'     it must be separated from tehm by the separator. e.g. a comma
#' @param indat the readLines object
#' @param locate the line number from which to extract the numbers
#' @param sep the separator between numbers, defaults to ","
#'
#' @return a vector of numbers
#' @export
#'
#' @examples
#' \dontrun{
#' x <- "12.3, 15.1, 8.7,10.3,  # this is a vector of numbers"
#' y <- "21.3 # 22.3 # 8.7 # 10.3 # here are four numbers"
#' getvector(x)
#' getvector(y,sep="#")
#' }
getvector <- function(indat,locate,sep=",") { # indat=dat; locate=pick+2;sep=","
  vect <- indat[locate]
  if (length(grep("\t",vect) > 0)) vect <- gsub("\t",sep,vect)
  vect <- unlist(strsplit(vect,sep))
  vect <- gsub(" ","",vect)
  vect1 <- vect
  vect <- suppressWarnings(as.numeric(vect))
  vect <- vect[nchar(vect) > 0]
  if (!any(vect1 == "NA")) {
    vect <- vect[!is.na(vect)]
  }
  return(vect)
}

#' @title halftable halves the height of a tall narrow data.frame
#'
#' @description halftable would be used when printing a table using kable
#'     from knitr where one of the columns was Year. The objective would be to
#'     split the table in half taking the bottom half and attaching it on
#'     the right hand side of the top half. The year column would act as the
#'     index.
#'
#' @param inmat the data.frame to be subdivided
#' @param yearcol the column name of the year field default="year"
#' @param subdiv the number of times the data.frame should be subdivided;
#'     the default is 3 but the numbers can only be 2 or 3.
#'
#' @return a data.frame half the height and double the width of the original
#' @export
#'
#' @examples
#' \dontrun{
#' x <- as.data.frame(matrix(runif(80),nrow=20,ncol=4))
#' x[,1] <- 1986:2005
#' x[,4] <- paste0("text",1:20)
#' halftable(x,yearcol="V1",subdiv=2)
#' halftable(x[,c(1,2,4)],yearcol="V1")
#' x1 <- rbind(x,x[1,])
#' x1[21,"V1"] <- 2006
#' halftable(x1,yearcol="V1",subdiv=3)
#' }
halftable <- function(inmat,yearcol="year",subdiv=3) {
   if (!(subdiv %in% c(2,3))) stop("\n subdiv must be 2 or 3 \n")
   numrow <- dim(inmat)[1]
   numcol <- dim(inmat)[2]
   extra <- rep(NA,numcol)
   if ((numrow %% subdiv) == 0) {
      newnr <- numrow/subdiv
      incomplete <- FALSE
   } else {
      newnr <- trunc(numrow/subdiv) + 1
      incomplete <- TRUE
   }
   # years <- inmat[,yearcol]
   first <- inmat[1:newnr,]
   if (subdiv == 2) {
      second <- inmat[-c(1:newnr),]
      diff <- (nrow(first) - nrow(second))
      if (diff > 0) {
         numcol <- ncol(inmat)
         third <- rbind(second,extra)
      } else {
         third <- second
      }
   } else {
      second <- inmat[c(newnr+1):c(2*newnr),]
      first <- cbind(first,second)
      third <- inmat[-c(1:(2*newnr)),]
      diff <- nrow(first) - nrow(third)
      if (diff > 0) third <- rbind(third,extra)
      if (diff > 1) third <- rbind(third,extra)
   }
   outmat <- cbind(first,third)
   rownames(outmat) <- 1:newnr
   return(outmat)
} # end of halftable

#' @title incol is a utility to determine is a column is present in a matrix
#'
#' @description incol is a utility to determine whether a names columns is
#'     present in a given matrix or data.frame.
#'
#' @param incol the name of the column; defaults to "year" as an example
#' @param inmat the matrix or data.frame within which to search for incol
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
#' \dontrun{
#' test <- matrix(c(1,2,3,4),nrow=2,ncol=2,dimnames=list(1:2,c("year","Catch")))
#' print(test)
#' iscol("year",test)
#' iscol("Catch",test)
#' iscol("catch",test)
#' iscol("ages",test)
#' }
iscol <- function(incol="year",inmat) { # incol="ages"; inmat=dat
  if (length(grep(incol,colnames(inmat))) < 1) return(FALSE)
  else return(TRUE)
}

#' @title magnitude returns the magnitude of numbers
#'
#' @description magnitude is useful when using an
#'     optimizer such as optim, which uses a parscale parameter.
#'     magnitude can determine the respective parscale value for each
#'     parameter value.
#'
#' @param x the vector of numbers (parameters) whose magnitudes are
#'     needed
#'
#' @return a vector of magnitudes
#' @export
#'
#' @examples
#' \dontrun{
#'   x <- c(0,0.03,0.3,3,30,300,3000)
#'   magnitude(x)
#' }
magnitude <- function(x) {
   return(10^(floor(log10(abs(x)))))
}

#' @title makelabel generates a label from text and values
#'
#' @description It is common to want a label with text and a series of
#'     values. But paste and paste0 cycles the text and the values. To
#'     avoid this makelabel first combines the values as text and then
#'     adds the input text to the front of the values
#'
#' @param txt the input text for the label
#' @param vect the series of values to be included in the label
#' @param sep the separator for the components; defaults to  _
#' @param sigdig how many significant digits for the values; default = 3
#'
#' @return a character string made up of text and values
#' @export
#'
#' @examples
#' \dontrun{
#' pars <- c(18.3319532,33.7935124,3.0378107,6.0194465,0.5815360,0.4270468)
#' makelabel("Cohort1",pars[c(1,3,5)],sep="__")
#' }
makelabel <- function(txt,vect,sep="_",sigdig=3) {
   tmp <- NULL
   nnum <- length(vect)
   for (i in 1:nnum) tmp <- paste(tmp,round(vect[i],sigdig),sep=sep)
   label <- paste0(txt,tmp)
   return(label)
} # end of makelabel

#' @title outfit tidy print of output from optim, nlminb, or nlm
#'
#' @description outfit takes in the output list from either optim,
#'     nlminb, or nlm and prints it more tidily to the console, In the
#'     case of nlm it also prints the conclusion regarding the
#'     solution
#'
#' @param inopt the list object output by nlm, nlminb, or optim
#' @param backtran a logical default = TRUE If TRUE it assumes
#'     that the parameters have been log-transformed for stability
#'     and need back-transforming
#' @param digits the number of digits to round the backtransformed 
#'     parameters. defaults to 5.
#' @param title character string used to label the output if desired,
#'     default = empty charcter string
#'
#' @return nothing but it does print the list to the console tidily
#' @export
#'
#' @examples
#' \dontrun{
#'  x <- 1:10  # generate power function data from c(2,2) + random
#'  y <- c(2.07,8.2,19.28,40.4,37.8,64.68,100.2,129.11,151.77,218.94)
#'  alldat <- cbind(x=x,y=y)
#'  pow <- function(par,x) return(par[1] * x ^ par[2])
#'  ssq <- function(par,indat) {
#'     return(sum((indat[,"y"] - pow(par,indat[,"x"]))^2))
#'  }
#'  par=c(2,2)
#'  best <- nlm(f=ssq,p=par,typsize=magnitude(par),indat=alldat)
#'  outfit(best)  # a=1.3134 and b=2.2029 -veLL=571.5804
#' }
outfit <- function(inopt,backtran=TRUE,digits=5,title=""){
   nlmcode <- c("relative gradient close to zero, probably solution.",
                "repeated iterates in tolerance, probably solution.",
                paste0("nothing lower than estimate.",
                       "Either ~local min or steptol too small."),
                "iteration limit exceeded.",
                paste0("stepmax exceeded ,5 times. Unbounded or ",
                       "asymptotic below or stepmax too small."))
   if (length(grep("value",names(inopt))) > 0) { # optim
      cat("optim solution: ", title,"\n")
      cat("minimum     : ",inopt$value,"\n")
      cat("iterations  : ",inopt$counts," iterations, gradient\n")
      cat("code        : ",inopt$convergence,"\n")
      if (backtran) {
        ans <- cbind(par=inopt$par,transpar=round(exp(inopt$par),digits))
      } else {
        ans <- t(inopt$par)
      }
      rownames(ans) <- 1:length(inopt$par)
      print(ans)
      cat("message     : ",inopt$message,"\n")
   } # end of optim
   if (length(grep("minimum",names(inopt))) > 0) {  # nlm - preferred
      cat("nlm solution: ", title,"\n")
      cat("minimum     : ",inopt$minimum,"\n")
      cat("iterations  : ",inopt$iterations,"\n")
      cat("code        : ",inopt$code,"  ",nlmcode[inopt$code],"\n")
      if (backtran) {
         ans <- cbind(par=inopt$estimate,gradient=inopt$gradient,
                      transpar=round(exp(inopt$estimate),digits))
         } else {
         ans <- cbind(par=inopt$estimate,gradient=inopt$gradient)
      }
      rownames(ans) <- 1:length(inopt$estimate)
      print(ans)
   } # end of nlm
   if (length(grep("objective",names(inopt))) > 0) {
      cat("nlminb solution: ", title,"\n")   # nlminb seems to be deprecated
      cat("par        : ",inopt$par,"\n")
      cat("minimum    : ",inopt$objective,"\n")
      cat("iterations : ",inopt$iterations,"\n")
      cat("code       : ",inopt$evaluations," iterations, gradient\n")
      cat("message    : ",inopt$message,"\n")
   }
   if (length(grep("hessian",names(inopt))) > 0) {
      cat("hessian     : \n")
      print(inopt$hessian)
   }
} # end of outfit

#' @title penalty0 enables the adding of a large penalty as one approaches 0.0
#'
#' @description penalty0 allows for the option of adding a large penalty as
#'     a parameter approaches 0.0 . See negLL1 for example code that 
#'     contains such a parameter. For example, when
#'     fitting an spm sometimes the optimal mathematical model fit can occur
#'     by depressing the r value to 0 or even go negative. Input values 
#'     < 0.006 begin to generate large values as one goes smaller. The
#'     examples below illustrate this.
#'
#' @param x the parameter value that potentially incurs a penalty
#'
#' @return a single value as a penalty to be added to a Log-Likelihood or SSQ
#' @export
#'
#' @examples
#' \dontrun{
#'   penalty0(0.5)
#'   penalty0(0.1)
#'   penalty0(0.01)
#'   penalty0(0.005)
#' }
penalty0 <- function(x){
  ans <- 100*exp(-1000*x)
  return(ans)
} # end of penalty0

#' @title plotfishM plots the catch and optionally the cpue from fish
#'
#' @description plotfishM uses the matrix of fishery data used in the
#'     simpleSA standard data format. It requires the matrix or data.frame
#'     to contain the columns 'year', 'catch', and optionally 'cpue'.
#'
#' @param fish the matrix or data.frame containing year, catch, and cpue.
#' @param glb the list of biologicals potentially containing the spsname
#' @param ce a logical parameter determining whether to plot the cpue or not.
#'     the default = TRUE
#' @param title determines whether or not the spsname is printed at the top
#'     of the plot. Default = TRUE but for a more formal publication it
#'     might need to be set to FALSE, which also reallocates the room
#'     given to the title to the plot.
#' @param fnt the font used in the plot and axes.
#' @param both plot both the catches and the CPUE
#' @param maxy a vector of two zeros. These define the maximum y-axis value
#'     for each plot. If it remains zero then getmax will be used to find
#'     the maximum.
#'
#' @return plots a graph but returns nothing to the console
#' @export
#'
#' @examples
#' \dontrun{
#'   data(dataspm)
#'   plotfishM(dataspm$fish,dataspm$glb,ce=TRUE)
#' }   # fish=fish; glb=glb; ce=TRUE; title=FALSE; fnt=7;both=TRUE;maxy=c(0,124.5)
plotfishM <- function(fish,glb,ce=TRUE,title=TRUE,fnt=7,both=TRUE,
                      maxy=c(0,0)) {
  colnames(fish) <- tolower(colnames(fish))
  rows <- 1
  if (ce) rows <- 2
  yrs <- fish[,"year"]
  par(mfrow=c(rows,1),mai=c(0.5,0.45,0.025,0.05))
  if (title) {
    par(oma=c(0.0,0,1.0,0.0))
  } else {
    par(oma=c(0.0,0,0.0,0.0))
  }
  par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=fnt,font=fnt,font.lab=fnt)
  ymax <- maxy[1]
  if (maxy[1] == 0) ymax <- getmax(fish[,"catch"])
  plot(yrs,fish[,"catch"],type="l",lwd=2,ylab="Catch",xlab="Year",
       ylim=c(0,ymax),yaxs="i",panel.first = grid())
  if (title)
    mtext(glb$spsname,side=3,cex=1.0,line=0,font=fnt,outer=TRUE)
  if (ce) {
    pickI <- grep("cpue",colnames(fish))
    nce <- length(pickI)
    ymax <- maxy[2]
    if (maxy[2] == 0) ymax <- getmax(fish[,pickI])
    plot(yrs,fish[,pickI[1]],type="l",lwd=2,ylab="CPUE",xlab="Year",
         ylim=c(0,ymax),yaxs="i",panel.first = grid())
    if (both) points(yrs,fish[,pickI[1]],cex=1.0,pch=16)
    if (nce > 1) {
      for (i in 2:nce) {
        lines(yrs,fish[,pickI[i]],lwd=2,col=i)
        if (both) points(yrs,fish[,pickI[i]],col=i,cex=1.0,pch=16)
      }
    }
  }
} # end of plotfishM


#' @title printV returns a vector cbinded to 1:length(invect)
#'
#' @description printV takes an input vector and generates another vector of
#'     numbers 1:length(invect) which it cbinds to itself. This is primarily
#'     useful when trying to print out a vector which can be clumsy to read when
#'     print across the screen. applying printV leads to a single vector being
#'     printed down the screen
#'
#' @param invect the input vector to be more easily visualized, this can be
#'     numbers, characters, or logical. If logical the TRUE and FALSE are
#'     converted to 1's and 0's
#' @param label the column labels for the vector, default is 'index' and 'value'
#'
#' @return a dataframe containing the vector 1:length(invect), and invect.
#' @export
#'
#' @examples
#' vec <- rnorm(10,mean=20,sd=2)
#' printV(vec)
#' vec <- letters
#' printV(vec)
#' vec <- c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE)
#' printV(vec,label=c("index","logicstate"))
printV <- function(invect,label=c("index","value")) {
   n <- length(invect)
   outvect <- as.data.frame(cbind(1:n,invect))
   colnames(outvect) <- label
   return(outvect)
} # end of print_V

#' @title properties - used to check a data.frame before standardization
#'
#' @description properties - used to check a data.frame before
#'     standardization
#' @param indat the data.frame containing the data fields to be used
#'     in the subsequent standardization. It tabulates the number of
#'     NAs and the number of unique values for each variable and finds
#'     the minimum and maximum of the numeric variables
#' @param dimout determines whether or noth the dimensions of the data.frame
#'     are printed to the screen or not; defaults to FALSE
#' @return a data.frame with the rows being each variable from the input
#'     input data.frame and the columns being the number of NAs, the
#'     number of unique values, and minimum and maximum (where possible).
#' @export properties
#' @examples
#' \dontrun{
#' data(abdat)
#' properties(abdat$fish)
#' }
properties <- function(indat,dimout=FALSE) {
  if(dimout) print(dim(indat))
  isna <- sapply(indat,function(x) sum(is.na(x)))
  uniques <- sapply(indat,function(x) length(unique(x)))
  clas <- sapply(indat,class)
  numbers <- c("integer","numeric")
  pick <- which(clas %in% numbers)
  minimum <- numeric(length(uniques))
  maximum <- minimum
  minimum[pick] <- sapply(indat[,pick],min,na.rm=TRUE)
  maximum[pick] <- sapply(indat[,pick],max,na.rm=TRUE)
  index <- 1:length(isna)
  props <- as.data.frame(cbind(index,isna,uniques,clas,minimum,
                               maximum,t(indat[1,])))
  colnames(props) <- c("Index","isNA","Unique","Class","Min",
                       "Max","Example")
  return(props)
} # end of properties


#' @title readdata reads in a standard format data file
#'
#' @description readdata reads in a standard format data file. An example of the
#'     standard format is generated by using the function dataTemplate, which
#'     generates an example datafile which can be used to demonstrate the
#'     methods present in FMR, or can be used as a template to edit and
#'     input a new or different dataset.
#'
#' @param filename the filename (including the full path if required) containing
#'     the data in the standard format.
#' @param property does the data include a table of length-at-age, maturity-at-
#'     age, weight-at-age, fecundity-at-age, and that kind of thing (see the
#'     standard format for a list of data.)
#' @param verbose Default = TRUE, which prints out details as data is read in.
#'
#' @return a list of three objects, fish includes the Year, Catch, CPUE, and SE
#'     of the CPUE, glb containing an array of biological properties that can
#'     be global, and props containing age, laa, waa, maa, sela.
#' @export
#'
#' @examples
#' \dontrun{
#' dataTemplate(filename="test.csv", title="A test of the functions")
#' ans <- readdata("test.csv",property=FALSE)
#' str(ans)
#' }             #
readdata <- function(filename,property=FALSE,verbose=TRUE) {
  #  filename=filename; property=FALSE; verbose=TRUE
  dat <- readLines(filename)
  spsname <- scan(file=filename,skip=0,what=character(),nlines = 1,quiet=TRUE)
  spsname <- removeEmpty(gsub(",","",spsname))
  pick <- grep("RESILIENCE",dat)[1]
  resilience <- scan(file=filename,skip=pick,what=character(),nlines = 1,quiet=TRUE)
  resilience <- removeEmpty(gsub(",","",resilience))
  pick <- grep("NYRS",dat)[1] + 1
  nyrs <- getsingle(dat[pick])
  if (verbose) cat(spsname,"\n\n resilience = ",resilience,
                   "  Num Years = ",nyrs,"\n\n")
  # get the fishery data into as many columns as required
  pick <- grep("YEARS",dat)[1]
  columns <- tolower(removeEmpty(unlist(strsplit(dat[pick],","))))
  numcols <- length(columns)
  columns <- c("year",columns[2:numcols])
  skips <- pick:(pick+nyrs-1)
  fish <- as.data.frame(matrix(NA,nrow=nyrs,ncol=length(columns),
                               dimnames=list(1:nyrs,columns)))
  for (i in 1:nyrs) { # i = 1
    fish[i,] <- scan(file=filename,skip=skips[i],sep=",",nlines = 1,quiet=TRUE)[1:numcols]
  }
  if (verbose) {
    cat("fish \n\n")
    print(head(fish,10))
    cat("\n")
  }
  glb=list(resilience=resilience,spsname=spsname)
  pick <- grep("BIOLOGY",dat)[1] + 1
  if (is.na(pick)) {
    props <- NULL
  } else {
    maxage <- getsingle(dat[pick])
    M <- getsingle(dat[pick+1])
    Linf <- getsingle(dat[pick+2])
    K <- getsingle(dat[pick+3])
    t0 <- getsingle(dat[pick+4])
    Waa <- getsingle(dat[pick+5])
    Wab <- getsingle(dat[pick+6])
    M50a <- getsingle(dat[pick+7])
    deltaM <- getsingle(dat[pick+8])
    sela50 <- getsingle(dat[pick+9])
    deltaS <- getsingle(dat[pick+10])
    steep <- getsingle(dat[pick+11])
    R0 <- getsingle(dat[pick+12])
    ages <- 0:maxage
    nages <- length(ages)
    glb <- list(maxage=maxage,M=M,
                Linf=Linf, K=K, t0=t0,
                Waa=Waa, Wab=Wab,
                M50a=M50a, deltaM=deltaM,
                steep=steep,R0=R0,
                sela50=sela50, deltaS=deltaS,
                resilience=resilience,
                nages=nages,ages=ages,nyrs=nyrs,spsname=spsname)
    columns <- c("age","laa","waa","maa","sela","feca")
    props <- as.data.frame(matrix(NA,nrow=nages,ncol=length(columns),
                                  dimnames=list(ages,columns)))
    if (property) {
      pick <- grep("PROPERTY",dat)[1] + 1
      for (i in 1:nages) {
        props[i,] <- getvector(dat,pick,sep=",")
        pick <- pick + 1
      }
    } else {
      # now calculate the properties
      laa <- vB(c(Linf,K,t0),ages)
      waa <- (Waa * laa ^ Wab)/1000
      maa <- logist(M50a,deltaM,ages)
      sela <- logist(sela50,deltaS,ages)
      feca <- sela * waa
      props <- as.data.frame(cbind(ages,laa,waa,maa,sela,feca))
    }
  }
  if (verbose) {
    cat("biology \n")
    print(unlist(glb))
    cat("\n properties \n")
    print(head(props,10))
  }
  #   dat <- readLines(filename)
  pick <- grep("AGE",dat)[1] + 1
  if (is.na(pick)) {
    agedata <- NULL
  } else {
    yrage <- getsingle(dat[pick])
    numsex <- getsingle(dat[(pick+1)])
    pickA <- grep("AGES",dat)[1]
    ages <- getvector(dat,pickA,sep=",")
    agemax <- max(ages)
    nage <- length(ages)
    naa <- matrix(0,nrow=(yrage*numsex),ncol=(nage+2))
    colnames(naa) <- c("year","sex",ages)
    for (i in 1:(yrage*numsex)) naa[i,] <- getvector(dat,(pickA+i),sep=",")
    rownames(naa) <- naa[,1]
    agedata <- list(yrage=yrage,ages=ages,agemax=agemax,nage=nage,naa=naa)
  }
  pick <- grep("LENGTH",dat)[1] + 1
  if (is.na(pick)) {
    lendata <- NULL
  } else {
    yrlen <- getsingle(dat[pick])
    numsexl <- getsingle(dat[(pick+1)])
    lengths <- getvector(dat,(pick+2),sep=",")
    maxlen <- max(lengths)
    nlength <- length(lengths)
    if (verbose) {
      cat("Number of years of Data: ",yrlen,"\n")
      cat("Number of sexes with Data: ",numsexl,"\n")
      cat("Length classes: ",lengths,"\n")
      cat("Number of length classes ",nlength,"\n")
    }
    nal <- matrix(0,nrow=(yrlen*numsexl),ncol=(nlength+2))
    colnames(nal) <- c("year","sex",lengths)
    pick <- pick+2
    for (i in 1:(yrlen*numsexl)) nal[i,] <- getvector(dat,(pick+i),sep=",")
    rownames(nal) <- nal[,1]
    
    lendata <- list(yrlen=yrlen,lengths=lengths,maxlen=maxlen,
                    nlength=nlength,nal=nal)
  }
  ans <- list(fish=fish,glb=glb,props=props,agedata=agedata,lendata=lendata)
  return(ans)
} # end of readdata

#' @title removeEmpty removes empty strings from a vector of strings
#'
#' @description removeEmpty removes empty strings from a vector of strings.
#'     Such spaces often created by spurious commas at the end of lines. It
#'     also removes strings made up only of spaces and removes spaces from
#'     inside of inidivdual chunks of text.
#'
#' @param invect a vector of input strings, possibly containing empty strings
#'
#' @return a possibly NULL vector of strings
#' @export
#'
#' @examples
#' \dontrun{
#' x <- c("1","","2","","   ","3"," ","4","","a string","end")
#' x
#' length(x)
#' length(removeEmpty(x))
#' removeEmpty(x)
#' }
removeEmpty <- function(invect) {
  tmp <- gsub(" ","",invect)
  tmp <- tmp[nchar(tmp) > 0]
  return(tmp)
}


#' @title quants used in apply to estimate quantiles across a vector
#'
#' @description quants used in 'apply' to estimate quantiles across a vector
#' @param invect vector of values
#' @return a vector of the c(0.025,0.05,0.5,0.95,0.975) quantiles
#' @export quants
#' @examples
#' cat("Example still be made /n")
quants <- function(invect) {
   ans <- quantile(invect,probs = c(0.025,0.05,0.5,0.95,0.975),na.rm=T)
   return(ans)
}


#' @title which.closest find the number closest to a given value
#'
#' @description which.closest finds either the number in a vector which is
#'     closest to the input value or its index value
#'
#' @param x the value to lookup
#' @param invect the vector in which to lookup the value x
#' @param index should the closest value be returned or its index; default=TRUE
#'
#' @return by default it returns the index in the vector of the value closest to
#'     the input  value
#' @export
#'
#' @examples
#' vals <- rnorm(100,mean=5,sd=2)
#' pick <- which.closest(5.0,vals,index=TRUE)
#' pick
#' vals[pick]
#' which.closest(5.0,vals,index=FALSE)
which.closest <- function(x,invect,index=T) {
   pick <- which.min(abs(invect-x))
   if (index) {
      return(pick)
   } else {
      return(invect[pick])
   }
} # end of which_.closest



#' @title '\%ni\%' identifies which element in x is NOT in y
#'
#' @param x a vector of elements which can be numeric or character
#' @param y a vector of elements which can be numeric or character
#' 
#' @export
`%ni%` <- function(x,y) {  
  !(x %in% y)
}
