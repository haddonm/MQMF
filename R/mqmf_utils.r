

#' @title aicbic returns the AIC and BIC for a given model
#' 
#' @description aicbic calculates and returns the AIC and BIC using the
#'     standard definitions. It defaults to assuming that negative log-
#'     likelihoods have been used in the model fitting, but provides the
#'     option of having used SSQ (set nLL to FALSE). If using SSQ it 
#'     uses Burnham and Anderson's (2002) definition but sets BIC to NA. 
#'     aicbic can recognize the outputs from optim, nlm, and nlminb.
#'
#' @param model the optimum model fitted by either optim, nlm, or nlminb
#' @param dat the data set used in the modelling or just n the number of
#'     observations; it can distinguish between them
#' @param nLL uses negative log-likelihood? default=TRUE
#'
#' @return a vector of three numbers, AIC first, then BIC, then negLL or 
#'     SSQ, depending on nLL, then number of parameters p
#' @export
#' 
#' @references Burnham, K.P. and D.R. Anderson (2002) \emph{Model Selection and Inference. A Practical Information-Theoretic Approach.} Second Edition Springer-Verlag, New York. 488 p.
#'
#' @examples
#' \dontrun{
#' data(blackisland); bi <- blackisland
#' param <- c(Linf=170.0,K=0.3,sigma=4.0)
#' modelvb <- nlm(f=negNLL,p=param,funk=fabens,observed=bi$dl,indat=bi,
#'                initL="l1",delT="dt") # could have used the defaults
#' aicbic(modelvb,blackisland)  # 588.3382 596.3846 291.1691   3
#' }
aicbic <- function(model,dat,nLL=TRUE) {  # model <- modelil; dat=bi
  if (class(dat) %in% c("matrix","data.frame")) {
    n <- nrow(dat)
  } else {
    n <- dat
    if (class(n) != "integer")
      stop("input dat is neither the data used or the number of obs \n")
  }
    outputs <- c("value","minimum","objective")
  pars <- c("par","estimate","par")
  pick <- match(names(model),outputs)
  pickmeth <- pick[which(pick > 0)]
  LL <- model[[outputs[pickmeth]]]
  npar <- length(model[[pars[pickmeth]]])
  if (nLL) {
     aic <- -2.0*(-LL) + (2 * npar)
     bic <- -2.0*(-LL) + (log(n) * npar)
     out <- c(aic=aic,bic=bic,negLL=LL,p=npar)
  } else {
     aic <- n * (log(LL/n)) + 2.0 * npar
     bic <- NA
     out <- c(aic=aic,bic=bic,ssq=LL,p=npar)
  }
  return(out)
} # end of aicbic

#' @title bracket bounds a value on the x-axis and y-axis
#' 
#' @description bracket scans through a series of predicted values for
#'     the location of a target value of the y-axis and returns the two
#'     y values that bracket the target, perhaps finding the values 
#'     closest to 0.5 in a vector between 0 and 1. It also returns the
#'     x-axis values that gave rise to the two values bracketing the 
#'     target, and finally returns the target. For example, imagine 
#'     generating the proportion mature for a given length of fish using 
#'     an equation for which there was no analytical solution to what 
#'     the value of the L50 or the inter-quartile distance was. Bracket
#'     can find the two lengths that generate proportions just below 0.5
#'     and just above. It does not matter if, by chance, the target is
#'     one of those y-axis values. 
#'
#' @param x the target predicted value of interest
#' @param yaxis the predicted values reflecting the xaxis values 
#' @param xaxis the series of values used to generate the predicted 
#'     values
#'
#' @seealso linter
#'
#' @return a vector of 5 values, left, right, bottom, top and target
#' @export
#'
#' @examples
#' \dontrun{
#'  L = seq(60,160,1)
#'  p=c(a=0.075,b=0.075,c=1.0,alpha=100)
#'  asym <- srug(p=p,sizeage=L)
#'  L25 <- linter(bracket(0.25,asym,L)) 
#'  L50 <- linter(bracket(0.5,asym,L)) 
#'  L75 <- linter(bracket(0.75,asym,L)) 
#'  ans <- c(L25,L50,L75,L50-L25,L75-L50)
#'  {cat("   L25    L50      L75   L50-L25 L75-L50 \n")
#'  cat(round(ans,4),"\n")} 
#' }
bracket <- function(x,yaxis,xaxis) {
  pick <- which(yaxis < x) 
  bot <- max(pick)
  bottom<- yaxis[bot]
  top <- yaxis[bot+1]
  ans <- c(xaxis[bot], xaxis[bot+1],bottom, top,x )
  names(ans) <- c("left","right","bottom","top","target")
  return(ans)
} # end of bracket

#' @title countgtone used in apply to count the number > 1 in a vector
#'
#' @description countgtone used in apply to count the number > 1 in a vector
#' 
#' @param invect vector of values
#' 
#' @return A single value of zero or the number of ones
#' @export
#' @examples
#' \dontrun{
#' x <- matrix(trunc(runif(20)*10),nrow=4,ncol=5)
#' print(x)
#' apply(x,1,countgtone)
#' }
countgtone <- function(invect) {
  pick1 <- which(invect > 1.0)
  return(length(pick1))
} # end of countgtone

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
#' apply(x,1,countgtzero) # count by rows
#' apply(x,2,countgtzero) # count by columns
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
#' @export
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

#' @title countones used in apply to count the number of ones in a vector
#'
#' @description countones used in apply to count the number of ones in a vector
#' @param invect vector of values
#' @return A single value of zero or the number of ones
#' @export
#' @examples
#' \dontrun{
#' x <- matrix(trunc(runif(20)*10),nrow=4,ncol=5)
#' print(x)
#' apply(x,1,countones)  # by rows
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
#' @export
#' @examples
#' \dontrun{
#' x <- matrix(trunc(runif(20)*10),nrow=4,ncol=5)
#' print(x)
#' apply(x,1,countzeros) # count by rows
#' apply(x,2,countzeros) # count by columns
#' }
countzeros <- function(invect) {
   pick <- which(invect == 0.0)
   return(length(pick))
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
#'  DepCat / 2.0
#'  x <- factor(letters) # don't be silly, characters are not numbers
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
#' @description getmax generates an upper bound for a plot where it 
#'     is unknown whether the maximum is greater than zero of not. 
#'     If > 0 then multiplying by the default mult of 1.05 works well 
#'     but if the outcome if < 0 then the multiplier needs to be 
#'     adjusted appropriately so the maximum is slightly higher than 
#'     the maximum of the data
#'
#' @param x the vector of data to be tested for its maximum
#' @param mult the multiplier for both ends, defaults to 1.05 (=0.95 if < 0)
#'
#' @return a suitable upper bound for a plot if required
#' @export
#'
#' @examples
#' \dontrun{
#'  vect <- rnorm(10,mean=0,sd=2)
#'  sort(vect,decreasing=TRUE)
#'  getmax(vect,mult=1.0)
#'  vect <- rnorm(10,mean = -5,sd = 1.5)
#'  sort(vect,decreasing=TRUE)
#'  getmax(vect,mult=1.0)
#' }   
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
#' @return  an integer up to 7 digits long
#' @export
#'
#' @examples
#' \dontrun{
#' useseed <- getseed()
#' set.seed(useseed)
#' rnorm(5)
#' set.seed(12345)
#' rnorm(5)
#' set.seed(useseed)
#' rnorm(5)
#' }
getseed <- function() {
  pickseed <- as.character(as.integer(Sys.time()))
  nc <- nchar(pickseed)
  if (nc > 7) pickseed <- substr(pickseed,(nc-6),nc)
  nc <- nchar(pickseed)  
  pseed <- unlist(strsplit(pickseed,split=character(0)))
  pseed <- sample(pseed,nc)
  newseed <- paste(pseed,collapse="")
  newseed <- as.numeric(newseed)
  return(newseed)
} # end of getseed

#' @title getsingle extracts one number from an input line of characters
#'
#' @description getsingle splits up a text line and translates the 
#'     first non-empty character string into a number.
#'
#' @param inline the line of text, usually taken after using readLines
#' @param sep the separator used to divide the numbers from descriptive 
#'     text, defaults to a comma.
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
#' getsingle(y) # be sure to get the separator correct
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

#' @title getvector extracts a vector of numbers from a line of characters
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
#' getvector(x)    # uses default separator
#' getvector(y,sep="#")
#' }
getvector <- function(indat,locate,sep=",") { 
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

#' @title likeratio conducts a likelihood ratio test
#' 
#' @description likeratio conducts a likelihood ratio test on two
#'     negative log-likelihoods. It produces the LR plus related 
#'     statistics detailing if a significant difference has been found.
#'     The order in which the log-likelihoods are entered does not 
#'     matter as that is checked for so a positive likelihood ratio
#'     is always generated.
#'
#' @param nLL1 the first -ve log-likelihood
#' @param nLL2 the second -ve log-likelihood 
#' @param df the number of degrees of freedom difference between the two
#'     model, a minimum of 1, which is the default.
#'
#' @return a vector of the likelihood ratio, the significance of any 
#'     difference, the minmum difference required for significance, and
#'     the degrees of freedom.
#' @export
#'
#' @examples
#' \dontrun{
#'   one <- 291.1691
#'   two <- 277.0122
#'   dof <- 1
#'   round(likeratio(one,two,dof),8)
#' }
likeratio <- function(nLL1,nLL2,df=1) {
  if (!(nLL1 < nLL2)) {
    tmp <- nLL1
    nLL1 <- nLL2
    nLL2 <- tmp
  }
  dif <- -2.0 * (nLL1 - nLL2)
  signif <- dchisq(dif,df) 
  mindiff <- qchisq(0.95,df)
  ans <- c(LR=dif,P=signif,mindif=mindiff,df=df)
  return(ans)
} # end of likeratio

#' @title linter finds a value in a series using its location in another
#' 
#' @description linter is a tool for linearly interpolating in a 2-D
#'     cartesian space to search out an unknown value between two known
#'     points on the x-axis, based on a known value between two known 
#'     points on the y-axis. This might be answering the question of
#'     what would be the length at 50 percent maturity for a curve with no 
#'     analytical solution. We could find two points in a series of
#'     proportion values on the y-axis that bracketed the 50 percent value 
#'     using the function bracket. They would be associated with the two 
#'     length values on the x-axis used to generate the predicted 
#'     proportion values. If we assume the various points in the 2-D 
#'     space to be approximated by linear relations then the location 
#'     between the two known x-axis length values corresponding to the 
#'     L50 would have the same ratio as the 50 percent value has to the two
#'     points on the y-axis. See the example for details. The input
#'     arguments include five values, left, right, bottom, top, and 
#'     target. So, left and right are sequential values on the x-axis, 
#'     bottom and top are the corresponding sequential values on the 
#'     y-axis, and target is the value we are looking for on the y-axis.
#'     
#'
#' @param pars a vector of 5 values, left, right, bottom, top and target
#'     
#' @seealso bracket
#'     
#' @return a single value being the x-axis value between left and right
#'     corresponding to the target on the x-axis 
#' @export
#'
#' @examples
#' \dontrun{
#'  L = seq(60,160,1)
#'  p=c(a=0.075,b=0.075,c=1.0,alpha=100)
#'  asym <- srug(p=p,sizeage=L)
#'  L25 <- linter(bracket(0.25,asym,L)) 
#'  L50 <- linter(bracket(0.5,asym,L)) 
#'  L75 <- linter(bracket(0.75,asym,L)) 
#'  ans <- c(L25,L50,L75,L50-L25,L75-L50)
#'  {cat("   L25    L50      L75   L50-L25 L75-L50 \n")
#'  cat(round(ans,4),"\n")} 
#' }
linter <- function(pars) {
  # pars= "left","right","bottom","top","target"
  rge <- pars[4] - pars[3]
  tarrge <- pars[5] - pars[3]
  ratio <- tarrge/rge  
  deprge <- pars[2] - pars[1]
  ans <- pars[1] + ratio * deprge  
  names(ans) <- "target"
  return(ans)
} # end of linter



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
#'     solution. It might be more effective to implement an S3 method.
#'
#' @param inopt the list object output by nlm, nlminb, or optim
#' @param backtran a logical default = TRUE If TRUE it assumes
#'     that the parameters have been log-transformed for stability
#'     and need back-transforming
#' @param digits the number of digits to round the backtransformed 
#'     parameters. defaults to 5.
#' @param title character string used to label the output if desired,
#'     default = empty charcter string
#' @param parnames default="" which means the estimated parameters
#'     will merely be numbered. If a vector of names is given 
#'     then this will be used instead, at least, for nlm and optim.
#'
#' @return nothing but it does print the list to the console tidily
#' @export
#'
#' @examples
#' \dontrun{
#'  x <- 1:10  # generate power function data from c(2,2) + random
#'  y <- c(2.07,8.2,19.28,40.4,37.8,64.68,100.2,129.11,151.77,218.94)
#'  alldat <- cbind(x=x,y=y)
#'  pow <- function(pars,x) return(pars[1] * x ^ pars[2])
#'  ssq <- function(pars,indat) {
#'     return(sum((indat[,"y"] - pow(pars,indat[,"x"]))^2))
#'  }  # fit a power curve using normal random errors
#'  pars <- c(2,2)
#'  best <- nlm(f=ssq,p=pars,typsize=magnitude(pars),indat=alldat)
#'  outfit(best,backtran=FALSE) #a=1.3134, b=2.2029 ssq=571.5804
#' }
outfit <- function(inopt,backtran=TRUE,digits=5,title="",
                   parnames=""){
#  inopt=bestvB; backtran = FALSE; digits=5; title=""; parnames=""
   nlmcode <- c("gradient close to 0, probably solution",
                ">1 iterates in tolerance, probably solution",
                "Either ~local min or steptol too small",
                "iteration limit exceeded",
                "stepmax exceeded ,5 times")
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
      if ((length(parnames) > 1) & (length(parnames) == length(inopt$par))) {
         rownames(ans) <- parnames
       } else {
         rownames(ans) <- 1:length(inopt$par)
      }
      print(ans)
      cat("message     : ",inopt$message,"\n")
   } # end of optim
   if (length(grep("minimum",names(inopt))) > 0) {  # nlm - preferred
      cat("nlm solution: ", title,"\n")
      cat("minimum     : ",inopt$minimum,"\n")
      cat("iterations  : ",inopt$iterations,"\n")
      cat("code        : ",inopt$code,nlmcode[inopt$code],"\n")
      if (backtran) {
         ans <- cbind(par=inopt$estimate,gradient=inopt$gradient,
                      transpar=round(exp(inopt$estimate),digits))
         } else {
         ans <- cbind(par=inopt$estimate,gradient=inopt$gradient)
      }
      if ((length(parnames) > 1) & 
          (length(parnames) == length(inopt$estimate))) {
        rownames(ans) <- parnames
      } else {
        rownames(ans) <- 1:length(inopt$estimate)
      }
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

#' @title penalty1 enables the adding of a large penalty as one approaches 1.0
#'
#' @description penalty1 allows for the option of adding a large penalty as
#'     a parameter approaches 1.0 and moves to become larger than 1. For 
#'     example, when fitting a surplus production model sometimes the 
#'     optimal mathematical model fit can occur by implying catches greater
#'     than available biomass, implying harvest rates > 1.0. By adding a 
#'     large penalty to such values and adding those to the likelihood 
#'     such strange outcomes can be avoided. This will accept a single
#'     value or a vector.
#'
#' @param x the parameter value that potentially incurs a penalty
#'
#' @return a single value as a penalty to be added to a Log-Likelihood or SSQ
#' @export
#'
#' @examples
#' \dontrun{
#'  x <- c(0.5,0.8,0.88,0.9,0.98,0.99,1.01)
#'  round(cbind(x,penalty1(x)),4)
#' }
penalty1 <- function(x){
  nl <- length(x)
  ans <- numeric(nl)
  pick <- which(x > 0.5)
  ans[pick] <- 100.0*(abs((1-abs(x[pick])-0.5))/0.5)^50
  return(ans)
} # end of penalty1

#' @title plotfishM plots the catch and optionally the cpue
#'
#' @description plotfishM uses a matrix of fishery data. It requires 
#'     the matrix or data.frame to contain the columns 'year', 
#'     'catch', and optionally 'cpue'.
#'
#' @param fish the matrix or data.frame containing year, catch, and cpue.
#' @param spsname the name of the species concerned
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
#' @param year the name of the column containing the years, default="year"
#' @param catch the name of the column containing catches, default="catch"
#' @param cpue the name of the column containing cpue, default="cpue"
#'
#' @return plots a graph but returns nothing to the console
#' @export
#'
#' @examples
#' \dontrun{
#'   data(dataspm)
#'   plotfishM(fish=dataspm,spsname="Pink Ling",ce=TRUE)
#' }
plotfishM <- function(fish,spsname="",ce=TRUE,title=TRUE,fnt=7,both=TRUE,
                      maxy=c(0,0),year="year",catch="catch",cpue="cpue") {
  colnames(fish) <- tolower(colnames(fish))
  rows <- 1
  if (ce) rows <- 2
  yrs <- fish[,year]
  par(mfrow=c(rows,1),mai=c(0.5,0.45,0.025,0.05))
  if (title) par(oma=c(0.0,0,1.0,0.0))
      else   par(oma=c(0.0,0,0.0,0.0))
  par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=fnt,font=fnt,font.lab=fnt)
  ymax <- maxy[1]
  if (maxy[1] == 0) ymax <- getmax(fish[,catch])
  plot(yrs,fish[,catch],type="l",lwd=2,ylab="Catch",xlab="Year",
       ylim=c(0,ymax),yaxs="i",panel.first = grid())
  if (title)
    mtext(spsname,side=3,cex=1.0,line=0,font=fnt,outer=TRUE)
  if (ce) {
    pickI <- grep(cpue,colnames(fish))
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

#' @title predfreq is used for modal analysis of count data
#' 
#' @description predfreq is used to calculate the expected 
#'     frequencies (counts) from a series of Normal distributions
#'     that are combined to describe observed counts from a 
#'     sample of the size distribution of a population,
#'     which may, or may not be made up of a number of cohorts.
#'     When used with a negative log-likelihood from a
#'     multinomial this can lead to estimates of the mean and
#'     standard deviation of the size of each cohort. Two 
#'     approaches are possible. An approximate method that 
#'     calculates the likelihood at the center of each size-class
#'     and a more precise analytical method that subtracts the
#'     cumulative probability of the bottom bound from each size-
#'     class from the upper bound. Usually the two are very 
#'     close.
#'
#' @param pars a vector of parameters, with the means first,
#'     followed by the standard deviations, followed by the
#'     proportion of total observations in each cohort expect
#'     the last, which is obtained through subtraction. 
#' @param n the sum of counts observed from nature
#' @param sizecl a representation of the sizeclasses used, these 
#'     can be the nid-points of each size-class or the lower and
#'     upper bounds of each class.
#' @param midval if TRUE, the default, the approximate analytical
#'     approach will be used.
#'
#' @return a vector of expected frequencies
#' @export
#'
#' @examples
#' \dontrun{
#' mids <- seq(6,56,2) #size classes = 2 mm as in 5-7, 7-9,...
#' av <- c(18.0,34.5)   # the means
#' stdev <- c(2.75,5.5)  # the standard deviations
#' prop1 <- 0.55  # the proportion of observations in cohort 1
#' pars <-c(av,stdev,prop1)  # combine parameters into a vector
#' predf <- predfreq(pars,n=262,sizecl=mids,midval=TRUE)
#' plot1(mids,predf,xlab="Sizes",ylab="Predicted Frequency",lwd=2)
#' }
predfreq <- function(pars,n,sizecl,midval=TRUE) { 
  cw <- sizecl[2] - sizecl[1]
  c <- (length(pars) + 1)/3 # number of normal cohorts
  props <- tail(pars,(c-1)) # input c-1 proportions, 
  props <- c(props,(1-sum(props))) #the last prop is difference
  nval <- length(sizecl)
  if (midval) { # use approximation
    freqs <- numeric(nval) #nval zero vector hold freqs
    for (i in 1:c) # step through the cohorts
      freqs <- freqs+(n*props[i]*cw)*dnorm(sizecl,
                                           pars[i],pars[i+c])
  } else { #use cumulative probablities
    freqs <- numeric(nval-1) #nval-1 zero vector to hold freqs
    for (i in 1:c) { # step through the cohorts  i=1
      tmpcump <- (n*props[i])*pnorm(sizecl,pars[i],pars[i+c])
      freqs <- freqs + (tmpcump[2:nval] - tmpcump[1:(nval-1)]) 
    } 
  } # end of if(midval) statement
  return(freqs)
} # end of predfreq

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
#' @description properties used to check a data.frame before
#'     standardization
#' @param indat the data.frame containing the data fields to be used
#'     in the subsequent standardization. It tabulates the number of
#'     NAs and the number of unique values for each variable and finds
#'     the minimum and maximum of the numeric variables
#' @param dimout determines whether or noth the dimensions of the data.frame
#'     are printed to the screen or not; defaults to FALSE
#'     
#' @return a data.frame with the rows being each variable from the input
#'     input data.frame and the columns being the number of NAs, the
#'     number of unique values, and minimum and maximum (where possible).
#' @export
#' 
#' @examples
#' \dontrun{
#' data(abdat)
#' properties(abdat)
#' }
properties <- function(indat,dimout=FALSE) {
  dominmax <- function(x) {
    if (length(which(x > 0)) == 0) return(c(NA,NA))
    mini <- min(x,na.rm=TRUE)
    maxi <- max(x,na.rm=TRUE)
    return(c(mini,maxi))
  }
  if(dimout) print(dim(indat))
  isna <- sapply(indat,function(x) sum(is.na(x)))
  uniques <- sapply(indat,function(x) length(unique(x)))
  columns <- length(indat)
  clas <- character(columns)
  for (i in 1:columns) {
    clas[i] <- class(indat[,i])[1]
  }
  numbers <- c("integer","numeric")
  pick <- which(clas %in% numbers)
  minimum <- numeric(length(uniques))
  maximum <- minimum
  for (i in 1:length(pick)) {
    minmax <- dominmax(indat[,pick[i]])
    minimum[pick[i]] <- minmax[1]
    maximum[pick[i]] <- minmax[2]
  }
  index <- 1:length(isna)
  props <- as.data.frame(cbind(index,isna,uniques,clas,round(minimum,4),
                               round(maximum,4),t(indat[1,])))
  colnames(props) <- c("Index","isNA","Unique","Class","Min",
                       "Max","Example")
  return(props)
} # end of properties

#' @title removeEmpty removes empty strings from a vector of strings
#'
#' @description removeEmpty removes empty strings from a vector of strings.
#'     Such spaces often created by spurious commas at the end of lines. It
#'     also removes strings made up only of spaces and removes spaces from
#'     inside of inidivdual chunks of text. So, should be useful when
#'     reading in data from a custom csv file when parsing different formats
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
#' 
#' @param invect vector of values
#' @param probs a vector of quantile, default=c(0.025,0.05,0.5,0.95,0.975)
#' 
#' @return a vector of the probs quantiles
#' @export quants
#' 
#' @examples
#' x <- matrix(rnorm(1000),ncol=10,nrow=100)
#' apply(x,2,quants)
#' apply(x,2,quants,probs=c(0.1,0.5,0.9))
quants <- function(invect,probs=c(0.025,0.05,0.5,0.95,0.975)) {
   ans <- quantile(invect,probs = probs,na.rm=T)
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
#' pick <- which.closest(5.0,vals,index=TRUE) #closest to 5?
#' pick        # the index of the closest
#' vals[pick]  # its value
#' which.closest(5.0,vals,index=FALSE) # straight to the value
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
#' 
#' @examples 
#' \dontrun{
#'    x <- 1:10
#'    y <- 6:15
#'    x
#'    y
#'    x[(x %ni% y)]   # are not in y
#' }
`%ni%` <- function(x,y) {  
  !(x %in% y)
}
