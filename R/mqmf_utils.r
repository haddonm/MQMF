


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

#' @title halftable halves the height of a tall narrow data.frame
#'
#' @description halftable would be used when printing a table using kable
#'     from knitr where one of the columns was Year. The objective would be to
#'     split the table in half taking the bottom half and attaching it on
#'     the right hand side of the top half. The year column would act as the
#'     index.
#'
#' @param inmat the data.frame to be subdivided
#' @param yearcol the column name of the year field
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
halftable <- function(inmat,yearcol="Year",subdiv=3) {
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
#' @param backtransform a logical default = FALSE. If TRUE it assumes
#'     that the parameters have been log-transformed for stability
#'     and need back-transforming
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
outfit <- function(inopt,backtransform=FALSE){
   nlmcode <- c("relative gradient close to zero, probably solution.",
                "repeated iterates in tolerance, probably solution.",
                paste0("nothing lower than estimate.",
                       "Either ~local min or steptol too small."),
                "iteration limit exceeded.",
                paste0("stepmax exceeded ,5 times. Unbounded or ",
                       "asymptotic below or stepmax too small."))
   if (length(grep("value",names(inopt))) > 0) { # optim
      cat("optim solution:  \n")
      cat("par         : ",inopt$par,"\n")
      cat("minimum     : ",inopt$value,"\n")
      cat("iterations  : ",inopt$counts," iterations, gradient\n")
      cat("code        : ",inopt$convergence,"\n")
      cat("message     : ",inopt$message,"\n")
   }
   if (length(grep("minimum",names(inopt))) > 0) {  # nlm - preferred
      cat("nlm solution:  \n")
      cat("minimum     : ",inopt$minimum,"\n")
      cat("iterations  : ",inopt$iterations,"\n")
      cat("code        : ",inopt$code,"  ",nlmcode[inopt$code],"\n")
      if (backtransform) {
         ans <- cbind(par=inopt$estimate,gradient=inopt$gradient,
                      transpar=round(exp(inopt$estimate),6))
         } else {
         ans <- cbind(par=inopt$estimate,gradient=inopt$gradient)
      }
      rownames(ans) <- 1:length(inopt$estimate)
      print(ans)
   }
   if (length(grep("objective",names(inopt))) > 0) {
      cat("nlminb solution:  \n")   # nlminb seems to be deprecated
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

#' @export '%ni%'
`%ni%` <- function(x,y) {
  !(x %in% y)
}
