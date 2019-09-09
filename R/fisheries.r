

#' @title bce Baranov catch equation
#'
#' @description bce the Baranov Catch Equation. The total kill is made
#'     up of fish being killed by fishing and other dying naturally.
#'     We use the bce to estimate the catch (those killed by fishing).
#'     The problem is that some fish that would be expected to die 
#'     naturally can be expected to be caught and killed by fishing
#'     so estimating the catch is slightly more complex than Nt x Ht.
#'     It is invariably better to use the Baranov Catch Equation.
#'
#' @param M instantaneous rate of natural mortality, assumed constant
#' @param Ft a vector of age-specific instantaneous rates of fishing
#'     mortality over the time period t, this is usually estimated by
#'     multiplying the fully selected F by the selectivity/availability
#'     at age.
#' @param Nt The population numbers at the start of time t
#' @param ages the ages 0:maxage used in the calculations
#'
#' @return a matrix of numbers-, natural mortality-, and catch-at-age
#' @export
#'
#' @examples
#' \dontrun{
#' Ft <- -log(1 - 0.2) # harvest rate of 0.2
#' M <- 0.12
#' Nt <- 1000
#' bce(M,Ft,Nt)   # should give 188.8862
#' }
bce <- function(M,Ft,Nt,ages) {  # M=M; Ft=Ft; Nt=N0; ages=age
  nage <- length(ages)
  if (length(Ft) < nage) {
    Ft <- rep(Ft[1],nage)
    warning("Ft contains fewer values than number of ages  \n")
  }
  Z <- (M + Ft)
  columns <- c("Nt","N-Dying","Catch")
  ans <- matrix(0,nrow=nage,ncol=length(columns),
                dimnames=list(ages,columns))
  ans[1,] <- c(Nt,NA,NA)
  for (a in 2:nage) {  # a=2
    Na <- ans[(a-1),"Nt"]
    newN <- Na * exp(-Z[a])
    catch <- (Ft[a]/(M + Ft[a])) * Na * (1 - exp(-(Z[a])))
    mort <- Na - (newN + catch)
    ans[a,] <- c(newN,mort,catch)
  }
  return(ans)
}

#' @title discretelogistic example box 2.2 Discrete logistic model
#'
#' @description discretelogistic is an implementation of example box 2.2 
#'     from the the Excel version of MQMF. It enables the exploration of 
#'     the dynamics of the Discrete logistic model, based around the 
#'     classical Schaefer model. 
#'     
#'     The time series nature of population growth is clear from 
#'     the fatc that Nt+1 is a function of Nt. One can expect serial
#'     correlation. Setting the r parameter to <= 1.0,
#'     would generate monotonically damped equilibria. r values between 
#'     1 < r < 2.03 would generate damped oscillatory equilibria, r 
#'     values from 2.03 < r < 2.43 should generate stable limit cycles 
#'     on a cycle of 2., 2.43 < r < 2.54 gives stable limit cycles of 
#'     cycle 4, then 2.54 < r < 2.57 gives cycles > 4, and ~2.575 < r 
#'     gives chaos (though r = 2.63 appears to generate a repeat period
#'     of six!). discretelogistic should be used in conjunction with 
#'     plot, for which an S3 method has been defined plot.dynpop. As the
#'     dynamics are obviously sequential (i.e. n at t+1 is dependent 
#'     upon n at t) the last line of the dynamics is removed to avoid 
#'     an empty nt1 row.
#'
#' @param r intrinsic rate of population increase; default = 0.5
#' @param K carrying capacity; default = 1000
#' @param N0 Initial population size; default = 50
#' @param Ct annual catch default = 0.0
#' @param Yrs years of population growth
#' @param p the asymmetry parameter. the default value of 1.0 leads
#'     to the classical Schaefer model. A value of 1e-08 approximates
#'     the Fox model where the mode is skewed to the left of centre.
#'
#' @return invisibly returns a matrix of year, nt, and nt1
#' @export
#'
#' @examples
#' \dontrun{
#'   discretelogistic(0.5,1000.0,25,0.0,50) # asymptotic
#'   discretelogistic(2.5,1000.0,25,0.0,50) # 4-phase stable limit
#'   ans <- discretelogistic(r=2.55,K=1000.0,N0=100,Ct=95.0,Yrs=100)
#'   head(ans,20)
#'   plot(ans)
#' }
discretelogistic <- function(r=0.5,K=1000.0,N0=50.0,Ct=0.0,Yrs=50,p=1.0) {
  yr1 <- Yrs + 1
  years <- seq(1,yr1,1)
  pop <- numeric(yr1)
  pop[1] <- N0
  for (year in 2:yr1) {
    Bt <- pop[year-1]
    pop[year] <- max((Bt + (r*Bt/p)*(1-(Bt/K)^p) - Ct),0)
  }
  pop2 <- pop[2:yr1]
  out <- cbind(year=years,nt=pop,nt1=c(pop2,NA))
  rownames(out) <- years
  out <- out[-yr1,]  # to remove the empty last year for nt1
  class(out) <- "dynpop"
  return(invisible(out))
} # End of discretelogistic generating class dlpop


#' @title Gz calculates predicted Gompertz length at age growth curve
#'
#' @description Gz calculates length at age for the Gompertz curve.
#'     This curve can have an inflection near the origin as well
#'     as at an age where growth slows due to maturity occurring.
#'
#' @param p is a vector the first three cells of which are a, b, c, for
#'     the Gz curve.
#' @param ages is a vector of ages; could be a single number
#'
#' @return a vector of predicted lengths for a vector of ages in 'ages'
#' @export
#'
#' @examples
#' \dontrun{
#' ages <- seq(0,20,1)   # sigma is ignored here
#' pars <- c(a=26.0,b=0.7,c=-0.5,sigma=1.0) # a, b, c, sigma; 
#' cbind(ages,Gz(pars,ages))
#' }
Gz <- function(p, ages) {
  return(p[1]*exp(-p[2]*exp(p[3]*ages)))
}

#' @title logist Logistic selectivity function
#'
#' @description logist calcualtes a Logistic curve that can be used as a
#'     selectivity function, or maturity curve, of wherever a logistic 
#'     is required. This version uses the logistic function
#'     1/(1+exp(-log(19.0)*(lens-inL50)/delta)), which
#'     explicitly defines the L50 and uses delta = (inL95-inL50) as
#'     the second parameter.
#' @param inL50 is the length at 50 percent selection/maturity/whatever
#' @param delta is the difference in selection/maturity/whatever between
#'     inL50 and inL95
#' @param depend a vector of lengths/ages for which the logistic value
#'     will be calculated.
#' @param knifeedge defaults to FALSE. If knifeedge is TRUE then the
#'     logistic values < the depend value of inL50 is set to
#'     zero, and all those >= inL50 are set to 1.0, approximating
#'     knife-edge selectivity
#' @return A vector of length(depend) with the predicted logistic values
#' @export
#' @examples
#' \dontrun{
#' in50 <- 100.0
#' deltaS <- 8.0
#' lens <- seq(2,210,2)
#' select <- logist(inL50=in50,delta=deltaS,depend=lens)
#' selectk <- logist(in50,deltaS,lens,knifeedge=TRUE)
#' round(cbind(lens[35:70],select[35:70],selectk[35:70]),5)
#' }
logist <- function(inL50,delta,depend,knifeedge=FALSE) {
  ans <- 1/(1+exp(-log(19.0)*(depend-inL50)/(delta)))
  if (knifeedge) {
    pick <- which(depend < inL50)
    if (length(pick) > 0) {
      ans[pick] <- 0.0
      ans[-pick] <- 1.0
    }
  }
  return(ans)
} # end of logist

#' @title MaA alternative logistic function commonly used for maturity
#'
#' @description MaA a function 1/(1+(1/exp(a + b x depend))) which can
#'     be expressed as exp(a + b x depend)/(1 + exp(a + b x depend)).
#'     This describes a symmetric logistic curve that has the property
#'     SM50 = -a/b and the interquartile distance is 2.Ln(3)/b.
#'     
#' @param ina is the intercept of the exponential function
#' @param inb is the gradient of the exponential function
#' @param depend is a vector of lengths/ages for which the logistic 
#'     maturity value will be calculated
#' @return A vector of length(depend) with predicted maturity values
#'
#' @export
#' @examples
#' a <- -14.383
#' b <- 0.146017
#' lens <- seq(2,210,2)
#' round(MaA(a,b,depend=lens),5) # length based
#' round(MaA(-2.5,0.95,0:25),5)   # age based
MaA <- function(ina,inb,depend) {
  ans <- exp(ina + inb * depend)/(1 + exp(ina + inb * depend))
  return(ans)
} # end of Maturity at age

#' @title mm calculates the predicted Michaelis-Menton length at age
#'
#' @description mm calculates length at age for the generalized 
#'     Michaelis-Menton curve.
#'
#' @param p is a vector the first three cells of which are a, b, c
#'    for the mm curve.
#' @param ages is a vector of ages; could be a single number
#'
#' @return a vector of predicted lengths for a vector of ages in 'ages'
#' @export
#'
#' @examples
#' \dontrun{
#' ages <- seq(0,20,1)    # sigma is ignored here
#' pars <- c(a=23.0,b=1.0,c=1.0,sigma=1.0) # a, b, c, sigma
#' cbind(ages,mm(pars,ages))
#' }
mm <- function(p, ages) {
  return((p[1]*ages)/(p[2] + ages^p[3]))
}

#' @title mnnegLL generic multinomial negative log-likelihoods
#' 
#' @description mnnegLL a generic multinomial negative log-likelihood 
#'     that requires observed frequencies and predicted frequencies, 
#'     although the predicted frequencies could also be the final 
#'     proportions, as long as they summed to one. It checks that the 
#'     number of predicted values matches the number of observed values
#'
#' @param obs the original observed frequencies
#' @param predf the predicted frequencies or proportions
#'
#' @return a single scalar value
#' @export
#'
#' @examples
#' \dontrun{
#'   obs <- c(0,0,6,12,35,40,29,23,13,7,10,14,11,16,11,11,9,8,5,2,0)
#'   predf <- c(0.1,0.9,4.5,14.4,29.7,39.9,35.2,21.3,10.9,8.0,9.5,12.1,
#'              14.1,14.7,13.7,11.5,8.6,5.8,3.5,1.9,0.9) 
#'  mnnegLL(obs,predf)   # should be  705.5333
#' }
mnnegLL <- function(obs,predf) { 
  k <- length(obs)
  if (length(predf) != k) {
    label <- paste0("Need a predicted frequency for each observed ",
                    "frequency input to mnnegLL  \n")
    stop(label)
  } 
  return(-sum(obs * log(predf/sum(predf,na.rm=TRUE)),na.rm=TRUE))
} # end of mnnegLL 

#' @title negLL calculate log-normal log-likelihoods
#'
#' @description negLL calculates log-normal negative log-likelihoods. It
#'     expects the input parameters to be log-transformed, so the funk 
#'     used to calculate the log or the predicted values also needs to 
#'     expect log-transformed parameters. In addition, it checks that 
#'     there are no missing data (NA) within the input observed 
#'     log-transformed data. If there are it uses only those records 
#'     for which there are values.
#'
#' @param pars the log-transformed parameters to be used in the funk for
#'     calculating the log of the predicted values against which the log
#'     observed values will be compared
#' @param funk the function used to calculate the log-predicted values 
#'     of whatever variable is being used (eg. cpue, catches, etc.)
#' @param indat the data used by funk with pars to calculate the log-
#'     predicted values.
#' @param logobs the observed values log-transformed ready for 
#'     comparison with the log-predicted values from funk and pars.
#' @param ... required to allow funk to access its other parameters 
#'     without having to explicitly declare them in negLL
#'     
#' @return the negative log-likelihood using log-normal errors.
#' @export
#'
#' @examples
#' \dontrun{
#' data(abdat)
#' fish <- abdat$fish
#' param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05))
#' negLL(pars=param,funk=simpspm,indat=fish,logobs=log(fish[,"cpue"]))
#' }
negLL <- function(pars,funk,indat,logobs,...) {
  logpred <- funk(pars,indat,...)
  pick <- which(is.na(logobs))
  if (length(pick) > 0) {
    LL <- -sum(dnorm(logobs[-pick],logpred[-pick],exp(tail(pars,1)),log=T))
  } else {
    LL <- -sum(dnorm(logobs,logpred,exp(tail(pars,1)),log=T))
  }
  return(LL)
} # end of negLL

#' @title negLL1 calculate log-normal log-likelihoods
#'
#' @description negLL1 calculates log-normal negative log-likelihoods. It
#'     expects the input parameters to be log-transformed, so the funk used
#'     to calculate the log or the predicted values also needs to expect
#'     log-transformed parameters. In addition to estimating the negative 
#'     log-liklelihoods for log-normally distributed data it also places a 
#'     penalty on the first parameter if that parameter approaches very 
#'     close to zero; see the function penalty0.
#'
#' @param pars the log-transformed parameters to be used in the funk for
#'     calculating the log of the predicted values against which the log
#'     observed values will be compared
#' @param funk the function used to calculate the log-predicted values of
#'     whatever variable is being used (eg. cpue, catches, etc.)
#' @param indat the data used by funk with pars to calculate the log-
#'     predicted values.
#' @param logobs the observed values log-transformed ready for comparison
#'     with the log-predicted values from funk and pars.
#' @param ... required to allow funk to access its other parameters without
#'     having to explicitly declare them in negLL
#'     
#' @return the negative log-likelihood using log-normal errors.
#' @export
#'
#' @examples
#' \dontrun{
#' data(abdat)
#' fish <- abdat$fish
#' param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05))
#' negLL1(pars=param,funk=simpspm,indat=fish,logobs=log(fish[,"cpue"]))
#' }
negLL1 <- function(pars,funk,indat,logobs,...) {
  logpred <- funk(pars,indat,...)
  pick <- which(is.na(logobs))
  if (length(pick) > 0) {
    LL <- -sum(dnorm(logobs[-pick],logpred[-pick],exp(tail(pars,1)),log=T))
  } else {
    LL <- -sum(dnorm(logobs,logpred,exp(tail(pars,1)),log=T))
  }
  LL <- LL + penalty0(exp(pars[1]))
  return(LL)
} # end of negLL1


#' @title negNLL  -ve log-likelihood for normally distributed variables
#'
#' @description negNLL - Calculates the negative log-likelihood for
#'     normally distributed variables. It assumes the presence of a function
#'     'funk' that will calculate predicted values of a 'dependent' variable
#'     from a vector of 'independent' values.
#'     Using negNL takes approximately 0.4 milliseconds less time than the
#'     same analysis conducted using negNLP; this was determined using the
#'     microbenchmark package.
#' @param pars a vector containing the parameters being used in funk, plus
#'   an extra sigma which is the standard deviation of the normal random
#'   likelihoods in dnorm
#' @param funk the function name that calculates the predicted values from
#'   the independent values
#' @param independent the x-axis variable, which in the model gives rise
#'   to the predicted values to compare with observed 'dependent' variable
#' @param observed the observed values of the values that the model will
#'   predict for each of the independent values.
#' @param ... required to allow funk to access its other parameters without
#'     having to explicitly declare them in negNLL
#'     
#' @return the sum of the negative log-likelihoods using a normal PDF
#' @export
#'
#' @examples
#' \dontrun{
#'  data(kimura)
#'  pars <- c(Linf=56.0,K=0.4,t0=0.2,sigma=1.5)
#'  negNLL(pars,vB,independent=kimura[,"age"],observed=kimura[,"length"])
#'  # should be 19.20821
#' }
negNLL <- function(pars,funk,independent,observed,...) {
  predobs <- funk(pars,independent,...)
  LL <- -sum(dnorm(observed,predobs,tail(pars,1),log=T))
  return(LL)
}

#' @title negNLP  -ve log-likelihood for normally distributed variables
#'
#' @description negNLP Calculates the negative log-likelihood for normally
#'     distributed variables. It assumes the presence of a function 'funk'
#'     that will calculate predicted values of a dependent variable from a
#'     vector of independent values. By having a separate vector of
#'     parameters in 'initpar' and a vector of the indices of those 
#'     parameters that will be fitted it is possible to fit only a subset 
#'     of parameters. This is useful if generating a likelihood profile, or 
#'     setting up a likelihood ratio test. With more complex models it is 
#'     often a useful strategy to estimate the full number of parameters in 
#'     a series of phases, increasing the number being estimated each time 
#'     while keeping the rest fixed. 'negNLP' makes such phasing of the 
#'     fitting of a model to data possible. This function can be applied 
#'     directly to normally distributed data or to log-transformed data for 
#'     log-normally distributed data. There is a slight cost in terms of 
#'     time such that using negNLP takes approximate 0.4 milliseconds 
#'     longer than when using negNL; and that is with either 13 or 1300 
#'     observations.
#'
#' @param pars a vector containing the parameters being used in funk, plus
#'     an extra sigma which is the standard deviation of the normal random
#'     likelihoods in dnorm
#' @param funk the function name that calculates the predicted values from
#'     the independent values
#' @param independent the x-axis variable, that which in the model gives 
#'     rise to the predicted values of the dependent variable
#' @param dependent the observed values for comparison with the values that
#'     the model will predict for each of the independent values.
#' @param initpar this defaults to the same as pars - using all parameters
#' @param notfixed a vector identifying the indices of the parameters to be
#'     fitted, which also defines those that will be fixed; defaults
#'     to all parameters. If some need to be kept constant so as to generate
#'     a likelihood profile then omit their index from 'notfixed'.
#' @param ... required to allow funk to access its other parameters without
#'     having to explicitly declare them in negLL
#'     
#' @return the sum of the negative log-likelihoods using a normal PDF
#' @export
#' @examples
#' \dontrun{
#'   txt1 <- 'all example code should be able to run'
#' }
negNLP <- function(pars,funk,independent,dependent,initpar=pars,
                   notfixed=c(1:length(pars)),...) {
  predobs <- funk(pars,independent,initpar,notfixed,...)
  LL <- -sum(dnorm(dependent,predobs,tail(pars,1),log=T))
  return(LL)
}

#' @title negLLP  -ve log-likelihood for normally distributed variables
#'
#' @description negLLP calculates the negative log-likelihood for normally
#'     distributed variables. It assumes the presence of a function 'funk'
#'     that will calculate predicted values of a dependent variable from a
#'     vector of independent values. By having a separate vector of
#'     parameters in 'initpar' and a vector of the indices of those 
#'     parameters that will be fitted it is possible to fit only a subset 
#'     of parameters. This is useful if generating a likelihood profile, or 
#'     setting up a likelihood ratio test. With more complex models it is 
#'     often a useful strategy to estimate the full number of parameters in 
#'     a series of phases, increasing the number being estimated each time 
#'     while keeping the rest fixed. 'negLLP' makes such phasing of the 
#'     fitting of a model to data possible.
#'     This function can be applied directly to log-transformed data for 
#'     log-normally distributed data but also to normally distributed 
#'     data, in which case one would not log-transform the data being 
#'     input to the logobs argument and funk would not be generated the
#'     log-transformed rpedicted values.
#'     
#'     The selection of which parameters to vary is simply implemented 
#'     through repeatedly copying the original input values from initpar
#'     and then changing those notfixed from the varying par values
#'
#' @param pars a vector containing the log-transformed parameters being 
#'     used in funk, plus an extra sigma which is the standard deviation of 
#'     the log-normal random likelihoods in dnorm
#' @param funk the function name that calculates the predicted values from
#'     the independent values
#' @param indat the data.frame that contains the data used by the input funk
#' @param logobs the log-transformed observed values for comparison with the
#'     values that the model will predict for each of the independent values
#' @param initpar this defaults to the same as pars - using all parameters
#' @param notfixed a vector identifying the indices of the parameters to be
#'     fitted, which also defines those that will be fixed; defaults
#'     to all parameters set to vary. If some need to be kept constant 
#'     so as to generate a likelihood profile then omit their index from 
#'     'notfixed'.
#' @param ... required to allow funk to access its other parameters without
#'     having to explicitly declare them in negLL
#'     
#' @return the sum of the negative log-likelihoods using a normal PDF
#' @export
#' @examples
#' \dontrun{
#'  data(abdat)
#'  fish <- abdat$fish 
#'  param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05)) 
#'  optmod <- nlm(f=negLLP,p=param,funk=simpspm,initpar=param,
#'               notfixed=c(1,2,3,4),indat=fish,logobs=log(fish$cpue),
#'               typsize=magnitude(param),iterlim=1000)
#'  outfit(optmod,backtransform = TRUE)
#'   
#'  rval <- seq(0.325,0.45,0.0125)  # set up the test sequence
#'  columns <- c("r","K","Binit","sigma","-veLL")
#'  result <- matrix(0,nrow=11,ncol=5,dimnames=list(rval,columns))
#'  profest <- c(r= 0.32,K=11500,Binit=4250,sigma=0.05) # end of sequence
#'  for (i in 1:11) { 
#'    param <- log(c(rval[i],profest[2:4])) #recycle the profest values to
#'    parinit <- param                # improve the stability of nlm as  
#'    bestmodP <- nlm(f=negLLP,p=param,funk=simpspm,initpar=parinit,#the r
#'                indat=fish,logobs=log(fish$cpue),notfixed=c(2:4), #value
#'                typsize=magnitude(param),iterlim=1000)       # changes
#'    bestest <- exp(bestmodP$estimate)     
#'    result[i,] <- c(bestest,bestmodP$minimum)  # store each result
#'  }
#'  result   #Now plot -veLL againt r values for the profile
#' }
negLLP <- function(pars, funk, indat, logobs, initpar=pars,
                   notfixed=c(1:length(pars)),...) {
  usepar <- initpar
  usepar[notfixed] <- pars[notfixed]
  logpred <- funk(usepar,indat,...)
  pick <- which(is.na(logobs))
  if (length(pick) > 0) {
    LL <- -sum(dnorm(logobs[-pick],logpred[-pick],exp(tail(pars,1)),log=T))
  } else {
    LL <- -sum(dnorm(logobs,logpred,exp(tail(pars,1)),log=T))
  }
  return(LL)
} # end of negLLP


#' @title simpspm calculates only the predicted log(CE) for an SPM
#' 
#' @description simpspm calculates only the predicted log(CPUE) for an SPM 
#'     model. It sets the Polacheck et al parameter 'p' depending on the 
#'     schaefer boolean term, and this determines the asymmetry of the 
#'     production curve.
#'     If p = 1.0 then the SPM is the Schaefer model, if it is 1e-8 it
#'     approximates the Fox model. The output of log(CPUE) is to simplify
#'     the use of log-normal residual errors or likelihoods. This function 
#'     is designed for data consisting of only a single cpue time-series.
#'     simpspm must have at least three parameters, including the sigma, 
#'     even if sum-of-squared residuals is used as a minimizer, then sigma
#'     would just float.
#'
#' @param pars the parameters of the SPM are either c(r, K, Binit, sigma),
#'     or c(r, K, sigma), the sigma is required in all cases. Binit is 
#'     required if the fishery data starts after the stock has been
#'     depleted. Each parameter must be log-transformed for improved model
#'     stability and is transformed inside simpspm.
#' @param indat the data which needs to include year, catch, and cpue. 
#' @param schaefer a logical value determining whether the spm is to be a
#'     simple Schaefer model (p=1) or approximately a Fox model (p=1e-08). 
#'     The default is TRUE = Schaefer model
#' @param year the column name within indat containing the years
#' @param cats the column name within indat containing the catches
#' @param index the column name within indat containing the cpue.
#'
#' @return a vector of length nyrs of log(cpue)
#' @export
#'
#' @examples
#' \dontrun{
#'  data(abdat)
#'  fish <- abdat$fish
#'  colnames(fish) <- tolower(colnames(fish))
#'  param <- log(c(r=0.4,K=9400,Binit=3400,sigma=0.05))
#'  predCE <- simpspm(pars=param,fish)
#'  cbind(fish,exp(predCE))
#' } 
simpspm <- function(pars, indat,schaefer=TRUE,  
                    year="year",cats="catch",index="cpue") { 
  nyrs <- length(indat[,year])
  biom <- numeric(nyrs+1)
  catch <- indat[,cats]
  ep <- exp(pars) # par contains at least log of (r,K, and sigma)
  biom[1] <- ep[2]  
  if (length(pars) > 3) biom[1] <- ep[3] # Binit should be before sigma
  #p is the location of mode parameter 1 = Schaefer, 1e-8 ~ Fox model
  if(schaefer) p <- 1 else p <- 1e-8
  for (yr in 1:nyrs) { # fill biom using Bt as an interim step
    Bt <- biom[yr]  # avoid negative biomass using a max statement
    biom[yr+1] <- max(Bt + ((ep[1]/p)*Bt*(1-(Bt/ep[2])^p)-catch[yr]),40)
  }
  pick <- which(indat[,index] > 0)
  qval <- exp(mean(log(indat[pick,index]/biom[pick])))
  return(log(biom[1:nyrs] * qval))  # the log of predicted cpue
} # end of simpspm generates log-predicted cpue

#' @title simpspmM calculates the predicted CE for an SPM
#'
#' @description simpspmM calculates the predicted CPUE for an SPM model. It
#'     assumes that there is a variable called 'p' in the global environment
#'     and this 'p' variable determines the asymmetry of the production 
#'     curve. If p = 1.0 then the SPM is the Schaefer model, if it is 1e-8 
#'     it approximates the Fox model.
#'
#' @param par the parameters of the SPM = r, K, a q for each column of cpue,
#'     a sigma for each cpue, and Binit if fishery depleted to start with. 
#'     Each parameter is in log space and is transformed inside simpspmM
#' @param indat the data which needs to include year, catch, and cpue. The
#'    latter should have a separate column for each fleet, with a column 
#'    name beginning with cpue or whatever name you put in index (see below) 
#'    for example cpue1, cpue2,etc.
#' @param schaefer a logical value determining whether the spm is to be a
#'     simple Schaefer model (p=1) or approximately a Fox model (p=1e-08). 
#'     The default is TRUE
#' @param year the column name within indat containing the years
#' @param cats the cloumn name within indat containing the catches
#' @param index the prefix in the column names given to the indices of 
#'     relative abundance used, perhaps 'cpue' as in cpueTW, cpueAL, etc. 
#'     grep is used to search for columns containing this prefix to 
#'     identify whether there are more than one column of cpue data.
#'
#' @return a vector or matrix of nyrs of the predicted CPUE
#' @export
#'
#' @examples
#' \dontrun{
#' data(dataspm)
#' fish <- dataspm$fish
#' fish
#' colnames(fish) <- tolower(colnames(fish))
#' pars <- c(r=0.242,K=5170,Binit=2840)
#' predCE <- simpspmM(pars,fish)
#' cbind(fish[,"year"],fish[,"cpue"],predCE)
#' }
simpspmM <- function(par,indat,schaefer=TRUE,
                    year="year",cats="catch",index="cpue") {

  celoc <- grep(index,colnames(indat))
  nce <- length(celoc)
  npar <- 2 + nce + nce   # r + K + nce_sigma + nce_q
  nyrs <- length(indat[,"year"])
  biom <- numeric(nyrs+1)
  catch <- indat[,cats]
  predCE <- matrix(NA,nrow=nyrs,ncol=nce)
  r <- exp(par[1])
  K <- exp(par[2])
  biom[1] <- K
  if (length(par) > npar) biom[1] <- exp(par[npar+1]) 
  if(schaefer) p <- 1 else p <- 1e-8
  for (loc in 1:nyrs) {
    Bt <- biom[loc]
    biom[loc+1] <- max(Bt + ((r/p)*Bt*(1-(Bt/K)^p)-catch[loc]),40)
  }
  for (i in 1:nce) {
    pick <- which(indat[,celoc[i]] > 0)
    qval <- exp(mean(log(indat[pick,celoc[i]]/biom[pick])))
    predCE[pick,i] <- biom[pick] * qval
  }
  return(predCE)
} # end of simpspmM


#' @title ssq a generalized function for summing squared residuals
#'
#' @description ssq is a generalized function for summing squared
#'     residuals which is designed for ease of use in nlm (or
#'     optim, or nlminb). NAs are removed. It assumes the input of
#'     a predefined function 'funk' that will calculate predicted
#'     values of a 'dependent' variable from a vector of 'independent'
#'     values, for which one has observations. The predicted values
#'     are compared with the observed 'dependent data, and the
#'     resulting SSQ returned.
#'
#' @param par a vector of parameters used by funk
#' @param funk a function that uses par and indep to calculate
#'     predicted values to compare with the dep values
#' @param independent a vector containing the independent variable
#'     (x-axis) upon which the observed variable is assumed to be
#'     dependent.
#'
#' @param observed a vector containing the observed data (y-axis)
#'
#' @return a single number (scaler) that is the sum of squared
#'     residuals between the dep values and those calculated by funk
#' @export
#'
#' @examples
#' \dontrun{
#'    print("an example being developed")
#' }
ssq <- function(par,funk,independent,observed) {
  predval <- funk(par,independent)
  return(sum((observed - predval)^2,na.rm=TRUE))
} # end of general ssq


#' @title vB calculates the predicted von Bertalanffy length at age
#'
#' @description vB calculates length at age for the von Bertalanffy curve.
#'
#' @param par is a vector the first three cells of which are Linf, K, and t0
#'    for the vB curve.
#' @param ages is a vector of ages; could be a single number
#'
#' @return a vector of predicted lengths for the vector of ages in 'ages'
#' @export
#'
#' @examples
#' \dontrun{
#' ages <- seq(0,20,1)   # sigma is ignored here
#' pars <- c(Linf=50,K=0.3,t0=-1.0,sigma=1.0) # Linf, K, t0, sigma
#' cbind(ages,vB(pars,ages))
#' }
vB <- function(par,ages) {
  return(par[1] * (1 - exp(-par[2]*(ages-par[3]))))
}







