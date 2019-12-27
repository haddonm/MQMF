

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
#' @param Fat a vector of age-specific instantaneous rates of fishing
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
#' age <- 0:25
#' Ft <- -log(1 - 0.2) # harvest rate of 0.2
#' Faa <- rep(Ft,length(age))
#' M <- 0.12
#' Nt <- 1000
#' bce(M,Fat,Nt,ages=age)   # should give 188.8862
#' }
bce <- function(M,Fat,Nt,ages) {
  nage <- length(ages)
  lFat <- length(Fat)
  if (lFat < nage) {
    lastF <- tail(Fat,1)
    fill <- rep(lastF,(nage - lFat))
    Fat <- c(Fat,fill)
    warning("Fat contains fewer values than number of ages  \n")
  }
  Z <- (M + Fat)
  columns <- c("Nt","N-Dying","Catch")
  ans <- matrix(0,nrow=nage,ncol=length(columns),
                dimnames=list(ages,columns))
  ans[1,] <- c(Nt,NA,NA)
  for (a in 2:nage) {  # a=2
    Na <- ans[(a-1),"Nt"]
    newN <- Na * exp(-Z[a])
    catch <- (Fat[a]/(M + Fat[a])) * Na * (1 - exp(-(Z[a])))
    mort <- Na - (newN + catch)
    ans[a,] <- c(newN,mort,catch)
  }
  return(ans)
}

#' @title bh represents one version of Beverton-Holt recruitment
#' 
#' @description bh implements the Beverton-Holt stock recruitment
#'    equation where R = aB/(b + B), where R is the recruitment, a and
#'    b are the parameters and B is the spawning biomass. a is the 
#'    maximum recruitment level and b is the biomass required to
#'    generate 0.5 x maximum recruitment 
#'
#' @param p a vector of the a and b parameters
#' @param B a vector, possibly of length 1, of spawning biomass levels
#'
#' @return a vector equal in length to B or the predicted recruitments
#' @export
#'
#' @examples
#' \dontrun{
#'   B <- 1:3000
#'   rec <- bh(c(1000,200),B)
#'   plot1(B,rec,xlabel="SpB",ylabel="Recruitment",lwd=2)
#' }
bh <- function(p,B) {
  rec <- (p[1] * B)/(p[2] + B)
  return(rec)
} # end of Beverton-Holt


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
#'   plot(ans)  # uses an S3 plot method for dynpop objects
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


#' @title domed calculates domed selectivity curves
#' 
#' @description domed uses 6 parameters and a set of mean size or
#'     age classes to calculate a domed selectivity curve with a 
#'     maximum of 1.0 but has parameters for the selectivity of
#'     the initial and final size/age classes. There is an 
#'     ascending limb and a descending limb with the potential 
#'     of a plateau in between. The six parameters are 1) the
#'     age/size where selectivity first becomes 1.0, 2) the size/
#'     age where selectivity first begins to decline, 3) the 
#'     steepness of the ascending limb, 4) the steepness of the
#'     descending limb, 5) the selectivity of the first age/size
#'     class, and 6) the selectivity of the last age/size class.
#'
#' @param p a vector of six parameters.
#' @param L a vector of the mean of nL age/size classes
#'
#' @return a vector of selectivities
#' @export
#'
#' @examples
#' \dontrun{
#'   L <- seq(1,30,1)
#'   p <- c(10,11,16,33,-5,-2)
#'   sel <- domed(p,L)
#'   plot1(L,sel,xlabel="Age",ylabel="Selectivity",lwd=2)
#' }
domed <- function(p,L) {
  nL <- length(L)
  comp1 <- 1/(1 + exp(-p[5]))
  comp2 <- exp((-(L - p[1])^2)/p[3])
  comp3 <- exp((-(L[1] - p[1])^2)/p[3])
  asc <- comp1 + (1 - comp1) * ((comp2 - comp3)/(1 - comp3))
  comp4 <- 1/(1 + exp(-p[6]))
  comp5 <- exp((-(L - p[2])^2)/p[4])
  comp6 <- exp((-(L[nL] - p[2])^2)/(p[4]-1))
  dsc <- 1 + (comp4 - 1) * ((comp5 - 1)/(comp6 - 1))
  J1 <- 1/(1 + exp(-(20*(L - p[1])/(1 + abs(L - p[1])))))
  J2 <- 1/(1 + exp(-20*((L - p[2])/(1 + abs(L - p[2])))))
  sel <- (asc * (1 - J1)) + J1 * (1 - J2 + dsc * J2)
  return(sel)
} # end of domed

#' @title fabens calculates predicted growth increment for tagging data
#' 
#' @description fabens requires at least two parameters, Linf and K from
#'     the von Bertalanffy growth curve in a vector, as well as the 
#'     initial length and the change in time between tag release and 
#'     recapture. It then calculates the expected growth increment.
#'
#' @param par a vector of at least Linf, and K from teh von Bertalanffy 
#'     growth curve
#' @param indat the matrix or data.frame of data columns containing at
#'     least the initial lengths and the deltaT, time intervals between
#'     tag release and recapture.
#' @param initL column name of the initial lengths within indat
#' @param delT column name of the time interval, deltaT, within indat
#'
#' @return a vector of predicted growth increments 
#' @export
#'
#' @examples
#' \dontrun{
#'  data(blackisland)
#'  plot(blackisland$len1,blackisland$deltal,type="p",pch=16,
#'  xlab="Initial Length mm",ylab="Growth Increment mm",
#'  panel.first=grid())
#'  abline(h=0)
#'  param <- c(170, 0.3, 4.0) # Linf, K, sigma
#'  predDL <- fabens(param,blackisland,initL="len1",delT="deltat")
#'  lines(blackisland$len1,predDL,col=2,lwd=2)   
#' }
fabens <- function(par,indat,initL="len1",delT="deltat") {
  preddL <- (par[1] - indat[,initL])*(1 - exp(-(par[2] * indat[,delT])))
  return(preddL)
}

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

#' @title invl calculates growth increments for the inverse logistic
#' 
#' @description invl requires at least three parameters, MaxDL, L50, and
#'     delta, in a vector, as well as the initial length and the change 
#'     in time between tag release and recapture. Given those it 
#'     calculates the expected growth increment according to the inverse
#'     logistic curve. The parameter delta is equivalent to L95 - L50, 
#'     the length difference between the length at half MaxDL, and the
#'     length at 5% of MaxDL.
#'
#' @param par a vector of at least Linf, and K from teh von Bertalanffy 
#'     growth curve
#' @param indat the matrix or data.frame of data columns containing at
#'     least the initial lengths and the deltaT, time intervals between
#'     tag release and recapture.
#' @param initL column name of the initial lengths within indat
#' @param delT column name of the time interval, deltaT, within indat
#'
#'#' @references Haddon, M., Mundy, C., and D. Tarbath (2008) Using an 
#'    inverse-logistic model to describe growth increments of blacklip 
#'    abalone (Haliotis rubra) in Tasmania. Fishery Bulletin 106:58-71
#'
#' @return a vector of predicted growth increments 
#' @export
#'
#' @examples
#' \dontrun{
#'   data(blackisland)
#'   param <- c(25, 130, 35, 3) # MaxDL, L50, delta, sigma
#'   predDL <- fabens(param,blackisland,initL="len1",delT="deltat")
#'   cbind(blackisland[1:15,"deltal"],predDl[1:15])
#' }
invl <- function(par,indat,initL="len1",delT="deltat") {
  preddl <- (par[1] * indat[,delT])/
    (1 + exp(log(19) * (indat[,initL] - par[2])/par[3]))
  return(preddl)
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

#' @title mature alternative logistic function commonly used for maturity
#'
#' @description mature a function 1/(1+(1/exp(a + b x sizeage))) which can
#'     be expressed as exp(a + b x sizeage)/(1 + exp(a + b x sizeage)).
#'     This describes a symmetric logistic curve that has the property
#'     Lm50 = -a/b and the interquartile distance is 2.log(3)/b.
#'     
#' @param a is the intercept of the exponential function usually -ve
#' @param b is the gradient of the exponential function
#' @param sizeage is a vector of lengths/ages for which the logistic 
#'     maturity value will be calculated
#' @return A vector of predicted proportion mature for the sizeage
#'
#' @export
#' @examples
#' a <- -14.383
#' b <- 0.146017
#' lens <- seq(2,210,2)
#' round(mature(a,b,sizeage=lens),5) # length based
#' round(mature(-2.5,0.95,0:25),5)   # age based
mature <- function(a,b,sizeage) {
  term <- exp(a + b * sizeage)
  ans <- term/(1 + term)
  return(ans)
} # end of mature

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
#' @param logobs the observed values log-transformed ready for 
#'     comparison with the log-predicted values from funk and pars.
#' @param ... required to allow funk to access its other arguments 
#'     without having to explicitly declare them in negLL. In the 
#'     example below, indat is passed via the ...
#'     
#' @return the negative log-likelihood using log-normal errors.
#' @export
#'
#' @examples
#' \dontrun{
#' data(abdat)
#' param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05))
#' negLL(pars=param,funk=simpspm,logobs=log(abdat[,"cpue"]),indat=abdat)
#' }
negLL <- function(pars,funk,logobs,...) {
  logpred <- funk(pars,...)
  pick <- which(is.na(logobs))
  if (length(pick) > 0) {
    LL <- -sum(dnorm(logobs[-pick],logpred[-pick],exp(tail(pars,1)),log=T))
  } else {
    LL <- -sum(dnorm(logobs,logpred,exp(tail(pars,1)),log=T))
  }
  return(LL)
} # end of negLL

#' @title negLL1 calculate log-normal log-likelihoods with a penalty
#'
#' @description negLL1 calculates log-normal negative log-likelihoods. It
#'     expects the input parameters to be log-transformed, so the funk used
#'     to calculate the log or the predicted values also needs to expect
#'     log-transformed parameters. In addition to estimating the negative 
#'     log-liklelihoods for log-normally distributed data it also places a 
#'     penalty on the first parameter if that parameter approaches very 
#'     close to zero; see the function penalty0. With SPM the first 
#'     parameter is the population growth rate r, which obviously 
#'     should never be negative. The use of penalty0 prevents this.
#'
#' @param pars the log-transformed parameters to be used in the funk for
#'     calculating the log of the predicted values against which the log
#'     observed values will be compared
#' @param funk the function used to calculate the log-predicted values of
#'     whatever variable is being used (eg. cpue, catches, etc.)
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
#' param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05))
#' negLL1(pars=param,funk=simpspm,logobs=log(abdat[,"cpue"]),indat=abdat)
#' }
negLL1 <- function(pars,funk,logobs,...) {
  logpred <- funk(pars,...)
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
#'     normally distributed variables. It requires a function, 'funk' 
#'     as an argument, that will calculate predicted values of a 
#'     variable from a vector of input values. In the example below the
#'     predicted values are lengths-at-age and the input data are ages.
#'     Only the arguments used within negNLL are listetd explicitly, 
#'     which leaves the data required to drive the funk to generate the
#'     predicted values for comparison with the observed to be passed 
#'     using the ..., so take care with spelling of the variable name
#'     required by whatever funk is being used.

#' @param pars a vector containing the parameters being used in funk, 
#'     plus the sigma, which is the standard deviation of the normal 
#'     random likelihoods in dnorm, an extra estimated parameter.
#'     Because negNLL refers explicitly to sigma, which must be the 
#'     last parameter in the vector, it must be listed in the arguments.
#' @param funk the function name that calculates the predicted values 
#'     from the independent values passed using the ellipsis.
#' @param observed the observed values of the variable that the model 
#'     will predict to compare with each of the input observed values.
#' @param ... required to allow funk to access its other input data
#'     without having to explicitly declare them in negNLL
#'     
#' @return the sum of the negative log-likelihoods using a normal PDF
#' @export
#'
#' @examples
#' \dontrun{
#'  data(kimura)
#'  pars <- c(Linf=56.0,K=0.4,t0=0.2,sigma=1.5)
#'  negNLL(pars,funk=vB,observed=kimura[,"length"],ages=kimura[,"age"])
#'  # should be 19.20821
#' }
negNLL <- function(pars,funk,observed,...) {
  predobs <- funk(pars,...)
  LL <- -sum(dnorm(observed,predobs,tail(pars,1),log=T))
  return(LL)
} # end of negNLL

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
#' @param pars a vector containing the parameters being used in funk, 
#'     plus an extra sigma which is the standard deviation of the normal 
#'     random likelihoods in dnorm
#' @param funk the function name that calculates the predicted values 
#'     from the independent values
#' @param independent the x-axis variable, that which in the model gives 
#'     rise to the predicted values of the dependent variable
#' @param dependent the observed values for comparison with the values 
#'     that the model will predict for each of the independent values.
#' @param initpar this defaults to the same as pars using all parameters
#' @param notfixed a vector identifying the indices of the parameters to 
#'     be fitted, which also defines those that will be fixed; defaults
#'     to all parameters. If some need to be kept constant so as to 
#'     generate a likelihood profile then omit their index from 
#'     'notfixed'.
#' @param ... required to allow funk to access its other parameters if
#'     required without having to explicitly declare them in negNLP
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

#' @title negnormL an alternative -log-likelihood for normal errors
#' 
#' @description negnormL is an alternative to negNLL to produce
#'     -ve log-likelihoods for nromal random errors. The two functions
#'     differ by how they reference the independent and dependent 
#'     variables. In negnormL only the observed (for comparison with the
#'     predicted) is identified. This is to allow for different column
#'     headings in one's data.frames
#'
#' @param par  the vector of parameters, with sigma, the standard
#'     deviation of the normal random deviates at the end.
#' @param funk the funk needed to generate the predicted values
#' @param indat the data.frame or matrix containing the obs and the 
#'     independent variable used by funk
#' @param obs identifies the column name or column number that contains 
#'     the observed data for comparison with the predicted from funk
#' @param ... the standard R for including extra parameters needed by
#'     funk but without having to be explicitly defined.
#'
#' @return the negative log-likelihood, for use in an optimizer, eg nlm
#' @export
#'
#' @examples
#' \dontrun{
#'  data(blackisland)
#'  param <- c(Linf=173.65,K=0.2666,sigma=3.6)
#'  negnormL(par=param,funk=fabens,indat=blackisland,obs="deltal",
#'           initL="len1",delT="deltat")   # should be 291.6809
#'  param2 <- c(21.03,130.94,40.65,3.162)  
#'  negnormL(par=param2,funk=invl,indat=blackisland,obs="deltal",
#'           initL="len1",delT="deltat")  # should be 277.5663 
#' }
negnormL <- function(par,funk,indat,obs="deltal",...) {
  predobs <- funk(par,indat,...)
  LL <- -sum(dnorm(indat[,obs],predobs,tail(par,1),log=T))
  return(LL)
}

#' @title negLLP  -ve log-likelihood for normally distributed variables
#'
#' @description negLLP calculates the negative log-likelihood for normally
#'     distributed variables. It assumes the presence of a function 'funk'
#'     that will calculate predicted values of a dependent 
#'     variable from a vector of independent values (logobs). 
#'     By having a separate vector of parameters in 'initpar' and a vector 
#'     of the indices of those parameters that will be fitted (notfixed) 
#'     it is possible to fit only a subset of parameters. This is useful 
#'     if generating a likelihood profile, or 
#'     setting up a likelihood ratio test. With more complex models it is 
#'     often a useful strategy to estimate the full number of parameters in 
#'     a series of phases, increasing the number being estimated each time 
#'     while keeping the rest fixed. 'negLLP' makes such phasing of the 
#'     fitting of a model to data possible.
#'     This function can be applied directly to log-transformed data for 
#'     log-normally distributed data, in which case funk would need to 
#'     generate log-transformed values. But can also be applied to 
#'     normally distributed data, in which case one would not 
#'     log-transform the data being input to the logobs argument and funk 
#'     would generated the linear-space predicted values.
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
#'  param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05)) 
#'  optmod <- nlm(f=negLLP,p=param,funk=simpspm,initpar=param,
#'               notfixed=c(1,2,3,4),indat=abdat,logobs=log(abdat$cpue),
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
#'                indat=abdat,logobs=log(abdat$cpue),notfixed=c(2:4), #value
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


#' @title ricker one version of the Ricker stock recruitment
#' 
#' @description ricker implements the Ricker stock recruitment
#'    equation where R = aBexp(-bxB), where R is the recruitment, 
#'    a and b are the parameters and B is the spawning biomass. 
#'    a is recruits-per-spawner at low stock levels, and b
#'    relates to the decline in recruitment as spawning biomass
#'    increases.
#'
#' @param p a vector of the a and b parameters
#' @param B a vector, possibly of length 1, of spawning biomass 
#'     levels
#'
#' @return a vector of length = to B of predicted recruitments
#' @export
#'
#' @examples
#' \dontrun{
#'   B <- 1:10000
#'   rec <- ricker(c(10,0.0002,B)
#'   plot1(B,rec,xlabel="SpB",ylabel="Recruitment")
#' }
ricker <- function(p,B) {
  R <- p[1] * B * exp(-p[2] * B)
  return(R)
} # end of ricker

#' @title srug is the Schnute and Richards Unified Growth Curve
#' 
#' @description srug implements the Schnute and Richards (1990) unified 
#'     growth curve that can be used to describe fish growth, 
#'     maturation, and survivorship data. It is a very general curve 
#'     that generalizes the classical logistic model used for maturity 
#'     as well the growth models by Gompertz (1825), von Bertalanffy 
#'     (1938), Richards (1959), Chapman (1961), and Schnute (1981). As
#'     with any asymmetric, multi-parameter model, it can be hard to 
#'     obtain a stable fit of this curve to data. Here the model is
#'     implemented to range between 0 - 1, if you want to use it to 
#'     describe growth then re-cast the function and add a fifth
#'     parameter to replace the 1.0 on top of the divisor.
#'
#' @param p a vector of four parameters begin Schnute and Richards' 
#'     a, b, c, and alpha, in that order.
#' @param sizeage the age or size data used to describe the maturity
#'     transition.
#' 
#' @references Schnute, J.T. and L.J. Richards (1990) A unified approach 
#'     to the analysis of fish growth, maturity, and survivorship data. 
#'     _Canadian Journal of Fisheries and Aquatic Science_ __47__:24-40
#'
#' @return A vector of predicted proportion mature (proportion of 1.0) 
#'     for the given parameters and the sizeage data
#' @export
#'
#' @examples
#' \dontrun{
#'  L <- seq(50,160,1)
#'  p <- c(a=0.07,b=0.2,c=1.0,alpha=100.0)
#'  predm <- srug(p=p,sizeage=L)
#' }
srug <- function(p,sizeage) { # p = a, b, c, alpha
  ans <- 1.0/((1.0 + p[4] * exp(-p[1]*sizeage^p[3]))^(1/p[2]))
  return(ans)  
}

#' @title ssq a generalized function for summing squared residuals
#'
#' @description ssq is a generalized function for summing squared
#'     residuals which is designed for ease of use in nlm (or
#'     optim, or nlminb). NAs are removed automatically. It assumes the 
#'     input of a predefined function 'funk' that will calculate 
#'     predicted values of a 'dependent' variable from a vector of 
#'     'independent' or observed values, for which one has observations. 
#'     The dependent or predicted values are compared with the observed 
#'     or 'independent' data, and the resulting SSQ returned. The use 
#'     of ... means this is a very general function but it does mean you 
#'     need to be very careful with placement and spelling of the input 
#'     variables required by whatever funk you are using. It is
#'     always best to explicitly name the arguments although the correct
#'     order will also work correctly.
#'
#' @param funk a function that uses a parameter vector and vector of
#'     observations to calculate predicted values, which are compared
#'     with the observed values to give the ssq and be returned
#' @param observed a vector containing the observed data (y-axis)
#' @param ...  required to allow funk to access its parameters and data 
#'     without having to explicitly declare them in ssq. Note that
#'     this means inside the ssq function the call to funk also needs
#'     to have the ellipsis, otherwise it will not be able to see those
#'     other arguments.
#'
#' @return a single number (scaler) that is the sum of squared
#'     residuals between the dep values and those calculated by funk
#' @export
#'
#' @examples
#' \dontrun{
#'   data(minnow)
#'   pars <- c(89, 0.1,-13)  # ssq = 83477.84
#'   # the use of ... obviously means we need to know the vB arguments
#'   ssq(funk=vB,observed=minnow$length,p=pars,ages=minnow$week)
#' }
ssq <- function(funk,observed, ...) {
  predval <- funk(...)
  return(sum((observed - predval)^2,na.rm=TRUE))
} # end of general ssq


#' @title vB calculates the predicted von Bertalanffy length at age
#'
#' @description vB calculates length at age for the von Bertalanffy curve.
#'
#' @param p is a vector the first three cells of which are Linf, K, and t0
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
vB <- function(p,ages) {
  return(p[1] * (1 - exp(-p[2]*(ages-p[3]))))
}







