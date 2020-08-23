

#' @title bce Baranov catch equation
#'
#' @description bce the Baranov Catch Equation. The total mortality of
#'     fish in an exploited population is made up of fish being killed 
#'     by fishing and others dying naturally. We use the bce to estimate 
#'     the catch (those killed by fishing). The bce has value because some 
#'     fish that would be expected to die naturally can be expected to 
#'     be caught and killed by fishing so estimating the catch is 
#'     slightly more complex than numbers of fish available times the 
#'     harvest rate, Nt x Ht. It is invariably better to use the 
#'     Baranov Catch Equation when calculating the expected catches.
#'
#' @param M instantaneous rate of natural mortality, assumed constant
#' @param Fat a vector of age-specific instantaneous rates of fishing
#'     mortality over the time period t, this is usually estimated by
#'     multiplying the fully selected F by the selectivity/availability
#'     at age.
#' @param Nt The population numbers-at-age at the start of time t
#' @param ages the ages 0:maxage used in the calculations
#' 
#' @return a matrix of surviving numbers-at age, total mortality-at-age, and 
#'     catch-at-age
#' @export
#'
#' @examples
#' age <- 0:25
#' Ft <- -log(1 - 0.2) # harvest rate of 0.2
#' Faa <- rep(Ft,length(age))
#' M <- 0.12
#' Nt <- 1000
#' bce(M,Fat=Faa,Nt,ages=age)   # should give 188.8862
bce <- function(M,Fat,Nt,ages) {
  nage <- length(ages)
  lFat <- length(Fat)
  if (lFat < nage) {
    lastF <- Fat[lFat]
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
#'    equation R = aB/(b + B), where R is the recruitment, a and
#'    b are the parameters and B is the spawning biomass. a is the 
#'    maximum recruitment level and b is the biomass required to
#'    generate 0.5 x maximum recruitment 
#'
#' @param p a vector of the a and b parameters
#' @param B a vector, possibly of length 1, of spawning biomass levels
#'
#' @return a vector, the same length as B, of the predicted recruitment(s)
#' @export
#'
#' @examples
#'   B <- 1:3000
#'   rec <- bh(c(1000,200),B)
#'   plot1(B,rec,xlab="SpB",ylab="Recruitment",lwd=2)
bh <- function(p,B) {
  rec <- (p[1] * B)/(p[2] + B)
  return(rec)
} # end of Beverton-Holt

#' @title discretelogistic example and figure 3.2 Discrete logistic model
#'
#' @description discretelogistic is an implementation of equation 3.1
#'     in the Simple Population Models chapter 3. It enables the 
#'     exploration of the dynamics of the Discrete logistic model, 
#'     based around the classical Schaefer model. 
#'     
#'     The time-series nature of population growth is clear from 
#'     the fact that Nt+1 is a function of Nt. One can thus expect serial
#'     correlation. Setting the r parameter to <= 1.0, would 
#'     generate monotonically damped equilibria. r values between 
#'     1 < r < 2.03 would generate damped oscillatory equilibria, r 
#'     values from 2.03 < r < 2.43 should generate stable limit cycles 
#'     on a cycle of 2, 2.43 < r < 2.54 gives stable limit cycles of 
#'     cycle 4, then 2.54 < r < 2.57 gives cycles > 4, and ~2.575 < r 
#'     gives chaos (though r = 2.63 appears to generate a repeat period
#'     of six!). discretelogistic should be used in conjunction with 
#'     plot, for which an S3 method has been defined plot.dynpop. As the
#'     dynamics are obviously sequential (i.e. n at t+1 is dependent 
#'     upon n at t) the last line of the dynamics is removed to avoid 
#'     an empty nt1 row. 
#'
#' @param r intrinsic rate of population increase; default = 0.5
#' @param K carrying capacity; default = 1000.0
#' @param N0 Initial population size; default=50.0 = 5 percent depletion. Note
#'     that the term 'depletion' can be confusing. Surely 50 remaining from 1000
#'     should be a depletion of 95 precent? But no, it is deemed to be the 
#'     complement of 5 percent. 
#' @param Ct annual catch default = 0.0
#' @param Yrs years of population growth, default=50
#' @param p the production curve asymmetry parameter. the default 
#'     value of 1.0 gives the classical Schaefer model. A value of 
#'     1e-08 approximates the Fox model where the mode is skewed to 
#'     the left of centre.
#'
#' @return invisibly returns a matrix of year, nt, and nt1
#' @export
#'
#' @examples
#'   discretelogistic(0.5,1000.0,25,0.0,50) # asymptotic
#'   discretelogistic(2.5,1000.0,25,0.0,50) # 4-phase stable limit
#'   ans <- discretelogistic(r=2.55,K=1000.0,N0=100,Ct=95.0,Yrs=100)
#'   plot(ans)  # uses an S3 plot method for dynpop objects
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
#' @description domed uses 6 parameters and a set of mean size or age
#'     classes to calculate a domed selectivity curve with a maximum 
#'     of 1.0 (rescaling can be done outside the function), but has 
#'     parameters for the selectivity of the initial and final 
#'     size/age classes. There is an ascending limb and a descending 
#'     limb with the potential of a plateau in between. The six 
#'     parameters are 1) the age/size where selectivity first becomes 
#'     1.0, 2) the size/age where selectivity first begins to decline, 
#'     3) the steepness of the ascending limb, 4) the steepness of the
#'     descending limb, 5) the selectivity of the first age/size
#'     class, and 6) the selectivity of the last age/size class.
#'
#' @param p a vector of six parameters.
#' @param L a vector of the mean of nL age/size classes
#'
#' @return a vector of selectivities
#' @export
#' 
#' @references Methot, R.D. and C.R, Wetzel (2013) Stock synthesis: A biological 
#'     and statistical framework for fish stock assessment and fishery management. 
#'     Supplementary material, Appendix A. Equs A1.30 onwards. 
#'     \emph{Fisheries Research} 142:86-99.
#'
#' @examples
#'   L <- seq(1,30,1)
#'   p <- c(10,11,16,33,-5,-2)
#'   sel <- domed(p,L)
#'   plot1(L,sel,xlab="Age",ylab="Selectivity",lwd=2)
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
#' @param par a vector of at least Linf, and K from the von Bertalanffy 
#'     growth curve
#' @param indat the matrix or data.frame of data columns containing at
#'     least the initial lengths and the deltaT, time intervals between
#'     tag release and recapture.
#' @param initL column name of the initial lengths within indat, 
#'     default="l1"
#' @param delT column name of the time interval, deltaT, within indat,
#'     default="dt"
#'
#' @return a vector of predicted growth increments 
#' @export
#'
#' @examples
#'  data(blackisland)
#'  oldpar <- par(no.readonly=TRUE)
#'  plot(blackisland$l1,blackisland$dl,type="p",pch=16,
#'  xlab="Initial Length mm",ylab="Growth Increment mm",
#'  panel.first=grid())
#'  abline(h=0)
#'  param <- c(170, 0.3, 4.0) # Linf, K, sigma
#'  predDL <- fabens(param,blackisland,initL="l1",delT="dt")
#'  lines(blackisland$l1,predDL,col=2,lwd=2)  
#'  par(oldpar) 
fabens <- function(par,indat,initL="l1",delT="dt") {
  preddL <- (par[1] - indat[,initL])*(1 - exp(-(par[2] * indat[,delT])))
  return(preddL)
}

#' @title Gz calculates predicted Gompertz length-at-age growth curve
#'
#' @description Gz calculates length-at-age for the Gompertz curve.
#'     This curve can have an inflection near the origin as well
#'     as at an age where growth slows due to maturity occurring.
#'
#' @param p is a vector the first three values of which are a, b, c, for
#'     the Gz curve.
#' @param ages is a vector of ages; could be a single number
#'
#' @return a vector of predicted lengths for a vector of ages in 'ages'
#' @export
#'
#' @examples
#'  ages <- seq(0,20,1)   # sigma is ignored here
#'  oldpar <- par(no.readonly=TRUE)
#'  pars <- c(a=26.0,b=2.8,c=-0.65,sigma=1.0) # a, b, c, sigma;
#'  plot1(ages,Gz(pars,ages),xlab="Age",ylab="Length",lwd=2)
#'  par(oldpar)
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
#' @param par a vector of at least MaxDL, L50, and delta from the inverse 
#'     logistic growth curve
#' @param indat the matrix or data.frame of data columns containing at
#'     least the initial lengths and the deltaT, time intervals between
#'     tag release and recapture.
#' @param initL column name of the initial lengths within indat, 
#'     default="l1"
#' @param delT column name of the time interval, deltaT, within indat, 
#'     default="dt"
#'
#' @references Haddon, M., Mundy, C., and D. Tarbath (2008) Using an 
#'    inverse-logistic model to describe growth increments of blacklip 
#'    abalone (Haliotis rubra) in Tasmania. \emph{Fishery Bulletin} 
#'    106:58-71
#'
#' @return a vector of predicted growth increments 
#' @export
#'
#' @examples
#'  data(blackisland)
#'  oldpar <- par(no.readonly=TRUE)
#'  plot(blackisland$l1,blackisland$dl,type="p",pch=16,
#'  xlab="Initial Length mm",ylab="Growth Increment mm",panel.first=grid())
#'  abline(h=0)
#'  param <- c(25, 130, 35, 3) # MaxDL, L50, delta, sigma
#'  predDL <- invl(param,blackisland,initL="l1",delT="dt")
#'  lines(blackisland$l1,predDL,col=2,lwd=2) 
#'  par(oldpar)
invl <- function(par,indat,initL="l1",delT="dt") {
  preddl <- (par[1] * indat[,delT])/
    (1 + exp(log(19) * (indat[,initL] - par[2])/par[3]))
  return(preddl)
}

#' @title logist Logistic selectivity function
#'
#' @description logist calculates a Logistic curve that can be used as a
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
#' in50 <- 100.0
#' deltaS <- 8.0
#' lens <- seq(2,210,2)
#' select <- logist(inL50=in50,delta=deltaS,depend=lens)
#' selectk <- logist(in50,deltaS,lens,knifeedge=TRUE)
#' round(cbind(lens[35:70],select[35:70],selectk[35:70]),5)
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

#' @title mm calculates the predicted Michaelis-Menton length-at-age
#'
#' @description mm calculates length-at-age for the generalized 
#'     Michaelis-Menton curve. The equation being (a x ages)/(b + ages^c).
#'
#' @param p is a vector the first three cells of which are a, b, c
#'    for the mm curve.
#' @param ages is a vector of ages; could be a single number
#'
#' @return a vector of predicted lengths for a vector of ages in 'ages'
#' @export
#'
#' @examples
#'  ages <- seq(0,20,1)    # sigma is ignored here
#'  pars <- c(a=23.0,b=1.0,c=1.0,sigma=1.0) # a, b, c, sigma
#'  plot1(ages,mm(pars,ages),xlab="Age",ylab="Length",lwd=2)
mm <- function(p, ages) {
  return((p[1]*ages)/(p[2] + ages^p[3]))
}

#' @title mnnegLL generic multinomial negative log-likelihoods
#' 
#' @description mnnegLL a generic multinomial negative log-likelihood 
#'     that requires observed frequencies and predicted frequencies, 
#'     although the predicted frequencies could also be the final 
#'     proportions, as long as they sum to one. It checks that the 
#'     number of predicted values matches the number of observed values
#'
#' @param obs the original observed frequencies
#' @param predf the predicted frequencies or proportions
#'
#' @return a single scalar value
#' @export
#'
#' @examples
#'   obs <- c(0,0,6,12,35,40,29,23,13,7,10,14,11,16,11,11,9,8,5,2,0)
#'   predf <- c(0.1,0.9,4.5,14.4,29.7,39.9,35.2,21.3,10.9,8.0,9.5,12.1,
#'              14.1,14.7,13.7,11.5,8.6,5.8,3.5,1.9,0.9) 
#'  mnnegLL(obs,predf)   # should be  705.5333
mnnegLL <- function(obs,predf) { 
  if (length(predf) != length(obs)) {
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
#' @param logobs the observed values, log-transformed, ready for 
#'     comparison with the log-predicted values from funk and pars.
#' @param ... required to allow funk to access its other arguments 
#'     without having to explicitly declare them in negLL. In the 
#'     example below, indat is passed via the ...
#'     
#' @return the negative log-likelihood using log-normal errors.
#' @export
#'
#' @examples
#' data(abdat)  # expect an answer of -31.65035
#' param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05))
#' negLL(pars=param,funk=simpspm,logobs=log(abdat[,"cpue"]),indat=abdat)
negLL <- function(pars,funk,logobs,...) {
  npar <- length(pars)
  logpred <- funk(pars,...)
  pick <- which(is.na(logobs))
  if (length(pick) > 0) {
    LL <- -sum(dnorm(logobs[-pick],logpred[-pick],exp(pars[npar]),log=T))
  } else {
    LL <- -sum(dnorm(logobs,logpred,exp(pars[npar]),log=T))
  }
  return(LL)
} # end of negLL

#' @title negLL1 calculate log-normal log-likelihoods with a penalty
#'
#' @description negLL1 calculates log-normal negative log-likelihoods. It
#'     expects the input parameters to be log-transformed, so the funk used
#'     to calculate the log or the predicted values also needs to expect
#'     log-transformed parameters. In addition to estimating the negative 
#'     log-likelihoods for log-normally distributed data it also places a 
#'     penalty on the first parameter if that parameter approaches very 
#'     close to zero; see the help page for penalty0. With SPM the first 
#'     parameter is the population growth rate r, which obviously 
#'     should never be negative. The use of penalty0 prevents this happening.
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
#' data(abdat)  #expect an answer of -31.65035
#' param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05))
#' negLL1(pars=param,funk=simpspm,logobs=log(abdat[,"cpue"]),indat=abdat)
negLL1 <- function(pars,funk,logobs,...) {
  npar <- length(pars)
  logpred <- funk(pars,...)
  pick <- which(is.na(logobs))
  if (length(pick) > 0) {
    LL <- -sum(dnorm(logobs[-pick],logpred[-pick],exp(pars[npar]),log=T))
  } else {
    LL <- -sum(dnorm(logobs,logpred,exp(pars[npar]),log=T))
  }
  LL <- LL + penalty0(exp(pars[1]))
  return(LL)
} # end of negLL1

#' @title negLLM -ve log-normal likelihoods for multiple index time-series
#' 
#' @description negLLM we have negLL and negLL1 for use when using -ve
#'     log-likelihoods to fit surplus production models that only have 
#'     a single index of relative abundance, but there are many fisheries
#'     that have more than one index of relative abundance. negLLM is
#'     for those cases that have multiple (M) time-series of such indices. It
#'     is used in conjunction with simpspmM and spmCE.
#'
#' @param pars the log-transformed parameter starting points. For a 
#'     surplus production model these are r, K, Binit (if initial 
#'     depletion is likely, otherwise omit this and it will be set =K
#'     inside the function), then as many sigma values as there are 
#'     time-series of abundance indices; these are the associated standard 
#'     deviations of the log-normal residuals.
#' @param funk the function that generates the predicted cpue values.
#'     for multiple time-series in a SPM use simpspmM
#' @param logobs the log-transformed observed cpue columns in indat,
#'     the data needed by funk, tranferred inside the ...
#' @param indat the fisheries data used in the analysis 
#' @param index the prefix of the columns of each of the indices, 
#'     defaults to cpue
#' @param harvpen default = TRUE, which sets a penalty1 on each of the 
#'     implied harvest rates to ensure we do not get harvest rates > 1.0
#' @param ... the continuation ellipsis to allow the transfer of other
#'     arguments required by funk. The argument 'schaefer' in the example below
#'     illustrates such usage.
#'
#' @return a single scalar as the -ve log-likelihood of the input data
#' @export
#'
#' @examples
#'  data(twoindex)
#'  fish <- as.matrix(twoindex)
#'  pars <- log(c(0.04,155000,0.4,0.3))
#'  bestSP <- nlm(f=negLLM,p=pars,funk=simpspmM,indat=fish,
#'              schaefer=TRUE,logobs=log(fish[,c("cpue1","cpue2")]),
#'              steptol=1e-06,harvpen=TRUE)
#'  outfit(bestSP,digits=5,title="negLLM example") #optimum solution
#'  answer <- plotspmmod(bestSP$estimate,indat=fish,
#'                       plotprod=TRUE,maxy=3.4)
negLLM <- function(pars,funk,logobs,indat,index="cpue",harvpen=TRUE,...) {
  npar <- length(pars)
  logpred <- funk(pars,indat)
  nce <- ncol(logobs)
  sig <- pars[(npar- nce + 1):npar]
  LL <- 0
  for (i in 1:nce) {  #  i=1
    pick <- which(is.na(logobs[,i]))
    if (length(pick) > 0) {
      LL <- LL + (-sum(dnorm(logobs[-pick,i],logpred[-pick,i],exp(sig[i]),log=T)))
    } else {
      LL <- LL + (-sum(dnorm(logobs[,i],logpred[,i],exp(sig[i]),log=T)))
    }
  }
  hpen <- 0.0
  if (harvpen) # apply a penalty on harvest rates that approach 1.0
    hpen <- sum(penalty1(spmCE(pars,indat)$outmat[,"Harvest"]),na.rm=TRUE)
  LL <- LL + penalty0(exp(pars[1])) + hpen
  return(LL)
} # end of negLLM

#' @title negNLL  -ve log-likelihood for normally distributed variables
#'
#' @description negNLL - Calculates the negative log-likelihood for
#'     normally distributed variables. It requires a function, 'funk' 
#'     as an argument, that will calculate predicted values of a 
#'     variable from a vector of input values. In the example below the
#'     predicted values are lengths-at-age and the input data are ages.
#'     Only the arguments used within negNLL are listed explicitly, 
#'     which leaves the data required to drive the funk to generate the
#'     predicted values for comparison with the observed to be passed 
#'     using the ..., so take care with spelling of the variable name
#'     required by whatever funk is being used.
#'     
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
#'     without having to explicitly define them in negNLL
#'     
#' @return the sum of the negative log-likelihoods using a normal PDF
#' @export
#'
#' @examples
#'  data(minnow)
#'  pars <- c(89, 0.01,-15,3.75)  # ssq    = 785.6183
#'  age <- minnow$week            # negNLL = 151.1713
#'  ssq(funk=vB,observed=minnow$length,p=pars,ages=age)
#'  negNLL(pars,funk=vB,observed=minnow$length,ages=age)
#'  oldpar <- par(no.readonly=TRUE)
#'  plot1(age,vB(pars,age),xlab="Age",ylab="Length",lwd=2)
#'  points(age,minnow$length,pch=16,cex=1.2)
#'  par(oldpar)
negNLL <- function(pars,funk,observed,...) {
  npar <- length(pars)
  predobs <- funk(pars,...)
  LL <- -sum(dnorm(observed,predobs,pars[npar],log=T))
  return(LL)
} # end of negNLL

#' @title negnormL an alternative -log-likelihood for normal errors
#' 
#' @description negnormL is an alternative to negNLL to produce
#'     -ve log-likelihoods for normal random errors, allowing for the
#'     sigma parameter to vary as a function of the predicted value. 
#'     In negnormL only one needs both a funk and a funksig, the former 
#'     to calculate the predicted values using funk, and funksig to
#'     calculate the changing sigma values relative to the predicted
#'     values. The example code illustrates an example funksig that
#'     does nothing to the sigma value. See the chapter on Static Models to see
#'     further examples.
#'
#' @param pars  the vector of parameters, with sigma, the standard
#'     deviation of the normal random deviates at the end.
#' @param funk the funk needed to generate the predicted values
#' @param funksig the function used to calculate the sigma value based
#'     on the last parameter (=constant sigma) and the predicted values
#' @param indat the data.frame or matrix containing the obs and the 
#'     independent variable used by funk
#' @param obs identifies the column name or column number that contains 
#'     the observed data for comparison with the predicted from funk,
#'     the default="dl"
#' @param ... the standard R for including extra parameters needed by
#'     funk but without having to be explicitly defined.
#'
#' @return the negative log-likelihood, for use in an optimizer, eg nlm
#' @export
#'
#' @examples
#'  data(blackisland)
#'  param <- c(Linf=173.65,K=0.2666,sigma=3.6)
#'  sigfunk <- function(pars,predobs) return(tail(pars,1))
#'  negnormL(par=param,funk=fabens,funksig=sigfunk,indat=blackisland,
#'           obs="dl",initL="l1",delT="dt")   # should be 291.1757
#'  param2 <- c(21.03,130.94,40.65,3.162)  
#'  negnormL(par=param2,funk=invl,funksig=sigfunk,indat=blackisland,
#'            obs="dl",initL="l1",delT="dt")  # should be 277.0186
negnormL <- function(pars,funk,funksig,indat,obs="dl",...){
  predobs <- funk(pars,indat,...) #if sigma not a constant then
  sigma <- funksig(pars,predobs) #funksig produces a vector
  LL <- -sum(dnorm(indat[,obs],predobs,sigma,log=T))
  return(LL)
} # end of negnormL

#' @title negLLP  -ve log-likelihood for normally distributed variables
#'
#' @description negLLP calculates the negative log-likelihood for normally
#'     distributed variables allowing for some parameters to remain fixed. 
#'     It assumes the presence of a function 'funk' that will calculate 
#'     predicted values of a dependent variable from a vector of 
#'     independent values (logobs). By having a separate vector of 
#'     parameters in 'initpar' and a vector of the indices of those 
#'     parameters that will be fitted (notfixed) it is possible to fit 
#'     only a subset of parameters. This is useful, for example, if 
#'     generating a likelihood profile, or setting up a likelihood 
#'     ratio test. With more complex models it is often a useful 
#'     strategy to estimate the full number of parameters in a 
#'     sequence of phases, increasing the number being estimated each 
#'     time while keeping the rest fixed. 'negLLP' makes such phasing 
#'     of the fitting of a model to data possible.
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
#'  data(abdat)
#'  param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05)) 
#'  optmod <- nlm(f=negLLP,p=param,funk=simpspm,initpar=param,
#'               notfixed=c(1,2,3,4),indat=abdat,logobs=log(abdat$cpue))
#'  outfit(optmod,backtran= TRUE) #backtran=TRUE is default anyway
#'   
#'  rval <- seq(0.325,0.45,0.0125)  # set up the test sequence
#'  columns <- c("r","K","Binit","sigma","-veLL")
#'  result <- matrix(0,nrow=11,ncol=5,dimnames=list(rval,columns))
#'  profest <- c(r= 0.32,K=11500,Binit=4250,sigma=0.05) # end of sequence
#'  for (i in 1:11) { 
#'    param <- log(c(rval[i],profest[2:4])) #recycle the profest values to
#'    parinit <- param    # improve the stability of nlm as the r value
#'    bestmodP <- nlm(f=negLLP,p=param,funk=simpspm,initpar=parinit, #changes
#'                    indat=abdat,logobs=log(abdat$cpue),notfixed=c(2:4))
#'    bestest <- exp(bestmodP$estimate)     
#'    result[i,] <- c(bestest,bestmodP$minimum)  # store each result
#'  }
#'  result   #Now you can plot -veLL againt r values for the profile
#'  # parset()
#'  # plot(result[,"r"],result[,"-veLL"],type="l",lwd=2,panel.first=grid())
#'  # points(result[,"r"],result[,"-veLL"],pch=16,cex=1.1)
negLLP <- function(pars, funk, indat, logobs, initpar=pars,
                   notfixed=c(1:length(pars)),...) {
  usepar <- initpar
  usepar[notfixed] <- pars[notfixed]
  npar <- length(usepar)
  logpred <- funk(usepar,indat,...)
  pick <- which(is.na(logobs))
  if (length(pick) > 0) {
    LL <- -sum(dnorm(logobs[-pick],logpred[-pick],exp(pars[npar]),log=T))
  } else {
    LL <- -sum(dnorm(logobs,logpred,exp(pars[npar]),log=T))
  }
  return(LL)
} # end of negLLP


#' @title ricker one version of the Ricker stock recruitment
#' 
#' @description ricker implements the Ricker stock recruitment
#'    equation where R = aBexp(-bxB), R is the recruitment, 
#'    a and b are the parameters and B is the spawning biomass. 
#'    a is recruits-per-spawner at low stock levels, and b
#'    relates to the decline in recruitment as spawning biomass
#'    increases.
#'
#' @param p a vector of length two of the a and b parameters
#' @param B a vector, possibly of length 1, of spawning biomass 
#'     levels
#'
#' @return a vector of length = to B of predicted recruitments
#' @export
#'
#' @examples
#'   B <- 1:10000
#'   rec <- ricker(c(10,0.0002),B)
#'   oldpar <- par(no.readonly=TRUE)
#'   plot1(B,rec,xlab="SpB",ylab="Recruitment",lwd=2)
#'   par(oldpar)
ricker <- function(p,B) {
  R <- p[1] * B * exp(-p[2] * B)
  return(R)
} # end of ricker

#' @title srug is the Schnute and Richards Unified Growth Curve
#' 
#' @description srug implements the Schnute and Richards (1990) unified 
#'     growth curve that can be used to describe fish growth, 
#'     maturation, and survivorship data. It is a curve 
#'     that generalizes the classical logistic model used for maturity 
#'     as well the growth models by Gompertz (1825), von Bertalanffy 
#'     (1938), Richards (1959), Chapman (1961), and Schnute (1981). As
#'     with any asymmetric, multi-parameter model, it can be hard to 
#'     obtain a stable fit of this curve to data. Here the model is
#'     implemented to range between 0 - 1, if you want to use it to 
#'     describe growth then re-cast the function and add a fifth
#'     parameter to replace the 1.0 on top of the divisor. The main point of the
#'     curve, however, was to demonstrate how the different equations were 
#'     related to one another. In working situations it is most efficient 
#'     to use the original, simpler, curve/qeuation.
#'
#' @param p a vector of four parameters begin Schnute and Richards' 
#'     a, b, c, and alpha, in that order.
#' @param sizeage the age or size data used to describe the maturity
#'     transition.
#' 
#' @references Schnute, J.T. and L.J. Richards (1990) A unified approach 
#'     to the analysis of fish growth, maturity, and survivorship data. 
#'     \emph{Canadian Journal of Fisheries and Aquatic Science} 47:24-40
#'
#' @return A vector of predicted proportion mature (proportion of 1.0) 
#'     for the given parameters and the sizeage data
#' @export
#'
#' @examples
#'  L <- seq(50,160,1)
#'  p <- c(a=0.07,b=0.2,c=1.0,alpha=100.0)
#'  predR <- srug(p=p,sizeage=L) # proportion of total
#'  oldpar <- par(no.readonly=TRUE)
#'  plot1(L,predR,xlab="Length",ylab="Prop of Recruitment")
#'  abline(h=0.5) #visually confirm asymmetry
#'  par(oldpar)
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
#'     other arguments. It is vital to spell the names of funk's
#'     arguments correctly as errors are not always announced and 
#'     will obviously lead to misleading outputs.
#'
#' @return a single number (scaler) that is the sum of squared
#'     residuals between the dep values and those calculated by funk
#' @export
#'
#' @examples
#'   data(minnow)  # remember -13 is only 3+ months
#'   pars <- c(89, 0.1,-13)  # ssq = 83477.84
#'   ssq(funk=vB,observed=minnow$length,p=pars,ages=minnow$week)
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
#' ages <- seq(0,20,1)   # sigma is ignored here
#' pars <- c(Linf=50,K=0.3,t0=-1.0,sigma=1.0) # Linf, K, t0, sigma
#' oldpar <- par(no.readonly=TRUE)
#' plot1(ages,vB(pars,ages),xlab="Age",ylab="Length",lwd=2)
#' par(oldpar)
vB <- function(p,ages) {
  return(p[1] * (1 - exp(-p[2]*(ages-p[3]))))
}







