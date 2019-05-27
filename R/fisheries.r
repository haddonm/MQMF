

#' @title bce Baranov catch equation
#'
#' @description bce the Baranov Catch Equation
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
  columns <- c("Nt","Numbers Dying","Catch")
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
#'     classical Schaefer model. By setting the r parameter to <= 1.0, one
#'     would generate monotonically damped equilibria. r values between 
#'     1 < r < 2.03 would generate damped oscillatory equilibria, r values 
#'     from 2.03 < r < 2.43 should generate stable limit cycles on a cycle
#'     of 2., 2.43 < r < 2.54 gives stable limit cycles of cycle 4, then
#'     2.54 < r < 2.57 gives cycles > 4, and ~2.575 < r gives chaos.
#'
#' @param r intrinsic rate of population increase; default = 0.5
#' @param K carrying capacity; default = 1000
#' @param N0 Initial population size; default = 50
#' @param Ct annual catch default = 0.0
#' @param Yrs years of population growth
#' @param p the asymmetry parameter. the default value of 1.0 leads
#'     to the classical Schaefer model. A value of 1e-08 approximates
#'     the Fox model where the mode is skewed to the left of centre.
#' @param outplot default = TRUE; should the dynamics be plotted?
#' @param title If using the default = "" it is filled with r, K, Ct,
#'     N0, and p. But this can be replaced with text if required.
#'
#' @return if outplot = TRUE it generates a plot and alters default
#'     par settings. In addition, invisibly returns a matrix of year,
#'     Nt, and Nt+1
#' @export
#'
#' @examples
#' \dontrun{
#'   discretelogistic(0.5,1000.0,25,0.0,50)
#'   ans <- discretelogistic(r=2.55,K=1000.0,N0=100,Ct=95.0,Yrs=100)
#'   head(ans,20)
#' }
discretelogistic <- function(r=0.5,K=1000.0,N0=50.0,Ct=0.0,Yrs=50,
                             p=1.0,outplot=TRUE,title="") {
   fntsze <- 1.1
   Time <- seq(1,Yrs,1)
   pop <- numeric(Yrs)
   pop[1] <- N0
   for (year in 2:Yrs) {
      Bt <- pop[year-1]
      pop[year] <- max((Bt + (r*Bt/p)*(1-(Bt/K)^p) - Ct),0)
   }
   pop2 <- pop[2:Yrs]
   if (outplot) {
      if (nchar(title) == 0) {
         title <- paste0("r= ",r," K= ",K," Ct= ",Ct, " N0= ",N0,
                         " p= ",p)
      }
      ymax <- max(pop)*1.025
      x <- c(0,ymax)
      par(mfrow=c(1,2),mai=c(0.45,0.45,0.05,0.1),oma=c(0.0,0,2.0,0.0))
      par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
      plot(Time,pop,type="l",col=2,lwd=2,ylim=c(0,ymax),yaxs="i",
           panel.first = grid(),xlab="Time",ylab="Population Size")
      mtext("Population Dynamics",side=3,line=0.0,cex=fntsze,font=7)
      plot(pop[1:(Yrs-1)],pop2,type="p",pch=1,lwd=1.5,cex=1.2,
           yaxs="i",xlim=c(0,ymax),ylim=c(0,ymax),panel.first = grid(),
           xlab="Population Nt",ylab="Population Nt+1")
      begin <- trunc(Yrs * 0.8)      # final 20%
      lines(x,x,lwd=2,col="grey")
      points(pop[begin:(Yrs-1)],pop2[begin:(Yrs-1)],pch=16,col=2,cex=1.1)
      mtext("Phase Plot",side=3,line=0.0,cex=fntsze,font=7,outer=FALSE)
      mtext(title,side=3,line=1.0,cex=fntsze,font=7,outer=TRUE)
   }
   out <- cbind(Year=Time,Nt=pop,Nt1=c(pop2,NA))
   rownames(out) <- Time
   return(invisible(out))
} # End of discretelogistic


#' @title Gz calculates the predicted Gompertz length at age growth curve
#'
#' @description Gz calculates length at age for the Gompertz curve.
#'
#' @param p is a vector the first three cells of which are a, b, c, for
#'     the Gz curve.
#' @param ages is a vector of ages; could be a single number
#'
#' @return a vector of predicted lengths for the vector of ages in 'ages'
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
#'     selectivity function, or maturity curve, of wherever a logistic is
#'     required. This version uses the logistic function
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

#' @title MaA an alternative logistic function commonly used for maturity
#'
#' @description MaA - the logistic function exp(a+bxdepend)/(1+exp(a+bxdepend)),
#'     which can also be expressed as 1/(1+(1/exp(a + b x depend))). This has
#'     the property that the SM50 = -a/b and the interquartile distance is
#'     2.Ln(3)/b.
#' @param ina is the intercept of the exponential function
#' @param inb is the gradient of the exponential function
#' @param depend is a vector of lengths/ages for which the logistic maturity
#'     value will be calculated
#' @return A vector of length(depend) containing the predicted maturity values
#'
#' @export
#' @examples
#' a <- -14.383
#' b <- 0.146017
#' lens <- seq(2,210,2)
#' round(MaA(a,b,depend=lens),5) # length based
#' round(MaA(-2.5,0.95,0:25),5)   # age based
MaA <- function(ina,inb,depend) {
  ans <- exp(ina+inb*depend)/(1+exp(ina+inb*depend))
  return(ans)
} # end of Maturity at age

#' @title mm calculates the predicted Michaelis-Menton length at age
#'
#' @description mm calculates length at age for the generalized Michaelis-
#'     Menton curve.
#'
#' @param p is a vector the first three cells of which are a, b, c
#'    for the mm curve.
#' @param ages is a vector of ages; could be a single number
#'
#' @return a vector of predicted lengths for the vector of ages in 'ages'
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
#' @description mnnegLL a generic multinomial negative log-likelihood that
#'     requires observed frequencies and predicted frequencies, although
#'     the predicted frequencies could also be the final proportions, as 
#'     long as they summed to one. It checks that the number of predicted
#'     values matches the number of observed values
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
#'     expects the input parameters to be log-transformed, so the funk used
#'     to calculate the log or the predicted values also needs to expect
#'     log-transformed parameters
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
#' negLL(pars=param,funk=simpspm,indat=fish,logobs=log(fish[,"cpue"]))
#' }
negLL <- function(pars,funk,indat,logobs,...) {
  logpred <- funk(pars,indat,...)
  LL <- -sum(dnorm(logobs,logpred,exp(tail(pars,1)),log=T))
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
  LL <- -sum(dnorm(logobs,logpred,exp(tail(pars,1)),log=T))
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
#' @param independent the x-axis variable, that which in the model gives rise
#'   to the predicted values to compare with the observed 'dependent' variable
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
#'     parameters in 'initpar' and a vector of the indices of those parameters
#'     that will be fitted it is possible to fit only a subset of parameters.
#'     This is useful if generating a likelihood profile, or setting up a
#'     likelihood ratio test. With more complex models it is often a useful
#'     strategy to estimate the full number of parameters in a series of
#'     phases, increasing the number being estimated each time while keeping
#'     the rest fixed. 'negNLP' makes such phasing of the fitting of a model
#'     to data possible.
#'     This function can be applied directly to normally distributed data or to
#'     log-transformed data for log-normally distributed data.
#'     There is a slight cost in terms of time such that
#'     using negNLP takes approximate 0.4 milliseconds longer than when using
#'     negNL; and that is with either 13 or 1300 observations.
#'
#' @param pars a vector containing the parameters being used in funk, plus
#'     an extra sigma which is the standard deviation of the normal random
#'     likelihoods in dnorm
#' @param funk the function name that calculates the predicted values from
#'     the independent values
#' @param independent the x-axis variable, that which in the model gives rise
#'     to the predicted values of the dependent variable
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
#'
#' @param pars the parameters of the SPM are either c(r, K, Binit, sigma),
#'     Binit is required if the fishery data starts after the stock has been
#'     depleted. Each parameter must be log-transformed for improved model
#'     stability and is transformed inside simpspm.
#' @param indat the data which needs to include year, catch, and cpue. 
#' @param schaefer a logical value determining whether the spm is to be a
#'     simple Schaefer model (p=1) or approximately a Fox model (p=1e-08). The
#'     default is TRUE = Schaefer model
#' @param depleted default = TRUE; implies the fishery was already depleted
#'     once fishery data started to be collected. Implies that there needs
#'     to be a Binit in third position among the input parameters
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
simpspm <- function(pars, indat,schaefer=TRUE,depleted=TRUE,  
                    year="year",cats="catch",index="cpue") { 
  # pars=pars;indat=fish;schaefer=TRUE;depleted=FALSE;year="year";cats="catch";index="cpue"
  nyrs <- length(indat[,year])
  biom <- numeric(nyrs+1)
  catch <- indat[,cats]
  ep <- exp(pars) # par contains at least log of (r,K, and sigma)
  biom[1] <- ep[2]  
  if (depleted) biom[1] <- ep[3] # Binit should be before sigma
  #p is the location of mode parameter 1 = Schaefer, 1e-8 ~ Fox model
  if(schaefer) p <- 1 else p <- 1e-8
  for (yr in 1:nyrs) { # fill biom using Bt as an interim step
    Bt <- biom[yr]  # avoid negative biomass using a max statement
    biom[yr+1] <- max(Bt + ((ep[1]/p)*Bt*(1-(Bt/ep[2])^p)-catch[yr]),40)
  }
  pick <- which(indat[,"cpue"] > 0)
  qval <- exp(mean(log(indat[pick,"cpue"]/biom[pick])))
  return(log(biom[pick] * qval))  # the log of predicted cpue
} # end of simpspm generates log-predicted cpue

#' @title simpspmM simply calculates the predicted CE for an SPM
#'
#' @description simpspmM calculates the predicted CPUE for an SPM model. It
#'     assumes that there is a variable called 'p' in the global environment
#'     and this 'p' variable determines the asymmetry of the production curve.
#'     If p = 1.0 then the SPM is the Schaefer model, if it is 1e-8 it
#'     approximates the Fox model.
#'
#' @param par the parameters of the SPM = r, K, a q for each column of cpue,
#'     a sigma for each cpue, and Binit if fishery depleted to start with. Each
#'     parameter is in log space and is transformed inside simpspmM
#' @param indat the data which needs to include year, catch, and cpue. The
#'    latter should have a separate column for each fleet, with a column name
#'    beginning with cpue or whatever name you put in index (see below) for
#'    example cpue1, cpue2,etc.
#' @param schaefer a logical value determining whether the spm is to be a
#'     simple Schaefer model (p=1) or approximately a Fox model (p=1e-08). The
#'     default is TRUE
#' @param year the column name within indat containing the years
#' @param cats the cloumn name within indat containing the catches
#' @param index the prefix in the column names given to the indices of relative
#'     abundance used, perhaps 'cpue' as in cpueTW, cpueAL, etc. grep is used
#'     to search for columns containing this prefix to identify whether there
#'     are more than one column of cpue data.
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
  # par=log(pars); indat=fish; schaefer=TRUE;year="year";cats="catch";index="cpue"
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
  if (length(par) > npar) biom[1] <- exp(par[npar+1]) # BiNit at end of pars
  #p is the location of mode parameter 1 = Schaefer, 1e-8 ~ Fox model
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

#' @title spm - calculates the dynamics of a Schaefer or Fox model
#'
#' @description spm calculates the dynamics using a Schaefer of Fox model.
#'     The outputs include  predicted Biomass, year, catch, cpue, predicted
#'     cpue, contributions to q, ssq, and depletion levels. Generally it
#'     would be more sensible to use simpspm when fitting a Schaefer model 
#'     or a Fox model
#'     as those functions are designed to generate only the predicted cpue
#'     required by the functions ssq and negLL, but the example shows how it
#'     could be used. the function spm is used inside 'plotModel'
#'     and could be used alone, to generate a fullist of model outputs
#'     after the model has been fitted. spm is designed when working with a
#'     single vector of an index of relative abudnance. If there are 
#'     multiple vectors then use spmCE
#'
#' @param inp a vector of 3 or 4 model parameters (r,K,sigma) or 
#'     (r,K,Binit,sigma), you would use the latter if it was suspected that 
#'     the fishery data started after some initial depletion had occurred.
#'     The sigma is an estimate of the variation of the cpue through time.
#'     This is required but is only used when fitting the model using
#'     negative log-likelihoods.
#' @param indat a matrix with at least columns 'year', 'catch', and 'cpue'
#' @param schaefer a logical value determining whether the spm is to be a
#'     simple Schaefer model (p=1) or approximately a Fox model (p=1e-08). The
#'     default is TRUE
#' @param depleted default = TRUE; implies the fishery was already depleted
#'     once fishery data started to be collected. Implies that there needs
#'     to be a Binit in third position among the input parameters
#' @param year the name of the year variable (in case your dataset names it 
#'     fishingyearinwhichthecatchwastaken)
#' @param cats the name of the catch variable, again this is for generality
#' @param index the name of the cpue variable, for generality
#'
#' @return a list of five objects; outmat the matrix with the dynamics results,
#'     q catchability, msy the maximum sustainable yield, the parameter values,
#'     and sumout, which contains r, K, B0, msy, p, q, Depl, FinalB, and InitDepl
#' @export
#'
#' @examples
#' \dontrun{
#' year <- 1985:2008
#' catch <- c(1018,742,868,715,585,532,566,611,548,499,479,428,657,481,645,961,
#'            940,912,955,935,940,952,1030,985)
#' cpue <- c(0.6008,0.6583,0.6791,0.6889,0.7134,0.7221,0.7602,0.7931,0.8582,
#'           0.8876,1.0126,1.1533,1.2326,1.2764,1.3307,1.3538,1.2648,1.2510,
#'           1.2069,1.1552,1.1238,1.1281,1.1113,1.0377)
#' dat <- makespmdata(cbind(year,catch,cpue))
#' pars <- c(0.35,7800,3500)
#' ans <- plotModel(pars,dat)
#' bestSP <- optim(par=pars,fn=ssq,callfun=simpspm,indat=dat)
#' bestSP
#' ans <- plotModel(bestSP$par,dat,schaefer=TRUE)
#' str(ans)
#' }
spm <- function(inp,indat,schaefer=TRUE,depleted=TRUE,
                year="year",cats="catch",index="cpue") {
  #  inp=param; indat=schaef; schaefer=TRUE; depleted=TRUE
  #  index="cpue"; year="year";cats="catch"
  years <- indat[,year]
  catch <- indat[,cats]
  cpue <- indat[,index]
  nyrs <- length(years)
  biom <- numeric(nyrs+1)
  predCE <- matrix(NA,nrow=nyrs,ncol=1)
  columns <- c("Year","ModelB","Catch","Depletion","Harvest")
  addtxt <- c("CPUE","PredCE")
  columns <- c(columns,addtxt)
  extendyrs <- c(years,(years[nyrs]+1))
  answer <- matrix(NA,nrow=(nyrs+1),ncol=length(columns),
                   dimnames=list(extendyrs,columns))
  answer[,"Catch"] <- c(catch,NA)
  answer[,"Year"] <- extendyrs
  r <- exp(inp[1])
  K <- exp(inp[2])
  biom[1] <-K
  if (depleted) biom[1] <- exp(inp[3])
  if(schaefer) p <- 1 else p <- 1e-8
  for (index in 1:nyrs) {
    Bt <- biom[index]
    biom[index+1] <- max(Bt + ((r/p)*Bt*(1-(Bt/K)^p)-catch[index]),40)
  }
  qval <- exp(mean(log(indat[,"cpue"]/biom[1:nyrs])))
  answer[,"ModelB"] <- biom
  answer[,"Depletion"] <- biom/K
  answer[,"Harvest"] <- c(catch/biom[1:nyrs],NA)
  answer[,"CPUE"] <- c(cpue,NA)
  predCE <- biom * qval # apply same catchability across all years
  answer[,"PredCE"] <- predCE
  msy <- r*K/((p+1)^((p+1)/p))
  if (!depleted) { copyp <- c(inp,inp[2])
         } else { copyp <- inp
  }
  params <- c(exp(copyp),qval)
  if (length(params) == 4) {
    names(params) <- c("r","K","Sigma","q")
  } else {
      names(params) <- c("r","K","Binit","Sigma","q")
  }
  sumout <- c(msy,p,answer[(nyrs+1),"Depletion"],answer[1,"Depletion"],
              answer[(nyrs+1),"ModelB"])
  names(sumout) <- c("msy","p","FinalDepl","InitDepl","FinalB")
  output <- list(params,answer,msy,sumout)
  names(output) <- c("parameters","outmat","msy","sumout")
  return(output)
} # End of spm

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







