

#' @title calcprior return sum of a vector of constant values as priors
#' 
#' @description so as to include a prior probability into Bayesian
#'     calculations calcprior is a template for generating such priors.
#'     The default given here is to return a constant small number for the
#'     prior probability, it needs to sum to 1.0 so is the log(N) the number
#'     of replicates returned by do_MCMC. If non-uniform priors are 
#'     required write a different function and in do_MCMC point priorcalc
#'     at it. whatever function you define it needs to have the same input
#'     parameters as this calcprior, i.e. number of parameters and N. If
#'     something else if required then do_MCMC will need modification in
#'     the calculation of func1 where priorcalc is used.
#'
#' @param npars the number of model parameters being examined by the MCMC
#' @param N the number of replicate parameter vectors to be returned from 
#'     do_MCMC
#'
#' @return the sum of a vector of small constant valus to act as priors. 
#' @export
#'
#' @examples
#' \dontrun{
#' calcprior(npars=4,N=20000)  # should give -39.61395
#' }
calcprior <- function(npars,N) {
  # return log(1/N) for all values entered.
  return(sum(rep(log(1/N),npars)))
}


#' @title do_MCMC conducts an MCMC using Gibbs within Metropolis
#'
#' @description do_MCMC conducts a Gibbs within Metropolis algorithm. One
#'    can define the number of chains, the burnin before candidate
#'    parameter vectors and associated probabilities are stored, the total
#'    number of candidate vectors to keep and the step between accepting
#'    a result into the posterior. One needs to input three functions: 1)
#'    infunk to calculate the likelihoods, 2) calcpred used within infunk
#'    to calculate the predicted values,used to compare with the observed
#'    (found in obsdat), and 3) priorcalc used to calculate the prior
#'    probabilities of the various parameters in each trial. N * step
#'    iterations are made in total although only N are stored. The jumping
#'    function uses random normal deviates (mean=0, sd=1) to combine with
#'    each parameter value (after multiplication by the specific scaling
#'    factor held in scales). Determination of suitable scaling values
#'    is generally done empirically, perhaps by trailing a small number of
#'    iterations to begin with. Multiple chains would be usual and the 
#'    thinstep would be larger eg. 128, 256, or 512, but it would take 8, 
#'    16, or 32 times longer. The scales are usually expirementally set
#'    to otain an acceptance rate between 20 - 40%. It is also usual to
#'    run numerous diagnostic plots on th eoutputs to ensure convergence
#'    on the final stationary distribution.
#'
#' @param chains the number of independent MCMC chains produced
#' @param burnin the number of steps made before candidate parameter
#'     vectors begin to be kept.
#' @param N the total number of posterior parameter vectors retained
#' @param thinstep the number of interations before a vector is kept; must 
#'     be divisible by the number of parameters
#' @param inpar the initial parameter values (usually log-transformed)
#' @param infunk the function used to calculate the negative log-likelihood
#' @param calcpred the function used by infunk to calculate the predicted
#'     values for comparison with obsdat.
#' @param calcdat the data used by calcpred to calculate the predicted
#'     values
#' @param obsdat the observed data (on the same scale as the predicted)
#'     agsinst with the predicted values are compared.
#' @param priorcalc a function used to calculate the prior probability for
#'     each of the parameters in each trial parameter vector.
#' @param scales The re-scaling factors for each parameter to convert the
#'     normal random deviates into +/- values on a scale that leads to
#'     acceptance rates of between 20 and 40percent.
#'
#' @return a list of the result chains, the acceptance and rejection rates
#' @export
#'
#' @examples
#' \dontrun{
#' data(abdat)
#' fish <- abdat$fish
#'  simpsch <- function(pars, indat) {  # generate log-predicted cpue
#'    nyrs <- length(indat[,"year"])
#'    biom <- numeric(nyrs+1)
#'    catch <- indat[,"catch"]
#'    p <- exp(pars) # par contains log of (r,K,Binit, and sigma)
#'    biom[1] <- p[3]
#'    for (yr in 1:nyrs) {
#'      Bt <- biom[yr]  # avoid negative biomass using a max statement
#'      biom[yr+1] <- max(Bt + (p[1] * Bt) * (1 - (Bt/p[2])) - catch[yr],40)
#'    }
#'    qval <- exp(mean(log(indat[,"cpue"]/biom[1:nyrs])))
#'    return(log(biom[1:nyrs] * qval))  # the log of predicted cpue
#'  } # end of simpsch  
#'  param <- log(c(0.4,9400,3400,0.05))  
#'  result <- do_MCMC(chains=1,burnin=20,N=10000,thinstep=16,inpar=param,
#'                    infunk=negLL,calcpred=simpsch,calcdat=fish,
#'                    obsdat=log(fish$cpue),priorcalc=calcprior,
#'                    scales=c(0.06,0.05,0.06,0.325))
#'  cat("Acceptance Rate = ",result[[2]],"\n")
#'  cat("Failure Rate    = ",result[[3]],"\n")
#'  plotprep(width=6,height=5,newdev=FALSE)
#'  out <- result[[1]][[1]] # get the list containing the matrix
#'  pairs(out[,1:4],col=rgb(1,0,0,1/20))
#'  
#'  plot2(titleY=TRUE,newdev=FALSE)
#'  plot1(1:N,out[,1],ylabel="r",defpar=FALSE)
#'  plot1(1:N,out[,2],ylabel="K",defpar=FALSE)
#' }
do_MCMC <- function(chains,burnin,N,thinstep,inpar,infunk,calcpred,calcdat,
                    obsdat,priorcalc,scales) {
   totN <- N + burnin
   result <- vector("list",chains)
   for (ch in 1:chains) {
      param <- inpar
      np <- length(param)  # Number of parameters
      if ((thinstep %% np) != 0)  # thinstep must be divisible by np
         stop("Thinning step length must be a multiple of number of parameters")
      stepnp <- thinstep/np
      nRand <- np * thinstep #Number of random values needed for one thinstep
      posterior <- matrix(0,nrow=totN,ncol=(np+1))
      colnames(posterior) <- c("r","K","Binit","sigma","Post")
      arate <- numeric(np) # to store acceptance rate
      frate <- numeric(np)
      func0 <- exp(-infunk(param,calcpred,calcdat,obsdat) + priorcalc(np,N))
      posterior[1,] <- c(exp(param),func0)
      for (iter in 2:totN) {  # iter = 1
         randinc <-  matrix(rnorm(nRand,mean=0,sd=1),nrow=thinstep,ncol=np)
         for (i in 1:np) randinc[,i] <- randinc[,i] * scales[i]
         for (st in 1:stepnp) {
            uniform <- runif(np)
            for (i in 1:np) {       # loop through parameters
               oldpar <- param[i]
               param[i] <- param[i] + randinc[st,i]
               func1 <- exp(-infunk(param,calcpred,calcdat,obsdat) +
                               priorcalc(np,N)) # new +velog-likelihood
               if (func1/func0 > uniform[i]) { #
                  func0 = func1
                  arate[i] = arate[i] + 1
               } else {
                  param[i] <- oldpar
                  frate[i] <- frate[i] + 1
               }
            }    # end of parameter loop
         } # end of thinstep loop
         posterior[iter,] <- c(exp(param),func0)
      }
      posterior <- posterior[(burnin+1):totN,]
      posterior[,"Post"] <- posterior[,"Post"]/sum(posterior[,"Post"])
      result[[ch]] <- posterior
   } # end of chains loop
   return(list(result=result,arate=arate/(N*thinstep/np),
               frate=frate/(N*thinstep/np)))
} # end of do_MCMC
