

#' @title calcprior return sum of a vector of constant values as priors
#' 
#' @description so as to include a prior probability into Bayesian
#'     calculations calcprior is a template for generating such priors.
#'     The default given here is to return a constant small number for the
#'     prior probability, it needs to sum to 1.0 across the replicates 
#'     returned by do_MCMC. If non-uniform priors are required write
#'     a different function and in do_MCMC point priorcalc at it. 
#'     Whatever function you define needs to have the same input
#'     parameters as this calcprior, i.e. the parameters and N. If
#'     something else if required then do_MCMC will need modification in
#'     in the two places where priorcalc is used.
#'
#' @param pars the parameters of the model being examined by the MCMC
#' @param N the number of replicate parameter vectors to be returned from 
#'     do_MCMC
#'
#' @return the sum of a vector of small constant valus to act as priors. 
#' @export
#'
#' @examples
#' \dontrun{
#' param <- log(c(0.4,9400,3400,0.05))  
#' calcprior(pars=param,N=20000)  # should give -39.61395
#' }
calcprior <- function(pars,N) { # return log(1/N) for all values entered.
  return(sum(rep(log(1/N),length(pars))))
}


#' @title do_MCMC conducts an MCMC using Gibbs within Metropolis
#'
#' @description do_MCMC conducts a Gibbs within Metropolis algorithm. One
#'    can define the number of chains, the burnin before candidate
#'    parameter vectors and associated probabilities are stored, the total
#'    number of candidate vectors to keep and the step or thinning rate
#'    between accepting a result into the Markov Chain. One needs to 
#'    input three functions: 1) infunk to calculate the likelihoods, 
#'    2) calcpred used within infunk to calculate the predicted values
#'    used to compare with the observed (found in obsdat), and 
#'    3) priorcalc used to calculate the prior probabilities of the 
#'    various parameters in each trial. N * thinstep iterations are made 
#'    in total although only N are stored. The jumping
#'    function uses random normal deviates (mean=0, sd=1) to combine with
#'    each parameter value (after multiplication by the specific scaling
#'    factor held in scales). Determination of suitable scaling values
#'    is generally done empirically, perhaps by trialing a small number 
#'    of iterations to begin with. Multiple chains would be usual and 
#'    the thinstep would be larger eg. 128, 256, or 512, but it would 
#'    take 8, 16, or 32 times longer. The scales are usually 
#'    expirementally set to otain an acceptance rate between 20 - 40%. 
#'    It is also usual to run numerous diagnostic plots on th eoutputs 
#'    to ensure convergence on the final stationary distribution. There 
#'    are three main loops: 1) total number of iterations N * thinstep,
#'    2) thinstep/(number of parameters) so that at least all parameters 
#'    are stepped through at least once (=thinstep  = np) before any 
#'    combinations are considered for acceptance, this means that the
#'    true thinning rate is thinstep/np, and 3) the number of parameters
#'    loop that steps through the np parameters varying each one.
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
#'     values for comparison with obsdat; defaults to simpspm.
#' @param calcdat the data used by calcpred to calculate the predicted
#'     values
#' @param obsdat the observed data (on the same scale as the predicted)
#'     agsinst with the predicted values are compared.
#' @param priorcalc a function used to calculate the prior probability for
#'     each of the parameters in each trial parameter vector.
#' @param scales The re-scaling factors for each parameter to convert the
#'     normal random deviates into +/- values on a scale that leads to
#'     acceptance rates of between 20 and 40percent.
#' @param ... needed by the simpspm function = calcpred 
#'
#' @return a list of the result chains, the acceptance and rejection rates
#' @export
#'
#' @examples
#' \dontrun{
#' data(abdat); fish <- as.matrix(abdat) # to increase speed
#'  param <- log(c(0.4,9400,3400,0.05))
#'  N <- 10000  
#'  result <- do_MCMC(chains=1,burnin=20,N=N,thinstep=16,inpar=param,
#'                    infunk=negLL,calcpred=simpspm,calcdat=fish,
#'                    obsdat=log(fish[,"cpue"]),priorcalc=calcprior,
#'                    scales=c(0.06,0.05,0.06,0.42))
#'  cat("Acceptance Rate = ",result[[2]],"\n")
#'  cat("Failure Rate    = ",result[[3]],"\n")
#'  #plotprep(width=6,height=5,newdev=FALSE)
#'  out <- result[[1]][[1]] # get the list containing the matrix
#'  pairs(out[,1:4],col=rgb(1,0,0,1/20))
#'  
#'  parset(plots=c(1,2)) # Note the serial correlation in each trace
#'  plot1(1:N,out[,1],ylab="r",defpar=FALSE)
#'  plot1(1:N,out[,2],ylab="K",defpar=FALSE)
#' }
do_MCMC <- function(chains,burnin,N,thinstep,inpar,infunk,calcpred,calcdat,
                    obsdat,priorcalc,scales,...) {
   totN <- N + burnin   # total number of replicate steps
   result <- vector("list",chains) # to store all results
   for (ch in 1:chains) { # ch=1
      param <- inpar # initiate the chain from here to next for loop
      np <- length(param)  # Number of parameters
      if (ch > 1) param <- param + (rnorm(np) * scales) #change start pt
      if ((thinstep %% np) != 0)  # thinstep must be divisible by np
         stop("Thinning step must be a multiple of number of parameters")
      stepnp <- thinstep/np
      posterior <- matrix(0,nrow=totN,ncol=(np+1))
      colnames(posterior) <- c("r","K","Binit","sigma","Post")
      arate <- numeric(np) # to store acceptance rate
      func0 <- exp(-infunk(param,calcpred,indat=calcdat,obsdat,...) 
                   + priorcalc(np,N)) #back transform log-likelihoods 
      posterior[1,] <- c(exp(param),func0) # start the chain
      for (iter in 2:totN) {  # Generate the Markov Chain
         randinc <-  matrix(rnorm(thinstep,mean=0,sd=1),nrow=stepnp,ncol=np)
         for (i in 1:np) randinc[,i] <- randinc[,i] * scales[i]
         for (st in 1:stepnp) {
            uniform <- runif(np)
            for (i in 1:np) {      # loop through parameters
               oldpar <- param[i]  # incrementing them one a at time
               param[i] <- param[i] + randinc[st,i]
               func1 <- exp(-infunk(param,calcpred,indat=calcdat,obsdat,...) 
                            + priorcalc(np,N)) 
               if (func1/func0 > uniform[i]) { # Accept 
                  func0 = func1
                  arate[i] = arate[i] + 1
               } else {    # Reject
                  param[i] <- oldpar
               }
            }    # end of parameter loop
         } # end of thinstep loop
         posterior[iter,] <- c(exp(param),func0)
      } # end of Markov Chain generation; now remove burn-in and rescale 
      posterior <- posterior[(burnin+1):totN,] # to a post probability
      posterior[,"Post"] <- posterior[,"Post"]/sum(posterior[,"Post"])
      result[[ch]] <- posterior
   } # end of chains loop
   P_arate=arate/((N+burnin-1)*thinstep/np)
   return(list(result=result,arate=P_arate,frate=1-P_arate))
} # end of do_MCMC
