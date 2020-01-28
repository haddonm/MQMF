
#' @title altnegLL calculate the Normal negative log-likelihood
#'
#' @description altnegLL calculates negLLM using the simplification
#'     from Haddon (2011) using the ssq calculated within the function
#'     spm
#'
#' @param inp a vector of model parameters (r,K,Binit)
#' @param indat a matrix with at least columns 'year', 'catch', and 
#'     'cpue'
#'
#' @return a single value, the negative log-likelihood
#' @export
#'
#' @examples
#' \dontrun{
#'  data(dataspm)
#'  pars <- log(c(r=0.2,K=6000,Binit=2800,sigma=0.2))
#'  ans <- fitSPM(pars,fish=dataspm,schaefer=TRUE,maxiter=1000)
#'  outfit(ans)
#'  altnegLL(ans$estimate,dataspm) # should be -12.12879
#' }
altnegLL <- function(inp,indat) { # inp=pars; indat=dataspm
   out <- spm(inp,indat)$outmat
   pick <- which(indat[,"cpue"] > 0)
   Nobs <- length(pick)
   ssq <- sum((log(out[pick,"CPUE"])-log(out[pick,"predCE"]))^2)
   sigma <- sqrt(ssq/Nobs)
   negLogL <- (Nobs/2)*(log(2*pi) + 2*log(sigma) + 1)
   return(negLogL)
} # end of altnegLL


#' @title fitSPM fits a surplus production model
#'
#' @description fitSPM fits a surplus production model (either Schaefer or 
#'     Fox) by applying first optim (using Nelder-Mead) and then nlm. Being 
#'     automated it is recommended that this only be used once plausible 
#'     initial parameters have been identified (through rules of thumb or 
#'     trial and error). It uses negLL1 to apply a negative log-likelihood, 
#'     assuming log-normal residual errors and uses a penalty to prevent 
#'     the first parameter from becoming < 0.0. If that is not wanted then
#'     set funkone to FALSE, which would then use negLL by itself.
#'     The output object is the usual object output from nlm, which can 
#'     be neatly printed using outfit.
#'     The $estimate values can be used in plotspmmod to plot the 
#'     outcome, or in spmboot to conduct bootstrap sampling of the residuals 
#'     from the CPUE model fit to gain an appreciation of any uncertainty 
#'     in the analysis. Because it requires log(parametrs) it does not 
#'     use the magnitude function to set the values of the parscale 
#'     parameters.
#'
#' @param pars the initial parameter values to start the search for the 
#'     optimum. These need to be on the log-scale (log-transformed)
#' @param fish the matrix containing the fishery data 'year', 'catch', and
#'     'cpue' as a minimum. These exact headings are required.
#' @param schaefer if TRUE, the default, then simpspm is used to fit the
#'     Schaefer model. If FALSE then the approximate Fox model is fitted 
#'     by setting the p parameter to 1e-08 inside simpspm.
#' @param maxiter the maximum number of iterations to be used by nlm
#' @param funk the function used to generate the predicted cpue
#' @param funkone default = TRUE. Means use negLL1, which constrains the
#'     first parameter (r) to be greater than 0. If FALSE then use negLL
#'     which is identical to negLL1 but lacks the constraint.
#' @param hess default is FALSE; should one calculate the hessian matrix?
#' @param steptol the internal step tolerance, required in case nlm reports
#'     the steptol as being too small. defaults to 1e-06
#'
#' @return an nlm output object as a list
#' @export
#'
#' @examples
#' \dontrun{
#'  data(dataspm) 
#'  dataspm <- as.matrix(dataspm) # faster than a data.frame
#'  pars <- log(c(r=0.2,K=6000,Binit=2800,sigma=0.2))
#'  ans <- fitSPM(pars,fish=dataspm,schaefer=TRUE,maxiter=1000)
#'  outfit(ans)   # Schaefer model  -12.12879
#'  ansF <- fitSPM(pars,dataspm,schaefer=FALSE,maxiter=1000)
#'  outfit(ansF)  # Fox model       -12.35283 
#' }  
fitSPM <- function(pars,fish,schaefer=TRUE,maxiter=1000,
                   funk=simpspm,funkone=TRUE,hess=FALSE,steptol=1e-06) { 
   if (funkone) minim=negLL1 else minim=negLL
   best <- optim(par=pars,fn=minim,funk=funk,indat=fish,schaefer=schaefer,
           logobs=log(fish[,"cpue"]),method="Nelder-Mead",
                 control=list(maxit=maxiter))
   best2 <- nlm(f=minim,p=best$par,funk=funk,indat=fish,schaefer=schaefer,
                logobs=log(fish[,"cpue"]),steptol=steptol,
                iterlim=maxiter,hessian=hess)
   return(best2)
} # end of fitSPM

#' @title getlag used to look for the response of cpue to previous catches
#'
#' @description getlag is a wrapper for the ccf function (cross correlation)
#'     that is used within the spm analyses to determine at what
#'     negative lag, if any, cpue data is informative about the stock 
#'     dynamics beyond any information already available in the catch data.
#'     If the cpue is directly correlated with catches (lag=0 has a strong
#'     correlation) then cpue will not add much more information to an 
#'     analysis. Only if there is a significant negative correlation is it 
#'     likely that the cpue will increase the information available and 
#'     make it more likely that an assessment model may be able to be fitted 
#'     meaningfully to the available data. If there is no significant 
#'     negative correlations then it becomes much more unlikely that a 
#'     useful model fit to the cpue will be possible. The getlag function 
#'     first finds those rows for which both catch and cpue have values 
#'     and then it runs the cross-correlation analysis. Thus, you cannot 
#'     have gaps in your cpue data although there can be catches at the 
#'     start or end of the time-series, or both, for which there are no 
#'     cpue data.
#'
#' @param fish the matrix or data.frame containing the fishery data 
#'     (year, catch, and cpue)
#' @param maxlag the lag.max parameter for the ccf function; defaults to 10
#' @param plotout should a plot be made; default=TRUE. If FALSE then, 
#'     assuming the result of the analysis is put into an object called
#'     'ans' a call to plot(ans) will generate the required plot.
#' @param indexI if there are more than one time-series of cpue/indices then
#'     this parameter selects which to use
#'
#' @return an object of class acf, which can be plotted
#' @export
#'
#' @examples
#' \dontrun{
#' year <- 1985:2008
#' catch <- c(1018,742,868,715,585,532,566,611,548,499,479,428,657,481,
#'            645,961,940,912,955,935,940,952,1030,985)
#' cpue <- c(0.6008,0.6583,0.6791,0.6889,0.7134,0.7221,0.7602,0.7931,0.8582,
#'           0.8876,1.0126,1.1533,1.2326,1.2764,1.3307,1.3538,1.2648,1.2510,
#'           1.2069,1.1552,1.1238,1.1281,1.1113,1.0377)
#' dat <- as.data.frame(cbind(year,catch,cpue))
#' out <- getlag(dat,plotout=FALSE)
#' plot(out,lwd=3,col=2)
#' str(out)
#' }
getlag <- function(fish,maxlag=10,plotout=TRUE,indexI=1) {
   pickI <- grep("cpue",colnames(fish))
   pick <- which((fish[,"catch"] > 0) & (fish[,pickI[indexI]] > 0))
   ans <- ccf(x=fish[pick,"catch"],y=fish[pick,pickI[indexI]],
              lag.max=maxlag,main="",type="correlation",plot=plotout,
              ylab="Cross-Correlation")
   return(ans)
} # end of getlag


#' @title getMSY calculates the MSY for the Polacheck et al 1993 equation
#' 
#' @description getMSY calculates the MSY for the Polacheck et al 1993 
#'     generalized surplus production equation. This simplifies to rK/4 
#'     when p = 1.0. But this is a general equation that covers off for 
#'     all values of p.
#'
#' @param pars the model parameters r, K, Binit, sigma; p is separate
#' @param p asymmetry parameter for the polacheck et al 1993 equation,
#'     default=1.0 = Schaefer
#'     
#' @references Polacheck, T., Hilborn, R., and A.E. Punt (1993) Fitting 
#'     surplus production models: Comparing methods and measuring uncertainty. 
#'     \emph{Canadian Journal of Fisheries and Aquatic Sciences}, 
#'     50: 2597-2607.
#'
#' @return the MSY
#' @export
#'
#' @examples
#' \dontrun{
#' param <- c(r=1.1,K=1000.0,Binit=800.0,sigma=0.075)
#' getMSY(param,p=1.0)     #  275       Schaefer equivalent
#' getMSY(param,p=1e-08)   #  404.6674  Fox equivalent
#' }
getMSY <- function(pars,p=1.0) {
  msy <- (pars[1]*pars[2])/((p+1)^((p+1)/p))
  return(msy)
} # end of getMSY

#' @title getrmse calculates the rmse of the input 'invar' series
#'
#' @description getrmse calculates the rmse of the input invar series 
#'     (defaults to 'cpue') against an input 'year' time series. This is 
#'     primarily designed to generate a more feasible estimate of the 
#'     intrinsic variability of a cpue time-series that may be obtained 
#'     from a cpue standardization
#'
#' @param indat the matrix, spmdat, or data.frame containing both a 'year'
#'     column and an invar column (default to 'cpue')
#' @param invar the column whose rmse is wanted; defaults to 'cpue'
#' @param inyr the column that points to the year name
#' @return a list of the rmse and the loess predicted values of the invar 
#'     for each year in the time-series
#' @export
#'
#' @examples
#' \dontrun{
#' year <- 1986:1994
#' cpue <- c(1.2006,1.3547,1.0585,1.0846,0.9738,1.0437,0.7759,1.0532,1.284)
#' dat <- as.matrix(cbind(year,cpue))
#' getrmse(dat,invar="cpue")  # should be 0.08265127
#' getrmse(dat,invar="cpue")$rmse
#' }
getrmse <- function(indat,invar="cpue",inyr="year"){
   if (iscol(inyr,indat) & iscol(invar,indat)) {
      nyr <- dim(indat)[1]
      predictedCE <- rep(NA,nyr)
      varloc <- grep(invar,colnames(indat))
      nvar <- length(varloc)
      if (nvar > 1) {
         obsvar <- rep(NA,nyr)
         for (i in 1:nvar) {
            pick <- which(indat[,varloc[i]] > 0)
            obsvar[pick] <- indat[pick,varloc[i]]
         }
      } else {
         obsvar <- indat[,varloc]
      }
      picky <- which(obsvar > 0)
      model <- loess(obsvar[picky] ~ indat[picky,inyr])
      predictedCE[picky] <- model$fitted
      rmse <- sqrt(sum(model$residuals^2)/model$n)
      return(list(rmse=rmse,predictedCE=predictedCE))
   } else {
      cat("Input data should contain both 'year' and 'cpue'  \n")
   }
} # end of getrmseCE



#' @title parasympt generates N vectors from a multi-variate normal
#' 
#' @description parasympt generates N vectors from a multi-variate normal
#'     distribution for a surplus production model. This can be used 
#'     when estimating the uncertainty around an spm fit, or when 
#'     conducting projections from a model fit while attempting to 
#'     account for uncertainty. Use of this function requires the 
#'     mvnnorm package. It could be generalized to suit any model.
#'
#' @param bestmod the output from nlm containing the optimal parameters in
#'     log-space, and the hessian
#' @param N the number of parameter vectors to be sampled from the multi-
#'     variate normal defined by the optimal parameters and the inverse of
#'     the hessian (the variance covariance matrix).
#'
#' @return an N x numpar matrix of parameter vectors
#' @export
#'
#' @examples
#' \dontrun{
#'   data(abdat)
#'   schf <- FALSE
#'   param <- log(c(r=0.3,K=11500,Binit=3300,sigma=0.05))
#'   bestmod <- nlm(f=negLL1,p=param,funk=simpspm,logobs=log(abdat$cpue),
#'                  indat=abdat,typsize=magnitude(param),iterlim=1000,
#'                  schaefer=schf,hessian = TRUE)
#'   out <- spm(bestmod$estimate,indat=abdat,schaefer=schf)
#'   matpar <- parasympt(bestmod,1000)
#'   head(matpar,15)
#'   pairs(matpar)
#' }
parasympt <- function(bestmod,N) {
  optpar <- bestmod$estimate
  if (is.null(dim(bestmod$hessian))) 
    stop(cat("The input bestmod must include a Hessian estimate!  \n"))
  vcov <- solve(bestmod$hessian)
  numpar <- length(optpar)
  columns <- c("r","K","Sigma")  
  if (length(optpar) == 4) columns <- c(columns,"Sigma")
  mvnpar <- matrix(exp(rmvnorm(N,mean=optpar,sigma=vcov)),nrow=N,
                   ncol=numpar,dimnames=list(1:N,columns))
  return(mvnpar)
} # parasympt


#' @title plotlag plots the effect of a lag between two variables
#'
#' @description the use of the function ccf can suggest a lagged 
#'     relationship between a driver variable and a react(ing) variable. 
#'     For example, cpue may respond to catches in a negative manner after 
#'     a lag of a few years. One looks for a negative lag, which would 
#'     imply that the driver variable influences the react(ing) variable 
#'     after the given lag has passed. The lag is always assumed to be 
#'     based on yearly intervals, though this can be changed.
#'
#' @param x the matrix containing columns of the named variables. It must
#'     contain columns with the same names as the driver and react(ing)
#'     variables
#' @param driver the variable doing the influencing
#' @param react the variable being influenced
#' @param lag the time lag before the influence is felt
#' @param interval the name of the time-interval variable, default='year'
#' @param filename default is empty. If a filename is put here a .png file
#'     with that name will be put into the working directory.
#' @param resol the resolution of the png file, defaults to 200 dpi
#' @param fnt the font used in the plot and axes. Default=7, bold Times. 
#'     Using 6 gives Times, 1 will give SansSerif, 2 = bold Sans
#'
#' @return a list containing some summary results, the anova of the linear
#'     model fitted in aov, and a summary of the linear model in summ
#' @export
#'
#' @examples
#' \dontrun{
#' year <- 1985:2008
#' catch <- c(1018,742,868,715,585,532,566,611,548,499,479,428,657,481,645,
#'            961,940,912,955,935,940,952,1030,985)
#' cpue <- c(0.6008,0.6583,0.6791,0.6889,0.7134,0.7221,0.7602,0.7931,0.8582,
#'           0.8876,1.0126,1.1533,1.2326,1.2764,1.3307,1.3538,1.2648,1.2510,
#'           1.2069,1.1552,1.1238,1.1281,1.1113,1.0377)
#' dat <- cbind(year,catch,cpue)
#' out <- plotlag(dat,driver="catch",react="cpue",lag=7)
#' round(out$results,5)
#' out$summ
#' }
plotlag <- function(x, driver="catch",react="cpue",lag=0,interval="year",
                    filename="",resol=200,fnt=7){
   lenfile <- nchar(filename)
   if (lenfile > 3) {
      end <- substr(filename,(lenfile-3),lenfile)
      if (end != ".png") filename <- paste0(filename,".png")
      png(filename=filename,width=5,height=5.5,units="in",res=resol)
   }
   par(mfrow = c(3,1),mai = c(0.25, 0.45, 0.1, 0.05), oma = c(1.0,0,0,0))
   par(cex=0.75,mgp=c(1.35,0.35,0), font.axis = 7, font = 7, font.lab=7)
   colnames(x) <- tolower(colnames(x))
   nobs <- dim(x)[1]
   # plot 1
   ymax <- max(x[,driver],na.rm=TRUE) * 1.025
   plot(x[1:(nobs-lag),interval],x[1:(nobs-lag),driver],type="l",lwd=2,
        xlab="",ylab=driver,ylim=c(0,ymax),yaxs="i",
        panel.first = grid(col="grey"))
   abline(h=0,col=1)
   text(x[1,interval],0.9*ymax,paste0("lag = ",lag),cex=1.2,pos=4)
   #plot 2
   ymax <- max(x[,react],na.rm=TRUE) * 1.025
   plot(x[(1+lag):nobs,interval],x[(1+lag):nobs,react],type="l",lwd=2,
        xlab="",ylab=react,ylim=c(0,ymax),yaxs="i",
        panel.first = grid(col="grey"))
   abline(h=0,col=1)
   #plot 3
   plot(x[1:(nobs-lag),driver],x[(1+lag):nobs,react],type="p",pch=16,
        ylim=c(0,max(x[,react],na.rm=TRUE)),panel.first = grid(),
        ylab=react,xlab="")
   mtext(driver,side=1,outer=TRUE,cex=1.0,line=0)
   model <- lm(x[(1+lag):nobs,react] ~ x[1:(nobs-lag),driver])
   abline(model,col=2)
   if (lenfile > 0) {
      outfile <- paste0(getwd(),"/",filename)
      print(outfile)
      dev.off()
   }
   ano <- anova(model)
   summ <- summary(model)
   results <- c(lag=lag,p=ano$`Pr(>F)`[1],
                adj_r2=summ$adj.r.squared,df=ano$Df[2])
   return(list(results=results,aov=anova(model),summ=summary(model)))
} # end of plotlag


#' @title plotspmmod plots an spm model fit given parameters and data
#'
#' @description plotspmmod takes a parameter set and the spmdat matrix
#'     and plots the predicted depletion, catch, the CPUE and the model 
#'     fit to CPUE. If plotprod=TRUE, it also plots the productivity curves
#'
#' @param inp a vector of model parameters at least (r,K,B0)
#' @param indat a matrix with at least columns 'year', 'catch', and 'cpue'
#' @param schaefer if TRUE, the default, then the Schaefer SPM is used. 
#'     If FALSE then an approximate Fox SPM would be used
#' @param limit defaults to the Commonwealth limit of 0.2B0.
#' @param target defaults to the Commonwealth target of 0.48B0. Determines
#'     the green line plotted on the exploitable biomass plot.
#' @param addrmse default is FALSE but if set TRUE this will add the loess
#'     curve to the CPUE trend for comparison with the fitted line
#' @param fnt the font used in the plot and axes. Default=7, bold Times. 
#'     Using 6 gives Times, 1 will give SansSerif, 2 = bold Sans
#' @param plotout should one generate a plot or only do the calculations; 
#'     default is TRUE
#' @param vline defaults to 0 = no action but if a year is inserted,
#'     which should generally be between two years to designate some
#'     change in the time-series between two years, then a vertical line
#'     will be added on year-based plots
#' @param plotprod if FALSE, the default, the productivity curves are 
#'     not plotted. If TRUE, two extra plots are produced.
#' @param maxy defaults to 0, if > 0 then that value will be used in the
#'     plot of CPUE
#' @return invisibly a list of dynamics, production curve, MSY, and Bmsy
#' @export
#'
#' @examples
#' \dontrun{
#' data(dataspm)
#' pars <- log(c(0.264,4740,3064,0.2))
#' ans <- plotspmmod(pars,dataspm,schaefer=TRUE,plotprod=TRUE)
#' best <- nlm(negLL,pars,funk=simpspm,indat=dataspm,
#'        logobs=log(dataspm[,"cpue"]),steptol=1e-05)
#' outfit(best,backtran = TRUE)
#' ans <- plotspmmod(best$estimate,dataspm,schaefer=TRUE,plotout=TRUE,
#'                  plotprod=FALSE,addrmse=TRUE)
#' str(ans)
#' }
plotspmmod <- function(inp,indat,schaefer=TRUE,limit=0.2,
                      target=0.48,addrmse=FALSE,fnt=7,plotout=TRUE,
                      vline=0,plotprod=FALSE,maxy=0) {
  if(schaefer) p <- 1 else p <- 1e-8
  celoc <- grep("cpue",colnames(indat))
  if (length(celoc) > 1){
    out <- spmCE(inp=inp,indat=indat,schaefer = schaefer)
  } else {
    out <- spm(inp=inp,indat=indat,schaefer = schaefer)
  }
  ans <- out$outmat
  # calc sigmaCE by cpue column
  predloc <- grep("predCE",colnames(ans))
  celoc <- grep("CPUE",colnames(ans))
  nce <- length(celoc)
  sigmaCE <- numeric(nce)
  for (i in 1:nce) {
    pickY <- which(ans[,celoc[i]] > 0)
    sigmaCE[i] <- sum((log(ans[pickY,celoc[i]]) - 
                         log(ans[pickY,predloc[i]]))^2)
    sigmaCE[i] <- sqrt(sigmaCE[i]/length(pickY))
  }
  yrs <- ans[,"Year"]
  nyr <- length(yrs)
  depl <- ans[,"Depletion"] # for depletion plot
  rmse <- list(rmse=NA,predictedCE=NA) # for cpue plot
  rmseresid <- NA
  unfishedB <- out$parameters[2]    # for production results
  minB <- 100
  x <- seq(minB,unfishedB,length.out = 200)
  pars <- out$parameters
  y <- ((pars[1]/p)*x*(1-(x/pars[2])^p))
  pick <- which.max(y)
  pickLRP <- which.closest(limit*pars[2],x)
  pickTRP <- which.closest(target*pars[2],x)
  msy <- y[pick]
  Bmsy <- x[pick]
  Blim <- x[pickLRP]
  Btarg <- x[pickTRP]
  Ctarg <- y[pickTRP] # end of production results
  xd <- x/unfishedB
  Dmsy <- xd[pick]
  Dcurr <- tail(depl,1)
  if (plotout) {
    if (plotprod) par(mfcol= c(3,2)) else par(mfcol= c(2,2))
    par(mai=c(0.35,0.4,0.1,0.05),oma=c(0.0,0.0,0,0))
    par(cex=0.75, mgp=c(1.25,0.35,0), font.axis=7,font.lab=7,font=fnt)
    # plot 1 Depletion through time
    ymax <- max(depl) * 1.025
    plot(yrs,depl,type="l",col=1,ylab="",xlab="",ylim=c(0,ymax),lwd=2,
         yaxs="i", panel.first = grid())
    title(ylab=list("ExploitB Depletion", cex=1.0, col=1, font=fnt),
          xlab=list("Years", cex=1.0, col=1, font=fnt))
    abline(h=c(0.2,target),col=c(2,3),lwd=1)
    if (vline > 0) abline(v=vline,col=1,lty=2)
    # plot2 Catch
    ymax <- max(ans[,"Catch"],na.rm=T)*1.025
    plot(yrs,ans[,"Catch"],type="l",pch=16,col=1,ylab="",xlab="",
         ylim=c(0,ymax),lwd=2,yaxs="i",panel.first = grid())
    abline(h=(out$msy),col=2)
    if (vline > 0) abline(v=vline,col=1,lty=2)
    title(ylab=list("Catch (t)", cex=1.0, col=1, font=fnt),
          xlab=list("Years", cex=1.0, col=1, font=fnt))
    # Plot 3 CPUE
    celoc <- grep("CPUE",colnames(ans))
    predloc <- grep("predCE",colnames(ans))
    nce <- length(celoc)
    if (nce > 1) {
      obsce <- rep(NA,nyr)
      for (i in 1:nce) {
        pick <- which(ans[,celoc[i]] > 0)
        obsce[pick] <- ans[pick,celoc[i]]
      }
    } else {
      obsce <- ans[,celoc]
    }
    pickCE <- which(ans[,celoc[1]] > 0)
    ymax <- max(ans[pickCE,c(celoc,predloc)],na.rm=T)*1.025    
    if (maxy > 0) ymax <- maxy
    plot(yrs,ans[,celoc[1]],type="p",pch=16,col=1,cex=1.0,ylab="",xlab="",
         ylim=c(0,ymax),yaxs="i",xlim=range(yrs),panel.first = grid())
    if (vline > 0) abline(v=vline,col=1,lty=2)
    lines(yrs[pickCE],ans[pickCE,predloc[1]],col=1,lwd=2)
    if (nce > 1) {
      for (i in 2:nce) {
        pickCE <- which(ans[,celoc[i]] > 0)
        points(yrs[pickCE],ans[pickCE,celoc[i]],pch=1,cex=1.0,col=1)
        lines(yrs[pickCE],ans[pickCE,predloc[i]],col=i,lwd=2)
      }
    }
    if (addrmse) {
      CEnames <- colnames(ans)[celoc]
      rmse <- vector("list",nce)
      names(rmse) <- colnames(ans)[celoc]
      for (i in 1:nce) { # use getrmse to fit loess
        rmse[[i]] <- getrmse(ans,invar=CEnames[i],inyr="Year")
        lines(yrs,c(rmse[[i]]$predictedCE),lwd=2,col=3,lty=2)
      }
    }
    title(ylab=list("Scaled CPUE", cex=1.0, col=1, font=fnt),
          xlab=list("Years", cex=1.0, col=1, font=fnt))
    text(min(yrs),ymax*0.2,paste("p = ",p,sep=""),font=fnt,cex=1.0,pos=4)
    text(min(yrs),ymax*0.1,paste("MSY = ",round(out$msy,3),sep=""),
         font=fnt,cex=1.0,pos=4)
    # plot4 cpue residuals
    resid <- as.matrix(ans[,celoc]/ans[,predloc])
    rmseresid <- numeric(nce)
    nresid <- dim(resid)[1]
    ymax <- getmax(resid)
    ymin <- getmin(resid,mult=1.1)
    plot(yrs,resid[,1],"n",ylim=c(ymin,ymax),ylab="Log Residuals",
         xlab="Years",panel.first=grid())
    abline(h=1.0,col=1,lty=2)
    if (vline > 0) abline(v=vline,col=1,lty=2)
    for (i in 1:nce) {
      pickCE <- which(ans[,celoc[i]] > 0)
      segments(x0=yrs[pickCE],y0=rep(1.0,length(pickCE)),x1=yrs[pickCE],
               y1=resid[pickCE,i],lwd=2,col=2)
      points(yrs[pickCE],resid[pickCE,i],pch=16,col=1,cex=1.0)
      rmseresid[i] <- sqrt(sum(resid[,i]^2,na.rm=TRUE)/length(pickCE))
    }
    legend("topleft",colnames(ans)[celoc],round(rmseresid,3),
           cex=1.0,bty="n")
    if (plotprod) {
      #plot5 Production curve
      ymax <- getmax(y)
      plot(x,y,type="l",col=1,ylab="",xlab="",ylim=c(0,ymax),lwd=2,
           yaxs="i", panel.first = grid())
      abline(v=c(Blim,Bmsy,Btarg),col=2)
      abline(h=msy,col=2,lty=2)
      text(Bmsy*1.05,ymax*0.1,paste("Bmsy = ",round(Bmsy,3),sep=""),
           font=fnt,cex=1.0,pos=4)
      text(0.8*unfishedB,0.925*msy,round(out$msy,3),font=fnt,cex=1,pos=4)
      text(0.8*unfishedB,0.825*msy,"MSY",font=fnt,cex=1,pos=4)
      title(ylab=list("Surplus Production", cex=1.0, col=1, font=fnt),
            xlab=list("Biomass", cex=1.0, col=1, font=fnt))
      #plot6 depletion production curve from biomass one
      ymax <- getmax(y)
      plot(xd,y,type="l",col=1,ylab="",xlab="",ylim=c(0,ymax),lwd=2,
           yaxs="i", panel.first = grid())
      abline(v=c(limit,Dmsy,target),col=c(2,4,3))
      abline(h=c(Ctarg,msy),col=c(3,2),lty=2)
      title(ylab=list("Surplus Production(t)", cex=1.0, col=1, font=fnt),
            xlab=list("Depletion", cex=1.0, col=1, font=fnt))
    }
  }
  result <- list(out,cbind(x,y),rmseresid,msy,Bmsy,Dmsy,Blim,Btarg,Ctarg,
                 Dcurr,rmse,sigmaCE)
  names(result) <- c("Dynamics","BiomProd","rmseresid","MSY","Bmsy",
                     "Dmsy","Blim","Btarg","Ctarg","Dcurr","rmse","sigma")
  return(invisible(result))
}  # end of plotspmmod

#' @title plotproj generate a plot of a matrix of biomass projections
#' 
#' @description plotproj generate a plot of a matrix of biomass projections
#'     and includes the option of including reference points relative to 
#'     Bzero = K. Quantiles are included in the plot
#'
#' @param projs the N x yrs matrix of biomass trajectories
#' @param spmout the object output from the function spm
#' @param qprob the quantiles to include in the plot, default=c(0.1,0.5,0.9)
#' @param refpts the proportion of Bzero=K acting as limit and target
#'     reference points
#' @param fnt the font to use in the figure, default = 7 = bold Times
#'
#' @return This plots a graph and returns, invisibly, the requested 
#'     quantiles and the proportion less than the limit reference point.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(abdat)
#'   schf <- FALSE
#'   param <- log(c(r=0.3,K=11500,Binit=3300,sigma=0.05))
#'   bestmod <- nlm(f=negLL1,p=param,funk=simpspm,logobs=log(abdat$cpue),
#'                  indat=abdat,typsize=magnitude(param),iterlim=1000,
#'                  schaefer=schf,hessian = TRUE)
#'   out <- spm(bestmod$estimate,indat=abdat,schaefer=schf)
#'   matpar <- parasympt(bestmod,1000)
#'   projs <- spmproj(matpar,abdat,projyr=10,constC=900)
#'   plotproj(projs,out,qprob=c(0.1,0.5),refpts=c(0.2,0.4))
#' }
plotproj <- function(projs,spmout,qprob=c(0.1,0.5,0.9),
                     refpts=NULL,fnt=7) {
  yrs <- as.numeric(colnames(projs))
  N <- nrow(projs)
  dyn <- spmout$outmat
  lastyr <- max(dyn[,"Year"]) - 1
  Bzero <- spmout$parameters["K"]
  maxy <- getmax(projs)
  par(mfrow=c(1,1),mai=c(0.3,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0)) 
  par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=fnt,font=fnt,font.lab=fnt)
  plot(yrs,projs[1,],type="n",ylab="Stock Biomass (t)", xlab="",
       panel.first=grid(),ylim=c(0,maxy),yaxs="i")
  for (i in 1:N) lines(yrs,projs[i,],lwd=1,col="grey")
  qts <- NULL
  if (is.vector(qprob)) {
    qts <- as.matrix(apply(projs,2,function(x) quantile(x,probs=qprob)))
    nqts <- length(qprob)
    if (nqts > 1) {
      for (i in 1:nqts) lines(yrs,qts[i,],lwd=1,col=2)
    } else {
      lines(yrs,qts,lwd=1,col=2)
    }
  }
  lines(dyn[,"Year"],dyn[,"ModelB"],lwd=2,col=1)
  ltLRP <- NULL
  abline(v=(lastyr+0.5),col=4)
  nyr <- length(yrs)
  if (length(refpts) > 0) {
     abline(h=refpts*Bzero,lwd=2,col=1,lty=2)
     ltLRP <- length(which(projs[,nyr] < refpts[1]*Bzero))
  }
  ans <- list(qts=qts,ltLRP=ltLRP/N)
  return(invisible(ans))
} # end of plotproj

#' @title plotspmdat plots the fish data containing catches and cpue
#'
#' @description plotspmdat plots a fish data set. It plots the catch 
#'     and CPUE against time
#'
#' @param x a data set that should contain 'year', 'catch', and
#'     'cpue'
#' @param ... extra parameters potentially used by plotspmdat
#'
#' @return nothing, but it does generate a new plot
#' @export
#'
#' @examples
#' \dontrun{
#' yrs <- 2000:2010
#' catches <- rnorm(length(yrs),mean=150,sd=5)
#' ce <- rnorm(length(yrs),mean=20,sd=1)
#' fish <- as.data.frame(cbind(year=yrs,catch=catches,cpue=ce))
#' plotspmdat(fish)
#' }   # x=dat
plotspmdat <- function(x, ...){
  colnames(x) <- tolower(colnames(x)) 
  tmp <- match(c("year","catch","cpue"), colnames(x))
  if (countgtzero(tmp) != 3) {
    label <- paste0("Input data to plotspmdat must, at least, ",
                    "contain year, catch, and cpue")
    stop(label)
  }
  par(mfrow = c(2,1),mai = c(0.25, 0.45, 0.1, 0.05), oma = c(0,0,0,0))
  par(cex = 0.85, mgp = c(1.35,0.35,0),font.axis=7,font=7, font.lab=7)

  ymax <- max(x[,"catch"],na.rm=TRUE) * 1.025
  plot(x[,"year"],x[,"catch"],type="l",lwd=2,xlab="",ylab="Catch (t)",
      ylim=c(0,ymax),yaxs="i",panel.first=grid())
  abline(h=0,col=1)
  ymax <- max(x[,"cpue"],na.rm=TRUE) * 1.025
  plot(x[,"year"],x[,"cpue"],type="l",lwd=2,xlab="",ylab="CPUE",
      ylim=c(0,ymax),yaxs="i",panel.first=grid())
  abline(h=0,col=1)
} # end of plotspmdat


#' @title robustSPM doess a robustness test on quality of fit of an SPM
#'
#' @description robustSPM conducts a robustness test on the quality of 
#'     fit of an SPM. This is done by using the original optimal model 
#'     parameters or the original guessed parameter values, add random 
#'     variation to each of them, and re-fit the model. This process 
#'     needs to be repeated multiple times. This should enable an 
#'     analysis of the stability of the modelling outcomes. If the 
#'     optimum parameters are used then add more variation, if initial 
#'     guesses are used you may need to select different starting 
#'     points so that the random variation covers the parameter space 
#'     reasonably well.
#'
#' @param inpar the parameter set to begin the trials with
#' @param fish the fisheries data: at least year, catch, and cpue
#' @param N number of random trials to run; default = 10 = not enough
#' @param scaler the divisor that sets the degree of normal random 
#'     variation to add to the parameter values; default = 15 the 
#'     smaller the value the more variable the outcome
#' @param console print summary statistics to the screen? default = TRUE
#' @param schaefer default = TRUE, which sets the analysis to the 
#'     Schaefer model. setting it to FALSE applies the Fox model
#' @param funk the function used to generate the predicted cpue
#' @param funkone defaults=FALSE; use negLL or negLL1, with FALSE 
#'     robustSPM will use negLL, with TRUE it will use negLL1
#'     which has a constraint on the first parameter to keep it > 0
#' @param steptol is the steptol from nlm as used in fitSPM, the 
#'     default value is 1e-06, as usual.
#'
#' @return a list of results from each run, the range of values across 
#'     runs, and the median values.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(dataspm)
#'   param <- log(c(r=0.24,K=5174,Binit=2846,sigma=0.164))
#'   ans <- fitSPM(pars=param,fish=dataspm,schaefer=TRUE,maxiter=1000)
#'   out <- robustSPM(ans$estimate,dataspm,N=50,scaler=40,console=TRUE,
#'                    schaefer=TRUE)
#'   str(out)
#'   print(out$results[40:50,])
#'   pairs(out$results[,c(6:8,11)])
#' } 
robustSPM <- function(inpar,fish,N=10,scaler=40,console=TRUE,
                      schaefer=TRUE,funk=simpspm,funkone=FALSE,
                      steptol=1e-06) {
  origpar <- inpar
  if (length(origpar) == 3) {
    pars <- cbind(rnorm(N,mean=origpar[1],sd=abs(origpar[1])/scaler),
                  rnorm(N,mean=origpar[2],sd=abs(origpar[2])/scaler),
                  rnorm(N,mean=origpar[3],sd=abs(origpar[3])/scaler))
    columns <- c("ir","iK","isigma","iLike","r","K","sigma","-veLL","MSY",
                 "Iters")
  } else {
    pars <- cbind(rnorm(N,mean=origpar[1],sd=abs(origpar[1])/scaler),
                  rnorm(N,mean=origpar[2],sd=abs(origpar[2])/scaler),
                  rnorm(N,mean=origpar[3],sd=abs(origpar[3])/scaler),
                  rnorm(N,mean=origpar[4],sd=abs(origpar[4])/scaler))
    columns <- c("ir","iK","iBinit","isigma","iLike","r","K","Binit",
                 "sigma","-veLL","MSY","Iters")  # prefix i implies input
  }
  coln <- length(columns)
  results <- matrix(0,nrow=N,ncol=coln,dimnames=list(1:N,columns))
  for (i in 1:N) {  # i=1
    if (funkone) {
      origLL <-  negLL1(pars[i,],fish,funk=funk,logobs=log(fish[,"cpue"]),
                       schaefer=schaefer)      
    } else {
      origLL <-  negLL(pars[i,],fish,funk=funk,logobs=log(fish[,"cpue"]),
                       schaefer=schaefer)
    }
    bestSP <- fitSPM(pars[i,],fish,schaefer=schaefer,funk=funk, 
                     funkone=funkone,steptol=steptol)
    if (schaefer) pval=1.0 else pval=1e-08
    opar <- exp(bestSP$estimate)
    MSY <- getMSY(opar,p=pval)
    results[i,] <- c(exp(pars[i,]),origLL,opar,bestSP$minimum,MSY,
                     bestSP$iterations[1])
    if (console) cat(i,"   ")
  }
  if (console) cat("\n")
  ordres <- results[order(results[,"-veLL"]),] # see best and worst fit
  bounds <- apply(results,2,range)
  medvalues <- apply(results,2,median)
  if (console) {
    print(round(bounds,3))   # see the minimum and maximum)
    print(round(medvalues,3))
  }
  return(list(results=ordres,range=bounds,medians=medvalues))
} # end of robustSPM

#' @title simpspm calculates only the predicted log(CE) for an SPM
#' 
#' @description simpspm calculates only the predicted log(CPUE) for a 
#'     Surplus Production Model (SPM). It sets the Polacheck et al, 1993 
#'     parameter 'p' depending on the schaefer boolean argument, and 
#'     this determines the asymmetry of the production curve. Note p is
#'     set not estimated. If p = 1.0 then the SPM is the Schaefer model, 
#'     if it is 1e-8 it approximates the Fox model. The output of 
#'     log(CPUE) is to simplify the use of log-normal residual errors or 
#'     likelihoods. This function is designed for data consisting of 
#'     only a single cpue time-series. simpspm must have at least three 
#'     parameters, including the sigma, even if sum-of-squared residuals 
#'     is used as a minimizer, then sigma would just float. The column 
#'     titles for year, catch and cpue are included to facilitate ease
#'     of use with other data sets.
#'
#' @param pars the parameters of the SPM are either c(r,K,Binit,sigma),
#'     or c(r, K, sigma), the sigma is required in both cases. Binit is 
#'     required if the fishery data starts after the stock has been
#'     depleted. Each parameter must be log-transformed for improved 
#'     model stability and is transformed inside simpspm.
#' @param indat the data which needs to include year, catch, and cpue. 
#' @param schaefer a logical value determining whether the spm is to be 
#'     a simple Schaefer model (p=1) or approximately a Fox model 
#'     (p=1e-08). The default is TRUE = Schaefer model
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
#'  param <- log(c(r=0.4,K=9400,Binit=3400,sigma=0.05))
#'  predCE <- simpspm(pars=param,indat=abdat)
#'  cbind(abdat,exp(predCE))
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
#'     it approximates the Fox model. It is designed for when there are 
#'     mthan one time-series of an index of relative abundance.
#'
#' @param pars the parameters of the SPM = r, K, Binit if fishery is 
#'     depleted to start with (omit otherwise), and a sigma for each 
#'      cpue series. Each parameter is in log space and is 
#'      back-transformed inside simpspmM. sigmas are not used in this
#'      funciton but should still be in the parameter vector
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
#'  data(twoindex)
#'  fish <- as.matrix(twoindex)
#'  pars <- log(c(0.04,155000,0.4,0.3))
#'  bestSP <- nlm(f=negLLM,p=pars,funk=simpspmM,indat=fish,
#'              schaefer=TRUE,logobs=log(fish[,c("cpue1","cpue2")]),
#'              steptol=1e-06,harvpen=TRUE)
#'  outfit(bestSP)  # best fitting estimates
#'  simpspmM(bestSP$estimate,fish) # the two log(predicted cpue)
#' }
simpspmM <- function(pars,indat,schaefer=TRUE,
                     year="year",cats="catch",index="cpue") {
  celoc <- grep(index,colnames(indat))
  nce <- length(celoc)
  npar <- 2 + nce
  nyrs <- length(indat[,"year"])
  biom <- numeric(nyrs+1)
  catch <- indat[,cats]
  predCE <- matrix(NA,nrow=nyrs,ncol=nce)
  r <- exp(pars[1])
  K <- exp(pars[2])
  biom[1] <- K
  if (length(pars) > npar) biom[1] <- exp(pars[3])
  if(schaefer) p <- 1 else p <- 1e-8
  for (loc in 1:nyrs) {
    Bt <- biom[loc]
    biom[loc+1] <- max(Bt + ((r/p)*Bt*(1-(Bt/K)^p)-catch[loc]),40)
  }
  for (i in 1:nce) {
    pick <- which(indat[,celoc[i]] > 0)
    qval <- exp(mean(log(indat[pick,celoc[i]]/biom[pick])))
    predCE[pick,i] <- log(biom[pick] * qval)
  }
  return(predCE)
} # end of simpspmM

#' @title spm - calculates the dynamics of a Schaefer or Fox model
#'
#' @description spm calculates the dynamics using a Schaefer of Fox 
#'     model. The outputs include  predicted Biomass, year, catch, cpue,
#'     predicted cpue, contributions to q, ssq, and depletion levels. 
#'     Generally it would be more sensible to use simpspm when fitting 
#'     a Schaefer model or a Fox model as those functions are designed 
#'     to generate only the log of predicted cpue as required by the 
#'     functions ssq and negLL, but the example shows how it could be 
#'     used. The function spm is used inside 'plotspmmod' and could be 
#'     used alone, to generate a full list of model outputs after the 
#'     model has been fitted. spm is designed for working with a
#'     single vector of an index of relative abundance. If there are 
#'     multiple vectors of the index then use simpspmM and spmCE.
#'
#' @param inp a vector of 3 or 4 model parameters (r,K,sigma) or (r, K,
#'     Binit,sigma), you would use the latter if it was suspected that 
#'     the fishery data started after some initial depletion had 
#'     occurred. The sigma is an estimate of the variation of the cpue 
#'     through time. This is required but is only used when fitting the 
#'     model using negative log-likelihoods.
#' @param indat a matrix with at least columns year, catch, and cpue
#' @param schaefer a logical value determining whether the spm is to 
#'     be a simple Schaefer model (p=1) or approximately a Fox model 
#'     (p=1e-08). The default is TRUE
#' @param year the name of the year variable (in case your dataset 
#'     names it fishingyearinwhichthecatchwastaken)
#' @param cats name of the catch variable, again this is for generality
#' @param index the name of the cpue variable, for generality
#'
#' @return a list of five objects; parameters plus q, then outmat, the 
#'     matrix with the dynamics, msy the maximum sustainable yield, and 
#'     sumout, which contains r,K,B0,msy,p,q,Depl, FinalB, and InitDepl
#' @export
#'
#' @examples
#' \dontrun{
#'  data(abdat)   # spm is used inside plotspmmod
#'  pars <- log(c(0.35,7800,3500,0.05))
#'  ans <- plotspmmod(pars,abdat) #not fitted, just guessed
#'  bestSP <- fitSPM(pars=pars,fish=abdat,funk=simpspm)
#'  outfit(bestSP)  # best fitting estimates
#'  ans <- plotspmmod(bestSP$estimate,abdat,schaefer=TRUE)
#'  str(ans)
#' }
spm <- function(inp,indat,schaefer=TRUE,
                year="year",cats="catch",index="cpue") {
  years <- indat[,year]
  catch <- indat[,cats]
  cpue <- indat[,index]
  nyrs <- length(years)
  biom <- numeric(nyrs+1)
  predCE <- matrix(NA,nrow=nyrs,ncol=1)
  columns <- c("Year","ModelB","Catch","Depletion","Harvest")
  addtxt <- c("CPUE","predCE")
  columns <- c(columns,addtxt)
  extendyrs <- c(years,(years[nyrs]+1))
  answer <- matrix(NA,nrow=(nyrs+1),ncol=length(columns),
                   dimnames=list(extendyrs,columns))
  answer[,"Catch"] <- c(catch,NA)
  answer[,"Year"] <- extendyrs
  r <- as.numeric(exp(inp[1]))
  K <- as.numeric(exp(inp[2]))
  biom[1] <- K
  if (length(inp) > 3) biom[1] <- as.numeric(exp(inp[3]))
  if(schaefer) p <- 1 else p <- 1e-8
  for (index in 1:nyrs) { # index=1
    Bt <- biom[index]
    biom[index+1] <- max(Bt + ((r/p)*Bt*(1-(Bt/K)^p)-catch[index]),40)
  }
  pick <- which(indat[,"cpue"] > 0)
  qval <- exp(mean(log(indat[pick,"cpue"]/biom[pick])))
  answer[,"ModelB"] <- biom
  answer[,"Depletion"] <- biom/K
  answer[,"Harvest"] <- c(catch/biom[1:nyrs],NA)
  answer[,"CPUE"] <- c(cpue,NA)
  predCE <- biom * qval # apply same catchability across all years
  answer[,"predCE"] <- predCE
  msy <- r*K/((p+1)^((p+1)/p))
  params <- c(exp(inp),qval)
  if (length(params) == 4) {
    names(params) <- c("r","K","Sigma","q")
  } else {
    names(params) <- c("r","K","Binit","Sigma","q")
  }
  sumout <- c(msy,p,answer[(nyrs+1),"Depletion"],answer[1,"Depletion"],
              answer[(nyrs+1),"ModelB"])
  names(sumout) <- c("msy","p","FinalDepl","InitDepl","FinalB")
  output <- list(params,answer,msy,sumout,schaefer)
  names(output) <- c("parameters","outmat","msy","sumout","schaefer")
  return(output)
} # End of spm


#' @title spmboot conducts a bootstrap analysis on a spm model
#'
#' @description spmboot conducts a bootstrap analysis on a spm model. It 
#'     does this by saving the original fishery data, estimating the cpue 
#'     residuals, and multiplying the optimum predicted CPUE by a bootstrap 
#'     sample of the log-normal residuals (Haddon, 2011, p311). This 
#'     bootstrap sample of CPUE replaces the original fish[,"cpue"] and the 
#'     model is re-fitted. This is repeated iter times and the outputs 
#'     reported ready for the derivation of percentile confidence intervals. 
#'     The optimum solution is used as the first bootstrap replicate (it is 
#'     standard practice to include the original fit in the bootstrap 
#'     analysis). If 1000 replicates are run this procedure can take a 
#'     couple of minutes on a reasonably fast computer. A comparison of the 
#'     mean with the median should provide some notion of any bias in the 
#'     mean estimate.
#'
#' @param optpar The optimum model parameters from an earlier analysis
#' @param fishery fishery data containing the original observed cpue values
#' @param iter the number of boostrap replicates to be run
#' @param schaefer default=TRUE, should a Schaefer or a Fox model be run
#'
#' @return a list of two matrices. One containing the bootstrap parameters 
#'     and the other containing some of the dynamics, including the ModelB, 
#'     the bootstrap CPUE sample, the Depletion, and annual harvest rate.
#' @export
#'
#' @examples
#' \dontrun{
#'  data(dataspm); fish <- as.matrix(dataspm)
#'  pars <- log(c(r=0.24,K=5150,Binit=2800,0.15))
#'  ans <- fitSPM(pars,dataspm,schaefer=TRUE,maxiter=1000)
#'  boots <- spmboot(ans$estimate,fishery=fish,iter=500,schaefer=TRUE)
#'  dynam <- boots$dynam
#'  bootpar <- boots$bootpar
#'  rows <- colnames(bootpar)
#'  columns <- c(c(0.025,0.05,0.5,0.95,0.975),"Mean")
#'  bootCI <- matrix(NA,nrow=length(rows),ncol=length(columns),
#'                 dimnames=list(rows,columns))
#'  for (i in 1:length(rows)) {
#'    tmp <- sort(bootpar[,i])
#'    qtil <-  quantile(tmp,probs=c(0.025,0.05,0.5,0.95,0.975),na.rm=TRUE)
#'    bootCI[i,] <- c(qtil,mean(tmp,na.rm=TRUE))
#'  }
#'  round(bootCI,3)
#'  pairs(bootpar[,c("r","K","Binit","MSY")])
#' }
spmboot <- function(optpar,fishery,iter=100,schaefer=TRUE) {
  out <- spm(inp=optpar,indat=fishery,schaefer=schaefer)
  outmat <- out$outmat
  years <- fishery[,"year"]
  nyrs <- length(years)
  pickyr <- which(outmat[,"CPUE"] > 0)
  cpueO <- outmat[pickyr,"CPUE"]
  predO <- outmat[pickyr,"predCE"]  # original values
  resids <- cpueO/predO
  varib <- c("r","K","sigma","-veLL")
  lenpar <- length(optpar)
  if (lenpar > 3) varib <- c("r","K","Binit","sigma","-veLL")
  bootpar <- matrix(0,nrow=iter,ncol=length(varib),
                    dimnames=list(1:iter,varib))
  columns <- c("ModelB","BootCE","predCE","Depletion","Harvest")
  dynam <- array(0,dim=c(iter,nyrs,length(columns)),
                 dimnames=list(1:iter,years,columns))
  dynam[1,,] <- outmat[1:nyrs,c(2,6,7,4,5)]
  outspm <- fitSPM(optpar,fishery,schaefer=schaefer,maxiter=1000)
  bootpar[1,] <- c(outspm$estimate,outspm$minimum)
  for (i in 2:iter) { # i = 2
    cpueOB <- rep(NA,nyrs) # bootstrap sample
    cpueOB[pickyr] <- predO * sample(resids)
    fishery[,"cpue"] <- cpueOB
    outspm <- fitSPM(optpar,fishery,schaefer=schaefer,maxiter=1000)
    bootpar[i,] <- c(outspm$estimate,outspm$minimum)
    out <- spm(inp=outspm$estimate,indat=fishery,schaefer=schaefer)
    dynam[i,,] <- out$outmat[1:nyrs,c(2,6,7,4,5)]
  }
  bootpar[,1:lenpar] <- exp(bootpar[,1:lenpar]) # backtransform parameters
  if(schaefer) p <- 1 else p <- 1e-8
  MSY <- (bootpar[,"r"]*bootpar[,"K"])/((p+1)^((p+1)/p))
  Depl <- dynam[,nyrs,"Depletion"]
  Harv <- dynam[,nyrs,"Harvest"]
  bootpar <- cbind(bootpar,MSY,Depl,Harv)
  return(list(dynam=dynam,bootpar=bootpar))
} # end of spmboot

#' @title spmCE - calculates the dynamics for nultiple cpue time-series
#'
#' @description spmCE calculates the dynamics using a Schaefer of Fox model
#'     and is used instead of spm when there are multiple index vectors.
#'     The outputs include  predicted Biomass, year, catch, cpue, predicted
#'     cpue, contributions to q, ssq, and depletion levels. Generally it
#'     would be more sensible to use simpspm when fitting a Schaefer model 
#'     and simpfox when fitting a Fox model
#'     as those functions are designed to generate only the predicted cpue
#'     required by the functions ssq and negLLM, but the example shows how 
#'     it could be used. 
#'
#' @param inp a vector of 2 or 3 model parameters (r,K) or (r,K,Binit), you
#'     would use the latter if it was suspected that the fishery data 
#'     started after some initial depletion had occurred. Then there should 
#'     be the same number of sigma values as tehre are cpue time-series
#' @param indat a matrix with at least columns 'year', 'catch', and 'cpue'
#' @param schaefer a logical value determining whether the spm is to be a
#'     simple Schaefer model (p=1) or approximately a Fox model (p=1e-08). 
#'     The default is TRUE
#' @param year the column name within indat containing the years
#' @param cats the column name within indat containing the catches
#' @param index the column name within indat containing the cpue.
#' @return a list of five objects; outmat the matrix with the dynamics 
#'     results, q catchability, msy the maximum sustainable yield, the 
#'     parameter values, and sumout, which contains r, K, B0, msy, p, q, 
#'     Depl, FinalB, and InitDepl
#'     
#' @export
#'
#' @examples
#' \dontrun{
#'  data(twoindex)
#'  fish <- as.matrix(twoindex)
#'  pars <- log(c(0.04,155000,0.4,0.3))
#'  bestSP <- nlm(f=negLLM,p=pars,funk=simpspmM,indat=fish,
#'              schaefer=TRUE,logobs=log(fish[,c("cpue1","cpue2")]),
#'              steptol=1e-06,harvpen=TRUE)
#'  outfit(bestSP)  # best fitting estimates
#'  getMSY(exp(bestSP$estimate))
#' }
spmCE <- function(inp,indat,schaefer=TRUE,
                  year="year",cats="catch",index="cpue") {
  celoc <- grep(index,colnames(indat))
  nce <- length(celoc)
  npar <- 2 + nce   # r + K + nce_sigma
  years <- indat[,year]
  catch <- indat[,cats]
  cpue <- as.matrix(indat[,celoc])
  nyrs <- length(years)
  biom <- numeric(nyrs+1)
  qval <- numeric(nce)
  predCE <- matrix(NA,nrow=nyrs,ncol=nce)
  columns <- c("Year","ModelB","Catch","Depletion","Harvest")
  addtxt <- c("CPUE","predCE")
  if (nce > 1) {
    addtxt <- c(paste0("CPUE",1:nce),paste0("predCE",1:nce))
  }
  columns <- c(columns,addtxt)
  extendyrs <- c(years,(years[nyrs]+1))
  answer <- matrix(NA,nrow=(nyrs+1),ncol=length(columns),
                   dimnames=list(extendyrs,columns))
  answer[,"Catch"] <- c(catch,NA)
  answer[,"Year"] <- extendyrs
  r <- exp(inp[1])
  K <- exp(inp[2])
  biom[1] <-K
  if (length(inp) > npar) biom[1] <- exp(inp[3])
  if(schaefer) p <- 1 else p <- 1e-8
  for (index in 1:nyrs) {
    Bt <- biom[index]
    biom[index+1] <- max(Bt + ((r/p)*Bt*(1-(Bt/K)^p)-catch[index]),40)
  }
  answer[,"ModelB"] <- biom
  answer[,"Depletion"] <- biom/K
  answer[,"Harvest"] <- c(catch/biom[1:nyrs],NA)
  answer[,(5+1)] <- c(cpue[,1],NA)
  pick <- which(indat[,celoc[1]] > 0)
  qval[1] <- exp(mean(log(indat[pick,celoc[1]]/biom[pick])))
  predCE <- biom *qval[1] # apply same catchability across all years
  answer[,(5+nce+1)] <- predCE
  if (nce > 1) {
    for (i in 2:nce) {
      pick <- which(indat[,celoc[i]] > 0)
      qval[i] <- exp(mean(log(indat[pick,celoc[i]]/biom[pick])))
      predCE <- biom * qval[i]
      answer[,(5+i)] <- c(cpue[,i],NA)
      answer[,(5+nce+i)] <- predCE
    }
  }
  msy <- r*K/((p+1)^((p+1)/p))
  if (length(inp) == npar) { copyp <- c(inp,inp[2])
  } else { copyp <- inp
  }
  params <- exp(copyp)
  names(params) <- c("r","K","Binit",paste0("Sigma",1:nce))
  sumout <- c(msy,p,answer[(nyrs+1),"Depletion"],answer[1,"Depletion"],
              answer[(nyrs+1),"ModelB"],qval)
  names(sumout) <- c("msy","p","FinalDepl","InitDepl","FinalB","qs")
  output <- list(params,answer,msy,sumout)
  names(output) <- c("parameters","outmat","msy","sumout")
  return(output)
} # End of spmCE



#' @title spmphaseplot - plots the phase plot of harvest rate vs biomass
#'
#' @description spmphaseplot uses the output from plotspmmod to plot up
#'     the phase plot of harvest rate vs Biomass, marked with the limit and
#'     default targets. It identifies the start and end years (green and red
#'     dots) and permits the stock status to be determined visually. It also
#'     plots out the catch time-series and harvest rate time-series to aid 
#'     in interpretation of the phase plot.
#'
#' @param answer the object output by the function plotspmmod, containing the
#'     production curve, the fishery dynamics (predicted harvest rate and
#'     biomass through time).
#' @param Blim limit reference point, defaults to 0.2 so that 0.2B0 is used.
#' @param Btarg what target reference point will be used in the phase plot. 
#'     A default of 0.5 is used.
#' @param filename default is empty. If a filename is put here a .png file
#'     with that name will be put into the working directory.
#' @param resol the resolution of the png file, defaults to 200 dpi
#' @param fnt the font used in the plot and axes. Default=7, bold Times. 
#'     Using 6 gives Times, 1 will give SansSerif, 2 = bold Sans
#'
#' @return an invisible list of B0, Bmsy, Hmsy, and Hlim.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(dataspm)
#'   pars <- log(c(0.164,6740,3564,0.05))
#'   bestSP <- fitSPM(pars,fish=dataspm,funkone=TRUE)
#'   ans <- plotspmmod(bestSP$estimate,dataspm,schaefer=TRUE,addrmse=TRUE)
#'   str(ans)
#'   outs <- spmphaseplot(ans,fnt=7)
#'   str(outs)
#' }
spmphaseplot <- function(answer,Blim=0.2,Btarg=0.5,filename="",resol=200,
                         fnt=7) {
   lenfile <- nchar(filename)
   if (lenfile > 3) {
      end <- substr(filename,(lenfile-3),lenfile)
      if (end != ".png") filename <- paste0(filename,".png")
      png(filename=filename,width=5.5,height=5.0,units="in",res=resol)
   }
   prod <- answer$BiomProd
   rK <- answer$Dynamics$parameters
   B0 <- rK[2]
   Bmsy <- answer$Bmsy
   fishery <- answer$Dynamics$outmat
   Hmsy <- answer$MSY/answer$Bmsy
   Hmax <- getmax(fishery[,"Harvest"])
   numval <- length(which(fishery[,"Harvest"] > 0))
   pickD <- which.closest(0.2*B0,prod[,"x"])
   Hlim <- prod[pickD,"y"]/prod[pickD,"x"]
   par(mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0))
   par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=fnt,font=fnt,font.lab=fnt)
   layout(matrix(c(1,2)),heights=c(3,1))
   plot(fishery[,"ModelB"],fishery[,"Harvest"],type="l",lwd=2,col=1,
        xlab="Biomass",ylab="Annual Harvest Rate",ylim=c(0,Hmax),yaxs="i",
        xlim=c(0,B0))
   points(fishery[,"ModelB"],fishery[,"Harvest"],pch=16,cex=1.0,col=4)
   points(fishery[1,"ModelB"],fishery[1,"Harvest"],pch=16,cex=1.5,col=3)
   points(fishery[numval,"ModelB"],fishery[numval,"Harvest"],pch=16,
          cex=1.5,col=2)
   abline(v=c(Blim*B0,Btarg*B0,B0),col=c(2,3,3),lty=2)
   abline(h=c(Hmsy,Hlim),col=c(3,2),lty=2)
   text(Bmsy,0.05*Hmax,"Btarg",cex=1.0,font=fnt,pos=4)
   text(Blim*B0,0.05*Hmax,"Blim",cex=1.0,font=fnt,pos=4)
   text(0,0.95*Hlim,"Hlim",cex=1.0,font=fnt,pos=4)
   text(0,0.95*Hmsy,"Hmsy",cex=1.0,font=fnt,pos=4)
   # plot the catch and harvets rates
   yrs <- as.numeric(rownames(fishery))
   par(mai=c(0.3,0.45,0.05,0.45))
   cmax <- getmax(fishery[,"Catch"])
   plot(yrs,fishery[,"Catch"],type="l",lwd=2,col=2,ylab="",xlab="",
        ylim=c(0,cmax),yaxs="i",panel.first=grid(ny=0))
   par(new=TRUE)
   hmax <- getmax(fishery[,"Harvest"])
   plot(yrs,fishery[,"Harvest"],type="l",lwd=2,col=4,ylim=c(0,hmax),
        yaxt="n",ylab="",yaxs="i",xlab="")
   points(yrs[1],fishery[1,"Harvest"],pch=16,cex=1.5,col=3)
   points(yrs[numval],fishery[numval,"Harvest"],pch=16,cex=1.5,col=2)
   abline(h=c(Hmsy),col=c(3),lty=2)
   ym2 <- round(hmax,2)
   axis(side=4,at=seq(0,ym2,length=3),labels = seq(0,ym2,length=3))
   mtext("Catch (t)",side=2,outer=F,line=1.2,font=fnt,cex=1.0,col=2)
   mtext("Harvest Rate",side=4,outer=F,line=1.1,font=fnt,cex=1.0,col=4)
   if (lenfile > 0) {
      outfile <- paste0(getwd(),"/",filename)
      print(outfile)
      dev.off()
   }
   result <- list(B0=B0,Bmsy=Bmsy,Hmsy=Hmsy,Hlim=Hlim)
   return(invisible(result))
} # end of spmphaseplot

#' @title spmproj calculates biomass trajectories for replicate parameters
#' 
#' @description spmproj uses a matrix of parameter vectors to project 
#'     surplus production dynamics forward including future projection 
#'     years under constant catches. This is used to conduct risk 
#'     assessments for different constant catches allowing a search for
#'     optimum future catch levels.
#'
#' @param parmat a matrix of N parameter vectors obtained from either 
#'     asymptotic errors (parasympt), bootstraps (parsboot), or from a
#'     Bayesian analysis (parsbayes).
#' @param indat the fisheries data used during model fitting
#' @param constC the constant catch level imposed in the projection years
#' @param projyr the number of years of projection, default = 10
#' @param year name of the year variable within indat, default=year
#' @param cats name of the catch variable within indat, default=catch
#' @param index name of the cpue variable within indat, default=cpue
#'
#' @return an N x years matrix of biomass trajectories, one for each
#'     parameter vector
#' @export
#'
#' @examples
#' \dontrun{
#'   data(abdat)
#'   schf <- FALSE
#'   param <- log(c(r=0.3,K=11500,Binit=3300,sigma=0.05))
#'   bestmod <- nlm(f=negLL1,p=param,funk=simpspm,logobs=log(abdat$cpue),
#'                  indat=abdat,typsize=magnitude(param),iterlim=1000,
#'                  schaefer=schf,hessian = TRUE)
#'   out <- spm(bestmod$estimate,indat=abdat,schaefer=schf)
#'   matpar <- parasympt(bestmod,1000)
#'   projs <- spmproj(matpar,abdat,projyr=10,constC=900)
#'   plotproj(projs,out)
#' }  
spmproj <- function(parmat,indat,constC,projyr=10,
                    year="year",cats="catch",index="cpue") {
  N <- nrow(parmat)
  lastyr <- max(indat[,year])
  yrproj <- (lastyr+1):(lastyr+projyr)
  addrow <- cbind(yrproj,rep(constC,projyr),rep(NA,projyr))
  colnames(addrow) <- c(year,cats,index)
  projfish <- rbind(indat,addrow)
  yrs <- projfish[,year]
  numyr <- length(yrs) 
  projbio <- matrix(0,nrow=N,ncol=numyr,dimnames=list(1:N,yrs))
  for (i in 1:N) { # i=2  # calculate the dynamics for each parameter set
    dynP <- spm(log(parmat[i,1:4]),projfish,schaefer=FALSE)
    projbio[i,] <- dynP$outmat[1:numyr,"ModelB"]
  }
  return(projbio)
} # end of spmproj


#' @title spmprojDet conducts forward projections from known conditions
#' 
#' @description spmprojDet conducts deterministic forward projections of the 
#'     dynamics of a fitted surplus production model. The paramters and 
#'     original data need to be put through the function spm to form the 
#'     spmobj, i.e. the list generated by the function spm. This contains 
#'     all required information except for details of the projection.
#'     The application of spm is where the dynamics are defined as
#'     either Schaefer or Fox. If no plot is generated then the 
#'     projected dynamics are output invisibly, where the biomass and
#'     predCE are matrices of years vs projcatch.     
#'
#' @param spmobj the list generated by the function spm
#' @param projcatch the projected constant catch levels as a vector
#' @param projyr the number of years of projection. default = 10
#' @param plotout should the projection be plotted? default=FALSE
#' @param useft which font used in a plot? default=7 = bold times
#'
#' @return the projected biomass, CPUE, and the projected years
#' @export
#'
#' @examples
#' \dontrun{
#'  data(abdat)
#'  param <- log(c(r=0.3,K=11500,Binit=3300,sigma=0.05))
#'  bestmod <- nlm(f=negLL1,p=param,funk=simpspm,logobs=log(abdat$cpue),
#'                 indat=abdat,typsize=magnitude(param),iterlim=1000,
#'                 schaefer=FALSE)
#'  out <- spm(bestmod$estimate,indat=abdat,schaefer=FALSE)
#'  catches <- seq(700,1000,50)
#'  spmprojDet(spmobj = out,projcatch=catches,projyr=10,plotout=TRUE)
#' }
spmprojDet <- function(spmobj,projcatch,projyr=10,plotout=FALSE,useft=7) {
  if (spmobj$schaefer) p <- 1.0 else p <- 1e-08
  ep <- spmobj$parameters
  dyn=spmobj$outmat; 
  dynyr <- dim(dyn)[1] - 1
  yrs <- dyn[1:dynyr,"Year"]
  lastyr <- dyn[dynyr,"Year"]
  yrsp <- seq((lastyr+1),(lastyr+projyr),1)
  lastproj <- max(yrsp)
  numcat <- length(projcatch)
  biom <- matrix(0,nrow=projyr,ncol=numcat,dimnames=list(yrsp,projcatch))
  for (ctch in 1:numcat) {
    biom[1,ctch] <- dyn[(dynyr+1),"ModelB"]
    for (index in 1:(projyr-1)) {
      Bt <- biom[index,ctch]
      biom[index+1,ctch] <- max(Bt + ((ep[1]/p)*Bt*(1-(Bt/ep[2])^p)-
                                        projcatch[ctch]),40)
    }
  }  
  if (plotout) {
    for (ctch in 1:numcat) {
      if (ctch > 1) {
        lines(yrsp,biom[,ctch],lwd=2,col=2)
        text(lastproj,tail(biom[,ctch],1),projcatch[ctch],pos=4)
      } else {
        maxyr <- lastproj + 2
        par(mfrow=c(1,1), mai=c(0.45,0.45,0.05,0.05), mgp=c(1.35,0.35,0)) 
        par(cex=0.75, font.axis=useft, font=useft, font.lab=useft)  
        plot(dyn[,"Year"],dyn[,"ModelB"],type="l",lwd=2,
             xlim=c(1985,maxyr),ylim=c(0,6500),yaxs="i",
             xlab="Year",ylab="Stock Biomass (t)",panel.first=grid())
        abline(v=(lastyr+0.5),col=3,lwd=2)
      }
    }
  } # end of plotout ifloop
  predCE=spmobj$parameters[5]*biom
  answer <- list(biom=biom,predCE=predCE,projyear=yrsp)
  return(answer)
} # end spmprojDet

#' @title summspm extracts critical statistics from output of plotspmmod
#'
#' @description summspm extracts critical statistics from output of 
#'     plotspmmod. In particular it pulls out the catchability q, the MSY, 
#'     Bmsy, Dmsy, Blim, Btarg, Ctarg. and Dcurr.
#'
#' @param ans the object output from the function plotspmmod used to plot
#'     the output of a surplus production analysis
#'
#' @return a matrix of statistics relating to MSY, expected yields, and
#'     depletion levels
#' @export
#'
#' @examples
#' \dontrun{
#' data(dataspm)
#' plotfishM(dataspm,spsname="Pink Ling")
#' pars <- log(c(0.264,4740,3064,0.05))
#' ans <- plotspmmod(pars,dataspm,schaefer=FALSE)
#' bestSP <- fitSPM(pars,dataspm,schaefer=FALSE,maxiter=1000)
#' outfit(bestSP)
#' ans <- plotspmmod(bestSP$estimate,dataspm,schaefer=FALSE)
#' summspm(ans)
#' }
summspm <- function(ans) {
  output <- c(ans$Dynamics$parameters["q"],ans$MSY,ans$Bmsy,ans$Dmsy,
              ans$Blim,ans$Btarg, ans$Ctarg,ans$Dcurr)
  output <- as.matrix(cbind(1:8,round(output,4)))
  rownames(output) <- c("q","MSY","Bmsy","Dmsy","Blim","Btarg",
                        "Ctarg","Dcurr")
  colnames(output) <- c("Index","Statistic")
   return(output)
} # end of summspm


