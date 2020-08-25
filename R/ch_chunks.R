
# chapter2 --------

#' @title chapter2 The 14 R-code chunks from A Non-Introduction to R
#'
#' @description chapter2 contains no active function but rather acts 
#'     as a repository for the various example code chunks found in 
#'     chapter2. There are 15 r-code chunks in chapter2.
#'     
#' @name chapter2
#'
#' @examples
#' \dontrun{
#' # All the example code from  # A Non-Introduction to R
#' ### Using Functions
#' 
#' # R-chunk 1  Page 18
#' #make a function called countones2, don't overwrite original
#' 
#' countones2 <- function(x) return(length(which(x == 1)))  # or
#' countones3 <- function(x) return(length(x[x == 1]))
#' vect <- c(1,2,3,1,2,3,1,2,3)  # there are three ones
#' countones2(vect)  # should both give the answer: 3
#' countones3(vect)
#' set.seed(7100809) # if repeatability is desirable.
#' matdat <- matrix(trunc(runif(40)*10),nrow=5,ncol=8)
#' matdat #a five by eight matrix of random numbers between 0 - 9
#' apply(matdat,2,countones3)  # apply countones3 to 8 columns
#' apply(matdat,1,countones3)  # apply countones3 to 5 rows
#' 
#' 
#' # R-chunk 2 Page 19
#'  #A more complex function prepares to plot a single base graphic
#'  #It has the syntax for opening a window outside of Rstudio and
#'  #defining a base graphic. It includes oldpar<-par(no.readonly=TRUE)
#'  #which is returned invisibly so that the original 'par' settings
#'  #can be recovered using par(oldpar) after completion of your plot.
#' 
#' plotprep2 <- function(plots=c(1,1),width=6, height=3.75,usefont=7,
#'                       newdev=TRUE) {
#'   if ((names(dev.cur()) %in% c("null device","RStudioGD")) &
#'       (newdev)) {
#'     dev.new(width=width,height=height,noRStudioGD = TRUE)
#'   }
#'   oldpar <- par(no.readonly=TRUE)  # not in the book's example
#'   par(mfrow=plots,mai=c(0.45,0.45,0.1,0.05),oma=c(0,0,0,0))
#'   par(cex=0.75,mgp=c(1.35,0.35,0),font.axis=usefont,font=usefont,
#'       font.lab=usefont)
#'   return(invisible(oldpar))
#' }  #  see ?plotprep; see also parsyn() and parset()
#' 
#' 
#' ### Random Number Generation
#' # R-chunk 3 pages 20 - 21
#' #Examine the use of random seeds.
#' 
#' seed <- getseed()  # you will very likely get different naswers
#' set.seed(seed)
#' round(rnorm(5),5)
#' set.seed(123456)
#' round(rnorm(5),5)
#' set.seed(seed)
#' round(rnorm(5),5)
#'
#' ### Plotting in R
#' # R-chunk 4  page 22
#' #library(MQMF)   # The development of a simple graph  see Fig. 2.1
#' #The statements below open the RStudio graphics window, but opening
#' #a separate graphics window using plotprep is sometimes clearer.
#' 
#' data("LatA")  #LatA = length at age data; try properties(LatA)
#' #plotprep(width=6.0,height=5.0,newdev=FALSE) #unhash for external plot
#' oldpar <- par(no.readonly=TRUE)  # not in the book's example
#' setpalette("R4") #a more balanced, default palette see its help
#' par(mfrow=c(2,2),mai=c(0.45,0.45,0.1,0.05))  # see ?parsyn
#' par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
#' hist(LatA$age) #examine effect of different input parameters
#' hist(LatA$age,breaks=20,col=3,main="") # 3=green #try ?hist
#' hist(LatA$age,breaks=30,main="",col=4) # 4=blue
#' hist(LatA$age, breaks=30,col=2, main="", xlim=c(0,43), #2=red
#'      xlab="Age (years)",ylab="Count")
#' par(oldpar)  # not in the book's example
#' 
#' ### Dealing with Factors
#' # R-chunk 5  pages 23 - 24
#' #Dealing with factors/categories can be tricky
#' 
#' DepCat <- as.factor(rep(seq(300,600,50),2)); DepCat
#' try(5 * DepCat[3], silent=FALSE) #only returns NA and a warning!
#' as.numeric(DepCat) # returns the levels not the original values
#' as.numeric(levels(DepCat)) #converts 7 levels not the replicates
#' DepCat <- as.numeric(levels(DepCat))[DepCat] # try ?facttonum
#' #converts replicates in DepCat to numbers, not just the levels
#' 5 * DepCat[3]   # now treat DepCat as numeric
#' DepCat <- as.factor(rep(seq(300,600,50),2)); DepCat
#' facttonum(DepCat)
#' 
#' ## Writing Functions
#' # R-chunk 6  page 25
#' #Outline of a function's structure
#' 
#' functionname <- function(argument1, fun,...) {
#'   # body of the function
#'   #
#'   # the input arguments and body of a function can include other
#'   # functions, which may have their own arguments, which is what
#'   # the ... is for. One can include other inputs that are used but
#'   # not defined early on and may depend on what function is brought
#'   # into the main function. See for example negLL(), and others
#'   answer <- fun(argument1) + 2
#'   return(answer)
#' } # end of functionname
#' functionname(c(1,2,3,4,5),mean)  # = mean(1,2,3,4,5)= 3 + 2 = 5
#' 
#' ### Simple Functions
#' # R-chunk 7  page 26
#' # Implement the von Bertalanffy curve in multiple ways
#' 
#' ages <- 1:20
#' nages <- length(ages)
#' Linf <- 50;  K <- 0.2;  t0 <- -0.75
#' # first try a for loop to calculate length for each age
#' loopLt <- numeric(nages)
#' for (ag in ages) loopLt[ag] <- Linf * (1 - exp(-K * (ag - t0)))
#' # the equations are automatically vectorized so more efficient
#' vecLt <- Linf * (1 - exp(-K * (ages - t0))) # or we can convert
#' # the equation into a function and use it again and again
#' vB <- function(pars,inages) { # requires pars=c(Linf,K,t0)
#'   Lt <- pars[1] * (1 - exp(-pars[2] * (inages - pars[3])))
#'   return(Lt)
#' }
#' funLt <- vB(c(Linf,K,t0),ages)
#' ans <- cbind(ages,funLt,vecLt,loopLt)
#' 
#' 
#' # R-chunk 8  page 26 - code not shown in book.
#' # Tabulate the ans from chunk 7
#' 
#' library(knitr)  # needed for the function knitr - pretty tables
#' kable(halftable(ans,yearcol="ages",subdiv=2),digits=c(0,3,3,3,0,3,3,3))
#' 
#' 
#' # R-chunk 9  page 27
#' #A vB function with some input error checking
#' 
#' vB <- function(pars,inages) { # requires pars=c(Linf,K,t0)
#'   if (is.numeric(pars) & is.numeric(inages)) {
#'     Lt <- pars[1] * (1 - exp(-pars[2] * (inages - pars[3])))
#'   } else { stop(cat("Not all input values are numeric! \n")) }
#'   return(Lt)
#' }
#' param <- c(50, 0.2,"-0.75")
#' funLt <- vB(as.numeric(param),ages) #try without the as.numeric
#' halftable(cbind(ages,funLt))
#' 
#' 
#' ### Scoping of Objects
#' # R-chunk 10   page 29
#' # demonstration that the globel environment is 'visible' inside a
#' # a function it calls, but the function's environment remains
#' # invisible to the global or calling environment
#' 
#' vBscope <- function(pars) { # requires pars=c(Linf,K,t0)
#'   rhside <- (1 - exp(-pars[2] * (ages - pars[3])))
#'   Lt <- pars[1] * rhside
#'   return(Lt)
#' }
#' ages <- 1:10; param <- c(50,0.2,-0.75)
#' vBscope(param)
#' try(rhside)    # note the use of try() which can trap errors ?try
#' 
#' 
#' ### Function Inputs and Outputs
#' # R-chunk 11  page 30
#' #Bring the data-set schaef into the working of global environment
#' 
#' data(schaef)
#' 
#' 
#' # R-chunk 12   page 30  Table 2.2 code not shown
#' #Tabulate the data held in schaef. Needs knitr
#' 
#' kable(halftable(schaef,yearcol="year",subdiv=2),digits=c(0,0,0,4))
#' 
#' # R-chunk 13  page 30
#' #examine the properties of the data-set schaef
#' 
#' class(schaef)
#' a <- schaef[1:5,2]
#' b <- schaef[1:5,"catch"]
#' c <- schaef$catch[1:5]
#' cbind(a,b,c)
#' mschaef <- as.matrix(schaef)
#' mschaef[1:5,"catch"]  # ok
#' d <- try(mschaef$catch[1:5]) #invalid for matrices
#' d  # had we not used try()eveerything would have stopped.
#' 
#' 
#' # R-chunk 14  page 31
#' #Convert column names of a data.frame or matrix to lowercase
#' 
#' dolittle <- function(indat) {
#'   indat1 <- as.data.frame(indat)
#'   colnames(indat) <- tolower(colnames(indat))
#'   return(list(dfdata=indat1,indat=as.matrix(indat)))
#' } # return the original and the new version
#' colnames(schaef) <- toupper(colnames(schaef))
#' out <- dolittle(schaef)
#' str(out, width=63, strict.width="cut")
#' 
#' 
#' # R-chunk 15  page 32
#' #Could have used an S3 plot method had we defined a class   Fig.2.2
#' 
#' plotspmdat(schaef) # examine the code as an eg of a custom plot
#' }
NULL


# chapter3 --------

#' @title chapter3 The 27 R-code chunks from Simple Population Models
#'
#' @description chapter3 is not an active function but rather acts 
#'     as a repository for the various example code chunks found in 
#'     chapter3. There are 27 r-code chunks in chapter3.
#'     
#' @name chapter3
#'
#' @examples
#' \dontrun{
#' ### The Discrete Logistic Model    
#' # R-chunk 1 page 36
#' # Code to produce Figure 3.1. Note the two one-line functions 
#'    
#' surprod <- function(Nt,r,K) return((r*Nt)*(1-(Nt/K)))    
#' densdep <- function(Nt,K) return((1-(Nt/K)))    
#' r <- 1.2; K <- 1000.0; Nt <- seq(10,1000,10)  
#' oldpar <- par(no.readonly=TRUE) # this line not in book
#' # plotprep(width=7, height=5, newdev=FALSE)  
#' par(mfrow=c(2,1),mai=c(0.4,0.4,0.05,0.05),oma=c(0.0,0,0.0,0.0))     
#' par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)     
#' plot1(Nt,surprod(Nt,r,K),xlab="Population Nt",defpar=FALSE,    
#'       ylab="Production")    
#' plot1(Nt,densdep(Nt,K),xlab="Population Nt",defpar=FALSE,    
#'       ylab="Density-Dependence")    
#' par(oldpar)  # this line not in book
#' 
#' 
#' ### Dynamic Behaviour    
#' # R-chunk 2 page 38
#' #Code for Figure 3.2. Try varying the value of rv from 0.5-2.8 
#'    
#' yrs <- 100; rv=2.8;  Kv <- 1000.0; Nz=100; catch=0.0; p=1.0    
#' ans <- discretelogistic(r=rv,K=Kv,N0=Nz,Ct=catch,Yrs=yrs,p=p)    
#' avcatch <- mean(ans[(yrs-50):yrs,"nt"],na.rm=TRUE) #used in text    
#' label <- paste0("r=",rv," K=",Kv," Ct=",catch, " N0=",Nz," p=",p=p)   
#' oldpar <- par(no.readonly=TRUE) # this line not in book
#' plot(ans, main=label, cex=0.9, font=7) #Schaefer dynamics    
#' par(oldpar)   # this line not in book
#' 
#' 
#' # R-chunk 3 page 39
#' #run discrete logistic dynamics for 600 years  
#'   
#' yrs=600    
#' ans <- discretelogistic(r=2.55,K=1000.0,N0=100,Ct=0.0,Yrs=yrs)    
#' 
#' # R-chunk 4  page 40, code not in the book
#' #tabulate the last 30 years of the dynamics   needs knitr
#' library(knitr) 
#' kable(halftable(ans[(yrs-29):yrs,],yearcol="year",subdiv=3),digits=c(0,1,1,0,1,1,0,1,1))    
#' 
#' 
#' ### Finding Boundaries between Behaviours.    
#' # R-chunk 5 page 40
#' #run discretelogistic and search for repeated values of Nt    
#' 
#' yrs <- 600    
#' ans <- discretelogistic(r=2.55,K=1000.0,N0=100,Ct=0.0,Yrs=yrs)    
#' avt <- round(apply(ans[(yrs-100):(yrs-1),2:3],1,mean),2)    
#' count <- table(avt)    
#' count[count > 1] # with r=2.55 you should find an 8-cycle limit    
#' 
#' # R-chunk 6  page 41
#' #searches for unique solutions given an r value  see Table 3.2  
#' 
#' testseq <- seq(1.9,2.59,0.01)    
#' nseq <- length(testseq)    
#' result <- matrix(0,nrow=nseq,ncol=2,    
#'                  dimnames=list(testseq,c("r","Unique")))    
#' yrs <- 600    
#' for (i in 1:nseq) {  # i = 31    
#'   rval <- testseq[i]    
#'   ans <- discretelogistic(r=rval,K=1000.0,N0=100,Ct=0.0,Yrs=yrs)    
#'   ans <- ans[-yrs,] # remove last year, see str(ans) for why    
#'   ans[,"nt1"] <- round(ans[,"nt1"],3) #try hashing this out    
#'   result[i,] <- c(rval,length(unique(tail(ans[,"nt1"],100))))    
#' }    
#' 
#' 
#' # R-chunk 7  page 41 - 42, Table 3.2. Code not in the book.
#' #unique repeated Nt values 100 = non-equilibrium or chaos   
#'  
#' kable(halftable(result,yearcol = "r"),)    
#' 
#' 
#' ### Classical Bifurcation Diagram of Chaos    
#' # R-chunk 8 pages 42 - 43
#' #the R code for the bifurcation function   
#'  
#' bifurcation <- function(testseq,taill=100,yrs=1000,limy=0,incx=0.001){    
#'   nseq <- length(testseq)    
#'   result <- matrix(0,nrow=nseq,ncol=2,    
#'                    dimnames=list(testseq,c("r","Unique Values")))    
#'   result2 <- matrix(NA,nrow=nseq,ncol=taill)    
#'   for (i in 1:nseq) {      
#'     rval <- testseq[i]    
#'     ans <- discretelogistic(r=rval,K=1000.0,N0=100,Ct=0.0,Yrs=yrs)    
#'     ans[,"nt1"] <- round(ans[,"nt1"],4)    
#'     result[i,] <- c(rval,length(unique(tail(ans[,"nt1"],taill))))    
#'     result2[i,] <- tail(ans[,"nt1"],taill)    
#'   }      
#'   if (limy[1] == 0) limy <- c(0,getmax(result2,mult=1.02))  
#'     
#'   oldpar <- parset() #plot taill values against taill of each r value  
#'   on.exit(par(oldpar))    #  this line not in book 
#'   plot(rep(testseq[1],taill),result2[1,],type="p",pch=16,cex=0.1,    
#'        ylim=limy,xlim=c(min(testseq)*(1-incx),max(testseq)*(1+incx)),    
#'        xlab="r value",yaxs="i",xaxs="i",ylab="Equilibrium Numbers",    
#'        panel.first=grid())    
#'   for (i in 2:nseq)    
#'     points(rep(testseq[i],taill),result2[i,],pch=16,cex=0.1)    
#'   return(invisible(list(result=result,result2=result2)))    
#' } # end of bifurcation    
#' 
#' 
#' # R-chunk 9 page 43 
#' #Alternative r value arrangements for you to try; Fig 3.3    
#' #testseq <- seq(2.847,2.855,0.00001) #hash/unhash as needed    
#' #bifurcation(testseq,limy=c(600,740),incx=0.0001) # t    
#' #testseq <- seq(2.6225,2.6375,0.00001) # then explore     
#' #bifurcation(testseq,limy=c(660,730),incx=0.0001)  
#'    
#' testseq <- seq(1.9,2.975,0.0005) # modify to explore    
#' bifurcation(testseq,limy=0)      
#' 
#' 
#' ### The Effect of Fishing on Dynamics    
#' # R-chunk 10  page 43 - 44.
#' #Effect of catches on stability properties of discretelogistic   
#'  
#' yrs=50; Kval=1000.0    
#' nocatch <- discretelogistic(r=2.56,K=Kval,N0=500,Ct=0,Yrs=yrs)    
#' catch50 <- discretelogistic(r=2.56,K=Kval,N0=500,Ct=50,Yrs=yrs)    
#' catch200 <- discretelogistic(r=2.56,K=Kval,N0=500,Ct=200,Yrs=yrs)    
#' catch300 <- discretelogistic(r=2.56,K=Kval,N0=500,Ct=300,Yrs=yrs)    
#' 
#' 
#' # R-chunk 11 page 45
#' #Effect of different catches on n-cyclic behaviour Fig3.4   
#'  
#' plottime <- function(x,ylab) {    
#'   yrs <- nrow(x)    
#'   plot1(x[,"year"],x[,"nt"],ylab=ylab,defpar=FALSE)    
#'   avB <- round(mean(x[(yrs-40):yrs,"nt"],na.rm=TRUE),3)    
#'   mtext(avB,side=1,outer=F,line=-1.1,font=7,cex=1.0)     
#' } # end of plottime    
#' #the oma argument is used to adjust the space around the graph 
#' oldpar <- par(no.readonly=TRUE) # this line not in book   
#' par(mfrow=c(2,2),mai=c(0.25,0.4,0.05,0.05),oma=c(1.0,0,0.25,0))     
#' par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)      
#' plottime(nocatch,"Catch = 0")    
#' plottime(catch50,"Catch = 50")    
#' plottime(catch200,"Catch = 200")    
#' plottime(catch300,"Catch = 300")    
#' mtext("years",side=1,outer=TRUE,line=-0.2,font=7,cex=1.0)     
#' par(oldpar)
#' 
#' # R-chunk 12 page 46
#' #Phase plot for Schaefer model Fig 3.5    
#' 
#' plotphase <- function(x,label,ymax=0) { #x from discretelogistic    
#'   yrs <- nrow(x)    
#'   colnames(x) <- tolower(colnames(x))    
#'   if (ymax[1] == 0) ymax <- getmax(x[,c(2:3)])    
#'   plot(x[,"nt"],x[,"nt1"],type="p",pch=16,cex=1.0,ylim=c(0,ymax),    
#'        yaxs="i",xlim=c(0,ymax),xaxs="i",ylab="nt1",xlab="",    
#'        panel.first=grid(),col="darkgrey")    
#'   begin <- trunc(yrs * 0.6) #last 40% of yrs = 20, when yrs=50    
#'   points(x[begin:yrs,"nt"],x[begin:yrs,"nt1"],pch=18,col=1,cex=1.2)    
#'   mtext(label,side=1,outer=F,line=-1.1,font=7,cex=1.2)     
#' } # end of plotphase    
#' oldpar <- par(no.readonly=TRUE) # this line not in book   
#' par(mfrow=c(2,2),mai=c(0.25,0.25,0.05,0.05),oma=c(1.0,1.0,0,0))     
#' par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)      
#' plotphase(nocatch,"Catch = 0",ymax=1300)    
#' plotphase(catch50,"Catch = 50",ymax=1300)    
#' plotphase(catch200,"Catch = 200",ymax=1300)    
#' plotphase(catch300,"Catch = 300",ymax=1300)    
#' mtext("nt",side=1,outer=T,line=0.0,font=7,cex=1.0)    
#' mtext("nt+1",side=2,outer=T,line=0.0,font=7,cex=1.0)    
#' par(oldpar)    # this line not in book
#' 
#' ### Determinism    
#' ## Age-Structured Modelling Concepts    
#' ### Survivorship in a Cohort    
#' # R-chunk 13 pages 48 - 49
#' #Exponential population declines under different Z. Fig 3.6   
#'  
#' yrs <- 50;  yrs1 <- yrs + 1 # to leave room for B[0]    
#' years <- seq(0,yrs,1)    
#' B0 <- 1000        # now alternative total mortality rates    
#' Z <- c(0.05,0.1,0.2,0.4,0.55)     
#' nZ <- length(Z)    
#' Bt <- matrix(0,nrow=yrs1,ncol=nZ,dimnames=list(years,Z))    
#' Bt[1,] <- B0    
#' for (j in 1:nZ) for (i in 2:yrs1) Bt[i,j] <- Bt[(i-1),j]*exp(-Z[j])    
#' oldp <- plot1(years,Bt[,1],xlab="Years",ylab="Population Size",lwd=2)    
#' if (nZ > 1) for (j in 2:nZ) lines(years,Bt[,j],lwd=2,col=j,lty=j)    
#' legend("topright",legend=paste0("Z = ",Z),col=1:nZ,lwd=3,    
#'        bty="n",cex=1,lty=1:5)     
#' par(oldp)  # this line not in book
#' 
#' ### Instantaneous vs Annual Mortality Rates    
#' # R-chunk 14 page 51
#' #Prepare matrix of harvest rate vs time to appoximate F   
#'  
#' Z <- -log(0.5)    
#' timediv <- c(2,4,12,52,365,730,2920,8760,525600)    
#' yrfrac <- 1/timediv    
#' names(yrfrac) <- c("6mth","3mth","1mth","1wk","1d","12h","3h","1h","1m")    
#' nfrac <- length(yrfrac)    
#' columns <- c("yrfrac","divisor","yrfracH","Remain")    
#' result <- matrix(0,nrow=nfrac,ncol=length(columns),    
#'                  dimnames=list(names(yrfrac),columns))    
#' for (i in 1:nfrac) {    
#'   timestepmort <- Z/timediv[i]     
#'   N <- 1000    
#'   for (j in 1:timediv[i]) N <- N * (1-timestepmort)    
#'   result[i,] <- c(yrfrac[i],timediv[i],timestepmort,N)    
#' }    
#' 
#' 
#' # R-chunk 15 page 51 Table 3.3, code not shown in book
#' #output of constant Z for shorter and shorter periods    
#' 
#' kable(result,digits=c(10,0,8,4))    
#' 
#' 
#' # R-chunk 16 page 51
#' #Annual harvest rate against instantaneous F, Fig 3.7  
#'   
#' Fi <- seq(0.001,2,0.001)    
#' H <- 1 - exp(-Fi)    
#' oldpar <- parset()  # a wrapper for simplifying defining the par values    
#' plot(Fi,H,type="l",lwd=2,panel.first=grid(),xlab="Instantaneous Fishing Mortality F",    
#'      ylab="Annual Proportion Mortality H")    
#' lines(c(0,1),c(0,1),lwd=2,lty=2,col=2)    
#' par(oldpar)   # this line not in book
#' 
#' 
#' ## Simple Yield per Recruit    
#' # R-chunk 17  page 53
#' # Simple Yield-per-Recruit see Russell (1942)   
#'  
#' age <- 1:11;  nage <- length(age); N0 <- 1000  # some definitions    
#' # weight-at-age values    
#' WaA <- c(NA,0.082,0.175,0.283,0.4,0.523,0.7,0.85,0.925,0.99,1.0)    
#' # now the harvest rates    
#' H <- c(0.01,0.06,0.11,0.16,0.21,0.26,0.31,0.36,0.55,0.8)    
#' nH <- length(H)    
#' NaA <- matrix(0,nrow=nage,ncol=nH,dimnames=list(age,H)) # storage    
#' CatchN <- NaA;  CatchW <- NaA      # define some storage matrices    
#' for (i in 1:nH) {                # loop through the harvest rates    
#'   NaA[1,i] <- N0  # start each harvest rate with initial numbers    
#'   for (age in 2:nage) {  # loop through over-simplified dynamics    
#'     NaA[age,i] <- NaA[(age-1),i] * (1 - H[i])    
#'     CatchN[age,i] <- NaA[(age-1),i] - NaA[age,i]    
#'   }    
#'   CatchW[,i] <- CatchN[,i] * WaA    
#' }                      # transpose the vector of total catches to    
#' totC <- t(colSums(CatchW,na.rm=TRUE))   # simplify later printing    
#' 
#' 
#' # R-chunk 18 page 54 Table 3.4 code not shown in book
#' #Tabulate numbers-at-age for different harvest rates  needs knitr
#'   
#' kable(NaA,digits=c(0,0,0,0,0,0,0,0,1,1),row.names=TRUE)    
#' 
#' 
#' # R-chunk 19 page 54, Table 3.5, code not shown in book.
#' #Tabulate Weight-at-age for different harvest rates   
#'  
#' kable(CatchW[2:11,],digits=c(2,2,2,2,2,2,2,2,2,2),row.names=TRUE)    
#' 
#' 
#' # R-chunk 20 page 54, Table 3.6, code not shown in book.
#' #Total weights vs Harvest rate   
#'  
#' kable(totC,digits=c(1,1,1,1,1,1,1,1,1,1))    
#' 
#' 
#' # R-chunk 21 page 55
#' #Use MQMF::plot1 for a quick plot of the total catches. Figure 3.8    
#' 
#' oldpar <- plot1(H,totC,xlab="Harvest Rate",ylab="Total Yield",lwd=2)    
#' par(oldpar) # to reset the par values if desired
#' 
#' ### Selectivity in Yield-per-Recruit    
#' # R-chunk 22 Page 56
#' #Logistic S shaped cureve for maturity    
#' 
#' ages <- seq(0,50,1)    
#' sel1 <- mature(-3.650425,0.146017,sizeage=ages) #-3.65/0.146=25    
#' sel2 <- mature(-6,0.2,ages)    
#' sel3 <- mature(-6,0.24,ages)    
#' oldp <- plot1(ages,sel1,xlab="Age Yrs",ylab="Selectivity",cex=0.75,lwd=2)    
#' lines(ages,sel2,col=2,lwd=2,lty=2)    
#' lines(ages,sel3,col=3,lwd=2,lty=3)    
#' abline(v=25,col="grey",lty=2)     
#' abline(h=c(0.25,0.5,0.75),col="grey",lty=2)    
#' legend("topleft",c("25_15.04","30_10.986","25_9.155"),col=c(1,2,3),    
#'        lwd=3,cex=1.1,bty="n",lty=1:3)    
#' par(oldp)
#' 
#' ### The Baranov Catch Equation    
#' # R-chunk 23 Page 58
#' # Baranov catch equation  
#'   
#' age <- 0:12;  nage <- length(age)     
#' sa <-mature(-4,2,age) #selectivity-at-age    
#' H <- 0.2;  M <- 0.35    
#' FF <- -log(1 - H)#Fully selected instantaneous fishing mortality    
#' Ft <- sa * FF     # instantaneous Fishing mortality-at-age    
#' N0 <- 1000    
#' out <- cbind(bce(M,Ft,N0,age),"Select"=sa)  # out becomes Table 3.7    
#' 
#' 
#' # R-chunk 24 page 59, Table 3.7, code not shown in book.
#' #tabulate output from Baranov Catch Equations     
#' 
#' kable(out,digits=c(3,3,3,3))    
#' 
#' 
#' ### Growth and Weight-at-Age   
#' ## Full Yield-per-Recruit    
#' # R-chunk 25 Page 60 - 61
#' # A more complete YPR analysis    
#' 
#' age <- 0:20;  nage <- length(age) #storage vectors and matrices    
#' laa <- vB(c(50.0,0.25,-1.5),age) # length-at-age    
#' WaA <- (0.015 * laa ^ 3.0)/1000  # weight-at-age as kg    
#' H <- seq(0.01,0.65,0.05);  nH <- length(H)       
#' FF <- round(-log(1 - H),5)  # Fully selected fishing mortality    
#' N0 <- 1000    
#' M <- 0.1    
#' numt <- matrix(0,nrow=nage,ncol=nH,dimnames=list(age,FF))    
#' catchN <- matrix(0,nrow=nage,ncol=nH,dimnames=list(age,FF))    
#' as50 <- c(1,2,3)      
#' yield <- matrix(0,nrow=nH,ncol=length(as50),dimnames=list(H,as50))    
#' for (sel in 1:length(as50)) {    
#'   sa <- logist(as50[sel],1.0,age)  # selectivity-at-age    
#'   for (harv in 1:nH) {    
#'     Ft <- sa * FF[harv]      # Fishing mortality-at-age    
#'     out <- bce(M,Ft,N0,age)    
#'     numt[,harv] <- out[,"Nt"]    
#'     catchN[,harv] <- out[,"Catch"]    
#'     yield[harv,sel] <- sum(out[,"Catch"] * WaA,na.rm=TRUE)    
#'   } # end of harv loop    
#' } # end of sel loop    
#' 
#' 
#' # R-chunk 26  Page 61
#' #A full YPR analysis  Figure 3.10    
#' 
#' oldp <- plot1(H,yield[,3],xlab="Harvest Rate",ylab="Yield",cex=0.75,lwd=2)    
#' lines(H,yield[,2],lwd=2,col=2,lty=2)    
#' lines(H,yield[,1],lwd=2,col=3,lty=3)    
#' legend("bottomright",legend=as50,col=c(3,2,1),lwd=3,bty="n",    
#'        cex=1.0,lty=c(3,2,1))     
#' par(oldp)
#' 
#' # R-chunk 27 page 62, Table 3.8, code not shown in book.
#' #Tabulate yield-per-recruit using Baranoc catch equation   
#'  
#' kable(yield,digits=c(2,3,3,3))    
#' 
#' }
NULL


# chapter4 --------

#' @title chapter4 The 47 R-code chunks from Model Parameter Estimation
#'
#' @description chapter4 is not an active function but rather acts 
#'     as a repository for the various example code chunks found in 
#'     chapter4. There are 47 r-code chunks in chapter3.
#'     
#' @name chapter4
#'
#' @examples
#' \dontrun{
#' # All the example code from  # Model Parameter Estimation    
#' # Model Parameter Estimation    
#' ## Introduction    
#' ### Optimization    
#' ## Criteria of Best Fit    
#' ## Model Fitting in R    
#' ### Model Requirements    
#' ### A Length-at-Age Example    
#' ### Alternative Models of Growth    
#' ## Sum of Squared Residual Deviations    
#' ### Assumptions of Least-Squares    
#' ### Numerical Solutions   
#'  
#' # R-chunk 1  Page 75 
#' #setup optimization using growth and ssq    
#' #convert equations 4.4 to 4.6 into vectorized R functions    
#' #These will over-write the same functions in the MQMF package    
#' 
#' data(LatA)      # try ?LatA   assumes library(MQMF) already run    
#' vB <- function(p, ages) return(p[1]*(1-exp(-p[2]*(ages-p[3]))))    
#' Gz <- function(p, ages) return(p[1]*exp(-p[2]*exp(p[3]*ages)))    
#' mm <- function(p, ages) return((p[1]*ages)/(p[2] + ages^p[3]))    
#' #specific function to calc ssq. The ssq within MQMF is more    
#' ssq <- function(p,funk,agedata,observed) {        #general and is    
#'   predval <- funk(p,agedata)        #not limited to p and agedata    
#'   return(sum((observed - predval)^2,na.rm=TRUE))    
#' } #end of ssq     
#' # guess starting values for Linf, K, and t0, names not needed    
#' pars <- c("Linf"=27.0,"K"=0.15,"t0"=-2.0) #ssq should=1478.449    
#' ssq(p=pars, funk=vB, agedata=LatA$age, observed=LatA$length)     
#' # try misspelling LatA$Length with a capital. What happens?    
#' 
#' ### Passing Functions as Arguments to other Functions    
#' # R-chunk 2  Page 76 
#' # Illustrates use of names within function arguments    
#' 
#' vB <- function(p,ages) return(p[1]*(1-exp(-p[2] *(ages-p[3]))))    
#' ssq <- function(funk,observed,...) { # only define ssq arguments    
#'   predval <- funk(...) # funks arguments are implicit    
#'   return(sum((observed - predval)^2,na.rm=TRUE))    
#' } # end of ssq     
#' pars <- c("Linf"=27.0,"K"=0.15,"t0"=-2.0) # ssq should = 1478.449    
#' ssq(p=pars, funk=vB, ages=LatA$age, observed=LatA$length) #if no    
#' ssq(vB,LatA$length,pars,LatA$age) # name order is now vital!    
#' 
#' 
#' # R-chunk 3  Page 77
#' # Illustrate a problem with calling a function in a function    
#' # LatA$age is typed as LatA$Age but no error, and result = 0    
#' 
#' ssq(funk=vB, observed=LatA$length, p=pars, ages=LatA$Age) # !!!    
#' 
#' ### Fitting the Models    
#' # R-chunk 4  Page 77
#' #plot the LatA data set   Figure 4.2    
#' 
#' oldpar <- parset()   # parset and getmax are two MQMF functions     
#' ymax <- getmax(LatA$length) # simplifies use of base graphics. For    
#' # full colour, with the rgb as set-up below, there must be >= 5 obs    
#' plot(LatA$age,LatA$length,type="p",pch=16,cex=1.2,xlab="Age Years",     
#'      ylab="Length cm",col=rgb(1,0,0,1/5),ylim=c(0,ymax),yaxs="i",    
#'      xlim=c(0,44),panel.first=grid()) 
#' par(oldpar) # this line not in book   
#' 
#' # R-chunk 5  Pages 78 - 79 
#' # use nlm to fit 3 growth curves to LatA, only p and funk change   
#' 
#' ages <- 1:max(LatA$age) # used in comparisons     
#' pars <- c(27.0,0.15,-2.0) # von Bertalanffy    
#' bestvB <- nlm(f=ssq,funk=vB,observed=LatA$length,p=pars,    
#'               ages=LatA$age,typsize=magnitude(pars))    
#' outfit(bestvB,backtran=FALSE,title="vB"); cat("\n")     
#' pars <- c(26.0,0.7,-0.5) # Gompertz    
#' bestGz <- nlm(f=ssq,funk=Gz,observed=LatA$length,p=pars,    
#'               ages=LatA$age,typsize=magnitude(pars))    
#' outfit(bestGz,backtran=FALSE,title="Gz"); cat("\n")     
#' pars <- c(26.2,1.0,1.0) # Michaelis-Menton - first start point    
#' bestMM1 <- nlm(f=ssq,funk=mm,observed=LatA$length,p=pars,    
#'                ages=LatA$age,typsize=magnitude(pars))    
#' outfit(bestMM1,backtran=FALSE,title="MM"); cat("\n")    
#' pars <- c(23.0,1.0,1.0) # Michaelis-Menton - second start point    
#' bestMM2 <- nlm(f=ssq,funk=mm,observed=LatA$length,p=pars,    
#'                ages=LatA$age,typsize=magnitude(pars))    
#' outfit(bestMM2,backtran=FALSE,title="MM2"); cat("\n")     
#' 
#' # R-chunk 6  Page 81 
#' #The use of args() and formals()     
#' 
#' args(nlm) # formals(nlm) uses more screen space. Try yourself.    
#' 
#' 
#' # R-chunk 7  Page 81, code not in the book 
#' #replacement for args(nlm) to keep within page borders without truncation   
#' 
#' {cat("function (f, p, ..., hessian = FALSE, typsize = rep(1,\n")    
#'   cat("  length(p)),fscale = 1, print.level = 0, ndigit = 12, \n")    
#'   cat("  gradtol = 1e-06, stepmax = max(1000 * \n")    
#'   cat("  sqrt(sum((p/typsize)^2)), 1000), steptol = 1e-06, \n")     
#'   cat("  iterlim = 100, check.analyticals = TRUE)\n")}    
#' 
#' 
#' # R-chunk 8  Pages 81 - 82 
#' #Female length-at-age + 3 growth fitted curves Figure 4.3    
#' 
#' predvB <- vB(bestvB$estimate,ages) #get optimumpredicted lengths    
#' predGz <- Gz(bestGz$estimate,ages) # using the outputs    
#' predmm <- mm(bestMM2$estimate,ages) #from the nlm analysis above    
#' ymax <- getmax(LatA$length) #try ?getmax or getmax [no brackets]    
#' xmax <- getmax(LatA$age)  #there is also a getmin, not used here    
#' oldpar <- parset(font=7) #or use parsyn() to prompt for par syntax    
#' plot(LatA$age,LatA$length,type="p",pch=16, col=rgb(1,0,0,1/5),    
#'      cex=1.2,xlim=c(0,xmax),ylim=c(0,ymax),yaxs="i",xlab="Age",    
#'      ylab="Length (cm)",panel.first=grid())    
#' lines(ages,predvB,lwd=2,col=4)        # vB    col=4=blue    
#' lines(ages,predGz,lwd=2,col=1,lty=2)  # Gompertz  1=black    
#' lines(ages,predmm,lwd=2,col=3,lty=3)  # MM        3=green    
#' #notice the legend function and its syntax.    
#' legend("bottomright",cex=1.2,c("von Bertalanffy","Gompertz",    
#'                    "Michaelis-Menton"),col=c(4,1,3),lty=c(1,2,3),lwd=3,bty="n") 
#' par(oldpar)  # this line not in book                       
#' 
#' ### Objective Model Selection    
#' ### The Influence of Residual Error Choice on Model Fit    
#' # R-chunk 9  Page 84 - 85 
#' # von Bertalanffy     
#' 
#' pars <- c(27.25,0.15,-3.0)    
#' bestvBN <- nlm(f=ssq,funk=vB,observed=LatA$length,p=pars,    
#'                ages=LatA$age,typsize=magnitude(pars),iterlim=1000)    
#' outfit(bestvBN,backtran=FALSE,title="Normal errors"); cat("\n")     
#' # modify ssq to account for log-normal errors in ssqL    
#' ssqL <- function(funk,observed,...) {    
#'   predval <- funk(...)    
#'   return(sum((log(observed) - log(predval))^2,na.rm=TRUE))    
#' } # end of ssqL    
#' bestvBLN <- nlm(f=ssqL,funk=vB,observed=LatA$length,p=pars,    
#'                 ages=LatA$age,typsize=magnitude(pars),iterlim=1000)    
#' outfit(bestvBLN,backtran=FALSE,title="Log-Normal errors")    
#' 
#' 
#' # R-chunk 10  Pages 85 - 86
#' # Now plot the resultibng two curves and the data Fig 4.4    
#' 
#' predvBN <- vB(bestvBN$estimate,ages)     
#' predvBLN <- vB(bestvBLN$estimate,ages)     
#' ymax <- getmax(LatA$length)     
#' xmax <- getmax(LatA$age)        
#' oldpar <- parset()                  
#' plot(LatA$age,LatA$length,type="p",pch=16, col=rgb(1,0,0,1/5),    
#'      cex=1.2,xlim=c(0,xmax),ylim=c(0,ymax),yaxs="i",xlab="Age",    
#'      ylab="Length (cm)",panel.first=grid())    
#' lines(ages,predvBN,lwd=2,col=4,lty=2)   # add Normal dashed    
#' lines(ages,predvBLN,lwd=2,col=1)        # add Log-Normal solid    
#' legend("bottomright",c("Normal Errors","Log-Normal Errors"),    
#'        col=c(4,1),lty=c(2,1),lwd=3,bty="n",cex=1.2)    
#' par(oldpar)
#' 
#' ### Remarks on Initial Model Fitting    
#' ## Maximum Likelihood     
#' ### Introductory Examples    
#' # R-chunk 11  Page 88
#' # Illustrate Normal random likelihoods. see Table 4.1    
#' 
#' set.seed(12345)       # make the use of random numbers repeatable    
#' x <- rnorm(10,mean=5.0,sd=1.0)      # pseudo-randomly generate 10     
#' avx <- mean(x)                      # normally distributed values    
#' sdx <- sd(x)          # estimate the mean and stdev of the sample              
#' L1 <- dnorm(x,mean=5.0,sd=1.0)   # obtain likelihoods, L1, L2 for     
#' L2 <- dnorm(x,mean=avx,sd=sdx)    # each data point for both sets    
#' result <- cbind(x,L1,L2,"L2gtL1"=(L2>L1))      # which is larger?    
#' result <- rbind(result,c(NA,prod(L1),prod(L2),1)) # result+totals    
#' rownames(result) <- c(1:10,"product")    
#' colnames(result) <- c("x","original","estimated","est > orig")    
#' 
#' 
#' # R-chunk 12  page 88, Table 4.1, code not shown in book.
#' #tabulate results of Normal Likelihoods    
#' 
#' kable(result,digits=c(4,8,8,0),row.names = TRUE, caption='(ref:tab401)')    
#' 
#' # R-chunk 13  Page 89 
#' # some examples of pnorm, dnorm, and qnorm, all mean = 0    
#' 
#' cat("x = 0.0        Likelihood =",dnorm(0.0,mean=0,sd=1),"\n")     
#' cat("x = 1.95996395 Likelihood =",dnorm(1.95996395,mean=0,sd=1),"\n")     
#' cat("x =-1.95996395 Likelihood =",dnorm(-1.95996395,mean=0,sd=1),"\n")     
#' # 0.5 = half cumulative distribution    
#' cat("x = 0.0        cdf = ",pnorm(0,mean=0,sd=1),"\n")     
#' cat("x = 0.6744899  cdf = ",pnorm(0.6744899,mean=0,sd=1),"\n")    
#' cat("x = 0.75       Quantile =",qnorm(0.75),"\n") # reverse pnorm    
#' cat("x = 1.95996395 cdf = ",pnorm(1.95996395,mean=0,sd=1),"\n")    
#' cat("x =-1.95996395 cdf = ",pnorm(-1.95996395,mean=0,sd=1),"\n")    
#' cat("x = 0.975      Quantile =",qnorm(0.975),"\n") # expect ~1.96    
#' # try x <- seq(-5,5,0.2); round(dnorm(x,mean=0.0,sd=1.0),5)    
#' 
#' ## Likelihoods from the Normal Distribution    
#' # R-chunk 14   Page 90
#' # Density plot and cumulative distribution for Normal   Fig 4.5    
#' 
#' x <- seq(-5,5,0.1)  # a sequence of values around a mean of 0.0    
#' NL <- dnorm(x,mean=0,sd=1.0)   # normal likelihoods for each X    
#' CD <- pnorm(x,mean=0,sd=1.0)   # cumulative density vs X    
#' oldp <- plot1(x,CD,xlab="x = StDev from Mean",ylab="Likelihood and CDF")    
#' lines(x,NL,lwd=3,col=2,lty=3) # dashed line as these are points    
#' abline(h=0.5,col=4,lwd=1)  
#' par(oldp)  
#' 
#' # R-chunk 15  Pages 91 - 92 
#' #function facilitates exploring different polygons Fig 4.6    
#' 
#' plotpoly <- function(mid,delta,av=5.0,stdev=1.0) {    
#'   neg <- mid-delta;  pos <- mid+delta    
#'   pdval <- dnorm(c(mid,neg,pos),mean=av,sd=stdev)    
#'   polygon(c(neg,neg,mid,neg),c(pdval[2],pdval[1],pdval[1],    
#'                                pdval[2]),col=rgb(0.25,0.25,0.25,0.5))    
#'   polygon(c(pos,pos,mid,pos),c(pdval[1],pdval[3],pdval[1],    
#'                                pdval[1]),col=rgb(0,1,0,0.5))       
#'   polygon(c(mid,neg,neg,mid,mid),    
#'           c(0,0,pdval[1],pdval[1],0),lwd=2,lty=1,border=2)    
#'   polygon(c(mid,pos,pos,mid,mid),    
#'           c(0,0,pdval[1],pdval[1],0),lwd=2,lty=1,border=2)     
#'   text(3.395,0.025,paste0("~",round((2*(delta*pdval[1])),7)),  
#'        cex=1.1,pos=4)    
#'   return(2*(delta*pdval[1])) # approx probability, see below    
#' } # end of plotpoly, a temporary function to enable flexibility    
#' #This code can be re-run with different values for delta    
#' x <- seq(3.4,3.6,0.05) # where under the normal curve to examine    
#' pd <- dnorm(x,mean=5.0,sd=1.0) #prob density for each X value    
#' mid <- mean(x)        
#' delta <- 0.05  # how wide either side of the sample mean to go?     
#' oldpar <- parset() #pre-defined MQMF base graphics set-up for par    
#' ymax <- getmax(pd) # find maximum y value for the plot    
#' plot(x,pd,type="l",xlab="Variable x",ylab="Probability Density",    
#'      ylim=c(0,ymax),yaxs="i",lwd=2,panel.first=grid())    
#' approxprob <- plotpoly(mid,delta)  #use function defined above 
#' par(oldpar)  # this line not in the book
#' 
#' ### Equivalence with Sum-of-Squares     
#' ### Fitting a Model to Data using Normal Likelihoods    
#' # R-chunk 16  Page 94 
#' #plot of length-at-age data  Fig 4.7    
#' 
#' data(LatA) # load the redfish data set into memory and plot it    
#' ages <- LatA$age;  lengths <- LatA$length    
#' oldpar <- plot1(ages,lengths,xlab="Age",ylab="Length",type="p",cex=0.8,    
#'       pch=16,col=rgb(1,0,0,1/5))    
#' par(oldpar)
#' 
#' 
#' # R-chunk 17  Page 95
#' # Fit the vB growth curve using maximum likelihood    
#' 
#' pars <- c(Linf=27.0,K=0.15,t0=-3.0,sigma=2.5) # starting values    
#' # note, estimate for sigma is required for maximum likelihood    
#' ansvB <- nlm(f=negNLL,p=pars,funk=vB,observed=lengths,ages=ages,    
#'              typsize=magnitude(pars))    
#' outfit(ansvB,backtran=FALSE,title="vB by minimum -veLL")    
#' 
#' # R-chunk 18 Page 96
#' #Now fit the Michaelis-Menton curve    
#' 
#' pars <- c(a=23.0,b=1.0,c=1.0,sigma=3.0) # Michaelis-Menton  
#' ansMM <- nlm(f=negNLL,p=pars,funk=mm,observed=lengths,ages=ages,    
#'              typsize=magnitude(pars))    
#' outfit(ansMM,backtran=FALSE,title="MM by minimum -veLL")    
#' 
#' # R-chunk 19  Page 96 
#' #plot optimum solutions for vB and mm. Fig 4.8    
#' 
#' Age <- 1:max(ages) # used in comparisons     
#' predvB <- vB(ansvB$estimate,Age) #optimum solution    
#' predMM <- mm(ansMM$estimate,Age) #optimum solution    
#' oldpar <- parset()               # plot the deata points first  
#' plot(ages,lengths,xlab="Age",ylab="Length",type="p",pch=16,    
#'      ylim=c(10,33),panel.first=grid(),col=rgb(1,0,0,1/3))    
#' lines(Age,predvB,lwd=2,col=4)     # then add the growth curves  
#' lines(Age,predMM,lwd=2,col=1,lty=2)    
#' legend("bottomright",c("von Bertalanffy","Michaelis-Menton"),    
#'        col=c(4,1),lwd=3,bty="n",cex=1.2,lty=c(1,2)) 
#' par(oldpar)   
#' 
#' # R-chunk 20   Pages 96 - 97
#' # residual plot for vB curve   Fig 4.9    
#' 
#' predvB <- vB(ansvB$estimate,ages) # predicted values for age data    
#' resids <- lengths - predvB               # calculate vB residuals     
#' oldpar <- plot1(ages,resids,type="p",col=rgb(1,0,0,1/3),
#'            xlim=c(0,43),pch=16,xlab="Ages Years",ylab="Residuals")    
#' abline(h=0.0,col=1,lty=2)    # emphasize the zero line  
#' par(oldpar)
#' 
#' ## Log-Normal Likelihoods     
#' ### Simplification of Log-Normal Likelihoods    
#' ### Log-Normal Properties    
#' # R-chunk 21  Page 100 
#' # meanlog and sdlog affects on mode and spread of lognormal Fig 4.10     
#' 
#' x <- seq(0.05,5.0,0.01)  # values must be greater than 0.0    
#' y <- dlnorm(x,meanlog=0,sdlog=1.2,log=FALSE) #dlnorm=likelihoods    
#' y2 <- dlnorm(x,meanlog=0,sdlog=1.0,log=FALSE)#from log-normal     
#' y3 <- dlnorm(x,meanlog=0,sdlog=0.6,log=FALSE)#distribution     
#' y4 <- dlnorm(x,0.75,0.6)         #log=TRUE = log-likelihoods    
#' oldpar <- parset(plots=c(1,2)) #MQMF base plot formatting function    
#' plot(x,y3,type="l",lwd=2,panel.first=grid(),    
#'      ylab="Log-Normal Likelihood")    
#' lines(x,y,lwd=2,col=2,lty=2)    
#' lines(x,y2,lwd=2,col=3,lty=3)    
#' lines(x,y4,lwd=2,col=4,lty=4)    
#' legend("topright",c("meanlog sdlog","    0.0      0.6    0.0",    
#'                     "      1.0","    0.0      1.2","    0.75    0.6"),    
#'        col=c(0,1,3,2,4),lwd=3,bty="n",cex=1.0,lty=c(0,1,3,2,4))    
#' plot(log(x),y3,type="l",lwd=2,panel.first=grid(),ylab="")    
#' lines(log(x),y,lwd=2,col=2,lty=2)    
#' lines(log(x),y2,lwd=2,col=3,lty=3)    
#' lines(log(x),y4,lwd=2,col=4,lty=4)  
#' par(oldpar)  # return par to old settings; this line not in book
#' 
#' 
#' # R-chunk 22  Pages 100 - 101
#' 
#' set.seed(12354) # plot random log-normal numbers as Fig 4.11    
#' meanL <- 0.7;   sdL <- 0.5  # generate 5000 random log-normal     
#' x <- rlnorm(5000,meanlog = meanL,sdlog = sdL) # values    
#' oldpar <- parset(plots=c(1,2)) # simplifies plots par() definition    
#' hist(x[x < 8.0],breaks=seq(0,8,0.25),col=0,main="")     
#' meanx <- mean(log(x)); sdx <- sd(log(x))    
#' outstat <- c(exp(meanx-(sdx^2)),exp(meanx),exp(meanx+(sdx^2)/2))    
#' abline(v=outstat,col=c(4,1,2),lwd=3,lty=c(1,2,3))    
#' legend("topright",c("mode","median","bias-correct"),    
#'        col=c(4,1,2),lwd=3,bty="n",cex=1.2,lty=c(1,2,3))    
#' outh <- hist(log(x),breaks=30,col=0,main="")   # approxnormal    
#' hans <- addnorm(outh,log(x)) #MQMF function; try  ?addnorm    
#' lines(hans$x,hans$y,lwd=3,col=1) # type addnorm into the console 
#' par(oldpar)  # return par to old settings; this line not in book   
#' 
#' 
#' # R-chunk 23  Page 101  
#' #examine log-normal propoerties. It is a bad idea to reuse     
#' 
#' set.seed(12345) #'random' seeds, use getseed() for suggestions    
#' meanL <- 0.7;   sdL <- 0.5  #5000 random log-normal values then    
#' x <- rlnorm(5000,meanlog = meanL,sdlog = sdL) #try with only 500     
#' meanx <- mean(log(x)); sdx <- sd(log(x))    
#' cat("               Original  Sample \n")    
#' cat("Mode(x)     = ",exp(meanL - sdL^2),outstat[1],"\n")    
#' cat("Median(x)   = ",exp(meanL),outstat[2],"\n")    
#' cat("Mean(x)     = ",exp(meanL + (sdL^2)/2),outstat[3],"\n")    
#' cat("Mean(log(x) =  0.7     ",meanx,"\n")    
#' cat("sd(log(x)   =  0.5     ",sdx,"\n")    
#' 
#' ### Fitting a Curve using Log-Normal Likelihoods    
#' # R-chunk 24  Page 103 
#' # fit a Beverton-Holt recruitment curve to tigers data Table 4.2    
#' 
#' data(tigers)   # use the tiger prawn data set    
#' lbh <- function(p,biom) return(log((p[1]*biom)/(p[2] + biom)))    
#' #note we are returning the log of Beverton-Holt recruitment   
#' pars <- c("a"=25,"b"=4.5,"sigma"=0.4)   # includes a sigma    
#' best <- nlm(negNLL,pars,funk=lbh,observed=log(tigers$Recruit),    
#'             biom=tigers$Spawn,typsize=magnitude(pars))    
#' outfit(best,backtran=FALSE,title="Beverton-Holt Recruitment")    
#' predR <- exp(lbh(best$estimate,tigers$Spawn))     
#' #note exp(lbh(...)) is the median because no bias adjustment    
#' result <- cbind(tigers,predR,tigers$Recruit/predR)    
#' 
#' # R-chunk 25  Page 103 
#' # Fig 4.12 visual examination of the fit to the tigers data    
#' 
#' oldp <- plot1(tigers$Spawn,predR,xlab="Spawning Biomass","Recruitment",    
#'       maxy=getmax(c(predR,tigers$Recruit)),lwd=2)    
#' points(tigers$Spawn,tigers$Recruit,pch=16,cex=1.1,col=2)  
#' par(oldp)  # return par to old settings; this line not in book   
#' 
#' # R-chunk 26  page 104, Table 4.12, code not shown in book. 
#' #tabulating observed, predicted and residual recruitment    
#' 
#' colnames(result) <- c("SpawnB","Recruit","PredR","Residual")    
#' kable(result,digits=c(1,1,3,4), caption='(ref:tab402)')    
#' 
#' ### Fitting a Dynamic Model using Log-Normal Errors    
#' # R-chunk 27  Page 106
#' 
#' data(abdat)  # plot abdat fishery data using a MQMF helper  Fig 4.13    
#' plotspmdat(abdat) # function to quickly plot catch and cpue  
#' 
#' 
#' # R-chunk 28  Pages 106 - 107 
#' # Use log-transformed parameters for increased stability when    
#' # fitting the surplus production model to the abdat data-set    
#' 
#' param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05))     
#' obslog <- log(abdat$cpue) #input log-transformed observed data    
#' bestmod <- nlm(f=negLL,p=param,funk=simpspm,indat=as.matrix(abdat),    
#'                logobs=obslog)  # no typsize, or iterlim needed    
#' #backtransform estimates, outfit's default, as log-transformed     
#' outfit(bestmod,backtran = TRUE,title="abdat")        # in param    
#' 
#' 
#' # R-chunk 29  Pages 107 - 108 
#' # Fig 4.14 Examine fit of predicted to data    
#' 
#' predce <- simpspm(bestmod$estimate,abdat) #compare obs vs pred    
#' ymax <- getmax(c(predce,obslog))    
#' oldp <- plot1(abdat$year,obslog,type="p",maxy=ymax,ylab="Log(CPUE)",    
#'       xlab="Year",cex=0.9)    
#' lines(abdat$year,predce,lwd=2,col=2) 
#' par(oldp)  # return par to old settings; this line not in book    
#' 
#' 
#' ## Likelihoods from the Binomial Distribution    
#' ### An Example using Binomial Likelihoods    
#' # R-chunk 30  Page 109 
#' #Use Binomial distribution to test biased sex-ratio Fig 4.15    
#' 
#' n <- 60    # a sample of 60 animals    
#' p <- 0.5   # assume a sex-ration of 1:1     
#' m <- 1:60  # how likely is each of the 60 possibilites?    
#' binom <- dbinom(m,n,p)   # get individual likelihoods    
#' cumbin <- pbinom(m,n,p)  # get cumulative distribution    
#' oldp <- plot1(m,binom,type="h",xlab="Number of Males",ylab="Probability")     
#' abline(v=which.closest(0.025,cumbin),col=2,lwd=2) # lower 95% CI    
#' par(oldp)  # return par to old settings; this line not in book  
#' 
#' 
#' # R-chunk 31  Page 111 
#' # plot relative likelihood of different p values Fig 4.16    
#' 
#' n <- 60  # sample size; should really plot points as each independent     
#' m <- 20  # number of successes = finding a male    
#' p <- seq(0.1,0.6,0.001) #range of probability we find a male     
#' lik <- dbinom(m,n,p)    # R function for binomial likelihoods    
#' oldp <- plot1(p,lik,type="l",xlab="Prob. of 20 Males",ylab="Prob.")    
#' abline(v=p[which.max(lik)],col=2,lwd=2) # try "p" instead of "l" 
#' par(oldp)  # return par to old settings; this line not in book     
#' 
#' # R-chunk 32  Page 111 
#' # find best estimate using optimize to finely search an interval    
#' 
#' n <- 60; m <- 20  # trials and successes    
#' p <- c(0.1,0.6) #range of probability we find a male     
#' optimize(function(p) {dbinom(m,n,p)},interval=p,maximum=TRUE)    
#' 
#' ### Open Bay Juvenile Fur Seal Population Size    
#' # R-chunk 33  Page 112 
#' # Juvenile furseal data-set Greaves, 1992.  Table 4.3    
#' 
#' furseal <- c(32,222,1020,704,1337,161.53,31,181,859,593,1125,    
#'              135.72,29,185,936,634,1238,153.99)    
#' columns <- c("tagged(m)","Sample(n)","Population(X)",    
#'              "95%Lower","95%Upper","StErr")    
#' furs <- matrix(furseal,nrow=3,ncol=6,dimnames=list(NULL,columns),    
#'                byrow=TRUE)    
#' #tabulate fur seal data Table 4.3    
#' kable(furs, caption='(ref:tab403)')    
#' 
#' # R-chunk 34  Pages 113 - 114 
#' # analyse two pup counts 32 from 222, and 31 from 181, rows 1-2 in    
#' # Table 4.3.   Now set-up storage for solutions    
#' 
#' optsol <- matrix(0,nrow=2,ncol=2,    
#'                  dimnames=list(furs[1:2,2],c("p","Likelihood")))    
#' X <- seq(525,1850,1) # range of potential population sizes    
#' p <- 151/X  #range of proportion tagged; 151 originally tagged    
#' m <- furs[1,1] + 1 #tags observed, with Bailey's adjustment    
#' n <- furs[1,2] + 1 # sample size with Bailey's adjustment    
#' lik1 <- dbinom(m,n,p) # individaul likelihoods    
#' #find best estimate with optimize to finely search an interval    
#' #use unlist to convert the output list into a vector    
#' #Note use of Bailey's adjustment (m+1), (n+1) Caughley, (1977)    
#' optsol[1,] <- unlist(optimize(function(p) {dbinom(m,n,p)},p,    
#'                               maximum=TRUE))    
#' m <- furs[2,1]+1;  n <- furs[2,2]+1 #repeat for sample2    
#' lik2 <- dbinom(m,n,p)      
#' totlik <- lik1 * lik2 #Joint likelihood of 2 vectors    
#' optsol[2,] <- unlist(optimize(function(p) {dbinom(m,n,p)},p,    
#'                               maximum=TRUE))    
#' 
#' # R-chunk 35  Page 114
#' # Compare outcome for 2 independent seal estimates Fig 4.17    
#' # Should plot points not a line as each are independent     
#' 
#' oldp <- plot1(X,lik1,type="l",xlab="Total Pup Numbers",    
#'       ylab="Probability",maxy=0.085,lwd=2)    
#' abline(v=X[which.max(lik1)],col=1,lwd=1)    
#' lines(X,lik2,lwd=2,col=2,lty=3)  # add line to plot    
#' abline(v=X[which.max(lik2)],col=2,lwd=1) # add optimum    
#' #given p = 151/X, then X = 151/p and p = optimum proportion     
#' legend("topright",legend=round((151/optsol[,"p"])),col=c(1,2),lwd=3,    
#'        bty="n",cex=1.1,lty=c(1,3)) 
#' par(oldp)  # return par to old settings; this line not in book     
#' 
#' ### Using Multiple Independent Samples    
#' 
#' # R-chunk 36  Pages 114 - 115 
#' #Combined likelihood from 2 independent samples Fig 4.18    
#' 
#' totlik <- totlik/sum(totlik) # rescale so the total sums to one    
#' cumlik <- cumsum(totlik) #approx cumulative likelihood for CI        
#' oldp <- plot1(X,totlik,type="l",lwd=2,xlab="Total Pup Numbers",    
#'       ylab="Posterior Joint Probability")    
#' percs <- c(X[which.closest(0.025,cumlik)],X[which.max(totlik)],    
#'            X[which.closest(0.975,cumlik)])    
#' abline(v=percs,lwd=c(1,2,1),col=c(2,1,2))    
#' legend("topright",legend=percs,lwd=c(2,4,2),bty="n",col=c(2,1,2),    
#'        cex=1.2)  # now compare with averaged count    
#' m <- furs[3,1];  n <- furs[3,2] # likelihoods for the     
#' lik3 <- dbinom(m,n,p)            # average of six samples    
#' lik4 <- lik3/sum(lik3)  # rescale for comparison with totlik    
#' lines(X,lik4,lwd=2,col=3,lty=2) #add 6 sample average to plot 
#' par(oldp)  # return par to old settings; this line not in book     
#' 
#' ### Analytical Approaches    
#' ## Other Distributions    
#' ## Likelihoods from the Multinomial Distribution    
#' ### Using the Multinomial Distribution    
#' # R-chunk 37  Page 119 
#' #plot counts x shell-length of 2 cohorts   Figure 4.19    
#' 
#' cw <- 2  # 2 mm size classes, of which mids are the centers    
#' mids <- seq(8,54,cw) #each size class = 2 mm as in 7-9, 9-11, ...    
#' obs <- c(0,0,6,12,35,40,29,23,13,7,10,14,11,16,11,11,9,8,5,2,0,0,0,0)    
#' # data from (Helidoniotis and Haddon, 2012)    
#' dat <- as.matrix(cbind(mids,obs)) #xy matrix needed by inthist    
#' oldp <- parset()  #set up par declaration then use an MQMF function     
#' inthist(dat,col=2,border=3,width=1.8, #histogram of integers    
#'         xlabel="Shell Length mm",ylabel="Frequency",xmin=7,xmax=55)   
#' par(oldp)  # return par to old settings; this line not in book    
#' 
#' # R-chunk 38  Page 121
#' #cohort data with 2 guess-timated normal curves Fig 4.20    
#' 
#' oldp <- parset()  # set up the required par declaration    
#' inthist(dat,col=0,border=8,width=1.8,xlabel="Shell Length mm",    
#'         ylabel="Frequency",xmin=7,xmax=55,lwd=2)  # MQMF function          
#' #Guess normal parameters and plot those curves on histogram    
#' av <- c(18.0,34.5)    # the initial trial and error means and    
#' stdev <- c(2.75,5.75)  # their standard deviations    
#' prop1 <- 0.55       #  proportion of observations in cohort 1    
#' n <- sum(obs) #262 observations, now calculate expected counts    
#' cohort1 <- (n*prop1*cw)*dnorm(mids,av[1],stdev[1]) # for each    
#' cohort2 <- (n*(1-prop1)*cw)*dnorm(mids,av[2],stdev[2])# cohort    
#' #(n*prop1*cw) scales likelihoods to suit the 2mm class width    
#' lines(mids,cohort1,lwd=2,col=1)    
#' lines(mids,cohort2,lwd=2,col=4) 
#' par(oldp)  # return par to old settings; this line not in book     
#' 
#' # R-chunk 39 Page 122 
#' #wrapper function for calculating the multinomial log-likelihoods    
#' #using predfreq and mnnegLL, Use ? and examine their code    
#' 
#' wrapper <- function(pars,obs,sizecl,midval=TRUE) {    
#'   freqf <- predfreq(pars,sum(obs),sizecl=sizecl,midval=midval)    
#'   return(mnnegLL(obs,freqf))    
#' } # end of wrapper which uses MQMF::predfreq and MQMF::mnnegLL    
#' mids <- seq(8,54,2) # each size class = 2 mm as in 7-9, 9-11, ...    
#' av <- c(18.0,34.5)   # the trial and error means and    
#' stdev <- c(2.95,5.75)  # standard deviations    
#' phi1 <- 0.55      # proportion of observations in cohort 1    
#' pars <-c(av,stdev,phi1)  # combine parameters into a vector    
#' wrapper(pars,obs=obs,sizecl=mids) # calculate total -veLL    
#' 
#' # R-chunk 40  Page 122 
#' # First use the midpoints    
#' 
#' bestmod <- nlm(f=wrapper,p=pars,obs=obs,sizecl=mids,midval=TRUE,     
#'                typsize=magnitude(pars))    
#' outfit(bestmod,backtran=FALSE,title="Using Midpts"); cat("\n")    
#' #Now use the size class bounds and cumulative distribution    
#' #more sensitive to starting values, so use best pars from midpoints    
#' X <- seq((mids[1]-cw/2),(tail(mids,1)+cw/2),cw)    
#' bestmodb <- nlm(f=wrapper,p=bestmod$estimate,obs=obs,sizecl=X,    
#'                 midval=FALSE,typsize=magnitude(pars))    
#' outfit(bestmodb,backtran=FALSE,title="Using size-class bounds")     
#' 
#' # R-chunk 41 Page 123 
#' #prepare the predicted Normal distribution curves    
#' 
#' pars <- bestmod$estimate # best estimate using mid-points    
#' cohort1 <- (n*pars[5]*cw)*dnorm(mids,pars[1],pars[3])     
#' cohort2 <- (n*(1-pars[5])*cw)*dnorm(mids,pars[2],pars[4])     
#' parsb <- bestmodb$estimate # best estimate with bounds    
#' nedge <- length(mids) + 1  # one extra estimate    
#' cump1 <- (n*pars[5])*pnorm(X,pars[1],pars[3])#no need to rescale    
#' cohort1b <- (cump1[2:nedge] - cump1[1:(nedge-1)])     
#' cump2 <- (n*(1-pars[5]))*pnorm(X,pars[2],pars[4])  # cohort 2    
#' cohort2b <- (cump2[2:nedge] - cump2[1:(nedge-1)])    
#' 
#' # R-chunk 42  Page 123 
#' #plot the alternate model fits to cohorts  Fig 4.21    
#' 
#' oldp <- parset() #set up required par declaration; then plot curves    
#' pick <- which(mids < 28)    
#' inthist(dat[pick,],col=0,border=8,width=1.8,xmin=5,xmax=28,    
#'         xlabel="Shell Length mm",ylabel="Frequency",lwd=3)     
#' lines(mids,cohort1,lwd=3,col=1,lty=2) # have used setpalette("R4")    
#' lines(mids,cohort1b,lwd=2,col=4)      # add the bounded results    
#' label <- c("midpoints","bounds")      # very minor differences   
#' legend("topleft",legend=label,lwd=3,col=c(1,4),bty="n",    
#'        cex=1.2,lty=c(2,1))    
#' par(oldp)  # return par to old settings; this line not in book  
#' 
#' # R-chunk 43  Page 124 
#' # setup table of results for comparison of fitting strategies    
#' 
#' predmid <- rowSums(cbind(cohort1,cohort2))    
#' predbnd <- rowSums(cbind(cohort1b,cohort2b))    
#' result <- as.matrix(cbind(mids,obs,predmid,predbnd,predbnd-predmid))    
#' colnames(result) <- c("mids","Obs","Predmid","Predbnd","Difference")    
#' result <- rbind(result,c(NA,colSums(result,na.rm=TRUE)[2:5]))    
#' 
#' 
#' # R-chunk 44  page 125, Table 4.4, code not shown in book. 
#' #tabulate the results of fitting cohort data  in two ways    
#' 
#' kable(result,digits=c(0,0,4,4,4),align=c("r","r","r","r","r"))    
#' 
#' ## Likelihoods from the Gamma Distribution    
#' 
#' # R-chunk 45  Pages 126 - 127 
#' #Illustrate different Gamma function curves  Figure 4.22    
#' 
#' X <- seq(0.0,10,0.1) #now try different shapes and scale values    
#' dg <- dgamma(X,shape=1,scale=1)     
#' oldp <- plot1(X,dg,xlab = "Quantile","Probability Density")    
#' lines(X,dgamma(X,shape=1.5,scale=1),lwd=2,col=2,lty=2)    
#' lines(X,dgamma(X,shape=2,scale=1),lwd=2,col=3,lty=3)    
#' lines(X,dgamma(X,shape=4,scale=1),lwd=2,col=4,lty=4)    
#' legend("topright",legend=c("Shape 1","Shape 1.5","Shape 2",    
#'                            "Shape 4"),lwd=3,col=c(1,2,3,4),bty="n",cex=1.25,lty=1:4)    
#' mtext("Scale c = 1",side=3,outer=FALSE,line=-1.1,cex=1.0,font=7)    
#' par(oldp)  # return par to old settings; this line not in book  
#' 
#' ## Likelihoods from the Beta Distribution    
#' # R-chunk 46  Pages 127 - 128
#' #Illustrate different Beta function curves. Figure 4.23    
#' 
#' x <- seq(0, 1, length = 1000)    
#' oldp <- parset()    
#' plot(x,dbeta(x,shape1=3,shape2=1),type="l",lwd=2,ylim=c(0,4),    
#'      yaxs="i",panel.first=grid(), xlab="Variable 0 - 1",     
#'      ylab="Beta Probability Density - Scale1 = 3")    
#' bval <- c(1.25,2,4,10)    
#' for (i in 1:length(bval))     
#'   lines(x,dbeta(x,shape1=3,shape2=bval[i]),lwd=2,col=(i+1),lty=c(i+1))    
#' legend(0.5,3.95,c(1.0,bval),col=c(1:7),lwd=2,bty="n",lty=1:5)    
#' par(oldp)  # return par to old settings; this line not in book  
#' 
#' 
#' ## Bayes' Theorem    
#' ### Introduction    
#' ### Bayesian Methods    
#' ### Prior Probabilities    
#' # R-chunk 47  Pages 132 - 133 
#' # can prior probabilities ever be uniniformative?  Figure 4.24    
#' 
#' x <- 1:1000    
#' y <- rep(1/1000,1000)    
#' cumy <- cumsum(y)    
#' group <- sort(rep(c(1:50),20))    
#' xlab <- seq(10,990,20)  
#' oldp <- par(no.readonly=TRUE)  # this line not in book  
#' par(mfrow=c(2,1),mai=c(0.45,0.3,0.05,0.05),oma=c(0.0,1.0,0.0,0.0))     
#' par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)      
#' yval <- tapply(y,group,sum)    
#' plot(x,cumy,type="p",pch=16,cex=0.5,panel.first=grid(),    
#'      xlim=c(0,1000),ylim=c(0,1),ylab="",xlab="Linear Scale")    
#' plot(log(x),cumy,type="p",pch=16,cex=0.5,panel.first=grid(),    
#'      xlim=c(0,7),xlab="Logarithmic Scale",ylab="")    
#' mtext("Cumulative Probability",side=2,outer=TRUE,cex=0.9,font=7)
#' par(oldp)  # return par to old settings; this line not in book      
#' }
NULL


# chapter5 --------

#' @title chapter5 The 23 R-code chunks from Static Models
#'
#' @description chapter5 is not an active function but rather acts 
#'     as a repository for the various example code chunks found in 
#'     chapter5. There are 23 r-code chunks in chapter5. You should,
#'     of course, feel free to use and modify any of these example 
#'     chunks in your own work.
#'     
#' @name chapter5
#'
#' @examples
#' \dontrun{
#' # All the example code from  # Static Models     
#' # Static Models     
#' ## Introduction     
#' ## Productivity Parameters     
#' ## Growth     
#' ### Seasonal Growth Curves    
#'  
#' # R-chunk 1  Page 138
#'  #vB growth curve fit to Pitcher and Macdonald derived seasonal data     
#'  
#' data(minnow); week <- minnow$week; length <- minnow$length     
#' pars <- c(75,0.1,-10.0,3.5); label=c("Linf","K","t0","sigma")     
#' bestvB <- nlm(f=negNLL,p=pars,funk=vB,ages=week,observed=length,     
#'               typsize=magnitude(pars))     
#' predL <- vB(bestvB$estimate,0:160)     
#' outfit(bestvB,backtran = FALSE,title="Non-Seasonal vB",parnames=label)     
#'  
#' # R-chunk 2  Page 139
#'  #plot the non-seasonal fit and its residuals.  Figure 5.1     
#'  
#' oldp <- parset(plots=c(2,1),margin=c(0.35,0.45,0.02,0.05))      
#' plot1(week,length,type="p",cex=1.0,col=2,xlab="Weeks",pch=16,     
#'       ylab="Length (mm)",defpar=FALSE)     
#' lines(0:160,predL,lwd=2,col=1)     
#'  # calculate and plot the residuals     
#' resids <- length - vB(bestvB$estimate,week)     
#' plot1(week,resids,type="l",col="darkgrey",cex=0.9,lwd=2,     
#'     xlab="Weeks",lty=3,ylab="Normal Residuals",defpar=FALSE)     
#' points(week,resids,pch=16,cex=1.1,col="red")     
#' abline(h=0,col=1,lwd=1) 
#' par(oldp)  # return par to old settings; this line not in book      
#'      
#' # R-chunk 3  Pages 139 - 140
#'  # Fit seasonal vB curve, parameters = Linf, K, t0, C, s, sigma     
#'  
#' svb <- function(p,ages,inc=52) {     
#'   return(p[1]*(1 - exp(-(p[4] * sin(2*pi*(ages - p[5])/inc) +      
#'                            p[2] * (ages - p[3])))))     
#' } # end of svB     
#' spars <- c(bestvB$estimate[1:3],0.1,5,2.0)  # keep sigma at end     
#' bestsvb <- nlm(f=negNLL,p=spars,funk=svb,ages=week,observed=length,     
#'               typsize=magnitude(spars))      
#' predLs <- svb(bestsvb$estimate,0:160)     
#' outfit(bestsvb,backtran = FALSE,title="Seasonal Growth",     
#'        parnames=c("Linf","K","t0","C","s","sigma"))     
#'  
#' # R-chunk 4  Page 140
#'  #Plot seasonal growth curve and residuals   Figure 5.2     
#'  
#' oldp <- parset(plots=c(2,1))  # MQMF utility wrapper function     
#' plot1(week,length,type="p",cex=0.9,col=2,xlab="Weeks",pch=16,     
#'       ylab="Length (mm)",defpar=FALSE)     
#' lines(0:160,predLs,lwd=2,col=1)     
#'  # calculate and plot the residuals     
#' resids <- length - svb(bestsvb$estimate,week)     
#' plot1(week,resids,type="l",col="darkgrey",cex=0.9,xlab="Weeks",     
#'       lty=3,ylab="Normal Residuals",defpar=FALSE)     
#' points(week,resids,pch=16,cex=1.1,col="red")     
#' abline(h=0,col=1,lwd=1)  
#' par(oldp)  # return par to old settings; this line not in book     
#'  
#' ### Fabens Method with Tagging Data     
#' # R-chunk 5  Pages 142 - 143
#'  # tagging growth increment data from Black Island, Tasmania     
#'  
#' data(blackisland);  bi <- blackisland # just to keep things brief     
#' oldp <- parset()     
#' plot(bi$l1,bi$dl,type="p",pch=16,cex=1.0,col=2,ylim=c(-1,33),     
#'      ylab="Growth Increment mm",xlab="Initial Length mm",     
#'      panel.first = grid())     
#' abline(h=0,col=1)  
#' par(oldp)  # return par to old settings; this line not in book     
#'  
#' ### Fitting Models to Tagging Data     
#' # R-chunk 6  Page 144
#'  # Fit the vB and Inverse Logistic to the tagging data     
#'  
#' linm <- lm(bi$dl ~ bi$l1) # simple linear regression     
#' param <- c(170.0,0.3,4.0); label <- c("Linf","K","sigma")     
#' modelvb <- nlm(f=negNLL,p=param,funk=fabens,observed=bi$dl,indat=bi,     
#'                initL="l1",delT="dt") # could have used the defaults     
#' outfit(modelvb,backtran = FALSE,title="vB",parnames=label)     
#' predvB <- fabens(modelvb$estimate,bi)     
#' cat("\n")     
#' param2 <- c(25.0,130.0,35.0,3.0)      
#' label2=c("MaxDL","L50","delta","sigma")     
#' modelil <- nlm(f=negNLL,p=param2,funk=invl,observed=bi$dl,indat=bi,     
#'                initL="l1",delT="dt")     
#' outfit(modelil,backtran = FALSE,title="IL",parnames=label2)     
#' predil <- invl(modelil$estimate,bi)     
#'  
#' # R-chunk 7  Page 145
#'  #growth curves and regression fitted to tagging data Fig 5.4     
#'  
#' oldp <- parset(margin=c(0.4,0.4,0.05,0.05))     
#' plot(bi$l1,bi$dl,type="p",pch=16,cex=1.0,col=3,ylim=c(-2,31),     
#'      ylab="Growth Increment mm",xlab="Length mm",panel.first=grid())     
#' abline(h=0,col=1)     
#' lines(bi$l1,predvB,pch=16,col=1,lwd=3,lty=1)  # vB     
#' lines(bi$l1,predil,pch=16,col=2,lwd=3,lty=2)  # IL     
#' abline(linm,lwd=3,col=7,lty=2) # add dashed linear regression     
#' legend("topright",c("vB","LinReg","IL"),lwd=3,bty="n",cex=1.2,     
#'                     col=c(1,7,2),lty=c(1,2,2))     
#' par(oldp)  # return par to old settings; this line not in book  
#'  
#' # R-chunk 8  Pages 145 - 146
#'  #residuals for vB and inverse logistic for tagging data Fig 5.5     
#'  
#' oldp <- parset(plots=c(1,2),outmargin=c(1,1,0,0),margin=c(.25,.25,.05,.05))     
#' plot(bi$l1,(bi$dl - predvB),type="p",pch=16,col=1,ylab="",     
#'      xlab="",panel.first=grid(),ylim=c(-8,11))     
#' abline(h=0,col=1)     
#' mtext("vB",side=1,outer=FALSE,line=-1.1,cex=1.2,font=7)     
#' plot(bi$l1,(bi$dl - predil),type="p",pch=16,col=1,ylab="",     
#'      xlab="",panel.first=grid(),ylim=c(-8,11))     
#' abline(h=0,col=1)     
#' mtext("IL",side=3,outer=FALSE,line=-1.2,cex=1.2,font=7)     
#' mtext("Length mm",side=1,line=-0.1,cex=1.0,font=7,outer=TRUE)     
#' mtext("Residual",side=2,line=-0.1,cex=1.0,font=7,outer=TRUE)   
#' par(oldp)  # return par to old settings; this line not in book    
#'  
#' ### A Closer Look at the Fabens Methods     
#' ### Implementation of Non-Constant Variances     
#' # R-chunk 9  Page 149
#'  # fit the Fabens tag growth curve with and without the option to      
#'  # modify variation with predicted length. See the MQMF function     
#'  # negnormL. So first no variation and then linear variation.      
#'  
#' sigfunk <- function(pars,predobs) return(tail(pars,1)) #no effect     
#' data(blackisland)     
#' bi <- blackisland # just to keep things brief     
#' param <- c(170.0,0.3,4.0); label=c("Linf","K","sigma")     
#' modelvb <- nlm(f=negnormL,p=param,funk=fabens,funksig=sigfunk,     
#'                indat=bi,initL="l1",delT="dt")     
#' outfit(modelvb,backtran = FALSE,title="vB constant sigma",parnames = label)     
#'     
#' sigfunk2 <- function(pars,predo) { # linear with predicted length     
#'   sig <- tail(pars,1) * predo      # sigma x predDL, see negnormL     
#'   pick <- which(sig <= 0)          # ensure no negative sigmas from      
#'   sig[pick] <- 0.01           # possible negative predicted lengths     
#'   return(sig)     
#' } # end of sigfunk2     
#' param <- c(170.0,0.3,1.0); label=c("Linf","K","sigma")     
#' modelvb2 <- nlm(f=negnormL,p=param,funk=fabens,funksig=sigfunk2,     
#'                 indat=bi,initL="l1",delT="dt",       
#'                 typsize=magnitude(param),iterlim=200)     
#' outfit(modelvb2,backtran = FALSE,parnames = label,title="vB inverse DeltaL, sigma < 1")     
#'  
#' # R-chunk 10  Page 150
#'  #plot to two Faben's lines with constant and varying sigma Fig 5.6     
#'  
#' predvB <- fabens(modelvb$estimate,bi)     
#' predvB2 <- fabens(modelvb2$estimate,bi)     
#' oldp <- parset(margin=c(0.4,0.4,0.05,0.05))     
#' plot(bi$l1,bi$dl,type="p",pch=1,cex=1.0,col=1,ylim=c(-2,31),     
#'      ylab="Growth Increment mm",xlab="Length mm",panel.first=grid())     
#' abline(h=0,col=1)     
#' lines(bi$l1,predvB,col=1,lwd=2)         # vB     
#' lines(bi$l1,predvB2,col=2,lwd=2,lty=2)  # IL     
#' legend("topright",c("Constant sigma","Changing sigma"),lwd=3,     
#'        col=c(1,2),bty="n",cex=1.1,lty=c(1,2))     
#' par(oldp)  # return par to old settings; this line not in book  
#'  
#' ## Objective Model Selection     
#' ### Akiake's Information Criterion     
#' # R-chunk 11  Page 152
#'  #compare the relative model fits of Vb and IL     
#'  
#' cat("von Bertalanffy \n")     
#' aicbic(modelvb,bi)     
#' cat("inverse-logistic \n")     
#' aicbic(modelil,bi)     
#'  
#' ### Likelihood Ratio Test     
#' # R-chunk 12  Page 154
#'  # Likelihood ratio comparison of two growth models see Fig 5.4     
#'  
#' vb <- modelvb$minimum # their respective -ve log-likelihoods     
#' il <- modelil$minimum     
#' dof <- 1     
#' round(likeratio(vb,il,dof),8)     
#'  
#' ### Caveats on Likelihood Ratio Tests     
#' ## Remarks on Growth     
#' ## Maturity     
#' ### Introduction     
#' ### Alternative Maturity Ogives     
#' # R-chunk 13  Page 158
#'  # The Maturity data from tasab data-set     
#'  
#' data(tasab)       # see ?tasab for a list of the codes used     
#' properties(tasab) # summarize properties of columns in tasab     
#' table(tasab$site,tasab$sex) # sites 1 & 2 vs F, I, and M     
#'  
#' # R-chunk 14  Page 158
#'  #plot the proportion mature vs shell length  Fig 5.7     
#'  
#' propm <- tapply(tasab$mature,tasab$length,mean) #mean maturity at L     
#' lens <- as.numeric(names(propm))            # lengths in the data     
#' oldp <- plot1(lens,propm,type="p",cex=0.9,xlab="Length mm",     
#'       ylab="Proportion Mature")     
#' par(oldp)  # return par to old settings; this line not in book  
#'      
#' # R-chunk 15  Pages 159 - 160
#'  #Use glm to estimate mature logistic     
#'  
#' binglm <- function(x,digits=6) { #function to simplify printing     
#'   out <- summary(x)     
#'   print(out$call)     
#'   print(round(out$coefficients,digits))     
#'   cat("\nNull Deviance  ",out$null.deviance,"df",out$df.null,"\n")     
#'   cat("Resid.Deviance ",out$deviance,"df",out$df.residual,"\n")     
#'   cat("AIC  = ",out$aic,"\n\n")     
#'   return(invisible(out)) # retain the full summary     
#' } #end of binglm     
#' tasab$site <- as.factor(tasab$site) # site as a factor     
#' smodel <- glm(mature ~ site + length,family=binomial,data=tasab)    
#' outs <- binglm(smodel)  #outs contains the whole summary object     
#'      
#' model <- glm(mature ~ length, family=binomial, data=tasab)     
#' outm <- binglm(model)     
#' cof <- outm$coefficients     
#' cat("Lm50 = ",-cof[1,1]/cof[2,1],"\n")     
#' cat("IQ   = ",2*log(3)/cof[2,1],"\n")     
#'  
#' # R-chunk 16  Page 161
#'  #Add maturity logistics to the maturity data plot Fig 5.8     
#'  
#' propm <- tapply(tasab$mature,tasab$length,mean) #prop mature     
#' lens <- as.numeric(names(propm))       # lengths in the data     
#' pick <- which((lens > 79) & (lens < 146))     
#' oldp <- parset()   
#' plot(lens[pick],propm[pick],type="p",cex=0.9, #the data points     
#'       xlab="Length mm",ylab="Proportion Mature",pch=1)      
#' L <- seq(80,145,1) # for increased curve separation     
#' pars <- coef(smodel)     
#' lines(L,mature(pars[1],pars[3],L),lwd=3,col=3,lty=2)       
#' lines(L,mature(pars[1]+pars[2],pars[3],L),lwd=3,col=2,lty=4)       
#' lines(L,mature(coef(model)[1],coef(model)[2],L),lwd=2,col=1,lty=1)       
#' abline(h=c(0.25,0.5,0.75),lty=3,col="grey")   
#' legend("topleft",c("site1","both","site2"),col=c(3,1,2),lty=c(2,1,4),     
#'        lwd=3,bty="n")     
#' par(oldp)  # return par to old settings; this line not in book  
#' 
#'  
#' ### The Assumption of Symmetry     
#' # R-chunk 17  Page 163
#'  #Asymmetrical maturity curve from Schnute and Richard's curve Fig5.9     
#'  
#' L = seq(50,160,1)     
#' p=c(a=0.07,b=0.2,c=1.0,alpha=100)     
#' asym <- srug(p=p,sizeage=L)     
#' L25 <- linter(bracket(0.25,asym,L))      
#' L50 <- linter(bracket(0.5,asym,L))      
#' L75 <- linter(bracket(0.75,asym,L))    
#' oldp <- parset()   
#' plot(L,asym,type="l",lwd=2,xlab="Length mm",ylab="Proportion Mature")     
#' abline(h=c(0.25,0.5,0.75),lty=3,col="grey")     
#' abline(v=c(L25,L50,L75),lwd=c(1,2,1),col=c(1,2,1))  
#' par(oldp)  # return par to old settings; this line not in book     
#'  
#' # R-chunk 18  Page 164 code not printed in the book     
#'  #Variation possible using the Schnute and Richard's Curve fig 5.10     
#'  # This code not printed in the book     
#'  
#' tmplot <- function(vals,label) {     
#'   text(170,0.6,paste0("  ",label),font=7,cex=1.5)     
#'   legend("bottomright",legend=vals,col=1:nvals,lwd=3,bty="n",   
#'          cex=1.25,lty=c(1:nvals))     
#' }     
#' L = seq(50,180,1)     
#' vals <- seq(0.05,0.09,0.01) # a value     
#' nvals <- length(vals)     
#' asym <-  srug(p=c(a=vals[1],b=0.2,c=1.0,alpha=100),sizeage=L)     
#' oldp <- parset(plots=c(2,2))      
#' plot(L,asym,type="l",lwd=2,xlab="Length mm",ylab="Proportion Mature",     
#'       ylim=c(0,1.05))     
#' abline(h=c(0.25,0.5,0.75),lty=3,col="darkgrey")     
#' for (i in 2:nvals) {     
#'   asym <- srug(p=c(a=vals[i],b=0.2,c=1.0,alpha=100),sizeage=L)     
#'   lines(L,asym,lwd=2,col=i,lty=i)     
#' }     
#' tmplot(vals,"a")     
#' vals <- seq(0.02,0.34,0.08) # b value     
#' nvals <- length(vals)     
#' asym <-  srug(p=c(a=0.07,b=vals[1],c=1.0,alpha=100),sizeage=L)     
#' plot(L,asym,type="l",lwd=2,xlab="Length mm",ylab="Proportion Mature",     
#'       ylim=c(0,1.05))     
#' abline(h=c(0.25,0.5,0.75),lty=3,col="darkgrey")     
#' for (i in 2:nvals) {     
#'   asym <- srug(p=c(a=0.07,b=vals[i],c=1.0,alpha=100),sizeage=L)     
#'   lines(L,asym,lwd=2,col=i,lty=i)     
#' }     
#' tmplot(vals,"b")     
#' vals <- seq(0.95,1.05,0.025) # c value     
#' nvals <- length(vals)     
#' asym <-  srug(p=c(a=0.07,b=0.2,c=vals[1],alpha=100),sizeage=L)     
#' plot(L,asym,type="l",lwd=2,xlab="Length mm",ylab="Proportion Mature",     
#'       ylim=c(0,1.05))     
#' abline(h=c(0.25,0.5,0.75),lty=3,col="darkgrey")     
#' for (i in 2:nvals) {     
#'   asym <- srug(p=c(a=0.07,b=0.2,c=vals[i],alpha=100),sizeage=L)     
#'   lines(L,asym,lwd=2,col=i,lty=i)     
#' }     
#' tmplot(vals,"c")      
#' vals <- seq(25,225,50) # alpha value     
#' nvals <- length(vals)     
#' asym <-  srug(p=c(a=0.07,b=0.2,c=1.0,alpha=vals[1]),sizeage=L)     
#' plot(L,asym,type="l",lwd=2,xlab="Length mm",ylab="Proportion Mature",     
#'       ylim=c(0,1.05))     
#' abline(h=c(0.25,0.5,0.75),lty=3,col="darkgrey")     
#' for (i in 2:nvals) {     
#'   asym <- srug(p=c(a=0.07,b=0.2,c=1.0,alpha=vals[i]),sizeage=L)     
#'   lines(L,asym,lwd=2,col=i,lty=i)     
#' }     
#' tmplot(vals,"alpha") 
#' par(oldp)  # return par to old settings; this line not in book      
#'  
#' ## Recruitment     
#' ### Introduction     
#' ### Properties of Good Stock Recruitment Relationships     
#' ### Recruitment Overfishing     
#' ### Beverton and Holt Recruitment     
#' # R-chunk 19  Page 169
#'  #plot the MQMF bh function for Beverton-Holt recruitment  Fig 5.11     
#'  
#' B <- 1:3000     
#' bhb <- c(1000,500,250,150,50)     
#' oldp <- parset()   
#' plot(B,bh(c(1000,bhb[1]),B),type="l",ylim=c(0,1050),   
#'       xlab="Spawning Biomass",ylab="Recruitment")     
#' for (i in 2:5) lines(B,bh(c(1000,bhb[i]),B),lwd=2,col=i,lty=i)     
#' legend("bottomright",legend=bhb,col=c(1:5),lwd=3,bty="n",lty=c(1:5))     
#' abline(h=c(500,1000),col=1,lty=2)   
#' par(oldp)  # return par to old settings; this line not in book    
#'  
#' ### Ricker Recruitment     
#' # R-chunk 20  Page 170
#'  #plot the MQMF ricker function for Ricker recruitment  Fig 5.12     
#'  
#' B <- 1:20000     
#' rickb <- c(0.0002,0.0003,0.0004)    
#' oldp <- parset()   
#' plot(B,ricker(c(10,rickb[1]),B),type="l",xlab="Spawning Biomass",ylab="Recruitment")     
#' for (i in 2:3)      
#'    lines(B,ricker(c(10,rickb[i]),B),lwd=2,col=i,lty=i)     
#' legend("topright",legend=rickb,col=1:3,lty=1:3,bty="n",lwd=2)     
#' par(oldp)  # return par to old settings; this line not in book   
#'  
#'  
#' ### Deriso's Generalized Model     
#'  
#' # R-chunk 21  Page 172
#'  # plot of three special cases from Deriso-Schnute curve  Fig. 5.13     
#' deriso <- function(p,B) return(p[1] * B *(1 - p[2]*p[3]*B)^(1/p[3]))     
#' B <- 1:10000     
#' oldp <- plot1(B,deriso(c(10,0.001,-1),B),lwd=2,xlab="Spawning Biomass",
#'               ylab="Recruitment") 
#' lines(B,deriso(c(10,0.0004,0.25),B),lwd=2,col=2,lty=2)  # DS     
#' lines(B,deriso(c(10,0.0004,1e-06),B),lwd=2,col=3,lty=3) # Ricker     
#' lines(B,deriso(c(10,0.0004,0.5),B),lwd=2,col=1,lty=3)   # odd line     
#' legend(x=7000,y=8500,legend=c("BH","DS","Ricker","odd line"),     
#'        col=c(1,2,3,1),lty=c(1,2,3,3),bty="n",lwd=3)     
#' par(oldp)  # return par to old settings; this line not in book  
#' 
#'  
#' ### Re-Parameterized Beverton-Holt Equation     
#' ### Re-Parameterized Ricker Equation     
#' ## Selectivity     
#' ### Introduction     
#' ### Logistic Selection     
#' # R-chunk 22  Page 177
#'  #Selectivity curves from logist and mature functions  See Fig 5.14   
#'  
#' ages <- seq(0,50,1);   in50 <- 25.0     
#' sel1 <- logist(in50,12,ages)         #-3.65/0.146=L50=25.0     
#' sel2 <- mature(-3.650425,0.146017,sizeage=ages)     
#' sel3 <- mature(-6,0.2,ages)     
#' sel4 <- logist(22.0,14,ages,knifeedge = TRUE)     
#' oldp <- plot1(ages,sel1,xlab="Age Years",ylab="Selectivity",cex=0.75,lwd=2)     
#' lines(ages,sel2,col=2,lwd=2,lty=2)     
#' lines(ages,sel3,col=3,lwd=2,lty=3)     
#' lines(ages,sel4,col=4,lwd=2,lty=4)     
#' abline(v=in50,col=1,lty=2); abline(h=0.5,col=1,lty=2)     
#' legend("topleft",c("25_eq5.30","25_eq5.31","30_eq5.31","22_eq5.30N"),     
#'        col=c(1,2,3,4),lwd=3,cex=1.1,bty="n",lty=c(1:4))     
#' par(oldp)  # return par to old settings; this line not in book  
#'  
#'  
#' ### Dome Shaped Selection     
#' # R-chunk 23  Page 179
#'  #Examples of domed-shaped selectivity curves from domed. Fig.5.15     
#'  
#' L <- seq(1,30,1)     
#' p <- c(10,11,16,33,-5,-2)     
#' oldp <- plot1(L,domed(p,L),type="l",lwd=2,ylab="Selectivity",xlab="Age Years")     
#' p1 <- c(8,12,16,33,-5,-1)     
#' lines(L,domed(p1,L),lwd=2,col=2,lty=2)     
#' p2 <- c(9,10,16,33,-5,-4)     
#' lines(L,domed(p2,L),lwd=2,col=4,lty=4)  
#' par(oldp)  # return par to old settings; this line not in book     
#' }
NULL

# chapter6 --------

#' @title chapter6 The 53 R-code chunks from On Uncertainty
#'
#' @description chapter6 is not an active function but rather acts 
#'     as a repository for the various example code chunks found in 
#'     chapter6. There are 53 r-code chunks in chapter6. You should,
#'     of course, feel free to use and modify any of these example 
#'     chunks in your own work.
#'     
#' @name chapter6
#'
#' @examples
#' \dontrun{
#' # All the example code from  # On Uncertainty     
#' # On Uncertainty     
#' ## Introduction     
#' ### Types of Uncertainty     
#' ### The Example Model   
#'   
#' # R-chunk 1  Page 189
#'  #Fit a surplus production model to abdat fisheries data     
#'  
#' data(abdat); logce <- log(abdat$cpue)       
#' param <- log(c(0.42,9400,3400,0.05))      
#' label=c("r","K","Binit","sigma") # simpspm returns      
#' bestmod <- nlm(f=negLL,p=param,funk=simpspm,indat=abdat,logobs=logce)     
#' outfit(bestmod,title="SP-Model",parnames=label) #backtransforms     
#'  
#' # R-chunk 2  Page 190
#'  #plot the abdat data and the optimum sp-model fit  Fig 6.1     
#'  
#' predce <- exp(simpspm(bestmod$estimate,abdat))      
#' optresid <- abdat[,"cpue"]/predce #multiply by predce for obsce     
#' ymax <- getmax(c(predce,abdat$cpue))     
#' oldp <- plot1(abdat$year,(predce*optresid),type="l",maxy=ymax,cex=0.9,     
#'       ylab="CPUE",xlab="Year",lwd=3,col="grey",lty=1)     
#' points(abdat$year,abdat$cpue,pch=1,col=1,cex=1.1)     
#' lines(abdat$year,predce,lwd=2,col=1)  # best fit line   
#' par(oldp)  # return par to old settings; this line not in book    
#'  
#' ## Bootstrapping     
#' ### Empirical Probability Density Distributions     
#' ## A Simple Bootstrap Example     
#' # R-chunk 3  Page 193
#'  #regression between catches of NPF prawn species Fig 6.2     
#'  
#' data(npf)     
#' model <- lm(endeavour ~ tiger,data=npf)     
#' oldp <- plot1(npf$tiger,npf$endeavour,type="p",xlab="Tiger Prawn (t)",     
#'       ylab="Endeavour Prawn (t)",cex=0.9)     
#' abline(model,col=1,lwd=2)     
#' correl <- sqrt(summary(model)$r.squared)     
#' pval <- summary(model)$coefficients[2,4]     
#' label <- paste0("Correlation ",round(correl,5)," P = ",round(pval,8))     
#' text(2700,180,label,cex=1.0,font=7,pos=4)     
#' par(oldp)  # return par to old settings; this line not in book  
#'   
#'  
#' # R-chunk 4  Page 194
#' # 5000 bootstrap estimates of correlation coefficient Fig 6.3     
#'  
#' set.seed(12321)     # better to use a less obvious seed, if at all     
#' N <- 5000                            # number of bootstrap samples     
#' result <- numeric(N)          #a vector to store 5000 correlations     
#' for (i in 1:N) {          #sample index from 1:23 with replacement     
#'    pick <- sample(1:23,23,replace=TRUE)   #sample is an R function     
#'    result[i] <- cor(npf$tiger[pick],npf$endeavour[pick])      
#' }     
#' rge <- range(result)                  # store the range of results     
#' CI <- quants(result)     # calculate quantiles; 90%CI = 5% and 95%     
#' restrim <- result[result > 0] #remove possible -ve values for plot     
#' oldp <- parset(cex=1.0)  #set up a plot window and draw a histogram     
#' bins <- seq(trunc(range(restrim)[1]*10)/10,1.0,0.01)      
#' outh <- hist(restrim,breaks=bins,main="",col=0,xlab="Correlation")     
#' abline(v=c(correl,mean(result)),col=c(4,3),lwd=c(3,2),lty=c(1,2))     
#' abline(v=CI[c(2,4)],col=4,lwd=2) # and 90% confidence intervals     
#' text(0.48,400,makelabel("Range ",rge,sep="  ",sigdig=4),font=7,pos=4)     
#' label <- makelabel("90%CI ",CI[c(2,4)],sep="  ",sigdig=4)     
#' text(0.48,300,label,cex=1.0,font=7,pos=4)   
#' par(oldp)  # return par to old settings; this line not in book    
#'  
#' ## Bootstrapping Time-Series Data     
#' # R-chunk 5  Page 196
#'  # fitting Schaefer model with log-normal residuals with 24 years      
#'  
#' data(abdat); logce <- log(abdat$cpue) # of abalone fisheries data     
#' param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05)) #log values     
#' bestmod <- nlm(f=negLL,p=param,funk=simpspm,indat=abdat,logobs=logce)     
#' optpar <- bestmod$estimate      # these are still log-transformed     
#' predce <- exp(simpspm(optpar,abdat))      #linear-scale pred cpue     
#' optres <- abdat[,"cpue"]/predce     # optimum log-normal residual     
#' optmsy <- exp(optpar[1])*exp(optpar[2])/4     
#' sampn <- length(optres)        # number of residuals and of years     
#'  
#' # R-chunk 6  Page 196 Table 6.1 code not included in the book      
#'  
#' outtab <- halftable(cbind(abdat,predce,optres),subdiv=2)     
#' kable(outtab, digits=c(0,0,3,3,3,0,0,3,3,3), caption='(ref:tab601)')     
#'  
#' # R-chunk 7  Pages 196 - 197
#'  # 1000 bootstrap Schaefer model fits; takes a few seconds     
#'  
#' start <- Sys.time() # use of as.matrix faster than using data.frame     
#' bootfish <- as.matrix(abdat)  # and avoid altering original data     
#' N <- 1000;   years <- abdat[,"year"] # need N x years matrices     
#' columns <- c("r","K","Binit","sigma")      
#' results <- matrix(0,nrow=N,ncol=sampn,dimnames=list(1:N,years))     
#' bootcpue <- matrix(0,nrow=N,ncol=sampn,dimnames=list(1:N,years))     
#' parboot <- matrix(0,nrow=N,ncol=4,dimnames=list(1:N,columns))     
#' for (i in 1:N) {  # fit the models and save solutions     
#'   bootcpue[i,] <- predce * sample(optres, sampn, replace=TRUE)     
#'   bootfish[,"cpue"] <- bootcpue[i,] #calc and save bootcpue     
#'   bootmod <- nlm(f=negLL,p=optpar,funk=simpspm,indat=bootfish,     
#'         logobs=log(bootfish[,"cpue"]))    
#'   parboot[i,] <- exp(bootmod$estimate)  #now save parameters    
#'   results[i,] <- exp(simpspm(bootmod$estimate,abdat))  #and predce      
#' }     
#' cat("total time = ",Sys.time()-start, "seconds   \n")     
#'  
#' # R-chunk 8  Page 197
#'  # bootstrap replicates in grey behind main plot Fig 6.4     
#'  
#' oldp <- plot1(abdat[,"year"],abdat[,"cpue"],type="n",xlab="Year",     
#'               ylab="CPUE") # type="n" just lays out an empty plot     
#' for (i in 1:N)      # ready to add the separate components     
#'   lines(abdat[,"year"],results[i,],lwd=1,col="grey")     
#' points(abdat[,"year"],abdat[,"cpue"],pch=16,cex=1.0,col=1)     
#' lines(abdat[,"year"],predce,lwd=2,col=1) 
#' par(oldp)  # return par to old settings; this line not in book      
#'  
#' # R-chunk 9  Pages 198 - 199
#'  #histograms of bootstrap parameters and model outputs Fig 6.5     
#'  
#' dohist <- function(invect,nmvar,bins=30,bootres,avpar) { #adhoc     
#'   hist(invect[,nmvar],breaks=bins,main="",xlab=nmvar,col=0)     
#'   abline(v=c(exp(avpar),bootres[pick,nmvar]),lwd=c(3,2,3,2),     
#'          col=c(3,4,4,4))     
#' }     
#' msy <- parboot[,"r"]*parboot[,"K"]/4 #calculate bootstrap MSY      
#' msyB <- quants(msy)        #from optimum bootstrap parameters     
#' oldp <- parset(plots=c(2,2),cex=0.9)     
#' bootres <- apply(parboot,2,quants); pick <- c(2,3,4) #quantiles     
#' dohist(parboot,nmvar="r",bootres=bootres,avpar=optpar[1])     
#' dohist(parboot,nmvar="K",bootres=bootres,avpar=optpar[2])     
#' dohist(parboot,nmvar="Binit",bootres=bootres,avpar=optpar[3])     
#' hist(msy,breaks=30,main="",xlab="MSY",col=0)     
#' abline(v=c(optmsy,msyB[pick]),lwd=c(3,2,3,2),col=c(3,4,4,4)) 
#' par(oldp)  # return par to old settings; this line not in book      
#'  
#' ### Parameter Correlation     
#' # R-chunk 10  Page 200
#'  #relationships between parameters and MSY  Fig 6.6     
#'  
#' parboot1 <- cbind(parboot,msy)     
#'  # note rgb use, alpha allows for shading, try 1/15 or 1/10     
#' pairs(parboot1,pch=16,col=rgb(red=1,green=0,blue=0,alpha = 1/20))     
#'  
#' ## Asymptotic Errors     
#' # R-chunk 11  Page 203
#'  #Fit Schaefer model and generate the Hessian     
#'  
#' data(abdat)     
#' param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05))      
#'  # Note inclusion of the option hessian=TRUE in nlm function     
#' bestmod <- nlm(f=negLL,p=param,funk=simpspm,indat=abdat,     
#'                logobs=log(abdat[,"cpue"]),hessian=TRUE)      
#' outfit(bestmod,backtran = TRUE) #try typing bestmod in console     
#'  # Now generate the confidence intervals     
#' vcov <- solve(bestmod$hessian)      # solve inverts matrices     
#' sterr <- sqrt(diag(vcov)) #diag extracts diagonal from a matrix     
#' optpar <- bestmod$estimate      #use qt for t-distrib quantiles     
#' U95 <- optpar + qt(0.975,20)*sterr # 4 parameters hence     
#' L95 <- optpar - qt(0.975,20)*sterr # (24 - 4) df     
#' cat("\n               r      K     Binit    sigma \n")      
#' cat("Upper 95% ",round(exp(U95),5),"\n") # backtransform     
#' cat("Optimum   ",round(exp(optpar),5),"\n")#\n =linefeed in cat     
#' cat("Lower 95% ",round(exp(L95),5),"\n")     
#'  
#' ### Uncertainty about the Model Outputs     
#' ### Sampling from a Multi-Variate Normal Distribution     
#' # R-chunk 12  Page 204
#'  # Use multi-variate normal to generate percentile CI    Fig 6.7     
#'  
#' library(mvtnorm) # use RStudio, or install.packages("mvtnorm")     
#' N <- 1000 # number of multi-variate normal parameter vectors     
#' years <- abdat[,"year"];  sampn <- length(years)  # 24 years     
#' mvncpue <- matrix(0,nrow=N,ncol=sampn,dimnames=list(1:N,years))     
#' columns <- c("r","K","Binit","sigma")     
#'  # Fill parameter vectors with N vectors from rmvnorm     
#' mvnpar <- matrix(exp(rmvnorm(N,mean=optpar,sigma=vcov)),     
#'                  nrow=N,ncol=4,dimnames=list(1:N,columns))     
#'  # Calculate N cpue trajectories using simpspm     
#' for (i in 1:N) mvncpue[i,] <- exp(simpspm(log(mvnpar[i,]),abdat))     
#' msy <- mvnpar[,"r"]*mvnpar[,"K"]/4 #N MSY estimates      
#'  # plot data and trajectories from the N parameter vectors     
#' oldp <- plot1(abdat[,"year"],abdat[,"cpue"],type="p",xlab="Year",     
#'               ylab="CPUE",cex=0.9)     
#' for (i in 1:N) lines(abdat[,"year"],mvncpue[i,],col="grey",lwd=1)     
#' points(abdat[,"year"],abdat[,"cpue"],pch=16,cex=1.0)#orig data     
#' lines(abdat[,"year"],exp(simpspm(optpar,abdat)),lwd=2,col=1)  
#' par(oldp)  # return par to old settings; this line not in book     
#'      
#' # R-chunk 13  Page 205
#'  #correlations between parameters when using mvtnorm Fig 6.8     
#'  
#' pairs(cbind(mvnpar,msy),pch=16,col=rgb(red=1,0,0,alpha = 1/10))    
#'  
#' # R-chunk 14  Pages 206 - 207
#'  #N parameter vectors from the multivariate normal Fig 6.9     
#'  
#' mvnres <- apply(mvnpar,2,quants)  # table of quantiles     
#' pick <- c(2,3,4)   # select rows for 5%, 50%, and 95%      
#' meanmsy <- mean(msy)     # optimum bootstrap parameters     
#' msymvn <- quants(msy)   # msy from mult-variate normal estimates     
#' plothist <- function(x,optp,label,resmvn) {     
#'   hist(x,breaks=30,main="",xlab=label,col=0)     
#'   abline(v=c(exp(optp),resmvn),lwd=c(3,2,3,2),col=c(3,4,4,4))      
#' } # repeated 4 times, so worthwhile writing a short function  
#' oldp <- par(no.readonly=TRUE)   
#' par(mfrow=c(2,2),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0))      
#' par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)     
#' plothist(mvnpar[,"r"],optpar[1],"r",mvnres[pick,"r"])     
#' plothist(mvnpar[,"K"],optpar[2],"K",mvnres[pick,"K"])     
#' plothist(mvnpar[,"Binit"],optpar[3],"Binit",mvnres[pick,"Binit"])     
#' plothist(msy,meanmsy,"MSY",msymvn[pick])    
#' par(oldp)  # return par to old settings; this line not in book   
#'  
#' # R-chunk 15   Page 208 Table 6.2 code not included in the book
#'  #Tabulate percentile CI from bootstrap (B) and multi-variate (mvn)     
#'  
#' kable(cbind(bootres,msyB),digits=c(4,3,3,4,3), caption='(ref:tab602)')     
#' kable(cbind(mvnres,msymvn),digits=c(4,3,3,4,3))     
#'  
#' ## Likelihood Profiles     
#' # R-chunk 16  Page 209
#'  #Fit the Schaefer surplus production model to abdat     
#'  
#' data(abdat); logce <- log(abdat$cpue)    # using negLL     
#' param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05))      
#' optmod <- nlm(f=negLL,p=param,funk=simpspm,indat=abdat,logobs=logce)     
#' outfit(optmod,parnames=c("r","K","Binit","sigma"))     
#'  
#' # R-chunk 17  Page 210
#'  #the code for MQMF's negLLP function     
#'  
#' negLLP <- function(pars, funk, indat, logobs, initpar=pars,     
#'                    notfixed=c(1:length(pars)),...) {     
#'   usepar <- initpar  #copy the original parameters into usepar     
#'   usepar[notfixed] <- pars[notfixed] #change 'notfixed' values     
#'   npar <- length(usepar)      
#'   logpred <- funk(usepar,indat,...) #funk uses the usepar values     
#'   pick <- which(is.na(logobs))  # proceed as in negLL     
#'   if (length(pick) > 0) {     
#'     LL <- -sum(dnorm(logobs[-pick],logpred[-pick],exp(pars[npar]),     
#'                      log=T))     
#'   } else {     
#'     LL <- -sum(dnorm(logobs,logpred,exp(pars[npar]),log=T))     
#'   }     
#'   return(LL)     
#' } # end of negLLP     
#'  
#' # R-chunk 18  Page 211
#'  #does negLLP give same answers as negLL when no parameters fixed?     
#'  
#' param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05))      
#' bestmod <- nlm(f=negLLP,p=param,funk=simpspm,indat=abdat,logobs=logce)     
#' outfit(bestmod,parnames=c("r","K","Binit","sigma"))     
#'  
#' # R-chunk 19  Page 211
#'  #Likelihood profile for r values 0.325 to 0.45     
#'  
#' rval <- seq(0.325,0.45,0.001)  # set up the test sequence     
#' ntrial <- length(rval)        # create storage for the results     
#' columns <- c("r","K","Binit","sigma","-veLL")     
#' result <- matrix(0,nrow=ntrial,ncol=length(columns),     
#'                  dimnames=list(rval,columns))# close to optimum     
#' bestest <- c(r= 0.32,K=11000,Binit=4000,sigma=0.05)      
#' for (i in 1:ntrial) {  #i <- 1     
#'   param <- log(c(rval[i],bestest[2:4])) #recycle bestest values     
#'   parinit <- param  #to improve the stability of nlm as r changes            
#'   bestmodP <- nlm(f=negLLP,p=param,funk=simpspm,initpar=parinit,      
#'                   indat=abdat,logobs=log(abdat$cpue),notfixed=c(2:4),     
#'                   typsize=magnitude(param),iterlim=1000)   
#'   bestest <- exp(bestmodP$estimate)          
#'   result[i,] <- c(bestest,bestmodP$minimum)  # store each result     
#' }     
#' minLL <- min(result[,"-veLL"]) #minimum across r values used.     
#'  
#' # R-chunk 20   Page 212 Table 6.3 code not included in the book
#'  #tabulate first 12 records from likelihood profile     
#'  
#' kable(head(result,12),digits=c(3,3,3,4,5), caption='(ref:tab603)')     
#'  
#' ### Likelihood Ratio Based Confidence Intervals     
#' # R-chunk 21  Page 213
#'  #likelihood profile on r from the Schaefer model Fig 6.10     
#'  
#' plotprofile(result,var="r",lwd=2)  # review the code      
#'  
#' # R-chunk 22  Page 214
#'  #Likelihood profile for K values 7200 to 12000     
#'  
#' Kval <- seq(7200,12000,10)     
#' ntrial <- length(Kval)     
#' columns <- c("r","K","Binit","sigma","-veLL")     
#' resultK <- matrix(0,nrow=ntrial,ncol=length(columns),     
#'                  dimnames=list(Kval,columns))     
#' bestest <- c(r= 0.45,K=7500,Binit=2800,sigma=0.05)      
#' for (i in 1:ntrial) {    
#'   param <- log(c(bestest[1],Kval[i],bestest[c(3,4)]))      
#'   parinit <- param     
#'   bestmodP <- nlm(f=negLLP,p=param,funk=simpspm,initpar=parinit,     
#'                 indat=abdat,logobs=log(abdat$cpue),     
#'                 notfixed=c(1,3,4),iterlim=1000)     
#'   bestest <- exp(bestmodP$estimate)     
#'   resultK[i,] <- c(bestest,bestmodP$minimum)     
#' }     
#' minLLK <- min(resultK[,"-veLL"])     
#'  #kable(head(result,12),digits=c(4,3,3,4,5))  # if wanted.     
#'  
#'  
#' # R-chunk 23  Page 214
#'  #likelihood profile on K from the Schaefer model Fig 6.11     
#'  
#' plotprofile(resultK,var="K",lwd=2)     
#'  
#' ### -ve Log-Likelihoods or Likelihoods     
#' # R-chunk 24  Page 215
#'  #translate -velog-likelihoods into likelihoods     
#'  
#' likes <- exp(-resultK[,"-veLL"])/sum(exp(-resultK[,"-veLL"]),na.rm=TRUE)     
#' resK <- cbind(resultK,likes,cumlike=cumsum(likes))     
#'  
#' # R-chunk 25   Page 216 Table 6.4 code not included in the book
#'  #tabulate head of likelihood profile matrix for K     
#'  
#' kable(head(resK,8),digits=c(4,0,3,4,5,9,7),caption='(ref:tab604)')     
#'  
#' # R-chunk 26  Page 216 Figure 6.12 code not in the book
#'  #K parameter likelihood profile  Fig 6.12     
#'  
#' oldp <- plot1(resK[,"K"],resK[,"likes"],xlab="K value",     
#'               ylab="Likelihood",lwd=2)     
#' lower <- which.closest(0.025,resK[,"cumlike"])     
#' mid <- which(resK[,"likes"] == max(resK[,"likes"]))     
#' upper <- which.closest(0.975,resK[,"cumlike"])     
#' abline(v=c(resK[c(lower,mid,upper),"K"]),col=1,lwd=c(1,2,1))     
#' label <- makelabel("",resK[c(lower,mid,upper),"K"],sep="  ")     
#' text(9500,0.005,label,cex=1.2,pos=4)    
#' par(oldp)  # return par to old settings; this line not in book   
#'  
#' ### Percentile Likelihood Profiles for Model Outputs     
#' # R-chunk 27  Page 217 - 218
#'  #examine effect on -veLL of MSY values from 740 - 1050t     
#'  #need a different negLLP() function, negLLO(): O for output.     
#'  #now optvar=888.831 (rK/4), the optimum MSY, varval ranges 740-1050      
#'  #and wght is the weighting to give to the penalty     
#'  
#' negLLO <- function(pars,funk,indat,logobs,wght,optvar,varval) {     
#'   logpred <- funk(pars,indat)     
#'   LL <- -sum(dnorm(logobs,logpred,exp(tail(pars,1)),log=T)) +     
#'              wght*((varval - optvar)/optvar)^2  #compare with negLL     
#'   return(LL)     
#' } # end of negLLO     
#' msyP <- seq(740,1020,2.5);      
#' optmsy <- exp(optmod$estimate[1])*exp(optmod$estimate[2])/4     
#' ntrial <- length(msyP)     
#' wait <- 400     
#' columns <- c("r","K","Binit","sigma","-veLL","MSY","pen","TrialMSY")     
#' resultO <- matrix(0,nrow=ntrial,ncol=length(columns),dimnames=list(msyP,columns))     
#' bestest <- c(r= 0.47,K=7300,Binit=2700,sigma=0.05)      
#' for (i in 1:ntrial) {  # i <- 1     
#'   param <- log(bestest)      
#'   bestmodO <- nlm(f=negLLO,p=param,funk=simpspm,indat=abdat,     
#'                   logobs=log(abdat$cpue),wght=wait,     
#'                   optvar=optmsy,varval=msyP[i],iterlim=1000)     
#'   bestest <- exp(bestmodO$estimate)     
#'   ans <- c(bestest,bestmodO$minimum,bestest[1]*bestest[2]/4,     
#'            wait *((msyP[i] - optmsy)/optmsy)^2,msyP[i])     
#'   resultO[i,] <- ans     
#' }     
#' minLLO <- min(resultO[,"-veLL"])     
#'  
#' # R-chunk 28   Page 218 Table 6.5 code not included in the book
#'  #tabulate first and last few records of profile on MSY     
#'  
#' kable(head(resultO[,1:7],4),digits=c(3,3,3,4,2,3,2),caption='(ref:tab605)')     
#' kable(tail(resultO[,1:7],4),digits=c(3,3,3,4,2,3,2))     
#'  
#' # R-chunk 29   Page 219 Figure 6.13 code not included in the book
#'  #likelihood profile on MSY from the Schaefer model Fig 6.13     
#'  
#' plotprofile(resultO,var="TrialMSY",lwd=2)     
#'  
#' ## Bayesian Posterior Distributions     
#' ### Generating the Markov Chain     
#' ### The Starting Point     
#' ### The Burn-in Period   
#' ### Convergence to the Stationary Distribution     
#' ### The Jumping Distribution     
#' ### Application of MCMC to the Example    
#'   
#' # R-chunk 30  Page 225
#'  #activate and plot the fisheries data in abdat  Fig 6.14     
#'  
#' data(abdat)   # type abdat in the console to see contents     
#' plotspmdat(abdat) #use helper function to plot fishery stats vs year   
#'  
#'  
#' ### Markov Chain Monte Carlo     
#' ### A First Example of an MCMC     
#' # R-chunk 31  Pages 228 - 229
#'  # Conduct MCMC analysis to illustrate burn-in. Fig 6.15     
#'  
#' data(abdat);  logce <- log(abdat$cpue)     
#' fish <- as.matrix(abdat) # faster to use a matrix than a data.frame!     
#' begin <- Sys.time()       # enable time taken to be calculated     
#' chains <- 1                # 1 chain per run; normally do more      
#' burnin <- 0                # no burn-in for first three chains     
#' N <- 100                        # Number of MCMC steps to keep     
#' step <- 4       # equals one step per parameter so no thinning     
#' priorcalc <- calcprior # define the prior probability function     
#' scales <- c(0.065,0.055,0.065,0.425) #found by trial and error     
#' set.seed(128900) #gives repeatable results in book; usually omitted     
#' inpar <- log(c(r= 0.4,K=11000,Binit=3600,sigma=0.05))     
#' result1 <- do_MCMC(chains,burnin,N,step,inpar,negLL,calcpred=simpspm,     
#'                    calcdat=fish,obsdat=logce,priorcalc,scales)     
#' inpar <- log(c(r= 0.35,K=8500,Binit=3400,sigma=0.05))     
#' result2 <- do_MCMC(chains,burnin,N,step,inpar,negLL,calcpred=simpspm,     
#'                    calcdat=fish,obsdat=logce,priorcalc,scales)     
#' inpar <- log(c(r= 0.45,K=9500,Binit=3200,sigma=0.05))     
#' result3 <- do_MCMC(chains,burnin,N,step,inpar,negLL,calcpred=simpspm,     
#'                    calcdat=fish,obsdat=logce,priorcalc,scales)     
#' burnin <- 50 # strictly a low thinning rate of 4; not enough   
#' step <- 16   # 16 thinstep rate = 4 parameters x 4 = 16     
#' N <- 10000   # 16 x 10000 = 160,000 steps + 50 burnin   
#' inpar <- log(c(r= 0.4,K=9400,Binit=3400,sigma=0.05))     
#' result4 <- do_MCMC(chains,burnin,N,step,inpar,negLL,calcpred=simpspm,     
#'                    calcdat=fish,obsdat=logce,priorcalc,scales)     
#' post1 <- result1[[1]][[1]]     
#' post2 <- result2[[1]][[1]]     
#' post3 <- result3[[1]][[1]]     
#' postY <- result4[[1]][[1]]     
#' cat("time   = ",Sys.time() - begin,"\n")     
#' cat("Accept = ",result4[[2]],"\n")     
#'  
#'  
#' # R-chunk 32  Pages 229 - 230
#' #first example and start of 3 initial chains for MCMC Fig6.15     
#'  
#' oldp <- parset(cex=0.85)        
#' P <- 75  # the first 75 steps only start to explore parameter space   
#' plot(postY[,"K"],postY[,"r"],type="p",cex=0.2,xlim=c(7000,13000),     
#'    ylim=c(0.28,0.47),col=8,xlab="K",ylab="r",panel.first=grid())     
#' lines(post2[1:P,"K"],post2[1:P,"r"],lwd=1,col=1)     
#' points(post2[1:P,"K"],post2[1:P,"r"],pch=15,cex=1.0)     
#' lines(post1[1:P,"K"],post1[1:P,"r"],lwd=1,col=1)     
#' points(post1[1:P,"K"],post1[1:P,"r"],pch=1,cex=1.2,col=1)     
#' lines(post3[1:P,"K"],post3[1:P,"r"],lwd=1,col=1)     
#' points(post3[1:P,"K"],post3[1:P,"r"],pch=2,cex=1.2,col=1)    
#' par(oldp)  # return par to old settings; this line not in book   
#'  
#' # R-chunk 33  Pages 230 - 231
#'  #pairs plot of parameters from the first MCMC Fig 6.16     
#'  
#' posterior <- result4[[1]][[1]]     
#' msy <-posterior[,1]*posterior[,2]/4        
#' pairs(cbind(posterior[,1:4],msy),pch=16,col=rgb(1,0,0,1/50),font=7)     
#'  
#' # R-chunk 34  Pages 231 - 232
#'  #plot the traces from the first MCMC example Fig 6.17     
#'  
#' posterior <- result4[[1]][[1]]  
#' oldp <- par(no.readonly=TRUE)   # this line not in book
#' par(mfrow=c(4,2),mai=c(0.4,0.4,0.05,0.05),oma=c(0.0,0,0.0,0.0))     
#' par(cex=0.8, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)     
#' label <- colnames(posterior)     
#' N <- dim(posterior)[1]     
#' for (i in 1:4) {     
#'   ymax <- getmax(posterior[,i]); ymin <- getmin(posterior[,i])     
#'   plot(1:N,posterior[,i],type="l",lwd=1,ylim=c(ymin,ymax),     
#'        panel.first=grid(),ylab=label[i],xlab="Step")     
#'   plot(density(posterior[,i]),lwd=2,col=2,panel.first=grid(),main="")     
#' }    
#' par(oldp)  # return par to old settings; this line not in book   
#'  
#' # R-chunk 35  Page 233
#'  #Use acf to examine auto-correlation with thinstep = 16   Fig 6.18     
#'  
#' posterior <- result4[[1]][[1]]     
#' label <- colnames(posterior)[1:4]     
#' oldp <- parset(plots=c(2,2),cex=0.85)     
#' for (i in 1:4) auto <- acf(posterior[,i],type="correlation",lwd=2,     
#'                            plot=TRUE,ylab=label[i],lag.max=20)     
#' par(oldp)  # return par to old settings; this line not in book  
#'  
#'  
#' # R-chunk 36  Pages 233 - 234
#'  #setup MCMC with thinstep of 128 per parameter  Fig 6.19     
#'  
#' begin=gettime()     
#' scales <- c(0.06,0.05,0.06,0.4)     
#' inpar <- log(c(r= 0.4,K=9400,Binit=3400,sigma=0.05))     
#' result <- do_MCMC(chains=1,burnin=100,N=1000,thinstep=512,inpar,     
#'                   negLL,calcpred=simpspm,calcdat=fish,     
#'                   obsdat=logce,calcprior,scales,schaefer=TRUE)     
#' posterior <- result[[1]][[1]]     
#' label <- colnames(posterior)[1:4]     
#' oldp <- parset(plots=c(2,2),cex=0.85)     
#' for (i in 1:4) auto <- acf(posterior[,i],type="correlation",lwd=2,     
#'                            plot=TRUE,ylab=label[i],lag.max=20)     
#' par(oldp)  # return par to old settings; this line not in book  
#' cat(gettime() - begin)     
#'  
#'  
#'  
#' ### Marginal Distributions      
#' # R-chunk 37  Pages 235 - 236
#'  # plot marginal distributions from the MCMC  Fig 6.20     
#'  
#' dohist <- function(x,xlab) { # to save a little space     
#'   return(hist(x,main="",breaks=50,col=0,xlab=xlab,ylab="",     
#'                panel.first=grid()))      
#' }     
#'  # ensure we have the optimum solution available     
#' param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05))      
#' bestmod <- nlm(f=negLL,p=param,funk=simpspm,indat=abdat,     
#'                logobs=log(abdat$cpue))     
#' optval <- exp(bestmod$estimate)     
#' posterior <- result[[1]][[1]] #example above N=1000, thin=512  
#' oldp <- par(no.readonly=TRUE)   
#' par(mfrow=c(5,1),mai=c(0.4,0.3,0.025,0.05),oma=c(0,1,0,0))      
#' par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)      
#' np <- length(param)     
#' for (i in 1:np) { #store invisible output from hist for later use     
#'   outH <- dohist(posterior[,i],xlab=colnames(posterior)[i])     
#'   abline(v=optval[i],lwd=3,col=4)     
#'   tmp <- density(posterior[,i])     
#'   scaler <- sum(outH$counts)*(outH$mids[2]-outH$mids[1])     
#'   tmp$y <- tmp$y * scaler     
#'   lines(tmp,lwd=2,col=2)     
#' }     
#' msy <- posterior[,"r"]*posterior[,"K"]/4     
#' mout <- dohist(msy,xlab="MSY")     
#' tmp <- density(msy)     
#' tmp$y <- tmp$y * (sum(mout$counts)*(mout$mids[2]-mout$mids[1]))     
#' lines(tmp,lwd=2,col=2)     
#' abline(v=(optval[1]*optval[2]/4),lwd=3,col=4)     
#' mtext("Frequency",side=2,outer=T,line=0.0,font=7,cex=1.0)     
#' par(oldp)  # return par to old settings; this line not in book  
#'   
#'  
#' ## The Use of Rcpp     
#' # R-chunk 38  Pages 236 - 237
#'  #profile the running of do_MCMC  using the now well known abdat      
#'  
#' data(abdat); logce <- log(abdat$cpue); fish <- as.matrix(abdat)       
#' param <- log(c(r=0.39,K=9200,Binit=3400,sigma=0.05))     
#' Rprof(append=TRUE)  # note the use of negLL1()     
#' result <- do_MCMC(chains=1,burnin=100,N=20000,thinstep=16,inpar=param,     
#'                  infunk=negLL1,calcpred=simpspm,calcdat=fish,     
#'                  obsdat=logce,priorcalc=calcprior,     
#'                  scales=c(0.07,0.06,0.07,0.45))     
#' Rprof(NULL)     
#' outprof <- summaryRprof()     
#'  
#'  
#' # R-chunk 39   Page 238 Table 6.6 code not included in the book
#'  #tabulate output of Rprof on do_MCMC function     
#'  
#' kable(head(outprof$by.self,12),caption='(ref:tab606)')     
#'  
#' ### Addressing Vectors and Matrices     
#' ### Replacement for simpspm()     
#' # R-chunk 40  Page 240
#'  
#' library(Rcpp)     
#'  #Send a text string containing the C++ code to cppFunction this will      
#'  #take a few seconds to compile, then the function simpspmC will      
#'  #continue to be available during the rest of your R session. The      
#'  #code in this chunk could be included into its own R file, and then     
#'  #the R source() function can be used to include the C++ into a      
#'  #session. indat must have catch in col2 (col1 in C++), and cpue in     
#'  #col3 (col2 in C++). Note the use of ; at the end of each line.      
#'  #Like simpspm(), this returns only the log(predicted cpue).     
#' cppFunction('NumericVector simpspmC(NumericVector pars,     
#'              NumericMatrix indat, LogicalVector schaefer) {     
#'     int nyrs = indat.nrow();     
#'     NumericVector predce(nyrs);     
#'     NumericVector biom(nyrs+1);     
#'     double Bt, qval;     
#'     double sumq = 0.0;     
#'     double p = 0.00000001;     
#'     if (schaefer(0) == TRUE) {     
#'       p = 1.0;     
#'     }     
#'     NumericVector ep = exp(pars);     
#'     biom[0] = ep[2];     
#'     for (int i = 0; i < nyrs; i++) {     
#'       Bt = biom[i];     
#'       biom[(i+1)]=Bt+(ep[0]/p)*Bt*(1-pow((Bt/ep[1]),p))-     
#'                       indat(i,1);     
#'       if (biom[(i+1)] < 40.0) biom[(i+1)] = 40.0;     
#'       sumq += log(indat(i,2)/biom[i]);     
#'     }     
#'     qval = exp(sumq/nyrs);     
#'     for (int i = 0; i < nyrs; i++) {     
#'       predce[i] = log(biom[i] * qval);     
#'     }     
#'     return predce;     
#' }')     
#'  
#'  
#' # R-chunk 41  Page 241
#'  #Ensure results obtained from simpspm and simpspmC are same     
#'  
#' library(microbenchmark)     
#' data(abdat)     
#' fishC <- as.matrix(abdat) # Use a matrix rather than a data.frame     
#' inpar <- log(c(r= 0.389,K=9200,Binit=3300,sigma=0.05))     
#' spmR <- exp(simpspm(inpar,fishC)) # demonstrate equivalence     
#'  #need to declare all arguments in simpspmC, no default values     
#' spmC <- exp(simpspmC(inpar,fishC,schaefer=TRUE))     
#' out <- microbenchmark( # verything identical calling function     
#'   simpspm(inpar,fishC,schaefer=TRUE),      
#'   simpspmC(inpar,fishC,schaefer=TRUE),     
#'   times=1000     
#' )     
#' out2 <- summary(out)[,2:8]     
#' out2 <- rbind(out2,out2[2,]/out2[1,])     
#' rownames(out2) <- c("simpspm","simpspmC","TimeRatio")     
#'  
#'  
#' # R-chunk 42   Page 241 Table 6.7 code not included in the book
#'  #compare results from simpspm and simpspmC     
#'  
#' kable(halftable(cbind(spmR,spmC)),row.names=TRUE,digits=c(4,4,4,4,4,4),caption='(ref:tab607)')     
#'  
#'  
#' # R-chunk 43   Page 242 Table 6.8 code not included in the book
#'  #output from microbenchmark comparison of simpspm and simpspmC     
#'  
#' kable(out2,row.names=TRUE,digits=c(3,3,3,3,3,3,3,0),caption='(ref:tab608)')     
#'  
#' # R-chunk 44  Pages 242 - 243
#'  #How much does using simpspmC in do_MCMC speed the run time?     
#'  #Assumes Rcpp code has run, eg source("Rcpp_functions.R")     
#'  
#' set.seed(167423) #Can use getseed() to generate a suitable seed     
#' beginR <- gettime()  #to enable estimate of time taken     
#' setscale <- c(0.07,0.06,0.07,0.45)     
#' reps <- 2000  #Not enough but sufficient for demonstration     
#' param <- log(c(r=0.39,K=9200,Binit=3400,sigma=0.05))     
#' resultR <- do_MCMC(chains=1,burnin=100,N=reps,thinstep=128,     
#'                   inpar=param,infunk=negLL1,calcpred=simpspm,     
#'                   calcdat=fishC,obsdat=log(abdat$cpue),schaefer=TRUE,     
#'                   priorcalc=calcprior,scales=setscale)     
#' timeR <- gettime() - beginR      
#' cat("time = ",timeR,"\n")     
#' cat("acceptance rate = ",resultR$arate," \n")     
#' postR <- resultR[[1]][[1]]     
#' set.seed(167423)     # Use the same pseudo-random numbers and the      
#' beginC <- gettime()  # same starting point to make the comparsion     
#' param <- log(c(r=0.39,K=9200,Binit=3400,sigma=0.05))     
#' resultC <- do_MCMC(chains=1,burnin=100,N=reps,thinstep=128,     
#'                  inpar=param,infunk=negLL1,calcpred=simpspmC,     
#'                  calcdat=fishC,obsdat=log(abdat$cpue),schaefer=TRUE,     
#'                  priorcalc=calcprior,scales=setscale)     
#' timeC <- gettime() - beginC     
#' cat("time = ",timeC,"\n")  # note the same acceptance rates     
#' cat("acceptance rate = ",resultC$arate," \n")     
#' postC <- resultC[[1]][[1]]     
#' cat("Time Ratio = ",timeC/timeR)     
#'  
#'  
#' # R-chunk 45  Page 243
#' #compare marginal distributions of the 2 chains  Fig 6.21     
#'
#' oldp <- par(no.readonly=TRUE)  # this line not in the book
#' par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0))      
#' par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)      
#' maxy <- getmax(c(density(postR[,"K"])$y,density(postC[,"K"])$y))     
#' plot(density(postR[,"K"]),lwd=2,col=1,xlab="K",ylab="Density",     
#'      main="",ylim=c(0,maxy),panel.first=grid())     
#' lines(density(postC[,"K"]),lwd=3,col=5,lty=2)     
#' par(oldp)  # return par to old settings; this line not in book  
#'   
#'  
#' ### Multiple Independent Chains     
#' # R-chunk 46  Page 244
#'  #run multiple = 3 chains     
#'  
#' setscale <- c(0.07,0.06,0.07,0.45)  # I only use a seed for      
#' set.seed(9393074) # reproducibility within this book     
#' reps <- 10000   # reset the timer     
#' beginC <- gettime()  # remember a thinstep=256 is insufficient     
#' resultC <- do_MCMC(chains=3,burnin=100,N=reps,thinstep=256,     
#'                    inpar=param,infunk=negLL1,calcpred=simpspmC,     
#'                    calcdat=fishC,obsdat=log(fishC[,"cpue"]),     
#'                    priorcalc=calcprior,scales=setscale,schaefer=TRUE)     
#' cat("time = ",gettime() - beginC," secs  \n")     
#'  
#'  
#' # R-chunk 47  Pages 244 - 245
#'  #3 chain run using simpspmC, 10000 reps, thinstep=256 Fig 6.22     
#' 
#' oldp <- par(no.readonly=TRUE) 
#' par(mfrow=c(2,2),mai=c(0.4,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0))      
#' par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)      
#' label <- c("r","K","Binit","sigma")     
#' for (i in 1:4) {     
#'    plot(density(resultC$result[[2]][,i]),lwd=2,col=1,     
#'         xlab=label[i],ylab="Density",main="",panel.first=grid())       
#'    lines(density(resultC$result[[1]][,i]),lwd=2,col=2)     
#'    lines(density(resultC$result[[3]][,i]),lwd=2,col=3)     
#' } 
#' par(oldp)  # return par to old settings; this line not in book      
#'  
#' # R-chunk 48  Pages 245 - 246
#'  #generate summary stats from the 3 MCMC chains     
#'  
#' av <- matrix(0,nrow=3,ncol=4,dimnames=list(1:3,label))     
#' sig2 <- av  # do the variance     
#' relsig <- av # relative to mean of all chains     
#' for (i in 1:3) {      
#'   tmp <- resultC$result[[i]]     
#'   av[i,] <- apply(tmp[,1:4],2,mean)     
#'   sig2[i,] <- apply(tmp[,1:4],2,var)     
#' }     
#' cat("Average \n")     
#' av     
#' cat("\nVariance per chain \n")     
#' sig2     
#' cat("\n")     
#' for (i in 1:4) relsig[,i] <- sig2[,i]/mean(sig2[,i])     
#' cat("Variance Relative to Mean Variance of Chains \n")     
#' relsig                                             
#'  
#'  
#' # R-chunk 49  Pages 246 - 247
#'  #compare quantile from the 2 most widely separate MCMC chains     
#'  
#' tmp <- resultC$result[[2]] # the 10000 values of each parameter     
#' cat("Chain 2 \n")     
#' msy1 <- tmp[,"r"]*tmp[,"K"]/4     
#' ch1 <- apply(cbind(tmp[,1:4],msy1),2,quants)     
#' round(ch1,4)     
#' tmp <- resultC$result[[3]]     
#' cat("Chain 3 \n")     
#' msy2 <- tmp[,"r"]*tmp[,"K"]/4     
#' ch2 <-  apply(cbind(tmp[,1:4],msy2),2,quants)     
#' round(ch2,4)     
#' cat("Percent difference ")     
#' cat("\n2.5%  ",round(100*(ch1[1,] - ch2[1,])/ch1[1,],4),"\n")     
#' cat("50%   ",round(100*(ch1[3,] - ch2[3,])/ch1[3,],4),"\n")     
#' cat("97.5% ",round(100*(ch1[5,] - ch2[5,])/ch1[5,],4),"\n")     
#'  
#'  
#' ### Replicates Required to Avoid Serial Correlation     
#' # R-chunk 50  Page 248
#'  #compare two higher thinning rates per parameter in MCMC     
#'  
#' param <- log(c(r=0.39,K=9200,Binit=3400,sigma=0.05))     
#' setscale <- c(0.07,0.06,0.07,0.45)     
#' result1 <- do_MCMC(chains=1,burnin=100,N=2000,thinstep=1024,     
#'                    inpar=param,infunk=negLL1,calcpred=simpspmC,     
#'                    calcdat=fishC,obsdat=log(abdat$cpue),     
#'                    priorcalc=calcprior,scales=setscale,schaefer=TRUE)     
#' result2 <- do_MCMC(chains=1,burnin=50,N=1000,thinstep=2048,     
#'                    inpar=param,infunk=negLL1,calcpred=simpspmC,     
#'                    calcdat=fishC,obsdat=log(abdat$cpue),     
#'                    priorcalc=calcprior,scales=setscale,schaefer=TRUE)     
#'  
#'  
#' # R-chunk 51  Page 248
#'  #autocorrelation of 2 different thinning rate chains Fig6.23     
#'  
#' posterior1 <- result1$result[[1]]     
#' posterior2 <- result2$result[[1]]     
#' label <- colnames(posterior1)[1:4]  
#' oldp <- par(no.readonly=TRUE)   
#' par(mfrow=c(4,2),mai=c(0.25,0.45,0.05,0.05),oma=c(1.0,0,1.0,0.0))      
#' par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)       
#' for (i in 1:4) {     
#'   auto <- acf(posterior1[,i],type="correlation",plot=TRUE,     
#'               ylab=label[i],lag.max=20,xlab="",ylim=c(0,0.3),lwd=2)     
#'   if (i == 1) mtext(1024,side=3,line=-0.1,outer=FALSE,cex=1.2)     
#'   auto <- acf(posterior2[,i],type="correlation",plot=TRUE,     
#'               ylab=label[i],lag.max=20,xlab="",ylim=c(0,0.3),lwd=2)     
#'   if (i == 1) mtext(2048,side=3,line=-0.1,outer=FALSE,cex=1.2)     
#' }     
#' mtext("Lag",side=1,line=-0.1,outer=TRUE,cex=1.2)     
#' par(oldp)  # return par to old settings; this line not in book    
#'  
#' # R-chunk 52  Page 249
#' #visual comparison of 2 chains marginal densities  Fig 6.24     
#'  
#' oldp <- parset(plots=c(2,2),cex=0.85)      
#' label <- c("r","K","Binit","sigma")     
#' for (i in 1:4) {     
#'    plot(density(result1$result[[1]][,i]),lwd=4,col=1,xlab=label[i],     
#'         ylab="Density",main="",panel.first=grid())       
#'    lines(density(result2$result[[1]][,i]),lwd=2,col=5,lty=2)     
#' }    
#' par(oldp)  # return par to old settings; this line not in book   
#'  
#' # R-chunk 53  Pages 250 - 251
#'  #tablulate a summary of the two different thinning rates.     
#'  
#' cat("1024 thinning rate \n")     
#' posterior <- result1$result[[1]]     
#' msy <-posterior[,1]*posterior[,2]/4      
#' tmp1 <- apply(cbind(posterior[,1:4],msy),2,quants)     
#' rge <- apply(cbind(posterior[,1:4],msy),2,range)     
#' tmp1 <- rbind(tmp1,rge[2,] - rge[1,])     
#' rownames(tmp1)[6] <- "Range"     
#' print(round(tmp1,4))     
#' posterior2 <- result2$result[[1]]     
#' msy2 <-posterior2[,1]*posterior2[,2]/4       
#' cat("2048 thinning rate \n")     
#' tmp2 <- apply(cbind(posterior2[,1:4],msy2),2,quants)     
#' rge2 <- apply(cbind(posterior2[,1:4],msy2),2,range)     
#' tmp2 <- rbind(tmp2,rge2[2,] - rge2[1,])     
#' rownames(tmp2)[6] <- "Range"     
#' print(round(tmp2,4))     
#' cat("Inner 95% ranges and Differences between total ranges \n")      
#' cat("95% 1 ",round((tmp1[5,] - tmp1[1,]),4),"\n")     
#' cat("95% 2 ",round((tmp2[5,] - tmp2[1,]),4),"\n")     
#' cat("Diff  ",round((tmp2[6,] - tmp1[6,]),4),"\n")      
#' }
NULL

# chapter7 --------

#' @title chapter7 The 67 R-code chunks from Surplus Production Models
#'
#' @description chapter7 is not an active function but rather acts 
#'     as a repository for the various example code chunks found in 
#'     chapter7. There are 67 r-code chunks in chapter7 You should,
#'     of course, feel free to use and modify any of these example 
#'     chunks in your own work.
#'     
#' @name chapter7
#'
#' @examples
#' \dontrun{
#' # All the example code from  # Surplus Production Models     
#' # Surplus Production Models     
#' ## Introduction     
#' ### Data Needs     
#' ### The Need for Contrast     
#' ### When are Catch-Rates Informative   
#'   
#' # R-chunk 1  Page 256
#'  #Yellowfin-tuna data from Schaefer 12957     
#'  
#' # R-chunk 2  Page 256 Table 7.1 code not in the book  
#' data(schaef)     
#' kable(halftable(schaef,subdiv=2),digits=c(0,0,0,4))     
#'  
#' # R-chunk 3  Page 256
#'  #schaef fishery data and regress cpue and catch    Fig 7.1     
#'  
#' oldp <- parset(plots=c(3,1),margin=c(0.35,0.4,0.05,0.05))     
#' plot1(schaef[,"year"],schaef[,"catch"],ylab="Catch",xlab="Year",     
#'       defpar=FALSE,lwd=2)     
#' plot1(schaef[,"year"],schaef[,"cpue"],ylab="CPUE",xlab="Year",     
#'       defpar=FALSE,lwd=2)     
#' plot1(schaef[,"catch"],schaef[,"cpue"],type="p",ylab="CPUE",     
#'       xlab="Catch",defpar=FALSE,pch=16,cex=1.0)     
#' model <- lm(schaef[,"cpue"] ~ schaef[,"catch"])     
#' abline(model,lwd=2,col=2)   # summary(model) 
#' par(oldp)  # return par to old settings; this line not in book      
#'  
#' # R-chunk 4  Page 257
#'  #cross correlation between cpue and catch in schaef Fig 7.2     
#'  
#' oldp <- parset(cex=0.85) #sets par values for a tidy base graphic     
#' ccf(x=schaef[,"catch"],y=schaef[,"cpue"],type="correlation",     
#'     ylab="Correlation",plot=TRUE) 
#' par(oldp)  # return par to old settings; this line not in book      
#'  
#' # R-chunk 5  Page 257
#'  #now plot schaef data with timelag of 2 years on cpue   Fig 7.3     
#'  
#' oldp <- parset(plots=c(3,1),margin=c(0.35,0.4,0.05,0.05))     
#' plot1(schaef[1:20,"year"],schaef[1:20,"catch"],ylab="Catch",     
#'       xlab="Year",defpar=FALSE,lwd=2)     
#' plot1(schaef[3:22,"year"],schaef[3:22,"cpue"],ylab="CPUE",     
#'       xlab="Year",defpar=FALSE,lwd=2)     
#' plot1(schaef[1:20,"catch"],schaef[3:22,"cpue"],type="p",     
#'       ylab="CPUE",xlab="Catch",defpar=FALSE,cex=1.0,pch=16)     
#' model2 <- lm(schaef[3:22,"cpue"] ~ schaef[1:20,"catch"])     
#' abline(model2,lwd=2,col=2) 
#' par(oldp)  # return par to old settings; this line not in book      
#'  
#' # R-chunk 6  Page 259
#'  #write out a summary of he regression model2     
#'  
#' summary(model2)     
#'  
#' ## Some Equations     
#' ### Production Functions     
#' # R-chunk 7  Page 262
#'  #plot productivity and density-dependence functions Fig7.4     
#'  
#' prodfun <- function(r,Bt,K,p) return((r*Bt/p)*(1-(Bt/K)^p))     
#' densdep <- function(Bt,K,p) return((1/p)*(1-(Bt/K)^p))      
#' r <- 0.75; K <- 1000.0; Bt <- 1:1000     
#' sp <- prodfun(r,Bt,K,1.0)  # Schaefer equivalent     
#' sp0 <- prodfun(r,Bt,K,p=1e-08)  # Fox equivalent     
#' sp3 <- prodfun(r,Bt,K,3) #left skewed production, marine mammal?     
#' oldp <- parset(plots=c(2,1),margin=c(0.35,0.4,0.1,0.05))     
#' plot1(Bt,sp,type="l",lwd=2,xlab="Stock Size",     
#'       ylab="Surplus Production",maxy=200,defpar=FALSE)     
#' lines(Bt,sp0 * (max(sp)/max(sp0)),lwd=2,col=2,lty=2) # rescale      
#' lines(Bt,sp3*(max(sp)/max(sp3)),lwd=3,col=3,lty=3)   # production     
#' legend(275,100,cex=1.1,lty=1:3,c("p = 1.0 Schaefer","p = 1e-08 Fox",     
#'                  "p = 3 LeftSkewed"),col=c(1,2,3),lwd=3,bty="n")     
#' plot1(Bt,densdep(Bt,K,p=1),xlab="Stock Size",defpar=FALSE,     
#'       ylab="Density-Dependence",maxy=2.5,lwd=2)     
#' lines(Bt,densdep(Bt,K,1e-08),lwd=2,col=2,lty=2)     
#' lines(Bt,densdep(Bt,K,3),lwd=3,col=3,lty=3)
#' par(oldp)  # return par to old settings; this line not in book       
#'  
#' ### The Schaefer Model     
#' ### Sum of Squared Residuals     
#' ### Estimating Management Statistics     
#' 
#' # R-chunk 8  Page 266
#'  #compare Schaefer and Fox MSY estimates for same parameters     
#'  
#' param <- c(r=1.1,K=1000.0,Binit=800.0,sigma=0.075)     
#' cat("MSY Schaefer = ",getMSY(param,p=1.0),"\n") # p=1 is default     
#' cat("MSY Fox      = ",getMSY(param,p=1e-08),"\n")     
#'  
#' ### The Trouble with Equilibria     
#' ## Model Fitting     
#' ### A Possible Workflow for Stock Assessment     
#' # R-chunk 9  Page 269
#'  #Initial model 'fit' to the initial parameter guess  Fig 7.5     
#'  
#' data(schaef); schaef <- as.matrix(schaef)     
#' param <- log(c(r=0.1,K=2250000,Binit=2250000,sigma=0.5))     
#' negatL <- negLL(param,simpspm,schaef,logobs=log(schaef[,"cpue"]))     
#' ans <- plotspmmod(inp=param,indat=schaef,schaefer=TRUE,     
#'                  addrmse=TRUE,plotprod=FALSE)     
#'  
#' # R-chunk 10  Pages 270 - 271
#'  #Fit the model first using optim then nlm in sequence     
#'  
#' param <- log(c(0.1,2250000,2250000,0.5))      
#' pnams <- c("r","K","Binit","sigma")     
#' best <- optim(par=param,fn=negLL,funk=simpspm,indat=schaef,     
#'              logobs=log(schaef[,"cpue"]),method="BFGS")     
#' outfit(best,digits=4,title="Optim",parnames = pnams)     
#' cat("\n")     
#' best2 <- nlm(negLL,best$par,funk=simpspm,indat=schaef,     
#'            logobs=log(schaef[,"cpue"]))     
#' outfit(best2,digits=4,title="nlm",parnames = pnams)     
#'  
#' # R-chunk 11  Page 271
#'  #optimum fit. Defaults used in plotprod and schaefer Fig 7.6     
#'  
#' ans <- plotspmmod(inp=best2$estimate,indat=schaef,addrmse=TRUE,     
#'                   plotprod=TRUE)     
#'  
#' # R-chunk 12  Page 272
#'  #the high-level structure of ans; try str(ans$Dynamics)     
#'  
#' str(ans, width=65, strict.width="cut",max.level=1)     
#'  
#' # R-chunk 13  Page 273
#'  #compare the parameteric MSY with the numerical MSY     
#'  
#' round(ans$Dynamics$sumout,3)     
#' cat("\n Productivity Statistics \n")     
#' summspm(ans) # the q parameter needs more significantr digits    
#'  
#' ### Is the Analysis Robust?     
#' # R-chunk 14  Page 274
#'  #conduct a robustness test on the Schaefer model fit     
#'  
#' data(schaef); schaef <- as.matrix(schaef); reps <- 12     
#' param <- log(c(r=0.15,K=2250000,Binit=2250000,sigma=0.5))     
#' ansS <- fitSPM(pars=param,fish=schaef,schaefer=TRUE,    #use     
#'                maxiter=1000,funk=simpspm,funkone=FALSE) #fitSPM     
#'  #getseed() #generates random seed for repeatable results     
#' set.seed(777852) #sets random number generator with a known seed     
#' robout <- robustSPM(inpar=ansS$estimate,fish=schaef,N=reps,     
#'                     scaler=40,verbose=FALSE,schaefer=TRUE,     
#'                     funk=simpspm,funkone=FALSE)      
#'  #use str(robout) to see the components included in the output     
#'  
#'  
#' # R-chunk 15  Page 275 Table 7.2 code not in the book 
#'  #outcome of robustness tests     
#'  
#' kable(robout$results[,1:5],digits=c(3,4,3,4,3))     
#' kable(robout$results[,6:11],digits=c(3,4,3,4,5,0))     
#'  
#' # R-chunk 16 Pages 275 - 276 
#'  #Repeat robustness test on fit to schaef data 100 times     
#'  
#' set.seed(777854)     
#' robout2 <- robustSPM(inpar=ansS$estimate,fish=schaef,N=100,     
#'                      scaler=25,verbose=FALSE,schaefer=TRUE,     
#'                      funk=simpspm,funkone=TRUE,steptol=1e-06)      
#' lastbits <- tail(robout2$results[,6:11],10)     
#'  
#' # R-chunk 17  Page 276 Table 7.3 code not in the book 
#'  #last 10 rows of robustness test showing deviations     
#'  
#' kable(lastbits,digits=c(5,1,1,4,5,0))     
#'  
#' # R-chunk 18  Page 276
#'  # replicates from the robustness test        Fig 7.7     
#'  
#' result <- robout2$results     
#' oldp <- parset(plots=c(2,2),margin=c(0.35,0.45,0.05,0.05))     
#' hist(result[,"r"],breaks=15,col=2,main="",xlab="r")     
#' hist(result[,"K"],breaks=15,col=2,main="",xlab="K")     
#' hist(result[,"Binit"],breaks=15,col=2,main="",xlab="Binit")     
#' hist(result[,"MSY"],breaks=15,col=2,main="",xlab="MSY")   
#' par(oldp)  # return par to old settings; this line not in book    
#'  
#' # R-chunk 19  Page 277
#'  #robustSPM parameters against each other  Fig 7.8     
#'  
#' pairs(result[,c("r","K","Binit","MSY")],upper.panel=NULL,pch=1)     
#'  
#' ### Using Different Data?     
#' # R-chunk 20  Page 278
#'  #Now use the dataspm data-set, which is noisier     
#'  
#' set.seed(777854) #other random seeds give different results     
#' data(dataspm);   fish <- dataspm #to generalize the code     
#' param <- log(c(r=0.24,K=5174,Binit=2846,sigma=0.164))     
#' ans <- fitSPM(pars=param,fish=fish,schaefer=TRUE,maxiter=1000,     
#'              funkone=TRUE)      
#' out <- robustSPM(ans$estimate,fish,N=100,scaler=15, #making     
#'                 verbose=FALSE,funkone=TRUE) #scaler=10 gives     
#' result <- tail(out$results[,6:11],10) #16 sub-optimal results     
#'  
#'  
#' # R-chunk 21  Page 279 Table 7.4 code not in the book 
#'  #last 10 trials of robustness on dataspm fit     
#'  
#' kable(result,digits=c(4,2,2,4,4,3))     
#'  
#' ## Uncertainty     
#' ### Likelihood Profiles     
#' # R-chunk 22  Page 280
#'  # Fig 7.9 Fit of optimum to the abdat data-set     
#'  
#' data(abdat);     fish <- as.matrix(abdat)     
#' colnames(fish) <- tolower(colnames(fish))  # just in case     
#' pars <- log(c(r=0.4,K=9400,Binit=3400,sigma=0.05))     
#' ans <- fitSPM(pars,fish,schaefer=TRUE) #Schaefer     
#' answer <- plotspmmod(ans$estimate,abdat,schaefer=TRUE,addrmse=TRUE)     
#'  
#' # R-chunk 23  Pages 280 - 282
#'  # likelihood profiles for r and K for fit to abdat  Fig 7.10     
#'  #doprofile input terms are vector of values, fixed parameter      
#'  #location, starting parameters, and free parameter locations.     
#'  #all other input are assumed to be in the calling environment     
#'  
#' doprofile <- function(val,loc,startest,indat,notfix=c(2:4)) {      
#'   pname <- c("r","K","Binit","sigma","-veLL")     
#'   numv <- length(val)     
#'   outpar <- matrix(NA,nrow=numv,ncol=5,dimnames=list(val,pname))     
#'   for (i in 1:numv) {  #      
#'     param <- log(startest) # reset the parameters     
#'     param[loc] <- log(val[i]) #insert new fixed value     
#'     parinit <- param   # copy revised parameter vector     
#'     bestmod <- nlm(f=negLLP,p=param,funk=simpspm,initpar=parinit,     
#'                    indat=indat,logobs=log(indat[,"cpue"]),notfixed=notfix)     
#'     outpar[i,] <- c(exp(bestmod$estimate),bestmod$minimum)     
#'   }     
#'   return(outpar)     
#' }     
#' rval <- seq(0.32,0.46,0.001)     
#' outr <- doprofile(rval,loc=1,startest=c(rval[1],11500,5000,0.25),     
#'                   indat=fish,notfix=c(2:4))     
#' Kval <- seq(7200,11500,200)     
#' outk <- doprofile(Kval,loc=2,c(0.4,7200,6500,0.3),indat=fish,notfix=c(1,3,4))     
#' oldp <- parset(plots=c(2,1),cex=0.85,outmargin=c(0.5,0.5,0,0))     
#' plotprofile(outr,var="r",defpar=FALSE,lwd=2) #MQMF function     
#' plotprofile(outk,var="K",defpar=FALSE,lwd=2) 
#' par(oldp)  # return par to old settings; this line not in book      
#'  
#' ### Bootstrap Confidence Intervals     
#' # R-chunk 24  Page 283
#'  #find optimum Schaefer model fit to dataspm data-set Fig 7.11     
#'  
#' data(dataspm)     
#' fish <- as.matrix(dataspm)     
#' colnames(fish) <- tolower(colnames(fish))     
#' pars <- log(c(r=0.25,K=5500,Binit=3000,sigma=0.25))     
#' ans <- fitSPM(pars,fish,schaefer=TRUE,maxiter=1000) #Schaefer     
#' answer <- plotspmmod(ans$estimate,fish,schaefer=TRUE,addrmse=TRUE)     
#'  
#' # R-chunk 25  Page 284
#'  #bootstrap the log-normal residuals from optimum model fit     
#'  
#' set.seed(210368)     
#' reps <- 1000 # can take 10 sec on a large Desktop. Be patient     
#'  #startime <- Sys.time()  # schaefer=TRUE is the default     
#' boots <- spmboot(ans$estimate,fishery=fish,iter=reps)     
#'  #print(Sys.time() - startime) # how long did it take?     
#' str(boots,max.level=1)     
#'  
#' # R-chunk 26  Page 285
#'  #Summarize bootstrapped parameter estimates as quantiles  Table 7.6    
#'  
#' bootpar <- boots$bootpar     
#' rows <- colnames(bootpar)     
#' columns <- c(c(0.025,0.05,0.5,0.95,0.975),"Mean")     
#' bootCI <- matrix(NA,nrow=length(rows),ncol=length(columns),     
#'                  dimnames=list(rows,columns))     
#' for (i in 1:length(rows)) {     
#'    tmp <- bootpar[,i]     
#'    qtil <- quantile(tmp,probs=c(0.025,0.05,0.5,0.95,0.975),na.rm=TRUE)     
#'    bootCI[i,] <- c(qtil,mean(tmp,na.rm=TRUE))     
#' }     
#' kable(bootCI,digits=c(4,4,4,4,4,4))     
#'  
#' # R-chunk 28  Page 286
#'  #boostrap CI. Note use of uphist to expand scale  Fig 7.12     
#'  
#' colf <- c(1,1,1,4); lwdf <- c(1,3,1,3); ltyf <- c(1,1,1,2)     
#' colsf <- c(2,3,4,6)  
#' oldp <- parset(plots=c(3,2))     
#' hist(bootpar[,"r"],breaks=25,main="",xlab="r")     
#' abline(v=c(bootCI["r",colsf]),col=colf,lwd=lwdf,lty=ltyf)     
#' uphist(bootpar[,"K"],maxval=14000,breaks=25,main="",xlab="K")     
#' abline(v=c(bootCI["K",colsf]),col=colf,lwd=lwdf,lty=ltyf)     
#' hist(bootpar[,"Binit"],breaks=25,main="",xlab="Binit")     
#' abline(v=c(bootCI["Binit",colsf]),col=colf,lwd=lwdf,lty=ltyf)     
#' uphist(bootpar[,"MSY"],breaks=25,main="",xlab="MSY",maxval=450)     
#' abline(v=c(bootCI["MSY",colsf]),col=colf,lwd=lwdf,lty=ltyf)     
#' hist(bootpar[,"Depl"],breaks=25,main="",xlab="Final Depletion")     
#' abline(v=c(bootCI["Depl",colsf]),col=colf,lwd=lwdf,lty=ltyf)     
#' hist(bootpar[,"Harv"],breaks=25,main="",xlab="End Harvest Rate")     
#' abline(v=c(bootCI["Harv",colsf]),col=colf,lwd=lwdf,lty=ltyf)   
#' par(oldp)  # return par to old settings; this line not in book    
#'  
#' # R-chunk 29  Page 286
#'  #Fig7.13 1000 bootstrap trajectories for dataspm model fit      
#'  
#' dynam <- boots$dynam     
#' years <- fish[,"year"]     
#' nyrs <- length(years)     
#' oldp <- parset()     
#' ymax <- getmax(c(dynam[,,"predCE"],fish[,"cpue"]))     
#' plot(fish[,"year"],fish[,"cpue"],type="n",ylim=c(0,ymax),     
#'      xlab="Year",ylab="CPUE",yaxs="i",panel.first = grid())     
#' for (i in 1:reps) lines(years,dynam[i,,"predCE"],lwd=1,col=8)     
#' lines(years,answer$Dynamics$outmat[1:nyrs,"predCE"],lwd=2,col=0)     
#' points(years,fish[,"cpue"],cex=1.2,pch=16,col=1)     
#' percs <- apply(dynam[,,"predCE"],2,quants)     
#' arrows(x0=years,y0=percs["5%",],y1=percs["95%",],length=0.03,     
#'        angle=90,code=3,col=0)     
#' par(oldp)  # return par to old settings; this line not in book  
#' 
#'  
#' # R-chunk 30  Page 288
#'  #Fit the Fox model to dataspm; note different parameters     
#'  
#' pars <- log(c(r=0.15,K=6500,Binit=3000,sigma=0.20))     
#' ansF <- fitSPM(pars,fish,schaefer=FALSE,maxiter=1000) #Fox version     
#' bootsF <- spmboot(ansF$estimate,fishery=fish,iter=reps,schaefer=FALSE)     
#' dynamF <- bootsF$dynam     
#'  
#' # R-chunk 31 Pages 288 - 289 
#'  # bootstrap trajectories from both model fits  Fig 7.14     
#'  
#' oldp <- parset()     
#' ymax <- getmax(c(dynam[,,"predCE"],fish[,"cpue"]))     
#' plot(fish[,"year"],fish[,"cpue"],type="n",ylim=c(0,ymax),     
#'      xlab="Year",ylab="CPUE",yaxs="i",panel.first = grid())     
#' for (i in 1:reps) lines(years,dynamF[i,,"predCE"],lwd=1,col=1,lty=1)     
#' for (i in 1:reps) lines(years,dynam[i,,"predCE"],lwd=1,col=8)     
#' lines(years,answer$Dynamics$outmat[1:nyrs,"predCE"],lwd=2,col=0)     
#' points(years,fish[,"cpue"],cex=1.1,pch=16,col=1)     
#' percs <- apply(dynam[,,"predCE"],2,quants)     
#' arrows(x0=years,y0=percs["5%",],y1=percs["95%",],length=0.03,     
#'        angle=90,code=3,col=0)     
#' legend(1985,0.35,c("Schaefer","Fox"),col=c(8,1),bty="n",lwd=3)     
#' par(oldp)  # return par to old settings; this line not in book  
#' 
#'  
#' ### Parameter Correlations     
#' # R-chunk 32  Page 290 
#'  # plot variables against each other, use MQMF panel.cor  Fig 7.15     
#'  
#' pairs(boots$bootpar[,c(1:4,6,7)],lower.panel=panel.smooth,      
#'       upper.panel=panel.cor,gap=0,lwd=2,cex=0.5)     
#'  
#' ### Asymptotic Errors     
#' # R-chunk 33  Page 290
#'  #Start the SPM analysis using asymptotic errors.     
#'  
#' data(dataspm)    # Note the use of hess=TRUE in call to fitSPM      
#' fish <- as.matrix(dataspm)     # using as.matrix for more speed     
#' colnames(fish) <- tolower(colnames(fish))  # just in case   
#' pars <- log(c(r=0.25,K=5200,Binit=2900,sigma=0.20))     
#' ans <- fitSPM(pars,fish,schaefer=TRUE,maxiter=1000,hess=TRUE)  
#' 
#' # R-chunk 34  page 291         
#'  #The hessian matrix from the Schaefer fit to the dataspm data     
#' outfit(ans)     
#'  
#' # R-chunk 35  Page 292
#'  #calculate the var-covar matrix and the st errors     
#'  
#' vcov <- solve(ans$hessian) # calculate variance-covariance matrix     
#' label <- c("r","K", "Binit","sigma")     
#' colnames(vcov) <- label; rownames(vcov) <- label     
#' outvcov <- rbind(vcov,sqrt(diag(vcov)))     
#' rownames(outvcov) <- c(label,"StErr")     
#'  
#' # R-chunk 36  Page 290 Table 7.6 code not in the book 
#'  # tabulate the variance covariance matrix and StErrs     
#'  
#' kable(outvcov,digits=c(5,5,5,5))     
#'  
#' # R-chunk 37  Pages 292 - 293
#'  #generate 1000 parameter vectors from multi-variate normal     
#'  
#' library(mvtnorm)   # use RStudio, or install.packages("mvtnorm")     
#' N <- 1000 # number of parameter vectors, use vcov from above     
#' mvn <- length(fish[,"year"]) #matrix to store cpue trajectories     
#' mvncpue <- matrix(0,nrow=N,ncol=mvn,dimnames=list(1:N,fish[,"year"]))     
#' columns <- c("r","K","Binit","sigma")     
#' optpar <- ans$estimate # Fill matrix with mvn parameter vectors      
#' mvnpar <- matrix(exp(rmvnorm(N,mean=optpar,sigma=vcov)),nrow=N,     
#'                  ncol=4,dimnames=list(1:N,columns))     
#' msy <- mvnpar[,"r"]*mvnpar[,"K"]/4     
#' nyr <- length(fish[,"year"])     
#' depletion <- numeric(N) #now calculate N cpue series in linear space     
#' for (i in 1:N) { # calculate dynamics for each parameter set     
#'   dynamA <- spm(log(mvnpar[i,1:4]),fish)     
#'   mvncpue[i,] <- dynamA$outmat[1:nyr,"predCE"]     
#'   depletion[i] <- dynamA$outmat["2016","Depletion"]     
#' }     
#' mvnpar <- cbind(mvnpar,msy,depletion) # try head(mvnpar,10)     
#'  
#' # R-chunk 38  Page 293
#'  #data and trajectories from 1000 MVN parameter vectors   Fig 7.16     
#'  
#'oldp <-  plot1(fish[,"year"],fish[,"cpue"],type="p",xlab="Year",
#'               ylab="CPUE",maxy=2.0)     
#' for (i in 1:N) lines(fish[,"year"],mvncpue[i,],col="grey",lwd=1)     
#' points(fish[,"year"],fish[,"cpue"],pch=1,cex=1.3,col=1,lwd=2) # data     
#' lines(fish[,"year"],exp(simpspm(optpar,fish)),lwd=2,col=1)# pred      
#' percs <- apply(mvncpue,2,quants)  # obtain the quantiles     
#' arrows(x0=fish[,"year"],y0=percs["5%",],y1=percs["95%",],length=0.03,     
#'        angle=90,code=3,col=1) #add 90% quantiles     
#' msy <- mvnpar[,"r"]*mvnpar[,"K"]/4  # 1000 MSY estimates     
#' text(2010,1.75,paste0("MSY ",round(mean(msy),3)),cex=1.25,font=7) 
#' par(oldp)  # return par to old settings; this line not in book      
#'  
#' # R-chunk 39  Pages 293 - 294
#'  #Isolate errant cpue trajectories Fig 7.17     
#'  
#' pickd <- which(mvncpue[,"2016"] < 0.40)     
#' oldp <- plot1(fish[,"year"],fish[,"cpue"],type="n",xlab="Year",
#'               ylab="CPUE",maxy=6.25)     
#' for (i in 1:length(pickd))      
#'   lines(fish[,"year"],mvncpue[pickd[i],],col=1,lwd=1)     
#' points(fish[,"year"],fish[,"cpue"],pch=16,cex=1.25,col=4)      
#' lines(fish[,"year"],exp(simpspm(optpar,fish)),lwd=3,col=2,lty=2)      
#' par(oldp)  # return par to old settings; this line not in book  
#' 
#'  
#' # R-chunk 40  Page 294
#'  #Use adhoc function to plot errant parameters Fig 7.18     
#'  
#' oldp <- parset(plots=c(2,2),cex=0.85)     
#' outplot <- function(var1,var2,pickdev) {     
#'   plot1(mvnpar[,var1],mvnpar[,var2],type="p",pch=16,cex=1.0,     
#'         defpar=FALSE,xlab=var1,ylab=var2,col=8)     
#'   points(mvnpar[pickdev,var1],mvnpar[pickdev,var2],pch=16,cex=1.0)     
#' }     
#' outplot("r","K",pickd) # assumes mvnpar in working environment     
#' outplot("sigma","Binit",pickd)     
#' outplot("r","Binit",pickd)     
#' outplot("K","Binit",pickd) 
#' par(oldp)  # return par to old settings; this line not in book      
#'  
#' # R-chunk 41  Page 296
#'  #asymptotically sampled parameter vectors  Fig 7.19     
#'  
#' pairs(mvnpar,lower.panel=panel.smooth, upper.panel=panel.cor,   
#'       gap=0,cex=0.25,lwd=2)     
#'  
#'  
#' # R-chunk 42  Page 297
#'  # Get the ranges of parameters from bootstrap and asymptotic     
#'  
#' bt <- apply(bootpar,2,range)[,c(1:4,6,7)]        
#' ay <- apply(mvnpar,2,range)     
#' out <- rbind(bt,ay)     
#' rownames(out) <- c("MinBoot","MaxBoot","MinAsym","MaxAsym")     
#'  
#' # R-chunk 43  Page 297 Table 7.7 code not in the book 
#'  #tabulate ranges from two approsches     
#'  
#' kable(out,digits=c(4,3,3,4,3,4))     
#'  
#' ### Sometimes Asymptotic Errors Work     
#' # R-chunk 44  Pages 297 - 298
#'  #repeat asymptotice errors using abdat data-set Figure 7.20     
#'  
#' data(abdat)     
#' fish <- as.matrix(abdat)     
#' pars <- log(c(r=0.4,K=9400,Binit=3400,sigma=0.05))     
#' ansA <- fitSPM(pars,fish,schaefer=TRUE,maxiter=1000,hess=TRUE)      
#' vcovA <- solve(ansA$hessian) # calculate var-covar matrix     
#' mvn <- length(fish[,"year"])     
#' N <- 1000   # replicates     
#' mvncpueA <- matrix(0,nrow=N,ncol=mvn,dimnames=list(1:N,fish[,"year"]))     
#' columns <- c("r","K","Binit","sigma")     
#' optparA <- ansA$estimate  # Fill matrix of parameter vectors      
#' mvnparA <- matrix(exp(rmvnorm(N,mean=optparA,sigma=vcovA)),     
#'                   nrow=N,ncol=4,dimnames=list(1:N,columns))     
#' msy <- mvnparA[,"r"]*mvnparA[,"K"]/4     
#' for (i in 1:N) mvncpueA[i,]<-exp(simpspm(log(mvnparA[i,]),fish))     
#' mvnparA <- cbind(mvnparA,msy)     
#' oldp <- plot1(fish[,"year"],fish[,"cpue"],type="p",xlab="Year",
#'               ylab="CPUE",maxy=2.5)     
#' for (i in 1:N) lines(fish[,"year"],mvncpueA[i,],col=8,lwd=1)     
#' points(fish[,"year"],fish[,"cpue"],pch=16,cex=1.0) #orig data     
#' lines(fish[,"year"],exp(simpspm(optparA,fish)),lwd=2,col=0)    
#' par(oldp)  # return par to old settings; this line not in book    
#'  
#' # R-chunk 45  Page 298
#'  #plot asymptotically sampled parameter vectors Figure 7.21     
#'  
#' pairs(mvnparA,lower.panel=panel.smooth, upper.panel=panel.cor,     
#'       gap=0,pch=16,col=rgb(red=0,green=0,blue=0,alpha = 1/10))     
#'  
#' ### Bayesian Posteriors     
#' # R-chunk 46  Page 299
#'  #Fit the Fox Model to the abdat data Figure 7.22     
#'  
#' data(abdat); fish <- as.matrix(abdat)     
#' param <- log(c(r=0.3,K=11500,Binit=3300,sigma=0.05))     
#' foxmod <- nlm(f=negLL1,p=param,funk=simpspm,indat=fish,     
#'               logobs=log(fish[,"cpue"]),iterlim=1000,schaefer=FALSE)     
#' optpar <- exp(foxmod$estimate)     
#' ans <- plotspmmod(inp=foxmod$estimate,indat=fish,schaefer=FALSE,     
#'                  addrmse=TRUE, plotprod=TRUE)     
#'  
#' # R-chunk 47  Page 301
#'  # Conduct an MCMC using simpspmC on the abdat Fox SPM     
#'  # This means you will need to compile simpspmC from appendix     
#' set.seed(698381) #for repeatability, possibly only on Windows10     
#' begin <- gettime()  # to enable the time taken to be calculated     
#' inscale <- c(0.07,0.05,0.09,0.45) #note large value for sigma     
#' pars <- log(c(r=0.205,K=11300,Binit=3200,sigma=0.044))     
#' result <- do_MCMC(chains=1,burnin=50,N=2000,thinstep=512,     
#'                   inpar=pars,infunk=negLL,calcpred=simpspmC,     
#'                   obsdat=log(fish[,"cpue"]),calcdat=fish,     
#'                   priorcalc=calcprior,scales=inscale,schaefer=FALSE)     
#'  # alternatively, use simpspm, but that will take longer.      
#' cat("acceptance rate = ",result$arate," \n")     
#' cat("time = ",gettime() - begin,"\n")     
#' post1 <- result[[1]][[1]]     
#' p <- 1e-08     
#' msy <- post1[,"r"]*post1[,"K"]/((p + 1)^((p+1)/p))     
#'  
#' # R-chunk 48 Page 302 
#'  #pairwise comparison for MCMC of Fox model on abdat  Fig 7.23     
#'  
#' pairs(cbind(post1[,1:4],msy),upper.panel = panel.cor,lwd=2,cex=0.2,   
#'       lower.panel=panel.smooth,col=1,gap=0.1)     
#'  
#' # R-chunk 49  Page 302
#'  # marginal distributions of 3 parameters and msy  Figure 7.24     
#'  
#' oldp <- parset(plots=c(2,2), cex=0.85)     
#' plot(density(post1[,"r"]),lwd=2,main="",xlab="r") #plot has a method     
#' plot(density(post1[,"K"]),lwd=2,main="",xlab="K")   #for output from     
#' plot(density(post1[,"Binit"]),lwd=2,main="",xlab="Binit")  # density     
#' plot(density(msy),lwd=2,main="",xlab="MSY")   #try str(density(msy)) 
#' par(oldp)  # return par to old settings; this line not in book      
#'  
#' # R-chunk 50  Page 304
#'  #MCMC r and K parameters, approx 50 + 90% contours. Fig7.25     
#'  
#' puttxt <- function(xs,xvar,ys,yvar,lvar,lab="",sigd=0) {     
#'   text(xs*xvar[2],ys*yvar[2],makelabel(lab,lvar,sep="  ",     
#'        sigdig=sigd),cex=1.2,font=7,pos=4)     
#' } # end of puttxt - a quick utility function     
#' kran <- range(post1[,"K"]);  rran <- range(post1[,"r"])     
#' mran <- range(msy)         #ranges used in the plots     
#' oldp <- parset(plots=c(1,2),margin=c(0.35,0.35,0.05,0.1)) #plot r vs K     
#' plot(post1[,"K"],post1[,"r"],type="p",cex=0.5,xlim=kran,     
#'      ylim=rran,col="grey",xlab="K",ylab="r",panel.first=grid())     
#' points(optpar[2],optpar[1],pch=16,col=1,cex=1.75) # center     
#' addcontours(post1[,"K"],post1[,"r"],kran,rran,  #if fails make     
#'             contval=c(0.5,0.9),lwd=2,col=1)   #contval smaller     
#' puttxt(0.7,kran,0.97,rran,kran,"K= ",sigd=0)     
#' puttxt(0.7,kran,0.94,rran,rran,"r= ",sigd=4)     
#' plot(post1[,"K"],msy,type="p",cex=0.5,xlim=kran,  # K vs msy     
#'      ylim=mran,col="grey",xlab="K",ylab="MSY",panel.first=grid())     
#' points(optpar[2],getMSY(optpar,p),pch=16,col=1,cex=1.75)#center     
#' addcontours(post1[,"K"],msy,kran,mran,contval=c(0.5,0.9),lwd=2,col=1)     
#' puttxt(0.6,kran,0.99,mran,kran,"K= ",sigd=0)     
#' puttxt(0.6,kran,0.97,mran,mran,"MSY= ",sigd=3) 
#' par(oldp)  # return par to old settings; this line not in book      
#'  
#' # R-chunk 51  Page 305
#'  #Traces for the Fox model parameters from the MCMC  Fig7.26     
#'  
#' oldp <- parset(plots=c(4,1),margin=c(0.3,0.45,0.05,0.05),     
#'                outmargin = c(1,0,0,0),cex=0.85)     
#' label <- colnames(post1)     
#' N <- dim(post1)[1]     
#' for (i in 1:3) {     
#'   plot(1:N,post1[,i],type="l",lwd=1,ylab=label[i],xlab="")     
#'   abline(h=median(post1[,i]),col=2)     
#' }     
#' msy <- post1[,1]*post1[,2]/4     
#' plot(1:N,msy,type="l",lwd=1,ylab="MSY",xlab="")     
#' abline(h=median(msy),col=2)     
#' mtext("Step",side=1,outer=T,line=0.0,font=7,cex=1.1)     
#' par(oldp)  # return par to old settings; this line not in book  
#'  
#' # R-chunk 52  Page 306
#'  #Do five chains of the same length for the Fox model     
#'  
#' set.seed(6396679)  # Note all chains start from same place, which is      
#' inscale <- c(0.07,0.05,0.09,0.45)  # suboptimal, but still the chains     
#' pars <- log(c(r=0.205,K=11300,Binit=3220,sigma=0.044))  # differ     
#' result <- do_MCMC(chains=5,burnin=50,N=2000,thinstep=512,     
#'                   inpar=pars,infunk=negLL1,calcpred=simpspmC,     
#'                   obsdat=log(fish[,"cpue"]),calcdat=fish,     
#'                   priorcalc=calcprior,scales=inscale,     
#'                   schaefer=FALSE)     
#' cat("acceptance rate = ",result$arate," \n") # always check this     
#'  
#' # R-chunk 53  Page 306
#'  #Now plot marginal posteriors from 5 Fox model chains    Fig7.27     
#'  
#' oldp <- parset(plots=c(2,1),cex=0.85,margin=c(0.4,0.4,0.05,0.05))     
#' post <- result[[1]][[1]]     
#' plot(density(post[,"K"]),lwd=2,col=1,main="",xlab="K",     
#'      ylim=c(0,4.4e-04),panel.first=grid())     
#' for (i in 2:5) lines(density(result$result[[i]][,"K"]),lwd=2,col=i)     
#' p <- 1e-08     
#' post <- result$result[[1]]     
#' msy <-  post[,"r"]*post[,"K"]/((p + 1)^((p+1)/p))     
#' plot(density(msy),lwd=2,col=1,main="",xlab="MSY",type="l",     
#'      ylim=c(0,0.0175),panel.first=grid())     
#' for (i in 2:5) {     
#'   post <- result$result[[i]]     
#'   msy <-  post[,"r"]*post[,"K"]/((p + 1)^((p+1)/p))     
#'   lines(density(msy),lwd=2,col=i)     
#' }
#' par(oldp)  # return par to old settings; this line not in book       
#'  
#' # R-chunk 54  Page 307 
#'  # get quantiles of each chain     
#'  
#' probs <- c(0.025,0.05,0.5,0.95,0.975)     
#' storeQ <- matrix(0,nrow=6,ncol=5,dimnames=list(1:6,probs))     
#' for (i in 1:5) storeQ[i,] <- quants(result$result[[i]][,"K"])     
#' x <- apply(storeQ[1:5,],2,range)     
#' storeQ[6,] <- 100*(x[2,] - x[1,])/x[2,]     
#'  
#' # R-chunk 55 Page 308 Table 7.8 code not in the book  
#'  #tabulate qunatiles of the five chains     
#'  
#' kable(storeQ,digits=c(3,3,3,3,3))     
#'  
#' ## Management Advice     
#' ### Two Views of Risk     
#' ### Harvest Strategies     
#' ## Risk Assessment Projections     
#' ### Deterministic Projections    
#'  
#' # R-chunk 56  Pages 310 - 311
#'  #Prepare Fox model on abdat data for future projections Fig7.28     
#'  
#' data(abdat); fish <- as.matrix(abdat)     
#' param <- log(c(r=0.3,K=11500,Binit=3300,sigma=0.05))     
#' bestmod <- nlm(f=negLL1,p=param,funk=simpspm,schaefer=FALSE,   
#'                logobs=log(fish[,"cpue"]),indat=fish,hessian=TRUE)     
#' optpar <- exp(bestmod$estimate)     
#' ans <- plotspmmod(inp=bestmod$estimate,indat=fish,schaefer=FALSE,     
#'                  target=0.4,addrmse=TRUE, plotprod=FALSE)     
#'  
#'  
#' # R-chunk 57 Page 312 
#'  
#' out <- spm(bestmod$estimate,indat=fish,schaefer=FALSE)     
#' str(out, width=65, strict.width="cut")     
#'  
#' # R-chunk 58  Page 312 Table 7.9 code not in the book 
#'  #     
#'  
#' kable(out$outmat[1:10,],digits=c(0,4,4,4,4,4,4))     
#'  
#' # R-chunk 59  Page 313
#'  #  Fig 7.29     
#'  
#' catches <- seq(700,1000,50)   # projyr=10 is the default     
#' projans <- spmprojDet(spmobj=out,projcatch=catches,plotout=TRUE)     
#'  
#' ### Accounting for Uncertainty     
#' ### Using Asymptotic Errors     
#' # R-chunk 60  Page 315
#'  # generate parameter vectors from a multivariate normal      
#'  # project dynamics under a constant catch of 900t     
#'  
#' library(mvtnorm)     
#' matpar <- parasympt(bestmod,N=1000) #generate parameter vectors     
#' projs <- spmproj(matpar,fish,projyr=10,constC=900)#do dynamics     
#'  
#' # R-chunk 61  Page 315
#'  # Fig 7.30  1000 replicate projections asymptotic errors     
#'  
#' outp <- plotproj(projs,out,qprob=c(0.1,0.5),refpts=c(0.2,0.4))     
#'  
#' ### Using Bootstrap Parameter Vectors     
#' # R-chunk 62  Page 316
#'  #bootstrap generation of plausible parameter vectors for Fox     
#'  
#' reps <- 1000      
#' boots <- spmboot(bestmod$estimate,fishery=fish,iter=reps,schaefer=FALSE)     
#' matparb <- boots$bootpar[,1:4] #examine using head(matparb,20)     
#'  
#' # R-chunk 63  Page 316
#'  #bootstrap projections. Lower case b for boostrap  Fig7.31     
#'  
#' projb <- spmproj(matparb,fish,projyr=10,constC=900)     
#' outb <- plotproj(projb,out,qprob=c(0.1,0.5),refpts=c(0.2,0.4))     
#'  
#' ### Using Samples from a Bayesian Posterior     
#' # R-chunk 64  Pages 317 - 318 
#'  #Generate 1000 parameter vectors from Bayesian posterior     
#'  
#' param <- log(c(r=0.3,K=11500,Binit=3300,sigma=0.05))     
#' set.seed(444608)     
#' N <- 1000     
#' result <- do_MCMC(chains=1,burnin=100,N=N,thinstep=2048,     
#'                   inpar=param,infunk=negLL,calcpred=simpspmC,     
#'                   calcdat=fish,obsdat=log(fish[,"cpue"]),     
#'                   priorcalc=calcprior,schaefer=FALSE,     
#'                   scales=c(0.065,0.055,0.1,0.475))     
#' parB <- result[[1]][[1]] #capital B for Bayesian     
#' cat("Acceptance Rate = ",result[[2]],"\n")     
#'  
#' # R-chunk 65  Page 318
#'  # auto-correlation, or lack of, and the K trace Fig 7.32     
#'  
#' oldp <- parset(plots=c(2,1),cex=0.85)      
#' acf(parB[,2],lwd=2)     
#' plot(1:N,parB[,2],type="l",ylab="K",ylim=c(8000,19000),xlab="")   
#' par(oldp)  # return par to old settings; this line not in book    
#'  
#' # R-chunk 66  Page 318
#'  #  Fig 7.33     
#'  
#' matparB <- as.matrix(parB[,1:4]) # B for Bayesian     
#' projs <- spmproj(matparB,fish,constC=900,projyr=10) # project them     
#' plotproj(projs,out,qprob=c(0.1,0.5),refpts=c(0.2,0.4)) #projections     
#'  
#' ## Concluding Remarks     
#' ## Appendix: The Use of Rcpp to Replace simpspm     
#' # R-chunk 67  Page 321
#'  
#' library(Rcpp)     
#' cppFunction('NumericVector simpspmC(NumericVector pars,      
#'              NumericMatrix indat, LogicalVector schaefer) {     
#'    int nyrs = indat.nrow();     
#'    NumericVector predce(nyrs);     
#'    NumericVector biom(nyrs+1);     
#'    double Bt, qval;     
#'    double sumq = 0.0;     
#'    double p = 0.00000001;     
#'    if (schaefer(0) == TRUE) {     
#'      p = 1.0;     
#'    }     
#'    NumericVector ep = exp(pars);     
#'    biom[0] = ep[2];     
#'    for (int i = 0; i < nyrs; i++) {     
#'       Bt = biom[i];     
#'       biom[(i+1)] = Bt + (ep[0]/p)*Bt*(1 - pow((Bt/ep[1]),p)) -      
#'                           indat(i,1);     
#'       if (biom[(i+1)] < 40.0) biom[(i+1)] = 40.0;     
#'       sumq += log(indat(i,2)/biom[i]);     
#'     }     
#'     qval = exp(sumq/nyrs);     
#'     for (int i = 0; i < nyrs; i++) {     
#'       predce[i] = log(biom[i] * qval);     
#'     }     
#'     return predce;     
#'  }')     
#' }
NULL