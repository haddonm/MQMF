


data(invert)
fish <- invert$fish
fish
colnames(fish) <- tolower(colnames(fish))
pars <- c(r=0.642,K=5170,Binit=2800,sigma=2)
epars <- log(pars)
predCE <- simpspm(epars,fish)
cbind(fish[,"year"],fish[,"cpue"],predCE)
nlm(f=negNLL,epars,simpspm,independent=)


year <- 1985:2008
catch <- c(1018,742,868,715,585,532,566,611,548,499,479,428,657,481,645,961,
           940,912,955,935,940,952,1030,985)
cpue <- c(0.6008,0.6583,0.6791,0.6889,0.7134,0.7221,0.7602,0.7931,0.8582,
          0.8876,1.0126,1.1533,1.2326,1.2764,1.3307,1.3538,1.2648,1.2510,
          1.2069,1.1552,1.1238,1.1281,1.1113,1.0377)
dat <- makespmdata(cbind(year,catch,cpue))
pars <- c(0.35,7800,3500)
ans <- displayModel(pars,dat)
bestSP <- optim(par=pars,fn=ssq,funk=simpspm,independent=cpue,)
bestSP
ans <- displayModel(bestSP$par,dat,schaefer=TRUE)
str(ans)

pars <- c(r=0.642,K=5170,Binit=2800,sigma=2)
epars <- log(pars)
best <- nlm(negNLL,pars,funk=simpspm,independent=tigers$Spawn,
            dependent=log(fish$cpue),schaefer=TRUE)
outfit(best)

 data(kimura)
 pars <- c(Linf=56.0,K=0.4,t0=0.2,sigma=1.5)
 negNLL(pars,vB,independent=kimura[,"age"],observed=kimura[,"length"])

