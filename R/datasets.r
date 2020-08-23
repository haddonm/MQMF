#' @title abdat A list of fishery data for blacklip abalone
#'
#' @description A dataset of fishery data for blacklip abalone 
#'     (\emph{Haliotis rubra}) from part of the Tasmanian west coast 
#'     for the years 1985 - 2008. It contains a data.frame containing
#'     the year, the catch, and the standardized CPUE from four 
#'     statistical blocks of Tasmania's west coast combined. In 
#'     particular, it can be used when fitting a surplus production 
#'     model. Workable initial parameter values, before log-transformation 
#'     might be: r= 0.4, K=9400, Binit=3400, sigma=0.05 for the 
#'     Schaefer version, while these also work for the Fox model 
#'     one could more efficiently use r=0.3, K=12000, Binit=4000, sigma=0.05.
#'
#' @name abdat
#' 
#' @docType data
#' 
#' @format A data.frame of three columns
#' \describe{
#'   \item{year}{the annual year in which the catches occurred}
#'   \item{catch}{the reported landed catch in tonnes, to the nearest kilogram}
#'   \item{cpue}{the standardized catch-per-unit-effort for this dive fishery}
#' }
#' 
#' @section Subjects:
#'  \itemize{
#'   \item Surplus Production Modelling, Schaefer and Fox models
#'   \item Model fitting using maximum likelihood
#'   \item Uncertainty examples
#'  }
#' 
#' @source Catch data from Mundy, C. and J. McAllister (2019) Tasmanian abalone fishery assessment 2018, Institute for Marine and Antarctic Studies, University of Tasmania, 190p. ISBN: 978-1-925646-46-7. The cpue data is an unpublished early attempt at standardizing the cpue data with respect to month, block, and diver. Many more details are now included in such analyses.
#' 
#' @examples
#'  data(abdat)
#'  print(abdat)
#'  oldpar <- par(no.readonly=TRUE)
#'  plot(abdat$year,abdat$cpue, type="l",xlab="year",ylab="CPUE",
#'       panel.first=grid())
#'  points(abdat$year,abdat$cpue,pch=16,cex=1.2)
#'  par(oldpar)
NULL

#' @title blackisland tagging data from a blacklip abalone population
#'
#' @description A 108 x 4 data.frame containing dt, the time
#'     in years between tagging and recapture, l1 the shell length 
#'     at tagging, and l2, the length at recapture, with the growth 
#'     increment, dl as the last column. This data can be used to 
#'     estimate the growth characteristics of 
#'     abalone from the Black Island site, which is on the south west 
#'     coast of Tasmania, Australia.  The mean time 
#'     interval between tagging and recapture is 1 year and 1 week,
#'     1.02 years, which reflects the practical problems of taking a 
#'     vessel around the bottom of Tasmania, where it is essential to
#'     wait on suitable weather for such sub-tidal field work.
#'
#' @name blackisland
#' 
#' @docType data
#' 
#' @format A data.frame of four columns
#' \describe{
#'   \item{dt}{the time between tagging and recapture, in years}
#'   \item{l1}{the shell length when tagged in mm}
#'   \item{l2}{the shell length at recapture in mm}
#'   \item{dl}{the growth increment between tagging and recapture
#'                 in mm; there are zero values.}
#' }
#' 
#' @section Subjects:
#'  \itemize{
#'   \item Tagging data
#'   \item Estimation of individual growth
#'   \item Binomial likelihoods
#'   \item Faben's version of the von Bertalanffy curve
#'  }
#'  
#' @source Thanks to Dr Craig Mundy and the abalone team at the Institute of 
#'     Marine and Antarctic Studies, of the University of Tasmania for the use 
#'     of this data.
#'     
#' @examples
#'  data(blackisland)
#'  print(head(blackisland,20))
#'  oldpar <- par(no.readonly=TRUE)
#'  plot(blackisland$l1,blackisland$dl,type="p",pch=16,
#'  xlab="Initial Length mm",ylab="Growth Increment mm",
#'  panel.first=grid())
#'  abline(h=0)
#'  par(oldpar)
NULL

#' @title dataspm A data.frame of fisheries catch and cpue data 
#'
#' @description A data.frame containing 31 years of catch, standardized
#'     cpue, number of records, and the unstandardized geometric mean
#'     cpue for Pink Ling (\emph{Genypterus blacodes}). The fisheries data can 
#'     be used in the surplus production modelling in Chapter 7. Initial 
#'     parameter estimates very close to the optimum values could be 
#'     param <- log(c(r=0.25, K=5500, Binit=3000,sigma=0.2)) for the Schaefer 
#'     model and log(c(r=0.15, K=6500, Binit=3000, sigma=0.2)) for the Fox model
#'     
#' @name dataspm
#' 
#' @docType data    
#'
#' @format A 31 x 5 data.frame
#' \describe{
#'   \item{year}{the year from 1986 t0 2016}
#'   \item{catch}{the catch in tonnes to the nearest 100kg}
#'   \item{cpue}{the standardized cpue scaled to the mean of the series}
#'   \item{records}{the number of records making up the yearly totals}
#'   \item{geom}{the naive geometric mean cpue of the raw data as kg/hr, also 
#'               rescaled to the mean of the series}
#' }
#' 
#' @section Subjects:
#'  \itemize{
#'   \item Fishery data-set
#'   \item Surplus Production Modelling
#'   \item Log-Normal likelihoods
#'  }
#' 
#' @source Haddon, M. and M. Sporcic (2017) Catch rate standardizations for 
#'     selected SESSF Species (data to 2016) pp 43-406 in Tuck, G.N. 
#'     (\emph{ed}) \emph{Stock Assessment for the Southern and Eastern scalefish 
#'     and shark fishery 2016 and 2017.} 837 pp. ISBN 978-1-4863-1012-8 data 
#'     extracted from Table 7.96 PinkLing4050 page 216.
#' 
#' @examples 
#'  data(dataspm)
#'  oldpar <- par(no.readonly=TRUE)
#'  plot(dataspm$year,dataspm$geom,type="l",lwd=2,xlab="Year",
#'  ylab="CPUE",panel.first=grid())
#'  lines(dataspm$year,dataspm$cpue*mean(dataspm$geom),lwd=2,col=2)
#'  legend("topright",c("cpue","geom"), col=c(1,2), lwd=3, bty="n",
#'  cex=1.2)
#'  par(oldpar)
NULL


#' @title LatA Simulated length-at-age for 358 female fish 
#'
#' @description A data.frame containing the simulated 
#'     age for an array of different lengths based upon the properties
#'     of an extensive collection of redfish (\emph{Centroberyx affinis}) 
#'     length-at-age data from eastern Australia sampled in the 1990's. 
#'     
#' @name LatA
#' 
#' @docType data
#' 
#' @format A data.frame with 358 rows and 2 variables:
#' \describe{
#'   \item{age}{simulated ages in years}
#'   \item{length}{consequent simulated fork length of the fish, in cms}
#' }
#' 
#' @section Subjects:
#'  \itemize{
#'   \item Estimating individual growth from length-at-age data
#'   \item von Bertalanffy growth curve
#'   \item Gompertz growth curve
#'   \item Michaelis-Menton curve used as a growth curve 
#'  }
#'  
#' @source The data this simulation is based upon is from length-at-age 
#'     data for one species collected over many years by the many 
#'     excellent people running the Integrated Stock Monitoring Program 
#'     in the Australian South East Fishery over the years of its 
#'     existence. The simulation is based on a characterization of 
#'     redfish properties and includes random error in the hypothetical 
#'     measurements as well as the processes of growth (i.e. both 
#'     measurement and process error). The other inputs were a selected 
#'     set of growth parameters and the relative frequency of different 
#'     ages vs lengths.
#'     
#' @examples 
#'   data(LatA)
#'   pars <- c(27.0,0.15,-2.0) # von Bertalanffy
#'   bestvB <- nlm(f=ssq,funk=vB,observed=LatA$length,p=pars,
#'                 ages=LatA$age,typsize=magnitude(pars))
#'   outfit(bestvB,backtran=FALSE,title="vB")
NULL

#' @title minnow contains weekly growth data for use with seasonal growth curves
#'
#' @description minnow is a dataset of mean length against time in weeks for
#'     minnows (\emph{Phoxinus phoxinus}), derived from Pitcher & 
#'     Macdonald (1973) for use when fitting growth curves, especially 
#'     seasonal growth curves. The data exhibit increases and decreases 
#'     in length as each year progresses because these are mean lengths 
#'     rather than individual measurements (which would, more typically,
#'     be used these days). The data have been read off a graph within 
#'     the original paper as it is not reported explicitly, and are 
#'     therefore only approximate, but will do for our purposes (but 
#'     expect different parameters to those reported in the original 
#'     paper). This is length at time not age. Though time is being 
#'     used as a proxy for age there is no age 0.
#'
#' @name minnow
#' 
#' @docType data
#' 
#' @format A data.frame of mean length-at-time data
#' \describe{
#'   \item{week}{the week of sampling minnows for lengths}
#'   \item{length}{the estimated mean length in the corresponding week in mm}
#' }
#' 
#' @section Subjects:
#'  \itemize{
#'    \item seasonal growth curves
#'    \item von Bertalanffy
#'    \item Model residuals
#'  }
#'  
#' @source data measured from Figure 2, page 602 in Pitcher, T.J., and P.D M. MacDonald. (1973) 
#' Two models of seasonal growth. \emph{Journal of Applied Ecology} 10:599–606.
#' 
#' @examples
#'  data(minnow)
#'  oldpar <- par(no.readonly=TRUE)
#'  plot1(minnow$week,minnow$length,type="p",pch=16,cex=1.2,
#'        xlab="Week",ylab="Length mm")
#'  par(oldpar)
NULL

#' @title npf fishery catch data from Northern Prawn Fishery 1970-1992
#'
#' @description npf is fishery catch data from Australia's Northern 
#'     Prawn Fishery from 1970 to 1992 summarized from Robins and
#'     Somers, 1994. It contains the catches, in tonnes, 
#'     of banana prawns (\emph{Penaeus merguiensis} and \emph{P. indicus}), 
#'     tiger prawns (brown - \emph{P. esculentus}) and (grooved - \emph{P. 
#'     semisulcatus}), endeavour prawns, (\emph{Metapenaeus endeavouri} and 
#'     \emph{M. ensis}), king prawns (\emph{P. latisulcatus} and 
#'     \emph{P. longistylus}), the number of vessels fishing, and 
#'     the annual effort as boat-days.
#'
#' @name npf
#' 
#' @docType data
#' 
#' @format A data.frame of fisheries data
#' \describe{
#'   \item{year}{the fishing year from 1970 - 1992.}
#'   \item{banana}{banana prawn catches, tonnes.}
#'   \item{tiger}{tiger prawn catches, tonnes.}
#'   \item{endevaour}{endeavour prawn catches, tonnes.}
#'   \item{king}{king prawn catches, tonnes.}
#'   \item{boats}{the number of vessels fishing in that year.}
#'   \item{boatday}{the total annual effort as boatdays.}
#' }
#' 
#' @section Subjects:
#'  \itemize{
#'    \item correlation analysis and regression
#'    \item Bootstrap percentile confidence intervals
#'    \item Model residuals
#'  }
#'   
#' @source Robins, C. and I. Somers (1994) Appendix A. Fishery 
#'     Statistics pp 141 - 164 in Pownall, P.C. (ed.) Australia's 
#'     Northern Prawn Fishery: The first 25 years. NPF25. Cleveland, 
#'     Australia. 179p.
#' @examples
#'  data(npf)
#'  npf
#'  oldpar <- par(no.readonly=TRUE)
#'  plot1(npf$year,npf$tiger,xlab="Year",ylab="Tonnes",lwd=2)
#'  lines(npf$year,npf$endeavour,col=2,lwd=2)
#'  legend("topleft",c("Tiger","Endeavour"),col=c(1,2),lwd=3,
#'         bty="n",cex=1.5)
#'  par(oldpar)
NULL

#' @title pttuna is yellowfin tuna fishery data from Pella-Tomlinson 1969
#'
#' @description pttuna is yellowfin tuna fishery data from Pella-Tomlinson's
#'     (1969) classical paper describing their generalized surplus production 
#'     model. This is the same data as contained in the schaef data-set, except
#'     it is extended from 1934 - 1967. Some of the values are slightly 
#'     different, and their table rounds off the cpue estimates slightly 
#'     differently but the catch and effort figures are theirs. It contains 
#'     the year, the catch, the effort, and the cpue (which is just the total
#'     catch divided by the total effort, a ratio estimate). Initial parameter 
#'     estimates close to the optimum values for the Schaefer model could be
#'     param <- log(c(r=0.28,K=2100000,Binit=2400000,sigma=0.16)). With this
#'     longer time-series the eventual MSY estimate was somewhat larger than 
#'     when just the schaef data are used.
#'
#' @name pttuna
#' 
#' @docType data
#' 
#' @format A data.frame of fisheries data
#' \describe{
#'   \item{year}{the fishing year from 1934 - 1967}
#'   \item{catch}{the total annual catch, '000s of pounds }
#'   \item{effort}{the total effort as standard class 4 baitboat fishing days}
#'   \item{cpue}{the catch '000 pounds per standard class 4 day, a ratio cpue}
#' }
#' 
#' @section Subjects:
#'  \itemize{
#'    \item surplus production modelling
#'    \item classical fisheries data
#'    \item Log-Normal likelihoods
#'  }
#'  
#' @source from Table 6 page 457 in Pella, J.J. and P.K. Tomlinson (1969) A 
#'     Generalized Stock Production Model. \emph{Bulletin, Inter-American 
#'     Tropical Tuna Commission} 13(3): 421-458. Obtainable from 
#'     \emph{https://www.iattc.org/BulletinsENG.htm}
#' 
#' @examples
#'  data(pttuna)
#'  pars <- log(c(r=0.25,K=2.1e06,Binit=2.2e06,sigma=0.2))
#'  answer <- fitSPM(pars,pttuna,schaefer=TRUE,maxiter=1000)
#'  outfit(answer,title="Pella-Tomlinson Data",digits=4)
NULL


#' @title schaef is yellowfin tuna fishery data from Schaefer 1957
#'
#' @description schaef is yellowfin tuna fishery data from Schaefer
#'     (1957) It contains the year, the catch, the effort, and the cpue 
#'     and was used in one of the first descriptions of a stock 
#'     assessment that used a surplus production model. The catch-per-
#'     unit-effort, cpue, is a ratio cpue of the total catch divided by 
#'     the total effort as thousands of pounds per day. These days such 
#'     ratios tend not to be used, with individual records 
#'     for each day's effort being used instead. Using individual records 
#'     does not obscure the variation between different 
#'     vessels, areas, depths, and seasons. Initial parameter estimates 
#'     close to the optimum values for both the Schaefer model and the
#'     Fox model could be
#'     param <- log(c(r=0.24,K=2100000,Binit=2200000,sigma=0.2))
#'
#' @name schaef
#' 
#' @docType data
#' 
#' @format A data.frame of fisheries data
#' \describe{
#'   \item{year}{the fishing year from 1934 - 1955}
#'   \item{catch}{the total annual catch, '000s of pounds }
#'   \item{effort}{the total effort as standard class 4 clipper fishing days}
#'   \item{cpue}{the catch '000 pounds per standard class 4 day, a ratio cpue}
#' }
#' 
#' @section Subjects:
#'  \itemize{
#'    \item surplus production modelling
#'    \item classical fisheries data
#'    \item Log-Normal likelihoods
#'  }
#'  
#' @source from Table 1 page 266 in Schaefer, M.B. (1957) A study of the dynamics of the 
#'     fishery for yellowfin tuna in the Eastern Tropical Pacific Ocean. 
#'     Bulletin, Inter-American Tropical Tuna Commission 2: 247-285. Obtainable from
#'     \emph{https://www.iattc.org/BulletinsENG.htm}
#' 
#' @examples
#'  data(schaef)
#'  pars <- log(c(r=0.2,K=2.1e06,Binit=2.2e06,sigma=0.2))
#'  answer <- fitSPM(pars,schaef,schaefer=TRUE,maxiter=1000)
#'  outfit(answer,title="Schaefer, 1957 Data",digits=4)
NULL

#' @title tasab is a matrix of abalone maturity-at-length data
#'
#' @description tasab is a 715 x 4 matrix of maturity-at-length data
#'     for blacklip abalone (\emph{Haliotis rubra}) from two sites 
#'     along the Tasmanian west coast. All data was collected in 
#'     February 1995, but details, such as site name, accurate 
#'     location, statistical block, year, month, and other 
#'     details have been omitted for brevity. See section on maturity
#'     within the Static Models chapter for detailed use of this
#'     data-set.
#'
#' @name tasab
#' 
#' @docType data
#' 
#' @format A data.frame of maturity-at-length data
#' \describe{
#'   \item{site}{an identifier for the two different sites sampled}
#'   \item{sex}{I = immature, M = male, F = female}
#'   \item{length}{the shell length in mm}
#'   \item{mature}{was the animal mature = 1 or not = 0}
#' }
#' 
#' @section Subjects:
#'  \itemize{
#'    \item maturity ogives or logistic curves
#'    \item Binomial likelihoods
#'  }
#'
#' @source Many thanks to the Institute of Marine and Antarctic Science, 
#'     which is part of the University of Tasmania, and especially to 
#'     Dr Craig Mundy, leader of the Abalone Group, for permission to use 
#'     this data collected in February 1995.
#' 
#' @examples
#'  data(tasab)
#'  head(tasab,20)
#'  table(tasab$site,tasab$sex)
NULL

#' @title tigers is tiger prawn recruitment data from Penn & Caputi 1986
#'
#' @description tigers is a dataset of only 14 rows of data with a 
#'     column of Spawning index and Recruitment index, as a data.frame. 
#'     The timing of the recruitment index is up to half a year after 
#'     the spawning index.
#'
#' @name tigers
#' 
#' @docType data
#' 
#' @format A data.frame of spawning recruitment data
#' \describe{
#'   \item{Spawn}{the estimated spawning biomass index in a year (Aug - Oct)}
#'   \item{Recruit}{estimated recruitment from the biomass in each year (Mar - May)}
#' }
#' 
#' @section Subjects:
#'  \itemize{
#'    \item Stock-recruitment curves
#'    \item Beverton-Holt and Ricker Models
#'    \item Static model fitting
#'  }
#'  
#' @source Extracted from Table 2, page 496 of Penn, J.W. and N. Caputi 
#'     (1986) Spawning stock-recruitment relationships and environmental 
#'     influences on the tiger prawn (\emph{Penaeus esculentus}) fishery 
#'     in Exmouth Gulf, Western Australia. \emph{Australian Journal of 
#'     Marine and Freshwater Research} 37: 491-505. Sorted on spawning index.
#' 
#' @examples
#'  data(tigers)
#'  tigers
#'  oldpar <- par(no.readonly=TRUE)
#'  plot1(tigers$Spawn,tigers$Recruit,type="p",pch=16,cex=1.25)
#'  par(oldpar)
NULL

#' @title twoindex has orange roughy catches with hypothetical cpue
#'
#' @description twoindex is a 35 x 4 data.frame of fishery data made 
#'     up of smoothed real catches but two simulated indices of relative
#'     abundance. This data-set is designed to be used to illustrate
#'     the implementation of surplus production models when there are
#'     more than one time-series of relative abundance indices. This is not
#'     currently implemented in the book but is put here for use by readers
#'     should they wish to pursue this approach. The
#'     indices have been designed to generate a workable answer but also
#'     require the use of a penalty on harvest rates to avoid massively 
#'     inflated harvest rates well above 1. Instead of using simpspm,
#'     spm, and negLL1, we need to use simpspmM, spmCE, and negLLM.
#'     The cpue series are hypothetical and have been designed to 
#'     illustrate the use of penalty1 and the use of multiple indices 
#'     of relative abundance. The real stock assessment uses acoustic 
#'     survey indices and also uses many years of age composition data 
#'     inside Stock Synthesis 3, not surprisingly the inclusion of real
#'     time-series of indices and of age-composition data leads to very 
#'     different results.
#'
#' @name twoindex
#' 
#' @docType data
#' 
#' @format A data.frame of fishery data 
#' \describe{
#'   \item{year}{the calender year of fishing}
#'   \item{catch}{the reported catch in tonnes}
#'   \item{cpue1}{the first index of relative abundance}
#'   \item{cpue2}{the second index of relative abundance}
#' }
#' 
#' @section Subjects:
#'  \itemize{
#'    \item Surplus production models
#'    \item Dynamic model fitting
#'    \item -ve log-likelihoods
#'  }
#'  
#' @source Catches extracted from Table 4, page 11 of Haddon, M. (2017) 
#'    Orange Roughy East (Hoplostethus atlanticus) stock assessment
#'    using data to 2016 Report to November 2017 SE RAG meeting. CSIRO, 
#'    Oceans and Atmosphere, Australia. 51p. from 
#'    https://www.afma.gov.au/fisheries-management/species/orange-roughy 
#'    Catch data extended to 2019 using AFMA's catchwatch system. 
#' 
#' @examples
#'  data(twoindex)
#'  fish <- as.matrix(twoindex)
#'  pars <- log(c(0.04,155000,0.4,0.3))
#'  bestSP <- nlm(f=negLLM,p=pars,funk=simpspmM,indat=fish,
#'              schaefer=TRUE,logobs=log(fish[,c("cpue1","cpue2")]),
#'              steptol=1e-06,harvpen=TRUE)
#'  namepar <- c("r", "K", "Binit","sigma")
#'  outfit(bestSP,parnames=namepar)  # best fitting estimates
#'  # if 'Either ~local min or steptol too small try 'steptol=1e-05'
#'  # plotprep(width=7,height=5,newdev=FALSE) # for external plot
#'  answer <- plotspmmod(bestSP$estimate,indat=fish,
#'                       plotprod=TRUE,maxy=3.4)
NULL
