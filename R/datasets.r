#' @title abdat A list of fishery data for blacklip abalone
#'
#' @description A dataset of fishery data for blacklip abalone 
#'     (\emph{Haliotis rubra})from part of the Tasmanian west coast 
#'     for the years 1985 - 2008. It contains a data.frame containing
#'     the year, teh catch, and the standardizaed CPUE for four 
#'     statistical blocks of Tasmania's west coast. In particular, 
#'     it can be used when fitting a surplus production model. 
#'     Workable initial parameter values, before log-transformation 
#'     might be: r= 0.4, K=9400, Binit=3400, sigma=0.05 for the 
#'     Schaefer version, while these also work for the Fox model 
#'     one could use r=0.3, K=12000, Binit=4000, sigma=0.05.
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
#' @source Catch data from Mundy, C. and J. McAllister (2019) Tasmanian abalone fishery assessment 2018, Institute for Marine and Antarctic Studies, University of Tasmania, 190p. ISBN: 978-1-925646-46-7. The cpue data is unpublished but  
#' 
#' @examples
#' \dontrun{
#'  data(abdat)
#'  print(abdat)
#'  plot(abdat$year,abdat$cpue, type="l",xlab="year",ylab="CPUE",
#'  panel.first=grid())
#'  points(abdat$year,abdat$cpue,pch=16,cex=1.2)
#' }
NULL

#' @title blackisland tagging data from a blacklip abalone population
#'
#' @description A 108 x 4 data.frame containing deltat, the time
#'     in years between tagging and recapture, len1 the shell length 
#'     at tagging, and len2, the length at recapture, with the growth 
#'     increment, deltal as the last column. This data can be used to 
#'     estimate the growth characteristics of 
#'     abalone from the Black Island site, which is on the south west 
#'     coast of Tasmania, Australia.  The time 
#'     interval between tagging and recapture is 1 year and 1 week,
#'     1.02 years, which reflects the practical problems of taking a 
#'     vessel around the bottom of Tasmania, where it is essential to
#'     wait on suitable weather for such sub-tidal field work.
#'
#' @name blackisland
#' 
#' @docType data
#' 
#' @format A data.frame of six columns
#' \describe{
#'   \item{deltat}{the time between tagging and recapture, in years}
#'   \item{time1}{the date at tagging}
#'   \item{len1}{the shell length when tagged in mm}
#'   \item{time2}{the date of recapture}
#'   \item{len2}{the shell length at recapture in mm}
#'   \item{deltal}{the growth increment between tagging and recapture
#'                 in mm; there are a few negative values.}
#' }
#' 
#' @section Subjects:
#'  \itemize{
#'   \item Tagging data
#'   \item Growth of individuals estimation
#'   \item Binomial likelihoods
#'  }
#'  
#' @source Thanks to Dr Craig Mundy and the abalone team at the Institute of Marine and Antarctic Studies, of 
#'     the University of Tasmania for the use of this data.
#'     
#' @examples
#' \dontrun{
#'  data(blackisland)
#'  print(head(blackisland,20))
#'  plot(blackisland$len1,blackisland$deltal,type="p",pch=16,
#'  xlab="Initial Length mm",ylab="Growth Increment mm",
#'  panel.first=grid())
#'  abline(h=0)
#' }     
NULL


#' @title dataspm A data.frame of fisheries catch and cpue data 
#'
#' @description A data.frame containing 31 years of catch, standardized
#'     cpue, number of records, and the unstandardized geometric mean
#'     cpue. The fisherie sdata can is used in the surplus production
#'     modelling. Initial parameter estimates very close to the 
#'     optimum values could be param <- log(c(r=0.25, K=5500, Binit=3000,
#'     sigma=0.2)) for the Schaefer model and log(c(r=0.15, K=6500,
#'     Binit=3000, sigma=0.2)) for the Fox model
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
#'   \item{geom}{the naive geometric mean cpue of the raw data as kg/hr, also rescaled to the mean of the series}
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
#'     (\emph{ed}) \emph{Stock Assessment for the Southern and Eastern scalefish and shark
#'     fishery 2016 and 2017.} 837 pp. ISBN 978-1-4863-1012-8 data extracted 
#'     from Table 7.96 PinkLing4050 page 216.
#' 
#' @examples 
#' \dontrun{
#'  data(dataspm)
#'  plot(dataspm$year,dataspm$geom,type="l",lwd=2,xlab="Year",
#'  ylab="CPUE",panel.first=grid())
#'  lines(dataspm$year,dataspm$cpue*mean(dataspm$geom),lwd=2,col=2)
#'  legend("topright",c("cpue","geom"), col=c(1,2), lwd=3, bty="n",
#'  cex=1.2)
#' }
NULL


#' @title LatA Length at age for 358 female fish 
#'
#' @description A data.frame containing an index number, the estimated 
#'     age for 358 redfish (\emph{Centroberyx affinis}) from eastern Australia 
#'     sampled in 1997, all from a single port. 
#'     
#' @name LatA
#' 
#' @docType data
#' 
#' @format A data.frame with 358 rows and 3 variables:
#' \describe{
#'   \item{Sex}{gender coded as 1 = males and 2 = females, this is an 
#'       unusual coding as very often one finds females=1 and males=2. 
#'       This shows that simple codes cannot be taken for granted and
#'       can therefore be dangerous so watch out for them}
#'   \item{Length}{Fork length of the fish, in cms}
#'   \item{Age}{Estimated age from otolith reading}
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
#' @source This data is a small sub-set of very many more samples collected 
#'     across species and years by the many excellent people running the 
#'     Integrated Stock Monitoring Program in the Australian South East 
#'     Fishery over the years of its existence. Thanks to the Australian
#'     Fisheries Management Authority for permission to use this data.
#'     
#' @examples 
#' \dontrun{
#'   data(LatA)
#'   pars <- c(27.0,0.15,-2.0) # von Bertalanffy
#'   bestvB <- nlm(f=ssq,funk=vB,observed=LatA$length,p=pars,
#'                 ages=LatA$age,typsize=magnitude(pars),iterlim=1000)
#'                 outfit(bestvB,backtran=FALSE,title="vB")
#' }
NULL

#' @title minnow is individual growth data for use with growth curves
#'
#' @description minnow is a dataset of mean length against time in weeks for
#'     minnows (\emph{Phoxinus phoxinus}), derived from Pitcher & 
#'     Macdonald (1973) for use when fitting growth curves, especially 
#'     seasonal growth curves. The data exhibit increases and decreases 
#'     in length as each year progresses because these are mean lengths 
#'     rather than individual measurements (which would, more typically,
#'     be used these days). The data have been read off a graph within 
#'     the paper as it is not reported explicitly and are therefore 
#'     only approximate, but will do for our purposes (but expect 
#'     different parameters to those shown in the original paper).
#'     This is length at time not age, though time is being used as 
#'     a proxy for age there is no age 0.
#'
#' @name minnow
#' 
#' @docType data
#' 
#' @format A data.frame of mean length-at-time data
#' \describe{
#'   \item{week}{the week of sampling minnows for lengths}
#'   \item{length}{the mean length in the corresponding week in mm}
#' }
#' 
#' @section Subjects:
#'  \itemize{
#'    \item seasonal growth curves
#'    \item von Bertanlanffy
#'    \item Model residuals
#'  }
#'  
#' @source data measured from Figure 2, page 602 in Pitcher, T.J., and P.D M. MacDonald. (1973) 
#' Two models of seasonal growth. \emph{Journal of Applied Ecology} 10:599–606.
#' 
#' @examples
#'  \dontrun{
#'  data(minnow)
#'  plot1(minnow$week,minnow$length,type="p",pch=16,cex=1.2,
#'        xlab="Week",ylab="Length mm")
#' }
NULL


#' @title npf fishery catch data from Northern Prawn Fishery 1970-1992
#'
#' @description npf is fishery catch data from Australia's Northern 
#'     Prawn Fishery from 1970 to 1992 summarized from Robins and
#'     Somers, 1994. It contains the catches, in tonnes, 
#'     of banana prawns (\emph{Penaeus merguiensis} and \emph{P. indicus}), 
#'     tiger prawns (brown - \emph{P. esculentus}) and (grooved - \emph{P. 
#'     semisulcatus}), endeavour prawns, (\emph{Metapenaeus endevaouri} and 
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
#'   \item{boats}{the number of vessesl fishing in that year.}
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
#'  \dontrun{
#'  data(npf)
#'  npf
#'  plot1(npf$year,npf$tiger,xlab="Year",ylab="Tonnes",lwd=2)
#'  lines(npf$year,npf$endeavour,col=2,lwd=2)
#'  legend("topleft",c("Tiger","Endeavour"),col=c(1,2),lwd=3,
#'         bty="n",cex=1.5)
#' }
NULL

#' @title pttuna is yellowfin tuna fishery data from Pella-Tomlinson 1969
#'
#' @description pttuna is yellowfin tuna fishery data from Pella-Tomlinson's
#'     (1969) classical paper describing their generalized surplsu production 
#'     model. This is the same data as contained in the schaef data-set, except
#'     it is extended from 1934 - 1967. Some of the values are slightly 
#'     different, and their table rounds off the cpue estimates slightly 
#'     differently but the catch and effort figures are theirs. It contains 
#'     the year, the catch, the effort, and the cpue (which is just the total
#'     catch divided by the total effort, a ratio estimate). Initial parameter 
#'     estimates close to the optimum values for the Schaefer model could be
#'     param <- log(c(r=0.28,K=2100000,Binit=2400000,sigma=0.16)). With this
#'     longer time-series the eventuial MSY estimate was somewhat larger than 
#'     when just the schaef data are used.
#'
#' @name pttuna
#' 
#' @docType data
#' 
#' @format A data.frame of fisheries data
#' \describe{
#'   \item{year}{the fishing year from 1934 - 1955}
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
#' @source from Table 6 page 457 in Pella, J.J. and P.K. Tomlinson (1969) A Generalized Stock Production Model. \emph{Bulletin, Inter-American Tropical Tuna Commission} 13(3): 421-458. Obtainable from
#'     \emph{https://www.iattc.org/BulletinsENG.htm}
#' 
#' @examples
#'  \dontrun{
#'  data(pttuna)
#'  pars <- log(c(r=0.25,K=2.1e06,Binit=2.2e06,sigma=0.2))
#'  answer <- fitSPM(pars,pttuna,schaefer=TRUE,maxiter=1000)
#'  outfit(answer,title="Pella-Tomlinson Data",digits=4)
#' }
NULL


#' @title schaef is yellowfin tuna fishery data from Schaefer 1957
#'
#' @description schaef is yellowfin tuna fishery data from Schaefer
#'     (1957) It contains the year, the catch, the effort, and the cpue 
#'     and was used in one of the first descriptions of a stock 
#'     assessment that used a surplus production model. The catch-per-
#'     unit-effort, cpue, is a ratio cpue of the total catch divided by 
#'     the total effort as thousands of punds per day. These days such 
#'     ratios tend not to be used with individual records 
#'     for each day's effort being used instead. 
#'     This does not obscure the variation between different 
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
#'  \dontrun{
#'  data(schaef)
#'  pars <- log(c(r=0.2,K=2.1e06,Binit=2.2e06,sigma=0.2))
#'  answer <- fitSPM(pars,schaef,schaefer=TRUE,maxiter=1000)
#'  outfit(answer,title="Schaefer, 1957 Data",digits=4)
#' }
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
#'
#' @source Many thanks to the Institute of Marine and Antarctic Science, 
#'     which is part of the University of Tasmania, and especially to 
#'     Dr Craig Mundy, leader of the Abalone Group, for permission to use 
#'     this data collected in February 1995.
#' 
#' @examples
#'  \dontrun{
#'  data(tasab)
#'  head(tasab,20)
#'  table(tasab$site,tasab$sex)
#' }
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
#'     (1986) Spawning stock-recuitment relationships and environmental 
#'     influences on the tiger prawn (\emph{Penaeus esculentus}) fishery 
#'     in Exmouth Gulf, Western Australia. \emph{Australian Journal of 
#'     Marine and Freshwater Research} 37: 491-505. Sorted on spawning index.
#' 
#' @examples
#'  \dontrun{
#'  data(tigers)
#'  tigers
#'  plot1(tigers$Spawn,tigers$Recruit,type="p",pch=16,cex=1.25)
#' }
NULL

