#' @importFrom grDevices dev.new dev.cur dev.off png
#' @importFrom graphics lines mtext par plot points grid title
#' @importFrom graphics arrows axis legend polygon segments layout
#' @importFrom stats qnorm rnorm dnorm runif sd quantile optim loess
#' @importFrom stats dmultinom
#' @importFrom utils tail
NULL


#' @title MQMF R functions for Modelling and Quantitative Methods in Fisheries
#'
#' @description The MQMF package provides a set of functions that
#'     enable translations into R of the main examples in the book
#'     Modelling and Quantitative Methods in Fisheries 2nd edition.
#'     Try \code{browseVignettes("MQMF")}
#'
#' @references Haddon, M. (2011) Modelling and Quantitative Methods in
#'     Fisheries 2nd edition, Chapman & Hall/ CRC Press, Boca Raton,
#'     449 p.
#'
#' @section Fisheries functions:
#' \describe{
#'   \item{bce}{ the Baranov Catch Equation}
#'   \item{discretelogistic}{ surplus production model, either Schaefer or
#'      Fox models. Option to plot the outcomes.}
#'   \item{logist}{ calculates a logistic curve that can be used for 
#'      selectivity, maturity, etc. Uses L50 and delta = L95 - L50}
#'   \item{MaA}{ an alternative logistic curve using a and b L50 = -a/b and
#'      interquartile = 2.Ln(3).b}
#'   \item{negNLL}{ negative log-likelihood for normally distributed 
#'      variables}
#'   \item{negNLP}{ negative log-likelihood for normally distributed 
#'      variables with the option of fixing some parameters, which is 
#'      useful when generating likelihood profiles}
#'   \item{simpspm}{ calculates only the predicted cpue for an SPM}
#'   \item{spm}{ calculates and returns the full dynamics of an SPM}
#'   \item{vB}{ calculates the length-at-age for the von Bertalanffy curve}
#' }
#' @section Utility functions:
#' \describe{
#'   \item{countones}{ used in apply to count the ones in a vector}
#'   \item{countzeros}{ used in apply to count the zeros in a vector}
#'   \item{countgtzero}{ halves the height of a tall narrow data.frame}
#'   \item{countNAs}{ used in apply to count the NAs in a vector}
#'   \item{countgtone}{ used in apply to count the numbers > 1 in a vector}
#'   \item{freqMean}{ calculates the mean and st dev count data}
#'   \item{getmin}{ find the minimum of a series to help with ylim or xlim}
#'   \item{getmax}{ find the maximum of a series to help with ylim or xlim}
#'   \item{getname}{ extracts the name of a variable as character}
#'   \item{halftable}{ subdivides a table to make it shorter and wider}
#'   \item{magnitude}{ defines the relative size of parameters for use when
#'       using nlm or optim without log-transforming the parameters}
#'   \item{makelabel}{ simplifies combining a name with a vector of numbers
#'       for use as a label or a legend entry}
#'   \item{outfit}{ prints a pretty version of the results from optim, nlm,
#'       or nlminb}
#'   \item{printV}{ prints a vector of numbers vertically rather than 
#'      horizontally}
#'   \item{quants}{ used in apply to estimate quantiles across a vector}
#'   \item{which.closest}{ finds the value in a vector closest to a given
#'       value.}
#' }
#' @section Plotting and printing functions:
#' \describe{
#'   \item{addnorm}{ adds a normal distribution to the output from hist}
#'   \item{addlnorm}{ adds a log-normal distribution to output from hist}
#'   \item{inthist}{ plots a histogram of integer values more precisely
#'      than hist.}
#'   \item{newplot}{ is a bare-bones setup routine to generate a plot
#'      in RStudio using a floating window. If you want to alter the
#'      default par settings then you can use either setplot() to get
#'      suitable syntax or, more simply, use parsyn() which only give
#'      a template for the par syntax}
#'   \item{parset}{ defines the par statement for a single plot}
#'   \item{parsyn}{ types the standard syntax for the par command to
#'      the console}
#'   \item{plot1}{ simplifies the plotting of two variables in a single
#'      plot}
#'   \item{plot2}{ sets up a plotting window for two plots}
#'   \item{plotprep}{ Sets up a window and the par values for a plot.
#'      it checks to see if a graphics device is open and opens a new
#'      one if not. This is simply a utility function to save typing
#'      the standard syntax. Some of the defaults can be changed.
#'      Typing the name without () will provide a template for
#'      modification. If 'windows' is called repeatedly this will
#'      generate a new active graphics device each time leaving the
#'      older ones inactive but present. For quick exploratory plots
#'      this behaviour is not wanted, hence the check if an active
#'      device exists already or not.}
#'   \item{printV}{ returns a vector cbinded to 1:length(invect),
#'      which effectively prints the numbers vertically}
#' }
#' @section Data sets:
#' \describe{
#'   \item{abdat}{a fisheries dataset with a fish data.frame, resilience,
#'        and a spsname.}
#'   \item{fishdat}{A fisheries data set containing fisheries dependent
#'       data, biological parameters and biological properties}
#'   \item{invert}{ Fisheris dependent data, catches, cpue, etc}
#'   \item{kimura}{ Simplified length-at-age data}
#'   \item{LatA}{ A dataset containing the sex, the length and the
#'       estimated age for 576 redfish, (Centroberyx affinis) from
#'       eastern Australia sampled in 1997.}
#'   \item{plaice}{ A dataset containing the fish, glb, props, agedata,
#'       and lendata for North sea plaice. Data taken from Beverton and
#'       Holt (1957). The primary use of this data set is to illustrate
#'       the use of catch curves.}
#'   \item{tigers}{ A dataset from Penn and Caputi, 1986, containing the
#'       stock recruitment data for the fiurst example of fitting a
#'       model to data using Log-Normal Likelihoods}
#' }
#' @section Vignettes:
#' To learn more about MQMF, start with the still to be written vignette:
#' \code{browseVignettes(package = "MQMF")} or
#' \code{browseVignettes("MQMF")}
#'
#' @docType package
#' @name MQMF
NULL

#' @title abdat Three data objects suitable for use with MQMF.
#'
#' @description A dataset containing the fish data.frame, the
#'     resilience, and the spsname set up ready for use with MQMF.
#'     In particular it can be used when fitting a surplus
#'     production model.
#'
#' @format A list of three objects
#' \describe{
#'   \item{spsname}{ the name of the species concerned}
#'   \item{resilience}{ a proxy for the relative productivity, for
#'         use when using catch-MSY }
#'   \item{fish}{ a data.frame containing year, catch, and cpue}
#' }
"abdat"

#' @title fishdat Three data objects suitable for use with MQMF.
#'
#' @description A dataset containing the fish data.frame, the glb list, and the
#'     props data.frame set up ready for use with simpleSA. In particular it can
#'     be used with fitASPM, fitSPM, run_cMSY, and DBSRA.
#'
#' @format A list of three objects
#' \describe{
#'   \item{fish}{ a data.frame containing Year, Catch, CPUE, and SE, the standard
#'       error of the CPUE estimates, if present}
#'   \item{glb}{ a list of global variables including maxage, M, parameters for
#'       growth, weight-at-age, maturity-at-age, steepness, R0, selectivity,
#'       resilience, number of ages, and the ages themselves. }
#'   \item{props}{ a data.frame of age, laa, waa, maa, sela, and feca}
#' }
"fishdat"

#' @title invert data derived from a trawl caught invertebrate fishery.
#'
#' @description A dataset containing the invert data.frame as a 31 x 7
#'     matrix. The invert data.frame has both standardized cpue as
#'     well as the unstandardized geometric mean cpue, labelled 'geom'.
#'     There is also columns of the catch, the standard error of the
#'     standardized cpue (too small to be useful in stock assessments),
#'     the number of vessels reporting each year, and thje number of
#'     records ewach year
#'
#' @format A data.frame of fishery dependent data
#' \describe{
#'   \item{year}{literally the year in which the fishery occurred}
#'   \item{catch}{the catch landed intonnes; there is little or no
#'      discarding}
#'   \item{cpue}{the standardized cpue from the fishery, this omits
#'      major changes in the fishery such as the introduction of
#'      quotas in 1992 and a major structural adjustment at the end of
#'      2006}
#'   \item{se}{the standard error of the standardized cpue. This is
#'      very small as there are usually a large number of observations
#'      but that ignores the between year variation.}
#'   \item{geom}{the nominal geometric mean cpue}
#'   \item{vessels}{the number of vessels reporting catches in each year}
#'   \item{records}{the number of records each year.}
#' }
#' @examples
#'  \dontrun{
#'  data(invert)
#'  invert
#' }
"invert"

#' @title kimura simplified length-at-age data.
#'
#' @description A dataset containing the age and length data from
#'     Kimura, D.K. (1980) Likelihood methods for the von Bertalanffy
#'     growth curve. Fishery Bulletin, 77: 765-776. There are only 11
#'     observations because the lengths are averages, which, these
#'     days would be unusual as computers and software can easily
#'     handle many hundreds of observations. But things were more
#'     difficult in 1980!
#'
#' @format A data.frame of simplified length-at-age data
#' \describe{
#'   \item{age}{the estimated age of fish}
#'   \item{length}{the mean length of fish at each age}
#' }
#' @examples
#'  \dontrun{
#'  data(kimura)
#'  kimura
#' }
"kimura"

#' @title LatA Length at age for 576 fish including both males and females.
#'
#' @description A dataset containing the sex, the length and the estimated age
#'    for 576 redfish (Centroberyx affinis) from eastern Australia sampled in 1997,
#'    all from a single port. This data is a small sub-set of very many more
#'    samples collected across species and years by the many excellent people 
#'    running the Integrated Stock Monitoring Program in the Australian 
#'    South East Fishery over the years of its existence.
#'
#' @format A data frame with 576 rows and 3 variables:
#' \describe{
#'   \item{Sex}{gender coded as 1 = males and 2 = females}
#'   \item{Length}{Fork length of the fish, in cms}
#'   \item{Age}{Estimated age from otolith reading}
#' }
"LatA"

#' @title plaice data derived from Beverton and Holt, 1957 for European Plaice.
#'
#' @description plaice data including fish, glb, props, agedata, and lendata
#'     for North sea plaice dervied from tables and the text of the classical
#'     Beverton and Holt, 1957, book. Includes age data that is useful for
#'     illustratung the catch curves. Much of this data has also been included
#'     in the age-structured model described in Haddon, 2011.
#'
#' @format A list of five objects with only the first four containing data, the
#'     lendata only contains formatted data for illustrating that format, it is
#'     not real data. The other objects contain real data.
#' \describe{
#'   \item{fish}{ a data.frame containing year, catch, cpue, SE of the cpue }
#'   \item{glb}{biological parameters relating to growth, selectivity,
#'     weight-at-age, steepness, and resilience and spsname }
#'   \item{props}{ contains six variables ages, laa, waa, maa, sela, and feca,
#'     which are all relative to age.}
#'   \item{agedata}{ a list of 5 objects, yrage - the years in which age data are
#'     available, ages - the observed ages, agemax - the maximum age, nage -
#'     the number of observed ages, and naa - the numbers-at-age by year}
#'   \item{lendata}{ a list of 5 objects akin to the agedata object but for
#'     length data.}
#' }
#' @examples
#'  \dontrun{
#'  data(plaice)
#'  str(plaice)
#'  print(plaice$fish)
#'  print(plaice$agedata)
#' }
"plaice"

#' @title schaefer is the yellowfin tuna fishery data from Schaefer 1957
#'
#' @description schaefer is the yellowfin tuna fishery data from Schaefer, 
#'     M.B. (1957) A study of the dynamics of the fishery for yellowfin 
#'     tuna in the Eastern Tropical Pacific Ocean. _Bulletin, Inter-American
#'     Tropical Tuna Commission_ __2__: 247-285. It contains the year,
#'     the catch, the effort, and the cpue and was used in one of the first 
#'     descriptions of a stock assessment that used a surplus production 
#'     model. The catch-per-unit-effort, cpue, is a ratio cpue of the total
#'     catch divided by the total effort. These days such ratios tend not
#'     to be used with individual records for each day's effort being used
#'     instead. This does not obscure the variation between different 
#'     vessels, areas, depths, and seasons.
#'
#' @format A matrix of fisheries data
#' \describe{
#'   \item{year}{the fishing year from 1934 - 1955}
#'   \item{catch}{the total annual catch, tonnes }
#'   \item{effort}{ the total effort as standard fishing days}
#'   \item{cpue}{the catch per standard fishing day, a ratio cpue}
#' }
#' @examples
#'  \dontrun{
#'  data(schaefer)
#'  schaefer
#' }
"schaefer"

#' @title tigers is spawning biomass and subsequent recruitment data
#'
#' @description tigers is a dataset from Penn, J.W. and N. Caputi (1986)
#'     Spawning stock-recuitment relationships and environmental
#'     influences on the tiger prawn (_Penaeus esculentus_) fishery in
#'     Exmouth Gulf, Western Australia. _Australian Journal of Marine
#'     and Freshwater Research_ __37__: 491-505. It is only 14 rows of
#'     data with a column of Spawn and Recruit, as a data.frame
#'
#' @format A data.frame of spawning recruitment data
#' \describe{
#'   \item{Spawn}{the estimated spawning biomass in a year}
#'   \item{Recruit}{estimated recruitment from the biomass in each year}
#' }
#' @examples
#'  \dontrun{
#'  data(tigers)
#'  tigers
#' }
"tigers"

