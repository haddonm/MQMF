
#' @importFrom grDevices dev.new dev.cur dev.off png palette
#' @importFrom graphics lines mtext par plot points grid title abline
#' @importFrom graphics arrows axis legend polygon segments layout text
#' @importFrom graphics contour hist
#' @importFrom stats qnorm rnorm dnorm runif sd quantile optim loess
#' @importFrom stats dmultinom anova ccf lm median nlm dchisq qchisq dlnorm
#' @importFrom stats pnorm cor
#' @importFrom utils tail head
#' @importFrom MASS kde2d
#' @importFrom mvtnorm rmvnorm
NULL


#' @title MQMF functions for Using R for Modelling and Quantitative Methods in Fisheries
#'
#' @description The MQMF package Provides functions for use with the book
#'     (\emph{Using R for Modelling and Quantitative Methods in Fisheries}), 
#'     being published by CRC Press / Chapman & Hall in 
#'     their (\emph{Using R}) series. Although no vignettes are included 
#'     all examples from the chapters are included as help pages for functions. 
#'     Try ?chapter2, ?chapter3, ..., or ?chapter7, which will provide a listing
#'     of all the code chunks included in the book (and some that are not). 
#'     To see a listing of all the help files, scroll to the bottom of any help 
#'     screen and click the link, and an ordered listing of all visible 
#'     functions will be shown. A development version of MQMF is available on 
#'     GitHub at https://github.com/haddonm/MQMF.
#'
#' @references Haddon, M. (2021) Using R for Modelling and Quantitative 
#'     Methods in Fisheries, CRC Press / Chapman & Hall/ Boca Raton 337p.
#'     ISBN: 9780367469894.
#'     
#'     Haddon, M. (2023) https://haddonm.github.io/URMQMF  A GitBook version
#'     
#'
#' @section Data sets:
#' \describe{
#'   \item{abdat}{ a 3 column data.frame of blacklip abalone fishery data}
#'   \item{blackisland}{ tagging data for growth of Black Island abalone}
#'   \item{dataspm}{ a data.frame of fisheries catch and cpue data}
#'   \item{LatA}{ simulated length-at-age for 358 female fish}
#'   \item{minnow}{ weekly growth data for use with seasonal growth curves}
#'   \item{npf}{ fishery catch data from the Northern Prawn Fishery 1970-1992}
#'   \item{pttuna}{ yellowfin tunafishery data from Pella-Tomlinson 1969}
#'   \item{schaef}{ yellowfin tuna fishery data from Schaefer 1957}
#'   \item{tasab}{ a matrix of abalone maturity-at-length data}
#'   \item{tigers}{ tiger prawn recruitment data from Penn and Caputi 1986}
#'   \item{twoindex}{ orange roughy catches with hypothetical cpue}
#' }
#' @docType package
#' @name MQMF
#' @keywords internal
"_PACKAGE"
NULL
