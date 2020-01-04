# MQMF
An R package for the book _Using R for Modelling and Quantitative Methods in Fisheries_.

To install from github it is most simple from inside RStudio using:

if (!require(devtools)){install.packages("devtools")} 

devtools::install_github("https://github.com/haddonm/MQMF",build_vignettes=TRUE)

This new book is an evolution and adaptation my book _Modelling and Quantitative Methods in Fisheries_ (Haddon, 2011). It is designed to introduce materials needed to understand the use of R in fisheries and ecology. The book is not really for beginners to R, but remains an introductory text in fisheries because it covers a wide array of subjects and hence is limited in the depth in which each can be treated. Despite the breath of material, sufficient detail and worked examples are given that anyone should be able to make a good start with model fitting, characterizing uncertainty, and other basic fisheries model fitting. More advanced subjects such as age- and size-structured models are not included as they are both very large subjects and are not suited to brief treatments. Nevertheless, subjects such as maximum likelihood, model fitting, and the estimation of uncertainty do receive relatively detailed attention.

This R package is almost ready to be submitted to CRAN, and currently has no errors, no warnings, and no notes, and passes the tests used on rhub.

Malcolm
January 02 2020

Haddon, M. (2011) _Modelling and Quantitative Methods in Fisheries_. 2nd Ed. CRC/Chapman & Hall. 449p.
Haddon, M. (2020) _Using R for Modelling and Quantitative Methods in Fisheries_. CRC/Chapman & Hall. In Press
