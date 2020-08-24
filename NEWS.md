# MQMF 0.1.0.1000 pre-release

This is primarily the correction of a number of typos throughout the help pages after I discovered the excellent "spelling" package on CRAN. There are also a few clarifications in some of the help files.


# MQMF 0.1.0 19 March 2020

First successful submission to CRAN (and release from GitHub). This provides the first full implementation of the functions required to use the book "Using R for Modelling and Quantitative Methods in Fisheries".  ISBN: 9780367469894  Due for publication by CRC Press - Chapman & Hall in June 2020. The examples on the help pages of each function are more tests of each function than worked examples, which is especially the case for the more computer intensive methods (bootstrapping and MCMC). However, while there are currently no vignettes, the example code-chunks from the book are all available as the help pages of functions chapter2, chapter3, ..., and chapter7. That includes the Rcpp code used in the Uncertainty chapter.

# MQMF 0.1.0.900 23-08-2020

I have been re-reading the help pages for each function and fixing up typos as I find them, and extending or clarifying the text in the help pages where I feel it might be helpful. Once finished (soon) I will send in a maintenance update to CRAN.

# MQMF 0.1.0.850 24-08-2020

Further amendments to the help pages within the MQMF functions. Modified plot1 to remove fixed parameters that could be passed via the ellipsis, ..., and modified setpalette in recognition that R4 is fully active so that now it can be used to reset the palette to the old R3 palette, should that be desired. 