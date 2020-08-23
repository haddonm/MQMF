
<!-- README.md is generated from this README.Rmd. Please do not edit that file -->

# MQMF

An R package for the book *Using R for Modelling and Quantitative
Methods in Fisheries* (2020). ISBN: 9780367469894 due to be made
available/published in September 2020.

Hardback and softback versions are available from *www.routledge.com*,
but is also available for pre-order from Amazon (best to use one’s local
Amazon site). A gitbook version is under development and once completed
will at least be available on gitpages. I will annouce here when it is
ready.

The release version of the **MQMF** package (commit 88bc002499), is
available on CRAN, but to install this development version from
[GitHub](https://github.com/) you can use:

``` r
if (!require(devtools)){install.packages("devtools")} 

devtools::install_github("https://github.com/haddonm/MQMF")
```

The development version differs from having more details put into the
help pages, plus various typos and obscure text fixed. No changes have
been made to the code base yet, though various developments are now in
mind to increase robustness and generality of a number of the functions.

There are currently no vignettes, although the example code-chunks from
the book are all available as the help pages of functions chapter2,
chapter3, …, and chapter7.

This new book is an evolution and adaptation of my book *Modelling and
Quantitative Methods in Fisheries* (Haddon, 2011). It is designed to
introduce materials needed to understand the use of R in fisheries and
ecology. The book is not really for total beginners to R, and only a
brief introduciton to a few less commonly used aspects of R is provided.
In terms of fisheries and population ecology, because it covers a wide
array of subjects, and hence is limited in the depth in which each can
be treated, it should probably be considered an introductory text. Even
so, sufficient detail and worked examples are given that anyone should
be able to make a good start with simple model fitting, characterizing
uncertainty, and other basic fisheries model fitting. Subjects such as
maximum likelihood, simple and dynamic model fitting, and the estimation
of uncertainty do receive relatively detailed attention. For example,
there is a sufficient development around the use of MCMC that anyone
should be able to develop a better understanding of its strengths and
weaknesses. More advanced subjects such as age- and size-structured
models are not included here as they are not really suited to brief
treatments.

The primary objective of this book is that it be useful to workers in
the field. Hopefully, this R package will assist with that objective.

Malcolm Haddon

Hobart, March 10, 2020

## 

Haddon, M. (2011) *Modelling and Quantitative Methods in Fisheries*. 2nd
Ed. CRC/Chapman & Hall. 449p.

Haddon, M. (2020) *Using R for Modelling and Quantitative Methods in
Fisheries*. CRC/Chapman & Hall. In Press
