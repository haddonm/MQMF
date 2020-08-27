
## Update submission to package MQMF

*  https://CRAN.R-project.org/package=MQMF

* This update is made up of minor revisions and clarifications to a number of the help pages as well as applying the spell_check function from the spelling package.


## Test environments
* local OS Windows 10 Home 2004 OS Build 19041.450
* rhub: Ubuntu Linux 16.04 LTS, R-devel, GCC
* rhub: Ubuntu Linux 16.04 LTS, R-release, GCC
* rhub: Fedora Linux, R-devel, clang, gfortran
* rhub: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* rhub: macOS 10.13.6 High Sierra, R-release, CRAN's setup

## R CMD check results:
OK in all cases, with no Errors, no Warnings, and no Notes


### Comments

* This package complements the book 'Using R for Modelling and Quantitative Methods in Fisheries', which is in the 'Using R' series by CRC Press/Chapman & Hall. It will be formally published (made available) in September 2020. 

* The original submission raised two issues that needed explanation, I repeat those explanations here, just in case:

* There are three functions (parset, plot1, and plotprep) whose objective is explicitly to alter the 'par' settings because they aim to facilitate the use of base graphics functions. These functions return, invisibly, the result of  'oldpar=par(no.readonly=TRUE)' to enable the user to revert their settings should they desire that. The examples, for each function, demonstrate how this can be done.  

* The ch_chunks.R file contains seven functions whose only purpose is to provide detailed worked examples within the help pages for each chapter, so the commenting of code is deliberate. These are more realistic examples rather than just tests and are present to illustrate how the R functions operate when multiple replicates (1000+) or other time-consuming actions are included. In particular, these enable worked examples of some of the more computer-intensive methods to be presented that would grossly breach the time-limits on examples (hence the \dontrun{} options on all the ch_chunks.R function examples).


