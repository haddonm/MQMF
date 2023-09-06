
## Update submission to package MQMF

*  https://CRAN.R-project.org/package=MQMF

* This update is in response to an email from Kurt Hornik  on 19 August 2023 about a change in Rmarkdown with @docType no longer automatically adding an \alias in the
package help file. I have fixed this using the "_PACKAGE" sentinal. In addition, I have taken the opportunity to correct numerous small typos, 
clarify the usage of some of the more complex functions, and add usage instructions to the six 'chapter#' functions. 


## Test environments on Wed 26th August
* local OS Windows 11 Pro OS Build 22621 (10 x64)

Local Checks

## ── R CMD check results ── MQMF 0.1.5 ────
Duration: 30.8s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## R CMD check(remote=TRUE, manual=TRUE) results MQMF 0.1.5
Duration: 1m 33s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔


## Using devtools::check_win_devel() on 6/Sept/23
* Check time in seconds: 73
* Status: OK
* R Under development (unstable) (2023-09-04 r85066 ucrt)

## Using:  devtools::check_mac_release()
*  r-release-macosx-arm64|4.3.0|macosx|macOS 13.3.1 (22E261)
*  Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 12.2.0
*  checked OK


## Using rhub::check_for_cran() 
* Fedora Linux, R-devel, clang, gfortran
  GNU Fortran (GCC) 12.2.1 20221121 (Red Hat 12.2.1-4)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC  
* {"status":"ok"}
  Finished: SUCCESS

### However: 
with the rhub checks, both reported a Note:

* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found

This initially occurred in my local checks until I installed Rtools 4.3, after which it disappeared.
I concluded this is not something I can amend of fix and hence, continued with my submission.
This not did not occur in the other test arrangements.


###Other Comments

* This package complements the book 'Using R for Modelling and Quantitative Methods in Fisheries', which is in the 'Using R' series by CRC Press/Chapman & Hall. It was formally published in 2021. 

* The original submission raised two issues that needed explanation, I repeat those explanations here, just in case:

* There are three functions (parset, plot1, and plotprep) whose objective is explicitly to alter the 'par' settings because they aim to facilitate the use of base graphics functions. These functions return, invisibly, the result of  'oldpar=par(no.readonly=TRUE)' to enable the user to revert their settings should they desire that. The examples, for each function, demonstrate how this can be done.  

* The ch_chunks.R file contains seven functions whose only purpose is to provide detailed worked examples within the help pages for each chapter, so the commenting of code is deliberate. These are more realistic examples rather than just tests and are present to illustrate how the R functions operate when multiple replicates (1000+) or other time-consuming actions are included. In particular, these enable worked examples of some of the more computer-intensive methods to be presented that would grossly breach the time-limits on examples (hence the \dontrun{} options on all the ch_chunks.R function examples).


