
## Resubmission

This is a resubmission. In this version, as directed, I have:


* Deleted the CRAN-RELEASE file; the previous date of submission was 2020-02-22

* Modified the book reference in the DESCRIPTION to include the ISBN, have put quotes around the title, and have simplified the description paragraph. I have not been sent a DOI by the publishers yet.

* I have either removed messages to the console, turned them into warnings, or added a default 'verbose=FALSE' to the flawed functions.

* I have added oldpar = par(no.readonly = TRUE), and later on.exit(par(oldpar)) to all plotting functions, except three (see next point), that used to modify the user's 'par' settings.

* There are three functions (parset, plot1, and plotprep) whose objective is explicitly to alter the 'par' settings because they aim to facilitate the use of base graphics functions. Now, however, these functions return, invisibly, the result of 'oldpar=par(no.readonly=TRUE)' to enable the user to revert their settings should they desire that. The examples, in each case. demonstrate how this can be done.  

* The ch_chunks.R file contains seven functions whose only purpose is explicitly to provide detailed worked examples within the help pages for each chapter, so the commenting of code is deliberate. These are more realistic examples rather than just tests and are present to illustrate how the R functions operate when 1000 replicates or other time-consuming actions are included. In particular, these enable worked examples of some of the more computer-intensive methods to be presented that would grossly breach the time-limits on examples (hence the \dontrun{} options on all). I have also added 'oldpar=par(no.readonly=TRUE)' and 'par(oldpar)' as appropriate to these examples to avoid altering the 'par' settings. 

* I have removed \dontrun{} from everywhere except from ch_chunks.R. Some of the help pages for the computer-intensive methods now act solely as tests rather than examples, but ch_chunks.R covers that. On my machine, in total the examples now take 4.9 seconds combined (according to R CMD check).

## Test environments
* local OS Windows 10 Home 1903 OS Build 18362.657 (using R CMD check on source package)
* macOS 10.11 El Capitan, R-release (experimental) (using check on r-hub builder)
* Ubuntu Linux 16.04 LTS, R-devel, GCC (using check on r-hub builder)
* Ubuntu Linux 16.04 LTS, R-release, GCC (using check on r-hub builder)
* Fedora Linux, R-devel, clang, gfortran (using check on r-hub builder)
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (using check on r-hub builder)


## R CMD check results
There were no ERRORs or WARNINGs or NOTES. 


On rhub::check_for_cran (using Windows Server 2008 R2 SP1, R-devel) there was 1 NOTE identifying I was making a new submission:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Malcolm Haddon <malcolm.haddon@gmail.com>'

New submission


