
## Resubmission

This is a resubmission in response to the email from Jelana Saf on March 2nd. In this version, as directed, I have:


* Deleted the CRAN-RELEASE file; the previous date of submission was 2020-02-22

* Modified the book reference in the DESCRIPTION to include the ISBN, have put quotes around the title, and have simplified the description paragraph. I have not been sent a DOI by the publishers yet. When I including my surname that led to a note about a possibly mispelled word (being my surname; but only on rhub::check_for cran), so I left it out to avoid the problem. I have also changed the version number from 1.0.0 to 0.1.0.

* I have either removed messages to the console, turned them into warnings, or added a default 'verbose=FALSE' to the flawed functions.

* I have added oldpar = par(no.readonly = TRUE), and later on.exit(par(oldpar)) to all plotting functions, except three (see next point), that used to modify the user's 'par' settings.

* There are three functions (parset, plot1, and plotprep) whose objective is explicitly to alter the 'par' settings because they aim to facilitate the use of base graphics functions. Now, however, these functions return, invisibly, the result of 'oldpar=par(no.readonly=TRUE)' to enable the user to revert their settings should they desire that. The examples, in each case. demonstrate how this can be done.  

* The ch_chunks.R file contains seven functions whose only purpose is explicitly to provide detailed worked examples within the help pages for each chapter, so the commenting of code is deliberate. These are more realistic examples rather than just tests and are present to illustrate how the R functions operate when multiple replicates (1000+) or other time-consuming actions are included. In particular, these enable worked examples of some of the more computer-intensive methods to be presented that would grossly breach the time-limits on examples (hence the \dontrun{} options on all). I have also added 'oldpar=par(no.readonly=TRUE)' and 'par(oldpar)' as appropriate to these examples to avoid altering the 'par' settings. 

* I have removed \dontrun{} from everywhere except from ch_chunks.R. Some of the help pages for the computer-intensive methods now act solely as tests rather than examples, but ch_chunks.R covers that. On my machine, in total the examples now take 4.9 seconds combined (according to R CMD check).



## Test environments
* local OS Windows 10 Home 1903 OS Build 18362.657 OK
* rhub: Ubuntu Linux 16.04 LTS, R-devel, GCC OK
* Ubuntu Linux 16.04 LTS, R-release, GCC (1 NOTE)
* Fedora Linux, R-devel, clang, gfortran (1 NOTE)
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (1 NOTE)


* macOS 10.11 El Capitan, R-release (experimental) No longer responded on rhub


### NOTE

On rhub::check_for_cran(using Windows Server 2008 R2 SP1, R-devel, 32/64 bit; Ubuntu Linux 16.04 LTS, R-release, GCC, and Fedora Linux, R-devel, clang, gfortran) there was 1 NOTE  identifying that I was making a new submission (it is actually a re-submission)

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Malcolm Haddon <malcolm.haddon@gmail.com>’

New submission
