
## Update submission to package MQMF

*  https://CRAN.R-project.org/package=MQMF

* This update is made up of minor revisions and clarifications to a number of the help pages as well as applying the spell_check function from the spelling package.


## Test environments on Wed 26th August
* local OS Windows 10 Home 2004 OS Build 19041.450
* rhub: Ubuntu Linux 16.04 LTS, R-devel, GCC
* rhub: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* rhub: macOS 10.13.6 High Sierra, R-release, CRAN's setup

## R CMD check results:
OK in all these cases, with no Errors, no Warnings, and no Notes


### However: 
Since those first checks, with 
Fedora Linux, R-devel, GCC, and then again with  
Ubuntu Linux 16.04 LTS, R-devel, GCC

I received a Note:

* checking for future file timestamps ... NOTE
unable to verify current time

I have tried to find the source of the note and failed but noticed a tread in R-package-devel, from Fri 28th August  suggesting this was a wider problem which related to an external source rather than R or the package under development, hence my current submission. 

On 2020-08-27 2:17 p.m., Duncan Murdoch wrote:
> R tries to get the time from
>
> http://worldtimeapi.org/api/timezone/UTC or
> http://worldclockapi.com/api/json/utc/now
>
> The first one doesn't accept UTC as a timezone; it appears to want
> etc/UTC instead.  The second one is offline.
>
> Duncan Murdoch
>
> If both of those fail, you'll get the message you saw.
>
> On 27/08/2020 1:23 p.m., John Fox wrote:
>> Dear r-package-devel list members,
>>
>> I got the following note when checking two different packages today
>> --as-cran, both under R 4.0.2 and under R-devel, on my Windows 10 and
>> macOS Catalina systems, and on several platforms on rhub:
>>
>>     * checking for future file timestamps ... NOTE
>> unable to verify current time
>>
>> I'm writing to inquire about the note because no one else has mentioned
>> this problem recently, in case it's produced by something that I'm
>> doing. There is a discussion of a similar problem from 2019 at
>> <https://stat.ethz.ch/pipermail/r-package-devel/2019q1/003577.html>.
>>
>> Both packages that I was checking are close to CRAN releases and so I'd
>> like to know whether I can disregard the note.

###Other Comments

* This package complements the book 'Using R for Modelling and Quantitative Methods in Fisheries', which is in the 'Using R' series by CRC Press/Chapman & Hall. It will be formally published (made available) in September 2020. 

* The original submission raised two issues that needed explanation, I repeat those explanations here, just in case:

* There are three functions (parset, plot1, and plotprep) whose objective is explicitly to alter the 'par' settings because they aim to facilitate the use of base graphics functions. These functions return, invisibly, the result of  'oldpar=par(no.readonly=TRUE)' to enable the user to revert their settings should they desire that. The examples, for each function, demonstrate how this can be done.  

* The ch_chunks.R file contains seven functions whose only purpose is to provide detailed worked examples within the help pages for each chapter, so the commenting of code is deliberate. These are more realistic examples rather than just tests and are present to illustrate how the R functions operate when multiple replicates (1000+) or other time-consuming actions are included. In particular, these enable worked examples of some of the more computer-intensive methods to be presented that would grossly breach the time-limits on examples (hence the \dontrun{} options on all the ch_chunks.R function examples).


