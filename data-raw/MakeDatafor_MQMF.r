#rm(list=ls())   # Cleanup the R console if required
# Set up the run ----------


options("show.signif.stars"=FALSE,"stringsAsFactors"=FALSE,
        "max.print"=50000,"width"=240)
# dataTemplate("C:/Users/had06a/Dropbox/A_Book/rcode/agestruct/data/westroughy2.csv",
#              title="Western Orange Roughy Data")
#     listFunctions("C:/A_Mal/A_Book/rcode/agestruct/age_utils.r")

# abdat abalone fishery data-------------------------------------

year <- 1985:2008
catch <- c(1020,743,867,724,586,532,567,609,548,498,480,424,655,
494,644,960,938,911,955,936,941,954,1027,980)
cpue <- c(1.0000,1.0957,1.1303,1.1466,1.1873,1.2018,1.2652,1.3199,
          1.4284,1.4772,1.6853,1.9195,2.0515,2.1244,2.2148,2.2531,
          2.1051,2.0820,2.0087,1.9226,1.8703,1.8776,1.8496,1.7271)           

abdat <- as.data.frame(cbind(year,catch,cpue))
save(abdat,file="data-raw/abdat.RData")

# blackisland abalone tagging data ------------------------------

infile <- "C:/Users/User/Dropbox/rcode/MQMF/data-raw/blackisland.csv"
blackisland <- read.csv(infile,header=TRUE)
blackisland <- droplevels(blackisland[,-c(1,2,3,5,7)])
colnames(blackisland) <- c("dt","l1","l2","dl")

pick <- which(blackisland$dl < 0)
blackisland[pick,]
blackisland[pick[1],"l2"] <- 155; blackisland[pick[1],"dl"] <- 0
blackisland[pick[2],"l2"] <- 169; blackisland[pick[2],"dl"] <- 0
blackisland[pick,]

filename <- "C:/Users/User/Dropbox/rcode/MQMF/data-raw/blackisland.RData"
save(blackisland,file=filename)

# dataspm pink ling west data -----------------------------------

filename <- "data-raw/dataspm.csv"
dataspm <- read.csv(filename,header=TRUE)
str(dataspm)
save(dataspm,file="data-raw/dataspm.RData")

# LatA redfish length-at-age data--------------------------------
filenameLAA <- "data-raw/LatA.csv"
#' This is a copy of REDCAF.csv from
#' 'C:/A_CSIRO/Rcode/SESSF/adhoc/redfish/' and contains all redfish
#' ageing data by zone and port of landing/area of capture

LatA <- read.csv(filenameLAA,header=TRUE)
dim(LatA)
head(LatA,20)
save(LatA,file="data-raw/LatA.RData")


# minnow seasonal growth data Pitcher and Macdonald 1973 --------
getfile <- filename <- "C:/Users/user/Dropbox/rcode/MQMF/data-raw/minnow.csv"
minnow <- read.csv(getfile,header=TRUE)
minnow
class(minnow)

filename <- "C:/Users/user/Dropbox/rcode/MQMF/data-raw/minnow.RData"

save(minnow,file=filename)


# npf prawn catch data-------------------------------------------

filename <- "C:/Users/Malcolm/Dropbox/rcode/MQMF/data-raw/npf.csv"
npf <- read.csv(filename,header=TRUE)
class(npf)
npf

save(npf,file="C:/Users/User/Dropbox/rcode/MQMF/data-raw/npf.RData")



# Schaef  Schaefer  Yellowfin Tuna ------------------------------

filename <- "C:/Users/user/Dropbox/rcode/MQMF/data-raw/schaef.csv"
schaef <- read.csv(filename,header=TRUE)
class(schaef)
schaef

 data(schaef)
 pars <- log(c(r=0.2,K=2.1e06,Binit=2.2e06,sigma=0.2))
 answer <- fitSPM(pars,schaef,schaefer=TRUE,maxiter=1000)
 outfit(answer,title="Schaefer, 1957 Data",digits=4)

save(schaef,file="C:/Users/user/Dropbox/rcode/MQMF/data-raw/schaef.RData")

# tasab maturity data -------------------------------------------

infile <- "C:/Users/user/Dropbox/rcode/MQMF/data-raw/tasw.csv"
tasab <- read.csv(infile,header=TRUE)
head(tasab)

tasab <- tasab[order(tasab$site,tasab$length),]

colnames(tasab) <- c("site","sex","length","mature")

filename <- "C:/Users/user/Dropbox/rcode/MQMF/data-raw/tasab.RData"
save(tasab,file=filename)


# tigers  prawn stock recruitment data --------------------------
Spawn <- c(2.4,3.2,3.9,5.7,6.0,7.4,8.2,10.0,10.1,10.4,11.3,
           12.8,18.0,24.0)
Recruit <- c(11.6,7.1,14.3,19.1,12.4,19.7,37.5,18.4,22.1,26.9,
             19.2,21.0,9.9,26.8)
tigers <- as.data.frame(cbind(Spawn=Spawn,Recruit=Recruit))
rownames(tigers) <- 1:14
tigers

save(tigers,file="data-raw/tigers.RData")

# pellatomlinson alternative yellowfin tuna data ----------------
filename <- "C:/Users/Malcolm/Dropbox/rcode/MQMF/data-raw/pellatomlinson.csv"
pttuna <- read.csv(filename,header=TRUE)

plotprep(width=7,height=5,newdev=FALSE)
plot1(pttuna$year,pttuna$cpue)


 pars <- log(c(r=0.25,K=2.1e06,Binit=2.2e06,sigma=0.2))
 answer <- fitSPM(pars,pttuna,schaefer=TRUE,maxiter=1000,steptol = 1e-05)
 outfit(answer,title="Pella-Tomlinson Data",digits=4)
 
 ans <- plotmodel(inp=answer$estimate,indat=pttuna,addrmse=TRUE)

 save(pttuna,file="C:/Users/Malcolm/Dropbox/rcode/MQMF/data-raw/pttuna.RData")
 
 
# check and re-store data ---------------------------------------
tools::checkRdaFiles(paths="C:/Users/user/DropBox/rcode/MQMF/data-raw")
tools::resaveRdaFiles(paths="C:/Users/User/DropBox/rcode/MQMF/data-raw",compress="auto")
tools::checkRdaFiles(paths="C:/Users/User/DropBox/rcode/MQMF/data-raw")

# l---------------------------------------------------------------

# Orange Roughy West -----------
# now called trawldeep_scalefish
filename="westroughy.csv"
fishdat <- readdata(filename)
str(fishdat)

save(fishdat,file="data/fishdat.RData")

# RRP -----------
# now called trawlcaughy_invertebrate
# For MQMF
filename <- "RRPfishdata.csv"
invert <- read.csv(filename,header=TRUE)
invert
save(invert,file="data/invert.RData")

# plaice ------------
filename <- "plaice.csv"
plaice <- readdata(filename)
str(plaice)
save(plaice,file="data/plaice.RData")

# Pacific Hake Kiumura 1980 ------------------------
age <- c(1,2,3.3,4.3,5.3,6.3,7.3,8.3,9.3,10.3,11.3)
length <- c(15.4,26.93,42.23,44.59,47.63,49.67,50.87,
            52.3,54.77,56.43,55.88)
kimura <- as.data.frame(cbind(age,length))
colnames(kimura) <- c("age","length")
rownames(kimura) <- 1:11
kimura
filename <- "data/kimura.RData"
save(kimura,file=filename)

# LatA -------------
filenameLAA <- "data-raw/redfish_LAA_data_from_CAF_forFMR.csv"
#' This is a copy of REDCAF.csv from
#' 'C:/A_CSIRO/Rcode/SESSF/adhoc/redfish/' and contains all redfish
#' ageing data by zone and port of landing/area of capture

dat <- read.csv(filenameLAA,header=TRUE)
head(dat,20)

table(dat$AreaCapture,dat$Zone)

duplicates <- c("Ulladulla","Ulladulla,NSW")
pick <- which(dat$AreaCapture %in% duplicates)
if (length(pick) > 0) dat$AreaCapture[pick] <- "Ulladulla"

table(dat$AreaCapture,dat$Zone)

pick <- which((dat$AreaCapture == "Ulladulla") &
                 (dat$Sex < 3) & (dat$Age > -1) & (dat$Length > 0))
length(pick)  # should be 5543

dat1 <- droplevels(dat[pick,])

table(dat1$Year,dat1$Sex)

# select data for the package

pickdat <- which((dat$AreaCapture == "Ulladulla") &
                    (dat$Sex < 3) & ((dat$Age > -1) & (dat$Length > 0)) &
                    (dat$Year == 1997))
length(pickdat)  # should be 576
pickcols <- c(8,9,11)
laa <- droplevels(dat[pickdat,pickcols])
head(laa,20)
r4cpue::properties(laa)
LatA <- laa[order(laa$Sex,laa$Age),]


save(LatA,file="data/LatA.RData")



#  check data ---------------
tools::checkRdaFiles(paths="C:/A_Mal/Rcode/MQMF/data-raw")
tools::resaveRdaFiles(paths="C:/A_Mal/Rcode/MQMF/data-raw",compress="auto")
tools::checkRdaFiles(paths="C:/A_Mal/Rcode/MQMF/data-raw")








# west pink ling age data --------------------------------------------------
# try to get age data for Western Pink Ling
filename <- "C:/A_CSIRO/Rcode/simpleSA_use/CAF16in17.csv"
dat <- read.csv(filename,header=TRUE,"stringsAsFactors"=FALSE)
dim(dat)

pick <- which((dat$CAAB == 37228002) & (dat$Year > 1986))# Pink Ling CAAB code
lig <- droplevels(dat[pick,])
head(lig,20)
table(lig$Year,lig$Sex)
table(lig$Length)
table(lig$Age1,lig$Age2)
lig$Sex <- as.numeric(lig$Sex)
#  NOTE sexes in ages, from the CAF are 1 = male 2 - female
#  BUT  sexes in SS3 are                2 = male 1 - female
pick <- which((lig$Sex < 3) & (lig$Length > 0) & (lig$Age1 >= 0) &
              (lig$AgeZone %in% c("SF40","SF50")))
lig <- droplevels(lig[pick,])
r4cpue::properties(lig)
lig$Age1 <- as.numeric(lig$Age1)
plotprep()
plot(lig$Age1,lig$Length,type="p")
table(lig$Year,lig$AgeZone)

caa <- as.matrix(table(lig$Year,lig$Age1))
tot <- rowSums(caa)
pickN <- which(tot > 200)
cbind(caa[pickN,],tot[pickN])

filename <- "C:/Users/user/Dropbox/rcode/simpleSA_use/makedatasimpleSA/PinkLingW_Report.sso"
dat <- readLines(filename)
pick <- grep("X",dat)
dat[pick[1:56]]
grep("TIME_SERIES",dat)
dat[1019:1070]

titles <- removeEmpty(unlist(strsplit(dat[1020]," ")))
numcol <- length(titles)
timeser <- as.data.frame(matrix(NA,nrow=49,ncol=numcol))
colnames(timeser) <- titles

pickL <- 1021:1069
numrow <- length(pickL)

for (i in 1:numrow) {
   oneline <- removeEmpty(unlist(strsplit(dat[pickL[i]]," ")))
   timeser[i,] <- oneline
}
head(timeser)
properties(timeser)
for (i in c(1,2,4:30)) timeser[,i] <- as.numeric(timeser[,i])
timeser[18:49,c("Yr","obs_cat:_1","obs_cat:_2")]





# abalone tagging data ------------------------------------------------

infile <- "C:/Users/User/Dropbox/rcode/MQMF/data-raw/blackisland.csv"
blackisland <- read.csv(infile,header=TRUE)
blackisland <- droplevels(blackisland[,-c(1,2,3)])

colnames(blackisland) <- c("deltat","time1","len1","time2","len2",
                           "deltal")
filename <- "C:/Users/User/Dropbox/rcode/MQMF/data-raw/blackisland.RData"
save(blackisland,file=filename)

# abalone modal progression ------------------------------------------
# for May 18 1994

infile <- "C:/Users/User/Dropbox/rcode/MQMF/data-raw/hopeisland940518.csv"
hopeisland <- read.csv(infile,header=TRUE)

ab <- trunc(hopeisland[,1]/2) * 2



plotprep(cex=0.9,newdev=FALSE)
outh <- inthist(ab,col="red",border="black",width=1.5,xaxis=TRUE,inc=2,
                xlabel="Shell Length mm",ylabel="Frequency")




# fit logistic ----------------------------
p <- tapply(tasab$mature,tasab$length,mean)
Lp <- as.numeric(names(p))

plotprep(width=7,height=4,newdev=FALSE)
plot1(Lp,p,type="p",cex=1.0,xlabel="Length mm",
      ylabel="Proportion Mature")

L <- seq(min(tasab$length),max(tasab$length),1)

tasab$site <- as.factor(tasab$site)

model <- glm(tasab$mature ~ tasab$length,binomial)
summary(model)
a <- model$coef[1]
b <- model$coef[2]
Lm50 <- -a/b
interQ <- 2*log(3)/b
cat(a,b,Lm50,interQ,"\n")
y <- (exp(a + b * L)/(1+exp(a + b * L)))
lines(L,y,type="l",lwd=2,col=2)
abline(v=c(Lm50,(Lm50-interQ/2),(Lm50+interQ/2)),col=c(2,3,3),lwd=c(2,1,1))
abline(h=c(0.25,0.5,0.75),lwd=1,col=1,lty=3)













