#rm(list=ls())   # Cleanup the R console if required
# Set up the run ----------
library(humbleSA)

options("show.signif.stars"=FALSE,"stringsAsFactors"=FALSE,
        "max.print"=50000,"width"=240)
# dataTemplate("C:/Users/had06a/Dropbox/A_Book/rcode/agestruct/data/westroughy2.csv",
#              title="Western Orange Roughy Data")
#     listFunctions("C:/A_Mal/A_Book/rcode/agestruct/age_utils.r")

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

# tigers ------------
Spawn <- c(2.4,3.2,3.9,5.7,6.0,7.4,8.2,10.0,10.1,10.4,11.3,
           12.8,18.0,24.0)
Recruit <- c(11.6,7.1,14.3,19.1,12.4,19.7,37.5,18.4,22.1,26.9,
             19.2,21.0,9.9,26.8)
tigers <- as.data.frame(cbind(Spawn=Spawn,Recruit=Recruit))
rownames(tigers) <- 1:14
tigers

save(tigers,file="data/tigers.RData")

# LatA -------------
filenameLAA <- "data-raw/redfish_LAA_data_from_CAF_forFMR.csv"
#' This is a copy of REDCAF.csv from
#' 'C:/A_CSIRO/Rcode/SESSF/adhoc/redfish/' and contains all redfish
#' ageing data by zone and port of landing/area of capture

dat <- read.csv(filenameLAA,header=TRUE)
head(dat,20)

table(dat$AreaCapture,dat$Zone)

# label <- sort(unique(dat$AreaCapture))
# dat$AreaCapture <- as.character(dat$AreaCapture)
# duplicates <- c("GreenwellPoint","Greenwellpoint","GreenwellPoint,NSW")
# pick <- which(dat$AreaCapture %in% duplicates)
# if (length(pick) > 0) dat$AreaCapture[pick] <- "GreenwellPoint"
# duplicates <- c("Eden","Eden,NSW")
# pick <- which(dat$AreaCapture %in% duplicates)
# if (length(pick) > 0) dat$AreaCapture[pick] <- "Eden"
# duplicates <- c("Bermagui","Bermagui,NSW")
# pick <- which(dat$AreaCapture %in% duplicates)
# if (length(pick) > 0) dat$AreaCapture[pick] <- "Bermagui"
# duplicates <- c("NewSouthWales","NSW")
# pick <- which(dat$AreaCapture %in% duplicates)
# if (length(pick) > 0) dat$AreaCapture[pick] <- "NSW"
# duplicates <- c("Tathra","Tathra,NSW")
# pick <- which(dat$AreaCapture %in% duplicates)
# if (length(pick) > 0) dat$AreaCapture[pick] <- "Tathra"
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


# Schaefer Yellowfin Tuna --------------------------------------------------

filename <- "C:/A_Mal/Rcode/MQMF/data-raw/schaef.csv"
schaef <- read.csv(filename,header=TRUE)
class(schaef)
schaef

save(schaef,file="C:/A_Mal/Rcode/MQMF/data-raw/schaef.RData")

# npfprawn -----------------------------------------------------------------

filename <- "C:/A_Mal/Rcode/MQMF/data-raw/npf.csv"
npf <- read.csv(filename,header=TRUE)
class(npf)
npf

save(npf,file="C:/A_Mal/Rcode/MQMF/data-raw/npf.RData")




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

filename <- "C:/Users/had06a/Dropbox/rcode/simpleSA_use/makedatasimpleSA/PinkLingW_Report.sso"
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
















