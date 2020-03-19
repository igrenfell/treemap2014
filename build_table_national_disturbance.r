<<<<<<< HEAD
# build table with national disturbance data
# written by Karin Riley, 4/17/2019

library(foreign)

zonenums = c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,98,99)

fireacc <- as.numeric()
insectacc <- as.numeric()
firepltnum <- as.numeric()
inscpltnum <- as.numeric()
firepltpct <- as.numeric()
inscpltpct <- as.numeric()
for (j in 1:length(zonenums))
{
  inmat <- paste("F:\\Tree_List_c2014\\outputs\\z", zonenums[j], "_disturb\\z", zonenums[j], "_disturb_code_plotcount_vs_accuracy.txt", sep="")
  dc <- read.table(inmat, header=T)
  fireacc[j] <- dc[2,3]
  insectacc[j] <- dc[3,3]
  firepltnum[j] <- dc[2,2]
  inscpltnum[j] <- dc[3,2]
  firepltpct[j] <- firepltnum[j]/sum(dc[,2], na.rm=T)
  inscpltpct[j] <- inscpltnum[j]/sum(dc[,2], na.rm=T)
}

firelfpct <- as.numeric()
insclfpct <- rep(NA, 67)
for (j in 1:length(zonenums))
{
  inmat <- paste("F:\\Tree_List_c2014\\target_data\\final\\z", zonenums[j], "\\disturb_code.tif.vat.dbf", sep="")
  dclf <- read.dbf(inmat)
  firerow <- which(dclf$VALUE==1)
  insectrow <- which(dclf$VALUE==2)
  firelfpct[j] <- dclf[firerow,2]/sum(dclf[,2], na.rm=T)
  if (length(insectrow>0)) { insclfpct[j] <- dclf[insectrow,2]/sum(dclf[,2], na.rm=T) }
}

outmat <- cbind(zonenums, fireacc, insectacc, firepltnum, inscpltnum, firepltpct, inscpltpct, firelfpct, insclfpct)
write.table(outmat, "F:\\Tree_List_c2014\\outputs\\national\\national_disturbance_stats.txt")
=======
# build table with national disturbance data
# written by Karin Riley, 4/17/2019

library(foreign)

zonenums = c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,98,99)

fireacc <- as.numeric()
insectacc <- as.numeric()
firepltnum <- as.numeric()
inscpltnum <- as.numeric()
firepltpct <- as.numeric()
inscpltpct <- as.numeric()
for (j in 1:length(zonenums))
{
  inmat <- paste("F:\\Tree_List_c2014\\outputs\\z", zonenums[j], "_disturb\\z", zonenums[j], "_disturb_code_plotcount_vs_accuracy.txt", sep="")
  dc <- read.table(inmat, header=T)
  fireacc[j] <- dc[2,3]
  insectacc[j] <- dc[3,3]
  firepltnum[j] <- dc[2,2]
  inscpltnum[j] <- dc[3,2]
  firepltpct[j] <- firepltnum[j]/sum(dc[,2], na.rm=T)
  inscpltpct[j] <- inscpltnum[j]/sum(dc[,2], na.rm=T)
}

firelfpct <- as.numeric()
insclfpct <- rep(NA, 67)
for (j in 1:length(zonenums))
{
  inmat <- paste("F:\\Tree_List_c2014\\target_data\\final\\z", zonenums[j], "\\disturb_code.tif.vat.dbf", sep="")
  dclf <- read.dbf(inmat)
  firerow <- which(dclf$VALUE==1)
  insectrow <- which(dclf$VALUE==2)
  firelfpct[j] <- dclf[firerow,2]/sum(dclf[,2], na.rm=T)
  if (length(insectrow>0)) { insclfpct[j] <- dclf[insectrow,2]/sum(dclf[,2], na.rm=T) }
}

outmat <- cbind(zonenums, fireacc, insectacc, firepltnum, inscpltnum, firepltpct, inscpltpct, firelfpct, insclfpct)
write.table(outmat, "F:\\Tree_List_c2014\\outputs\\national\\national_disturbance_stats.txt")
>>>>>>> a37b3d83e84e16242c5ecbdbe5aa48f0e35ecee5
