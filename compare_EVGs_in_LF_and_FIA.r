# compare number of pixels in each EVG in Landfire c2014 to number of FIA plots in that EVG
# written by Karin Riley, 2/6/19

library(foreign)

# find the number of pixels in each EVG for Landfire c2014 (where pixels is considered forested as per EVC 101-109)
lf <- read.dbf("E:\\Tree_List_c2014\\target_data\\working_KLR\\forest_mask\\EVT_masked_to_EVC_forest_pixels.tif.vat.dbf")

lfevglist <- sort(unique(lf$EVT_GP))
lfevglist2 <- lfevglist[5:84]  # subset EVGs to exclude developed and agricultural

lfpixel <- as.numeric()
for (j in 1:length(lfevglist2))
{
  rownums <- which(lf$EVT_GP==lfevglist2[j])
  lfpixel[j] <- sum(lf$COUNT[rownums])
}
outmat <- cbind(lfevglist2, lfpixel)
write.table(outmat, "E:\\Tree_List_c2014\\EVG_analysis\\EVG_counts_LF2014.txt")

# find mangrove plots (SPCD=986, 987, 988, 989, 6266, 6268, 6709, 8463, 8639)

# necessary steps to get R connected to Access 2016:
# download and install 32-bit version of R (3.5.1 had both 32- and 64-bit within it)
# switch R studio to 32-bit version (Tools-->Global options-->R version-->Change) --remember to switch back when done with this session
# search on Windows for "Set up ODBC data sources (32 bit)" (opens a GUI)
# Go to System DSN-->Add
# Choose Driver do Microsoft Access (*.mdb, *.accdb) --> Finish
# Data source name: AllPlots
# Description: AllPlots
# Make sure to actually select the database > OK

# now the code works, but I'll have to change the ODBC data source in the GUI for each state database. What a pain!!

# set up connection to database with info on all plots in all Auto-Key regions
# ("E:\\_LANDFIRE_AutoKey\\AutoKey_CONUS\\AutoKey_InputTemplates\\working_KLR\\AutoKeyInputs_AllPlots.accdb")

library(RODBC)

options("scipen"=100, "digits"=15)

# connect to database and table that has the autokey region assigned to each plot
channel <- odbcConnect("National_plots")
regions <- sqlFetch(channel, "National TREE")

rownums <- which(regions$SPCD==986 | regions$SPCD==987 | regions$SPCD==988 | regions$SPCD==989 | regions$SPCD==6266 | regions$SPCD==6268 | regions$SPCD==6709 | regions$SPCD==8463 | regions$SPCD==8639)
submat <- regions[rownums,]

mangroveplots <- sort(unique(submat$PLT_CN)) # 17 plots have mangroves present
#  [1]  43854644020004  43855021020004 218218760020004 218218898020004 218219187020004 218219188020004 249673916010854
# [8] 249674369010854 249674493010854 249674629010854 259178744010854 259179148010854 259179189010854 289515223489998
# [15] 289515904489998 289516210489998 289516296489998
