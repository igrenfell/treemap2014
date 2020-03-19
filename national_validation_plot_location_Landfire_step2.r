# national-scale validation for tree list c2014
# written by Karin Riley, 10/28/2019

library (foreign)

# compare characteristics of plot location to imputed data c2014
combine = read.dbf("F:\\Tree_List_c2014\\validation\\with_plot_location\\X-table_INVYR2014_35mbuffer_treelist_natdist_combine.tif.vat.dbf")
names(combine)
length(unique(combine$national_y)) # 11860
length(unique(combine$X_table_fi)) #8741
# of 9322 FIA plots in INVYR=2014, 8741 (93.8%) had at least one forested pixel with cell center within 34.75m (the plot radius)
# some FIA plot locations had more than one plot imputed to the pixels within that radius

# read in x table for imputation
xtable <- read.table("F:\\Tree_List_c2014\\x_table\\x_table_final_EVG_Karin_reclass_plus_loblolly_manual.txt", sep=",", header=T)

anymatchvec <- as.numeric()
plotlist <- sort(unique(combine$X_table_fi))
covanymatchvec <- as.numeric()
weightcovvec <- as.numeric()
htanymatchvec <- as.numeric()
weighthtvec5 <- as.numeric()
weighthtvec10 <- as.numeric()
evganymatchvec <- as.numeric()
for (j in 1:length(plotlist))
{
  curmat <- combine[(combine$X_table_fi==plotlist[j]),]
  # see if plot CN imputed to any pixels within that plot's radius
  anymatch <- which(curmat$national_y==curmat$X_table_fi)
  if (length(anymatch)==0) { anymatchvec[j] <- 0 }
  if (length(anymatch)>0)  { anymatchvec[j] <- 1 }
  # see if cover matches...
  currownum <- which(xtable$ID==plotlist[j])
  currow <- xtable[currownum,]
  plotcov <- currow$canopy_cover
  #...in any pixels or weighted value of all pixels within 35m range (within 5%)
  numimpplot <- dim(curmat)[[1]]
  covmatchcheck <- as.logical()
  pixelccvec <- as.numeric()
  for (k in 1:numimpplot)
  {
    curplot <- curmat$national_y[k]
    curplotrownum <- which(xtable$ID==curplot)
    curplotrow <- xtable[curplotrownum,]
    covmatchcheck[k] <- curplotrow$canopy_cover==plotcov
    pixelccvec[k] <- curplotrow$canopy_cover
  }
  if (sum(covmatchcheck)==0) { covanymatchvec[j]=0 }
  if (sum(covmatchcheck)>0) { covanymatchvec[j]=1 }
  weightedcover <- sum((pixelccvec * curmat$Count))/sum(curmat$Count)
  if (abs(plotcov-weightedcover)<=10) { weightcovvec[j] <- 1 }
  if (abs(plotcov-weightedcover)>10) { weightcovvec[j] <- 0 }
  # see if height matches in any pixels
  plotheight <- currow$canopy_height
  htmatchcheck <- as.logical()
  pixelhtvec <- as.logical()
  for (k in 1:numimpplot)
  {
    curplot <- curmat$national_y[k]
    curplotrownum <- which(xtable$ID==curplot)
    curplotrow <- xtable[curplotrownum,]
    htmatchcheck[k] <- curplotrow$canopy_height==plotheight
    pixelhtvec[k] <- curplotrow$canopy_height
  }
  if (sum(htmatchcheck)==0) { htanymatchvec[j]=0 }
  if (sum(htmatchcheck)>0) { htanymatchvec[j]=1 }
  weightedheight <- sum((pixelhtvec * curmat$Count))/sum(curmat$Count)
  if (abs(plotheight-weightedheight)<=10) { weighthtvec10[j] <- 1 }
  if (abs(plotheight-weightedheight)>10) { weighthtvec10[j] <- 0 }
  if (abs(plotheight-weightedheight)<=5) { weighthtvec5[j] <- 1 }
  if (abs(plotheight-weightedheight)>5) { weighthtvec5[j] <- 0 }
  # see if EVG matches in any pixels
  plotevg <- currow$EVT_GP
  evgmatchcheck <- as.logical()
  pixelevgvec <- as.logical()
  for (k in 1:numimpplot)
  {
    curplot <- curmat$national_y[k]
    curplotrownum <- which(xtable$ID==curplot)
    curplotrow <- xtable[curplotrownum,]
    evgmatchcheck[k] <- curplotrow$EVT_GP==plotevg
    pixelevgvec[k] <- curplotrow$EVT_GP
  }
  if (sum(evgmatchcheck)==0) { evganymatchvec[j]=0 }
  if (sum(evgmatchcheck)>0) { evganymatchvec[j]=1 }
}
sum(anymatchvec) # of 9322 plots, at least one pixel within a 35m radius was imputed with that value in 1490 cases (16.0%)
sum(covanymatchvec) # of 9322 plots, the cover of at least one pixel within a 35m radius matched the plot value in 3755 cases (40.3%)
sum(weightcovvec) # of 9322 plots, the weighted cover of pixels within a 35m radius is within 5% of the plot value in 2681 cases (28.8%)
sum(htanymatchvec) # of 9322 plots, the height of at least one pixel within a 35m radius matched the plot value in 7124 cases (76.4%)
sum(weighthtvec5) # of 9322 plots, the weighted height of pixels within a 35m radius is within 5m of the plot value in 6482 cases (69.5%)
sum(weighthtvec10) # of 9322 plots, the weighted height of pixels within a 35m radius is within 10m of the plot value in 7436 cases (79.8%)
sum(evganymatchvec) # of 9322 plots, the EVG of at least one pixel within a 35m radius matched the plot value in 5363 cases (57.5%)

# if the standard is relaxed to being within 10% of the cover value, then 5079 plots match (54.5%)


# using plot centroid as metric only
plotcent <- read.dbf("F:\\Tree_List_c2014\\validation\\with_plot_location\\X_table_INVYR2014_natyesdist.dbf")
length(which(plotcent$RASTERVALU==-9999))
sum(plotcent$ID==plotcent$RASTERVALU) # 804 of 9322 plots (8.6%) imputed to the pixel where the centroid is located
covermatch <- as.numeric()
heightmatch <- as.numeric()
evgmatch <- as.numeric()
for (j in 1:dim(plotcent)[[1]])
{
  currownum <- which(xtable$ID==plotcent$ID[j])
  plotchar <- xtable[currownum,]
  if (plotcent$RASTERVALU[j]==-9999) {
    covermatch[j] <- 0
    heightmatch[j] <- 0
    evgmatch[j] <- 0
  }
  if (plotcent$RASTERVALU[j]!=-9999) {
    currownum2 <- which(xtable$ID==plotcent$RASTERVALU[j])
    pixelchar <- xtable[currownum2,]
    covermatch[j] <- plotchar$canopy_cover==pixelchar$canopy_cover
    heightmatch[j] <- plotchar$canopy_height==pixelchar$canopy_height
    evgmatch[j] <- plotchar$EVT_GP==pixelchar$EVT_GP
  }
}
sum(covermatch) # 2244/9322 FIA plots (24.1%) match the imputed value for forest cover at plot centroid
sum(heightmatch) # 6114/9322 FIA plots (65.6%) match the imputed value for forest height at plot centroid
sum(evgmatch) # 4169/9322 FIA plots (44.7%) match the imputed value for EVG at plot centroid

# assessing accuracy of Landfire compared to plot characteristics
xcov <- read.dbf("F:\\Tree_List_c2014\\x_table\\shapefile\\X_table_final_INVYR2014_Albers_canopy_cover.dbf")
xht <- read.dbf("F:\\Tree_List_c2014\\x_table\\shapefile\\X_table_final_INVYR2014_Albers_canopy_height.dbf")
xevg <- read.dbf("F:\\Tree_List_c2014\\x_table\\shapefile\\X_table_final_INVYR2014_Albers_EVG_forest.dbf")
lfcovacc <- as.logical()
lfhtacc <- as.logical()
lfevgacc <- as.logical()
for (j in 1:dim(xcov)[[1]])
{
  currownum <- which(xtable$ID==xcov$ID[j])
  plotchar <- xtable[currownum,]
  lfcovacc[j] <- plotchar$canopy_cover==xcov$RASTERVALU[j]
  lfhtacc[j] <- plotchar$canopy_height==xht$RASTERVALU[j]
  lfevgacc[j] <- plotchar$EVT_GP==xevg$RASTERVALU[j]
}
sum(lfcovacc) # 2158/9322 = proportion of time Landfire is "right" about cover at the centroid pixel
sum(lfcovacc)/dim(xcov)[[1]] # 0.2314954 = 23.1% = percent of time Landfire is "right" about cover at the centroid pixel
sum(lfhtacc) # 6097/9322 = proportion of time Landfire is "right" about height at the centroid pixel
sum(lfhtacc)/dim(xcov)[[1]] # 0.6540442 = 65.4% = percent of time Landfire is "right" about height at the centroid pixel
sum(lfevgacc) # 3986/9322 = proportion of time Landfire is "right" about EVG at the centroid pixel
sum(lfevgacc)/dim(xcov)[[1]] # 0.4275906 = 42.8% percent of time Landfire is "right" about EVG at the centroid pixel



# compare characteristics of plot location to LANDFIRE data c2014 -------------------------------------------------------
# compare characteristics of plot location to imputed data c2014
combinecov <- read.dbf("F:\\Tree_List_c2014\\validation\\with_plot_location\\X_table_canopy_cover_buffer35m_combine.tif.vat.dbf")
names(combinecov)
length(unique(combinecov$X_TABLE_FI)) #8741
# of 9322 FIA plots in INVYR=2014, 8741 (93.8%) had at least one forested pixel with cell center within 34.75m (the plot radius)
# some FIA plot locations had more than one plot imputed to the pixels within that radius
combineht <- read.dbf("F:\\Tree_List_c2014\\validation\\with_plot_location\\X_table_canopy_height_buffer35m_combine.tif.vat.dbf")
combineevg <- read.dbf("F:\\Tree_List_c2014\\validation\\with_plot_location\\X_table_EVG_forest_buffer35m_combine.tif.vat.dbf")

# cover
plotlist <- sort(unique(combinecov$X_TABLE_FI))
covanymatchvec <- as.numeric()
weightcovvec5 <- as.numeric()
weightcovvec10 <- as.numeric()
weighthtvec5 <- as.numeric()
weighthtvec10 <- as.numeric()
for (j in 1:length(plotlist))
{
  curmat <- combinecov[(combinecov$X_TABLE_FI==plotlist[j]),]
  # see if cover matches...
  currownum <- which(xtable$ID==plotlist[j])
  currow <- xtable[currownum,]
  plotcov <- currow$canopy_cover
  #...in any pixels or weighted value of all pixels within 35m range (within 5% or 10%)
  numimpplot <- dim(curmat)[[1]]
  covmatchcheck <- as.logical()
  for (k in 1:numimpplot)
  {
    covmatchcheck[k] <- curmat$CANOPY_COV[k]==plotcov
  }
  if (sum(covmatchcheck)==0) { covanymatchvec[j]=0 }
  if (sum(covmatchcheck)>0) { covanymatchvec[j]=1 }
  weightedcover <- sum((curmat$CANOPY_COV * curmat$COUNT))/sum(curmat$COUNT)
  if (abs(plotcov-weightedcover)<=5) { weightcovvec5[j] <- 1 }
  if (abs(plotcov-weightedcover)>5) { weightcovvec5[j] <- 0 }
  if (abs(plotcov-weightedcover)<=10) { weightcovvec10[j] <- 1 }
  if (abs(plotcov-weightedcover)>10) { weightcovvec10[j] <- 0 }
}
# height
for (j in 1:length(plotlist))
{
  curmat <- combineht[(combineht$X_TABLE_FI==plotlist[j]),]
  # see if height matches...
  currownum <- which(xtable$ID==plotlist[j])
  currow <- xtable[currownum,]
  # see if height matches in any pixels
  plotheight <- currow$canopy_height
  htmatchcheck <- as.logical()
  numimpplot <- dim(curmat)[[1]]
  for (k in 1:numimpplot)
  {
    htmatchcheck[k] <- curmat$CANOPY_HEI[k]==plotheight
  }
  if (sum(htmatchcheck)==0) { htanymatchvec[j]=0 }
  if (sum(htmatchcheck)>0) { htanymatchvec[j]=1 }
  weightedheight <- sum((curmat$CANOPY_HEI * curmat$COUNT))/sum(curmat$COUNT)
  if (abs(plotheight-weightedheight)<=5) { weighthtvec5[j] <- 1 }
  if (abs(plotheight-weightedheight)>5) { weighthtvec5[j] <- 0 }
  if (abs(plotheight-weightedheight)<=10) { weighthtvec10[j] <- 1 }
  if (abs(plotheight-weightedheight)>10) { weighthtvec10[j] <- 0 }
}
# EVG
for (j in 1:length(plotlist))
{
  curmat <- combineevg[(combineevg$X_TABLE_FI==plotlist[j]),]
  # see if height matches...
  currownum <- which(xtable$ID==plotlist[j])
  currow <- xtable[currownum,]
  # see if EVG matches in any pixels
  plotevg <- currow$EVT_GP
  evgmatchcheck <- as.logical()
  for (k in 1:numimpplot)
  {
    evgmatchcheck[k] <- curmat$EVG_FOREST[k]==plotevg
  }
  if (sum(evgmatchcheck)==0) { evganymatchvec[j]=0 }
  if (sum(evgmatchcheck)>0) { evganymatchvec[j]=1 }
}
sum(covanymatchvec) # of 9322 plots, the cover of at least one pixel within a 35m radius matched the plot value in 3646 cases (39.1%)
sum(weightcovvec5) # of 9322 plots, the weighted cover of pixels within a 35m radius is within 5% of the plot value in 2631 cases (28.2%)
sum(weightcovvec10) # 5013/9322 plots (53.8%) have weighted cover of pixels within a 35m radius within 10% of the plot value
sum(htanymatchvec) # of 9322 plots, the height of at least one pixel within a 35m radius matched the plot value in 7100 cases (76.2%)
sum(weighthtvec5) # of 9322 plots, the weighted height of pixels within a 35m radius is within 5m of the plot value in 6465 cases (69.4%)
sum(weighthtvec10) # of 9322 plots, the weighted height of pixels within a 35m radius is within 10m of the plot value in 7419 cases (79.6%)
sum(evganymatchvec) # of 9322 plots, the EVG of at least one pixel within a 35m radius matched the plot value in 4011 cases (43.0%)



# using plot centroid as metric only
# cover
plotcentevc <- read.dbf("F:\\Tree_List_c2014\\x_table\\shapefile\\X_table_final_INVYR2014_Albers_canopy_cover.dbf")
covermatch2 <- as.numeric()
counter <- 0
for (j in 1:dim(plotcentevc)[[1]])
{
  currownum <- which(xtable$ID==plotcentevc$ID[j])
  plotcharevc <- xtable[currownum,]
  if (plotcentevc$RASTERVALU[j]==-9999) {
    covermatch2[j] <- 0
    counter <- counter + 1 
    ##heightmatch[j] <- 0
    ##evgmatch[j] <- 0
  }
  if (plotcentevc$RASTERVALU[j]!=-9999) {
    covermatch2[j] <- plotcharevc$canopy_cover==plotcentevc$RASTERVALU[j] }
    ##heightmatch[j] <- plotchar$canopy_height==pixelchar$canopy_height
    ##evgmatch[j] <- plotchar$EVT_GP==pixelchar$EVT_GP
  ##}
}
# height
plotcentht <- read.dbf("F:\\Tree_List_c2014\\x_table\\shapefile\\X_table_final_INVYR2014_Albers_canopy_height.dbf")
heightmatch2 <- as.numeric()
for (j in 1:dim(plotcentht)[[1]])
{
  currownum <- which(xtable$ID==plotcentht$ID[j])
  plotcharht <- xtable[currownum,]
  if (plotcentht$RASTERVALU[j]==-9999) {
    heightmatch2[j] <- 0 }
  if (plotcentht$RASTERVALU[j]!=-9999) {
    heightmatch2[j] <- plotcharht$canopy_height==plotcentht$RASTERVALU[j] }
}
# EVG
plotcentevg <- read.dbf("F:\\Tree_List_c2014\\x_table\\shapefile\\X_table_final_INVYR2014_Albers_EVG_forest.dbf")
evgmatch2 <- as.numeric()
for (j in 1:dim(plotcentevg)[[1]])
{
  currownum <- which(xtable$ID==plotcentevg$ID[j])
  plotcharevg <- xtable[currownum,]
  if (plotcentevg$RASTERVALU[j]==-9999) {
    evgmatch2[j] <- 0 }
  if (plotcentevg$RASTERVALU[j]!=-9999) {
    evgmatch2[j] <- plotcharevg$EVT_GP==plotcentevg$RASTERVALU[j] }
}
counter # in 972/9322 cases (10.4%), the pixel at plot centroid was not mapped as forested in the LANDFIRE data
sum(covermatch2) # 2158/9322 FIA plots (23.1%) match the LANDFIRE value for forest cover at plot centroid
sum(heightmatch2) # 6097/9322 FIA plots (65.4%) match the LANDFIRE value for forest height at plot centroid
sum(evgmatch2) # 3986/9322 FIA plots (42.8%) match the imputed value for EVG at plot centroid
