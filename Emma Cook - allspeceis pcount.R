install.packages("unmarked")
library("unmarked")
#install.packages("AICcmodavg")
library(AICcmodavg)


#correlation in buffers
test <-read.csv("bhnu_abundance.csv")
testpine<-test[c(16,18,20)]
cor(testpine)
summary(test)

setwd("C:/Users/jquinn2/Dropbox/Writing and Projects/1.5.2.1 Easements and matrix/Emma")


#null and global models
fmnull.4aou <- pcount(~1 ~1, wbnu)
fmglobal.4aou <- pcount(~ Noise ~ YearCat + CanopyHt + CanopyCvr + TreeDensity  +PineDensity +AreaA +Pine50mA +Pine100mA +Pine500mA  +LC.Type +Snag, wbnu)

#white breasted nuthatch
wbnu <- csvToUMF("wbnu_abundance.csv", long = FALSE, type = "unmarkedFramePCount")    # type changed for pcount
summary(wbnu)

test <-read.csv("wbnu_abundance.csv")
#str(test)

fmnull.wbnu <- pcount(~1 ~1, wbnu)
fmglobal.wbnu <- pcount(~ Noise ~ YearCat + CanopyHt + CanopyCvr + TreeDensity  +AreaA +LC.Type +Snag, wbnu)
#just structure
fm1.wbnu <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity +Snag, wbnu)
#just patch
fm2.wbnu <- pcount(~ Noise ~ YearCat + AreaA +Snag, wbnu)
#just matrix                    
fm3.wbnu <- pcount(~ Noise  ~ YearCat +LC.Type+Snag, wbnu)
#structure and patch
fm4.wbnu <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity+ AreaA +Snag, wbnu)
#structure and matrix
fm5.wbnu <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity +LC.Type+Snag, wbnu)
#patch and matrix
fm6.wbnu <- pcount(~ Noise  ~ YearCat +AreaA +LC.Type+ Snag, wbnu)

summary(fmnull.wbnu)
summary(fmglobal.wbnu)
summary(fm1.wbnu)
summary(fm2.wbnu)
summary(fm3.wbnu)
summary(fm4.wbnu)
summary(fm5.wbnu)
summary(fm6.wbnu)

wbnu.fms <- fitList(fmglobal.wbnu,fm1.wbnu,fm2.wbnu,fm3.wbnu,fm4.wbnu,fm5.wbnu,fm6.wbnu )
wbnu.fms <- fitList(fm2.wbnu,fm6.wbnu)
#fms <- fitList(CanopyHeight.Snag=fm1.wbnu, TreeDensity.Snag=fm2.wbnu, TreeBA.Snag=fm3.wbnu, PineBA.Snag=fm4.wbnu, PineDensity.Snag=fm5.wbnu, CanopyCover.Snag=fm6.wbnu, CanopyHeight.PineBA.Snag=fm7.wbnu, CanopyHeight.TreeBA.PineDensity.Snag=fm8.wbnu, CanopyCover.TreeDensity.Snag=fm9.wbnu, CanopyCover.CanopyHeight.Snag=fm10.wbnu, System.Pine500m.Snag=fm11.wbnu, PineBA.CanopyCover.Pine500m.Snag=fm12.wbnu, PatchArea.PineBA.Snag=fm13.wbnu, PatchArea.CanopyHeight.CanopyCover.PineBA.Snag=fm14.wbnu, System.PineBA.Snag=fm15.wbnu, Pine500m.Snag=fm16.wbnu, Pine50m.Snag=fm17.wbnu, Pine100m.Snag=fm18.wbnu, PatchArea.Snag=fm20.wbnu, PatchArea.Pine500m.Snag=fm21.wbnu, PineBA.PineDensity.Snag=fm23.wbnu, CanopyHeight.TreeDensity.Snag=fm24.wbnu, TreeDensity.Pine500m.System.Snag=fm25.wbnu, PineDensity.PatchArea.Snag=fm26.wbnu, PatchArea.Pine500m.System.Snag=fm27.wbnu, PatchArea.PineBA.CanopyHeight.Snag=fm28.wbnu)
wbnu.ms1 <- modSel(wbnu.fms)
#wbnu.ms1@Full
wbnu.ms1
Cand.models.wbnu <- list(fm2.wbnu,fm6.wbnu)
Modnames.wbnu <- c("Patch", "Patch + Matrix")


#tufted Titmouse
tuti <- csvToUMF("tuti_abundance.csv", long = FALSE, type = "unmarkedFramePCount")    # type changed for pcount
summary(tuti)
test <-read.csv("tuti_abundance.csv")
#str(test)

fmnull.tuti <- pcount(~1 ~1, tuti)
fmglobal.tuti <- pcount(~ Noise ~ YearCat + CanopyHt + CanopyCvr + TreeDensity  +AreaA +LC.Type +Snag, tuti)
#just structure
fm1.tuti <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity +Snag, tuti)
#just patch
fm2.tuti <- pcount(~ Noise ~ YearCat + AreaA +Snag, tuti)
#just matrix                    
fm3.tuti <- pcount(~ Noise  ~ YearCat +LC.Type+ Snag, tuti)
#structure and patch
fm4.tuti <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity+ AreaA +Snag, tuti)
#structure and matrix
fm5.tuti <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity + LC.Type+Snag, tuti)
#patch and matrix
fm6.tuti <- pcount(~ Noise  ~ YearCat +AreaA +LC.Type+ Snag, tuti)

summary(fmnull.tuti)
summary(fmglobal.tuti)
summary(fm1.tuti)
summary(fm2.tuti)
summary(fm3.tuti)
summary(fm4.tuti)
summary(fm5.tuti)
summary(fm6.tuti)

tuti.fms <- fitList(fmglobal.tuti,fm1.tuti,fm2.tuti,fm3.tuti,fm4.tuti,fm5.tuti,fm6.tuti )
tuti.fms <- fitList(fmglobal.tuti,fm1.tuti, fm3.tuti, fm4.tuti,  fm6.tuti )
#fms <- fitList(CanopyHeight.Snag=fm1.tuti, TreeDensity.Snag=fm2.tuti, TreeBA.Snag=fm3.tuti, PineBA.Snag=fm4.tuti, PineDensity.Snag=fm5.tuti, CanopyCover.Snag=fm6.tuti, CanopyHeight.PineBA.Snag=fm7.tuti, CanopyHeight.TreeBA.PineDensity.Snag=fm8.tuti, CanopyCover.TreeDensity.Snag=fm9.tuti, CanopyCover.CanopyHeight.Snag=fm10.tuti, System.Pine500m.Snag=fm11.tuti, PineBA.CanopyCover.Pine500m.Snag=fm12.tuti, PatchArea.PineBA.Snag=fm13.tuti, PatchArea.CanopyHeight.CanopyCover.PineBA.Snag=fm14.tuti, System.PineBA.Snag=fm15.tuti, Pine500m.Snag=fm16.tuti, Pine50m.Snag=fm17.tuti, Pine100m.Snag=fm18.tuti, PatchArea.Snag=fm20.tuti, PatchArea.Pine500m.Snag=fm21.tuti, PineBA.PineDensity.Snag=fm23.tuti, CanopyHeight.TreeDensity.Snag=fm24.tuti, TreeDensity.Pine500m.System.Snag=fm25.tuti, PineDensity.PatchArea.Snag=fm26.tuti, PatchArea.Pine500m.System.Snag=fm27.tuti, PatchArea.PineBA.CanopyHeight.Snag=fm28.tuti)
tuti.ms1 <- modSel(tuti.fms)
tuti.ms1@Full
tuti.ms1
Cand.models.tuti <- list(fmglobal.tuti,fm1.tuti, fm3.tuti, fm4.tuti,  fm5.tuti)
Modnames.tuti <- c("Global","Structure","Matrix" , "Structure + Patch", "Structre + Matrix")


#Red-bellied woodpecker
rbwo <- csvToUMF("rbwo_abundance.csv", long = FALSE, type = "unmarkedFramePCount")    # type changed for pcount
summary(rbwo)
test <-read.csv("rbwo_abundance.csv")
#str(test)

fmnull.rbwo <- pcount(~1 ~1, rbwo)
fmglobal.rbwo <- pcount(~ Noise ~ YearCat + CanopyHt + CanopyCvr + TreeDensity  +AreaA +LC.Type +Snag, rbwo)
#just structure
fm1.rbwo <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity +Snag, rbwo)
#just patch
fm2.rbwo <- pcount(~ Noise ~ YearCat + AreaA +Snag, rbwo)
#just matrix                    
fm3.rbwo <- pcount(~ Noise  ~ YearCat +LC.Type+ Snag, rbwo)
#structure and patch
fm4.rbwo <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity+ AreaA +Snag, rbwo)
#structure and matrix
fm5.rbwo <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity + LC.Type+Snag, rbwo)
#patch and matrix
fm6.rbwo <- pcount(~ Noise  ~ YearCat +AreaA +LC.Type+ Snag, rbwo)

summary(fmnull.rbwo)
summary(fmglobal.rbwo)
summary(fm1.rbwo)
summary(fm2.rbwo)
summary(fm3.rbwo)
summary(fm4.rbwo)
summary(fm5.rbwo)
summary(fm6.rbwo)

rbwo.fms <- fitList(fmglobal.rbwo,fm1.rbwo,fm2.rbwo,fm3.rbwo,fm4.rbwo,fm5.rbwo,fm6.rbwo )
rbwo.fms <- fitList( fm2.rbwo,fm3.rbwo, fm5.rbwo, fm6.rbwo )
#fms <- fitList(CanopyHeight.Snag=fm1.rbwo, TreeDensity.Snag=fm2.rbwo, TreeBA.Snag=fm3.rbwo, PineBA.Snag=fm4.rbwo, PineDensity.Snag=fm5.rbwo, CanopyCover.Snag=fm6.rbwo, CanopyHeight.PineBA.Snag=fm7.rbwo, CanopyHeight.TreeBA.PineDensity.Snag=fm8.rbwo, CanopyCover.TreeDensity.Snag=fm9.rbwo, CanopyCover.CanopyHeight.Snag=fm10.rbwo, System.Pine500m.Snag=fm11.rbwo, PineBA.CanopyCover.Pine500m.Snag=fm12.rbwo, PatchArea.PineBA.Snag=fm13.rbwo, PatchArea.CanopyHeight.CanopyCover.PineBA.Snag=fm14.rbwo, System.PineBA.Snag=fm15.rbwo, Pine500m.Snag=fm16.rbwo, Pine50m.Snag=fm17.rbwo, Pine100m.Snag=fm18.rbwo, PatchArea.Snag=fm20.rbwo, PatchArea.Pine500m.Snag=fm21.rbwo, PineBA.PineDensity.Snag=fm23.rbwo, CanopyHeight.TreeDensity.Snag=fm24.rbwo, TreeDensity.Pine500m.System.Snag=fm25.rbwo, PineDensity.PatchArea.Snag=fm26.rbwo, PatchArea.Pine500m.System.Snag=fm27.rbwo, PatchArea.PineBA.CanopyHeight.Snag=fm28.rbwo)
rbwo.ms1 <- modSel(rbwo.fms)
#rbwo.ms1@Full
rbwo.ms1
Cand.models.rbwo <- list(fm2.rbwo,fm3.rbwo, fm5.rbwo, fm6.rbwo)
Modnames.rbwo <- c("Patch","Matrix" , "Structure + Matrix", "Patch + Matrix")


#Hairy Woodpecker
hawo <- csvToUMF("hawo_abundance.csv", long = FALSE, type = "unmarkedFramePCount")    # type changed for pcount
summary(hawo)
test <-read.csv("hawo_abundance.csv")
#str(test)

fmnull.hawo <- pcount(~1 ~1, hawo)
fmglobal.hawo <- pcount(~ Noise ~ YearCat + CanopyHt + CanopyCvr + TreeDensity  +AreaA +LC.Type +Snag, hawo)
#just structure
fm1.hawo <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity +Snag, hawo)
#just patch
fm2.hawo <- pcount(~ Noise ~ YearCat + AreaA +Snag, hawo)
#just matrix                    
fm3.hawo <- pcount(~ Noise  ~ YearCat +LC.Type+ Snag, hawo)
#structure and patch
fm4.hawo <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity+ AreaA +Snag, hawo)
#structure and matrix
fm5.hawo <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity + LC.Type+Snag, hawo)
#patch and matrix
fm6.hawo <- pcount(~ Noise  ~ YearCat +AreaA +LC.Type+Snag, hawo)

summary(fmnull.hawo)
summary(fmglobal.hawo)
summary(fm1.hawo)  
summary(fm2.hawo)
summary(fm3.hawo)
summary(fm4.hawo)
summary(fm5.hawo)
summary(fm6.hawo)

hawo.fms <- fitList(fmglobal.hawo,fm1.hawo,fm2.hawo,fm3.hawo,fm4.hawo,fm5.hawo,fm6.hawo )
hawo.fms <- fitList( fm2.hawo,fm3.hawo, fm6.hawo )
#fms <- fitList(CanopyHeight.Snag=fm1.hawo, TreeDensity.Snag=fm2.hawo, TreeBA.Snag=fm3.hawo, PineBA.Snag=fm4.hawo, PineDensity.Snag=fm5.hawo, CanopyCover.Snag=fm6.hawo, CanopyHeight.PineBA.Snag=fm7.hawo, CanopyHeight.TreeBA.PineDensity.Snag=fm8.hawo, CanopyCover.TreeDensity.Snag=fm9.hawo, CanopyCover.CanopyHeight.Snag=fm10.hawo, System.Pine500m.Snag=fm11.hawo, PineBA.CanopyCover.Pine500m.Snag=fm12.hawo, PatchArea.PineBA.Snag=fm13.hawo, PatchArea.CanopyHeight.CanopyCover.PineBA.Snag=fm14.hawo, System.PineBA.Snag=fm15.hawo, Pine500m.Snag=fm16.hawo, Pine50m.Snag=fm17.hawo, Pine100m.Snag=fm18.hawo, PatchArea.Snag=fm20.hawo, PatchArea.Pine500m.Snag=fm21.hawo, PineBA.PineDensity.Snag=fm23.hawo, CanopyHeight.TreeDensity.Snag=fm24.hawo, TreeDensity.Pine500m.System.Snag=fm25.hawo, PineDensity.PatchArea.Snag=fm26.hawo, PatchArea.Pine500m.System.Snag=fm27.hawo, PatchArea.PineBA.CanopyHeight.Snag=fm28.hawo)
hawo.ms1 <- modSel(hawo.fms)
#hawo.ms1@Full
hawo.ms1
Cand.models.hawo <- list(fm2.hawo,fm3.hawo, fm6.hawo )
Modnames.hawo <- c("Patch","Matrix" , "Patch + Matrix")


#Great Creasted Flycatcher
gcfl <- csvToUMF("gcfl_abundance.csv", long = FALSE, type = "unmarkedFramePCount")    # type changed for pcount
summary(gcfl)
test <-read.csv("gcfl_abundance.csv")
#str(test)

fmnull.gcfl <- pcount(~1 ~1, gcfl)
fmglobal.gcfl <- pcount(~ Noise ~ YearCat + CanopyHt + CanopyCvr + TreeDensity  +AreaA +LC.Type +Snag, gcfl)
#just structure
fm1.gcfl <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity +Snag, gcfl)
#just patch
fm2.gcfl <- pcount(~ Noise ~ YearCat + AreaA +Snag, gcfl)
#just matrix                    
fm3.gcfl <- pcount(~ Noise  ~ YearCat +LC.Type+ Snag, gcfl)
#structure and patch
fm4.gcfl <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity+ AreaA +Snag, gcfl)
#structure and matrix
fm5.gcfl <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity + LC.Type+Snag, gcfl)
#patch and matrix
fm6.gcfl <- pcount(~ Noise  ~ YearCat +AreaA +LC.Type+ Snag, gcfl)

summary(fmnull.gcfl)
summary(fmglobal.gcfl)
summary(fm1.gcfl)  
summary(fm2.gcfl)
summary(fm3.gcfl)
summary(fm4.gcfl)
summary(fm5.gcfl)
summary(fm6.gcfl)

gcfl.fms <- fitList(fmglobal.gcfl,fm1.gcfl,fm2.gcfl,fm3.gcfl,fm4.gcfl,fm5.gcfl,fm6.gcfl )
gcfl.fms <- fitList(fm1.gcfl, fm2.gcfl,fm3.gcfl,  fm5.gcfl, fm6.gcfl )
#fms <- fitList(CanopyHeight.Snag=fm1.gcfl, TreeDensity.Snag=fm2.gcfl, TreeBA.Snag=fm3.gcfl, PineBA.Snag=fm4.gcfl, PineDensity.Snag=fm5.gcfl, CanopyCover.Snag=fm6.gcfl, CanopyHeight.PineBA.Snag=fm7.gcfl, CanopyHeight.TreeBA.PineDensity.Snag=fm8.gcfl, CanopyCover.TreeDensity.Snag=fm9.gcfl, CanopyCover.CanopyHeight.Snag=fm10.gcfl, System.Pine500m.Snag=fm11.gcfl, PineBA.CanopyCover.Pine500m.Snag=fm12.gcfl, PatchArea.PineBA.Snag=fm13.gcfl, PatchArea.CanopyHeight.CanopyCover.PineBA.Snag=fm14.gcfl, System.PineBA.Snag=fm15.gcfl, Pine500m.Snag=fm16.gcfl, Pine50m.Snag=fm17.gcfl, Pine100m.Snag=fm18.gcfl, PatchArea.Snag=fm20.gcfl, PatchArea.Pine500m.Snag=fm21.gcfl, PineBA.PineDensity.Snag=fm23.gcfl, CanopyHeight.TreeDensity.Snag=fm24.gcfl, TreeDensity.Pine500m.System.Snag=fm25.gcfl, PineDensity.PatchArea.Snag=fm26.gcfl, PatchArea.Pine500m.System.Snag=fm27.gcfl, PatchArea.PineBA.CanopyHeight.Snag=fm28.gcfl)
gcfl.ms1 <- modSel(gcfl.fms)
#gcfl.ms1@Full
gcfl.ms1
Cand.models.gcfl <- list(fm1.gcfl, fm2.gcfl,fm3.gcfl,  fm5.gcfl, fm6.gcfl)
Modnames.gcfl <- c("Structure","Patch","Matrix" , "Structure + Matrix", "Patch + Matrix")

#Downy Woodpecker
dowo <- csvToUMF("dowo_abundance.csv", long = FALSE, type = "unmarkedFramePCount")    # type changed for pcount
summary(dowo)
test <-read.csv("dowo_abundance.csv")
#str(test)

fmnull.dowo <- pcount(~1 ~1, dowo)
fmglobal.dowo <- pcount(~ Noise ~ YearCat + CanopyHt + CanopyCvr + TreeDensity  +AreaA +LC.Type +Snag, dowo)
#just structure
fm1.dowo <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity +Snag, dowo)
#just patch
fm2.dowo <- pcount(~ Noise ~ YearCat + AreaA +Snag, dowo)
#just matrix                    
fm3.dowo <- pcount(~ Noise  ~ YearCat +LC.Type+Snag, dowo)
#structure and patch
fm4.dowo <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity+ AreaA +Snag, dowo)
#structure and matrix
fm5.dowo <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity + LC.Type+Snag, dowo)
#patch and matrix
fm6.dowo <- pcount(~ Noise  ~ YearCat +AreaA +LC.Type +Snag, dowo)

summary(fmnull.dowo)
summary(fmglobal.dowo)
summary(fm1.dowo)  
summary(fm2.dowo)
summary(fm3.dowo)
summary(fm4.dowo)
summary(fm5.dowo)
summary(fm6.dowo)

dowo.fms <- fitList(fmglobal.dowo,fm1.dowo,fm2.dowo,fm3.dowo,fm4.dowo,fm5.dowo,fm6.dowo )
dowo.fms <- fitList(fmglobal.dowo,fm3.dowo, fm5.dowo, fm6.dowo )
#fms <- fitList(CanopyHeight.Snag=fm1.dowo, TreeDensity.Snag=fm2.dowo, TreeBA.Snag=fm3.dowo, PineBA.Snag=fm4.dowo, PineDensity.Snag=fm5.dowo, CanopyCover.Snag=fm6.dowo, CanopyHeight.PineBA.Snag=fm7.dowo, CanopyHeight.TreeBA.PineDensity.Snag=fm8.dowo, CanopyCover.TreeDensity.Snag=fm9.dowo, CanopyCover.CanopyHeight.Snag=fm10.dowo, System.Pine500m.Snag=fm11.dowo, PineBA.CanopyCover.Pine500m.Snag=fm12.dowo, PatchArea.PineBA.Snag=fm13.dowo, PatchArea.CanopyHeight.CanopyCover.PineBA.Snag=fm14.dowo, System.PineBA.Snag=fm15.dowo, Pine500m.Snag=fm16.dowo, Pine50m.Snag=fm17.dowo, Pine100m.Snag=fm18.dowo, PatchArea.Snag=fm20.dowo, PatchArea.Pine500m.Snag=fm21.dowo, PineBA.PineDensity.Snag=fm23.dowo, CanopyHeight.TreeDensity.Snag=fm24.dowo, TreeDensity.Pine500m.System.Snag=fm25.dowo, PineDensity.PatchArea.Snag=fm26.dowo, PatchArea.Pine500m.System.Snag=fm27.dowo, PatchArea.PineBA.CanopyHeight.Snag=fm28.dowo)
dowo.ms1 <- modSel(dowo.fms)
#dowo.ms1@Full
dowo.ms1
Cand.models.dowo <- list(fmglobal.dowo,fm3.dowo, fm5.dowo, fm6.dowo )
Modnames.dowo <- c("Global","Matrix" , "Structure + Matrix", "Patch + Matrix")

#Carolina Wren
cawr <- csvToUMF("cawr_abundance.csv", long = FALSE, type = "unmarkedFramePCount")    # type changed for pcount
summary(cawr)
test <-read.csv("cawr_abundance.csv")
#str(test)

fmnull.cawr <- pcount(~1 ~1, cawr)
fmglobal.cawr <- pcount(~ Noise ~ YearCat + CanopyHt + CanopyCvr + TreeDensity  +AreaA +LC.Type +Snag, cawr)
#just structure
fm1.cawr <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity +Snag, cawr)
#just patch
fm2.cawr <- pcount(~ Noise ~ YearCat + AreaA +Snag, cawr)
#just matrix                    
fm3.cawr <- pcount(~ Noise  ~ YearCat +LC.Type+Snag, cawr)
#structure and patch
fm4.cawr <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity+ AreaA +Snag, cawr)
#structure and matrix
fm5.cawr <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity + LC.Type+Snag, cawr)
#patch and matrix
fm6.cawr <- pcount(~ Noise  ~ YearCat +AreaA +LC.Type+ Snag, cawr)

summary(fmnull.cawr)
summary(fmglobal.cawr)
summary(fm1.cawr)  
summary(fm2.cawr)
summary(fm3.cawr)
summary(fm4.cawr)
summary(fm5.cawr)
summary(fm6.cawr)

cawr.fms <- fitList(fmglobal.cawr,fm1.cawr,fm2.cawr,fm3.cawr,fm4.cawr,fm5.cawr,fm6.cawr )
cawr.fms <- fitList(fm1.cawr, fm2.cawr,fm3.cawr, fm6.cawr )
#fms <- fitList(CanopyHeight.Snag=fm1.cawr, TreeDensity.Snag=fm2.cawr, TreeBA.Snag=fm3.cawr, PineBA.Snag=fm4.cawr, PineDensity.Snag=fm5.cawr, CanopyCover.Snag=fm6.cawr, CanopyHeight.PineBA.Snag=fm7.cawr, CanopyHeight.TreeBA.PineDensity.Snag=fm8.cawr, CanopyCover.TreeDensity.Snag=fm9.cawr, CanopyCover.CanopyHeight.Snag=fm10.cawr, System.Pine500m.Snag=fm11.cawr, PineBA.CanopyCover.Pine500m.Snag=fm12.cawr, PatchArea.PineBA.Snag=fm13.cawr, PatchArea.CanopyHeight.CanopyCover.PineBA.Snag=fm14.cawr, System.PineBA.Snag=fm15.cawr, Pine500m.Snag=fm16.cawr, Pine50m.Snag=fm17.cawr, Pine100m.Snag=fm18.cawr, PatchArea.Snag=fm20.cawr, PatchArea.Pine500m.Snag=fm21.cawr, PineBA.PineDensity.Snag=fm23.cawr, CanopyHeight.TreeDensity.Snag=fm24.cawr, TreeDensity.Pine500m.System.Snag=fm25.cawr, PineDensity.PatchArea.Snag=fm26.cawr, PatchArea.Pine500m.System.Snag=fm27.cawr, PatchArea.PineBA.CanopyHeight.Snag=fm28.cawr)
cawr.ms1 <- modSel(cawr.fms)
#cawr.ms1@Full
cawr.ms1
Cand.models.cawr <- list(fm1.cawr, fm2.cawr,fm3.cawr, fm6.cawr)
Modnames.cawr <- c("Structure","Patch","Matrix" , "Patch + Matrix")

#Carolina Chickadee
cach <- csvToUMF("cach_abundance2.csv", long = FALSE, type = "unmarkedFramePCount")    # type changed for pcount
summary(cach)
test <-read.csv("cach_abundance.csv")
#str(test)

fmnull.cach <- pcount(~1 ~1, cach)
fmglobal.cach <- pcount(~ Noise ~ YearCat + CanopyHt + CanopyCvr + TreeDensity  +AreaA +LC.Type +Snag, cach, K=10)
#just structure
fm1.cach <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity +Snag, cach, K=10)
#just patch
fm2.cach <- pcount(~ Noise ~ YearCat + AreaA +Snag, cach, K=10)
#just matrix                    
fm3.cach <- pcount(~ Noise  ~ YearCat +LC.Type +Snag, cach, K=10)
#structure and patch
fm4.cach <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity+ AreaA +Snag, cach, K=10)
#structure and matrix
fm5.cach <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + TreeDensity + LC.Type+Snag, cach, K=10)
#patch and matrix
fm6.cach <- pcount(~ Noise  ~ YearCat +AreaA +LC.Type+Snag, cach, K=10)

summary(fmnull.cach)
summary(fmglobal.cach)
summary(fm1.cach)  
summary(fm2.cach)
summary(fm3.cach)
summary(fm4.cach)
summary(fm5.cach)
summary(fm6.cach)

cach.fms <- fitList(fmglobal.cach,fm1.cach,fm2.cach,fm3.cach,fm4.cach,fm5.cach,fm6.cach )
cach.fms <- fitList(fmglobal.cach,fm1.cach, fm2.cach,fm3.cach,fm4.cach, fm5.cach,fm6.cach)
#fms <- fitList(CanopyHeight.Snag=fm1.cach, TreeDensity.Snag=fm2.cach, TreeBA.Snag=fm3.cach, PineBA.Snag=fm4.cach, PineDensity.Snag=fm5.cach, CanopyCover.Snag=fm6.cach, CanopyHeight.PineBA.Snag=fm7.cach, CanopyHeight.TreeBA.PineDensity.Snag=fm8.cach, CanopyCover.TreeDensity.Snag=fm9.cach, CanopyCover.CanopyHeight.Snag=fm10.cach, System.Pine500m.Snag=fm11.cach, PineBA.CanopyCover.Pine500m.Snag=fm12.cach, PatchArea.PineBA.Snag=fm13.cach, PatchArea.CanopyHeight.CanopyCover.PineBA.Snag=fm14.cach, System.PineBA.Snag=fm15.cach, Pine500m.Snag=fm16.cach, Pine50m.Snag=fm17.cach, Pine100m.Snag=fm18.cach, PatchArea.Snag=fm20.cach, PatchArea.Pine500m.Snag=fm21.cach, PineBA.PineDensity.Snag=fm23.cach, CanopyHeight.TreeDensity.Snag=fm24.cach, TreeDensity.Pine500m.System.Snag=fm25.cach, PineDensity.PatchArea.Snag=fm26.cach, PatchArea.Pine500m.System.Snag=fm27.cach, PatchArea.PineBA.CanopyHeight.Snag=fm28.cach)
cach.ms1 <- modSel(cach.fms)
#cach.ms1@Full
cach.ms1
Cand.models.cach <- list(fm1.cach, fm4.cach,  fm5.cach)
Modnames.cach <- c("Global","Structure", "Structure + Matrix")

#brown headed nuthatch
bhnu <- csvToUMF("bhnu_abundance.csv", long = FALSE, type = "unmarkedFramePCount")    # type changed for pcount
summary(bhnu)
test <-read.csv("bhnu_abundance.csv")
#str(test)

fmnull.bhnu <- pcount(~1 ~1, bhnu)
fmglobal.bhnu <- pcount(~ Noise ~ YearCat + CanopyHt + CanopyCvr + PineDensity  +AreaA +LC.Type +Pine50mA +Pine500mA +Snag, bhnu)
#just structure
fm1.bhnu <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + PineDensity +Snag, bhnu)
#just patch
fm2.bhnu <- pcount(~ Noise ~ YearCat + AreaA +Snag, bhnu)
#just matrix                    
fm3.bhnu <- pcount(~ Noise  ~ YearCat +LC.Type+ Pine50mA +Pine500mA+Snag, bhnu)
#structure and patch
fm4.bhnu <- pcount(~ Noise  ~ YearCat + CanopyHt + CanopyCvr + PineDensity+ AreaA +Snag, bhnu)
#structure and matrix
fm5.bhnu <- pcount(~ Noise  ~ YearCat + CanopyHt.m + CanopyCvr + PineDensity +Pine50mA +Pine500mA+LC.Type+Snag, bhnu)
#patch and matrix
fm6.bhnu <- pcount(~ Noise  ~ YearCat +AreaA +Pine50mA +Pine500mA+LC.Type+ Snag, bhnu)

summary(fmnull.bhnu)
summary(fmglobal.bhnu)
summary(fm1.bhnu)
summary(fm2.bhnu)
summary(fm3.bhnu)
summary(fm4.bhnu)
summary(fm5.bhnu)
summary(fm6.bhnu)

bhnu.fms <- fitList(fmglobal.bhnu,fm1.bhnu,fm2.bhnu,fm3.bhnu,fm4.bhnu,fm5.bhnu,fm6.bhnu )
bhnu.fms <- fitList(fm5.bhnu, fm6.bhnu )
#fms <- fitList(CanopyHeight.Snag=fm1.bhnu, TreeDensity.Snag=fm2.bhnu, TreeBA.Snag=fm3.bhnu, PineBA.Snag=fm4.bhnu, PineDensity.Snag=fm5.bhnu, CanopyCover.Snag=fm6.bhnu, CanopyHeight.PineBA.Snag=fm7.bhnu, CanopyHeight.TreeBA.PineDensity.Snag=fm8.bhnu, CanopyCover.TreeDensity.Snag=fm9.bhnu, CanopyCover.CanopyHeight.Snag=fm10.bhnu, System.Pine500m.Snag=fm11.bhnu, PineBA.CanopyCover.Pine500m.Snag=fm12.bhnu, PatchArea.PineBA.Snag=fm13.bhnu, PatchArea.CanopyHeight.CanopyCover.PineBA.Snag=fm14.bhnu, System.PineBA.Snag=fm15.bhnu, Pine500m.Snag=fm16.bhnu, Pine50m.Snag=fm17.bhnu, Pine100m.Snag=fm18.bhnu, PatchArea.Snag=fm20.bhnu, PatchArea.Pine500m.Snag=fm21.bhnu, PineBA.PineDensity.Snag=fm23.bhnu, CanopyHeight.TreeDensity.Snag=fm24.bhnu, TreeDensity.Pine500m.System.Snag=fm25.bhnu, PineDensity.PatchArea.Snag=fm26.bhnu, PatchArea.Pine500m.System.Snag=fm27.bhnu, PatchArea.PineBA.CanopyHeight.Snag=fm28.bhnu)
bhnu.ms1 <- modSel(bhnu.fms)
#bhnu.ms1@Full
bhnu.ms1
Cand.models.bhnu <- list(fm5.bhnu, fm6.bhnu )
Modnames.bhnu <- c("Str. + Matrix","Patch + Matrix")


summary(bhnu)
summary(cach)
summary(cawr)
summary(dowo)
summary(gcfl)
summary(hawo)
summary(rbwo)
summary(tuti)
summary(wbnu)

#Full Models 
bhnu.fms <- fitList(fmglobal.bhnu,fm1.bhnu,fm2.bhnu,fm3.bhnu,fm4.bhnu,fm5.bhnu,fm6.bhnu )
cach.fms <- fitList(fmglobal.cach,fm1.cach,fm2.cach,fm3.cach,fm4.cach,fm5.cach,fm6.cach )
cawr.fms <- fitList(fmglobal.cawr,fm1.cawr,fm2.cawr,fm3.cawr,fm4.cawr,fm5.cawr,fm6.cawr )
dowo.fms <- fitList(fmglobal.dowo,fm1.dowo,fm2.dowo,fm3.dowo,fm4.dowo,fm5.dowo,fm6.dowo )
gcfl.fms <- fitList(fmglobal.gcfl,fm1.gcfl,fm2.gcfl,fm3.gcfl,fm4.gcfl,fm5.gcfl,fm6.gcfl )
hawo.fms <- fitList(fmglobal.hawo,fm1.hawo,fm2.hawo,fm3.hawo,fm4.hawo,fm5.hawo,fm6.hawo )
rbwo.fms <- fitList(fmglobal.rbwo,fm1.rbwo,fm2.rbwo,fm3.rbwo,fm4.rbwo,fm5.rbwo,fm6.rbwo )
tuti.fms <- fitList(fmglobal.tuti,fm1.tuti,fm2.tuti,fm3.tuti,fm4.tuti,fm5.tuti,fm6.tuti )
wbnu.fms <- fitList(fmglobal.wbnu,fm1.wbnu,fm2.wbnu,fm3.wbnu,fm4.wbnu,fm5.wbnu,fm6.wbnu )

bhnu.ms1.full <- modSel(bhnu.fms)
cach.ms1.full <- modSel(cach.fms)
cawr.ms1.full <- modSel(cawr.fms)
dowo.ms1.full <- modSel(dowo.fms)
gcfl.ms1.full <- modSel(gcfl.fms)
hawo.ms1.full <- modSel(hawo.fms)
rbwo.ms1.full <- modSel(rbwo.fms)
tuti.ms1.full <- modSel(tuti.fms)
wbnu.ms1.full <- modSel(wbnu.fms)

#Top Models
bhnu.fms <- fitList(fmglobal.bhnu,fm5.bhnu,fm6.bhnu )
cach.fms <- fitList(fmglobal.cach,fm1.cach,fm4.cach,fm5.cach)
cawr.fms <- fitList(fm2.cawrfm3.cawr,fm6.cawr)
dowo.fms <- fitList(fm3.dowo,fm5.dowo,fm6.dowo,fmglobal.dowo )
gcfl.fms <- fitList(fm1.gcfl,fm2.gcfl,fm3.gcfl,fm5.gcfl,fm6.gcfl)
hawo.fms <- fitList(fm2.hawo,fm3.hawo,fm6.hawo)
rbwo.fms <- fitList(fm2.rbwo,fm3.rbwo,,fm6.rbwo)
tuti.fms <- fitList(fmglobal.tuti,fm1.tuti,fm3.tuti,fm4.tuti,fm5.tuti,fm6.tuti )
wbnu.fms <- fitList(fm2.wbnu)

#Model summary (AIC Table)
bhnu.ms1 <- modSel(bhnu.fms)
cach.ms1 <- modSel(cach.fms)
cawr.ms1 <- modSel(cawr.fms)
dowo.ms1 <- modSel(dowo.fms)
gcfl.ms1 <- modSel(gcfl.fms)
hawo.ms1 <- modSel(hawo.fms)
rbwo.ms1 <- modSel(rbwo.fms)
tuti.ms1 <- modSel(tuti.fms)
wbnu.ms1 <- modSel(wbnu.fms)

bhnu.ms1
cach.ms1
cawr.ms1
dowo.ms1
gcfl.ms1
hawo.ms1
rbwo.ms1
tuti.ms1
wbnu.ms1


bhnu.ms1@Full
cach.ms1@Full
cawr.ms1@Full
dowo.ms1@Full
gcfl.ms1@Full
hawo.ms1@Full
rbwo.ms1@Full
tuti.ms1@Full
wbnu.ms1@Full


#model average
Cand.models.bhnu <- list(fmglobal.bhnu,fm5.bhnu,fm6.bhnu)
Modnames.bhnu <- c("Global","Str. + Matrix","Patch + Matrix")
Cand.models.cach <- list(fm1.cach,fm4.cach)
Modnames.cach <- c("Str.","Str.+ Patch")
Cand.models.cawr <- list(fm1.cawr,fm2.cawr)
Modnames.cawr <- c("Str.","Patch")
Cand.models.dowo <- list(fm1.dowo,fm2.dowo,fm4.dowo)
Modnames.dowo <- c("Str.","Patch","Str.+ Patch")
Cand.models.gcfl<- list(fm1.gcfl,fm2.gcfl,fm3.gcfl,fm4.gcfl)
Modnames.gcfl <- c("Str.","Patch","Matrix","Str.+ Patch")
Cand.models.hawo <- list(fm1.hawo,fm2.hawo)
Modnames.hawo <- c("Str.","Patch")
Cand.models.rbwo <- list(fm1.rbwo,fm2.rbwo)
Modnames.rbwo <- c("Str.","Patch")
Cand.models.tuti <- list(fm1.tuti,fm2.tuti,fm4.tuti)
Modnames.tuti <- c("Str.","Patch","Str.+ Patch")
Cand.models.wbnu <- list(fm2.wbnu,fm4.wbnu)
Modnames.wbnu <- c("Patch","Str.+ Patch")









# Old  and uses AICc
#bhnu
print(aictab(cand.set = Cand.models.bhnu, modnames = Modnames.bhnu, second.ord = TRUE), digits = 4)
confset(cand.set = Cand.models.bhnu, modnames = Modnames.bhnu, second.ord = TRUE, method = "raw")
print(modavg(cand.set = Cand.models.bhnu, modnames = Modnames.bhnu,parm = "Int", c.hat = 1, conf.level = 0.95, second.ord = TRUE, nobs = NULL, exclude = NULL, warn = TRUE, uncond.se = "revised", parm.type = "lambda"),digits = 3)
fm6.bhnu
fm5.bhnu
#cach
print(aictab(cand.set = Cand.models.cach, modnames = Modnames.cach, second.ord = TRUE), digits = 4)
confset(cand.set = Cand.models.cach, modnames = Modnames.cach, second.ord = TRUE, method = "raw")
#cawr
print(aictab(cand.set = Cand.models.cawr, modnames = Modnames.cawr, second.ord = TRUE), digits = 4)
confset(cand.set = Cand.models.cawr, modnames = Modnames.cawr, second.ord = TRUE, method = "raw")
#dowo
print(aictab(cand.set = Cand.models.dowo, modnames = Modnames.dowo, second.ord = TRUE), digits = 4)
confset(cand.set = Cand.models.dowo, modnames = Modnames.dowo, second.ord = TRUE, method = "raw")
#gcfl
print(aictab(cand.set = Cand.models.gcfl, modnames = Modnames.gcfl, second.ord = TRUE), digits = 4)
confset(cand.set = Cand.models.gcfl, modnames = Modnames.gcfl, second.ord = TRUE, method = "raw")
#hawo
print(aictab(cand.set = Cand.models.hawo, modnames = Modnames.hawo, second.ord = TRUE), digits = 4)
confset(cand.set = Cand.models.hawo, modnames = Modnames.hawo, second.ord = TRUE, method = "raw")
#rbwo
print(aictab(cand.set = Cand.models.rbwo, modnames = Modnames.rbwo, second.ord = TRUE), digits = 4)
confset(cand.set = Cand.models.rbwo, modnames = Modnames.rbwo, second.ord = TRUE, method = "raw")
#tuti
print(aictab(cand.set = Cand.models.tuti, modnames = Modnames.tuti, second.ord = TRUE), digits = 4)
confset(cand.set = Cand.models.tuti, modnames = Modnames.tuti, second.ord = TRUE, method = "raw")
#wbnu
print(aictab(cand.set = Cand.models.wbnu, modnames = Modnames.wbnu, second.ord = TRUE), digits = 4)
confset(cand.set = Cand.models.wbnu, modnames = Modnames.wbnu, second.ord = TRUE, method = "raw")





