library(reshape2)

#Inventory of historic specimens

#Inventory 2021
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/SpecimenData/")
mus= read.csv("AlexanderSpecimens2021.csv")

#fix elevation
mus$Elevation_m_low= as.numeric(mus$Elevation_m_low)
mus$Elevation_m_up= as.numeric(mus$Elevation_m_up)
mus$elev= mus$Elevation_m_low

inds= !is.na(mus$Elevation_m_up)
mus$elev[inds]= (mus$Elevation_m_low[inds]+mus$Elevation_m_up[inds])/2

#drop juveniles
mus= mus[-grep("nymph", mus$Stage.Sex),]
mus= mus[-grep("exuvia", mus$Stage.Sex),]
mus= mus[-grep("juvenile", mus$Stage.Sex),]

mus1= mus[,c("SpeciesName","LocalityCode","DateCollected","Elevation..m.","elev","Stage.Sex","SpecimenCode")]

#mus1= mus1[which(mus1$elev %in% c(2195, 2591, 3048) ),]

#species
specs=c("conspersa", "simplex","corallipes","clavatus","dodgei","pellucida","sanguinipes")

mus1= mus1[which(mus1$SpeciesName %in% specs),]

#sites
locs.all=unique(mus$LocalityCode)

locs= c("us_co_boulder.chau.5800","us_co_boulder.2S.bureau.",
        "us_co_uofcolo.a-1.7000","us_co_uofcolo.a-1.7200","us_co_uofcolo.a-1.2195",
        "us_co_uofcolo.b-1.8500","us_co_uofcolo.b-1.W.8500", "us_co_uofcolo.b-1.1.5w",
        "us_co_uofcolo.c-1.10000","us_co_uofcolo.c-1.elk.10000","us_co_elk.meadows.mrs.3048",
        "us_co_uofcolo.d-1.12280", "us_co_uofcolo.d-1.3499", "us_co_niwot.alpine",
        "us_co_rollins.pass.11700", "us_co_rollins.pass.rd.9.2mi", 
        "us_co_mt.evans.12300.sumL.2N","us_co_mt.evans.12750.sumL", "us_co_mt.evans.12100.goliath", "us_co_mt.evans.11500.tline","us_co_mt.evans.12800.sumL",
        "us_co_sunshine-fourmile.can","us_co_sunshine.can.rd.6600"," us_co_sunshine.can.rd.7600","us_co_sunshine.can.rd.9.3.9000",
        "us_co_chicken.ranch.gulch.6700",
        "us_co_mont.alto.8500", "us_co_rainbow.Ls.9600", "us_co_rainbowL.11200", "us_co_pikes.pk.12000-13000")
mus1= mus1[which(mus1$LocalityCode %in% locs),]

#check
#"us_co_mont.alto.8500" #Mount Alto picnic area off of Switzerland Trail
#"us_co_pikes.pk.12000-13000" #pikes peak- clavatus and dodgei
#"us_co_rainbow.Ls.9400-9600" #dodgei
"us_co_rollins.pass.rd.E.11000" #clavatus and dodgei

#dates
dates= as.Date(mus1$DateCollected, format="%m/%d/%y")
mus1$year= as.numeric( format(dates, format="%Y") )
#fix 19xx years
inds= which(mus1$year>2017)
mus1$year[inds]= mus1$year[inds]-1000
#historic, current
mus1$period= "initial"
mus1$period[mus1$year>1990]= "resurvey"

#-------------
#assess what specimens have been measured
#load body size data
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
bs= read.csv("BodySize_sub_Apr2022.csv")

#match bs to museum code
mus1$Barcode= mus1$SpecimenCode
mus1$Barcode= as.numeric(sub("UCMC ", "", mus1$Barcode))
bs$Barcode= as.numeric(bs$Barcode)

#check that body size measurements match museum
#match1= match(bs$Barcode, mus1$Barcode)

#match
match1= match(mus1$Barcode, bs$Barcode)
mus1$measured= FALSE
mus1$measured[which(!is.na(match1))]= TRUE 
#-------------

#combine period and measured
mus1$per_meas= paste(mus1$period, mus1$measured, sep="_")

#summary
tab1= table(mus1[,c("SpeciesName","per_meas","LocalityCode")]) #"LocalityCode","Elevation..m."

#-------
#Figure out specimens to measure
#locations
tab1[1,1,]

#Historical
#Chat
tab1[, , "us_co_boulder.chau.5800"]
#measure: clavatus resurvey, sanguinipes initial and resurvey

#A1
tab1[, , "us_co_uofcolo.a-1.7000"]

#B1
tab1[, , c("us_co_uofcolo.b-1.8500")]+tab1[, , c("us_co_uofcolo.b-1.W.8500")]
#add conspersa, measure sanguinipes both time periods

#C1
tab1[, , "us_co_uofcolo.c-1.10000"]
#measure historic sanguinipes, but transient?

#D1
tab1[, , "us_co_niwot.alpine"]+tab1[, , c("us_co_uofcolo.d-1.12280")]+tab1[, , c("us_co_uofcolo.d-1.3499")]
mc= mus1[which(mus1$LocalityCode %in% c("us_co_niwot.alpine") & mus1$SpeciesName=="sanguinipes" & mus1$period=="initial"),]
#historic dodgei

#Mt. Evans
tab1[, , "us_co_mt.evans.11500.tline"]
tab1[, , "us_co_mt.evans.12100.goliath"]
tab1[, , "us_co_mt.evans.12300.sumL.2N"]
tab1[, , "us_co_mt.evans.12800.sumL"]
#"us_co_mt.evans.12100.goliath","us_co_mt.evans.11500.tline" dodgei
#"us_co_mt.evans.12800.sumL" clavatus

#Rollins: 
tab1[, , "us_co_rollins.pass.11700"]+tab1[, , "us_co_rollins.pass.rd.9.2mi"]
#more historical boulderensis, clavatus, and sanguinipes to measure
#but no modern

#Sunshine
tab1[, , "us_co_sunshine-fourmile.can"]+tab1[, , "us_co_sunshine.can.rd.6600"]+tab1[, , "us_co_sunshine.can.rd.9.3.9000"]

locs.sun=locs.all[which(grepl("sunshine", locs.all))]
mc= mus1[which(mus1$LocalityCode %in% locs.sun),]
table(mc[,c("SpeciesName","LocalityCode","period")])
#measure historic and modern?

#Chicken Gulch Ranch
tab1[, , "us_co_chicken.ranch.gulch.6700"]

mc= mus1[which(mus1$LocalityCode %in% c("us_co_chicken.ranch.gulch.6700") ),]
table(mc[,c("SpeciesName","LocalityCode","period")])
#measure more historic specimens
#collect more modern?

#other potential sites for future collections
loc=c("us_co_baldy.mtn.10800-11000","us_co_buchanan.E1080","us_co_buchanan.E11300+","us_co_greenhorn.mtn.12300-1240","us_co_mont.alto.8500","us_co_rmnp.little.horse.8700","us_co_pikes.pk.12900-13000","us_co_pikes.pk.12900.SW")
mc= mus1[which(mus1$LocalityCode %in% c(loc) ),]
table(mc[,c("SpeciesName","LocalityCode","period")])

#====================

#Add modern specimens?
#A. clavatus
#Baldy mountain: 60 initial
#Buchanan Pass: 30 initial
#Chicken  Ranch Gulch: 30 initial
#Mt. Evans: 192 initial, 21 resurvey
#Sunshine: 45 initial

#E. simplex
#Sunshine: 10 initial

#M. boulderensis
#Chicken Ranch Gulch: 30

#M. sanguinipes (low priority?)
#D1: 18 initial

#X. corallipes
#Baldy mountain: 31 initial
#Chicken Ranch Gulch: 50 initial
#Sunshine Canyon: 49 initial

#COLLECTING
#Sunshine (2317m): A. clavatus, E. simplex, X. corallipes
#Baldy (3322m): A. clavatus, X. corallipes
#Chicken Ranch Gulch (no longer viable?, 2042m): A. clavatus, M. boulderensis, X. corallipes
#Mount Evans (3505, 3688, 3883, 3993 initial only, 33901 31 initial, 75 recent): A. clavatus
#Buchanan Pass (tough to get to?): A. clavatus

#Baldy Mountain	Boulder County	39.988	-105.61	3290
#Up road 128 from Nederland

#----------------
#Further compare museum and body size
#museum summary
mus1$Sites=NA
mus1$Sites[mus1$LocalityCode %in% c("us_co_uofcolo.a-1.7000","us_co_uofcolo.a-1.7200","us_co_uofcolo.a-1.2195")]<-"A1"
mus1$Sites[mus1$LocalityCode %in% c("us_co_uofcolo.b-1.8500","us_co_uofcolo.b-1.W.8500", "us_co_uofcolo.b-1.1.5w")]<-"B1"
  mus1$Sites[mus1$LocalityCode %in% c("us_co_uofcolo.d-1.12280", "us_co_uofcolo.d-1.3499", "us_co_niwot.alpine")]<-"Niwot Ridge (D1)"
  mus1$Sites[mus1$LocalityCode %in% c("us_co_boulder.chau.5800","us_co_boulder.2S.bureau.")]<-"Chautauqua Mesa"
  mus1$Sites[mus1$LocalityCode %in% c("us_co_rollins.pass.11700", "us_co_rollins.pass.rd.9.2mi")]<-"Rollin's Pass"
  mus1$Sites[mus1$LocalityCode %in% c("us_co_mt.evans.12300.sumL.2N","us_co_mt.evans.12750.sumL","us_co_mt.evans.12800.sumL")]<-"Summit lake"
  mus1$Sites[mus1$LocalityCode %in% c("us_co_uofcolo.c-1.10000","us_co_uofcolo.c-1.elk.10000","us_co_elk.meadows.mrs.3048")]<-"C1"
  mus1$Sites[mus1$LocalityCode %in% c("us_co_chicken.ranch.gulch.6700")]<-"Chicken Ranch Gulch"
  mus1$Sites[mus1$LocalityCode %in% c("us_co_mt.evans.12100.goliath", "us_co_mt.evans.11500.tline")]<-"Mt. Evans"
  mus1$Sites[mus1$LocalityCode %in% c("us_co_sunshine-fourmile.can","us_co_sunshine.can.rd.6600"," us_co_sunshine.can.rd.7600","us_co_sunshine.can.rd.9.3.9000")]<-"Sunshine Canyon"

mus.tab= tab1= table(mus1[,c("SpeciesName","per_meas","Sites")]) 
mus.tab.melt= melt(mus.tab)

#prepare body size data for comparison
has.barcode= which(!is.na(bs$Barcode))
match1= match(bs$Barcode[has.barcode], mus1$Barcode)
bs$Museum<-FALSE
bs$Museum[has.barcode[which(!is.na(match1))]]<-TRUE
bs$per_mus= paste(bs$time, bs$Museum, sep="_")

bs.tab=table(bs[,c("Species","per_mus","Sites")])
bs.tab.melt= melt(bs.tab)

#fix species names and per_meas
specs.gs= c("A. conspersa", "E. simplex","X. corallipes","A. clavatus","M. dodgei","C. pellucida","M. sanguinipes")
mus.tab.melt$Species= specs.gs[match(mus.tab.melt$SpeciesName, specs)]

#species sites
bs.tab.melt$SpSi= paste(bs.tab.melt$Species, bs.tab.melt$Sites, sep="_")
mus.tab.melt$SpSi= paste(mus.tab.melt$Species, mus.tab.melt$Sites, sep="_")

#cast
bs.tab= dcast(bs.tab.melt, Species+Sites+SpSi~per_mus)
mus.tab= dcast(mus.tab.melt, Species+Sites+SpSi+SpeciesName~per_meas)

#merge museum and body size
bs.all= merge(bs.tab, mus.tab, by="SpSi")

#save
write.csv(bs.all, "MusBsCounts.csv")

##To measure
#Mt. Evans, A. clavatus, resurvey_FALSE 17
#Summit lake, A. clavatus, initial_FALSE 155

#A1, A. conspersa, initial_FALSE	resurvey_FALSE
#B1, A. conspersa, initial_FALSE	resurvey_FALSE
#C1, A. conspersa, initial_FALSE	resurvey_FALSE
#Chicken Ranch Gulch, A. conspersa, initial_FALSE	resurvey_FALSE
#Sunshine Canyon, A. conspersa, initial_FALSE	resurvey_FALSE

#Chicken Ranch Gulch, E. simplex, initial_FALSE	resurvey_FALSE

#A1, M. sanguinipes, initial_FALSE
#B1, M. sanguinipes, initial_FALSE	
#C1, M. sanguinipes, initial_FALSE
#Chautauqua Mesa, M. sanguinipes, initial_FALSE	resurvey_FALSE
#Chicken Ranch Gulch, M. sanguinipes, initial_FALSE	resurvey_FALSE
#Niwot Ridge (D1), M. sanguinipes, initial_FALSE	resurvey_FALSE
#Sunshine Canyon, M. sanguinipes, get modern specimens and measure initial?

#-------------
#Assemble list of specimens to measure
mus.meas= rbind(
#A. clavatus
mus1[which(mus1$SpeciesName=="clavatus" & mus1$Sites=="Mt. Evans" & mus1$per_meas=="resurvey_FALSE"),],
mus1[which(mus1$SpeciesName=="clavatus" & mus1$Sites=="Summit lake" & mus1$per_meas=="initial_FALSE"),],

#A. conspersa
mus1[which(mus1$SpeciesName=="conspersa" & mus1$Sites %in% c("A1","B1","C1","Chicken Ranch Gulch","Sunshine Canyon") & mus1$per_meas=="initial_FALSE"),],

#E. simplex
mus1[which(mus1$SpeciesName=="simplex" & mus1$Sites %in% c("A1","B1","C1","Chicken Ranch Gulch","Sunshine Canyon") & mus1$per_meas=="initial_FALSE"),],

#M. sanguinipes
mus1[which(mus1$SpeciesName=="sanguinipes" & mus1$Sites %in% c("A1","B1","C1") & mus1$per_meas=="initial_FALSE"),],
mus1[which(mus1$SpeciesName=="sanguinipes" & mus1$Sites %in% c("Chautauqua Mesa","Chicken Ranch Gulch","Niwot Ridge (D1)") & mus1$per_meas %in% c("initial_FALSE", "resurvey_FALSE")),]
) #end rbind

#write out
write.csv(mus.meas, "MuseumMeasure2022.csv")



