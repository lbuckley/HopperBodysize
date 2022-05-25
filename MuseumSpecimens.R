
#load body size data
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
bs= read.csv("BodySize_all_Apr2022.csv")

#load museum data
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/SpecimenData/")
mus= read.csv("AlexanderSpecimens2021.csv")

#species
specs=c("simplex","corallipes","clavatus","dodgei","pellucida","sanguinipes")
mus1= mus[which(mus$SpeciesName %in% specs),]

#sites
locs= c("us_co_boulder.chau.5800","us_co_boulder.2S.bureau.","us_co_uofcolo.a-1.7000","us_co_uofcolo.a-1.7200","us_co_uofcolo.a-1.2195","us_co_uofcolo.b-1.8500",
        "us_co_uofcolo.b-1.W.8500", "us_co_uofcolo.b-1.1.5w","us_co_uofcolo.d-1.12280", 
        "us_co_uofcolo.d-1.3499","us_co_uofcolo.c-1.10000","us_co_uofcolo.c-1.elk.10000","us_co_elk.meadows.mrs.3048",
        "us_co_rollins.pass.11700", "us_co_rollins.pass.rd.9.2mi", "us_co_mt.evans.12300.sumL.2N",
        "us_co_mt.evans.12750.sumL", "us_co_mt.evans.12100.goliath")
#mus1= mus1[which(mus1$LocalityCode %in% locs),]

#Historical
#Chat
mc1= mus1[which(mus1$LocalityCode=="us_co_boulder.chau.5800" & mus1$SpeciesName=="sanguinipes"),]
#& mus1$period=="initial"
#sanguinipes: 13 measured, 46 available

#A1
mc2= mus1[which(mus1$LocalityCode=="us_co_uofcolo.a-1.7000" & mus1$SpeciesName=="corallipes" & mus1$period=="initial"),]
mc3= mus1[which(mus1$LocalityCode=="us_co_uofcolo.a-1.7000" & mus1$SpeciesName=="clavatus" & mus1$period=="initial"),]
mc4= mus1[which(mus1$LocalityCode=="us_co_uofcolo.a-1.7000" & mus1$SpeciesName=="sanguinipes" & mus1$period=="initial"),]
#corallipes: 20 measured, 61 available
#clavatus: 11 measured, 22 available
#sanguinipes: 12 measured, 40 available

#B1
mc= mus1[which(mus1$LocalityCode %in% c("us_co_uofcolo.b-1.8500","us_co_uofcolo.b-1.W.8500") & mus1$SpeciesName=="sanguinipes" & mus1$period=="initial"),]
mc= mus1[which(mus1$LocalityCode %in% c("us_co_uofcolo.b-1.8500","us_co_uofcolo.b-1.W.8500") & mus1$SpeciesName=="simplex" & mus1$period=="initial"),]
#sanguinipes: 16 measured, 63 available
#simplex: 15 f measured, 77 available

#C1
mc= mus1[which(mus1$LocalityCode %in% c("us_co_uofcolo.c-1.10000") & mus1$SpeciesName=="sanguinipes" & mus1$period=="initial"),]
mc= mus1[which(mus1$LocalityCode %in% c("us_co_uofcolo.c-1.10000") & mus1$SpeciesName=="pellucida" & mus1$period=="initial"),]
#pellucida: 39 measured, 58 available
#sanguinipes: 0 measured, 26 available

#D1
mc= mus1[which(mus1$LocalityCode %in% c("us_co_niwot.alpine") & mus1$SpeciesName=="sanguinipes" & mus1$period=="initial"),]
#some historic sanguinipes available

#Mt. Evans
mc= mus1[which(mus1$LocalityCode %in% c("us_co_mt.evans.11500.tline","us_co_mt.evans.12100.goliath","us_co_mt.evans.12800.sumL") ),]
table(mc[,c("SpeciesName","LocalityCode","period")])
#clavatus: 31 hist measured 
#boulderensis: 47 current measured, 30 hist measured
#more current and historical clavatus and boulderensis to measure
#historical sanguinipes to measure, collect current?

#Rollins: 
mc= mus1[which(mus1$LocalityCode %in% c("us_co_rollins.pass.11700","us_co_rollins.pass.rd.15.2mi","us_co_rollins.pass.rd.E.11000","us_co_rollins.pass.rd.E.11200") ),]
table(mc[,c("SpeciesName","LocalityCode","period")])
#more historical boulderensis, clavatus, and sanguinipes to measure

#Sunshine
mc= mus1[which(mus1$LocalityCode %in% c("us_co_sunshine-fourmile.can","us_co_sunshine.can.rd.6600"," us_co_sunshine.can.rd.7600","us_co_sunshine.can.rd.9.3.9000") ),]
table(mc[,c("SpeciesName","LocalityCode","period")])
#check out modern?

#Chicken Gulch Ranch
mc= mus1[which(mus1$LocalityCode %in% c("us_co_chicken.ranch.gulch.6700") ),]
table(mc[,c("SpeciesName","LocalityCode","period")])
#many historic and some modern available
#check out modern?

#other potential sites for future collections
loc=c("us_co_baldy.mtn.10800-11000","us_co_buchanan.E1080","us_co_buchanan.E11300+","us_co_greenhorn.mtn.12300-1240","us_co_mont.alto.8500","us_co_rmnp.little.horse.8700","us_co_pikes.pk.12900-13000","us_co_pikes.pk.12900.SW")
mc= mus1[which(mus1$LocalityCode %in% c(loc) ),]
table(mc[,c("SpeciesName","LocalityCode","period")])

