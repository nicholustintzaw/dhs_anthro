*** dhs_anthro
*** calculation of childhood nturition status syntex

*** written by Nicholus Tint Zaw


//##############################################################################//
//&&&&&&&&&&&&& (I) U5 CHILDREN NUTRITION STATUS VAR DATASET &&&&&&&&&&&&&&&&&&//
//##############################################################################//


*-------------------------------------------------------------------------------
** (1): USE ALL HH MEMBER DATASET - TO GET CHILDREN < 5 OF INTERVIEWED WOMEN **
*-------------------------------------------------------------------------------

use "${pr_person}/pr_person.dta", clear // change your diretory and pr dataset file name here to launch dataset
gen in_pr = 1
order in_pr
sort hv001 hv002

** (A): CONSTRUCTION OF INDIVIDUAL NUTRITIONAL STATUS INDICATORS VAR **

tab hv103, m // should be 1 - de facto
keep if hv103 == 1


tab hc70, m
tab hc70, nolab m

// WHO SD Z Score //
foreach var of varlist hc70 hc71 hc72 {
	gen `var'_rev = `var'
	replace `var'_rev = .m if `var' == 9999
	replace `var'_rev = .d if `var' == 9998 | `var' == 9996
	replace `var'_rev = .n if hv103 != 1 // keep only de facto children
	replace `var'_rev = `var'/100 if !mi(`var'_rev)
	order `var'_rev, after (`var')
	sum `var'_rev
}

lab var hc70_rev "HAZ Score WHO"
lab var hc71_rev "WAZ Score WHO"
lab var hc72_rev "WHZ Score WHO"

rename hc70_rev haz_dhs
rename hc71_rev waz_dhs
rename hc72_rev whz_dhs


foreach var of varlist haz_dhs waz_dhs whz_dhs {
	gen `var'_2sd = (`var' < -2 )
	replace `var'_2sd = .n if mi(`var')
	tab `var'_2sd, m
	
	gen `var'_3sd = (`var' < -3 )
	replace `var'_3sd = .n if mi(`var')
	tab `var'_3sd, m
	
	gen `var'_plus_2sd = (`var' > 2 )
	replace `var'_plus_2sd = .n if mi(`var')
	tab `var'_plus_2sd, m
}

lab def yesno 1"yes" 0"no"
foreach var of varlist *sd {
	lab val `var' yesno
	tab `var',m
}

// Stunting //
rename haz_dhs_2sd stunting
rename haz_dhs_3sd severe_stunting

lab var stunting "Proportion of Stuntend Chidlren - <-2 SD HAZ WHO"
lab var severe_stunting "Proportion of Severely Stuntend Chidlren - <-3 SD HAZ WHO"


// Wasting //
rename whz_dhs_2sd wasting 
rename whz_dhs_3sd severe_wasting 

lab var wasting "Proportion of Wasted Chidlren - <-2 SD WHZ WHO"
lab var severe_wasting "Proportion of Severely Wasted Chidlren - <-2 SD WHZ WHO"

// Underweight //
rename waz_dhs_2sd underweight
rename waz_dhs_3sd severe_underweight

lab var underweight "Proportion of Underweight Chidlren - <-2 SD WAZ WHO"
lab var severe_underweight "Proportion of Severely Underweight Chidlren - <-2 SD WAZ WHO"

** (B): CONSTRUCTION OF CIAF INDICATORS VAR (CIAF - Composite Index of Anthropometric Failure) **
// weblink >>> https://www.researchgate.net/publication/279884136_The_Composite_Index_of_Anthropometric_Failure_CIAF_An_Alternative_Indicator_for_Malnutrition_in_Young_Children

tab stunting, m
tab wasting, m
tab underweight, m

count if !mi(stunting) & !mi(wasting) & !mi(underweight) // 4,612

gen ciaf_sum = (stunting == 1 | wasting == 1 | underweight == 1)
replace ciaf_sum = .n if mi(stunting) | mi(wasting) | mi(underweight)
tab ciaf_sum, m
lab val ciaf_sum yesno

gen ciaf_type = (ciaf_sum == 0)
replace ciaf_type = 2 if wasting == 1 & underweight == 0 & stunting == 0
replace ciaf_type = 3 if wasting == 1 & underweight == 1 & stunting == 0
replace ciaf_type = 4 if wasting == 1 & underweight == 1 & stunting == 1
replace ciaf_type = 5 if wasting == 0 & underweight == 1 & stunting == 1 
replace ciaf_type = 6 if wasting == 0 & underweight == 0 & stunting == 1
replace ciaf_type = 7 if wasting == 0 & underweight == 1 & stunting == 0
replace ciaf_type = .n if mi(stunting) | mi(wasting) | mi(underweight)

lab def ciaf 1"No Failure" 2"Wasting Only" 3"Wasting & Underweight" ///
		4"Wasting, Stunting and Underweight" 5"Stunting and Underweight" ///
		6"Stunting Only" 7"Underweight Only"
lab val ciaf_type ciaf
tab ciaf_type, m

forvalues x = 1/7 {
	egen ciaf_type_`x' = anymatch(ciaf_type), values(`x')
	replace ciaf_type_`x' = .n if mi(ciaf_type) 
	tab ciaf_type_`x', m
}

lab var ciaf_sum "Proportion of children experiencing at least one type of anthropometric failure - CIAF"
lab var ciaf_type "Proportion of children experiencing anthropometric failure - CIAF in each group" 

lab var ciaf_type_1 "Proportion of children experiencing no anthropometric failure" 
lab var ciaf_type_2 "Proportion of children experiencing only wasting" 
lab var ciaf_type_3 "Proportion of children experiencing wasting & underweight" 
lab var ciaf_type_4 "Proportion of children experiencing wasting & underweight & stunting" 
lab var ciaf_type_5 "Proportion of children experiencing stunting & underweight" 
lab var ciaf_type_6 "Proportion of children experiencing only stunting" 
lab var ciaf_type_7 "Proportion of children experiencing only underweight" 


** MUAC - Wasting **
lookfor muac

tab shcac1, m
tab shcac1, m nolab

gen child_muac_sd = shcac1
replace child_muac_sd = .m if shcac1 > 473 & !mi(shcac1)
replace child_muac_sd = .n if hv103 != 1
replace child_muac_sd = child_muac_sd/100 if !mi(child_muac_sd)
lab var child_muac_sd "Child MUAC SD"
tab child_muac_sd, m

gen child_muac_sd_m2		= (child_muac_sd < -2) 
replace child_muac_sd_m2 	= .n if mi(child_muac_sd)
lab var child_muac_sd_m2 "Children MUAC SD < -2"
tab  child_muac_sd_m2, m

gen child_muac_sd_m3		= (child_muac_sd < -3) 
replace child_muac_sd_m3 	= .n if mi(child_muac_sd)
lab var child_muac_sd_m3 "Children MUAC SD < -3"
tab  child_muac_sd_m3, m


tab sh207a, m
tab sh207a, m nolab 

gen child_muac = sh207a 
replace child_muac = .n if mi(sh207a)
replace child_muac = .d if sh207a == 994 | sh207a == 999 | sh207a == 996
replace child_muac = .r if sh207a == 995
replace child_muac = .n if hv103 != 1
lab var child_muac "Child MUAC in mm"
tab child_muac, m

gen wasting_muac		= (child_muac < 125)
replace wasting_muac	= .n if mi(child_muac)
lab var wasting_muac "Wasting by MUAC"
tab wasting_muac, m

gen wasting_muac_sev		= (child_muac < 115)
replace wasting_muac_sev	= .n if mi(child_muac)
lab var wasting_muac_sev "Severe Wasting by MUAC"
tab wasting_muac_sev, m

** (C) Construct WEIGHT var using Women Insidividual Sample Weight **
gen wgt = hv005/1000000
tab wgt, m

lookfor sampling // search for samping unit var

** (D) Analysis on Stunting & HAZ Score **
// note: population size should be matched with # from DHS report.


svyset, clear
svyset [pw = wgt], psu(hv021) strata (hv022)
svy: mean haz_dhs* *stunting 
svy: mean whz_dhs* *wasting 
svy: mean waz_dhs* *underweight 
svy: mean ciaf*  
svyset, clear

*-------------------------------------------------------------------------------
** (2): PREPARE THE COVARIATES FOR ANALYSIS **
*-------------------------------------------------------------------------------

** (A.1) Geographical Location ** // this will be varied based on your study country
// create dummies var for each state/region var
tab hv024, m
tab hv024, m nolab

gen state_region_name = hv024
lab def state_region_name ///
1 "Kachin" ///
2 "Kayah" ///
3 "Kayin" ///
4 "Chin" ///
5 "Sagaing" ///
6 "Taninthayi" ///
7 "Bago" ///
8 "Magway" ///
9 "Mandalay" ///
10 "Mon" ///
11 "Rakhine" ///
12 "Yangon" ///
13 "Shan" ///
14 "Ayeyarwaddy" ///
15 "Naypyitaw"
lab val state_region_name state_region_name

forvalues x = 1/15 {
	egen region_`x' = anymatch(hv024), values(`x')
	replace region_`x' = .n if mi(hv024) 
	tab region_`x', m
}

rename region_1 region_kachin
rename region_2 region_kayah
rename region_3 region_kayin
rename region_4 region_chin
rename region_5 region_sagaing
rename region_6 region_taninthayi
rename region_7 region_bago
rename region_8 region_magway
rename region_9 region_mandalay
rename region_10 region_mon
rename region_11 region_rakhine
rename region_12 region_yangon
rename region_13 region_shan
rename region_14 region_ayeyarwaddy
rename region_15 region_naypyitaw

// create dummies var to identify state/region
gen state_region = 0
lab def state_region 1"State" 0"Region"
foreach num of numlist 1/4 10/11 13 {
	replace state_region = 1 if hv024 == `num'
	replace state_region = .n if mi(hv024) 
	lab val state_region state_region
	tab state_region, m

}
// create dummies var to identify rural/urban
gen rural_urban 	= (hv025 == 1)
//replace rural_urban = 0 if hv025 == 1
lab def rural_urban 1"Urban" 0"Rural"
lab val rural_urban rural_urban
tab rural_urban, m


** (A.2) Child Age Group - Breakdown **
gen child_valid_age		= hc1
replace child_valid_age = .n if mi(hc1)

recode child_valid_age	(0/5 = 1 "0-5")(6/8 = 2 "6-8")(9/11 = 3 "9-11")(12/17 = 4 "12-17") ///
						(18/23 = 5 "18-23")(24/35 = 6 "24-35")(36/47 = 7 "36-47") ///
						(48/59 = 8 "48-59"), gen(child_age_grp)

recode child_valid_age	(0/5 = 1 "0-5")(6/11 = 2 "6-11")(12/23 = 3 "12-23")(24/59 = 4 "24-59"), gen(child_age_grp_2)
recode child_valid_age	(0/23 = 3 "0-23")(24/59 = 4 "24-59"), gen(child_age_grp_3)
recode child_valid_age 	(3/6 = 1 "3-6")(7/11 = 2 "7-11")(12/23 = 3 "12-23")(24/35 = 4 "24-35") ///
						(36/47 = 5 "36-47")(48/59 = 6 "48-59")(0/2 = .n), gen(child_age_grp_4)

foreach var of varlist *_dhs {
mean `var' [iw = wgt], over(child_age_grp)
}

foreach var of varlist haz_dhs_* *stunting whz_dhs_* *wasting waz_dhs_* *underweight {
	tab child_age_grp `var' [iw = wgt], row
}

foreach var of varlist child_muac_sd {
mean `var' [iw = wgt], over(child_age_grp_4)
tab child_age_grp_4 child_muac_sd_m2 [iw = wgt], row 
tab child_age_grp_4 wasting_muac_sev [iw = wgt], row 
tab child_age_grp_4 wasting_muac [iw = wgt], row 
}


forvalues x = 1/8 {
	egen childage_`x' = anymatch(child_age_grp), values(`x')
	replace childage_`x' = .n if mi(child_age_grp) 
	tab childage_`x', m
}
rename childage_1 childage_0_5
rename childage_2 childage_6_8
rename childage_3 childage_9_11
rename childage_4 childage_12_17
rename childage_5 childage_18_23
rename childage_6 childage_24_35
rename childage_7 childage_36_47
rename childage_8 childage_48_59

** (A.3) Socio-Economic Status **
// create dummies var to identify respondent native language
gen native_language		= (hv045c == 6)
//replace native_language = 0 if hv045c == 1
lab def native 0"Myanmar" 1"Other"
lab val native_language native
tab native_language, m

// create dummies var for wealth index var
tab hv270, m
tab hv270, nolab m // wealth index

gen wealth_index = hv270
lab var wealth_index "HH Wealth Index"
lab def wealth_index 1"Poorest" 2"Poorer" 3"Middle" 4"Richer" 5"Richest"
lab val wealth_index wealth_index
tab wealth_index,m

forvalues x = 1/5 {
	egen wi_`x' = anymatch(hv270), values(`x')
	replace wi_`x' = .n if mi(hv270) 
	tab wi_`x', m
	lab val wi_`x' yesno
}
rename wi_1 index_poorest
rename wi_2 index_poorer
rename wi_3 index_middle
rename wi_4 index_richer
rename wi_5 index_richest

mean haz_dhs [iw = wgt], over(hv270)
tab hv270 stunting [iw = wgt], row
tab hv270 severe_stunting [iw = wgt], row 

mean child_muac_sd [iw = wgt], over(hv270)
tab hv270 child_muac_sd_m2 [iw = wgt], row 

// create dummies var for mother education level
tab hc61, m
tab hc61, nolab m

gen mom_edu = hc61
//replace mom_edu = .d if hc61 == 8
replace mom_edu = .m if hc61 == 9
lab var mom_edu "Mother Education Level - highest attained"
lab def mom_edu 0"No education" 1"Primary" 2"Secondary" 3"Higher"
lab val mom_edu mom_edu
tab mom_edu, m

mean haz_dhs [iw = wgt], over(mom_edu)
tab mom_edu stunting [iw = wgt], row
tab mom_edu severe_stunting [iw = wgt], row // one obs not matched in higher education group

forvalues x = 0/3 {
	egen mom_edu_`x' = anymatch(mom_edu), values(`x')
	replace mom_edu_`x' = .n if mi(mom_edu) 
	tab mom_edu_`x', m
}
rename mom_edu_0 mom_edu_no
rename mom_edu_1 mom_edu_primary
rename mom_edu_2 mom_edu_secondary
rename mom_edu_3 mom_edu_higher


** (A.4) Child Related Info **
// create dummies var for birth interval 
tab hc63, m
recode hc63		(1/23 = 1 "<24 months")(24/47 = 2 "24-47 months")(48/226 = 3 "48+ months") ///
				(999 = .d)(.n = 0 "First Birth"), gen(birth_interval)
tab birth_interval, m

tab birth_interval stunting [iw = wgt], row

forvalues x = 0/3 {
	egen birth_interval_`x' = anymatch(birth_interval), values(`x')
	replace birth_interval_`x' = .n if mi(birth_interval) 
	tab birth_interval_`x', m
}
rename birth_interval_0 binter_first
rename birth_interval_1 binter_less_24
rename birth_interval_2 binter_24_47
rename birth_interval_3 binter_more47

// create dummies var for child sex 
gen child_sex		= hc27
replace child_sex	= 0 if hc27 == 2
lab def child_sex 1"Male" 0"Female"
lab val child_sex child_sex
tab child_sex,m

tab child_sex stunting [iw = wgt], row

** (A.5) Child Birth Order **
// create dummies var for birth order 

** CHECK AGAIN THIS VARIABLE DEVELOPMENT WITH REFERENCE CODING GUIDELIN DHS

tab hc64, m

gen child_birthorder = hc64
replace child_birthorder = .n if mi(hc64)
tab child_birthorder, m

gen birthorder_1st = (child_birthorder == 1)
replace birthorder_1st = .n if mi(child_birthorder) // assume 99 (don't know) as later birth order children
lab def birthorder_1st 1"first born child" 0"not first born child"
lab val birthorder_1st birthorder_1st
tab birthorder_1st, m

tab stunting, m

tab child_birthorder stunting [iw = wgt], row
tab birthorder_1st stunting [iw = wgt], row

