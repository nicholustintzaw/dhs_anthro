# dhs_anthro


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

