

clear all
capture log close

* set path
global path ""
cd $path


/*

This do-file

1) prepares market-level data (data set IDAS is required)

2) computes number of shops by firms

3) prepares sector-level imports


*/



set linesize 200
log using Log\Data_Management_5_Ansat.log,replace


************************************************
************************************************

************* 1) Workplace Data ****************

************************************************
************************************************

/* 
data required to 
- define local markets
- compute number of shops by firm

required steps:
- concord municipality codes over time
- combine data sets - (idas and firm)
- compute number of shops by firm (to define chains)
*/


// Condord municipalities
* load concordance table
use "G:\Data\Workdata\703989\PHILIPP\General_Data\Other General Data Files\noegle_ny_gammel_kommune_1-2.dta", clear

su komgl komny
bys komgl: g N=_N
tab N
order N
sort N komgl komny

* codes above 900 are new codes due to unclear concordance between komgl (old codes) and komny (new codes)

g komny2=komny if N==1

replace komny2=900 if komny==219 | komny==250 // komgl 233

replace komny2=901 if komny==510 | komny==621 // komgl 509
replace komny2=901 if komny==510 | komny==550 // komgl 525
replace komny2=901 if komny==621 | komny==630 // komgl 605
replace komny2=901 if komny==630 | komny==766 // komgl 627

replace komny2=902 if komny==561 | komny==573 // komgl 567

replace komny2=903 if komny==615 | komny==746 // komgl 601

replace komny2=904 if komny==710 | komny==730 // komgl 717
replace komny2=904 if komny==730 | komny==846 // komgl 719
replace komny2=904 if komny==707 | komny==730 // komgl 747
replace komny2=904 if komny==840 | komny==846 // komgl 833
replace komny2=904 if komny==791 | komny==820 | komny==846 // komgl 793

su komny2 komny komgl
bys komgl komny2: keep if _n==1
keep komgl komny2

save Data\komgl_komny2.dta, replace



// Load workplace data called idas
use "G:\Data\Workdata\703989\STATA FILES\idas.dta" , clear

cap destring aar , replace force
ren aar year
drop if year<1997
drop if year>2008

cap ren arb_kom kom

keep cvrnr year lbnr arbgnr kom antaar antnov lonaar longns

destring cvrnr, replace
format cvrnr %10.0f
save Data\Herf_Workplace0.dta, replace



// get firm-level information on komkode and branches
** merge firm-level information to idas

* retail firms with main activity acc. to firm
use Data\firm_exp_imp_v03.dta, clear
count if ind4==.
tab ind_id, mi
* mark-ups
merge 1:1 cvrnr year using Data\TFP_MU_adj.dta, update keepusing(mu)
drop if _merge==2
drop _merge
* labor productivity
merge 1:1 cvrnr year using Data\firm_exp_imp_v04.dta, update keepusing(lprod lnlprod outlier_*)
drop if _merge==2
drop _merge

merge 1:m cvrnr year using Data\Herf_Workplace0.dta, update
tab year ind_id if _merge==3, mi
tab year ind_id if _merge==1, mi
tab year if _merge==2, mi
* drop firms that are not part of firm data (maybe inactive?!)
drop if _merge==2
tab year _merge

// check missings due to merge
preserve
bys cvrnr year: keep if _n==1
su exp_f
g oms_m=oms if _merge==3
g oms_nm=oms if _merge==1
g m=1 if _merge==3
g nm=1 if _merge==1
collapse (sum) oms* m nm, by(year)
g s_m=oms_m/oms
g s_nm=oms_nm/oms
drop if year<1999
list year s_m s_nm m nm
tostring year, replace
export excel year s_m m nm using "Log\Sector_Stats.xls" , sheet(Merge_Ansat_1) firstrow(variables) sheetreplace
restore

preserve
bys cvrnr year: keep if _n==1
drop if ind5<5220
su exp_f
g oms_m=oms if _merge==3
g oms_nm=oms if _merge==1
g m=1 if _merge==3
g nm=1 if _merge==1
collapse (sum) oms* m nm, by(year)
g s_m=oms_m/oms
g s_nm=oms_nm/oms
drop if year<1999
list year s_m s_nm m nm
tostring year, replace
export excel year s_m m nm using "Log\Sector_Stats.xls" , sheet(Merge_Ansat_2) firstrow(variables) sheetreplace
restore

g drop_obs=1 if _merge==1
tab year drop_obs
drop _merge
cap drop N*
bys cvrnr year: g N=_N
order cvrnr year N kom komkode ind* antaar antnov lonaar longns

save Data\Herf_Workplace.dta, replace



// concord municipality data

use Data\Herf_Workplace.dta, clear

* use information from firmstat (komkode) to impute missing / non-
* existing municipalities in case of single shop firms in idas (kom)
destring kom komkode, replace
su kom komkode year
tab kom if kom>900
tab N if kom==.
replace kom=komkode if kom==. & N==1
replace kom=komkode if kom==950 & N==1
replace kom=komkode if kom==999 & N==1
replace kom=komkode if kom==401 & N==1
replace kom=komkode if kom==403 & N==1
replace kom=komkode if kom==405 & N==1
replace kom=komkode if kom==407 & N==1
replace kom=komkode if kom==409 & N==1
replace kom=komkode if kom==492 & N==1
drop komkode

* merge new municipality codes to old codes (new code available after 2006)
g komgl=kom if year<2007

merge m:1 komgl using Data\komgl_komny2.dta, update
tab year if _merge==1
tab komgl if _merge==1 & year<2007, mi
replace komgl=. if _merge==1 & year<2007

tab komny if _merge==1 & year<2007, mi
replace komny2=kom if year>2006
replace komny2=. if komny2==950
g komny3=kom if year>2006

* aggregate some municipalities acc. to new komkode
replace komny2=900 if komny3==219 | komny3==250 // komgl 233
replace komny2=901 if komny3==510 | komny3==621 // komgl 509
replace komny2=901 if komny3==510 | komny3==550 // komgl 525
replace komny2=901 if komny3==621 | komny3==630 // komgl 605
replace komny2=901 if komny3==630 | komny3==766 // komgl 627
replace komny2=902 if komny3==561 | komny3==573 // komgl 567
replace komny2=903 if komny3==615 | komny3==746 // komgl 601
replace komny2=904 if komny3==710 | komny3==730 // komgl 717
replace komny2=904 if komny3==730 | komny3==846 // komgl 719
replace komny2=904 if komny3==707 | komny3==730 // komgl 747
replace komny2=904 if komny3==840 | komny3==846 // komgl 833
replace komny2=904 if komny3==791 | komny3==820 | komny3==846 // komgl 793
drop komny3

* interpolate missing municipality codes
tab year if komny2==.
* use mode of existing municipality by lbnr
bys lbnr: egen tmp=mode(komny2)
count if komny2==. & tmp!=. & year>1998
replace komny2=tmp if komny2==. & year>1998 // only few cases
tab year if komny2==.
drop tmp*

replace drop_obs=1 if komny2==.
tab year drop_obs,mi
drop kom komgl _merge

save Data\Herf_Workplace_3.dta, replace



// prepare variables

use Data\Herf_Workplace_3.dta, clear
tab year

* use share of total employment by shop in case of chains 
* to distribute firm-level turnover and imports across shops
replace oms=domsales
replace drop_obs=1 if oms==. | oms<=0

su antaar lonaar year
replace drop_obs=1 if antaar==0 | antaar==.

replace drop_obs=0 if drop_obs==.
tab year drop_obs

drop if drop_obs==1

save Data\Herf_Workplace_4.dta, replace




************************************************
************************************************

************* 2) Number of Shops ***************

************************************************
************************************************


// Compute number of shops
// Number of shops in different regions

use Data\Herf_Workplace_4.dta, clear
tab year
g nbshops=1
collapse (sum) nbshops, by(cvrnr komny2 year)

g nbshops2=1
collapse (sum) nbshops nbshops2, by(cvrnr year)

su nbshops*,d

label var nbshops "number of shops - total"
label var nbshops2 "number of shops - different regions"

tab year
* shops and chains
g lnnbshops=ln(nbshops)
g chain=(nbshops>1)
replace chain=. if nbshops==.
tab year chain
g outlier_dlnnbshops=.
g outlier_dchain=.
xtset cvrnr year, yearly
g dlnnbshops=d.lnnbshops	
g l1nbshops=l.nbshops
g l2nbshops=l2.nbshops
g l1lnnbshops=l.lnnbshops
g l1chain=l.chain
g l2lnnbshops=l2.lnnbshops
g l2chain=l2.chain
g l1dlnnbshops=l.dlnnbshops	

save Data\NbShops2.dta, replace

tab year

cap log close





set linesize 200
log using Log\Data_Management_5_SecImp.log,replace


************************************************
************************************************

********** 3) Sector-level Imports *************

************************************************
************************************************


*** Imports at sector-level ***

* i.  direct sector-level imports
* ii. proxy for indirect imports (used in an robustness exercise in the appendix)
 

// i.  direct sector-level imports


use Data\uhdi_9508_key_5_hs6plus.dta, clear
drop if imp==0
drop if imp==.
tab iso2 if eu15==1
* focus on non-EU15 imports
keep if eu15==0

merge m:1 cvrnr year using Data\maj_firmastat_9308_2.dta, update keepusing(ind4 branche)
drop if _merge==2
drop _merge

destring branche, g(ind) force

*1) textiles --> NACE4: 5241-5243
gen ind_id=1 if ind4 >= 5241 & ind4 <=5243
*2) furniture --> NACE4: 5244
replace ind_id=2 if ind4 == 5244
*3) electrial household appliances --> NACE4: 5245
replace ind_id=3 if ind4 == 5245
*4) hardware --> NACE4: 5246
replace ind_id=4 if ind4 == 5246
*5) retail sale in non-specialized stores (department stores)
replace ind_id=5 if ind4 >= 5211 & ind4<=5212
*6) others retail sale in specialized stores
replace ind_id=6 if ind4 >= 5248 & ind4<=5249

tab ind4 if ind_id!=.
g ind5=ind4
replace ind5 = ind if ind_id==6
replace ind5 =524810 if ind==524805 // combine groups on jewellery, watches, clocks
replace ind5 =524810 if ind==524815
replace ind5 =524865 if ind==524870 // Computer/ software & telecommunication equipment
replace ind5 =524875 if ind==524880 // combine groups on florits and planst/seeds
tab ind5 if ind_id!=.

tab ind_id,mi
order hs ind5
save Data\Imp_Sector_Retail_1.dta, replace


// sector-level imports of retailers
use Data\Imp_Sector_Retail_1.dta, clear
* drop imports by non-retailers
drop if ind_id==.

* collapse imports to sector level
collapse (sum) imp, by(ind5 year)
drop if imp==0
ren imp imp_r

* prepare variables
tab year
xtset ind year, yearly
foreach v of varlist imp_r {
	g ln`v'=ln(`v')
}

save Data\Imp_Sector_Retail_2.dta, replace



// ii. proxy for indirect imports (only used in appendix)


** Step 1: share of imported varietiey by retail sector

use Data\Imp_Sector_Retail_1.dta, clear
* drop imports by non-retailers
drop if ind_id==.

collapse (sum) imp, by(ind5 iso2 hs6pl year)

* compute shares
bys ind5 year: egen imp_tot=total(imp)

g sh_tot=imp/imp_tot

* check
bys ind5 year: egen tmp=total(sh_tot)
su tmp,d

keep ind5 iso2 hs6pl year sh_

save Data\SecImpShare.dta, replace



** Step 2: imports by non-retailers

use Data\Imp_Sector_Retail_1.dta, clear
* drop imports by retailers
drop if ind_id!=.

collapse (sum) imp, by(iso2 hs6pl year)

merge 1:m iso2 hs6pl year using Data\SecImpShare.dta, update
drop if _merge==1
drop _merge

g imp_w=sh_tot*imp
collapse (sum) imp_w, by(ind5 year)

su imp*,d

tab year
xtset ind year, yearly
foreach v of varlist imp_w {
	g ln`v'=ln(`v')
}

save Data\Imp_Sector_NonRetail.dta, replace



cap log close
exit, clear
