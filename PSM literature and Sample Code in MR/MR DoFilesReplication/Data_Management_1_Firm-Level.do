clear all
capture log close

set matsize 800
set more off


* set path
global path ""
cd $path

/*

This do-file

1) prepares the balance sheet data (data set FIRM is required)

2) prepares the trade data (data set UHDI is required)

3) combines the data sets, keeps retail firms, and prepares variabkes

*/



set linesize 200
log using Log\Data_Management_1_Firms.log, replace

************************************************
************************************************

**************** 1) Firm Data ******************

************************************************
************************************************


// Load data
// Rename variables
// Industry concordance (db93, db03)

* load data set containing firm characteristics called "firm"
use "G:\Data\Workdata\703989\STATA FILES\OLD VERSIONS\replaced in March 2017\firm2.dta", clear 

destring cvrnr, replace
format cvrnr %10.0f
destring aar, g(year)
drop aar
order cvrnr year

* renam variable
rename GF_OMS oms
rename GF_EKSP exp_firmstat
rename GF_AARSV empl_firm
rename GF_LGAGMV wage
rename GF_ATT capital
rename GF_AT balance_sum
rename GF_VTV va
rename GF_BRANCHE_03 ind_03
rename GF_BRANCHE_93 ind_93
rename GF_BRANCHE_07 ind_07
order cvrnr year ind_03 ind_93 ind_07 GF_BRANCHE
destring ind_* GF_BRANCHE, replace
sort cvrnr year
* note GF_BRANCHE combines ind_*
* ind_07: back to 2000
* ind_03: 2003-2008
* ind_93: 1993-2002
drop GF_BRANCHE

ren Start_dato date_start
ren Ophoer_dato date_resign
ren GF_KOM_KODE komkode
ren GF_KOM_KODE_2007 komkode_2007
ren GF_RFEP profits

order GF*, last
drop if year==1992

gen branche = ind_93
replace branche = ind_03 if branche ==.
tostring branche, replace
save Data\maj_firmastat_9308.dta, replace


* ind_07: back to 2000
* ind_03: 2003-2008
* ind_93: 1993-2002

*************************************
** Industry Codes
*************************************

use Data\maj_firmastat_9308.dta, clear

// industry concordance DB93 and DB03 for retail sector
tab year if ind_93!=.
tab year if ind_03!=.
tab year
su ind*
/*
	DB93	DB03
	
	521130	521130	Supermarkt
			521140	Discounter
			
	524420	524801	Carpets (Taeppeforretninger)
	
	524865	524866	Detailhandel med computer og standard software
			524867	Detailhandel med kontomaskiner
*/

* concord industry codes (to DB93)
count if ind_93==521130
count if ind_03==521130
count if ind_03==521140
replace ind_03 = 521130 if ind_03==521140

count if ind_93==524420
count if ind_03==524801
count if ind_03==524420
count if ind_93==524801
replace ind_03 = 524420 if ind_03==524801

count if ind_93==524865
count if ind_93==524866
count if ind_93==524867
count if ind_03==524865
count if ind_03==524866
count if ind_03==524867
replace ind_03 = 524865 if ind_03==524866
replace ind_03 = 524865 if ind_03==524867

drop branche
g branche = ind_93 if year<2003
replace branche = ind_03 if year>2002
tostring branche, replace

g ind4=substr(branche,1,4)
destring ind4, replace
label var ind4 "NACE 4-digit"
g ind2=substr(branche,1,2)
destring ind2, replace
label var ind2 "NACE 2-digit"

tab year
bys cvrnr: egen max_year=max(year)
tab max_year

save Data\maj_firmastat_9308_2.dta, replace

cap log close




set linesize 200
log using Log\Data_Management_2_trade.log, replace

************************************************
************************************************

**************** 2) Trade Data *****************

************************************************
************************************************


* load trade data set called "uhdi"
use "G:\data\workdata\703989\STATA FILES\uhdi.dta" ,clear

destring cvrnr, replace
format cvrnr %10.0f
destring aar, replace force
ren aar year
drop if cvrnr==.
destring senr, replace force
format senr %10.0f
drop if year < 1995
drop if year > 2008

save Data\uhdi_9508.dta, replace


/// Apply senr-cvrnr key 

* some cvrnr in uhdi are wrong, i.e. firms cannot be matched with firmstat data
* Use key provided by DST to correct these cvrnr
* --> merge key and replace cvrnr by cvrnr_key if cvrnr_key is non-missing

// load senr-cvrnr key		   
use "G:\Data\Workdata\703989\PHILIPP\General_Data\RawData\senr-cvrnr\senr-cvrnr_3.dta", clear		   
destring cvrnr, replace force
drop if cvrnr==.
destring senr, replace force
format cvrnr %10.0f
format senr %10.0f
drop if year < 1995
drop if year > 2008

merge 1:m senr year using Data\uhdi_9508.dta, update
drop if _merge==1

replace cvrnr = cvrnr_key if _merge==3 & cvrnr_key!=.

drop _merge cvrnr_key fra_d til_d

save Data\uhdi_9508_key.dta, replace


/// Deal with few doublicates by collapsing data set

use Data\uhdi_9508_key.dta, clear
su year vgt mgd vrd
count if vgt==0
count if vrd==0
count if mgd==0
tab year if vgt==0
drop if vrd==0
drop if vrd==.
bys cvrnr ie vare land year: g N=_N
tab N
tab year if N>1
collapse (sum) vgt mgd vrd, by(cvrnr ie vare land year)
tab year

g eu15=0
replace eu15 = 1 if land=="AT"
replace eu15 = 1 if land=="BE"
replace eu15 = 1 if land=="FI"
replace eu15 = 1 if land=="FR"
replace eu15 = 1 if land=="DE"
replace eu15 = 1 if land=="GR"
replace eu15 = 1 if land=="IE"
replace eu15 = 1 if land=="IT"
replace eu15 = 1 if land=="LU"
replace eu15 = 1 if land=="NL"
replace eu15 = 1 if land=="PT"
replace eu15 = 1 if land=="ES"
replace eu15 = 1 if land=="SE"
replace eu15 = 1 if land=="GB"

g eu25=0
replace eu25=1 if eu15==1
replace eu25 = 1 if land=="EE"
replace eu25 = 1 if land=="LV"
replace eu25 = 1 if land=="LT"
replace eu25 = 1 if land=="MT"
replace eu25 = 1 if land=="PL"
replace eu25 = 1 if land=="SK"
replace eu25 = 1 if land=="SI"
replace eu25 = 1 if land=="CZ"
replace eu25 = 1 if land=="HU"
replace eu25 = 1 if land=="CY"

g eu27=0
replace eu27=1 if eu25==1
replace eu27 = 1 if land=="BG"
replace eu27 = 1 if land=="RO"

* missing product codes
destring vare, gen(tmp) force
drop if tmp==.
drop tmp
save Data\uhdi_9508_key_2.dta, replace



use Data\uhdi_9508_key_2.dta, clear

destring ie, replace
g imp=vrd if ie==1
g exp=vrd if ie==2

g imp_vgt=vgt if ie==1
g exp_vgt=vgt if ie==2

collapse (sum) imp* exp* (mean) eu*, by(cvrnr vare land year)

save Data\uhdi_9508_key_3.dta, replace



/// HS codes to 6-digit level 

* hs6-level
use Data\uhdi_9508_key_3.dta, clear
drop if cvrnr==.
gen hs6=substr(vare,1,6)
destring hs6, replace force
drop if hs6==.
ren land iso2
collapse (sum) imp* exp* (max) eu* , by(cvrnr iso2 hs6 year)
sum imp* exp* year

save Data\uhdi_9508_key_3_hs6.dta, replace



/// Consumer goods

* merge bec classification by hs6 product code
use "G:\Data\Workdata\703989\PHILIPP\General_Data\Correspondence Tables\HS_SITC_BEC\HS96-BEC_v02.dta", clear
append using "G:\Data\Workdata\703989\PHILIPP\General_Data\Correspondence Tables\HS_SITC_BEC\HS02-BEC_v02.dta"
append using "G:\Data\Workdata\703989\PHILIPP\General_Data\Correspondence Tables\HS_SITC_BEC\HS07-BEC_v02.dta"
tab year
drop if year>2008
merge 1:m hs6 year using Data\uhdi_9508_key_3_hs6.dta, update
drop if _merge==1
tab year if _merge==2
drop if year<1995
drop if year>2008
tab hs6 if _merge==2
tab good_type
replace good_type=4 if _merge==2
drop _merge

// Prepare data for merge with BACI and correct some iso codes
* ComTrade combines some countries into groups:
* BEL and LUX grouped together under iso code BE/BEL 
* Switzerland (CH) and Liechtenstein (LI) grouped together under iso code CH/CHE 

replace iso2="BE" if iso2=="LU" // combine BEL and LUX
replace iso2="CH" if iso2=="LI" // combine CHE and LIE

replace iso2="RS" if iso2=="XS" // Serbia
replace iso2="ME" if iso2=="XM" // Montenegro
replace iso2="PS" if iso2=="XP" // Gaza

save Data\uhdi_9508_key_4_hs6.dta, replace



* hs6plus (concordance over time) - BACI based on HS96

use Data\uhdi_9508_key_4_hs6.dta, clear
merge m:1 hs6 year using "G:\Data\Workdata\703989\PHILIPP\General_Data\Correspondence Tables\HS6-over_time\Output\hs6_hs6plus_1996_2008.dta", update
drop if year<1996
drop if year>2008
tab year _merge
drop if _merge==2
tab hs6 if _merge==1
* note: hs product code starting with 99 usually some kind of "rest group"
* i.e. commodities not elsewhere specified (not internationally standardized) 
* e.g. 999099 = "Returvarer (Ikke prøve- el bestillingssvarende varer)"
* e.g. 991099 = "Fortrolige forsendelser"
* e.g. 993099 = "Proviantering med proviantbegæring"
* hence, okay to drop

* keep consumer goods only
tab good_type
keep if good_type==3
tab hs6 if _merge==1

keep if _merge==3
tab year

collapse (sum) imp* exp* (max) eu*, by(cvrnr iso2 hs6plus year)

sum exp* imp* year

order cvrnr iso2 hs6plus year

save Data\uhdi_9508_key_5_hs6plus.dta, replace

cap log close




set linesize 200
log using Log\Data_Management_4_combine.log, replace

************************************************
************************************************

**************** 3) Combine Data ***************

************************************************
************************************************


** (i)   Load Trade Data
** (ii)  Merge with Firm Data
** (iii) Firms in Analysis (retail sector)
** (iv)  Prepare Variables
** (v)   Clean Data


*************************************
** (i) Trade Data
*************************************
// imports
use Data\uhdi_9508_key_5_hs6plus.dta, clear
tab year
drop if cvrnr==.

g imp_it=imp
g imp_variety_hs6plus=1 if imp>0

g imp_it_neu15=imp_it 
replace imp_it_neu15=0 if eu15==1

g imp_variety_hs6plus_neu15=1 if imp>0 & eu15==0

collapse (sum) imp_it* imp_variety*, by(cvrnr year)

* collapse data set
sort cvrnr year
tab year
su
save Data\imp_firms_v01.dta, replace



*************************************
** (ii) Merge Data
*************************************
use Data\maj_firmastat_9308_2.dta, clear
tab year
drop if year>2008
drop if year<1996
ren empl_firm empl

// merge import data
merge 1:1 cvrnr year using Data\imp_firms_v01.dta,update
drop if year>2008
tab year _merge, mi
drop if _merge ==2
g d_imp=(imp_it!=. & imp_it>0)
g d_imp_neu15=(imp_it_neu15!=. & imp_it_neu15>0)
foreach var of varlist imp_* {
replace `var'=0 if _merge==1
}
drop _merge
tab year d_imp
tab year d_imp_neu15

g exp_it=exp_firmstat
g d_exp=(exp_it>0)
tab d_exp
tab d_imp
tab d_exp d_imp


order cvrnr cvrnr year branche ind4 ind2 empl wage* oms va capital balance_sum date_resign max_year d_imp* imp_* d_exp exp_it exp_firmstat ind_93 ind_03

save Data\firm_exp_imp_v01.dta, replace


*************************************
** (iii) Firms in Analysis
*************************************

use Data\firm_exp_imp_v01.dta, clear

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
*7) food
*replace ind_id=7 if ind4 >= 5221 & ind4<=5227
*8) pharma
*replace ind_id=8 if ind4 >= 5231 & ind4<=5233
*9) newspapers, books
*replace ind_id=9 if ind4 == 5247
* other
*replace ind_id=10 if ind4 == 5250
*replace ind_id=11 if ind4 >= 5261 & ind4<=5263
*replace ind_id=12 if ind4 >= 5271 & ind4<=5274
tab ind_id,mi

tab ind4 if ind_id==. & ind2==52

* imports and sales of other retail firms
preserve
g firm_r=1 if ind2==52
g firm_or=1 if ind_id==. & ind2==52
g imp_r=imp_it if ind2==52
g imp_or=imp_it if ind_id==. & ind2==52
g doms=oms-exp_f
g doms_r=doms if ind2==52
g doms_or=doms if ind_id==. & ind2==52
collapse (sum) firm_r firm_or imp_r imp_or doms_r doms_or, by(year)
g s_imp=imp_or/imp_r
g s_doms=doms_or/doms_r
g s_firm=firm_or/firm_r
list year firm_or s_*
restore

drop if ind_id==.
tab ind_id

save Data\firm_exp_imp_v02.dta,replace

tab year


******************************************
** (iv) Prepare Varaibles
******************************************

use Data\firm_exp_imp_v02.dta, clear
bys cvrnr: egen max_year2=max(year)

// 1) deflate values using CPI (from DST, 2000=100)
gen cpi=85.7 if year == 1993
replace cpi = 87.4 if year == 1994
replace cpi = 89.2 if year == 1995
replace cpi = 91.1 if year == 1996
replace cpi = 93.1 if year == 1997
replace cpi = 94.8 if year == 1998
replace cpi = 97.2 if year == 1999
replace cpi = 100.0 if year == 2000
replace cpi = 102.4 if year == 2001
replace cpi = 104.8 if year == 2002
replace cpi = 107.0 if year == 2003
replace cpi = 108.3 if year == 2004
replace cpi = 110.2 if year == 2005
replace cpi = 112.3 if year == 2006
replace cpi = 114.2 if year == 2007
replace cpi = 118.1 if year == 2008


// unit value index for imports of consumer goods (BEC, Eurostat)
g PI_imp=.
replace PI_imp=100 if year==1999 // no data for 1999, set it equal to 2000
replace PI_imp=100 if year==2000
replace PI_imp=100.952 if year==2001
replace PI_imp=100.196 if year==2002
replace PI_imp=97.723 if year==2003
replace PI_imp=101.503 if year==2004
replace PI_imp=102.384 if year==2005
replace PI_imp=105.319 if year==2006
replace PI_imp=99.849 if year==2007
replace PI_imp=102.553 if year==2008
replace PI_imp=100 if year<1999

g materials=oms-va 

* define some variables
g domsales= oms-exp_firmstat
count if domsales<0
g imp_share= imp_it/oms
g exp_share=exp_firmstat/oms
count if exp_share>1 & exp_share!=.
count if imp_share>1 & imp_share!=.

* store nominal values
foreach var of varlist oms* va* profits* domsales balance_sum* wage* capital {
	g `var'_nom=`var'
}

* deflate with cpi
foreach var of varlist oms* va* profits* materials* domsales balance_sum* wage* {
	replace `var' = `var'/cpi*100
}

// capital and materials deflator 1993-2008 (2000=100) 
* materials are deflated with CPI (instead PPI for materials)
merge m:1 year using "G:\Data\Workdata\703989\PHILIPP\General_Data\Deflators\PPI_DDK\NACE-2-digit\Price_Index_K_M.dta", update
drop if _merge==2
tab year if _merge==1
drop _merge

replace capital=capital/p_K*100

tab year ind_id

g imp_nom=imp_it
replace imp_it=imp_it/PI_imp*100

* log transform variables
global vars empl capital* materials* va* oms* balance_sum* profits* wage* domsales*
foreach var of varlist $vars {
	gen ln`var'=ln(`var')
}
g lnexp=ln(exp_it)
g lnimp=ln(imp_it)

* lag logged variables
global vars lnva* lncapital* lnmaterials* lnempl* d_exp lnexp lnoms* lnbalance_sum* lnprofits* lnwage* lndomsales* balance_sum profits domsales imp_it*
xtset cvrnr year, yearly
sort cvrnr year
foreach var of varlist $vars {
	gen l1`var'=l.`var'
}


destring branche, g(ind)
* new industry classification which appears more appropriate
cap drop ind5
g ind5=ind4
replace ind5 = ind if ind_id==6
replace ind5 =524810 if ind==524805 // combine groups on jewellery, watches, clocks
replace ind5 =524810 if ind==524815
replace ind5 =524865 if ind==524870 // Computer/ software & telecommunication equipment
replace ind5 =524875 if ind==524880 // combine groups on florists and plants/seeds


save Data\firm_exp_imp_v03.dta,replace


******************************************
** (v) Clean Data
******************************************

use Data\firm_exp_imp_v03.dta, clear

drop if year<1997
tab year

* clean main variables - growth rats
foreach v of varlist lnbalance_sum lndomsales lnprofits {
	xtset cvrnr year, yearly
	g d`v'=d.`v'
	g outlier_d`v'=.
	bys year: egen p50=pctile(d`v'), p(50)
	bys year: egen sd=sd(d`v')
	replace outlier_d`v'=1 if d`v'<p50-5*sd & d`v'!=.
	replace outlier_d`v'=1 if d`v'>p50+5*sd & d`v'!=.
	drop p50 sd
}

* clean main variables - labor productivity, wage share
g lprod=va/empl
g lnlprod=ln(lprod)
g wshare=wage_nom/oms_nom
su wshare,d
foreach v of varlist lnlprod wshare {
	xtset cvrnr year, yearly
	g outlier_`v'=.
	bys ind_id: egen p50=pctile(`v'), p(50)
	bys ind_id: egen sd=sd(`v')
	replace outlier_`v'=1 if `v'<p50-5*sd & `v'!=.
	replace outlier_`v'=1 if `v'>p50+5*sd & `v'!=.
	drop p50 sd
}

xtset cvrnr year, yearly
replace lnlprod=. if outlier_lnlprod==1
g l1lnlprod=l.lnlprod
replace wshare=. if outlier_wshare==1
su wshare,d
replace wshare=. if wshare>1
g l1wshare=l.wshare
* lagged values
xtset cvrnr year, yearly
foreach var of varlist lndomsales lnprofits lnbalance_sum {
	replace d`var'=. if outlier_d`var'==1
	g l1d`var'=l.d`var'
	g l2`var'=l.l1`var'
}

save Data\firm_exp_imp_v04.dta, replace



cap log close


exit, clear
