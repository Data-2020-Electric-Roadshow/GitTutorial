


clear all
capture log close
set more off
set matsize 8000


* set path
global path ""
cd $path


set linesize 200
log using Log\RegressionsResults_Table4_baseline.log, replace

/*
Do file produces results presented in tables 4 and 8 of the paper

These are common premia regressions with and without firm fixed effects
*/

use Data\firm_exp_imp_v04.dta, clear
tab year

* merge markups
merge 1:1 cvrnr year using Data\TFP_MU_adj.dta, update
drop if _merge==2
drop _merge

* merge number of shops
merge 1:1 cvrnr year using Data\NbShops2.dta, update
drop if _merge==2
drop _merge

tab d_imp d_imp_neu15, mi

global cvar l1lnempl 

* drop missings 
foreach v of varlist $cvar {
	drop if `v'==.
}

drop if year<1999
drop if outlier_lnlprod==1

egen indyr=group(ind5 year)


///////////////////////////////////

///		  	Baseline OLS  		///

//////////////////////////////////


preserve
drop if outlier_dlndomsales==1
drop if lndomsales==.
bys cvrnr: g N=_N
drop if N==1
reghdfe lndomsales d_imp_neu15 $cvar, absorb(indyr) cluster(cvrnr)
est store domsales_1
reghdfe lndomsales d_imp_neu15 $cvar, absorb(cvrnr indyr) cluster(cvrnr)
est store domsales_2
restore

preserve
drop if outlier_dlnprofits==1
drop if lnprofits==.
bys cvrnr: g N=_N
drop if N==1
reghdfe lnprofits d_imp_neu15 $cvar, absorb(indyr) cluster(cvrnr)
est store profits_1
reghdfe lnprofits d_imp_neu15 $cvar, absorb(cvrnr indyr) cluster(cvrnr)
est store profits_2
restore

preserve
drop if outlier_dlnmu==1
drop if lnmu==.
bys cvrnr: g N=_N
drop if N==1
reghdfe lnmu d_imp_neu15 $cvar, absorb(indyr) cluster(cvrnr)
est store mu_1
reghdfe lnmu d_imp_neu15 $cvar, absorb(cvrnr indyr) cluster(cvrnr)
est store mu_2
restore


preserve
drop if lnnbshops==.
bys cvrnr: g N=_N
drop if N==1
reghdfe lnnbshops d_imp_neu15 $cvar, absorb(indyr) cluster(cvrnr)
est store shops_1
reghdfe lnnbshops d_imp_neu15 $cvar, absorb(cvrnr indyr) cluster(cvrnr)
est store shops_2
restore

g newshop=0 if dlnnbshops!=.
replace newshop=1 if dlnnbshops>0 & dlnnbshops!=.
tab newshop, mi

preserve
drop if newshop==.
bys cvrnr: g N=_N
drop if N==1
reghdfe newshop d_imp_neu15 $cvar, absorb(indyr) cluster(cvrnr)
est store newshop_1
reghdfe newshop d_imp_neu15 $cvar, absorb(cvrnr indyr) cluster(cvrnr)
est store newshop_2
restore

preserve
drop if chain==.
bys cvrnr: g N=_N
drop if N==1
reghdfe chain d_imp_neu15 $cvar, absorb(indyr) cluster(cvrnr)
est store chain_1
reghdfe chain d_imp_neu15 $cvar, absorb(cvrnr indyr) cluster(cvrnr)
est store chain_2
restore


xml_tab domsales_1 profits_1 mu_1 , ///
 save(Log\Results_Table04_08_Baseline.xml) replace sheet(OLS_base_1) sd below stats(N r2_a r2) ///
 keep(d_imp_neu15 $cvar)

xml_tab domsales_2 profits_2 mu_2 , ///
 save(Log\Results_Table04_08_Baseline.xml) append sheet(OLS_base_2) sd below stats(N r2_a r2) ///
 keep(d_imp_neu15 $cvar)

xml_tab chain_1 newshop_1 , ///
 save(Log\Results_Table04_08_Baseline.xml) append sheet(OLS_chain_1) sd below stats(N r2_a r2) ///
 keep(d_imp_neu15 $cvar)

xml_tab chain_2 newshop_2, ///
 save(Log\Results_Table04_08_Baseline.xml) append sheet(OLS_chain_2) sd below stats(N r2_a r2) ///
 keep(d_imp_neu15 $cvar)

 

exit, clear
log close



