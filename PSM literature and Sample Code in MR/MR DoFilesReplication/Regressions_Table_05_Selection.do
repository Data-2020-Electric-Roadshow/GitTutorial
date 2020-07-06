



clear all
capture log close
set more off
set matsize 8000


* set path
global path ""
cd $path


set linesize 200
log using Log\RegressionsResults_Table_05_Selection.log, replace

/*
Do file produces results presented in table 5 of the paper

1) merge data sets also used for propensity matching

2) identify import starters and non-importers

3) compare performance measures before and after import event, using OLS
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

tab year d_imp_neu15, mi

g outlier_dnbshops=.

save Data\PSP_Sample.dta, replace


use Data\PSP_Sample.dta, clear
foreach var of varlist lndomsales lnprofits lnmu {
	use Data\PSP_Sample.dta, clear

	* import starter
	xtset cvrnr year, yearly
	g d_imp_neu15_new_t0=1 if d_imp_neu15==1 & l.d_imp_neu15==0 & l2.d_imp_neu15==0
	replace d_imp_neu15_new_t0=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

	* control group should consist of non-importing firms
	g d_imp_neu15_nope=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

	g todrop=l2.d_imp_neu15
	
	drop if year<1999
	
	* drop missings 
	foreach v of varlist `var' l1`var' l1lnempl {
		drop if `v'==.
	}

	drop if outlier_lnlprod==1
	drop if outlier_d`var'==1

	* focus on first spell in case of multiple entries
	g d_imp_start=d_imp_neu15_new_t0
	bys cvrnr: egen temp=total(d_imp_start)
	g temp2=year if d_imp_start==1
	bys cvrnr: egen temp3=min(temp2)
	* replace with missing after first import market entry
	replace d_imp_start=. if year>temp3
	replace d_imp_start=. if d_imp_start==0
	drop temp*
	* ensure appropriate control group
	su d_imp_neu15_nope
	order cvrnr year d_imp_neu15 d_imp_start d_imp_neu15_nope
	bys cvrnr: egen tmp=total(d_imp_start)
	tab tmp
	g tmp2=d_imp_start if tmp==1
	tab tmp2, mi
	* note: import starter can be part of control group before import event (and later if it stops importing)
	replace tmp2=d_imp_neu15_nope if d_imp_neu15_nope==0
	tab tmp2, mi

	drop d_imp_start
	ren tmp2 d_imp_start
	drop if d_imp_start==.
	drop if todrop==.
	drop todrop d_imp_neu15_nope d_imp_neu15_new_t0 tmp*
	tab year

	drop if year<1999

	egen indyr=group(ind5 year)
	
	tabstat l1`var' `var', s(N mean sd) c(s) save
	tabstatmat all
	matrix ALL=all

	xml_tab ALL , ///
	 save(Log\Results_Table05_Selection.xml) append sheet(DV_`var')
	
	reghdfe l1`var' d_imp_start l1lnempl, absorb(indyr) cluster(cvrnr)
	est store select2_tm1_`var'

	reghdfe `var' d_imp_start l1lnempl, absorb(indyr) cluster(cvrnr)
	est store select2_t0_`var'

	
}

xml_tab select2_tm1_* select2_t0_* , ///
 save(Log\Results_Table05_Selection.xml) append sheet(WControl) sd below stats(N r2 r2_a) ///
 keep(d_imp_start l1lnempl)


log close
exit, clear
