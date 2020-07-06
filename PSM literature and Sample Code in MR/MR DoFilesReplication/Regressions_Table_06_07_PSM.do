


clear all
capture log close
set more off
set matsize 8000

* set path
global path ""
cd $path


set linesize 200
log using Log\Regressions_Table_06_07_PSM.log, replace


/*
Do file produces results presented in tables 6 and 7 of the paper

1) identify import starters and non-importers

2) estimate probit model

3) ensure common support

4) implement matching algorithm,using psmatch2

5) save data sets

6) repeat exercise for different time horizons (p=-2 ... p=0, ... p=+3)

7) compute regression-adjusted att
*/



use Data\PSP_Sample.dta, clear
fvset base 2002 year 
fvset base 5211 ind4 


///////////////////////////////
///////////////////////////////

///			Period 0		///

///////////////////////////////
///////////////////////////////

foreach var of varlist lndomsales lnprofits lnmu {

	use Data\PSP_Sample.dta, clear

	* import starter (treatment variable, "cvrnr" refers to retailing firm)
	xtset cvrnr year, yearly
	g d_imp_neu15_new_t0=1 if d_imp_neu15==1 & l.d_imp_neu15==0 & l2.d_imp_neu15==0
	replace d_imp_neu15_new_t0=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

	* control group should consist of non-importing firms
	g d_imp_neu15_nope=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

	g todrop=l2.d_imp_neu15
	
	drop if year<1999
	
	* drop missings 
	foreach v of varlist `var' l1`var' l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum {
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
	* ensure appropriate control group (su means summary)
	su d_imp_neu15_nope
	order cvrnr year d_imp_neu15 d_imp_start d_imp_neu15_nope
	bys cvrnr: egen tmp=total(d_imp_start)
	g tmp2=d_imp_start if tmp==1
	// note: control group (d_imp_neu15_nope==0) includes two parts: 1) import starter can be part of control group before import event (and later if it stops importing)\
	//                                                               2) firms that never import during sample periods
	replace tmp2=d_imp_neu15_nope if d_imp_neu15_nope==0

	drop d_imp_start
	ren tmp2 d_imp_start
	drop if d_imp_start==. /* drop the year after the first import market entry (not including the years they stop importing after the first import market entry*/
	drop if todrop==. /* drop those importing firms which do not have treatment status information in lagged two year */
	drop todrop d_imp_neu15_nope d_imp_neu15_new_t0 tmp*
	tab year
	
	* compute higher order terms
	foreach v2 of varlist l1lnlprod {
		egen tmp=mean(`v2')
		g `v2'_c=`v2'-tmp
		drop tmp
		g `v2'_sq=`v2'_c^2
	}

	save Data\PSP_Sample_`var'_BeforeMatched_t0.dta, replace

	global cvar l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum l1lnlprod_sq
	
	* probit by ind_id and year
	probit d_imp_start $cvar i.year i.ind4
	predict phat if e(sample), pr

	local caliper = 0.001
	g caliper2=0.001

	* ensure common support
	egen idyr=group(year)
	su idyr
	local idyrmax=r(max)
	forvalues jj=1/`idyrmax' {
		qui su phat if d_imp_start==0 & idyr==`jj'
		replace phat=. if d_imp_start==1 & idyr==`jj' & (phat>r(max) | phat<r(min))
	}
	drop if phat==.

	* matching by year
	g double phat_exact=phat+(idyr-1)*10 /* (idyr-1) part can reflect the difference of year */
	su phat_exact phat

	psmatch2 d_imp_start, radius caliper(`caliper') pscore(phat_exact) com outcome(`var')
	g esample=e(sample) /* e(sample) equals to 1 if the observation is in the estimation sample and 0 otherwise, thus we can know whether observation is in the matching sample */
	pstest $cvar
	est store pstest_`var'

	save Data\PSP_Sample_`var'_Matched_t0.dta, replace

	tab esample
	tab _support
	
	drop if _w==. /* drop unmatched sample */

	* store number of treated and caliper
	g treated=1 if d_imp_start==1
	g N=_N
	tabstat treated N caliper2, s(N mean) c(s) save
	tabstatmat all
	matrix Stats=all'
	xml_tab Stats , save(Log\PSM_Pstest_Table_0607.xml) append sheet(Stats_`var'_t0) 
	
	* store mean of treated and control group
	svyset _n [pw=_w]
	svy: mean $cvar if  d_imp_start==1
	est store treated_`var'
	svy: mean $cvar if  d_imp_start==0
	est store control_`var'
	
	xml_tab treated_`var' control_`var' pstest_`var', save(Log\PSM_Pstest_Table_0607.xml) ///
	 append sheet(PST_`var'_t0)
	est clear
	
}


///////////////////////////////
///////////////////////////////

///			Period 1		///

///////////////////////////////
///////////////////////////////

foreach var of varlist lndomsales lnprofits lnmu {

	use Data\PSP_Sample.dta, clear

	* import starter
	xtset cvrnr year, yearly
	g d_imp_neu15_new_t0=1 if d_imp_neu15==1 & l.d_imp_neu15==0 & l2.d_imp_neu15==0
	replace d_imp_neu15_new_t0=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

	* control group should consist of non-importing firms
	g d_imp_neu15_nope=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

	g f1`var'=f1.`var' /* f1. means generating the lead variable */
	g todrop=l2.d_imp_neu15
	drop if f1`var'==. /* drop those firms which has no 1-term leading outcome variables */
	
	drop if year<1999

	* drop missings 
	foreach v of varlist `var' l1`var' l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum {
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
	replace tmp2=d_imp_neu15_nope if d_imp_neu15_nope==0
	tab tmp2, mi

	drop d_imp_start
	ren tmp2 d_imp_start
	drop if d_imp_start==.
	drop if todrop==.
	drop todrop d_imp_neu15_nope d_imp_neu15_new_t0 tmp*
	tab year
	
	* compute higher order terms
	foreach v2 of varlist l1lnlprod {
		egen tmp=mean(`v2')
		g `v2'_c=`v2'-tmp
		drop tmp
		g `v2'_sq=`v2'_c^2
	}

	save Data\PSP_Sample_`var'_BeforeMatched_t1.dta, replace

	global cvar l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum l1lnlprod_sq

	* probit by ind_id and year
	probit d_imp_start $cvar i.year i.ind4
	predict phat if e(sample), pr

	local caliper = 0.001
	g caliper2=0.001

	* ensure support
	egen idyr=group(year)
	su idyr
	local idyrmax=r(max)
	forvalues jj=1/`idyrmax' {
		qui su phat if d_imp_start==0 & idyr==`jj'
		replace phat=. if d_imp_start==1 & idyr==`jj' & (phat>r(max) | phat<r(min))
	}
	drop if phat==.

	* matching by year
	g double phat_exact=phat+(idyr-1)*10
	su phat_exact phat

	psmatch2 d_imp_start, radius caliper(`caliper') pscore(phat_exact) com outcome(f1`var')
	g esample=e(sample)
	pstest $cvar
	est store pstest_`var'

	save Data\PSP_Sample_`var'_Matched_t1.dta, replace

	tab esample
	tab _support
	
	drop if _w==.

	* store number of treated and caliper
	g treated=1 if d_imp_start==1
	g N=_N
	tabstat treated N caliper2, s(N mean) c(s) save
	tabstatmat all
	matrix Stats=all'
	xml_tab Stats , save(Log\PSM_Pstest_Table_0607.xml) append sheet(Stats_`var'_t1) 
	
	* store mean of treated and control group
	svyset _n [pw=_w]
	svy: mean $cvar if  d_imp_start==1
	est store treated_`var'
	svy: mean $cvar if  d_imp_start==0
	est store control_`var'
	
	xml_tab treated_`var' control_`var' pstest_`var', save(Log\PSM_Pstest_Table_0607.xml) ///
	 append sheet(PST_`var'_t1)
	est clear
	
}



///////////////////////////////
///////////////////////////////

///			Period 2		///

///////////////////////////////
///////////////////////////////

foreach var of varlist lndomsales lnprofits lnmu {

	use Data\PSP_Sample.dta, clear

	* import starter
	xtset cvrnr year, yearly
	g d_imp_neu15_new_t0=1 if d_imp_neu15==1 & l.d_imp_neu15==0 & l2.d_imp_neu15==0
	replace d_imp_neu15_new_t0=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

	* control group should consist of non-importing firms
	g d_imp_neu15_nope=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

	g f1`var'=f1.`var'
	g f2`var'=f2.`var'
	g todrop=l2.d_imp_neu15
	drop if f1`var'==.
	drop if f2`var'==.
	
	drop if year<1999

	* drop missings 
	foreach v of varlist `var' l1`var' l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum {
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
	replace tmp2=d_imp_neu15_nope if d_imp_neu15_nope==0
	tab tmp2, mi

	drop d_imp_start
	ren tmp2 d_imp_start
	drop if d_imp_start==.
	drop if todrop==.
	drop todrop d_imp_neu15_nope d_imp_neu15_new_t0 tmp*
	tab year
	
	* compute higher order terms
	foreach v2 of varlist l1lnlprod {
		egen tmp=mean(`v2')
		g `v2'_c=`v2'-tmp
		drop tmp
		g `v2'_sq=`v2'_c^2
	}

	save Data\PSP_Sample_`var'_BeforeMatched_t2.dta, replace

	global cvar l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum l1lnlprod_sq

	* probit by ind_id and year
	probit d_imp_start $cvar i.year i.ind4
	predict phat if e(sample), pr

	local caliper = 0.001
	g caliper2=0.001

	* ensure support
	egen idyr=group(year)
	su idyr
	local idyrmax=r(max)
	forvalues jj=1/`idyrmax' {
		qui su phat if d_imp_start==0 & idyr==`jj'
		replace phat=. if d_imp_start==1 & idyr==`jj' & (phat>r(max) | phat<r(min))
	}
	drop if phat==.

	* matching by year
	g double phat_exact=phat+(idyr-1)*10
	su phat_exact phat

	psmatch2 d_imp_start, radius caliper(`caliper') pscore(phat_exact) com outcome(f2`var')
	g esample=e(sample)
	pstest $cvar
	est store pstest_`var'

	save Data\PSP_Sample_`var'_Matched_t2.dta, replace

	tab esample
	tab _support

	drop if _w==.

	* store number of treated and caliper
	g treated=1 if d_imp_start==1
	g N=_N
	tabstat treated N caliper2, s(N mean) c(s) save
	tabstatmat all
	matrix Stats=all'
	xml_tab Stats , save(Log\PSM_Pstest_Table_0607.xml) append sheet(Stats_`var'_t2) 
	
	* store mean of treated and control group
	svyset _n [pw=_w]
	svy: mean $cvar if  d_imp_start==1
	est store treated_`var'
	svy: mean $cvar if  d_imp_start==0
	est store control_`var'
	
	xml_tab treated_`var' control_`var' pstest_`var', save(Log\PSM_Pstest_Table_0607.xml) ///
	 append sheet(PST_`var'_t2)
	est clear
	
}



///////////////////////////////
///////////////////////////////

///			Period 3		///

///////////////////////////////
///////////////////////////////

foreach var of varlist lndomsales lnprofits lnmu {

	use Data\PSP_Sample.dta, clear

	* import starter
	xtset cvrnr year, yearly
	g d_imp_neu15_new_t0=1 if d_imp_neu15==1 & l.d_imp_neu15==0 & l2.d_imp_neu15==0
	replace d_imp_neu15_new_t0=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

	* control group should consist of non-importing firms
	g d_imp_neu15_nope=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

	g f1`var'=f1.`var'
	g f2`var'=f2.`var'
	g f3`var'=f3.`var'
	g todrop=l2.d_imp_neu15
	drop if f1`var'==.
	drop if f2`var'==.
	drop if f3`var'==.
	
	drop if year<1999

	* drop missings 
	foreach v of varlist `var' l1`var' l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum {
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
	replace tmp2=d_imp_neu15_nope if d_imp_neu15_nope==0
	tab tmp2, mi

	drop d_imp_start
	ren tmp2 d_imp_start
	drop if d_imp_start==.
	drop if todrop==.
	drop todrop d_imp_neu15_nope d_imp_neu15_new_t0 tmp*
	tab year
	
	cap drop mypscore

	* compute higher order terms
	foreach v2 of varlist l1lnlprod {
		egen tmp=mean(`v2')
		g `v2'_c=`v2'-tmp
		drop tmp
		g `v2'_sq=`v2'_c^2
	}

	save Data\PSP_Sample_`var'_BeforeMatched_t3.dta, replace

	global cvar l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum l1lnlprod_sq

	* probit by ind_id and year
	probit d_imp_start $cvar i.year i.ind4
	predict phat if e(sample), pr

	local caliper = 0.001
	g caliper2=0.001

	* ensure support
	egen idyr=group(year)
	su idyr
	local idyrmax=r(max)
	forvalues jj=1/`idyrmax' {
		qui su phat if d_imp_start==0 & idyr==`jj'
		replace phat=. if d_imp_start==1 & idyr==`jj' & (phat>r(max) | phat<r(min))
	}
	drop if phat==.

	* matching by year
	g double phat_exact=phat+(idyr-1)*10
	su phat_exact phat
	psmatch2 d_imp_start, radius caliper(`caliper') pscore(phat_exact) com outcome(f3`var')
	g esample=e(sample)
	pstest $cvar
	est store pstest_`var'

	save Data\PSP_Sample_`var'_Matched_t3.dta, replace

	tab esample
	tab _support
	
	drop if _w==.

	* store number of treated and caliper
	g treated=1 if d_imp_start==1
	g N=_N
	tabstat treated N caliper2, s(N mean) c(s) save
	tabstatmat all
	matrix Stats=all'
	xml_tab Stats , save(Log\PSM_Pstest_Table_0607.xml) append sheet(Stats_`var'_t3) 
	
	* store mean of treated and control group
	svyset _n [pw=_w]
	svy: mean $cvar if  d_imp_start==1
	est store treated_`var'
	svy: mean $cvar if  d_imp_start==0
	est store control_`var'
	
	xml_tab treated_`var' control_`var' pstest_`var', save(Log\PSM_Pstest_Table_0607.xml) ///
	 append sheet(PST_`var'_t3)
	est clear
	
}


///////////////////////////////
///////////////////////////////

///			Period t-2		///

///////////////////////////////
///////////////////////////////

foreach var of varlist lndomsales lnprofits lnmu {
	use Data\PSP_Sample.dta, clear

	* import starter
	xtset cvrnr year, yearly
	g d_imp_neu15_new_t0=1 if d_imp_neu15==1 & l.d_imp_neu15==0 & l2.d_imp_neu15==0
	replace d_imp_neu15_new_t0=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

	* control group should consist of non-importing firms
	g d_imp_neu15_nope=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

	cap drop l1`var'
	cap drop l2`var'
	g l1`var'=l1.`var'
	g l2`var'=l2.`var'
	g todrop=l2.d_imp_neu15
	drop if l1`var'==.
	drop if l2`var'==.
	
	drop if year<1999

	* drop missings 
	foreach v of varlist `var' l1`var' l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum {
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
	replace tmp2=d_imp_neu15_nope if d_imp_neu15_nope==0
	tab tmp2, mi

	drop d_imp_start
	ren tmp2 d_imp_start
	drop if d_imp_start==.
	drop if todrop==.
	drop todrop d_imp_neu15_nope d_imp_neu15_new_t0 tmp*
	tab year

	* compute higher order terms
	foreach v2 of varlist l1lnlprod {
		egen tmp=mean(`v2')
		g `v2'_c=`v2'-tmp
		drop tmp
		g `v2'_sq=`v2'_c^2
	}

	save Data\PSP_Sample_`var'_BeforeMatched_t-2.dta, replace

	global cvar l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum l1lnlprod_sq

	* probit by ind_id and year
	probit d_imp_start $cvar i.year i.ind4
	predict phat if e(sample), pr

	local caliper = 0.001
	g caliper2=0.001

	* ensure support
	egen idyr=group(year)
	su idyr
	local idyrmax=r(max)
	forvalues jj=1/`idyrmax' {
		qui su phat if d_imp_start==0 & idyr==`jj'
		replace phat=. if d_imp_start==1 & idyr==`jj' & (phat>r(max) | phat<r(min))
	}
	drop if phat==.

	* matching by year
	g double phat_exact=phat+(idyr-1)*10
	su phat_exact phat

	psmatch2 d_imp_start, radius caliper(`caliper') pscore(phat_exact) com outcome(`var')
	g esample=e(sample)
	pstest $cvar
	est store pstest_`var'

	save Data\PSP_Sample_`var'_Matched_t-2.dta, replace

	tab esample
	tab _support
	
	drop if _w==.

	* store number of treated and caliper
	g treated=1 if d_imp_start==1
	g N=_N
	tabstat treated N caliper2, s(N mean) c(s) save
	tabstatmat all
	matrix Stats=all'
	xml_tab Stats , save(Log\PSM_Pstest_Table_0607.xml) append sheet(Stats_`var'_t-2) 
	
	* store mean of treated and control group
	svyset _n [pw=_w]
	svy: mean $cvar if  d_imp_start==1
	est store treated_`var'
	svy: mean $cvar if  d_imp_start==0
	est store control_`var'
	
	xml_tab treated_`var' control_`var' pstest_`var', save(Log\PSM_Pstest_Table_0607.xml) ///
	 append sheet(PST_`var'_t-2)
	est clear
	
}


use Data\PSP_Sample_lndomsales_Matched_t-2.dta, clear



// OVERVIEW OF RESULTS

foreach var of varlist lndomsales lnprofits lnmu {

	// LELVEL
	* ATT - t-2 (placebo)
	use Data\PSP_Sample_`var'_Matched_t-2.dta, clear
	qui tab year, g(dyr) /* generate year fixed effects */
	reg l2`var' d_imp_start $cvar dyr* i.ind4  [pw=_weight], cluster(cvrnr)
	est store placebo2_t0
	* Placebo - t-1
	use Data\PSP_Sample_`var'_Matched_t-1.dta, clear
	qui tab year, g(dyr) /* generate year fixed effects */
	reg l1`var' d_imp_start $cvar dyr* i.ind4  [pw=_weight], cluster(cvrnr)
	est store placebo1_t0
	* ATT - t0
	use Data\PSP_Sample_`var'_Matched_t0.dta, clear
	qui tab year, g(dyr)
	reg `var' d_imp_start $cvar dyr* i.ind4 [pw=_weight], cluster(cvrnr)
	est store psps_t0
	* ATT - t1
	use Data\PSP_Sample_`var'_Matched_t1.dta, clear
	qui tab year, g(dyr)
	reg f1`var' d_imp_start $cvar dyr* i.ind4 [pw=_weight], cluster(cvrnr)
	est store psps_t1
	g cum_`var'=`var'+f1`var'
	reg cum_`var' d_imp_start $cvar dyr* i.ind4  [pw=_weight], cluster(cvrnr)
	est store psps_cum_t1
	* ATT - t2
	use Data\PSP_Sample_`var'_Matched_t2.dta, clear
	qui tab year, g(dyr)
	reg f2`var' d_imp_start $cvar dyr* i.ind4 [pw=_weight], cluster(cvrnr)
	est store psps_t2
	g cum_`var'=`var'+f1`var'+f2`var'
	reg cum_`var' d_imp_start $cvar dyr* i.ind4  [pw=_weight], cluster(cvrnr)
	est store psps_cum_t2
	* ATT - t3
	use Data\PSP_Sample_`var'_Matched_t3.dta, clear
	qui tab year, g(dyr)
	reg f3`var' d_imp_start $cvar dyr* i.ind4 [pw=_weight], cluster(cvrnr)
	est store psps_t3
	g cum_`var'=`var'+f1`var'+f2`var'+f3`var'
	reg cum_`var' d_imp_start $cvar dyr* i.ind4  [pw=_weight], cluster(cvrnr)
	est store psps_cum_t3

	xml_tab placebo2_t0 placebo1_t0 psps_t* psps_c*, ///
	 save(Log\Results_Table_0607_PSM.xml) ///
	 append sheet(`var') sd below stats(N r2) ///
	 keep(d_imp_start)
	 est clear

	// GROWTH
	* ATT - t-2 (placebo)
	use Data\PSP_Sample_`var'_Matched_t-2.dta, clear
	qui tab year, g(dyr)
	g tmp=l1`var'-l2`var'
	reg tmp d_imp_start $cvar dyr* i.ind4  [pw=_weight], cluster(cvrnr)
	est store placebo2_t0
	* ATT (base)- t0
	use Data\PSP_Sample_`var'_Matched_t0.dta, clear
	g tmp=`var'-l1`var'
	qui tab year, g(dyr)
	reg tmp d_imp_start $cvar dyr* i.ind4 [pw=_weight], cluster(cvrnr)
	est store psps_t0
	* ATT - t1
	use Data\PSP_Sample_`var'_Matched_t1.dta, clear
	g tmp2=f1`var'-l1`var'
	qui tab year, g(dyr)
	reg tmp2 d_imp_start $cvar dyr* i.ind4  [pw=_weight], cluster(cvrnr)
	est store psps_cum_t1
	* ATT - t2
	use Data\PSP_Sample_`var'_Matched_t2.dta, clear
	g tmp2=f2`var'-l1`var'
	qui tab year, g(dyr)
	reg tmp2 d_imp_start $cvar dyr* i.ind4  [pw=_weight], cluster(cvrnr)
	est store psps_cum_t2
	* ATT - t3
	use Data\PSP_Sample_`var'_Matched_t3.dta, clear
	g tmp2=f3`var'-l1`var'
	qui tab year, g(dyr)
	reg tmp2 d_imp_start $cvar dyr* i.ind4  [pw=_weight], cluster(cvrnr)
	est store psps_cum_t3

	xml_tab placebo2_t0 psps_t* psps_c*, ///
	 save(Log\Results_Table_0607_PSM.xml) ///
	 append sheet(`var'_gr) sd below stats(N r2) ///
	 keep(d_imp_start)
	 est clear
	 
	 
}



// Store probit results

foreach var of varlist lndomsales lnprofits lnmu {

use Data\PSP_Sample_`var'_BeforeMatched_t0.dta, clear

fvset base 2002 year 
fvset base 5211 ind4 

	global cvar l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum l1lnlprod_sq
	
	* probit by ind_id and year
	probit d_imp_start $cvar i.year i.ind4
	est store probit_`var'
}

xml_tab probit*, ///
 save(Log\Results_Table_0607_PSM.xml) ///
 append sheet(Probit_to) sd below stats(N r2) ///
 keep($cvar)
 est clear

 

log close
exit, clear
