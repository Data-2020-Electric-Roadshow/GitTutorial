

clear all
capture log close
set more off
set matsize 8000


* set path
global path ""
cd $path


set linesize 200
log using Log\Regressions_Table_09_PSM_Chains.log, replace


/*

Do file produces results presented in table 9 (upper panel) of the paper

1) identify import starters and non-importers

2) estimate probit model

3) ensure common support

4) implement matching algorithm,using psmatch2

5) save data sets

6) repeat exercise for different time horizons (p=-2 ... p=0, ... p=+3)

7) compute regression-adjusted att
*/



///////////////////////////////
///////////////////////////////

///			Period 0		///

///////////////////////////////
///////////////////////////////

use Data\PSP_Sample.dta, clear


fvset base 2002 year 
fvset base 5211 ind4 

* import starter
xtset cvrnr year, yearly
g d_imp_neu15_new_t0=1 if d_imp_neu15==1 & l.d_imp_neu15==0 & l2.d_imp_neu15==0
replace d_imp_neu15_new_t0=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

* control group should consist of non-importing firms
g d_imp_neu15_nope=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

g todrop=l1.d_imp_neu15

tab chain

* compare firms that are no chains in t-1 and t-2
replace todrop=. if l1chain!=0
replace todrop=. if l2chain!=0

drop if year<1999

* drop missings - drop lnlprod mit starkem einfluss 
foreach v of varlist chain l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum l1chain {
	drop if `v'==.
}

drop if outlier_lnlprod==1

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

save Data\PSP_Sample_chain_BeforeMatched_t0.dta, replace

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

psmatch2 d_imp_start, radius caliper(`caliper') pscore(phat_exact) com outcome(chain)
g esample=e(sample)
pstest $cvar
est store pstest_chain

reg chain d_imp_start i.year [pw=_weight], cluster(cvrnr)

save Data\PSP_Sample_chain_Matched_t0.dta, replace

tab esample
tab _support

drop if _w==.

* store number of treated and caliper
g treated=1 if d_imp_start==1
g N=_N
tabstat treated N caliper2, s(N mean) c(s) save
tabstatmat all
matrix Stats=all'
xml_tab Stats , save(Log\PSM_Pstest_Table_09.xml) append sheet(Stats_chain_t0) 

* store mean of treated and control group
svyset _n [pw=_w]
svy: mean $cvar if  d_imp_start==1
est store treated_chain
svy: mean $cvar if  d_imp_start==0
est store control_chain

xml_tab treated_chain control_chain pstest_chain, save(Log\PSM_Pstest_Table_09.xml) ///
 append sheet(PST_chain_t0)
est clear



///////////////////////////////
///////////////////////////////

///			Period 1		///

///////////////////////////////
///////////////////////////////

use Data\PSP_Sample.dta, clear

* import starter
xtset cvrnr year, yearly
g d_imp_neu15_new_t0=1 if d_imp_neu15==1 & l.d_imp_neu15==0 & l2.d_imp_neu15==0
replace d_imp_neu15_new_t0=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

* control group should consist of non-importing firms
g d_imp_neu15_nope=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

g todrop=l1.d_imp_neu15

tab chain

g f1chain=f1.chain

* compare firms that are no chains in t-1 and t-2
replace todrop=. if l1chain!=0
replace todrop=. if l2chain!=0
drop if f1chain==.

drop if year<1999

* drop missings - drop lnlprod mit starkem einfluss 
foreach v of varlist chain l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum l1chain {
	drop if `v'==.
}

drop if outlier_lnlprod==1

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

save Data\PSP_Sample_chain_BeforeMatched_t1.dta, replace

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

psmatch2 d_imp_start, radius caliper(`caliper') pscore(phat_exact) com outcome(f1chain)
g esample=e(sample)
pstest $cvar
est store pstest_chain

reg f1chain d_imp_start i.year [pw=_weight], cluster(cvrnr)

save Data\PSP_Sample_chain_Matched_t1.dta, replace

tab esample
tab _support

drop if _w==.

* store number of treated and caliper
g treated=1 if d_imp_start==1
g N=_N
tabstat treated N caliper2, s(N mean) c(s) save
tabstatmat all
matrix Stats=all'
xml_tab Stats , save(Log\PSM_Pstest_Table_09.xml) append sheet(Stats_chain_t1) 

* store mean of treated and control group
svyset _n [pw=_w]
svy: mean $cvar if  d_imp_start==1
est store treated_chain
svy: mean $cvar if  d_imp_start==0
est store control_chain

xml_tab treated_chain control_chain pstest_chain, save(Log\PSM_Pstest_Table_09.xml) ///
 append sheet(PST_chain_t1)
est clear



///////////////////////////////
///////////////////////////////

///			Period 2		///

///////////////////////////////
///////////////////////////////

use Data\PSP_Sample.dta, clear

* import starter
xtset cvrnr year, yearly
g d_imp_neu15_new_t0=1 if d_imp_neu15==1 & l.d_imp_neu15==0 & l2.d_imp_neu15==0
replace d_imp_neu15_new_t0=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

* control group should consist of non-importing firms
g d_imp_neu15_nope=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

g todrop=l1.d_imp_neu15

tab chain

g f1chain=f1.chain
g f2chain=f2.chain

* compare firms that are no chains in t-1 and t-2
replace todrop=. if l1chain!=0
replace todrop=. if l2chain!=0
drop if f1chain==.
drop if f2chain==.

drop if year<1999

* drop missings - drop lnlprod mit starkem einfluss 
foreach v of varlist chain l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum l1chain {
	drop if `v'==.
}

drop if outlier_lnlprod==1

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

save Data\PSP_Sample_chain_BeforeMatched_t2.dta, replace

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

psmatch2 d_imp_start, radius caliper(`caliper') pscore(phat_exact) com outcome(f2chain)
g esample=e(sample)
pstest $cvar
est store pstest_chain

reg f2chain d_imp_start i.year [pw=_weight], cluster(cvrnr)

save Data\PSP_Sample_chain_Matched_t2.dta, replace

tab esample
tab _support

drop if _w==.

* store number of treated and caliper
g treated=1 if d_imp_start==1
g N=_N
tabstat treated N caliper2, s(N mean) c(s) save
tabstatmat all
matrix Stats=all'
xml_tab Stats , save(Log\PSM_Pstest_Table_09.xml) append sheet(Stats_chain_t2) 

* store mean of treated and control group
svyset _n [pw=_w]
svy: mean $cvar if  d_imp_start==1
est store treated_chain
svy: mean $cvar if  d_imp_start==0
est store control_chain

xml_tab treated_chain control_chain pstest_chain, save(Log\PSM_Pstest_Table_09.xml) ///
 append sheet(PST_chain_t2)
est clear




///////////////////////////////
///////////////////////////////

///			Period 3		///

///////////////////////////////
///////////////////////////////

use Data\PSP_Sample.dta, clear

* import starter
xtset cvrnr year, yearly
g d_imp_neu15_new_t0=1 if d_imp_neu15==1 & l.d_imp_neu15==0 & l2.d_imp_neu15==0
replace d_imp_neu15_new_t0=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

* control group should consist of non-importing firms
g d_imp_neu15_nope=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

g todrop=l1.d_imp_neu15

tab chain

g f1chain=f1.chain
g f2chain=f2.chain
g f3chain=f3.chain

* compare firms that are no chains in t-1 and t-2
replace todrop=. if l1chain!=0
replace todrop=. if l2chain!=0
drop if f1chain==.
drop if f2chain==.
drop if f3chain==.

drop if year<1999

* drop missings - drop lnlprod mit starkem einfluss 
foreach v of varlist chain l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum l1chain {
	drop if `v'==.
}

drop if outlier_lnlprod==1

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

save Data\PSP_Sample_chain_BeforeMatched_t3.dta, replace

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

psmatch2 d_imp_start, radius caliper(`caliper') pscore(phat_exact) com outcome(f3chain)
g esample=e(sample)
pstest $cvar
est store pstest_chain

reg f3chain d_imp_start i.year [pw=_weight], cluster(cvrnr)

save Data\PSP_Sample_chain_Matched_t3.dta, replace

tab esample
tab _support

drop if _w==.

* store number of treated and caliper
g treated=1 if d_imp_start==1
g N=_N
tabstat treated N caliper2, s(N mean) c(s) save
tabstatmat all
matrix Stats=all'
xml_tab Stats , save(Log\PSM_Pstest_Table_09.xml) append sheet(Stats_chain_t3) 

* store mean of treated and control group
svyset _n [pw=_w]
svy: mean $cvar if  d_imp_start==1
est store treated_chain
svy: mean $cvar if  d_imp_start==0
est store control_chain

xml_tab treated_chain control_chain pstest_chain, save(Log\PSM_Pstest_Table_09.xml) ///
 append sheet(PST_chain_t3)
est clear


// regression results
use Data\PSP_Sample_chain_Matched_t0.dta, clear
qui: tab year, g(dyr)
reg chain d_imp_start $cvar dyr* i.ind4 [pw=_weight], cluster(cvrnr)
est store chain_t0
use Data\PSP_Sample_chain_Matched_t1.dta, clear
qui: tab year, g(dyr)
g f1chain_cum=chain
replace f1chain_cum=1 if f1chain==1
tab f1chain 
tab f1chain_cum
reg f1chain_cum d_imp_start $cvar dyr* i.ind4 [pw=_weight], cluster(cvrnr)
est store chain_cum_t1
use Data\PSP_Sample_chain_Matched_t2.dta, clear
qui: tab year, g(dyr)
g f2chain_cum=chain
replace f2chain_cum=1 if f1chain==1
replace f2chain_cum=1 if f2chain==1
tab f2chain 
tab f2chain_cum
reg f2chain_cum d_imp_start $cvar dyr* i.ind4 [pw=_weight], cluster(cvrnr)
est store chain_cum_t2
use Data\PSP_Sample_chain_Matched_t3.dta, clear
qui: tab year, g(dyr)
g f3chain_cum=chain
replace f3chain_cum=1 if f1chain==1
replace f3chain_cum=1 if f2chain==1
replace f3chain_cum=1 if f3chain==1
tab f3chain 
tab f3chain_cum
reg f3chain_cum d_imp_start $cvar dyr* i.ind4 [pw=_weight], cluster(cvrnr)
est store chain_cum_t3

xml_tab chain_t* chain_c*, ///
 save(Log\Results_Table_09_PSM.xml) ///
 append sheet(chain) sd below stats(N r2) ///
 keep(d_imp_start)
est clear

log close
exit, clear
