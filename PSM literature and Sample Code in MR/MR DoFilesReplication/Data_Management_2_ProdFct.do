

clear all
capture log close
set more off

* set path
global path ""
cd $path


set linesize 200
log using Log\Data_Management_4_PrdFct.log, replace


/*

1) Mata program for GMM routine
2) Prepare data for production function estimation
3) Loop over sectors to get output elasticities by sectors
4) Clean variables and compute markups

*/

global ind ind5
global base 5211


global reps 500

// 1) Mata program for GMM routine


*-------------------------BEGIN MATA PROGRAM---------------------------------------------------------------*
mata:

void GMM_TL(todo,betas,crit,g,H)
{
external PHI, PHI_LAG, Z, X, X_lag, Y, QR, QR_lag, C, OMEGA, OMEGA_lag, OMEGA_lag2, OMEGA_lag3, OMEGA_lag4, OMEGA_lag_pol, g_b, XI, W, crit

	PHI=st_data(.,("phi"))
	PHI_LAG=st_data(.,("phi_lag"))
	X=st_data(.,("const", "l", "k", "l2", "k2", "lk"))				
	Z=st_data(.,("const", "_l_lag", "k", "_l_lag2", "k2", "_l_lagk"))
	X_lag=st_data(.,("const", "_l_lag", "_k_lag", "_l_lag2", "_k_lag2", "_l_lagk_lag")) 
	
	Y=st_data(.,("va_log"))
	C=st_data(.,("const"))

	OMEGA=PHI-X*betas'
	OMEGA_lag=PHI_LAG-X_lag*betas'
	OMEGA_lag2=OMEGA_lag:^2
	OMEGA_lag3=OMEGA_lag:^3
	OMEGA_lag_pol=(C,OMEGA_lag,OMEGA_lag2,OMEGA_lag3)
	g_b = invsym(OMEGA_lag_pol'OMEGA_lag_pol)*OMEGA_lag_pol'OMEGA
	XI=OMEGA-OMEGA_lag_pol*g_b
	crit=(Z'XI)'(Z'XI)
}
void TransLog() 	
{
external init, S, p
	/// starting values
	init=st_data((1,1),("init_const", "init_bl", "init_bk", "init_bl2", "init_bk2", "init_blk"))
	
	S=optimize_init()
	optimize_init_evaluator(S, &GMM_TL())
	optimize_init_which(S,"min")
	optimize_init_evaluatortype(S,"d0")
	optimize_init_params(S,init)
	optimize_init_technique(S, "dfp")
	optimize_init_technique(S, "nm")
	optimize_init_nmsimplexdeltas(S,0.1)
	p=optimize(S) 
	st_matrix("bhat",p)
	stata("matrix list bhat")
}
end


* ------------ Do File ---------- *


// 2) Prepare data for production function estimation


use Data\firm_exp_imp_v04.dta,clear

tab ind_id
tab ind_id d_imp

* drop extreme lnlprod
drop if lnlprod==.

* keep relevant variables
g l=lnempl
g k=lncapital
g m=lnmaterials
g va_log=lnva

keep cvrnr year ind ind4 ind_id l k m va_log wage_nom va_nom d_imp d_exp ind5


* delete strange values
drop if l==.
drop if k==.
drop if m==.
drop if va_log==.
drop if wage_nom<=0 | wage_nom==.

drop if year<1998
g double id=cvrnr

save Data\ProdFctSample.dta,replace



// 3) Loop over sectors to get output elasticities by sectors

forvalues x=1/6 {

	use Data\ProdFctSample.dta,clear

	keep if ind_id == `x'
	keep cvrnr year id va_log m k l ind_id d_imp d_exp ind5
	
	/// higher order terms
	local M=3
	local N=3
	forvalues i=1/`M' {
		gen k`i'=k^(`i')
		gen m`i'=m^(`i')
		gen l`i'=l^(`i')
		* interaction terms
		forvalues j=1/`N'{
			gen l`i'k`j' = l^(`i')*k^(`j')
			gen l`i'm`j' = l^(`i')*m^(`j')
			gen k`i'm`j' = k^(`i')*m^(`j')
		}
	}

	gen lkm=l*k*m
	gen l2k2m2=l2*k2*m2
	gen l3k3m3=l3*k3*m3

	* interactions with import and export status
	foreach var of varlist l* k* m* {
		g `var'_imp=`var'*d_imp
		g `var'_exp=`var'*d_exp
	}
	g d_imp_exp=d_imp*d_exp
	
	/// Starting values
	g lk=l*k
	qui reg va_log l k l2 k2 lk i.year i.ind5
	gen init_bl=_b[l]
	gen init_bk=_b[k]
	gen init_bl2=_b[l2]
	gen init_bk2=_b[k2]
	gen init_blk=_b[lk]
	gen init_const=_b[_cons]
	
	xtset id year, year

	/// First stage
	qui reg va_log l* k* m* d_imp* d_exp i.year i.ind5
	predict phi
	predict epsilon, res
	preserve
	keep cvrnr year epsilon
	save Data\epsilon_adj_`x'.dta, replace
	restore
	gen phi_lag=L.phi

	g _l_lag=l.l
	g _k_lag=l.k
	g _l_lag2=_l_lag^2
	g _k_lag2=_k_lag^2
	g _l_lagk_lag=_l_lag*_k_lag
	g _l_lagk=_l_lag*k
	
	drop if phi==.
	drop if phi_lag==.
	drop if _l_lag==.

	* generate constant
	gen const = 1

	qui: mata TransLog()
		gen bl = bhat[1,2]
		gen bk = bhat[1,3]
		gen bl2 = bhat[1,4]
		gen bk2 = bhat[1,5]
		gen blk = bhat[1,6]

		matrix M = bhat
		matrix list M

	save Data\tmp_acf_adj_`x'.dta, replace
}

// 4) Clean variables and compute markups
use Data\epsilon_adj_1.dta, clear
forvalues i=2/6 { 
	append using Data\epsilon_adj_`i'.dta
}
save Data\epsilon_adj_14.dta, replace

use Data\tmp_acf_adj_1.dta, clear
forvalues i=2/6 { 
	append using Data\tmp_acf_adj_`i'.dta
}
tab year

* Use coefficients for all firms
keep cvrnr year bl* bk*
su bl* bk*
merge 1:1 cvrnr year using Data\ProdFctSample.dta, update
drop _merge
cap drop tmp
foreach var of varlist bl* bk* {
	g tmp=`var' if `var'!=.
	drop `var'
	bys cvrnr: egen `var'=max(tmp)
	drop tmp
}
su year bl* bk*
drop if bl==. // firms that are e.g. two years in sample, but these are not consecutive years

// output elasticities
g bl_tl= bl + 2*bl2*l + blk*k
g bk_tl= bk + 2*bk2*k + blk*l

merge 1:1 cvrnr year using Data\epsilon_adj_14.dta, update
drop if _merge==2
drop _merge
su epsilon, d
replace epsilon=exp(epsilon)
* clean extreme values of epsilon
egen p1=pctile(epsilon), p(1)
egen p99=pctile(epsilon), p(99)
replace epsilon=. if epsilon<p1
replace epsilon=. if epsilon>p99
drop p1 p99


// wage share
g va2=va_nom/epsilon
g alpha_l = wage/va2
su alpha_l,d
* clean wage share
foreach v of varlist alpha_l {
	egen p1=pctile(`v'), p(1)
	egen p99=pctile(`v'), p(99)
	replace `v'=. if `v'<p1
	replace `v'=. if `v'>p99
	drop p1 p99
}

// markups
g mu=bl_tl/alpha_l
su mu,d
* clean markups
foreach v of varlist mu* {
	egen p1=pctile(`v'), p(1)
	egen p99=pctile(`v'), p(99)
	bys ind_id: replace `v'=. if `v'<p1
	bys ind_id: replace `v'=. if `v'>p99
	drop p1 p99
}
su mu*,d

g lnmu=ln(mu)

keep cvrnr year bl_tl bk_tl mu lnmu
xtset cvrnr year, yearly

* clean also growth rate
foreach v of varlist lnmu {
	xtset cvrnr year, yearly
	g d`v'=d.`v'
	g outlier_d`v'=.
	bys year: egen p50=pctile(d`v'), p(50)
	bys year: egen sd=sd(d`v')
	replace outlier_d`v'=1 if d`v'<p50-5*sd & d`v'!=.
	replace outlier_d`v'=1 if d`v'>p50+5*sd & d`v'!=.
	drop p50 sd
}

* lagged values
xtset cvrnr year, yearly
foreach var of varlist lnmu {
	replace d`var'=. if outlier_d`var'==1
	g l1d`var'=l.d`var'
	g l1`var'=l1.`var'
	g l2`var'=l2.`var'
}


save Data\TFP_MU_adj.dta, replace

su mu,d

log close
exit, clear

