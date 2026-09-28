**#  This is the replication script for the manuscript titled "Haste makes waste: How Public Pressure increases Legislative Duration in the EU" by Anastasia Ershova, Aleksandra Khokhlova & Nikoleta Yordanova

/* The analysis is ran in Stata 19.5 SE—Standard Edition */

/* This do-file estimates a series of Cox proportional-hazards models with time-varying coefficients.
   This makes the do-file computationally intensive and time-consuming to run in full.
   On a standard laptop (Intel(R) Core(TM) i5-8265U CPU @ 1.60GHz (1.80 GHz), 8 GB RAM), a complete run of this do-file took approximately
   24 hours. */

* Set working directory
 cd "[REPLICATION]"


 **# install required packages 

 findit scurve_tvc // Click on install st0458 from http://www.stata-journal.com/software/sj16-4
//     SJ16-4 st0458. Calculates survival curves... / Calculates survival curves
//     from stcox with time- / varying coefficients / by Constantin Ruhe,
//     Department of Politics and / Public Administration, University of /
//     Konstanz, Konstanz, Germany / Support:  Constantin.Ruhe@uni-konstanz.de /
//

 findit plotplainblind // Click on install gr0070 from http://www.stata-journal.com/software/sj17-3
//     SJ17-3 gr0070. Provide graph schemes sensitive to color vision deficiency
//     / Provide graph schemes sensitive to color vision / deficiency / by Daniel
//     Bischof, Department of Political / Science, University of Zurich, Zurich,
//     / Switzerland / Support:  bischof@ipz.uzh.ch / After installation, type



* Start the log -- the models take a while to complete hence the log could be useful.
log using "july2026_FINALRUN.smcl"


*load the data


use "Final_Data.dta", clear

label var procedure_in_weeks "Legislative duration (in weeks)"
label var eu_salience "EU Salience"
label var eu_polarization "EU Public Division"
label var proposal_probability "Proposed EU Authority Expansion"
label var cn_ep_proposal "EP-Council Distance (at t of proposal)"
label var cn_polariz_proposal "Council Polarisation (at t of proposal)"
label var presid_elect_proposal_weeks "Weeks to Elections in Presidency Country"
label var admin_burden_year "Legislative Backlog"
label var st_complex "Structural Complexity"
label var prop_to_election_weeks_new "Weeks to EP Elections"
label var mobilization "Actor Mobilisation"
label var amending "Amending Act"
label var crisis_legislation "Crisis Legislation"
label var directive "Directive"
label var regulation "Regulation"
label var decision "Decision"




set scheme plotplainblind

**#Appendix 1.1  Figure 3 DV Distribution**

hist procedure_in_weeks, bin(20) fcolor(sea%80) xtitle("Duration of Legislative Procedures in Weeks")

graph export "DV.png", as(png) replace

*Appendix 1.1 Descriptive statistics**
* (already in manuscript order)

estpost  ///
 sum 					procedure_in_weeks   ///
 						eu_salience ///
						eu_polarization ///
						proposal_probability ///
						cn_ep_proposal ///
						cn_polariz_proposal ///
						presid_elect_proposal_weeks ///
						admin_burden_year  ///
						st_complex ///
						prop_to_election_weeks_new ///
						mobilization  ///
						amending ///
						crisis_legislation ///
					    directive regulation decision

est store descr

esttab descr  using "decriptive_table.tex", replace ///
 cells("mean(fmt(%6.2fc)) sd(fmt(%6.2fc)) min max count")   nonumber ///
 nomtitle nonote noobs label booktabs ///
	collabels( "Mean" "SD" "Min" "Max" "N")  ///
	title("Descriptive Statistics")



**#**** Correlation table in Appendix 1.1
* (already in manuscript order)

 estpost correlate  eu_salience ///
						eu_polarization ///
						proposal_probability ///
						cn_ep_proposal ///
						cn_polariz_proposal ///
						presid_elect_proposal_weeks ///
						admin_burden_year  ///
						st_complex ///
						prop_to_election_weeks_new ///
						mobilization  ///
						amending ///
						crisis_legislation ///
					    directive regulation  , matrix listwise


esttab using "corr.tex", unstack not noobs  label compress  cells(b(fmt(2))) replace


**# Appendix 1.2. Proportionate hazard test.
* (already in manuscript order)

stcox  		eu_salience ///
						eu_polarization ///
						proposal_probability ///
						cn_ep_proposal ///
						cn_polariz_proposal ///
						presid_elect_proposal_weeks ///
						admin_burden_year  ///
						st_complex ///
						prop_to_election_weeks_new ///
						mobilization  ///
						amending ///
						crisis_legislation ///
					    directive regulation  ///
						i.capcode

estat phtest, detail




**#***Prepare data for the models and set it as stset

stset procedure_in_weeks, failure (completion) id(cod)

 ** split data
 stsplit, at(failures)



**# Main models for the main text

/*Model 1 ** salience & polarization */

eststo crisis1b: stcox c.eu_salience ///
                   eu_polarization ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year  ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization  ///
                   amending ///
                   crisis_legislation ///
                   directive ///
                   regulation ///
                   i.capcode, ///
                        tvc( mobilization ///
                             st_complex ///
                             i.capcode ) ///
                        texp(ln(_t)) cluster(cod)


/*Model 2 ** EU Authority expansion */
eststo crisis2b: stcox proposal_probability ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year  ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization  ///
                   amending ///
                   crisis_legislation ///
                   directive ///
                   regulation ///
                   i.capcode, ///
                      tvc( proposal_probability   ///
						 mobilization ///
								st_complex ///	
								i.capcode   ) ///
								texp(ln(_t)) cluster(cod)

/*Model 3 ** FULL model*/
eststo crisis3b: stcox c.eu_salience ///
                   eu_polarization ///
                   proposal_probability ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year  ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization  ///
                   amending ///
                   crisis_legislation ///
                   directive ///
                   regulation ///
                   i.capcode, ///
                     tvc( proposal_probability   ///
						 mobilization ///
								st_complex ///	
								i.capcode   ) ///
								texp(ln(_t)) cluster(cod)

estout crisis1b crisis2b crisis3b 	, cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) stats( N, fmt(%9.3f %9.0g)) ///
  legend  label collabels(none)  drop(*capcode)   varlabels(_cons Constant) nobaselevels eform


esttab  crisis1b crisis2b crisis3b using "Main_withcrisis.tex",  eform replace f  ///
 b(3) p(3) scalars(ll chi2)  drop(*capcode)  nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs alignment(D{.}{.}{-1})


 **# Plot the effects for the models based on full spec

		* Figure 1
quietly scurve_tvc, generate(Salience5_crisis) at( eu_salience .51  proposal_probability 0.41 amending 0  crisis_legislation 0 ///
			pew 124.3867  cn_ep_proposal 0.566  cn_polariz_proposal 1.1865 st_complex .0004  ///
			pres_elect 132.4 admin_burden_year 10   eu_polarization .55	mobilization 16.4  cap_agri 0 ///
			cap_civil 0 cap_ir 0 cap_cult 0 cap_fish 0   cap_frade 0   cap_market 0 cap_econ 0  ///
			cap_transp 0 cap_defen 0 cap_educ 0 cap_energy 0 cap_envi 0  cap_health 0 cap_immig 0 ///
			cap_labor 0 cap_law 0 cap_regio 0 cap_social 0 cap_techno 0 decision 0 regulation 0  directive 0  ) ///
		  tvc( proposal_probability   ///
						mobilization ///
								st_complex ///
							cap_agri  cap_civil  cap_ir  cap_cult  cap_fish  cap_frade   cap_market  cap_econ   cap_transp  ///
			 cap_defen  cap_educ  cap_energy  cap_envi   cap_health  cap_immig   cap_labor  cap_law  ///
			 cap_regio  cap_social  cap_techno ) ///
								texp(ln(_t)) replace


quietly scurve_tvc, generate(Salience95_crisis ) at( eu_salience .97 proposal_probability 0.41  amending 0  crisis_legislation 0 ///
			pew	124.3867 cn_ep_proposal 0.566  cn_polariz_proposal 1.1865 st_complex .0004 ///
			pres_elect 132.4 admin_burden_year 10   eu_polarization .56	mobilization 16.4  cap_agri 0 cap_civil 0 ///
			cap_ir 0 cap_cult 0 cap_fish 0   cap_frade 0   cap_market 0 cap_econ 0  cap_transp 0 cap_defen 0 ///
			cap_educ 0 cap_energy 0 cap_envi 0  cap_health 0 cap_immig 0  cap_labor 0 cap_law 0 cap_regio 0 ///
			cap_social 0 cap_techno 0 decision 0 regulation 0  directive 0  ) ///
			  tvc( proposal_probability   ///
				   	mobilization ///
								st_complex ///
								 cap_agri  cap_civil  cap_ir  cap_cult  cap_fish  cap_frade   cap_market  cap_econ   cap_transp  ///
			 cap_defen  cap_educ  cap_energy  cap_envi   cap_health  cap_immig   cap_labor  cap_law  ///
			 cap_regio  cap_social  cap_techno ) ///
								texp(ln(_t)) replace


lab var Salience5_crisis"Low Salience (5th percentile)"
lab var Salience95_crisis  "High Salience (5th percentile)"



twoway line Salience5_crisis Salience95_crisis _tscurve, c(J J)  title("") ///
						scheme(plotplainblind)  ///
						lwidth(medthick medthick ) lcolor (black gs10 ) lpattern(solid dash) ///
						xtitle("Weeks after the proposal initiation") ytitle("Probability of survival") ///
						ylabel(0(.2)1)  xlabel(0(50)400) legend(order( 1 "Low Salience (5th percentile)"  2  "High Salience (95th percentile)"  ) cols(2) size(medsmall) position(6))


*** Obtaining the substantive effects for salience
 
gen diff_salience= Salience95_crisis - Salience5_crisis
sum diff_salience 
list _tscurve diff_salience   Salience95_crisis  Salience5_crisis if _tscurve!=.
 
 
 graph save Graph "figure1_with crisis.gph", replace
graph export "figure1_with crisis.png", as(png) replace





/*Figure 2. Main text*/
quietly scurve_tvc, generate(Ambition1) at(proposal_probability 1  eu_salience .86   crisis_legislation 0 ///
			amending 0  ///
			pew	124.3867 cn_ep_proposal 0.566  cn_polariz_proposal 1.1865 st_complex .0004 ///
			pres_elect 132.4 admin_burden_year 10   eu_polarization .56	mobilization 16.4  cap_agri 0 cap_civil 0 ///
			cap_ir 0 cap_cult 0 cap_fish 0   cap_frade 0   cap_market 0 cap_econ 0  cap_transp 0 cap_defen 0 ///
			cap_educ 0 cap_energy 0 cap_envi 0  cap_health 0 cap_immig 0  cap_labor 0 cap_law 0 cap_regio 0 ///
			cap_social 0 cap_techno 0 decision 0 regulation 0  directive 0  ) ///
		 tvc( proposal_probability   ///
				   	mobilization ///
								st_complex ///
								 cap_agri  cap_civil  cap_ir  cap_cult  cap_fish  cap_frade   cap_market  cap_econ   cap_transp  ///
			 cap_defen  cap_educ  cap_energy  cap_envi   cap_health  cap_immig   cap_labor  cap_law  ///
			 cap_regio  cap_social  cap_techno ) ///
								texp(ln(_t)) replace

quietly scurve_tvc, generate(Ambition05) at(proposal_probability 0.41  eu_salience .86   crisis_legislation 0 ///
			amending 0  ///
			pew	124.3867 cn_ep_proposal 0.566  cn_polariz_proposal 1.1865 st_complex .0004 ///
			pres_elect 132.4 admin_burden_year 10   eu_polarization .56	mobilization 16.4  cap_agri 0 cap_civil 0 ///
			cap_ir 0 cap_cult 0 cap_fish 0   cap_frade 0   cap_market 0 cap_econ 0  cap_transp 0 cap_defen 0 ///
			cap_educ 0 cap_energy 0 cap_envi 0  cap_health 0 cap_immig 0  cap_labor 0 cap_law 0 cap_regio 0 ///
			cap_social 0 cap_techno 0 decision 0 regulation 0  directive 0  ) ///
			tvc( proposal_probability   ///
				   	mobilization ///
								st_complex ///
								 cap_agri  cap_civil  cap_ir  cap_cult  cap_fish  cap_frade   cap_market  cap_econ   cap_transp  ///
			 cap_defen  cap_educ  cap_energy  cap_envi   cap_health  cap_immig   cap_labor  cap_law  ///
			 cap_regio  cap_social  cap_techno ) ///
								texp(ln(_t)) replace


quietly scurve_tvc, generate(Ambition0) at(proposal_probability 0 eu_salience .86 amending 0 ///
		amending 0  ///
			pew	124.3867 cn_ep_proposal 0.566  cn_polariz_proposal 1.1865 st_complex .0004 ///
			pres_elect 132.4 admin_burden_year 10   eu_polarization .56	mobilization 16.4  cap_agri 0 cap_civil 0 ///
			cap_ir 0 cap_cult 0 cap_fish 0   cap_frade 0   cap_market 0 cap_econ 0  cap_transp 0 cap_defen 0 ///
			cap_educ 0 cap_energy 0 cap_envi 0  cap_health 0 cap_immig 0  cap_labor 0 cap_law 0 cap_regio 0 ///
			cap_social 0 cap_techno 0 decision 0 regulation 0  directive 0  ) ///
			tvc( proposal_probability   ///
				   	mobilization ///
								st_complex ///
								 cap_agri  cap_civil  cap_ir  cap_cult  cap_fish  cap_frade   cap_market  cap_econ   cap_transp  ///
			 cap_defen  cap_educ  cap_energy  cap_envi   cap_health  cap_immig   cap_labor  cap_law  ///
			 cap_regio  cap_social  cap_techno ) ///
								texp(ln(_t)) replace


lab var Ambition1 "Highest Pr. expansion of EU authority"
lab var Ambition05 "Mean Pr. expansion of EU authority"
lab var Ambition0 "Lowest Pr. expansion of EU authority"


/*plot the two way*/
twoway line Ambition1 Ambition05 Ambition0 _tscurve, ///
							scheme(plotplainblind)  ///
							lwidth(medthick medthick thick) lcolor (black gs10 sea) ///
							c(J J)  title("") ///
							xtitle("Weeks after proposal initiation", size(medsmall)) ///
							ytitle("Survival probability", size(medsmall)) ///
							ylabel(0(.2)1)  xlabel(0(50)400) ///
							legend(order( 1 "Highest Pr. expansion of EU authority" 2 "Mean Pr. expansion of EU authority"   3 "Lowest Pr. expansion of EU authority" ) cols(2) size(medsmall) position(6))


** obtaining substantive effects for expansion of authority
** THIS IS ALSO USED FOR FIGURE 4 in Appendix 2.7 --> see script below
gen diff_ambition1= Ambition1 - Ambition0
gen diff_ambition2= Ambition1 - Ambition05
sum diff_ambition1


list _tscurve diff_ambition1 Ambition1 Ambition0  if _tscurve!=.


/*Save*/
 graph save Graph "figure2_withcrisis.gph", replace
graph export "figure2_withcrisis.png", as(png) replace








 **# Robustness checks

 **#Robustness 1: Public Opinion

* Run model with PUBLIC OPINION inidicator  ( Robustness 1)
* eu_support stands in for salience/division -> placed in the opinion slot (top).
eststo coxPO: stcox eu_support ///
                   proposal_probability ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization ///
                   amending ///
                   crisis_legislation ///
                   i.leg_instr_num ///
                   i.capcode, ///
                        tvc( proposal_probability ///
                             mobilization ///
                             st_complex ///
                             i.capcode ) ///
                        texp(ln(_t)) cluster(cod)

estout coxPO 	, cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) stats( N, fmt(%9.3f %9.0g)) ///
  legend  label collabels(none)  drop(*capcode)   varlabels(_cons Constant) nobaselevels eform


esttab  coxPO using "Robustness1_PublicSupport.tex",  eform replace f  ///
 b(3) p(3) scalars(ll chi2)  drop(*capcode)  nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs alignment(D{.}{.}{-1})



 **#Robustness2 Competence age
 * competence_age = added control -> placed after crisis_legislation, before instrument.

** salience & polarization and no proposal  probabilities
eststo cox_age1: stcox c.eu_salience ///
                   eu_polarization ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization ///
                   amending ///
                   crisis_legislation ///
                   competence_age ///
                   i.leg_instr_num ///
                   i.capcode, ///
                        tvc( mobilization ///
                             st_complex ///
                             i.capcode ) ///
                        texp(ln(_t)) cluster(cod)


eststo cox_age2: stcox proposal_probability ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization ///
                   amending ///
                   crisis_legislation ///
                   competence_age ///
                   i.leg_instr_num ///
                   i.capcode, ///
                        tvc( proposal_probability ///
                             mobilization ///
                             st_complex ///
                             i.capcode ) ///
                        texp(ln(_t)) cluster(cod)

** full model
eststo cox_age3: stcox c.eu_salience ///
                   eu_polarization ///
                   proposal_probability ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization ///
                   amending ///
                   crisis_legislation ///
                   competence_age ///
                   i.leg_instr_num ///
                   i.capcode, ///
                        tvc( proposal_probability ///
                             mobilization ///
                             st_complex ///
                             i.capcode ) ///
                        texp(ln(_t)) cluster(cod)


estout cox_age1 cox_age2 cox_age3	, cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) stats( N, fmt(%9.3f %9.0g)) ///
  legend  label collabels(none)  drop(*capcode)   varlabels(_cons Constant) nobaselevels eform


esttab  cox_age1 cox_age2 cox_age3 using "Robustness2_Competence.tex",  eform replace f  ///
 b(3) p(3) scalars(ll chi2)  drop(*capcode)  nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs alignment(D{.}{.}{-1})




**# Robustness 3: Unanimity vs QMV
* unanimity = added control -> placed after crisis_legislation, before instrument.

** salience & polarization and no proposal  probabilities
eststo cox_qmv1: stcox c.eu_salience ///
                   eu_polarization ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization ///
                   amending ///
                   crisis_legislation ///
                   unanimity ///
                   i.leg_instr_num ///
                   i.capcode, ///
                        tvc( mobilization ///
                             st_complex ///
                             i.capcode ) ///
                        texp(ln(_t)) cluster(cod)



eststo  cox_qmv2: stcox proposal_probability ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization ///
                   amending ///
                   crisis_legislation ///
                   unanimity ///
                   i.leg_instr_num ///
                   i.capcode, ///
                        tvc( proposal_probability ///
                             mobilization ///
                             st_complex ///
                             i.capcode ) ///
                        texp(ln(_t)) cluster(cod)

** full model
eststo  cox_qmv3: stcox c.eu_salience ///
                   eu_polarization ///
                   proposal_probability ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization ///
                   amending ///
                   crisis_legislation ///
                   unanimity ///
                   i.leg_instr_num ///
                   i.capcode, ///
                        tvc( proposal_probability ///
                             mobilization ///
                             st_complex ///
                             i.capcode ) ///
                        texp(ln(_t)) cluster(cod)


estout  cox_qmv1  cox_qmv2  cox_qmv3	, cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.1 ** 0.05 *** 0.010) stats( N, fmt(%9.3f %9.0g)) ///
  legend  label collabels(none)  drop(*capcode)   varlabels(_cons Constant) nobaselevels eform


esttab  cox_qmv1  cox_qmv2  cox_qmv3 using "Robustness3_Unanimity.tex",  eform replace f  ///
 b(3) p(3) scalars(ll chi2)  drop(*capcode)  nomtitle label star(* 0.1 ** 0.05 *** 0.01) ///
 booktabs alignment(D{.}{.}{-1})


**# Robustness 4 End of presidency
* weeks_till_end_presid = added control -> placed after crisis_legislation, before instrument.

eststo cox_pres1: stcox c.eu_salience ///
                   eu_polarization ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization ///
                   amending ///
                   crisis_legislation ///
                   weeks_till_end_presid ///
                   i.leg_instr_num ///
                   i.capcode, ///
                        tvc( mobilization ///
                             st_complex ///
                             i.capcode ) ///
                        texp(ln(_t)) cluster(cod)


eststo  cox_pres2: stcox proposal_probability ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization ///
                   amending ///
                   crisis_legislation ///
                   weeks_till_end_presid ///
                   i.leg_instr_num ///
                   i.capcode, ///
                        tvc( proposal_probability ///
                             mobilization ///
                             st_complex ///
                             i.capcode ) ///
                        texp(ln(_t)) cluster(cod)

** full model
eststo  cox_pres3: stcox c.eu_salience ///
                   eu_polarization ///
                   proposal_probability ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization ///
                   amending ///
                   crisis_legislation ///
                   weeks_till_end_presid ///
                   i.leg_instr_num ///
                   i.capcode, ///
                        tvc( proposal_probability ///
                             mobilization ///
                             st_complex ///
                             i.capcode ) ///
                        texp(ln(_t)) cluster(cod)


estout  cox_pres1  cox_pres2  cox_pres3	, cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) stats( N, fmt(%9.3f %9.0g)) ///
  legend  label collabels(none)  drop(*capcode)   varlabels(_cons Constant) nobaselevels eform


esttab  cox_pres1  cox_pres2  cox_pres3 using "Robustness4_Presidency_end.tex",  eform replace f  ///
 b(3) p(3) scalars(ll chi2)  drop(*capcode)  nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs alignment(D{.}{.}{-1})





 **# Robustness 5: Differnet TVC set
 * No new covariate; election variables added to tvc(). Covariates in manuscript order.


 /*Model 1 ** salience & polarization with both elections TVC*/

eststo cox1c: stcox c.eu_salience ///
                   eu_polarization ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization ///
                   amending ///
                   crisis_legislation ///
                   i.leg_instr_num ///
                   i.capcode, ///
                        tvc( mobilization ///
                             st_complex ///
                             presid_elect_proposal_weeks ///
                             prop_to_election_weeks_new ///
                             i.capcode ) ///
                        texp(ln(_t)) cluster(cod)

/*Model 2 ** EU Authority expansion */
eststo cox2c: stcox proposal_probability ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization ///
                   amending ///
                   crisis_legislation ///
                   i.leg_instr_num ///
                   i.capcode, ///
                        tvc( proposal_probability ///
                             mobilization ///
                             st_complex ///
                             presid_elect_proposal_weeks ///
                             prop_to_election_weeks_new ///
                             i.capcode ) ///
                        texp(ln(_t)) cluster(cod)

/*Model 3 ** FULL model with salience TVC*/
eststo cox3c: stcox c.eu_salience ///
                   eu_polarization ///
                   proposal_probability ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization ///
                   amending ///
                   crisis_legislation ///
                   i.leg_instr_num ///
                   i.capcode, ///
                        tvc( proposal_probability ///
                             mobilization ///
                             st_complex ///
                             presid_elect_proposal_weeks ///
                             prop_to_election_weeks_new ///
                             i.capcode ) ///
                        texp(ln(_t)) cluster(cod)

estout cox1c cox2c cox3c 	, cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) stats( N, fmt(%9.3f %9.0g)) ///
  legend  label collabels(none)  drop(*capcode)   varlabels(_cons Constant) nobaselevels eform


esttab  cox1c cox2c cox3c using "Robustness5_Selectiontvc.tex",  eform replace f  ///
 b(3) p(3) scalars(ll chi2)  drop(*capcode)  nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs alignment(D{.}{.}{-1})






**# Robustness 6: Commission-term fixed effects
* i.commission_term FE -> FE block before i.capcode.
* NOTE: these models intentionally drop prop_to_election_weeks_new (Commission
*       term is mechanically aligned with the EP electoral cycle; see manuscript 2.6).


/* Model 1 ** salience & polarisation with Commission FE */
eststo comm1b: stcox c.eu_salience ///
                    eu_polarization ///
                    cn_ep_proposal ///
                    cn_polariz_proposal ///
                    presid_elect_proposal_weeks ///
                    admin_burden_year ///
                    st_complex ///
                    mobilization ///
                    amending ///
                    crisis_legislation ///
                    i.leg_instr_num ///
                    i.commission_term ///
                    i.capcode, ///
                        tvc( mobilization st_complex i.capcode) ///
                        texp(ln(_t)) cluster(cod)


/* Model 2 ** EU authority expansion with Commission FE */
eststo comm2b: stcox proposal_probability ///
                    cn_ep_proposal ///
                    cn_polariz_proposal ///
                    presid_elect_proposal_weeks ///
                    admin_burden_year ///
                    st_complex ///
                    mobilization ///
                    amending ///
                    crisis_legislation ///
                    i.leg_instr_num ///
                    i.commission_term ///
                    i.capcode, ///
                        tvc(proposal_probability mobilization st_complex i.capcode) ///
                        texp(ln(_t)) cluster(cod)


/* Model 3 ** FULL model with Commission FE */
eststo comm3b: stcox c.eu_salience ///
                    eu_polarization ///
                    proposal_probability ///
                    cn_ep_proposal ///
                    cn_polariz_proposal ///
                    presid_elect_proposal_weeks ///
                    admin_burden_year ///
                    st_complex ///
                    mobilization ///
                    amending ///
                    crisis_legislation ///
                    i.leg_instr_num ///
                    i.commission_term ///
                    i.capcode, ///
                        tvc(proposal_probability  mobilization st_complex i.capcode) ///
                        texp(ln(_t)) cluster(cod)


*Report the three models side by side
estout comm1b comm2b comm3b, cells(b(star fmt(%9.3f)) p(par)) ///
    starlevels( * 0.10 ** 0.05 *** 0.010) stats(N ll chi2, fmt(%9.0g %9.3f %9.3f)) ///
    legend label collabels(none) ///
    drop(*capcode) varlabels(_cons Constant) nobaselevels eform



esttab   comm1b comm2b comm3b using "Robustness6_CommissionTerm.tex",  eform replace f  ///
 b(3) p(3) scalars(ll chi2)  drop(*capcode)  nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs alignment(D{.}{.}{-1})


**Optional — Wald test on the joint significance of Commission dummies
*test 2.commission_term 3.commission_term 1.commission_term



**# Robustness 7.1: controlling for media attention
* media_ave = added salience-type control -> placed just after the opinion block.
eststo cox_media1: stcox c.eu_salience ///
                   eu_polarization ///
                   media_ave ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization ///
                   amending ///
                   crisis_legislation ///
                   i.leg_instr_num ///
                   i.capcode, ///
                        tvc( mobilization ///
                             st_complex ///
                             i.capcode ) ///
                        texp(ln(_t)) cluster(cod)


eststo  cox_media2: stcox media_ave ///
                   proposal_probability ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization ///
                   amending ///
                   crisis_legislation ///
                   i.leg_instr_num ///
                   i.capcode, ///
                        tvc( proposal_probability ///
                             mobilization ///
                             st_complex ///
                             i.capcode ) ///
                        texp(ln(_t)) cluster(cod)

** full model
eststo  cox_media3: stcox c.eu_salience ///
                   eu_polarization ///
                   media_ave ///
                   proposal_probability ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization ///
                   amending ///
                   crisis_legislation ///
                   i.leg_instr_num ///
                   i.capcode, ///
                        tvc( proposal_probability ///
                             mobilization ///
                             st_complex ///
                             i.capcode ) ///
                        texp(ln(_t)) cluster(cod)


estout  cox_media1  cox_media2  cox_media3	, cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) stats( N, fmt(%9.3f %9.0g)) ///
  legend  label collabels(none)  drop(*capcode)   varlabels(_cons Constant) nobaselevels eform


esttab  cox_media1  cox_media2  cox_media3 using "Robustness7_1_Media.tex",  eform replace f  ///
 b(3) p(3) scalars(ll chi2)  drop(*capcode)  nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs alignment(D{.}{.}{-1})





**# Robustness 7.2: Salience as media attention
* media_ave REPLACES eu_salience -> placed in the salience slot (top).
eststo cox_smedia1: stcox media_ave ///
                   eu_polarization ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization ///
                   amending ///
                   crisis_legislation ///
                   i.leg_instr_num ///
                   i.capcode, ///
                        tvc( mobilization ///
                             st_complex ///
                             i.capcode ) ///
                        texp(ln(_t)) cluster(cod)


eststo  cox_smedia2: stcox proposal_probability ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization ///
                   amending ///
                   crisis_legislation ///
                   i.leg_instr_num ///
                   i.capcode, ///
                        tvc( proposal_probability ///
                             mobilization ///
                             st_complex ///
                             i.capcode ) ///
                        texp(ln(_t)) cluster(cod)

** full model
eststo  cox_smedia3: stcox media_ave ///
                   eu_polarization ///
                   proposal_probability ///
                   cn_ep_proposal ///
                   cn_polariz_proposal ///
                   presid_elect_proposal_weeks ///
                   admin_burden_year ///
                   st_complex ///
                   prop_to_election_weeks_new ///
                   mobilization ///
                   amending ///
                   crisis_legislation ///
                   i.leg_instr_num ///
                   i.capcode, ///
                        tvc( proposal_probability ///
                             mobilization ///
                             st_complex ///
                             i.capcode ) ///
                        texp(ln(_t)) cluster(cod)


estout  cox_smedia1  cox_smedia2  cox_smedia3	, cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) stats( N, fmt(%9.3f %9.0g)) ///
  legend  label collabels(none)  drop(*capcode)   varlabels(_cons Constant) nobaselevels eform


esttab  cox_smedia1  cox_smedia2  cox_smedia3 using "Robustness7_2_MediaasSalience.tex",  eform replace f  ///
 b(3) p(3) scalars(ll chi2)  drop(*capcode)  nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs alignment(D{.}{.}{-1})





**# Robustness 8. Visualisation Effects of EU authority expansion

**FIGURE 4 in Appendix 2.7.***
twoway line diff_ambition1   diff_ambition2    _tscurve ,  scheme(plotplainblind)  ///
			lwidth(thin)  lpattern(gs10  ) xline(83, lwidth( medthin) lpattern(dash_dot) lcolor(sea)) ///
			title("") 	 ///
			yline(0, lpattern(solid) lwidth(thin)  lcolor(gs10)) ylab(, nogrid)  ///
			xtitle("Weeks after proposal publication", size(medsmall)) ///
			ytitle("{&Delta} in Survival probabilities", size(medsmall))  xlabel(0(100) 400) ///
			xaxis(1 2) xla(83 "{&mu} duration",   axis(2) nogrid notick glcolor(sea)) xtitle("", axis(2) ) ///
			ylabel(-.05(.1) .4)  legend(on order(1 "{&Delta} High (1)  & Low Ambition (0)" 2 "{&Delta} High (0) & {&mu} Ambition (0.45)") ///
			size(medsmall)col(2) position(6))


graph save "Graph" "Figure4.gph" , replace
graph export "Figure4.png", as(png) name("Graph")	replace


 log close
