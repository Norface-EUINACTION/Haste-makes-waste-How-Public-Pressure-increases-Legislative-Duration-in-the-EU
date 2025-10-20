/*This is an integrated replications script for the manuscript titled
"Haste makes waste: How Public Pressure Increases Legislative Duration in the EU"  by
A.Ershova, A Khokhlova, and  N Yordanova, */

set scheme burd 

 * Checking different structure of the TVC
log using "DIRECTORY.smcl"
 cd "[PATH]"
 

use  "Data_Haste_makes_Waste.dta" , replace


**Appendix 1.1  Figure 3 DV Distribution**


hist procedure_in_weeks, bin(40) fcolor(sea%80) xtitle("Duration of Legislaitve Procedures in Weeks")

graph export "DV.png", as(png) replace

*Appendix 1.1 Descriptive statistics**

estpost  ///
 sum procedure_in_weeks  proposal_probability ///
						amending ///
						eu_salience ///
						eu_polarization ///
						mobilization  ///  * mobilization is a control here, so  goes in all models
						st_complex ///
						cn_ep_proposal ///
						cn_polariz_proposal ///
						admin_burden_year  ///
						presid_elect_proposal_weeks ///
						prop_to_election_weeks_new ///
						regulation decision directive 

est store descr						
						
esttab descr  using "decriptive_table.tex", replace ///
 cells("mean(fmt(%6.2fc)) sd(fmt(%6.2fc)) min max count")   nonumber ///
 nomtitle nonote noobs label booktabs ///
	collabels( "Mean" "SD" "Min" "Max" "N")  ///
	title("Descriptive Statistics")


* Appendix 1.2. Proportionate hazard test. 

stcox  		proposal_probability ///  
			eu_salience ///
			eu_polarization  ///
					mobilization ///
				   amending  ///
				   presid_elect_proposal_weeks ///
				   prop_to_election_weeks_new  ///
				   st_complex ///  TVC
				   cn_ep_proposal ///
				   cn_polariz_proposal ///			  
				   i.leg_instr_num  ///
				   admin_burden_year /// 
				   i.capcode
estat phtest, detail
***** Significance for   st_complex // mobilization /// proposal_p~y //  4.capcode  //   15.capcode  -->> these could be to be the TVC




*****Prepare data for the Duration models

stset procedure_in_weeks, failure (completion) id(cod)
 
 ** split data
 stsplit, at(failures)
 
 
 ****** Correlation table in Appendix 1.1 
 
 estpost correlate proposal_probability eu_salience ///
				eu_polarization mobilization ///
				amending cn_ep_proposal ///
				cn_polariz_proposal 	///				
				presid_elect_proposal_weeks ///
				st_complex prop_to_election_weeks_new  ///
				admin_burden_year, matrix listwise
				
				
esttab using "corr.tex", unstack not noobs  label compress  cells(b(fmt(2))) replace
 
 
 
 **Main models
 
 

*****MAIN MODELS************************Table 1.

 /*Model 1 ** salience & polarization */
 
eststo cox1b: stcox c.eu_salience ///
				   eu_polarization ///
				  mobilization ///
				   amending  ///
				   presid_elect_proposal_weeks ///
				   prop_to_election_weeks_new  ///
				   st_complex ///  
				   cn_ep_proposal ///
				   cn_polariz_proposal ///			  
				   i.leg_instr_num  ///
				   admin_burden_year /// 
				   i.capcode, ///
						 tvc(  ///
						mobilization ///
								st_complex ///	
							i.capcode	   ) ///
								texp(ln(_t)) cluster(cod)
				   

/*Model 2 ** EU Authority expansion */
eststo cox2b: stcox proposal_probability ///
				    mobilization ///
				   amending  ///
				   presid_elect_proposal_weeks ///
				   prop_to_election_weeks_new  ///
				   st_complex ///  
				   cn_ep_proposal ///
				   cn_polariz_proposal ///			  
				   i.leg_instr_num  ///
				   admin_burden_year /// 
				   i.capcode, ///
						 tvc( proposal_probability   ///
						 mobilization ///
								st_complex ///	
							i.capcode	   ) ///
								texp(ln(_t)) cluster(cod)
 
/*Model 2 ** FULL model*/
eststo cox3b: stcox c.eu_salience ///
				   eu_polarization ///
				   proposal_probability  ///
				   mobilization ///
				   amending  ///
				   presid_elect_proposal_weeks ///
				   prop_to_election_weeks_new  ///
				   st_complex ///  
				   cn_ep_proposal ///
				   cn_polariz_proposal ///			  
				   i.leg_instr_num  ///
				   admin_burden_year /// 
				   i.capcode, ///
				     tvc( proposal_probability   ///
						 mobilization ///
								st_complex ///	
								i.capcode   ) ///
								texp(ln(_t)) cluster(cod)

estout cox1b cox2b cox3b 	, cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) stats( N, fmt(%9.3f %9.0g)) ///
  legend  label collabels(none)  drop(*capcode)   varlabels(_cons Constant) nobaselevels eform 
 				   

esttab  cox1b cox2b cox3b using "Main_1510_cap_tvc.tex",  eform replace f  ///
 b(3) p(3) scalars(ll chi2)  drop(*capcode)  nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs alignment(D{.}{.}{-1})
 
 
 
 
 
 
 
** Generate MAIN FIGURES **						

/*Figure 1. Main text*/


**#Salience: For salience#1

******** Plot the effect of salience  only using full model****

*to plot the following variables need to be copied with a shorter name
gen pew=prop_to_election_weeks_new
lab var pew  "Time from Proposal to EU elections(Copy)"

gen pres_elect=presid_elect_proposal_weeks
lab var pres_elect "proximity of domestic elections in presidency(Copy)"

				
quietly scurve_tvc, generate(Salience5) at( eu_salience .51  proposal_probability 0.41 amending 0 ///
			pew 124.3867  cn_ep_proposal 0.566  cn_polariz_proposal 0.24 st_complex .0004  ///
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
				   				   
	
quietly scurve_tvc, generate(Salience95) at( eu_salience .97 proposal_probability 0.41  amending 0  ///
			pew	124.3867 cn_ep_proposal 0.566  cn_polariz_proposal 0.24 st_complex .0004 ///
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
				   				   

lab var Salience5 "Low Salience (5th percentile)"
lab var Salience95 "High Salience (5th percentile)"


					
twoway line Salience5 Salience95 _tscurve, c(J J)  title("") ///
						scheme(plotplainblind)  ///
						lwidth(medthick medthick ) lcolor (black gs10 ) lpattern(solid dash) ///
						xtitle("Weeks after the proposal publication") ytitle("Probability of survival") ///
						ylabel(0(.2)1)  xlabel(0(50)400) legend(order( 1 "Low Salience (5th percentile)"  2  "High Salience (95th percentile)"  ) cols(2) size(medsmall) position(6))					
						
							
 graph save Graph "figure1_cap_TVC.gph", replace
graph export "figure1_cap_TVC.png", as(png) replace

 *** Obtaining the substantive effects
 
 
gen diff_salience= Salience95 - Salience5
sum diff_salience 
list _tscurve diff_salience   Salience95  Salience5 if _tscurve!=.
 
 
 
 
/*Figure 2. Main text*/
quietly scurve_tvc, generate(Ambition1) at(proposal_probability 1  eu_salience .86  ///
			amending 0  ///
			pew	124.3867 cn_ep_proposal 0.566  cn_polariz_proposal 0.24 st_complex .0004 ///
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
		
quietly scurve_tvc, generate(Ambition05) at(proposal_probability 0.41  eu_salience .86  ///
			amending 0  ///
			pew	124.3867 cn_ep_proposal 0.566  cn_polariz_proposal 0.24 st_complex .0004 ///
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
			pew	124.3867 cn_ep_proposal 0.566  cn_polariz_proposal 0.24 st_complex .0004 ///
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
				
				
				
twoway line Ambition1 Ambition05 Ambition0 _tscurve, ///
							scheme(plotplainblind)  ///
							lwidth(medthick medthick thick) lcolor (black gs10 sea) ///
							c(J J)  title("") ///
							xtitle("Weeks after proposal publication", size(medsmall)) ///
							ytitle("Survival probability", size(medsmall)) ///
							ylabel(0(.2)1)  xlabel(0(50)400) ///
							legend(order( 1 "Highest Pr. expansion of EU authority" 2 "Mean Pr. expansion of EU authority"   3 "Lowest Pr. expansion of EU authority" ) cols(2) size(medsmall) position(6))					
						
 
 
 graph save Graph "figure2_cap_TVC.gph", replace
graph export "figure2_cap_TVC.png", as(png) replace

** obtaining substantive effects
gen diff_ambition1= Ambition1 - Ambition0

gen diff_ambition2= Ambition1 - Ambition05
sum diff_ambition1 
list _tscurve diff_ambition1   Ambition1  Ambition0 if _tscurve!=.
						




 
 
 ************** ROBUSTNESS CHECKS***************
 *robustness 1_PO
 						
*9. Run model with PUBLIC OPINION inidicator  ( Robustness 1)
eststo coxPO: stcox proposal_probability eu_support ///
				    mobilization ///
				   amending  ///
				   presid_elect_proposal_weeks ///
				   prop_to_election_weeks_new  ///
				   st_complex ///  
				   cn_ep_proposal ///
				   cn_polariz_proposal ///			  
				   i.leg_instr_num  ///
				   admin_burden_year /// 
				   i.capcode, ///
						   tvc( proposal_probability   ///
						 mobilization ///
								st_complex ///	
								i.capcode     ) ///
								texp(ln(_t)) cluster(cod)
 					
estout coxPO 	, cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) stats( N, fmt(%9.3f %9.0g)) ///
  legend  label collabels(none)  drop(*capcode)   varlabels(_cons Constant) nobaselevels eform 
 				   

esttab  coxPO using "Robustness1_PublicSupport.tex",  eform replace f  ///
 b(3) p(3) scalars(ll chi2)  drop(*capcode)  nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs alignment(D{.}{.}{-1})

 
 
 *Robustness2 competence age
 
** salience & polarization and no proposal  probabilities 
eststo cox_age1: stcox c.eu_salience ///
				   eu_polarization ///
				  mobilization ///
				  competence_age ///
				   amending  ///
				   presid_elect_proposal_weeks ///
				   prop_to_election_weeks_new  ///
				   st_complex ///  
				   cn_ep_proposal ///
				   cn_polariz_proposal ///			  
				   i.leg_instr_num  ///
				   admin_burden_year /// 
				   i.capcode, ///
							  tvc(   ///
						 mobilization ///
								st_complex ///	
							i.capcode  	   ) ///
								texp(ln(_t)) cluster(cod)


eststo cox_age2: stcox proposal_probability ///
				    mobilization ///
				   amending  ///
				   competence_age ///
				   presid_elect_proposal_weeks ///
				   prop_to_election_weeks_new  ///
				   st_complex ///  
				   cn_ep_proposal ///
				   cn_polariz_proposal ///			  
				   i.leg_instr_num  ///
				   admin_burden_year /// 
				   i.capcode, ///
						  tvc( proposal_probability   ///
						 mobilization ///
								st_complex ///	
							i.capcode  	   ) ///
								texp(ln(_t)) cluster(cod)
 
** full model	
eststo cox_age3: stcox c.eu_salience ///
				   eu_polarization ///
				   proposal_probability  ///
				   mobilization ///
				   competence_age ///
				   amending  ///
				   presid_elect_proposal_weeks ///
				   prop_to_election_weeks_new  ///
				   st_complex ///  
				   cn_ep_proposal ///
				   cn_polariz_proposal ///			  
				   i.leg_instr_num  ///
				   admin_burden_year /// 
				   i.capcode, ///
				     tvc( proposal_probability   ///
						 mobilization ///
								st_complex ///	
							i.capcode  	   ) ///
								texp(ln(_t)) cluster(cod)
				   				   

estout cox_age1 cox_age2 cox_age3	, cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) stats( N, fmt(%9.3f %9.0g)) ///
  legend  label collabels(none)  drop(*capcode)   varlabels(_cons Constant) nobaselevels eform 
 				   

esttab  cox_age1 cox_age2 cox_age3 using "Robustness2_Competence.tex",  eform replace f  ///
 b(3) p(3) scalars(ll chi2)  drop(*capcode)  nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs alignment(D{.}{.}{-1})
					
								
 
*robustness 3 crisis

** salience & polarization and no proposal  probabilities 
eststo cox_crisis1: stcox c.eu_salience ///
				   eu_polarization ///
				  mobilization ///
				  crisis ///
				   amending  ///
				   presid_elect_proposal_weeks ///
				   prop_to_election_weeks_new  ///
				   st_complex ///  
				   cn_ep_proposal ///
				   cn_polariz_proposal ///			  
				   i.leg_instr_num  ///
				   admin_burden_year /// 
				   i.capcode, ///
						     tvc(   ///
						 mobilization ///
								st_complex ///	
							i.capcode  	   ) ///
								texp(ln(_t)) cluster(cod)

eststo cox_crisis2: stcox proposal_probability ///
				    mobilization ///
				   amending  ///
				   crisis ///
				   presid_elect_proposal_weeks ///
				   prop_to_election_weeks_new  ///
				   st_complex ///  
				   cn_ep_proposal ///
				   cn_polariz_proposal ///			  
				   i.leg_instr_num  ///
				   admin_burden_year /// 
				   i.capcode, ///
						     tvc( proposal_probability   ///
						 mobilization ///
								st_complex ///	
							i.capcode  	   ) ///
								texp(ln(_t)) cluster(cod)
 
** full model	
eststo cox_crisis3: stcox c.eu_salience ///
				   eu_polarization ///
				   proposal_probability  ///
				   mobilization ///
				   crisis ///
				   amending  ///
				   presid_elect_proposal_weeks ///
				   prop_to_election_weeks_new  ///
				   st_complex ///  
				   cn_ep_proposal ///
				   cn_polariz_proposal ///			  
				   i.leg_instr_num  ///
				   admin_burden_year /// 
				   i.capcode , ///
				       tvc( proposal_probability   ///
						 mobilization ///
								st_complex ///	
							i.capcode  	   ) ///
								texp(ln(_t)) cluster(cod)

estout cox_crisis1 cox_crisis2 cox_crisis3	, cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) stats( N, fmt(%9.3f %9.0g)) ///
  legend  label collabels(none)  drop(*capcode)   varlabels(_cons Constant) nobaselevels eform 
 				   

esttab  cox_crisis1 cox_crisis2 cox_crisis3 using "Robustness3_Crisis.tex",  eform replace f  ///
 b(3) p(3) scalars(ll chi2)  drop(*capcode)  nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs alignment(D{.}{.}{-1})

*robustness 4 unanimity

** salience & polarization and no proposal  probabilities 
eststo cox_qmv1: stcox c.eu_salience ///
				   eu_polarization ///
				  mobilization ///
				  unanimity ///
				   amending  ///
				   presid_elect_proposal_weeks ///
				   prop_to_election_weeks_new  ///
				   st_complex ///  
				   cn_ep_proposal ///
				   cn_polariz_proposal ///			  
				   i.leg_instr_num  ///
				   admin_burden_year /// 
				   i.capcode, ///
							    tvc( proposal_probability   ///
						 mobilization ///
								st_complex ///	
								i.capcode     ) ///
								texp(ln(_t)) cluster(cod)
				   


eststo  cox_qmv2: stcox proposal_probability ///
				    mobilization ///
				   amending  ///
				   unanimity ///
				   presid_elect_proposal_weeks ///
				   prop_to_election_weeks_new  ///
				   st_complex ///  
				   cn_ep_proposal ///
				   cn_polariz_proposal ///			  
				   i.leg_instr_num  ///
				   admin_burden_year /// 
				   i.capcode, ///
							   tvc(   ///
						 mobilization ///
								st_complex ///	
								i.capcode     ) ///
								texp(ln(_t)) cluster(cod)
 
** full model	
eststo  cox_qmv3: stcox c.eu_salience ///
				   eu_polarization ///
				   proposal_probability  ///
				   mobilization ///
				   unanimity ///
				   amending  ///
				   presid_elect_proposal_weeks ///
				   prop_to_election_weeks_new  ///
				   st_complex ///  
				   cn_ep_proposal ///
				   cn_polariz_proposal ///			  
				   i.leg_instr_num  ///
				   admin_burden_year /// 
				   i.capcode, ///
				       tvc( proposal_probability   ///
						 mobilization ///
								st_complex ///	
							i.capcode  	   ) ///
								texp(ln(_t)) cluster(cod)
				   				   

estout  cox_qmv1  cox_qmv2  cox_qmv3	, cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.1 ** 0.05 *** 0.010) stats( N, fmt(%9.3f %9.0g)) ///
  legend  label collabels(none)  drop(*capcode)   varlabels(_cons Constant) nobaselevels eform 
 				   

esttab  cox_qmv1  cox_qmv2  cox_qmv3 using "Robustness4_Unanimity.tex",  eform replace f  ///
 b(3) p(3) scalars(ll chi2)  drop(*capcode)  nomtitle label star(* 0.1 ** 0.05 *** 0.01) ///
 booktabs alignment(D{.}{.}{-1})

*ribustness 5 end of presidency 

eststo cox_pres1: stcox c.eu_salience ///
				   eu_polarization ///
				  mobilization ///
				 weeks_till_end_presid ///
				   amending  ///
				   presid_elect_proposal_weeks ///
				   prop_to_election_weeks_new  ///
				   st_complex ///  
				   cn_ep_proposal ///
				   cn_polariz_proposal ///			  
				   i.leg_instr_num  ///
				   admin_burden_year /// 
				   i.capcode, ///
							  tvc(   ///
						 mobilization ///
								st_complex ///	
							i.capcode  	   ) ///
								texp(ln(_t)) cluster(cod)


eststo  cox_pres2: stcox proposal_probability ///
				    mobilization ///
				   amending  ///
				   weeks_till_end_presid ///
				   presid_elect_proposal_weeks ///
				   prop_to_election_weeks_new  ///
				   st_complex ///  
				   cn_ep_proposal ///
				   cn_polariz_proposal ///			  
				   i.leg_instr_num  ///
				   admin_burden_year /// 
				   i.capcode, ///
							   tvc( proposal_probability   ///
						 mobilization ///
								st_complex ///	
							i.capcode  	   ) ///
								texp(ln(_t)) cluster(cod)
 
** full model	
eststo  cox_pres3: stcox c.eu_salience ///
				   eu_polarization ///
				   proposal_probability  ///
				   mobilization ///
				  weeks_till_end_presid ///
				   amending  ///
				   presid_elect_proposal_weeks ///
				   prop_to_election_weeks_new  ///
				   st_complex ///  
				   cn_ep_proposal ///
				   cn_polariz_proposal ///			  
				   i.leg_instr_num  ///
				   admin_burden_year /// 
				   i.capcode, ///
				       tvc( proposal_probability   ///
						 mobilization ///
								st_complex ///	
							i.capcode  	   ) ///
								texp(ln(_t)) cluster(cod)
				   				   

estout  cox_pres1  cox_pres2  cox_pres3	, cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) stats( N, fmt(%9.3f %9.0g)) ///
  legend  label collabels(none)  drop(*capcode)   varlabels(_cons Constant) nobaselevels eform 
 				   

esttab  cox_pres1  cox_pres2  cox_pres3 using "Robustness5_Presidency_end.tex",  eform replace f  ///
 b(3) p(3) scalars(ll chi2)  drop(*capcode)  nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs alignment(D{.}{.}{-1})
					

						
***Extra Figures for the Appendix						

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
									
omtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs alignment(D{.}{.}{-1})
 

log close		
