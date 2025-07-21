import delimited "/Users/michaelodonnell/Dropbox/Research/UCSB Wave 3 Fetzer - Fast Social Desirability/fastSocDesirability_UCSB_to_analyze.csv", clear
drop if v10==0
tabu condition
tabu consent
drop if consent==2
tabu condition
destring condition, replace
tabu ac1 condition
ren condition condition_fast
drop if condition==0&ac1==1 | condition==1&ac1==2
tabu condition wave
tabu ac1 condition

recode q1 2=0
recode q2 2=0
recode q3 2=0
recode q4 2=0
recode q5 2=0
recode q6 2=0
recode q7 2=0
recode q8 2=0
recode q9 2=0
recode q10 2=0

gen q2r=q2
recode q2r 0=-1
recode q2r 1=0
recode q2r -1=1
gen q4r=q4
recode q4r 0=-1
recode q4r 1=0
recode q4r -1=1
gen q6r=q6
recode q6r 0=-1
recode q6r 1=0
recode q6r -1=1
gen q7r=q7
recode q7r 0=-1
recode q7r 1=0
recode q7r -1=1
gen q8r=q8
recode q8r 0=-1
recode q8r 1=0
recode q8r -1=1

egen soc_des_total = rowtotal(q1 q2r q3 q4r q5 q6r q7r q8r q9 q10)

save "/Users/michaelodonnell/Dropbox/Research/UCSB Wave 3 Fetzer - Fast Social//
Desirability/fastSocDesirability_UCSB_to_analyze_dropped_incompletes_dropped_att_check.dta", replace

preserve
keep if wave==1
bootstrap, reps(10000) seed(1): anova soc_des_total condition
margins condition
restore

preserve
keep if wave1==0
tabu wave
bootstrap, reps(10000) seed(1): anova soc_des_total condition
margins condition

restore
bootstrap, reps(10000) seed(1): anova soc_des_total condition
margins cond
