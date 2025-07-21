import delimited "/Users/michaelodonnell/Dropbox/Research/UCSB Worldview Misattribution/UCSB_Misattribution_Fetzer_to_analyze.csv", encoding(UTF-8) 
drop if v10==0
drop if consent==2

destring condition, replace
tabu att_check cond
keep if att_check==2 & cond==0 | att_check==1 & cond==1
tabu att_check cond

egen dv = rowtotal(rate1 rate2 rate3 rate4 rate5 rate6 rate7 rate8 rate9 rate10 rate11 rate12 rate13 rate14 rate15 a1_true a2_true a3_true a4_true a5_true a6_true a7_true a8_true a9_true a10_true a11_true a12_true a13_true a14_true a15_true)
replace dv = dv/15
summ dv

preserve
keep if first750==0
ttest dv, by(condition) unequal
summ v10
esize twosample dv, by(condition) cohensd
restore

preserve
keep if first750==1
ttest dv, by(condition) unequal
summ v10
esize twosample dv, by(condition) cohensd
restore

ttest dv, by(condition) unequal
summ v10
esize twosample dv, by(condition) cohensd
