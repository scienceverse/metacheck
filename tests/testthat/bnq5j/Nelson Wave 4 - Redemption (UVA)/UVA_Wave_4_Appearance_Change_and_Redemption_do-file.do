import delimited "/Users/michaelodonnell/Dropbox/Research/UVA Wave 4 Redemption/UVA_Wave_4__Appearance_Change_and_Redemption__Study_23_to_analyze.csv", encoding(UTF-8) 
tabu v5
keep if v5==1
replace condition="1" if condition== "appearance.change"
replace condition="0" if condition== "control"
destring condition, replace
label define condition 1 "appearance change" 0 "control"
label values condition condition

summ consent
tabu consent
keep if consent==4
tabu attcheck
keep if attcheck==1
tabu attcheck2
keep if attcheck2 ==7

egen remorse = rowtotal(male1remorse male2remorse female1remorse female2remorse)
replace remorse = remorse/4
summ remorse

tabu condition wave_1
save "/Users/michaelodonnell/Dropbox/Research/UVA Wave 4 Redemption/UVA_Wave_4_Appearance_Change_and_Redemption_cleaned.dta"

preserve
keep if wave_1==1
by condition, sort : summarize remorse
ttest remorse, by(condition)
esize twosample remorse, by(condition) cohensd
restore

preserve
keep if wave_1==0
by condition, sort : summarize remorse
ttest remorse, by(condition)
esize twosample remorse, by(condition) cohensd
restore

by condition, sort : summarize remorse
ttest remorse, by(condition)
esize twosample remorse, by(condition) cohensd

