drop if v10==0
drop if consent==2

keep if att_check==1

gen condition condition_worse =.
replace condition=1 if missing(better_b)
replace condition = 0 if missing(condition)

egen rating_b = rowtotal (better_b worse_b)

preserve
keep if wave_2==0
ttest better_b== worse_b, unpaired
esize twosample rating_b, by(condition) all
restore

preserve
keep if wave_2==1
ttest better_b== worse_b, unpaired
esize twosample rating_b, by(condition) all
restore

ttest better_b== worse_b, unpaired
esize twosample rating_b, by(condition) all
