**First 78 lines only apply to cleaning csv file. Can start on line 79 if data cleaned.
*Apply value labels to make analysis easier

label define good_first 1 "good first" 0 "bad first"
label define alex_first 1 "alex_first" 0 "mark_first"
label define first_1500 1 "first_750" 0 "second_750"
label values first_1500 first_1500
label values good_first good_first
label values alex_first alex_first


*recode Hexaco to analyze. I'm creating copies to determine the scores on each item
//but keeping the originals so I can weed out those who selected multiple answers

gen c_q24_1_2 = q24_1_2 
recode c_q24_1_2 1=2
gen c_q24_1_3 = q24_1_3 
recode c_q24_1_3 1=3
gen c_q24_1_4 = q24_1_4
recode c_q24_1_4 1=4
gen c_q24_1_5 = q24_1_5
recode c_q24_1_5 1=5

gen c_q24_2_2 = q24_2_2 
recode c_q24_2_2 1=2
gen c_q24_2_3 = q24_2_3 
recode c_q24_2_3 1=3
gen c_q24_2_4 = q24_2_4
recode c_q24_2_4 1=4
gen c_q24_2_5 = q24_2_5
recode c_q24_2_5 1=5

gen c_q24_3_2 = q24_3_2 
recode c_q24_3_2 1=2
gen c_q24_3_3 = q24_3_3 
recode c_q24_3_3 1=3
gen c_q24_3_4 = q24_3_4
recode c_q24_3_4 1=4
gen c_q24_3_5 = q24_3_5
recode c_q24_3_5 1=5

gen c_q24_4_2 = q24_4_2 
recode c_q24_4_2 1=2
gen c_q24_4_3 = q24_4_3 
recode c_q24_4_3 1=3
gen c_q24_4_4 = q24_4_4
recode c_q24_4_4 1=4
gen c_q24_4_5 = q24_4_5
recode c_q24_4_5 1=5

egen hex1_check = rowtotal( q24_1_1 q24_1_2 q24_1_3 q24_1_4 q24_1_5)
egen hex2_check = rowtotal( q24_2_1 q24_2_2 q24_2_3 q24_2_4 q24_2_5)
egen hex3_check = rowtotal( q24_3_1 q24_3_2 q24_3_3 q24_3_4 q24_3_5)
egen hex_ac_check = rowtotal( q24_4_1 q24_4_2 q24_4_3 q24_4_4 q24_4_5)

egen hex1 = rowtotal( q24_1_1 c_q24_1_2 c_q24_1_3 c_q24_1_4 c_q24_1_5)
egen hex2 = rowtotal( q24_2_1 c_q24_2_2 c_q24_2_3 c_q24_2_4 c_q24_2_5)
egen hex3 = rowtotal( q24_3_1 c_q24_3_2 c_q24_3_3 c_q24_3_4 c_q24_3_5)
egen hex_ac = rowtotal( q24_4_1 c_q24_4_2 c_q24_4_3 c_q24_4_4 q24_4_5)

*note, there was an error where I allowed people to select more than 1 response 
//for hex so some people have scores greater than 1. I'll drop these from the ANCOVA

*cynicism attention check

egen cyn_ac_check = rowtotal(q25_7_1 q25_7_2 q25_7_3 q25_7_4 q25_7_5)

gen c_q25_7_2 = q25_7_2 
recode c_q25_7_2 1=2
gen c_q25_7_3 = q25_7_3 
recode c_q25_7_3 1=3
gen c_q25_7_4 = q25_7_4
recode c_q25_7_4 1=4
gen c_q25_7_5 = q25_7_5
recode c_q25_7_5 1=5

egen cyn_ac = rowtotal (q25_7_1 c_q25_7_2 c_q25_7_3 c_q25_7_4 c_q25_7_5)

**Analyze 2nd 750 first. If working with cleaned data, can start here
//(note I misnamed this column. The indicator variable is 
//called first_1500, but I meant 50

**preserve seems not to work in do.file, might need to manually do in command line
preserve

drop if first_1500 == 1

*drop incompletes
drop if v10==0
*drop non-consent (their data is blank but dropping to make things cleaner)
drop if q27==2

*apply attention checks
keep if cyn_ac_check==1 & hex_ac_check==1
keep if hex_ac==1&cyn_ac==5

*main ANOVA analysis

anova responsibility good_first
margins good_first

esize twosample responsibility, by(good_first) all

*drop accidental hex_scores >5 // fortunately this only drops 53 participants in 
//the 2nd 750
drop if hex1_check > 1 | hex2_check>1 | hex3_check>1 

alpha hex1-hex3, item

gen hex_total = (hex1 + hex2 + hex3)/3

anova responsibility good_first c.hex_total

restore

**Analyze 1st 750 second

preserve

drop if first_1500 == 0

*drop incompletes
drop if v10==0
*drop non-consent (their data is blank but dropping to make things cleaner)
drop if q27==2

*apply attention checks
keep if cyn_ac_check==1 & hex_ac_check==1
keep if hex_ac==1&cyn_ac==5

*main ANOVA analysis

anova responsibility good_first
margins good_first

esize twosample responsibility, by(good_first) all

*drop accidental hex_scores >5 // fortunately this only drops 50 participants in 
//the 1st 750
drop if hex1_check > 1 | hex2_check>1 | hex3_check>1 

alpha hex1-hex3, item

gen hex_total = (hex1 + hex2 + hex3)/3

anova responsibility good_first c.hex_total

restore


**analyze total data file

preserve

*drop incompletes
drop if v10==0
*drop non-consent (their data is blank but dropping to make things cleaner)
drop if q27==2

*apply attention checks
keep if cyn_ac_check==1 & hex_ac_check==1
keep if hex_ac==1&cyn_ac==5

*main ANOVA analysis

anova responsibility good_first
margins good_first

esize twosample responsibility, by(good_first) all

*drop accidental hex_scores >5 // fortunately this only drops 103 participants in 
//total
drop if hex1_check > 1 | hex2_check>1 | hex3_check>1 

alpha hex1-hex3, item

gen hex_total = (hex1 + hex2 + hex3)/3

anova responsibility good_first c.hex_total

restore
