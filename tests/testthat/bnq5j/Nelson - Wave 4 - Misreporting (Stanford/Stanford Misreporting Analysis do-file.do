
*one person provided ZIP + 4, which couldn't destring, so deleted the +4"
replace zipcode = "85747" in 106
destring zipcode, replace
tabu att_check
keep if att_check==5

gen sex=1 if gender==2
replace sex=0 if missing(gender)
replace sex=0 if missing(sex)
gen age_years = 2019-year
gen age_18_to_24 = 1 if age_years >=18 & age_years <=24
gen age_25_to_34 = 1 if age_years >=25 & age_years <=34
gen age_35_to_44 = 1 if age_years >=35 & age_years <=44
gen age_45_to_54 = 1 if age_years >=45 & age_years <=54
gen age_55_to_64 = 1 if age_years >=55 & age_years <=64
gen age_65_plus = 1 if age_years >65
gen age_miss = 1 if missing(year)

replace age_18_to_24 = 0 if missing(age_18_to_24)
replace age_25_to_34 = 0 if missing(age_25_to_34)
replace age_35_to_44 = 0 if missing(age_35_to_44)
replace age_45_to_54 = 0 if missing(age_45_to_54)
replace age_55_to_64 = 0 if missing(age_55_to_64)
replace age_65_plus = 0 if missing(age_65_plus)
replace age_miss = 0 if missing(age_miss)

recode hispanic 2=0
gen hispanic_miss = 1 if missing(hispanic)
replace hispanic_miss=0 if missing(hispanic_miss)

gen black = 1 if race_2 ==1

replace black=0 if missing(black)
gen other_race=1 if race_3==1 | race_4==1 | race_5==1 | race_6==1

gen other_race=1 if race_3==1 | race_4==1 | race_5==1 | race_6==1
replace other_race=0 if missing(other_race)

gen race_missing = 1 if missing(race_1) & missing(race_2) & missing(race_3) & missing(race_4) & missing(race_5) & missing(race_6)
replace race_missing = 0 if missing(race_missing)

gen less_high_school = 1 if educ <= 8
gen hs_grad = 1 if educ==9
gen some_college = 1 if educ==10 | educ==11
gen college_grad =1 if educ >=12
replace less_high_school = 0 if missing(less_high_school)
replace hs_grad = 0 if missing(hs_grad)
replace some_college = 0 if missing(some_college)
replace college_grad = 0 if missing(college_grad)
gen edu_miss =1 if missing(educ)

replace edu_miss=0 if missing(edu_miss)
gen income_low = 1 if income2a ==1 | income2a==2 | income2a==3
gen income_lm = 1 if income2a ==4 | income2a==5
gen income_mid = 1 if income2b  ==1 
gen income_lh = 1 if income2b ==2
gen income_hi = 1 if income2b ==3 | income2b==4
gen income_miss = 1 if missing(income2a) & missing(income2b)

replace income_low = 0 if missing(income_low)
replace income_lm = 0 if missing(income_lm)
replace income_mid = 0 if missing(income_mid)
replace income_lh = 0 if missing(income_lh)
replace income_hi = 0 if missing(income_hi)
replace income_miss = 0 if missing(income_miss)

gen northeast=1 if region4==0
gen midwest=1 if region4==1
gen south=1 if region4==2
gen west=1 if region4==3
gen region_miss =1 if missing(region4)

replace northeast = 0 if missing(northeast)
replace midwest = 0 if missing(midwest)
replace south = 0 if missing(south)
replace west = 0 if missing(west)
replace region_miss = 0 if missing(region_miss)
