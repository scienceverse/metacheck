* Encoding: UTF-8.

DATASET ACTIVATE DataSet1.
RECODE B5 B6 B7 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1).
EXECUTE.

COMPUTE Favorite_Comp=(B1 + B2 + B3 + B4 + B5 + B6 + B7) / 7.
EXECUTE.

USE ALL.
COMPUTE filter_$=(Half = 1).
VARIABLE LABELS filter_$ 'Half = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

 CROSSTABS
  /TABLES=Final_Group BY Change
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ 
  /CELLS=COUNT
  /COUNT ROUND CELL.

FREQUENCIES VARIABLES=Pass
  /ORDER=ANALYSIS.

USE ALL.
COMPUTE filter_$=(Half = 1 & Pass = 1).
VARIABLE LABELS filter_$ 'Half = 1 & Pass = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

UNIANOVA Favorite_Comp BY Final_Group Change
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=Final_Group Change Final_Group*Change.

T-TEST GROUPS=Change(0 1)
  /MISSING=ANALYSIS
  /VARIABLES=Favorite_Comp
  /CRITERIA=CI(.95).



USE ALL.
COMPUTE filter_$=(Half = 2).
VARIABLE LABELS filter_$ 'Half = 2 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

CROSSTABS
  /TABLES=Final_Group BY Change
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ 
  /CELLS=COUNT
  /COUNT ROUND CELL.

FREQUENCIES VARIABLES=Pass
  /ORDER=ANALYSIS.

USE ALL.
COMPUTE filter_$=(Half = 2 & Pass = 1).
VARIABLE LABELS filter_$ 'Half = 2 & Pass = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

UNIANOVA Favorite_Comp BY Final_Group Change
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=Final_Group Change Final_Group*Change.

T-TEST GROUPS=Change(0 1)
  /MISSING=ANALYSIS
  /VARIABLES=Favorite_Comp
  /CRITERIA=CI(.95).

FILTER OFF.
USE ALL.
EXECUTE.

FREQUENCIES VARIABLES=Pass
  /ORDER=ANALYSIS.

FILTER OFF.
USE ALL.
EXECUTE.

CROSSTABS
  /TABLES=Final_Group BY Change
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ 
  /CELLS=COUNT
  /COUNT ROUND CELL.

FREQUENCIES VARIABLES=Pass
  /ORDER=ANALYSIS.
  
USE ALL.
COMPUTE filter_$=(Pass = 1).
VARIABLE LABELS filter_$ 'Pass = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

UNIANOVA Favorite_Comp BY Final_Group Change
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=Final_Group Change Final_Group*Change.

T-TEST GROUPS=Change(0 1)
  /MISSING=ANALYSIS
  /VARIABLES=Favorite_Comp
  /CRITERIA=CI(.95).
