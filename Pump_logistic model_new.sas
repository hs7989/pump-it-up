libname sas_data "C:/Users/jenil/Desktop/SAS project" access=read;
run;
 

proc import datafile="C:/Users/jenil/Desktop/SAS project/categorical_numeric_data.csv" 
out=pump_data
dbms=CSV  
replace;
getnames=yes;
run;

data pump_data ;
set pump_data;
/* Replacing with 0 and 1 */;
if public_meeting = "TRUE" then public_meeting1=1;
 else public_meeting1=0;
/*if funder < 11 then funder_0 =1;
else funder_0=0;
if  funder > 10 then funder_1 =1;
else funder_1=0;
*/
 if permit = "TRUE" then permit1=1;
 else permit1=0;
run;



proc princomp data=pump_data;
var funder installer basin region lga public_meeting1 scheme_management permit1 extraction_type extraction_type_class management management_group payment_type quality_group quantity source source_class waterpoint_type_group;
run;


proc princomp data=pump_data out=PCA_pump n=8;
var funder installer basin region lga public_meeting1 scheme_management permit1 extraction_type extraction_type_class management management_group payment_type quality_group quantity source source_class waterpoint_type_group;
run;

ods graphics on;
proc logistic data=PCA_pump descending plots(only)=(roc(id=obs)effect); 
model status_group = prin1 prin2 prin3 prin4 prin5 prin6 prin7 prin8/ scale = none clparm=wald clodds=pl rsquare CTABLE ; 
title1 'Predicting Functionality of Water pump '; 
run;
ods graphics off;
