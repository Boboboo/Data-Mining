-------------------------------------------------------------------------;
* Project        :  CS593                            					 ;
* Developer(s)   : Kathy Chowaniec, Li Zhang, Bo Zhang	                  ;
* Comments       : Final Project: Affordability Prediction                 ;
*                                             							  ;
* Dependencies   : libnames.sas                                           ;
*-------------------------------------------------------------------------;

libname sasdata "C:\CS 593\SAS_Data";
run;

proc copy in=sasdata out=work;
select hads;
run;

*remove unreasonable values (age less than 18 and value less than 1000);
data housing value_exptn;
set hads;
if  AGE1 < 18  then output value_exptn ;
else output housing;
run;

*transform data;
data housing2 exptn;
set housing;
lnarincome=log(LMED);
lnFMR=log(FMR);
sqhouseincome=sqrt(ZINC2);
sqmonthlycost=sqrt(ZSMHC);
squtility=sqrt(UTILITY);
lnother=log(OTHERCOST);
lnvalue=log(VALUE);
lnunits=log(NUNITS);
lnpeople=log(PER);
run;

*Create indicator variables;
data housing3 exptn;
set housing2;
if OWNRENT = 1 then v_owner = 1;
else v_owner = 0;
if FMTREGION = "Northeast" then v_northeast=1;
else v_northeast=0;
if FMTREGION = "South" then v_south=1;
else v_south=0;
if FMTREGION = "West" then v_west=1;
else v_west=0;
if STATUS = 3 then v_occupied=0;
else v_occupied=1;
if FMTBURDEN = "1 Less than 30%" then v_affordable=1;
else v_affordable=0;
if METRO3='1' then v_city=1;
else v_city=0;
if FMTBUILT = 'After 2010' or FMTBUILT = '2000-2009'  then v_y2000=1;
else v_y2000=0;
if FMTBUILT = ' Pre 1940'  then v_y1940=1;
else v_y1940=0;
if STRUCTURETYPE = 1 then v_singlefam=1;
else v_singlefam=0;
run;

*MODEL 3;
*logistic regression;
title "logistic regreesion data for affordability :stepwise selection";
proc logistic data=housing3 descending;
class v_city(ref='0')/param=ref;
class v_south(ref='0')/param=ref;
class v_northeast(ref='0')/param=ref;
class v_owner(ref='0')/param=ref;
class v_west(ref='0')/param=ref;
class v_singlefam(ref='0')/param=ref;
model v_affordable= v_owner v_y2000 v_y1940 v_northeast v_west sqhouseincome sqmonthlycost v_singlefam
v_south v_city squtility lnother lnFMR lnarincome AGE1  lnunits ROOMS PER / selection=stepwise;
quit;

*MODEL 3;
*logistic regression;
title "logistic regression data for affordability";
proc logistic data=housing3 descending;
class v_city(ref='0')/param=ref;
class v_south(ref='0')/param=ref;
class v_west(ref='0')/param=ref;
class v_northeast(ref='0')/param=ref;
class v_owner(ref='0')/param=ref;
class v_singlefam(ref='0')/param=ref;
model v_affordable= v_owner v_y2000 v_y1940 v_northeast v_west sqhouseincome sqmonthlycost v_singlefam
v_south v_city squtility lnother lnFMR lnarincome AGE1  lnunits ROOMS PER  ;
quit;

*MODEL 3;
*logistic regression;
title "logistic regression data for affordability ";
proc logistic data=housing3 descending;
model v_affordable= sqhouseincome sqmonthlycost;
quit;


**EVALUATION;
  *training and test data set;
data training test;
set housing3;
*determine if record number is even (divisible by 2);
id=1000+_n_;
if mod(id,2)=0 then output training;
else output test;
run;

*logistic regression of odd records;
title "logistic regression for affordability (training)";
proc logistic data=training descending outmodel=training_model;
model v_affordable=sqhouseincome sqmonthlycost;
quit;

*logistic regression of even records;
title "logistic regression for affordability (test)";
proc logistic data=test descending outmodel=test_model;
model v_affordable=sqhouseincome sqmonthlycost;
quit;

proc freq data=test;
table v_affordable/out=prior_dist2(rename=count=_prior_ drop=percent);
run;


*use training to score test;
proc logistic inmodel=training_model;
score data=test prior=prior_dist2
out=test_score fitstat;
run;






