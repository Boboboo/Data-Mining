-------------------------------------------------------------------------;
* Project        :  CS593                            					 ;
* Developer(s)   : Kathy Chowaniec, Li Zhang, Bo Zhang	                  ;
* Comments       : Final Project: Market Value Prediction                 ;
*                                             							  ;
* Dependencies   : libnames.sas                                           ;
*-------------------------------------------------------------------------;

libname sasdata "C:\CS 593\SAS_Data";
run;

proc copy in=sasdata out=work;
select hads;
run;

*remove unreasonable values (age less than 18 and value less than 1000);
data housing value_exptn ;
set hads;
if VALUE <= 1000 or AGE1 < 18 then output value_exptn ;
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

  title "market value : univariate distribution";
*show distribution of transformed variables;
proc univariate data=housing3 normaltest plot;	
	var lnvalue lnother squtility sqmonthlycost sqhouseincome lnFMR lnarincome AGE1 ;
run;

  ods graphics on;

*MODEL 2;
*predict market value;
    title "market value : regression without any variables removed";
proc reg data=housing3 outest=out_housing ;
model lnvalue= v_occupied v_owner v_y1940 v_y2000 v_northeast v_south  v_city squtility lnother
sqmonthlycost lnFMR lnarincome sqhouseincome ROOMS AGE1 lnunits PER / VIF;
      OUTPUT OUT=reg_housingOUT  PREDICTED=   RESIDUAL=Res   L95M=C_l95m  U95M=C_u95m  L95=C_l95 U95=C_u95
       rstudent=C_rstudent h=lev cookd=Cookd  dffits=dffit
         ;  
run;
quit;

**FORWARD SELECTION;
  title "market value : forward selection";
  proc reg data=housing3  outest=est_housing;
     model lnvalue= v_occupied v_owner v_y1940 v_y2000 v_northeast v_south  v_city squtility lnother
sqmonthlycost lnFMR lnarincome sqhouseincome ROOMS AGE1 lnunits PER 
                        /  selection=forward SLENTRY=0.05;
      OUTPUT OUT=reg_housingOUT  PREDICTED=   RESIDUAL=Res   L95M=C_l95m  U95M=C_u95m  L95=C_l95 U95=C_u95
       rstudent=C_rstudent h=lev cookd=Cookd  dffits=dffit
         ;  
  quit;

  
**BACKWARD SELECTION;
      title "market value : backward selection";
  proc reg data=housing3  outest=est_housing;
     model lnvalue= v_occupied v_owner v_y1940 v_y2000 v_northeast v_south  v_city squtility lnother
sqmonthlycost lnFMR lnarincome sqhouseincome ROOMS AGE1 lnunits PER 
                        /  selection=backward ;
      OUTPUT OUT=reg_housingOUT  PREDICTED=   RESIDUAL=Res   L95M=C_l95m  U95M=C_u95m  L95=C_l95 U95=C_u95
       rstudent=C_rstudent h=lev cookd=Cookd  dffits=dffit
         ;  
  quit;

  
**STEPWISE SELECTION;
      title "market value : stepwise selection";
  proc reg data=housing3  outest=est_housing;
     model  lnvalue= v_occupied v_owner v_y1940 v_y2000 v_northeast v_south  v_city squtility lnother
sqmonthlycost lnFMR lnarincome sqhouseincome ROOMS AGE1 lnunits PER 
                        /  selection=stepwise SLENTRY=0.05;
      OUTPUT OUT=reg_housingOUT  PREDICTED=   RESIDUAL=Res   L95M=C_l95m  U95M=C_u95m  L95=C_l95 U95=C_u95
       rstudent=C_rstudent h=lev cookd=Cookd  dffits=dffit
         ;  
  quit;

  
**MAXR SELECTION;
          title "market value: MAXR selection";
  proc reg data=housing3  outest=est_housing;
     model  lnvalue= v_occupied v_owner v_y1940 v_y2000 v_northeast v_south  v_city squtility lnother
sqmonthlycost lnFMR lnarincome sqhouseincome ROOMS AGE1 lnunits PER 
                        /  selection=MAXR;
      OUTPUT OUT=reg_housingOUT  PREDICTED=   RESIDUAL=Res   L95M=C_l95m  U95M=C_u95m  L95=C_l95 U95=C_u95
       rstudent=C_rstudent h=lev cookd=Cookd  dffits=dffit
         ;  
  quit;

            title "market value: rsquare selection";
  proc reg data=housing3  outest=est_housing;
     model lnvalue= v_occupied v_owner v_y1940 v_y2000 v_northeast v_south  v_city squtility lnother
sqmonthlycost lnFMR lnarincome sqhouseincome ROOMS AGE1 lnunits PER 
                        /  selection=rsquare;
      OUTPUT OUT=reg_housingOUT  PREDICTED=   RESIDUAL=Res   L95M=C_l95m  U95M=C_u95m  L95=C_l95 U95=C_u95
       rstudent=C_rstudent h=lev cookd=Cookd  dffits=dffit
         ;  
  quit;

     title "market value: regression with 12-variable model";
     *regression with 12 variables recommended;
  proc reg data=housing3  outest=est_housing  ;
     model  lnvalue= v_y2000 v_northeast v_south  v_city lnother
sqmonthlycost lnFMR sqhouseincome ROOMS AGE1 lnunits PER / VIF;
      OUTPUT OUT=reg_housingOUT  PREDICTED=   RESIDUAL=Res   L95M=C_l95m  U95M=C_u95m  L95=C_l95 U95=C_u95
       rstudent=C_rstudent h=lev cookd=Cookd  dffits=dffit
         ;  
  quit;
   
     title "market value: regression with 12-variable model and residual plots";
       *regression with 12 variables recommended (with residual plots);
  proc reg data=housing3  outest=est_housing PLOTS(MAXPOINTS=NONE);
     model  lnvalue= v_y2000 v_northeast v_south  v_city lnother
sqmonthlycost lnFMR sqhouseincome ROOMS AGE1 lnunits PER / VIF;
      OUTPUT OUT=reg_housingOUT  PREDICTED=   RESIDUAL=Res   L95M=C_l95m  U95M=C_u95m  L95=C_l95 U95=C_u95
       rstudent=C_rstudent h=lev cookd=Cookd  dffits=dffit
         ;  
  quit;


      *regression with 6-variable model from MAXR (no residual plots);
     title "market value using MAXR for 6-variable model";
  proc reg data=housing3  outest=est_housing;
     model lnvalue=lnother sqmonthlycost lnFMR sqhouseincome AGE1 ROOMS/VIF;
      OUTPUT OUT=reg_housingOUT  PREDICTED=   RESIDUAL=Res   L95M=C_l95m  U95M=C_u95m  L95=C_l95 U95=C_u95
       rstudent=C_rstudent h=lev cookd=Cookd  dffits=dffit
         ;  
  quit;

         *regression with 6-variable model from MAXR (with residual plots); 
     title "market value using MAXR for 6-variable model";
  proc reg data=housing3  outest=est_housing PLOTS(MAXPOINTS=NONE);
     model lnvalue=lnother sqmonthlycost lnFMR sqhouseincome AGE1 ROOMS/VIF;
      OUTPUT OUT=reg_housingOUT  PREDICTED=   RESIDUAL=Res   L95M=C_l95m  U95M=C_u95m  L95=C_l95 U95=C_u95
       rstudent=C_rstudent h=lev cookd=Cookd  dffits=dffit
         ;  
  quit;

  **Model using PCA;
*** Normalize the data ***;
PROC STANDARD DATA=housing3 MEAN=0 STD=1 
             OUT=housing_z;
  VAR  v_y2000 v_northeast v_south  v_city lnother
sqmonthlycost lnFMR sqhouseincome ROOMS AGE1 lnunits PER ;
RUN;

title "PCA : Correlation between components";
*principal component analysis;
proc princomp data=housing_z out=pca_housing;
var   v_y2000 v_northeast v_south  v_city lnother
sqmonthlycost lnFMR sqhouseincome ROOMS AGE1 lnunits PER ;
run;

title "PCA : market value regression model";
*regression using principal components;
  proc reg data=pca_housing outest=out_housing;
model lnvalue=prin1 prin2 prin3 prin4 prin5 prin6 prin7 prin8 prin9/ STB VIF dwprob;
      OUTPUT OUT=reg_housingOUT  PREDICTED=   RESIDUAL=Res   L95M=C_l95m  U95M=C_u95m  L95=C_l95 U95=C_u95
       rstudent=C_rstudent h=lev cookd=Cookd  dffits=dffit 
         ; 
run;
quit;

*regression using principal components (with residual plots);
  proc reg data=pca_housing outest=out_housing PLOTS(MAXPOINTS=NONE);
model lnvalue=prin1 prin2 prin3 prin4 prin5 prin6 prin7 prin8 prin9/ STB VIF dwprob;
      OUTPUT OUT=reg_housingOUT  PREDICTED=   RESIDUAL=Res   L95M=C_l95m  U95M=C_u95m  L95=C_l95 U95=C_u95
       rstudent=C_rstudent h=lev cookd=Cookd  dffits=dffit 
         ; 
run;
quit;
  ods graphics off;

**EVALUATION;
  *training and test data set;
data training test;
set housing3;
*determine if record number is even (divisible by 2);
id=1000+_n_;
if mod(id,2)=0 then output training;
else output test;
run;

*regression of odd records;
proc reg data=training outest=training_model;
market_value: model lnvalue=lnother sqmonthlycost lnFMR sqhouseincome AGE1 ROOMS ;
quit;

*regression of even records;
proc reg data=test outest=test_model;
market_value: model lnvalue=lnother sqmonthlycost lnFMR sqhouseincome AGE1 ROOMS ;
quit;

*score the data;
title "market value test dataset";
proc freq data=test;
table lnvalue/out=prior_dist2(rename=count=_prior_ drop=percent);
run;

proc score data=test score=training_model type=parms predict 
out=test_score ;
var lnvalue lnother sqmonthlycost lnFMR sqhouseincome AGE1 ROOMS;
run;
*RMSE and MAE for lnvalue (actual) and market_value (predicted) in dataset test_score;
%macro mae_rmse(
        dataset /* data set which contains the actual and predicted values */, 
        actual /* variable which contains the actual or observed valued */, 
        predicted /* variable which contains the predicted value */
        );
%global mae rmse; /* make the scope of the macro variables global */
proc sql noprint;
    select count(1) into :count from &dataset;
    select mean(abs(&actual-&predicted)) format 5.10 into :mae from &dataset;
    select sqrt(mean((&actual-&predicted)**2)) format 5.10 into :rmse from &dataset;
quit;
%mend;
%mae_rmse(test_score, lnvalue, market_value);
%put NOTE: Evaluation for market price: MAE=&MAE ;         
%put NOTE: Evaluation for market price: RMSE=&RMSE;



