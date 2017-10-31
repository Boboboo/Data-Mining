libname sasdata "C:\Users\pc\Desktop\CS593\SAS_data";

/*Problem #3*/
proc copy in=sasdata out=work;
select lung;
run;

/*univariate variables*/
proc univariate data=lung normal normaltest plot;
var AGE_Oldest_Child  WEIGHT_Oldest_Child  HEIGHT_Mother  WEIGHT_Mother  HEIGHT_Father WEIGHT_Father;
run; 
 

title "Multiple regression HEIGHT_Oldest_Child vs other 6 variables in forward selection";
proc reg data=lung;
   model HEIGHT_Oldest_Child=AGE_Oldest_Child  WEIGHT_Oldest_Child  HEIGHT_Mother  WEIGHT_Mother  HEIGHT_Father WEIGHT_Father
                    /selection=forward;
    OUTPUT OUT=reg_nutritionOUT PREDICTED=c_predict
    RESIDUAL=c_Res L95M=c_l95m U95M=C_u95m L95=C_l95 U95=C_u95
    rstudent=C_rstudent h=lev cookd=Cookd dffits=dffit;
quit;


title "Multiple regression HEIGHT_Oldest_Child vs other 6 variables in backward elimination";
proc reg data=lung;
    model HEIGHT_Oldest_Child=AGE_Oldest_Child  WEIGHT_Oldest_Child  HEIGHT_Mother  WEIGHT_Mother  HEIGHT_Father WEIGHT_Father
                    /selection=backward;
    OUTPUT OUT=reg_nutritionOUT PREDICTED=c_predict
    RESIDUAL=c_Res L95M=c_l95m U95M=C_u95m L95=C_l95 U95=C_u95
    rstudent=C_rstudent h=lev cookd=Cookd dffits=dffit;
quit;


title "Multiple regression HEIGHT_Oldest_Child vs other 6 variables in stepwise";
proc reg data=lung;
    model HEIGHT_Oldest_Child=AGE_Oldest_Child  WEIGHT_Oldest_Child  HEIGHT_Mother  WEIGHT_Mother  HEIGHT_Father WEIGHT_Father
                    /selection=stepwise;
    OUTPUT OUT=reg_nutritionOUT PREDICTED=c_predict
    RESIDUAL=c_Res L95M=c_l95m U95M=C_u95m L95=C_l95 U95=C_u95
    rstudent=C_rstudent h=lev cookd=Cookd dffits=dffit;
quit;

/*question II*/
title "Multiple regression HEIGHT_Oldest_Child vs other 6 variables using maxr to build the best model";
proc reg data=lung;
    model HEIGHT_Oldest_Child=AGE_Oldest_Child  WEIGHT_Oldest_Child  HEIGHT_Mother  WEIGHT_Mother  HEIGHT_Father WEIGHT_Father
                    /selection=maxr;
    OUTPUT OUT=reg_nutritionOUT PREDICTED=c_predict
    RESIDUAL=c_Res L95M=c_l95m U95M=C_u95m L95=C_l95 U95=C_u95
    rstudent=C_rstudent h=lev cookd=Cookd dffits=dffit;
quit;



/*Problem #4*/
proc copy in=sasdata out=work;
select heart_attack;
run;

/*logistic gression for heart_attack_2*/
proc logistic data=heart_attack descending;  
class anger_treatment(ref='0') / param=ref;
model heart_attack_2 = anger_treatment anxiety_treatment;
quit;

/*omitting anxiety_treatent from the model and run the logistic regression again with the remaining variable.*/
proc logistic data=heart_attack descending;  
class anger_treatment(ref='0') / param=ref;
model heart_attack_2 = anxiety_treatment;
quit;



/*Problem #5*/
proc copy in=sasdata out=work;
select breast_cancer_data;
run;

/*satndardize variables*/
PROC STANDARD DATA=breast_cancer_data MEAN=0 STD=1 
                OUT=cancer_z;
  VAR radius_mean texture_mean perimeter_mean area_mean smoothness_mean compactness_mean concavity_mean concave_points_mean symmetry_mean
fractal_dimension_mean;
RUN;

/*principal compoment procedure*/
proc princomp data=cancer_z out=cancer_pc;
var  radius_mean texture_mean perimeter_mean area_mean smoothness_mean compactness_mean concavity_mean concave_points_mean symmetry_mean
fractal_dimension_mean;
run;

/*test correlations between variables*/
proc corr data=cancer_pc cov;
var prin1-prin10;
run;



/*Problem #6*/

/*because node F is a dead node,so I change it so that this page can randomly move to other pages*/
data Arcs;
    infile datalines;
    input Node $ A B C D E F;
    datalines;

A   0   0   1   1   0   1
B   1   0   1   0   0   1
C   1   0   0   1   1   1
D   1   1   1   0   1   1
E   1   1   0   0   0   1
F   0   1   0   0   1   1
;
run;

/*get the transition matrix*/
proc sql;
    create table matrix_1 as
        select a/sum(a) as x1
              ,b/sum(b) as x2
              ,c/sum(c) as x3
              ,d/sum(d) as x4
              ,e/sum(e) as x5
              ,f/sum(f) as x6     
        from Arcs
    ;
quit;

/*Since there are 6 nodes, the initial vector v0 has 6 components, each 1/6*/
data rank_p;
    x1=1/6; 
    x2=1/6;
    x3=1/6;
    x4=1/6;
    x5=1/6;
    x6=1/6;
    output;
run;

/* After 50 times iteration, the vector we get shows little change at each round*/
proc iml;
    use matrix_1;
    read all var { x1 x2 x3 x4 x5 x6 } into M;
    print M;

    use rank_p;
    read all var { x1 x2 x3 x4 x5 x6 } into rank_p1;
    rank_p = t(rank_p1);
    print rank_p ;

    rank_p50=(M**50)*rank_p;
    print rank_p50 ;
quit;
