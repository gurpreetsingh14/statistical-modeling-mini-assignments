*DATA IMPORT;
*student data;
filename REFFILE '/home/u59394413/sasuser.v94/Personal Folder/Data/studentsdata.csv';
*rain data;
filename REFFILE1 '/home/u59394413/sasuser.v94/Personal Folder/Data/weatherAUS_clean.csv';

proc import datafile=REFFILE
	out=GSDATA.studentsdata
    dbms=csv;
	getnames=yes;
run;

proc import datafile=REFFILE1
	out=GSDATA.raindata
    dbms=csv;
	getnames=yes;
run;

*****************************************************************************************
*PRE-PROCESSING BEGINS;
* rename columns of studentdata;
data GSDATA.studentsdata
	(rename = (	"race.ethnicity"N = raceEthnicity
				"test.preparation.course"N = prepCourse
				"parental.level.of.education"N = prntEduLvl
				"math.score"N = mathScore
				"reading.score"N = readingScore
				"writing.score"N = writingScore));
	set GSDATA.studentsdata;
run;

* calculating totalScore;
data GSDATA.studentsdata;
	set GSDATA.studentsdata;
	totalScore = round(((math.score + writing.score + reading.score)/3),1);
run;

*add "passed" column;
data GSDATA.studentsdata;
	set GSDATA.studentsdata;
	passed = (mathScore > 60);
run;

*For raindata;
data GSDATA.raindata;
	set GSDATA.raindata;
	if RainTomorrow = "Yes"
		then RainTomorrow = 1; 
	else RainTomorrow = 0; 
run; 

*PRE-PROCESSING ENDS;
*****************************************************************************************
*MODELLING BEGINS;
*Q1 a);
*fitting linear regression model;
proc glm data=GSDATA.studentsdata;
	class gender prepCourse prntEduLvl raceEthnicity;
	model totalScore=gender prepCourse prntEduLvl  / ss3 solution clparm ;
	output out=residuals predicted=pred rstudent=dsresid;
run;

*Q1 b);
*residual analysis;
proc sgplot data=residuals;
	histogram dsresid;
	density dsresid;
	density dsresid / type=kernel;
	keylegend / location=inside position=topright;
run;

proc univariate data=residuals noprint;
	qqplot dsresid / normal(mu=est sigma=est color=red l=2) square;
run;

proc loess data=residuals plots(only)=(fit);
	model dsresid=pred / clm;
run;

*Q1 c);
* fitting linear regression model where HIGH SCHHOL is the parental level of education  ;
proc glm data=GSDATA.studentsdata;
	class gender raceEthnicity prntEduLvl(ref='high school') lunch prepCourse; 
	model totalScore=gender raceEthnicity prntEduLvl lunch prepCourse  / ss3 solution clparm; 
	output out=residuals predicted=pred rstudent=dsresid; 
run; 

* fitting linear regression model where ASSOCIATE degree is the parental level of education;
proc glm data=GSDATA.studentsdata; 
	class gender raceEthnicity prntEduLvl(ref="associate's degree") lunch prepCourse; 
	model totalScore=gender raceEthnicity prntEduLvl lunch prepCourse  / ss3 solution clparm;
	output out=residuals predicted=pred rstudent=dsresid; 
run;  

* fitting linear regression model where BACHELOR degree is the parental level of education; 
proc glm data=GSDATA.studentsdata; 
	class gender raceEthnicity prntEduLvl(ref="bachelor's degree") lunch prepCourse; 
	model totalScore=gender raceEthnicity prntEduLvl lunch prepCourse  / ss3 solution clparm ; 
	output out=residuals predicted=pred rstudent=dsresid; 
run; 

* fitting linear regression model where MASTER degree is the parental level of education;
proc glm data=GSDATA.studentsdata; 
	class gender raceEthnicity prntEduLvl(ref="master's degree") lunch prepCourse; 
	model totalScore=gender raceEthnicity prntEduLvl lunch prepCourse  / ss3 solution clparm; 
	output out=residuals predicted=pred rstudent=dsresid; 
run; 

*Q1 d);
proc glm data=GSDATA.studentsdata;
	class lunch;
	model totalScore=lunch  / ss3 solution clparm ;
run;

proc ttest data=GSDATA.studentsdata;
	class lunch;
	var totalScore;
run;

*****************************************************************************************
*Q2;
*2.a) logistic regression with prepCourse;
proc glimmix data=GSDATA.studentsdata ;
	class prepCourse;
	model passed(ref="0")= prepCourse/ dist=binary link=logit solution ;
run;

* 2.b) logistic regression;
proc glimmix data=GSDATA.studentsdata ;
	class gender raceEthnicity prntEduLvl lunch prepCourse;
	model passed(ref="0")= gender raceEthnicity prntEduLvl lunch prepCourse / dist=binary link=logit solution;
run;

*2.c) same odds for all race/Ethnicity groups;
proc glimmix data=GSDATA.studentsdata ;
	class raceEthnicity;
	model passed = raceEthnicity /dist = binary link=logit solution;
run;

* 2.d) add interactions between prntEduLvl and prepCourse;
proc glimmix data=GSDATA.studentsdata ;
	class gender raceEthnicity prntEduLvl lunch prepCourse;
	model passed(ref="0")= gender raceEthnicity prntEduLvl lunch prepCourse prepCourse*prntEduLvl / dist=binary link=logit solution ;
run;

* 2.e) p value for the likelihood ratio test;
data pval;
	y=1-cdf('CHISQUARE',2.97,3);
	p_val = y;
run;

*****************************************************************************************
*Q3 a)[only used by us to get the SAS output presented in the homework] ;
proc mixed data=GSDATA.raindata method=reml covtest;
	class WindDir3pm Location; 
	model Rainfall =  MaxTemp Humidity3pm WindDir3pm  /solution cl; 
	repeated / subject=Location type=cs r=1 rcorr=1;  
run;  

* 3.b) Fitting Basic Linear Regression;
proc mixed data=GSDATA.raindata method=reml ;
	class WindDir3pm; 
	model Rainfall = MinTemp MaxTemp Evaporation Humidity3pm Sunshine WindDir3pm/solution cl; 
run;  

* 3.d) Structuring our days;
data temp;
	set GSDATA.raindata; 
	tcat=day; 
run; 

* Regression with an AR(1) correlation structure on the errors;
proc mixed data=temp method=reml covtest; 
	class WindDir3pm tcat Location; 
	model Rainfall = MinTemp MaxTemp Humidity3pm WindDir3pm day /solution cl; 
	repeated tcat / subject=Location type=ar(1) r=1 rcorr=1;  
run;  

* 3.e) Regression with an CS correlation structure on the errors;
proc mixed data=temp method=reml covtest; 
	class WindDir3pm tcat Location; 
	model Rainfall = MinTemp MaxTemp Humidity3pm WindDir3pm day /solution cl; 
	repeated tcat / subject=Location type=cs r=1 rcorr=1; 
run; 

*MODELLING ENDS;
****************************************************************************************
