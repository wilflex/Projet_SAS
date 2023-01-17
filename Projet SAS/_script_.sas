

								 /*--------------------------------*/
				   				/*   Ressource bibliographique    */
				  			   /*--------------------------------*/

/** source base de données url = "http://www.creditriskanalytics.net/datasets-private2.html" **/
/** source methode imputation url = "http://wp.clubsasquebec.ca/wp-content/uploads/2016/10/NMN_CUSQ112016_MIANALYZE.pdf" **/
/** source learn proc univariate url "https://home.csulb.edu/~skim43/SAS/2008_Handout7.pdf" ***/
/** source proc univariate exmeple statment url "https://www.lexjansen.com/nesug/nesug07/np/np12.pdf" ***/
/** source comprendre le test de KHI-2 url "https://libguides.library.kent.edu/SAS/ChiSquare" **/
/** source to understands how does array worked url "https://www.listendata.com/2016/01/sas-arrays-and-do-loop-made-easy.html" **/
/** source understand proc logistic url "https://stats.oarc.ucla.edu/unlinked/sas-logistic/proc-logistic-and-logistic-regression-models/" **/
/** source comprendre la proc logistique url "https://www.math.wpi.edu/saspdf/stat/chap39.pdf" **/
/** source comprendre le LVT url "https://www.investopedia.com/terms/l/loantovalue.asp" **/



								 /*--------------------------*/
				   				/*    PROJET SCORING SAS    */
				  			   /*--------------------------*/

%let lib = PROJET;
%let path = "C:\Users\marsh\Desktop\Projet SAS\";


								/** Creation de la librairie **/

libname PROJET "C:\Users\marsh\Desktop\Projet SAS\";
run;
  
 								/** Chargement des données -option 1- **/						   
PROC IMPORT datafile='C:\Users\marsh\Desktop\Projet SAS\hmeq.csv'
	dbms=csv 
	out=&lib..credit;		
RUN;

							 /*****************************/
							/** Dictionnaire de données **/
						   /*****************************/


%macro description(var);

/**CHARGEMENT DE LA BASE**/

PROC IMPORT datafile='C:\Users\marsh\Desktop\Projet SAS\VAR_INTITULE.xlsx'
	dbms=xlsx 
	out=&lib..VAR_INTITULE;
RUN;

/**FILTRAGE DE LA BASE**/
proc sql;
create table info as 
select Var_Name,Var_Intitule
from work.VAR_INTITULE;
quit;
/**REQUETE QUI FOURNIT LA DESCRIPTIN DES VARAIBLES**/
proc sql;
select Var_Intitule as &var.
from info
where Var_Name = "&var.";
quit;
%mend;

%description(LOAN);

								/** Affichage des données **/

ods graphics / width=640px height=480px;  
proc print data = &lib..credit (obs=10);  
title 'Lecture de la base de données'; 
run;

								/** Description des données **/

proc contents data=&lib..credit; 
title 'Description des caracteristique des variables'; 
run;

								
								 /**********************************/
								/** Analyse de la variable cible **/
							   /**********************************/


/** __Analyse de la variable cible__ **/
PROC FREQ data = &lib..credit
	order=freq ;
	table BAD ;
	RUN ;

/** __Distribution de la variable cible__ **/
proc sgplot data = &lib..credit;
	VBAR BAD;
	RUN;


								 /************************************************/
								/****** Traitement des données manquantes *******/
							   /************************************************/


	/** -- identification des données manquantes -- **/

%macro get_char_type_missing(base,var);
data missing_summary;
	set &base.;
	if &var. = " " then &var._missing = "Missing";
	else &var._missing = "Available";
run;
proc freq data = work.missing_summary;
	table &var._missing / nocum nopercent;
%mend;


%macro get_missing_values(df);
	proc means data = &df. nmiss N;
		VAR _Numeric_;
	run;
%mend;

%get_missing_values(&lib..credit);
%get_char_type_missing(&lib..credit,JOB);

	/** -- identification de la strategie optimal -- **/
%macro Numeric_stg(df,variable);
	PROC MEANS data = &df. MEAN MEDIAN STD CV;
		VAR  &variable.;
	RUN;
%mend;

%LET var_numeric = LOAN MORTDUE VALUE YOJ CLAGE CLNO DEBTINC;
%LET var_binaire = DEROG DELINQ NINQ;

%Numeric_stg(&lib..credit,CLAGE);

%macro Categoric_stg(df,var);
	PROC FREQ data = &df.
	order=freq ;
	table &var. ;
	RUN ;
%mend;

%Categoric_stg(&lib..credit,REASON);
%description(LOAN);


	 /*********************************************/
	/** -- imputation des données manquantes -- **/
   /*********************************************/


/** Option : Mathode manuelle plus pratique mais moins optimal en cas de big data **/

data &lib..New_base;
	set &lib..credit;
	
						/** -- Traitement des variables Continues -- **/

	if LOAN = . then LOAN = 16300.00;
	if MORTDUE = .  then MORTDUE = 65019.00;
	if VALUE = . then VALUE = 89235.50;
	if YOJ = . then YOJ = 7.00;
	if CLAGE = . then CLAGE = 173.47;
	if CLNO = . then CLNO = 20.00;
	if DEBTINC = . then DEBTINC = 34.82;

						/** -- Traitement des variables Binaire -- **/

	if DEROG = . then DEROG = 0;
	if DELINQ = . then DELINQ = 0;
	if NINQ = . then NINQ = 0;

						/** -- Traitement des variables Categorielle -- **/

	if REASON = " " then REASON = "Unkown";
	if JOB = " " then JOB = "Unkown";

run;


  /******************************************/
 /** __Analyse exploratoire des données__ **/
/******************************************/

		/** -- Regroupement des variables en fonction de leur type (Numerique, Categorielle et Binaire) -- **/

%LET var_categoric = REASON JOB;
%LET var_numeric = LOAN MORTDUE VALUE YOJ CLAGE CLNO DEBTINC;
%LET var_binaire = DEROG DELINQ NINQ;

		/** -- visualisation des variables categorielles et binaire (bar plot or count plot) -- **/
%macro Barplot(var,df);
proc sgplot data = &df.;
	VBAR &var.;
RUN;
%mend;

%Barplot(NINQ,&lib..New_base);

		/** -- visualisation des variables numeriques (histogramme ou distplot) -- **/

%macro Histplot(var,df);
PROC UNIVARIATE DATA = &df. NOPRINT;
HISTOGRAM &var. / NORMAL CFILL = ltgray;
INSET N = "Nombre d'observation" MEDIAN (8.2) MEAN (8.2) STD='Ecart type' (8.3)
/ POSITION = ne;
RUN;
%mend;

%Histplot(&var_numeric.,&lib..New_base);


								 /****************************************/
								/** Statistique descriptif des données **/
							   /****************************************/


		/** -- Statistique des variables categorielles et binaires -- **/
%macro stat_feq(var,base);
PROC FREQ data = &base.
	order=freq ;
	table  &var.;
	RUN ;
%mend;

%stat_feq(&var_binaire.,&lib..New_base); /*var_binaire*/


		/** -- Statistique des variables numeriques -- **/
%macro stat_des(variable,base);
PROC MEANS data = &base. MEAN STD MIN Q1 MEDIAN Q3 MAX;
	VAR  &variable.;
	RUN ;
%mend;

%stat_des(&var_numeric.,&lib..New_base);


								 /*********************************/
								/** Analyse du Profil de défaut **/
							   /*********************************/


		/** -- variables categorielles et binaires -- **/
%macro Profile_risk_cat(base,target,var);
proc FREQ data = &base. ;
table &target. * &var.  / nocol norow nopercent;   
run;
%mend;

%Profile_risk_cat(&lib..New_base,JOB,JOB);

		/** -- variables numerique -- **/

%macro Profile_risk_num(base,target,var);
proc sql;
select &target.,count(&var.) as COUNT_&var., mean(&var.) as MEAN_&var.,std(&var.) as STD_&var.,min(&var.) as MIN_&var.,max(&var.) as MAX_&var.
from &base.
group by &target.
having MEAN_&var.;
quit;
%mend;

%Profile_risk_num(&lib..New_base,BAD,LOAN);


								 /**********************************************************************/
								/** Pretraitement des données et sélection des variables pertinentes **/
							   /**********************************************************************/


		/** -- analyse graphique Categorical AND Continuous  (Box Plot) -- **/
	
** script R pour graphe plus optimal;

		/** -- Mesure statistique : Test d'ANOVA -- **/

%macro AnovaFunction(exog,endog,base);
proc anova data =&base.;
 class &endog.;
 Model &exog. = &endog.;
run;
%mend;

%LET var_numeric = LOAN MORTDUE VALUE YOJ CLAGE CLNO DEBTINC;
%LET var_binaire = DEROG DELINQ NINQ;
%AnovaFunction(endog = BAD,exog = NINQ,base=&lib..New_base);

		/** -- analyse graphique Categorical VS  Categorical  (Bar Plot) -- **/

** script R pour graphe plus optimal;

		/** -- Mesure statistique : Test de Khi-2 -- **/
%macro ChisqFunction(df,target,var);
proc freq DATA=&df.;
TABLE &target.*&var. / chisq expected missing DEVIATION NOROW NOCOL NOPERCENT;
run;
%mend;

%ChisqFunction(&lib..New_base,BAD,REASON);



  /***********************************************************/
 /** Traitement des données aberrante : Methode du Z-score **/
/***********************************************************/


/** construction de la table avec les variable selectionneé **/ 

data &lib..echantillon_analyse;
	set &lib..New_base ;
	drop CLNO;
	LVT = MORTDUE / VALUE;
RUN;


/** Standardisation de données **/

PROC STANDARD DATA = &lib..echantillon_analyse 
MEAN=0 
STD=1 
OUT=&lib..zscores;
VAR _NUMERIC_;
RUN;

/** Elimination des données aberrantes **/

DATA &lib..clean_base;
	SET &lib..zscores;
		WHERE 
			ABS(clage) < 3 & 
			ABS(clno) <3 & 
			ABS(debtinc) < 3 & 
			ABS(delinq) <3 & 
			ABS(derog) < 3 & 
			ABS(loan) < 3 & 
			ABS(mortdue)< 3 & 
			ABS(ninq)<3 & 
			ABS(value) < 3 & 
			ABS(yoj) < 3;
RUN;


								 /*************************************************************/
								/** Création de classe et construction de nouvelle variable **/
							   /*************************************************************/

%macro segment (var,base);
proc rank data =  &lib..&base. out = echantillon_class group=12 ;
	var &var. ; 
	ranks q&var.;
run ;
proc freq data = echantillon_class ;
	tables 	q&var.*BAD /chisq out= chi_&var.;
run ;
proc transpose data = chi_&var. out = chi_&var._1;
	var count ;
	id BAD ;
	by q&var. ;
run ;
data chi_&var._1 ;
	set chi_&var._1 ;
	p1=_1/(_0+_1) ; /* P1 est le nombre de default sur la somme des contreparties*/
run ;
proc gplot data = chi_&var._1 ;
	plot p1*q&var. ;
run ;
quit ;

proc means data = echantillon_class;
	var &var. ;
	class q&var. ;
run ;
%mend;

	/* Variable DEBTINC */
%segment(DEBTINC,New_base);

	/* Variable DEROG */
%segment(DEROG,New_base);

	/* Variable DELINQ */
%segment(DELINQ,New_base);

	/* Variable NINQ */
%segment(NINQ,New_base);

	/* Variable VALUE */
%segment(VALUE,New_base);

	/* Variable LOAN */
%segment(LOAN,New_base);

	/* Variable YOJ */
%segment(YOJ,New_base);

	/* Variable CLAGE */
%segment(CLAGE,New_base);

	/* Variable LVT */
%segment(LVT,echantillon_analyse);

	/* Variable CLNO */
%segment(CLNO,New_base);


								/** Création de classe et construction de nouvelle variable**/













								 /***********************************/
								/** Construction de la base final **/
							   /***********************************/


data &lib..echantillon_final;
	set &lib..echantillon_analyse;

	/* Construction des classe pour la variable DEBTINC */
	if DEBTINC < 24 then DEBTINC_class = "DEBTINC < 24" ;
	else if 24 <= DEBTINC < 31 then DEBTINC_class = "24 <= DEBTINC < 31" ;
	else if 31 <= DEBTINC < 41.44 then DEBTINC_class = "31 <= DEBTINC < 41.44" ;
	else DEBTINC_class = "41.44 <= DEBTINC" ;

	/* Construction des classe pour la variable DEROG */
	if DEROG < 1 then DEROG_class = "DEROG < 1";
	else DEROG_class = "1 <= DEROG";

	/* Construction des classe pour la variable DELINQ */
	if DELINQ < 1 then DELINQ_class = "DELINQ < 1" ;
	else if 1 <= DELINQ < 2  then DELINQ_class = "1 <= DELINQ < 2" ;
	else  DELINQ_class = "2 <= DELINQ";

	/* Construction des classe pour la variable NINQ */
	if NINQ < 1 then NINQ_class = "NINQ < 1" ;
	else if 1 <= NINQ < 2 then NINQ_class = "1 <= NINQ < 2" ;
	else if 2 <= NINQ < 4 then NINQ_class = "2 <= NINQ < 4" ;
	else  NINQ_class = "4 <= NINQ" ;


	/* Construction des classe pour la variable VALUE  */
	if VALUE < 48900 then VALUE_class = "VALUE < 48900" ;
	else if 48900 <= VALUE < 89236 then VALUE_class = "48900 <= VALUE < 89236" ;
	else if 89236 <= VALUE < 132297 then VALUE_class = "89236 <= VALUE < 132297" ;
	else VALUE_class = "132297 <= VALUE";

	/* Construction des classe pour la variable LOAN */
	if  LOAN < 7600 then LOAN_class = "LOAN < 7600" ;
	else if 7600 <= LOAN < 10000 then LOAN_class = "7600 <= LOAN < 10000" ;
	else if 10000 <= LOAN < 15300 then LOAN_class = "10000 <= LOAN < 15300" ;
	else if 15300 <= LOAN < 40000 then LOAN_class = "15300 <= LOAN < 40000" ;
	else LOAN_class = "40000 <= LOAN" ; 

	/* Construction des classe pour la variable YOJ */
	if YOJ < 5 then YOJ_class = "YOJ < 5" ;
	else if 5 <= YOJ <  21 then YOJ_class = "5 <= YOJ <  21" ;
	else YOJ_class = "21 <= YOJ ";


	/* Construction des classe pour la variable CLAGE */

	if CLAGE <86 then CLAGE_class = "CLAGE < 86" ;
	else if 86 <= CLAGE < 174 then CLAGE_class = "86 <= CLAGE < 174" ;
	else if 174 <= CLAGE < 247.1 CLAGE_class = "174<= CLAGE < 247.1" ;
	else CLAGE_class = "247.1 <= CLAGE" ;

	/* Construction des classe pour la variable CLAGE */
	if JOB in ("MGR","OTHER") then JOB_class = "MGR - OTHER" ;
	if JOB in ("OFFICE") then JOB_class = "OFFICE" ;
	if JOB in ("PROFEXE") then JOB_class = "PROFEXE" ;
	if JOB in ("SALES", "SELF") then JOB_class = "SALES - SELF" ;
	else JOB_class = "Unkown" ;

	/* Construction des classe pour la variable CLAGE */
	if 0.5 <= LVT < 1 then LVT_class = "0.5 <= LVT < 1" ;
	else LVT_class = "1 <= LVT" ; 
run ;

%get_char_type_missing(&lib..echantillon_final,DELINQ_class);

proc sql;

create table base_finale as
	select
		a.BAD , 
		a.LOAN_class ,
		a.MORTDUE_class ,
		a.VALUE_class ,
		a.YOJ_class ,
		a.CLAGE_class ,
		a.DEBTINC_class ,
		a.DEROG_class ,
		a.DELINQ_class ,
		a.NINQ_class ,
		a.LVT_class ,
		a.REASON ,
		a.JOB

	from  &lib..echantillon_final as a
;
quit;


		/** -- Split des données (données des test et données d'apprentissage) -- **/


data training testing; 
	set base_finale nobs = nobs; 
	if _n_ <= .80*nobs then output &lib..training; 
	else output &lib..testing; 
run;

								 /******************/
								/** Modélisation **/
							   /******************/

data pre_processing;
set &lib..base_finale;
	


		/** -- Modèle logistique -- **/

PROC LOGISTIC DATA=&lib..training ;
class BAD LOAN_class MORTDUE_class VALUE_class YOJ_class CLAGE_class JOB
	  DEBTINC_class DEROG_class DELINQ_class NINQ_class LVT_class REASON  /PARAM=glm ;
MODEL BAD (event='1') = LOAN_class MORTDUE_class VALUE_class YOJ_class CLAGE_class JOB
	  DEBTINC_class DEROG_class DELINQ_class NINQ_class LVT_class REASON/ 
	LINK= LOGIT /*specification du modele utiliser*/
	ALPHA= 0.05 /*seuil pour la construction d'IC*/
	selection = stepwise; /*methode de selection des varaible*/
	OUTPUT OUT = &lib..resultat_/*preciser le fait qu'on souhaite obtenir les resultat en sortie dans une table*/
	PROB = probpred /*element que doit contenir la table en sortie*/
	XBETA = logcote;/*element que doit contenir la table en sortie*/
RUN;


/** Evaluation des metriques **/






					 /****************************************/
					/** construction de la grille de score **/
				   /****************************************/

/** on definit la nombre de points sur le quel la grille de score sera calibre et le odds ainsi que le PDO **/

/** nb_point = 1000 | ods = 500:1 |  pdo = 30 **/

%macro BuldScore(nb_point,ods,pdo,df);

data &lib..score_grid;
	set &lib..resultat_;

		odd = exp(logcote);

/** etape 1 : on calcul le factor et le offset pour determiner la formule de calcul de score fnale pour la grille de score **/
		Factor = &pdo. / ln (2);
		Offset = &nb_point. - (Factor * ln (&ods.));

/** etape 2 : on calcul le score fnale pour la grille de score **/
		Credit_Score = Offset + Factor * ln (odds);

run;
%mend;


/* A implementer dans une Macro pour le calcule des effets marginaux */

/* 
Etape 1 :
On prend comme reference la proba d'etre en defaut: p=p°

Etape 2 :
On calcule ensuite p*(1-p)=p°*(1-p°)= val_

Etape 2 :
On multiplie chaque coeff par cette valeur c_a_d
coef1_ = coef1*val_ = EM_

Interpretation : 

cela signifie qu'une hausse d'un millier d'euros de patrimoine implique une hausse de 0.7 points de poucentage,
c'est-a-dire que la probabilite sera alors egale a p=0.3955+0.0072=0.4027.

Pour le score, le changement mesuré ici est un changement d'une unite, c'est-a-dire d'un ecart-type de la variable
scopt = -0.230*0.239 = -0.055

*/

/* Modele de probabilité linéaire */
PROC DISCRIM DATA=&EM_IMPORT_DATA WCOV PCOV CROSSLIST
WCORR PCORR Manova testdata=&EM_IMPORT_VALIDATE testlist;
CLASS %EM_TARGET;
VAR %EM_INTERVAL;
run;


								/** Backtesting du score **/
* calculate IV for the variables;
PROC HPBIN DATA=data.mortgage NUMBIN=5;
INPUT FICO_orig_time;
ODS OUTPUT MAPPING=Mapping;
RUN;
PROC HPBIN DATA=data.mortgage WOE BINS_META=Mapping;
TARGET default_time/LEVEL=BINARY;
RUN;
