

								 /*--------------------------*/
				   				/*    PROJET SCORING SAS    */
				  			   /*--------------------------*/


/** Creation de la librairie **/
%let path = "C:\Users\marsh\Desktop\Projet SAS"; 
libname lb &path; 
run;

%let lib = work;


  
 								/** Chargement des données**/						   
PROC IMPORT datafile='&path.\hmeq.csv'
	dbms=csv 
	out=&lib..credit;		
RUN;

data credit;
	set &lib..HMEQ;
run;

							 /*****************************/
							/** Dictionnaire de données **/
						   /*****************************/


%macro description(var);

/**CHARGEMENT DE LA BASE**/
PROC IMPORT datafile='&path.\VAR_INTITULE.xlsx'
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

								/** Affichage et Description des données **/
  
proc print data = &lib..credit (obs=10);   
run;

proc contents data=&lib..credit; 
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
%get_char_type_missing(&lib..credit,REASON);


	 /*********************************************/
	/** -- imputation des données manquantes -- **/
   /*********************************************/


/** Option : Mathode manuelle plus pratique mais moins optimal en cas de big data **/

data &lib..New_base;
	set &lib..credit;
	
						/** -- Traitement des variables Continues : imputation par la median -- **/

	if LOAN = . then LOAN = 16300.00;
	if MORTDUE = .  then MORTDUE = 65019.00;
	if VALUE = . then VALUE = 89235.50;
	if YOJ = . then YOJ = 7.00;
	if CLAGE = . then CLAGE = 173.47;
	if CLNO = . then CLNO = 20.00;
	if DEBTINC = . then DEBTINC = 34.82;

						/** -- Traitement des variables Binaire : imputation par la valeur modale -- **/

	if DEROG = . then DEROG = 0;
	if DELINQ = . then DELINQ = 0;
	if NINQ = . then NINQ = 0;

						/** -- Traitement des variables Categorielle : imputation par la classe Unkown-- **/

	if REASON = " " then REASON = "Unkown";
	if JOB = " " then JOB = "Unkown";

run;


  /******************************************/
 /** __Analyse exploratoire des données__ **/
/******************************************/

		/** -- Regroupement des variables en fonction de leur type (Numerique, Categorielle et Binaire) -- **/

%LET var_categoric = REASON JOB;
%LET var_numeric = LOAN MORTDUE VALUE YOJ CLAGE CLNO DEBTINC DEROG DELINQ NINQ;

		/** -- visualisation des variables categorielles (bar plot or count plot) -- **/
%macro Barplot(var,df);
proc sgplot data = &df.;
	VBAR &var.;
RUN;
%mend;

%Barplot(JOB,&lib..New_base);
%Barplot(REASON,&lib..New_base);

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
								/** Statistique descriptive des données **/
							   /****************************************/


		/** -- Statistique des variables categorielles -- **/
%macro stat_feq(var,base);
PROC FREQ data = &base.
	order=freq ;
	table  &var.;
	RUN ;
%mend;

%stat_feq(&var_categoric.,&lib..New_base);


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


		/** -- variables categorielles -- **/
%macro Profile_risk_cat(base,target,var);
proc FREQ data = &base. ;
table &target. * &var.  / nocol norow nopercent;   
run;
%mend;

%Profile_risk_cat(&lib..New_base,BAD,JOB);
%Profile_risk_cat(&lib..New_base,BAD,REASON);

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



		/** -- Mesure correlation entre la target et les varaibles numeriques : Test d'ANOVA -- **/

%macro AnovaFunction(exog,endog,base);
proc anova data =&base.;
 class &endog.;
 Model &exog. = &endog.;
run;
%mend;


%AnovaFunction(endog = BAD,exog = LOAN,base=&lib..New_base);
%AnovaFunction(endog = BAD,exog = MORTDUE,base=&lib..New_base);
%AnovaFunction(endog = BAD,exog = VALUE,base=&lib..New_base);
%AnovaFunction(endog = BAD,exog = YOJ,base=&lib..New_base);
%AnovaFunction(endog = BAD,exog = CLAGE,base=&lib..New_base);
%AnovaFunction(endog = BAD,exog = CLNO,base=&lib..New_base);
%AnovaFunction(endog = BAD,exog = DEBTINC,base=&lib..New_base);
%AnovaFunction(endog = BAD,exog = DEROG,base=&lib..New_base);
%AnovaFunction(endog = BAD,exog = DELINQ,base=&lib..New_base);
%AnovaFunction(endog = BAD,exog = NINQ,base=&lib..New_base);


		/** -- Mesure correlation entre la target et les varaibles categorique : Test de Khi-2 -- **/
%macro ChisqFunction(df,target,var);
proc freq DATA=&df.;
TABLE &target.*&var. / chisq expected missing DEVIATION NOROW NOCOL NOPERCENT;
run;
%mend;

%ChisqFunction(&lib..New_base,BAD,REASON);
%ChisqFunction(&lib..New_base,BAD,JOB);

/** -- Mesure correlation entre les varaible continue -- **/
proc corr spearman data=&lib..New_base ; 
var _Numeric_ ; 
run; /**remarque : forte correlation entre VALUE  et MORTDUE**/ 

/** construction d'une nouvelle varaible LVT pour gerer le probleme de correlation entre VALUE  et MORTDUE **/ 
data &lib..echantillon_analyse;
	set &lib..New_base ;
	LVT = MORTDUE / VALUE;
RUN;


/** Segmentation varible continue **/

%macro segment (var,base);
proc rank data =  &base. out = echantillon_class group=12 ;
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
%segment(DEBTINC,&lib..echantillon_analyse);

	/* Variable DEROG */
%segment(DEROG,&lib..echantillon_analyse);

	/* Variable DELINQ */
%segment(DELINQ,&lib..echantillon_analyse);

	/* Variable NINQ */
%segment(NINQ,&lib..echantillon_analyse);

	/* Variable LOAN */
%segment(LOAN,&lib..echantillon_analyse);

	/* Variable VALUE */
%segment(VALUE,&lib..echantillon_analyse);

	/* Variable LVT */
%segment(LVT,&lib..echantillon_analyse);

	/* Variable YOJ */
%segment(YOJ,&lib..echantillon_analyse);

	/* Variable CLAGE */
%segment(CLAGE,&lib..echantillon_analyse);



				    	/** Construction de groupe en fonction de leur profile de rique **/


data &lib..echantillon_final;
	set &lib..echantillon_analyse;

	/* Construction des classe pour la variable DEBTINC */
	if 5.07 <= DEBTINC < 34.82 and 34.82 < DEBTINC < 41.33 then DEBTINC_class = "grp1 " ;
	else if DEBTINC = 34.82 then DEBTINC_class = "grp2" ;
	else if 41.33 <= DEBTINC <= 203.31 then DEBTINC_class = "grp3" ;
	else DEBTINC_class = "grp4";

	/* Construction des classe pour la variable DEROG */
	if DEROG = 0 and 34.82 < DEROG < 41.33 then DEROG_class = "grp1 " ;
	else if DEROG = 1 then DEROG_class = "grp2" ;
	else if 2 <= DEROG <= 10 then DEROG_class = "grp3" ;
	else DEROG_class = "grp4" ;

	/* Construction des classe pour la variable DELINQ */
	if DELINQ =0 then DELINQ_class = "grp1" ;
	else if DELINQ = 1  then DELINQ_class = "grp2" ;
	else if  2 <= DELINQ <= 15 then DELINQ_class = "grp3";
	else DELINQ_class = "grp4";

	/* Construction des classe pour la variable NINQ */
	if 0<= NINQ <= 1 then NINQ_class = "grp1" ;
	else if 2 <= NINQ <= 3 then NINQ_class = "grp2" ;
	else if  4 <= NINQ <= 17 then NINQ_class = "grp3" ;
	else NINQ_class = "grp4";

	/* Construction des classe pour la variable VALUE  */
	if 8000 <= VALUE <= 46045 and 89235.5 <= VALUE <= 96637 then VALUE_class = "grp1" ;
	else if 46053 <= VALUE <= 89231 and 119019 <= VALUE <= 855909 then VALUE_class = "grp2" ;
	else if 96642 <= VALUE <= 119000 then VALUE_class = "grp3";
	else VALUE_class = "grp4";

	/* Construction des classe pour la variable CLNO  */
	if 15 <= CLNO <= 16 and 24 <= CLNO <= 26 then CLNO_class = "grp1" ;
	else if 9 <= CLNO <= 14 and 17 <= CLNO <= 23 and 27 <= CLNO <= 71 then CLNO_class = "grp2" ;
	else if 0 <= CLNO <= 8 then CLNO_class = "grp3";
	else CLNO_class = "grp4";

	/* Construction des classe pour la variable LOAN */
	if 1100 <= LOAN <= 7000 then LOAN_class = "grp1";
	else if 7100 <= LOAN <= 11000 and  14900 <= LOAN <= 16300 and  32800 <= LOAN <= 89900 then LOAN_class = "grp2";
	else if 11100 <= LOAN <= 14800 and 18400 <= LOAN <= 20700 and 23400 <= LOAN <= 28200 then LOAN_class = "grp3" ; 
	else if 16400 <= LOAN <= 18300 and 20800 <= LOAN <= 23300 and 26300 <= LOAN <= 32700 then LOAN_class = "grp4";
	else LOAN_class = "grp5";

	/* Construction des classe pour la variable YOJ */
	if 6.5 <= YOJ <= 7 and 22 <= YOJ <= 41  then YOJ_class = "grp1";
	else if 5 <= YOJ <= 6 and 7.2 <= YOJ <= 8 and 8.3 <= YOJ <= 9.9 and 16 <= YOJ <= 21 then YOJ_class = "grp2";
	else if 0 <= YOJ <= 1.9 and 10 <= YOJ <= 15 then YOJ_class ="grp3";
	else if 2 <= YOJ <= 4.6 then YOJ_class ="grp4";
	else YOJ_class ="grp5";

	/* Construction des classe pour la variable CLAGE */
	if 0 <= CLAGE <= 81.22 then CLAGE_class = "grp1";
	else if 81.23 <= CLAGE <= 156.67 then CLAGE_class = "grp2";
	else if 156.68 <= CLAGE <= 184.55 then CLAGE_class = "grp3";
	else if 184.56 <= CLAGE <= 256.63 then CLAGE_class = "grp4";
	else if 256.64 <= CLAGE <= 1168.23 then CLAGE_class = "grp5";
	else CLAGE_class = "grp6";

	/* Construction des classe pour la variable JOB */
	if JOB in ("Mgr","Other") then JOB_class = "MGR - OTHER" ;
	else if JOB in ("Sales","Self") then JOB_class = "SALES - SELF" ;
	else if JOB in ("Unkown") then JOB_class = "Unkown" ;
	else JOB_class = "OFFICE - PROFEXE" ;

	/* Construction des classe pour la variable REASON */
	if REASON in ("HOMELMP") then REASON_class = "HOMELMP" ;
	else REASON_class = "DebtCon - Unkown" ;

	/* Construction des classe pour la variable LVT */
	if 0.57 <= LVT <= 0.63 and 0.76 <= LVT <= 0.78 and 0.85 <= LVT <= 0.93 then LVT_class = "grp1" ;
	else if 0.64 <= LVT < 0.67 and 0.81 <= LVT <= 0.85 then LVT_class = "grp2";
	else if 0.68 <= LVT < 0.7 and 0.73 < LVT <= 0.76 and 0.79 <= LVT <= 0.81 then LVT_class = "grp3";
	else if  0.02 <= LVT <= 0.56 and  0.71 <= LVT <= 0.73 and  0.94 <= LVT <= 7.38 then LVT_class = "grp4" ;
	else LVT_class = "grp5";

	selected = 1;
run ;

%let varmod = DEBTINC_class DEROG_class DELINQ_class LOAN_class 
VALUE_class YOJ_class LVT_class CLNO_class JOB_class REASON;

/** selection des varaible par la methode LASSO **/
 
PROC GLMSELECT DATA = &lib..echantillon_final 
	PLOTS = ALL 
	SEED = 42; 
	PARTITION ROLE = SELECTED (TRAIN='1' TEST='0');
	CLASS &varmod ; 
	MODEL BAD = &varmod  / 
		SELECTION = LAR (CHOOSE = CV STOP=NONE) 
		CVMETHOD= RANDOM(10); 
RUN;


/** Base final traiter sous python **/

PROC IMPORT datafile='&path.\train_test.csv'
	dbms=csv 
	out=&lib..credit;		
RUN;


		/** -- Split des données (données des test et données d'apprentissage) -- **/

data training testing; 
	set &lib..echantillon_final nobs = nobs; 
	if _n_ <= .80*nobs then output &lib..training; 
	else output &lib..testing; 
run;

								 /******************/
								/** Modélisation **/
							   /******************/
	
%let var_selected = DEBTINC_class DEROG_class DELINQ_class LOAN_class 
VALUE_class YOJ_class LVT_class CLNO_class JOB_class REASON;
		/** -- Modèle logistique -- **/

PROC LOGISTIC DATA=&lib..training ;
class &var_selected./PARAM=glm ;
MODEL BAD (event='1') = &var_selected./ 
	LINK= LOGIT 										/*specification du modele utiliser ici logit*/
	ALPHA= 0.05 										/*seuil pour la construction d'IC*/
	selection = stepwise 								/*methode de selection des varaible*/
	outroc=Tab_Roc_train  								/*output d'uen table pour la constructio de la courbe de roc*/
	ctable   								            /*output d'uen table de matrice de confusion*/
	sle=0.05   								            /*seuil de significativité de variable à retenir dans le modele*/
	rsquare   								            /*obtention du R² pour l'evaluation du modèle*/
	lackfit; 											/*obtention de la la statistique de lift pour l'evaluation du modèle*/
	OUTPUT OUT = &lib..resultat_train					/*preciser le fait qu'on souhaite obtenir les resultat en sortie dans une table*/
	PROB = probpred_train 								/*probabilité permettant de decidé de la classe predite*/
	XBETA = Y_etoile_train								/*valeur des coef permetant de calculer le odds ration en faisant odd = exp(Y_etoile_train)*/
	PREDICTED=Y_pred_train;  							/*valeur des Y_pred*/
RUN;


								 /**************************/
								/**  Analyse du Modèle   **/
							   /**************************/


data &lib..echantillon_top;
	set &lib..echantillon_final;

	/* Construction des classe pour la variable DELINQ */
	if DELINQ_class in ("grp2" "grp3") then DELINQ_class = "grp1";
	else DELINQ_class = "grp2";

	/* Construction des classe pour la variable VALUE  */
	if VALUE_class = "grp4" then VALUE_class = "grp1" ;
	else if VALUE_class in ("grp1" "grp3") then VALUE_class = "grp2";
	else VALUE_class = "grp3";
run ;


data training testing; 
	set &lib..echantillon_top nobs = nobs; 
	if _n_ <= .80*nobs then output &lib..training; 
	else output &lib..testing; 
run;

		/** -- Modèle logistique -- **/

PROC LOGISTIC DATA=&lib..training ;
class &var_selected./PARAM=glm ;
MODEL BAD (event='1') = &var_selected./ 
	LINK= LOGIT 										/*specification du modele utiliser ici logit*/
	ALPHA= 0.05 										/*seuil pour la construction d'IC*/
	selection = stepwise 								/*methode de selection des varaible*/
	outroc=Tab_Roc_train  								/*output d'uen table pour la constructio de la courbe de roc*/
	ctable   								            /*output d'uen table de matrice de confusion*/
	sle=0.05   								            /*seuil de significativité de variable à retenir dans le modele*/
	rsquare   								            /*obtention du R² pour l'evaluation du modèle*/
	lackfit; 											/*obtention de la la statistique de lift pour l'evaluation du modèle*/
	OUTPUT OUT = &lib..resultat_train					/*preciser le fait qu'on souhaite obtenir les resultat en sortie dans une table*/
	PROB = probpred_train 								/*probabilité permettant de decidé de la classe predite*/
	XBETA = Y_etoile_train								/*valeur des coef permetant de calculer le odds ration en faisant odd = exp(Y_etoile_train)*/
	PREDICTED=Y_pred_train;  							/*valeur des Y_pred*/
RUN;





								 /**************************/
								/** Evaluation du modele **/
							   /**************************/

/****COURBE DE ROC****/

/*--- Echantillon train ---*/
Proc Logistic Data=&lib..training OutEst=Tab_Est ; 
class &var_selected./PARAM=glm ;
MODEL BAD (event='1') = &var_selected./ 
	Link=logit 
	OutRoc=ROC_train ; 
Run ; 
proc gplot data= ROC_train ; 
 PLOT _SENSIT_*(_1MSPEC_ _SENSIT_) / OVERLAY ; 
 title "courbe de Roc echantillon apprentissage"; 
RUN ; 
QUIT ;
 
/*--- Echantillon test ---*/
Proc Logistic Data=&lib..testing InEst=Tab_Est ; 
class &var_selected./PARAM=glm ;
MODEL BAD (event='1') = &var_selected. /
	Link=logit 
	OutRoc=ROC_test 
	MaxIter=0 ; 
Run ; 
PROC GPLOT DATA = ROC_test ; 
 PLOT _SENSIT_*(_1MSPEC_ _SENSIT_) / OVERLAY ; 
 title "Courbe de Roc "; 
RUN ; 
QUIT ;



/****COURBES DE SELECTION, PERFORMANCE, DISCRIMINATION ET GINI****/

%Macro Indice_Gini(Tab_Score=,Var_Qual=,Var_Score=); 

Proc SQL NoPrint ; 
  Create Table Temp_1 As 
	Select &Var_Qual, &Var_Score From &Tab_Score 
	Order By &Var_Score DESC ; 
  Quit ;
 
Data Temp_1 ; 
  Set Temp_1 ; 
	Retain Nb Nb0 Nb1 0 ; 
	Nb=Nb+1 ; 
	If &Var_Qual=0 Then Nb0=Nb0+1 ; /* Défaillants */
	Else If &Var_Qual=1 Then Nb1=Nb1+1 ; /* Non-défaillants */
  Run ;

Data _NULL_ ; 
  Set Temp_1 ; 
	Call Symput ("Nb_Tot",Nb) ; 
	Call Symput ("Nb_Tot_0",Nb0) ; 
	Call Symput ("Nb_Tot_1",Nb1) ; 
  Run ;
 
/* Courbe de sélection */

Data Temp_1 ; 
  Set Temp_1 ; 
	X_Sel=Nb/&Nb_Tot ; 
	Y_Sel=Nb0/&Nb_Tot_0 ; 
	If (Nb/&Nb_Tot) <= (&Nb_Tot_1/&Nb_Tot) Then Y_Sel_Max=0 ; 
	Else Y_Sel_Max=1+((Nb/&Nb_Tot-1)/(&Nb_Tot_0/&Nb_Tot)) ; 
	y_discr=(Nb-Nb0)/(&Nb_Tot-&Nb_Tot_0); 
	x_discr=Y_Sel; 
	y_perf=(Nb0/Nb)/(&Nb_Tot_0/&Nb_Tot); 
	x_perf=x_sel; 
  run ; 

/* Calcul de l'indice de Gini */

Data Temp_1 ; 
  Set Temp_1 ; 
	Aire_Courbe_Sel=(1/&Nb_Tot)*(X_Sel-Y_Sel) ; 
  Run ; 

Proc SQL NoPrint ; 
	Select Sum(Aire_Courbe_Sel) Into : Som_ACS From Temp_1 ; 
  Quit ; 

%Put N : &Nb_Tot ; 
%Put N (Défaillants) : &Nb_Tot_0 ; 
%Put N (Non-Défaillants) : &Nb_Tot_1 ; 
%Let Gini_Index_Temp=%SysFunc(ABS(&Som_ACS/((1-(&Nb_Tot_0/&Nb_Tot))/2))) ; 
%Let Gini_Index=%SysFunc(Round(&Gini_Index_Temp,0.0001)) ; 

Proc SQL NoPrint ; 
  Create Table Gini (Gini Num) ; 
 	Insert Into Gini Values (&Gini_Index) ; 
  Quit ; 

Proc Print Data=Gini ; 
	Var Gini ; 
	Format Gini NUMX8.3 ; 
  Run ; 

GOptions Device=win Devmap=winansi Keymap=winansi FTitle=Triplex HTitle=3
CBack=White Border Htext=1 FText=Complex ; 
Symbol1 i=join v=none c=Red w=2 ; 
Symbol2 i=join v=none c=Orange w=2 l=3 ; 
Symbol3 i=join v=none c=yellow w=2 l=3 ; 
Symbol4 i=none v=none c=White width=1 ; /* Ligne blanche : permet d'afficher la statistique de Gini à la ligne */

/*** Proc Gplot - Courbe de sélection ***/

Proc Gplot Data=Temp_1 ; 
Title "Courbe de sélection" ; 
  Axis1 Label=(Color=Black F=Times H=2 Justify=Center "Part des individus selectionnes") 
	Order=0 To 1 By 0.1 ; 
  Axis2 Label=(Angle=90 Color=Black F=Times H=2.5 Justify=Center "Selection") 
	Order=0 To 1 By 0.1 ; 
  Legend1 
	Across=1 
	CBorder=Black 
	Position=(Top Inside Right) 
	OffSet=(-50,-5) 
	Mode=Share 
  Value=(Tick=1 "Score analysé" 	
		 Tick=2 "Score parfait" 
		 Tick=3 "Score aléatoire" 
		 Tick=4 "Indice de Gini = &Gini_Index") 
	Shape=symbol(6,2.5) 
  Label=None ; 
  Plot (Y_Sel Y_Sel_max X_Sel Nb)*X_Sel/ 
  OverLay 
	Legend=Legend1 
	NoFrame 
	Grid 
	HAxis=Axis1 
	VAxis=Axis2 
	VRef=1
	LVref=2 
	HRef=1 
	LHref=2 
	Name='CRBSEL' ; 
 Run ;
Quit ; 

/*** Proc Gplot - Courbe de performance ***/

Proc Gplot Data=Temp_1 ; 
Title "Courbe de performance" ; 
  Axis1 Label=(Color=Black F=Times H=2 Justify=Center "Part des individus selectionnes") 
	Order=0 To 1 By 0.1 ; 
  Axis2 Label=(Angle=90 Color=Black F=Times H=2.5 Justify=Center "Sélection") 
	Order=0 To 1 By 0.1 ; 
  Legend1 
	Across=1 
	CBorder=Black Position=(Top Inside Right) 
	OffSet=(-50,-5) 
	Mode=Share 
 	Value=(Tick=1 "Score analysé" Tick=2 "Score aléatoire" ) 
	Shape=symbol(6,2.5) 
	Label=None ; 
   Plot (Y_perf X_perf )*X_perf / 
  OverLay 
	Legend=Legend1 
	NoFrame 
	Grid 
	HAxis=Axis1 
	VAxis=Axis2 
	VRef=1
	LVref=2 
	HRef=1 
	LHref=2 
	Name='CRBSEL' ; 
 Run ; 

/*** Proc Gplot - Courbe de discrimination ***/

Proc Gplot Data=Temp_1 ; 
Title "Courbe de discrimination" ; 
  Axis1 Label=(Color=Black F=Times H=2 Justify=Center "Part des individus selectionnes") 
	Order=0 To 1 By 0.1 ; 
  Axis2 Label=(Angle=90 Color=Black F=Times H=2.5 Justify=Center "Selection") 
	Order=0 To 1 By 0.1 ; 
  Legend1 
	Across=1 
	CBorder=Black 
	Position=(Top Inside Right) 
	OffSet=(-50,-5) 
	Mode=Share 
 	Value=(Tick=1 "Score analysé" Tick=2 "Score aléatoire" ) 
  Shape=symbol(6,2.5) 
  	Label=None ; 
  Plot (Y_discr X_discr )*X_discr/ 
  OverLay 
	Legend=Legend1 
	NoFrame 
	Grid 
	HAxis=Axis1 
	VAxis=Axis2 
	VRef=1
	LVref=2 
	HRef=1 
	LHref=2 
	Name='CRBSEL' ; 
 Run ; 
Quit ; 
Quit ; 

/*--- Elimine les tables créées ---*/
/*Proc DataSets LIBRARY=WORK ; 
 Delete Temp_1 Gini ; 
Run ; 
Quit ;*/
%Mend ;

PROC LOGISTIC DATA=&lib..testing ;
class &var_selected./PARAM=glm ;
MODEL BAD (event='1') = &var_selected./ 
	LINK= LOGIT 										
	ALPHA= 0.05 									
	selection = stepwise 							
	outroc=Tab_Roc_test
	ctable
	sle=0.05
	rsquare
	lackfit; 										
	OUTPUT OUT = &lib..resultat_test				
	PROB = probpred_test 							
	XBETA = Y_etoile_test								
	PREDICTED=Y_pred_tets;
RUN;

**train gini;
%Indice_Gini(Tab_score=&lib..resultat_train,
			 Var_Qual=BAD,
			 Var_Score=Y_etoile_train); 
**test gini;
%Indice_Gini(Tab_score=&lib..resultat_test,
			 Var_Qual=BAD,
			 Var_Score=Y_etoile_test); 


/** Synthèse des indicateur de performance **/

%macro model_perf(gini_train=,gini_test=,def_train=,def_test=,obs_train=,obs_test=);
  data resume_perf;
	/**get GINI**/
	GINI_train = &gini_train.;
	GINI_test = &gini_test.;
	/**get AUC**/
	AUC_train = (&gini_train. + 1) / 2;
	AUC_test = (&gini_test. + 1) / 2;
	/**get IR : Indice de Robusteste**/
	IR = 1 - ((2*abs(&gini_train. - &gini_test.)) / max((1-(&def_train./&obs_train.)),(1-(&def_test./&obs_test.))));
  run;
%mend;

%model_perf(gini_train = 0.78,
			gini_test = 0.79,
			def_train = 985,
			def_test = 204,
			obs_train = 4768,
			obs_test = 1192);




					 /****************************************/
					/** construction de la grille de score **/
				   /****************************************/

ods trace off;
ods output OddsRatios = &lib..odds_estimation;
PROC LOGISTIC DATA = &lib..training;
class &var_selected./PARAM=glm ;
MODEL BAD (event='1') = &var_selected./ 
	OUTROC = Tab_Roc_App 
	Selection=Backward 
	SLE=0.05 
	Link=logit;
	OUTPUT OUT=logit_resultat 
	p=p_logit 
	XBETA = logcote;
	ods output ParameterEstimates = &lib..parmas_estimation
	TITLE "table de grille de score";

RUN ;

				/** POUR LA GRILLE FINAL VOIR LE FICHIER EXCEL JOINT **/









