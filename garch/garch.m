(* ::Package:: *)

(* Mathematica Package *)

(* Created by the Wolfram Workbench Apr 18, 2014 *)

BeginPackage["garch`garch`"]
ClearAll[Garch,Ugarch,Ogarch,DCCgarch,Ngarch,UgarchForecast]

Needs["RLink`"]

InstallR["RHomeLocation" -> FileNames["C:\\Program Files\\R\\R*"][[1]]](*it will be fixed..Leonid promissed*)
(*InstallR[]*)
 
REvaluate["if(require(\"ccgarch\")==T){library(\"ccgarch\")} else {install.packages(\"ccgarch\");library(\"ccgarch\")}"]
REvaluate["if(require(\"rugarch\")==T){library(\"rugarch\")} else {install.packages(\"rugarch\");library(\"rugarch\")}"]
REvaluate["if(require(\"fGarch\")==T){library(\"fGarch\")} else {install.packages(\"fGarch\");library(\"fGarch\")}"]
REvaluate["if(require(\"gogarch\")==T){library(\"gogarch\")} else {install.packages(\"gogarch\");library(\"gogarch\")}"]
REvaluate["if(require(\"parallel\")==T){library(\"parallel\")} else {install.packages(\"parallel\");library(\"parallel\")}"]
(* REvaluate["{if(require(\"Rsolnp\")==T){library(\"Rsolnp\")} else {install.packages(\"Rsolnp\");library(\"Rsolnp\")}"]*)
REvaluate["{if(require(truncnorm)==T){library(\"truncnorm\")} else {install.packages(\"truncnorm\");library(\"truncnorm\")}}"]
REvaluate["if(require(\"numDeriv\")==T){library(\"numDeriv\")} else {install.packages(\"numDeriv\");library(\"numDeriv\")}"]
REvaluate["if(require(\"tseries\")==T){library(\"tseries\")} else {install.packages(\"tseries\");library(\"tseries\")}"]
REvaluate["if(require(\"zoo\")==T){library(\"zoo\")} else {install.packages(\"zoo\");library(\"zoo\")}"]

(*functions*)
Ogarch::usage ="Ogarch[timeseries, garchmodel, method, dates]

Calculates the orthogonal garch for a given multi time series
	**********************************************************************************
						*- timeseries -*
	********************************************************************************** 
	Your time series without dates.
	If you need to indentify output of some methods 
	with dates you will supply dates as a one column in place of 4th argument.
	**********************************************************************************
						*- garchmodel -*
	**********************************************************************************
	Your specified univariate garch model for Ogarch function.
	It's the same input as in Garch function.Default value is \"garch(1,1)\"
	**********************************************************************************
						*- method -*
	**********************************************************************************
		\"coef\"
		\"residuals\"
		\"cvar\" - conditional variance
		\"ccor\" - conditional correlations
		\"ccov\" - conditional covariances
		\"summary\"
		\"plot\"
		
		(default is \"coef\")
	**********************************************************************************
						*- dates -*
	**********************************************************************************
	Optional, if you need to indentify output of some methods 
	with dates (usefull for DateListPlot function)"
	
Ngarch::usage ="Ngarch[timeseries, impliedvolatility, method]

	Calculates the Ngarch model parameters for given time series of returns and implied volatility
	********************************************************************************************
						*- timeseries -*
	******************************************************************************************** 
	Your time series with/without dates.
	If without dates then it is not important is it row or column.
	If with dates then use as from FinancialData output.
	********************************************************************************************
						*- impliedvolatility -*
	********************************************************************************************
	Time series of market implied volatility.
	Must be without dates and it is not important is it row or column.It has to be anualized!
	********************************************************************************************
						*- method -*
	********************************************************************************************
	\"coef\"
	\"summary\"
	\"volatility\"
	\"residuals\"
	-\"ll\"
	
	(default is \"coef\") 
	
	This is outdated function and will be ommited in future releses"
	
Garch::usage = "Garch[timeseries, garchmodel, method]

- Calculates the garch model parameters for given time series - 
**********************************************************************************
						*  timeseries  *
**********************************************************************************  
	Your time series with/without dates.
	If without dates then it is not important is it row or column.
	If with dates then use as from FinancialData output.
**********************************************************************************
						*  garchmodel  *
**********************************************************************************
	Uses values: \"garch(1,0)\" or \"garch(1,1)\" or \"garch(1,2)\"...or for example 
	\"arma(3,2)+garch(2,4)\" 
	(default is \"garch(1,1)\")
**********************************************************************************
						*  method  *
**********************************************************************************
	\"coef\"
	\"summary\"
	\"volatility\"
	\"residuals\"
	\"fitted\"
	\"plot\"
	\"ll\"
	
	(default is \"coef\")"
Ugarch::usage="Ugarch [ timeseries ,garchmodel, method ]

- Calculates the garch model parameters for given time series - 
**********************************************************************************
						*  timeseries  *
**********************************************************************************  
	Your time series with/without dates.
	If without dates then it is not important is it row or column.
	If with dates then use as from FinancialData output.
**********************************************************************************
						*  garchmodel  *
**********************************************************************************
	Uses values: \"garch(1,0)\" or \"garch(1,1)\" or \"garch(1,2)\"...or \"arma(1,1)+garch(1,1)\"... 
	Valid models (currently implemented) are: \"sGARCH\", \"eGARCH\", \"gjrGARCH\", \"apARCH\",
	\"iGARCH\" and \"csGARCH\".
	And submodels of \"fGARCH\": \"GARCH\", \"TGARCH\", \"AVGARCH\", \"NGARCH\", \"NAGARCH\",
	\"APARCH\",\"GJRGARCH\" and \"ALLGARCH\"
	(default is \"garch(1,1)\")
**********************************************************************************	
						*  method  *
**********************************************************************************
	\"coef\"
	\"summary\"
	\"volatility\"
	\"residuals\"
	\"fitted\"
	\"plot\"
	\"persistence\"
	\"ll\"
	\"uncvariance\"
	\"uncmean\"
	\"halflife\"
(default is \"coef\")
**********************************************************************************"
DCCgarch::usage="DCCgarch [ timeseries, method ]

This function carries out the two step estimation of the (E)DCC-GARCH model and returns estimates,
standardised residuals, the estimated conditional variances, and the dynamic conditional correlations"
UgarchForecast::usage="UgarchForecast[Ugarch[timeseries,garchmodel,opts],method,nahead,nroll]

Method for forecasting from a variety of univariate GARCH models"
(*options*)
ConditionalDistribution::usage="ConditionalDistribution->NormalDistribution

Specify what conditional distribution to use in estimating parameters with maximum likelihood.
**********************************************************************************
				*Avaible distributions(options):*
**********************************************************************************
-\"NormalDistribution\" *
-\"StudentTDistribution\"
-\"SkewNormalDistribution\"
-\"SkewStudentTDistribution\"
-\"Erf\" - Generalized Error Distribution
-\"SkewErf\" - Generalized Skewed Error Distribution
-\"NormalInverseGaussianDistribution\" - General Normal Inverse Gaussian Distribution
-\"QMLE\" - \"QMLE\" stands for Quasi-Maximum Likelihood Estimation, which assumes normal distribution
 and uses robust standard errors for inference. * only for Garch
-\"JohnsonSUDistribution\" * only for Ugarch
-\"GeneralizedHyperbolic\" * only for Ugarch"
IncludeMean::usage="IncludeMean->False

This flag determines if the parameter for the mean will be estimated or not.
If IncludeMean->True this will be the case,otherwise the parameter will be kept fixed during the process
of parameter optimization."
Algorithm::usage="Algorithm->\"nlminb\"

A string option that determines the numerical optimization algorithm used for maximum likelihood estimation.
**********************************************************************************
                  Posibile options for Garch function:                                               
**********************************************************************************
-\"nlminb\" - Unconstrained and box-constrained optimization using PORT routines.
For further reference wisit: http://netlib.bell-labs.com/netlib/port/

-\"lbfgsb\" - Low-storage version of the Broyden-Fletcher-Goldfarb-Shanno (BFGS) method.
For further reference wisit: http://ab-initio.mit.edu/nlopt

-\"nlminb+nm\" - nlminb plus a second step (finishing) optimization using the Nelder-Mead from R optim package

-\"lbfgsb+nm\" - lbfgsb plus a second step (finishing) optimization using the Nelder-Mead from R optim package


**********************************************************************************
                  Posibile options for Ngarch function:                                             
**********************************************************************************
-\"nlminb\"

-\"NelderMead\"

-\"solnp\"

-\"CG\"

-\"BFGS\""
ImageSize::usage="ImageSize->Automatic
Option for choosing size of image for Garch and Ugarch function when method (third argument) is \"plot\""
Leverage::usage="Leverage->Null

A logical flag for APARCH models in Garch function. Should the model be leveraged?
By default Leverage=Null,
which means that depending of taken model (garch or aparch),
this value will be true or false by default"
Gamma::usage="Gamma->0

APARCH leverage parameter entering into the formula for calculating the expectation value."
EstimationMethod::usage="EstimationMethod->\"IndependentComponentAnalysis\"

A string option that determines the method used in estimating parameters of Ogarch model
**********************************************************************************
					*Awailible estimation methods:*
**********************************************************************************
- \"IndependentComponentAnalysis\"
- \"MaximumLikelihood\"
- \"NonLinearLeastSquares\"
- \"MethodOfMoments\"
"
IncludeDelta::usage="IncludeDelta->Null

A logical flag which determines if the parameter for the recursion equation delta will be estimated or not.
If IncludeDelta->False then the shape parameter will be kept fixed during the process of parameter optimization."
IncludeSkew::usage="IncludeSkew->Null

A logical flag which determines if the parameter for the skewness of the conditional distribution will be estimated or not.
If IncludeSkew->False then the skewness parameter will be kept fixed during the process of parameter optimization."
IncludeShape::usage="IncludeShape->Null

A logical flag which determines if the parameter for the shape of the conditional distribution will be estimated or not.
If IncludeShape->False then the shape parameter will be kept fixed during the process of parameter optimization."
Delta::usage="Delta->2

A numeric value, the exponent delta of the variance recursion.
By default, this value will be fixed, otherwise the exponent will be estimated 
together with the other model parameters if IncludeDelta->False"
Skew::usage="Skew->1

A numeric value, the skewness parameter of the conditional distribution."
Shape::usage="Shape->4

A numeric value, the shape parameter of the conditional distribution."
ScaleModel::usage="ScaleModel->0

Ugarch option for scaling when solver fails to converge.Valuable because posibility of gaining log likelihood even when scaling"
IncludeMeanInArma::usage="IncludeMeanInArma->False

Options that determines weather we use (estimate) mean 
in arma process parameter estimation or not. Default value is False"
VarianceTargeting::usage="VarianceTargeting->False

Should we use variance targeting or not?It is also possible to pass a numeric value instead of a logical, in which case it is used for the
calculation instead of the variance of the conditional mean equation residuals"
Solver::usage="Solver->\"solnp\" 

*********************************************************************************
                Avaible solvers for optimization in Ugarch function:
*********************************************************************************
\"nlminb\" - Unconstrained and box-constrained optimization
   using PORT routines.The PORT documentation is at http://netlib.bell-labs.com/cm/cs/cstr/153.pdf

\"solnp\" - General nonlinear augmented Lagrange multiplier method solver (SQP based solver)

\"lbfgs\" - Low-storage version of the Broyden-Fletcher-Goldfarb-Shanno (BFGS) method from optim solver

\"gosolnp\" - The \"gosolnp\" solver allows for the initialization of multiple restarts of the solnp
   solver with randomly generated parameters

\"nloptr\" - A free/open-source library for nonlinear optimization started by Steven G. Johnson
  For further details visit http://ab-initio.mit.edu/wiki/index.php/NLopt

\"hybrid - The \"hybrid\" strategy solver first tries the \"solnp\" solver, in failing to converge
  then tries then \"nlminb\", the \"gosolnp\" and finally the \"nloptr\" solvers\"
  *******************************************************************************"
FixedParameters::usage="FixedParameters->Null

A string where we define parameters that we want to be fixed during estimation,such as skew,shape,beta1,alpha1,alpha2...
i.e.

FixedParameters->\"skew=1,shape=4,delta=2\"

Where delta is a power parameter for aparch model.This is the option we want to use if we want to estimate EWMA:

FixedParameters->\"omega=0,alpha1=0.96,beta1=0.04\""
StartParameters::usage="StartParameters->Null

List of staring parameters for the optimization routine.
These are not usually required unless the optimization has problems converging.
Example of input:
StartParameters->\"omega=0.000001,alpha1=0.2,beta1=0.7\""
OutSample::usage="OutSample->0

A positive integer indicating the number of periods before the last to keep for out of sample forecasting"
VExtRegressor::usage="VExtRegressor->Null

A matrix object containing the external regressors to include in the variance equation with as many rows as
 will be included in the data."
Model::usage="Model->\"Diagonal\"

A character string describing the model.\"Diagonal\" for the diagonal model and \"Extended\" for the extended
(full ARCH and GARCH parameter matrices) model"
Algorithm1::usage="Algorithm1 -> \"BFGS\"

A character string specifying the optimisation method for the first stage of DCCgarch fitting in optim (underlinghning solver).
There are three choices, namely, \"NelderMead\",\"BFGS\" (default) and \"ConjugateGradients\"  method."
Algorithm2::usage="Algorithm2 -> \"BFGS\"

A character string that determines the optimisation algorithm in the second stage optimisation.
By default \"BFGS\" is invokded, otherwise \"NelderMead\" can be used."
InitOmega::usage="InitOmega -> Null

A list of initial values for the constants in the GARCH equation length[InitOmega]=N.
Assumed values are 0.00001,in case of non convergence
we can change them by entering a list i.e.
{omega1,omega2,omega3,..,omegaN}"
InitBeta::usage="InitBeta -> Null

A list of initial values for the GARCH parameter
Assumed values are 0.2,in case of non convergence
we can change them by entering a list i.e.
{beta1,beta2,beta3,..,betaN}"
InitAlpha::usage="InitAlpha -> Null

A list of initial values for the GARCH parameter
Assumed values are 0.6,in case of non convergence
we can change them by entering a list i.e.
{alpha1,alpha2,alpha3,..,alphaN}"
InitDCC::usage="InitDCC -> {0.2, 0.6}

A list of initial values for the DCC parameters"
ScaleData::usage="ScaleData->100

Option in Ngarch function for scaling returns if we have convergence issues"
QProb::usage="QProb->0.01

Option value when using method \"quantile\""
MExtRegressor::usage="MExtRegressor -> Null

A matrix object containing the external regressors to include in the mean equation with as many rows as
 will be included in the data."
RollFrame::usage="RollFrame->0

Optional argument indicating the rolling sequence to plot."
Summary::usage="Summary->True"
NAhead::usage="NAhead->10
Garch option for \"forecast\" method.
An integer value, denoting the number of steps to be forecasted, by default 10."
mse::usage="mse->\"cond\"
Garch option for \"forecast\" method.
If set to \"cond\", mean Error is defined as the conditional mean errors Sqrt[Subscript[E, t] [Subscript[x, t+h]-Subscript[E, t](Subscript[x, t+h])]^2]  ,
If set to \"uncond\", it is defined as Sqrt[E [Subscript[x, t+h]-Subscript[E, t](Subscript[x, t+h])]^2] "

Begin["`Private`"]

(*define options*)
Options[Garch]={
	ConditionalDistribution->"NormalDistribution",
	IncludeMean->False,Algorithm->"nlminb",Leverage->Null,Gamma->0,
	IncludeDelta ->Null,IncludeSkew->Null,IncludeShape->Null,
	Delta->2,Skew->1,Shape->4,ImageSize->Automatic,NAhead->10,mse->"cond"
	}
	
Garch[
	data_List,formula_String:"garch(1,1)",method_String:"coef",OptionsPattern[]
	]:=
	Module[
		{
		 dist1,x0=data,dates,return,formula0=formula,method0=method,
		 l,getRPlot,im,lev,incdel,incshp,incskew,skew,delta,shape
		},
		If[
			Length@Dimensions[x0]==1\[Or]Dimensions[x0][[2]]==1,
			
			x0={Table[i,{i,Length[x0]}],Flatten@x0}//Transpose;
			dates=ToString/@x0[[All,1]];
			return=N[x0[[All,2]]],
			
			dates=Table[DateString[x0[[i,1]],{"Year","-","Month","-","Day"}],{i,1,Length[x0[[All,1]]]}];
			return=N[x0[[All,2]]]
		  ];(*this allow us to use returns or dates + returns*)
		return=N@return;
		REvaluate["remove(fit,fitted,date,return,ll)"];
		RSet["date",dates];
		RSet["return",return];
		(*translating options in R*)
		(*options for conditional distribution*)
		dist1=
		If[OptionValue[Garch,ConditionalDistribution]=="NormalDistribution","norm",
			If[OptionValue[Garch,ConditionalDistribution]=="StudentTDistribution","std",
				If[OptionValue[Garch,ConditionalDistribution]=="SkewNormalDistribution","snorm",
					If[OptionValue[Garch,ConditionalDistribution]=="SkewStudentTDistribution","sstd",
						If[OptionValue[Garch,ConditionalDistribution]=="Erf","ged",
							If[OptionValue[Garch,ConditionalDistribution]=="SkewErf","sged",
								If[OptionValue[Garch,ConditionalDistribution]=="NormalInverseGaussianDistribution","snig",
									If[OptionValue[Garch,ConditionalDistribution]=="QMLE","QMLE",Print[Style["Wrong conditional distribution option value,
									assuming normal conditional distribution.
									See '?ConditionalDistribution' for avaible distributions",Red]];"norm"
										]
								   ]
							   ]
							]
						]
					]
				]
			];
		
		im =If[OptionValue[Garch,IncludeMean]==False,"F","T"];
		
		lev=If[OptionValue[Garch,Leverage]==False,"F","T","NULL"];
		
		incskew=If[OptionValue[Garch,IncludeSkew]===Null,"NULL",If[OptionValue[Garch,IncludeSkew]==True,"T","F","NULL"],"NULL"];
		incdel=If[OptionValue[Garch,IncludeDelta]===Null,"NULL",If[OptionValue[Garch,IncludeDelta]==True,"T","F","NULL"],"NULL"];
		incshp=If[OptionValue[Garch,IncludeShape]===Null,"NULL",If[OptionValue[Garch,IncludeShape]==True,"T","F","NULL"],"NULL"];
		delta=OptionValue[Garch,Delta];
		skew=OptionValue[Garch,Skew];
		shape=OptionValue[Garch,Shape];
		
		
		(*Create a Mathematica counterpart wrapper that automates file import and adds error-checking.*)
		mathematicaRPlotWrapper=RFunction["function(filename, plotfun){
		     pdf(filename,useDingbats=F)
		     plotfun()
		     dev.off()
		     }"];
		(*The following creates a generic R wrapper that creates
		 the plot and saves it to a file,and defines certain parameters 
		for the resulting image.*)
		getRPlot[plotFun_RFunction]:=With[{tempfile=FileNameJoin[{$TemporaryDirectory,"temp.pdf"}]},If[FileExistsQ[tempfile],DeleteFile[tempfile]];
		mathematicaRPlotWrapper[tempfile,plotFun];
		If[!FileExistsQ[tempfile],Return[$Failed]];
		Import[tempfile]];
		
		l={"1:Time Series","2:Conditional SD","3:Series with 2 Conditional SD Superimposed ",
			"4:ACF of Observations","5:ACF of Squared Observations","6:Cross Correlation",
			"7:Residuals","8:Conditional SDs","9:Standardized Residuals","10:ACF of Standardized Residuals",
			"11:ACF of Squared Standardized Residuals","12:Cross Correlation between r^2 and r",
			"13:QQ-Plot of Standardized Residuals"};
		
		REvaluate["{
		return=as.matrix(return);
		rownames(return)=date;
		fit=garchFit(
			return,
			formula=~"<>formula0<>",
			include.mean="<>im<>",
			cond.dist=\""<>dist1<>"\",
			leverage="<>lev<>",
			algorithm =\""<>OptionValue[Garch,Algorithm]<>"\",
			delta="<>ToString@delta<>",
			skew="<>ToString@skew<>",
			shape="<>ToString@shape<>",
			include.delta="<>incdel<>",
			include.shape="<>incshp<>",
			include.skew="<>incskew<>",
			gamma="<>ToString@OptionValue[Garch,Gamma]<>");
		}"];
		If[method0=="coef",REvaluate["as.data.frame(fit@fit$matcoef)"]//TableForm
			(*{Flatten@RGetRowNames@REvaluate["as.data.frame(coef(fit))"],Flatten@RGetData@REvaluate["as.data.frame(coef(fit))"]}\[Transpose]*),
		If[method0=="matcoef",REvaluate["as.data.frame(fit@fit$matcoef)"]//TableForm,	
		If[method0=="residuals",{Flatten@REvaluate["as.matrix(date)"],Flatten@RGetData@REvaluate["as.data.frame(residuals(fit))"]}\[Transpose],
		If[method0=="fitted",{Flatten@RGetRowNames@REvaluate["as.data.frame(fit@fitted)"],Flatten@RGetData@REvaluate["as.data.frame(fit@fitted)"]}\[Transpose],
		If[method0=="volatility",{Flatten@REvaluate["as.matrix(date)"],Flatten@RGetData@REvaluate["as.data.frame(fit@sigma.t)"]}\[Transpose],
		If[method0=="forecast",REvaluate["as.data.frame(predict(fit,n.ahead="<>ToString@OptionValue[Garch,NAhead]<>",mse=\""<>ToString@OptionValue[Garch,mse]<>"\"))"]//TableForm,
		If[method0=="plot",Quiet@Panel@TabView[
		            Table[RSet["x", x]; 
		             l[[x]]->Show[getRPlot[
		               RFunction["function(){plot(fit,which=x)}"]], 
		              ImageSize->OptionValue[Garch,ImageSize], PlotRange -> All], {x, 1, 13}],ControlPlacement->Left],
		If[method0=="ll",REvaluate["{ll=fit@fit$llh;names(ll)=NULL;ll*(-1)}"][[1]],
		If[method0=="summary",Framed@StringJoin@Riffle[#,"\n"]&@REvaluate["capture.output(summary(fit))"],
		If[method0=="all",{
		{Flatten@RGetRowNames@REvaluate["as.data.frame(coef(fit))"],Flatten@RGetData@REvaluate["as.data.frame(coef(fit))"]}\[Transpose],
		{Flatten@REvaluate["as.matrix(date)"],Flatten@RGetData@REvaluate["as.data.frame(residuals(fit))"]}\[Transpose],
		{Flatten@RGetRowNames@REvaluate["as.data.frame(fit@fitted)"],Flatten@RGetData@REvaluate["as.data.frame(fit@fitted)"]}\[Transpose],
		{Flatten@REvaluate["as.matrix(date)"],Flatten@RGetData@REvaluate["as.data.frame(fit@sigma.t)"]}\[Transpose],
		{REvaluate["as.data.frame(predict(fit,n.ahead="<>ToString@OptionValue[Garch,NAhead]<>",mse=\""<>ToString@OptionValue[Garch,mse]<>"\"))"]//TableForm},
		REvaluate["{ll=fit@fit$llh;names(ll)=NULL;ll*(-1)}"][[1]]
		},
		"Wrong input on position 3.
		Input must be one of the following:
		-\"coef\"
		-\"matcoef\"
		-\"residuals\"
		-\"volatility\"
		-\"summary\"
		-\"fitted\"
		-\"forecast\"
		-\"ll\"
		-\"all\""]]]]]]]]]]]
		
		(*Ngarch*)
		Options[Ngarch]={ConditionalDistribution->"NormalDistribution",Algorithm->"nlminb",ScaleData->100}
		
		Ngarch[ret0_, impvolatility_: 0, value0_String: "coef", 
		  OptionsPattern[]] := 
		 Module[{Ngarch, scale = ToString@OptionValue[Ngarch, ScaleData], 
		   dist = ToString@OptionValue[Ngarch, ConditionalDistribution], 
		   ret = ret0, value = value0, dates, retrn, impvol = impvolatility,
		   alg = ToString@OptionValue[Ngarch, Algorithm]},
		  If[Length@Dimensions[ret] == 1 \[Or] Dimensions[ret][[2]] == 1, 
		   ret = {Table[i, {i, Length[ret]}], Flatten@ret}\[Transpose];
		   dates[b_] := ToString /@ b[[All, 1]];
		   retrn[b_] := b[[All, 2]],
		   dates[b_] := 
		    Table[DateString[
		      b[[i, 1]], {"Year", "-", "Month", "-", "Day"}], {i, 1, 
		      Length[b[[All, 1]]]}];
		   retrn[b_] := b[[All, 2]]];
		  If[alg == "NelderMead", alg = "Nelder-Mead", alg = alg];
		  (*ovo dolje je nezavisna funkcija i moze se koristiti sama po sebi,
		  ali uslozavam abog outputa u kome treba da se nalaze i datumi za \
		metod residuals i volatility*)
		  Ngarch := 
		   RFunction[
		    "function(rtn" <> If[ListQ@impvol == False, ",Null", ",impvol"] <>
		      ",value=\"" <> value <> "\"){
		     # Estimation of a non-symmertic GARCH-Ngarch(1,1),model.
		     # Assume normal distribution
		     # rtn-return series
		     # The likelihood function mxlk can be modified to fit more \
		general GARCH models.
		     # obtain initial estimates
		     scale=" <> scale <> "
		     mu=mean(rtn)
		     rtn=rtn-mu
		     rtn=scale*rtn
		     coef=c(0.0001,0.8,0.01,0.7" <> 
		     If[ListQ@impvol == False, ",NULL", ",0.07"] <> ")
		     u=length(coef)
		     " <> 
		     If[dist == "StudentTDistribution", "coef[u+1]=5", 
		      If[dist == "SkewStudentTDistribution", "coef[u+1]=5;
		       coef[u+2]=1.5;", 
		       If[dist == "SkewNormalDistribution", "coef[u+1]=1,5;", 
		        "NULL"]]] <> "
		     mxlk<-function(coef){
		     mxlk=0
		     sigma=var(rtn)
		     T=length(rtn)
		     if(T>40)sigma=var(rtn[1:40])
		     at=rtn-mu
		     for(i in 2:T){
		     sig2t=coef[1]+coef[2]*sigma[i-1]+coef[3]*sigma[i-1]*(at[i-1]/\
		sqrt(sigma[i-1])-coef[4])^2" <> 
		     If[ListQ@impvol == False, "+0", 
		      "+coef[5]*((impvol[i-1])^2)/252"] <> "
		     " <> 
		     If[dist == 
		        "NormalDistribution" && (alg == "Nelder-Mead" || 
		         alg == "solnp" || alg == "nlminb"), "
		      sigma[i]=sig2t
		      mxlk=mxlk+0.5*(1.8378770664093453+log(sig2t)+(at[i])^2/sigma[i])}
		      mxlk}", 
		      If[dist == 
		         "StudentTDistribution" && (alg == "Nelder-Mead" || 
		          alg == "solnp" || alg == "nlminb"), "
		       sigma[i]=sig2t
		       mxlk=mxlk-log(dstd(at[i],mean=0,sd=sqrt(sig2t),nu=coef[u+1]))}
		       mxlk}", 
		       If[dist == 
		          "SkewStudentTDistribution" && (alg == "Nelder-Mead" || 
		           alg == "solnp" || alg == "nlminb"), "sigma[i]=sig2t
		        mxlk=mxlk-log(dsstd(at[i],mean=0,sd=sqrt(sig2t),nu=coef[u+1],\
		xi=coef[u+2]))}
		        mxlk}", 
		        If[dist == 
		           "SkewNormalDistribution" && (alg == "Nelder-Mead" || 
		            alg == "solnp" || alg == "nlminb"), "sigma[i]=sig2t
		         mxlk=mxlk-log(dsnorm(at[i],mean=0,sd=sqrt(sig2t),xi=coef[u+1]\
		))}
		         mxlk}", 
		         If[dist == 
		            "NormalDistribution" && (alg == "BFGS" || alg == "CG"), 
		          "sig2t=abs(sig2t)
		          sigma[i]=sig2t
		          mxlk=mxlk+0.5*(1.8378770664093453+log(sig2t)+(at[i])^2/\
		sigma[i])}
		          mxlk}", 
		          If[dist == 
		             "SkewNormalDistribution" && (alg == "BFGS" || 
		              alg == "CG"), "sig2t=abs(sig2t)
		           sigma[i]=sig2t
		           mxlk=mxlk-log(dsnorm(at[i],mean=0,sd=sqrt(sig2t),xi=coef[u+\
		1]))
		           mxlk}", 
		           If[dist == 
		              "StudentTDistribution" && (alg == "BFGS" || 
		               alg == "CG"), "
		            sig2t=abs(sig2t)
		            sigma[i]=sig2t
		            mxlk=mxlk-log(dstd(at[i],mean=0,sd=sqrt(sig2t),nu=coef[u+\
		1]))}
		            mxlk}", 
		            If[dist == 
		               "SkewStudentTDistribution" && (alg == "BFGS" || 
		                alg == "CG"), "
		             sig2t=abs(sig2t)
		             sigma[i]=sig2t
		             mxlk=mxlk-log(dsstd(at[i],mean=0,sd=sqrt(sig2t),nu=coef[\
		u+1],xi=coef(u+2)))}
		             mxlk}"]]]]]]]] <> "
		     mm=" <>
		     If[alg == "Nelder-Mead" ||
		       	alg == "BFGS" ||
		       	alg == "CG", 
		      "optim(coef,mxlk,method=\"" <> alg <> "\",hessian=T)",
		      	If[alg == "solnp", "solnp(coef,mxlk)",
		       	If[alg == "nlminb", 
		        "nlminb(coef,mxlk,hessian=T,lower=c(0,0,0,0))", 
		        Print[Style["Wrong option value for Algorithm, assuming \"nlminb\",
		         availible algorithms are:
		         -\"nlminb\"
		         -\"NelderMead\"
		         -\"BFGS\"
		         -\"CG\"",Red]]; "nlminb(coef,mxlk,hessian=T)"]]] <> "
		     ## Print the results
		     coef=mm$par
		     coef[1]=coef[1]/scale^2
		     rtn=rtn/scale
		     # compute the volatility series and residuals
		     sigma=var(rtn)
		     T=length(rtn)
		     if(T>40)sigma=var(rtn[1:40])
		     at=rtn
		     sig2t=0
		     residuals=c(as.numeric(rtn))
		     if(value==\"residuals\"){
		     residuals
		     } else if(value==\"volatility\"){
		     for (i in 2:T){
		     sig2t=coef[1]+coef[2]*sigma[i-1]+coef[3]*sigma[i-1]*(at[i-1]/\
		sqrt(sigma[i-1])-coef[4])^2" <> 
		     If[ListQ@impvol == False, "+0", 
		      "+coef[5]*((impvol[i-1])^2)/252"] <> "
		     sigma=c(sigma,sig2t)}
		     sigma.t=sqrt(sigma)
		     volatility=c(as.numeric(sigma.t))
		     volatility
		     } else if(value==\"coef\"){
		     cbind(c(\"omega\",\"beta\",\"alpha\",\"theta\"" <> 
		     If[ListQ@impvol == False, ",NULL", ",\"y\""] <> 
		     If[dist == "StudentTDistribution", ",\"degrees of freedom\"", 
		      ",NULL"] <> 
		     If[dist == "SkewStudentTDistribution", 
		      ",\"degrees of freedom\",\"skew\"", ",NULL"] <> 
		     If[dist == "SkewNormalDistribution", ",\"skew\"", ",NULL"] <> 
		     "),as.numeric(coef))
		     } else if(value==\"summary\"){" <> 
		     If[alg == "nlminb", "H=hessian(glkn,coef1)", "H=mm$hessian"] <> "
		     Hi=solve(H)
		     se=sqrt(diag(Hi))
		     se[1]=se[1]/scale^2
		     tra=coef/se
		     a=matrix(nrow=3,ncol=length(coef))
		     rownames(a)=c(\"Estimates\",\"StdErrors\",\"tRatio\")
		     colnames(a)=c(\"omega\",\"beta\",\"alpha\",\"theta\"" <> 
		     If[ListQ@impvol == False, ",NULL", ",\"y\""] <>
		       If[dist == "StudentTDistribution", ",\"degrees of freedom\"", 
		      ",NULL"] <>
		       If[dist == "SkewStudentTDistribution", 
		      ",\"degrees of freedom\",\"skew\"", ",NULL"] <>
		       If[dist == "SkewNormalDistribution", ",\"skew\"", ",NULL"] <> ")
		     a[1,1:(length(coef))]=coef
		     a[2,1:(length(coef))]=se
		     a[3,1:(length(coef))]=tra
		     print(a)
		     } else if(value==\"ll\"){ll=mxlk(coef);
		     names(ll)=NULL
		     print(as.numeric(-ll))
		     }
		     }"];
		  If[value == "residuals" \[Or] value == "volatility", {dates[ret],
		     If[ListQ@impvol == False, Ngarch[retrn[ret], value], 
		      Ngarch[retrn[ret], impvol, value]]}\[Transpose],
		   If[value == "coef" \[Or] value == "summary",
		    If[ListQ@impvol == False, Ngarch[retrn[ret], value], 
		     Ngarch[retrn[ret], impvol, value]],
		    If[value == "ll",
		     If[ListQ@impvol == False, Ngarch[retrn[ret], "ll"][[1]], 
		      Ngarch[retrn[ret], impvol, "ll"][[1]]],
		     "Wrong input parameters.
		     Input at position 1 should be a list of return with or without \
		dates(as from FinancialData function).
		     Input at position 3 should be one of the folloving:
		     -\"coef\"
		     -\"volatility\"
		     -\"summary\"
		     -\"residuals\"
		     -\"ll\""]]]]
     
     
								(*Ogarch*)

Options[Ogarch]={
	ConditionalDistribution->"NormalDistribution",IncludeMean->False,
	Algorithm->"nlminb",EstimationMethod-> "IndependentComponentAnalysis",Leverage-> Null,
	IncludeDelta ->Null,IncludeSkew->Null,IncludeShape->Null,Delta->2,Skew->1,Shape->4
				}

Ogarch[
	data_List,formula_String:"garch(1,1)",method_String:"coef",date___,OptionsPattern[]]:=Module[{
			x=data,form=formula,dat=date,meth=method,
			dat1,dist1, im,em,lev,incdel,incshp,
			incskew,skew,delta,shape
		 },
		Quiet@If[ListQ[dat]==True,dat1:=Table[DateString[dat[[i]],{"Year","-","Month","-","Day"}],{i,1,Length[x]}],
		dat1:=Range@Length@x,dat1:=Range@Length@x];
		REvaluate["{
					remove(res);
					remove(m);
					remove(output);
					remove(x);
					remove(dat);}"
					];
		RSet["x",N[x]];
		RSet["dat",dat1];
		(**)
		dist1= (*options for conditional distribution*)
		If[OptionValue[Ogarch,ConditionalDistribution]=="NormalDistribution","norm",
			If[OptionValue[Ogarch,ConditionalDistribution]=="StudentTDistribution","std",
				If[OptionValue[Ogarch,ConditionalDistribution]=="SkewNormalDistribution","snorm",
					If[OptionValue[Ogarch,ConditionalDistribution]=="SkewStudentTDistribution","sstd",
						If[OptionValue[Ogarch,ConditionalDistribution]=="Erf","ged",
							If[OptionValue[Ogarch,ConditionalDistribution]=="SkewErf","sged",
								If[OptionValue[Ogarch,ConditionalDistribution]=="NormalInverseGaussianDistribution","snig",
									If[OptionValue[Ogarch,ConditionalDistribution]=="QMLE","QMLE",Print["Wrong conditional distribution option value,
										assuming normal conditional distribution.
										See '?ConditionalDistribution' for avaible distributions"];"norm"
										]
									]
								]
							]
						]
					]
				]
			];
		(*options for including mean*)
		im=If[OptionValue[Ogarch,IncludeMean]== False,"FALSE","TRUE","FALSE";Print["Wrong option value for IncludeMean option,
		assuming 'False',see '?IncludeMean' for more details about option"]];
		(*..................................................................*)
		em = If[OptionValue[Ogarch, EstimationMethod] =="IndependentComponentAnalysis", "ica",
				If[OptionValue[Ogarch, EstimationMethod] == "MaximumLikelihood","ml",
					If[OptionValue[Ogarch, EstimationMethod] == "NonLinearLeastSquares","nls",
						If[OptionValue[Ogarch, EstimationMethod] == "MethodOfMoments", "mm",
						Print[Style["Wrong option value for EstimationMethod option. Assuming \"NonLinearLeastSquares\". For more details type:
						?EstimationMethod",Red]];"nls"]]]];
		lev = If[OptionValue[Ogarch,Leverage]==False,"F","T","NULL"];
		incskew=If[OptionValue[Ogarch,IncludeSkew]==False,"F","T","NULL"];
		incdel=If[OptionValue[Ogarch,IncludeDelta]==False,"F","T","NULL"];
		incshp=If[OptionValue[Ogarch,IncludeShape]==False,"F","T","NULL"];
		delta=OptionValue[Ogarch,Delta];
		skew=OptionValue[Ogarch,Skew];
		shape=OptionValue[Ogarch,Shape];
		(* untill here was the option assighnment*)

		REvaluate["{
		rownames(x)=dat;
		colnames(x)=c(\"1\",2:length(x[1,]));
		res=gogarch(x,formula=~"<>form<>",estby=\""<>em<>"\",
		garchlist=list(leverage="<>lev<>",include.mean="<>im<>",
		cond.dist=\""<>dist1<>"\",algorithm=\""<>OptionValue[Ogarch,Algorithm]<>"\",
		delta="<>ToString@delta<>",skew="<>ToString@skew<>",shape="<>ToString@shape<>",
		include.delta="<>incdel<>",include.shape="<>incshp<>",include.skew="<>incskew<>"));
		m=as.data.frame("<>If[meth=="coef"\[Or]meth=="residuals"\[Or]meth=="cvar"\[Or]meth=="ccor"\[Or]meth=="ccov",meth,"coef"]<>"(res));}"];
		
		If[meth=="residuals"\[Or]meth=="cvar"\[Or]meth=="ccor"\[Or]meth=="ccov",
		   Prepend[Prepend[RGetData@REvaluate["m"]\[Transpose],RGetNames@REvaluate["m"]]\[Transpose],
		   Prepend[REvaluate["rownames(x)"],""]]\[Transpose],
				If[meth=="coef",Prepend[Prepend[RGetData@REvaluate["m"]\[Transpose],RGetNames@REvaluate["m"]]\[Transpose],
					Prepend[REvaluate["rownames(m)"],""]]\[Transpose],
					If[meth=="summary",Framed@StringJoin@Riffle[#,"\n"]&@REvaluate["capture.output(summary(res))"],
						If[meth=="plot",Quiet@Show[getRPlot[RFunction["function(){plot(res)}"]],
							ImageSize->Large,PlotRange->All],
							If[meth=="ll"\[And]em=="ml",RGetData@REvaluate["as.data.frame(logLik(res))"],
								If[meth=="ll"\[And]em!="ml","method \"ll\" can only be chosen with folowing option:
									EstimationMethod->\"MaximumLikelihood\"",
									"Wrong input parameters at position 3.
									Input should be one of the following:
									-\"coef\"
									-\"residuals\"
									-\"cvar\"
									-\"ccor\"
									-\"ccov\"
									-\"summary\"
									-\"plot\"
									-\"ll\""
									]
								]
							]
						]
					]
				]
			]

(*************************Ugarch************************************)
Options[Ugarch] := {ConditionalDistribution -> "NormalDistribution", 
  					IncludeMeanInArma-> False, VarianceTargeting -> False, 
					Solver -> "solnp",FixedParameters->Null,ImageSize->Automatic,
				    ScaleModel->0,OutSample->0,StartParameters->Null,
				    VExtRegressor->Null,QProb->{0.01},MExtRegressor -> Null}

Ugarch[data_List, 
	   formula_String:"garch(1,1)", 
	   method_String:"coef", 
	   OptionsPattern[]] :=
	    Module[{dist, x0 = data, dates, return, formula0 = formula, 
			   method0 = method, mathematicaRPlotWrapper, getRPlot, im, l, 
			   garchOrder, submodel, armaOrder, vt, sol,fp,Outsample,sp,vxreg,quant,mxreg},
			   (*if two columns then first is date column*)
			  If[Length@Dimensions[x0] == 1 \[Or] Dimensions[x0][[2]] == 1, 
			   x0 = {Table[i, {i, Length[x0]}], Flatten@x0}//Transpose;
			   dates = ToString /@ x0[[All, 1]];
			   return = x0[[All, 2]]//N,
			   dates = Table[DateString[x0[[i, 1]], {"Year", "-", "Month", "-", "Day"}], {i, 1,Length[x0[[All, 1]]]}];
			   return = x0[[All, 2]]//N];(*this allow us to use returns or dates + returns*)
			  REvaluate["remove(date,return,fitted,vxreg,out.sample,quant,mxreg,spec)"];
			  RSet["date", dates];
			  RSet["return", return];
			  (*external regressors optional input*)
			 vxreg=OptionValue[Ugarch,VExtRegressor];
			 If[vxreg===Null,REvaluate["vxreg=NULL"],
			 	If[
			 		If[ListQ@vxreg==True && Length@Dimensions[vxreg]>1,
			 			Dimensions[vxreg][[1]],Dimensions[vxreg]
					]==Length[return],
			 		Evaluate@RSet["vxreg",vxreg];REvaluate["vxreg=as.matrix(vxreg)"],
			 		Print[Style["External regressors should be the matrix with same number of rows as time series",Red]];Abort[];
			 		]
			 		];
			 mxreg=OptionValue[Ugarch,MExtRegressor];
			 If[mxreg===Null,REvaluate["mxreg=NULL"],
			 	If[
			 		If[
			 			ListQ@Dimensions[mxreg]==True,
			 			Dimensions[mxreg][[1]],Dimensions[mxreg]]==Length[return],
			 		Evaluate@RSet["mxreg",mxreg];REvaluate["mxreg=as.matrix(mxreg)"],
			 		Print[Style["External regressors should be the matrix with same number of rows as time series",Red]];Abort[];
			 		]
			 		];		
			  (********************************options*************************)
			  
			                       (*conditional distribution*)
			  dist = If[
					    OptionValue[Ugarch, ConditionalDistribution] == 
					     "NormalDistribution", "norm", 
					    If[OptionValue[Ugarch, ConditionalDistribution] == 
					      "StudentTDistribution", "std", 
					     If[OptionValue[Ugarch, ConditionalDistribution] == 
					       "SkewNormalDistribution", "snorm",
					      If[OptionValue[Ugarch, ConditionalDistribution] == 
					        "SkewStudentTDistribution", "sstd",
					       If[OptionValue[Ugarch, ConditionalDistribution] == 
					       	"Erf", "ged",
					        If[OptionValue[Ugarch, ConditionalDistribution] ==
					        	 "SkewErf","sged",
					         If[OptionValue[Ugarch, ConditionalDistribution] == 
					           "NormalInverseGaussianDistribution", "nig",
					          If[OptionValue[Ugarch, ConditionalDistribution] == 
					            "JohnsonSUDistribution","jsu",
					           If[OptionValue[Ugarch, ConditionalDistribution] == 
					             "GeneralizedHyperbolic","ghyp",
					            Print[Style["Wrong distribution input,type 
					?ConditionalDistribution 
					for available distributions,assuming NormalDistribution",Red]];"norm"
								  ]
								 ]
								]
					   		   ]
		   			 		  ]
							 ]
							]
						   ]
       					  ];
			
			                  (*Include mean in arma when  modeling mean option*)
			  im = If[OptionValue[Ugarch,IncludeMeanInArma] == False, "F", "T",
			  		  Print[Style["Wrong option value for IncludeMeanInArma option (it should be True or False)
			  		  ,assuming False",Red]];"F"];
			                    	
			                    	(*variance targeting*)
			  vt = ToString@If[OptionValue[Ugarch,VarianceTargeting] == False,
			  	 			  "F", "T",
			  	 			  If[NumberQ@OptionValue[Ugarch,VarianceTargeting]==True,OptionValue[Ugarch,VarianceTargeting],
			  	 				Print[Style["Variance Targeting option should have value True or False,calculation will proceed assuming False",Red]];"F"]];
			                       
			                        (*Fixed parameters option*)
			  fp = If[OptionValue[Ugarch, FixedParameters] ===Null, "NULL", 
			  OptionValue[Ugarch, FixedParameters], "NULL"];
			  
			  						(*Starting parameters-initial parameters values for solver*)
			  sp = If[OptionValue[Ugarch, StartParameters] ===Null, "NULL", 
			  OptionValue[Ugarch, StartParameters], "NULL"];                    
			                      
			                       (*OutSample option*)
			Outsample=If[NumberQ@OptionValue[Ugarch,OutSample]==True&&OptionValue[Ugarch,OutSample]>=0,
				RSet["out.sample",OptionValue[Ugarch,OutSample]],
				Print[Style["OutSample should be a number that's >=0",Red]];0,
				Print[Style["OutSample should be a number that's >=0",Red]];0];                  
			                       
			                        (*availible solver options*)
			sol = If[OptionValue[Ugarch, Solver] == "nlminb" \[Or]
			     OptionValue[Ugarch, Solver] == "solnp" \[Or]
			     OptionValue[Ugarch, Solver] == "lbfgs" \[Or]
			     OptionValue[Ugarch, Solver] == "gosolnp" \[Or]
			     OptionValue[Ugarch, Solver] == "nloptr" \[Or]
			     OptionValue[Ugarch, Solver] == "hybrid",
			     OptionValue[Ugarch, Solver],
			     Print[Style["Wrong option value for solver,type:
			?Solver 
			For available solvers in ugarch,assuming \"solnp\" for further 
			calculation",Red]];"solnp"];
			 							(*QProb*)
			 quant=If[VectorQ[OptionValue[Ugarch,QProb],#>0&]==True&&
			 	VectorQ[OptionValue[Ugarch,QProb]]==True&&
			 	VectorQ[OptionValue[Ugarch,QProb],#<1&]==True&&
			 	VectorQ[OptionValue[Ugarch,QProb],NumericQ]==True,OptionValue[Ugarch,QProb],
			 	Print[Style["Wrong input value for QProb option,input should be a list
			 	 with one or more numeric elements that are smaler than one",Red]];Abort[]];
			 RSet["quant",quant];
			 REvaluate["quant=as.numeric(quant)"];
			  (*.....................plot ............................................*)
			  
			  mathematicaRPlotWrapper = RFunction["function(filename, plotfun){
			         pdf(filename,useDingbats=F)
			         plotfun()
			         dev.off()
			         }"];
			  (*The following creates a generic R wrapper that creates the plot \
			and saves it to a file,
			  and defines certain parameters for the resulting image.*)
			  getRPlot[plotFun_RFunction] := 
			   With[{tempfile = FileNameJoin[{$TemporaryDirectory, "temp.pdf"}]}, 
			    If[FileExistsQ[tempfile],DeleteFile[tempfile]];
			    mathematicaRPlotWrapper[tempfile, plotFun];
			    If[! FileExistsQ[tempfile], Return[$Failed]];
			    Import[tempfile]];
			  (*Create a Mathematica counterpart wrapper that automates file \
			import and adds error-checking.*)
			  
			  l = {"1:Series with 2 Conditional SD Superimposed ",
			     "2:Series with 1% VaR Limits",
			     "3:Conditional SD (vs |returns|)",
			     "4:ACF of Observations",
			     "5:ACF of Squared Observations",
			     "6:ACF of Absolute Observations",
			     "7:Cross Correlation",
			     "8:Empirical Density of Standardized Residuals",
			     "9:QQ-Plot of Standardized Residuals",
			     "10:ACF of Standardized Residuals",
			     "11:ACF of Squared Standardized Residuals",
			     "12:News-Impact Curve"};
			                       
			                         (*Creating string input at position two to look like Garch function*)
			  If[StringTake[formula0, 5] != "arma(", armaOrder = "(0,0)"; 
			   If[StringDrop[formula0, -5] == "garch", 
			    garchOrder = StringTake[formula0, -5]; 
			    submodel = "NULL"; 
			    formula0 = "sGARCH",
			    If[StringDrop[formula0, -5] == "eGARCH" \[Or] 
			      StringDrop[formula0, -5] == "gjrGARCH" \[Or] 
			      StringDrop[formula0, -5] == "apARCH" \[Or] 
			      StringDrop[formula0, -5] == "iGARCH" \[Or] 
			      StringDrop[formula0, -5] == "csGARCH", 
			      garchOrder = StringTake[formula0, -5];
			      submodel = "NULL"; 
			      formula0 = StringDrop[formula0, -5],
			     If[StringDrop[formula0, -5] == "GARCH" \[Or] 
			       StringDrop[formula0, -5] == "TGARCH" \[Or] 
			       StringDrop[formula0, -5] == "AVGARCH" \[Or] 
			       StringDrop[formula0, -5] == "NGARCH" \[Or] 
			       StringDrop[formula0, -5] == "NAGARCH" \[Or] 
			       StringDrop[formula0, -5] == "APARCH" \[Or] 
			       StringDrop[formula0, -5] == "GJRGARCH" \[Or] 
			       StringDrop[formula0, -5] == "ALLGARCH",
			       garchOrder = StringTake[formula0, -5]; 
			       submodel = StringDrop[formula0, -5]; 
			       formula0 = "fGARCH",
			       Print[Style["Wrong model,please write one of following at the position 2:
			**********************************************************
			\"garch\"
			\"eGARCH\"
			\"gjrGARCH\"
			\"apARCH\"
			\"iGARCH\"
			\"csGARCH\"
			\"GARCH\"
			\"TGARCH\"	
			\"AVGARCH\"
			\"NGARCH\"
			\"NAGARCH\"
			\"APARCH\"
			\"GJRGARCH\"
			\"ALLGARCH\"
			**********************************************************
			with lags in parenthasies ie \"garch(1,3)\" or if you also \
			want to model mean,then enter arma(lag1,lag2)+ before garch model e.g.
			         \"arma(1,2)+garch(1,3)\"",Red]];Abort[]]]],
			   If[StringTake[formula0, 5] == "arma(", 
			    armaOrder = StringTake[formula0, {5, 9}]; 
			    If[StringTake[formula0, {11, -6}] == "garch", 
			     garchOrder = StringTake[formula0, -5]; submodel = "NULL"; 
			     formula0 = "sGARCH",
			     If[StringTake[formula0, {11, -6}] == "eGARCH" \[Or] 
			       StringTake[formula0, {11, -6}] == "gjrGARCH" \[Or] 
			       StringTake[formula0, {11, -6}] == "apARCH" \[Or] 
			       StringTake[formula0, {11, -6}] == "iGARCH" \[Or] 
			       StringTake[formula0, {11, -6}] == "csGARCH",
			       garchOrder = StringTake[formula0, -5]; submodel = "NULL"; 
			       formula0 = StringTake[formula0, {11, -6}],
			       If[StringTake[formula0, {11, -6}] == "GARCH" \[Or] 
			        StringTake[formula0, {11, -6}] == "TGARCH" \[Or] 
			        StringTake[formula0, {11, -6}] == "AVGARCH" \[Or] 
			        StringTake[formula0, {11, -6}] == "NGARCH" \[Or] 
			        StringTake[formula0, {11, -6}] == "NAGARCH" \[Or] 
			        StringTake[formula0, {11, -6}] == "APARCH" \[Or] 
			        StringTake[formula0, {11, -6}] == "GJRGARCH" \[Or] 
			        StringTake[formula0, {11, -6}] == "ALLGARCH", 
			       	garchOrder = StringTake[formula0, -5]; 
			       	submodel = StringTake[formula0, {11, -6}]; 
			       	formula0 = "fGARCH",
			        Print[Style["Wrong model,please write one of following at the position 2:
			**********************************************************
			\"garch\"
			\"eGARCH\"
			\"gjrGARCH\"
			\"apARCH\"
			\"iGARCH\"
			\"csGARCH\"
			\"GARCH\"
			\"TGARCH\"
			\"AVGARCH\"
			\"NGARCH\"
			\"NAGARCH\"
			\"APARCH\"
			\"GJRGARCH\"
			\"ALLGARCH\"
			**********************************************************
			with lags in parenthasies ie \"garch(1,3)\" or if you also \
			want to model mean,then enter arma(lag1,lag2)+ before garch model e.g.
			         \"arma(1,2)+garch(1,3)\"",Red]];Abort[]]]]]];
			 
			  (*putting it all together in R code*)
			  REvaluate["{return=as.matrix(return);
			    rownames(return)=date;
			    spec=ugarchspec(
			    	variance.model=list(
			    		model=\"" <> formula0 <> "\",
			    		garchOrder=c" <> garchOrder <> ",
			    		submodel=\"" <> ToString@submodel <> "\",
			    		external.regressors=vxreg,
			    		variance.targeting=" <> vt <>"),
			   		mean.model=list(
			   			armaOrder=c" <> armaOrder <> ",
			   			include.mean=" <> im <> ",
			   			archm=FALSE,
			   			archpow=1,
			   			arfima=FALSE,
			   			external.regressors=mxreg,
			   			archex=FALSE),
			   		distribution.model=\"" <> dist <>"\",
			   		start.pars=list(
			   			"<>ToString@sp<>"),
			   		fixed.pars=list(
			   			"<>ToString@fp<>"));
			   			}"];
			  REvaluate["{fitted=ugarchfit(
			  spec, 
			  return, 
			  out.sample = out.sample, 
			  solver = \"" <> sol <> "\", 
			  solver.control = list(), 
			  fit.control = list(
			  stationarity = 1, 
			  fixed.se = 0, 
			  scale = "<>ToString@OptionValue[Ugarch,ScaleModel] <> ", 
			  rec.init = 'all'));}"];
			 
			               (*reading data into mathematica*)
			  If[REvaluate["coef(fitted)"]==Null,(*message for failed convergence*)
			  	Print[Style["##################################################################################  	
                Solver failed to converge,try one of following:          
**********************************************************************************
* - Use different solver or solver settings                                      *
*(change initial values in StartParameters option)                               *
**********************************************************************************
* - Scale your data                                                              *
*(you will need to scale back some parameters depending of model you have chosen)*
**********************************************************************************
* - In case that model is nested in APARCH family you can try Garch function     *
**********************************************************************************
* - Set VarianceTargeting option to different value than the default one         *
**********************************************************************************
			",Red]];Abort[],"Error! Code needs update",(*methods*)
			  	If[method0 == "coef",
			   REvaluate["as.data.frame(fitted@fit$matcoef)"]//TableForm,
			      If[method0 == "matcoef",REvaluate["as.data.frame(fitted@fit$matcoef)"]//TableForm,
			      
			      If[method0 =="residuals",REvaluate["as.data.frame(residuals(fitted))"]//TableForm(* {(Flatten@REvaluate["as.matrix(date)"])[[1;;-(Outsample+1)]], 
			      Flatten@RGetData@REvaluate["as.data.frame(residuals(fitted))"]}\[Transpose]*), 
			       
			         If[method0 =="fitted", REvaluate["as.data.frame(fitted(fitted))"]//TableForm(*{dates[[1;;-(Outsample+1)]],
			         Flatten@REvaluate["as.vector(fitted@fit$fitted.values)"]}\[Transpose]*), 
			         
			        	 If[method0 == "volatility",REvaluate["as.data.frame(sigma(fitted))"]//TableForm (*{(Flatten@REvaluate["as.matrix(date)"])[[1;;-(Outsample+1)]], 
			        	 	
			         		Flatten@RGetData@REvaluate["as.data.frame(sigma(fitted))"]}\[Transpose]*),
			         		
			         		If[method=="halflife",REvaluate["halflife(fitted)"][[1]],
			         			
			        	 	  If[method=="persistence",REvaluate["persistence(fitted)"][[1]],
			        	 	  	
			         			If[method=="uncvariance",REvaluate["uncvariance(fitted)"][[1]],
			         				
			         				If[method=="uncmean",REvaluate["uncmean(fitted)"][[1]],
			         					
			         					If[method=="quantile",REvaluate["as.data.frame(quantile(fitted,quant))"]//TableForm, 
			         						   			
			         					  If[method0 == "plot",Quiet@Panel@TabView[Table[RSet["x", x]; 	
			         					  	l[[x]]->Show[getRPlot[RFunction["function(){plot(fitted,which=x)}"]],
			         							ImageSize->OptionValue[Ugarch,ImageSize], PlotRange -> All], {x, 1, 12}],ControlPlacement->Left],
			         						If[method0 == "summary",
			         						Framed@StringJoin@Riffle[#, "\n"] &@REvaluate["capture.output(show(fitted))"],
			         						
			         							If[method0 == "ll", REvaluate["likelihood(fitted)"][[1]],
			         								
			         								If[method0=="all",{
			         									REvaluate["as.data.frame(fitted@fit$matcoef)"]//TableForm,
			         									REvaluate["as.data.frame(sigma(fitted))"]//TableForm,
			         									REvaluate["as.data.frame(residuals(fitted))"]//TableForm,
			         									REvaluate["as.data.frame(sigma(fitted))"]//TableForm,
			         									REvaluate["halflife(fitted)"][[1]],
			         									REvaluate["persistence(fitted)"][[1]],
			         									REvaluate["uncvariance(fitted)"][[1]],
			         									REvaluate["uncmean(fitted)"][[1]],
			         									REvaluate["as.data.frame(quantile(fitted,quant))"]//TableForm,
			         									REvaluate["likelihood(fitted)"][[1]],
			         									REvaluate["as.data.frame(infocriteria(fitted))"]//TableForm
			         									},
			         									If[method0=="infocriteria",REvaluate["as.data.frame(infocriteria(fitted))"]//TableForm
			         									
			         								,Print[Style[
			         "Wrong input  3.
			         Input must be one of the following:
			         -\"coef\"
			         -\"residuals\"
			         -\"volatility\"
			         -\"fitted\"
			         -\"summary\"
			         -\"halflife\"
			         -\"persistence\"
			         -\"uncvariance\"
			         -\"uncmean\"
			         -\"quantile\"
					 -\"plot\"
					 -\"ll\"
					 -\"all\"",Red]];Abort[]]]]]]]]]]]]]]]]]]
(*DCCgarch*)
Options[DCCgarch] = {Model -> "Diagonal", Algorithm1 -> "BFGS",
  Algorithm2 -> "BFGS", InitOmega -> Null,
  InitAlpha -> Null, InitBeta -> Null, InitDCC -> {0.2, 0.6}}
DCCgarch[rmat_List, method_String: "coef", OptionsPattern[]] := 
 Module[{inia1 = 
    If[ListQ@OptionValue[DCCgarch, InitOmega] == False, 
     Table[0.00001, {i, (rmat // Dimensions)[[2]]}], 
     OptionValue[DCCgarch, InitOmega]], 
   iniA1 = DiagonalMatrix[
     If[ListQ@OptionValue[DCCgarch, InitAlpha] == False, 
      Table[0.2, {i, (rmat // Dimensions)[[2]]}], 
      OptionValue[DCCgarch, InitAlpha]]],
   iniB1 = 
    DiagonalMatrix[
     If[ListQ@OptionValue[DCCgarch, InitBeta] == False, 
      Table[0.6, {i, (rmat // Dimensions)[[2]]}], 
      OptionValue[DCCgarch, InitBeta]]], 
   inidcc1 = OptionValue[DCCgarch, InitDCC], r = rmat, alg1, alg2, 
   mod},
  REvaluate["remove(a,A,B,dccpar,rmat,dccresults,cor,l,o,b)"];
  RSet["a", inia1];
  RSet["A", iniA1];
  RSet["B", iniB1];
  RSet["dccpar", inidcc1];
  RSet["rmat", r];
  mod = If[OptionValue[DCCgarch, Model] == "Diagonal", "diagonal",
    If[OptionValue[DCCgarch, Model] == "Extended", "extended", 
     Print[Style["Wrong model selected,assuming Model->Diagonal",Red]]; 
     "diagonal"]];
  alg1 = If[OptionValue[DCCgarch, Algorithm1] == "BFGS", "BFGS",
    If[OptionValue[DCCgarch, Algorithm1] == "NelderMead", 
     "Nelder-Mead",
     If[OptionValue[DCCgarch, Algorithm1] == "ConjugateGradients", 
      "CG", Print[Style[
       "Wrong option value for Algorithm1 selected,choose one of:
       -\"BFGS\"
       -\"NelderMead\"
       -\"ConjugateGradients\"
       assuming \"BFGS\"",Red]]; "BFGS"]]];
  alg2 = If[OptionValue[DCCgarch, Algorithm2] == "BFGS", 1,
    If[OptionValue[DCCgarch, Algorithm2] == "NelderMead", 0, 
     Print[Style["Wrong option value for Algorithm1 selected,choose one of:
      -\"BFGS\"
      -\"NelderMead\"
      assuming \"BFGS\"",Red]]; 1]];
  REvaluate[
   "{dccresults=dcc.estimation(inia=as.numeric(a),iniA=A,iniB=B,
    ini.dcc=dccpar,dvar=rmat,model=\"" <> mod <> "\",method=\"" <> 
    alg1 <> "\",gradient=" <> ToString@alg2 <> ",message=0);
    #sve ovo samo zbog korelacija
    cor=dccresults$DCC;
    l=NULL;
    o=NULL;
    for(i in 1:dim(rmat)[2]){
         for(j in 1:dim(rmat)[2]){
            l=rbind(l,c(i,j))
    }};
    for(i in 1:(dim(rmat)[2])^2){
    o=cbind(o,paste(l[i,1], l[i,2],sep=\"-\"))
    };
    colnames(cor)=as.list(o);
    b=NULL
    for(i in 1:(dim(rmat)[2]-1)){
      b=cbind(b,cor[,((i*dim(rmat)[2])-((dim(rmat)[2])-1-i)):(i*dim(\
rmat)[2])])
    };
    colnames(b)[(dim(rmat)[2]*((dim(rmat)[2])-1)/2)]=paste(dim(rmat)[\
2]-1,\"-\",(dim(rmat)[2]),sep=\"\")
    }"];
  If[method == "coef", 
   Prepend[Insert[RGetData@REvaluate["as.data.frame(dccresults$out)"],
       RGetRowNames@REvaluate["as.data.frame(dccresults$out)"], 
       1]\[Transpose],
     Insert[RGetNames@REvaluate["as.data.frame(dccresults$out)"], "", 
      1]]\[Transpose],
   If[method == "ll", 
    RGetData@REvaluate["as.data.frame(dccresults$loglik)"][[1,1]],
     If[method == 
      "volatility", \[Sqrt]Insert[
       RGetData@REvaluate["as.data.frame(dccresults$h)"]\[Transpose],
       RGetNames@REvaluate["as.data.frame(dccresults$h)"], 1],
       If[method=="residuals",Insert[
        RGetData@REvaluate["as.data.frame(dccresults$std.resid)"]\[Transpose],
        RGetNames@REvaluate["as.data.frame(dccresults$std.resid)"], 1],
         If[method == "correlation", 
      Insert[RGetData@REvaluate["as.data.frame(b)"]\[Transpose],
       RGetNames@REvaluate["as.data.frame(b)"], 1], 
      Print[Style["Wrong input at position 2,assuming \"coef\"
       Type one of following:
       -\"coef\"
       -\"volatility\"
       -\"correlation\"
       -\"ll\"
       -\"residuals\"",Red]]; "coef"]]]]]]
       
Options[UgarchForecast] = {QProb -> 0.01, MExtForecast -> Null, 
  VExtForecast -> Null, ImageSize -> Automatic,RollFrame->0,Summary->True}
SetAttributes[UgarchForecast, HoldFirst]
UgarchForecast[
   Ugarch[data_, formula_:"garch(1,1)", Opts___],
   method_String,
   NAhead_: 1,
   NRoll_: 0,
   OptionsPattern[]
   ] /; IntegerQ@NAhead && Positive@NAhead && IntegerQ@NRoll && 
   NRoll >= 0&&ListQ@data :=
 Module[
  {nahead = NAhead, nroll = NRoll, mregfor, vregfor, methods = method,
    prob, plotlist, plotnum},
  REvaluate["remove(mregfor,vregfor,nroll,nahead,forecast,p,plotnum)"];
  (*Probability value for quantile method*)
  prob = OptionValue[UgarchForecast, QProb];
  If[(NumberQ@prob && prob > 0 && prob < 1) == True, RSet["p", prob],
    Print[Style["QProb should take values from 0 to 1", Red]]; 
   Abort[]];
  (*defining potencial variables for regressors*)
  mregfor = OptionValue[UgarchForecast, MExtForecast];
  vregfor = OptionValue[UgarchForecast, VExtForecast];
  If[
   vregfor === Null, REvaluate["vregfor=NULL"],
   If[ListQ@vregfor == True,
    If[Count[vregfor, _] == Count[data, _], RSet["vregfor", vregfor]; 
     REvaluate["vregfor=as.matrix(vregfor)"], 
     Print[Style[
       "Unequal row numbers in time series and variance external \
regressor", Red]]; Abort[]], 
    Print["Input variable for variance external regressors should be \
a list with same number of rows as time series"]; Abort[]]];
  If[
   mregfor === Null, REvaluate["mregfor=NULL"],
   If[ListQ@mregfor == True,
    If[Count[mregfor, _] == Count[data, _], RSet["mregfor", mregfor]; 
     REvaluate["mregfor=as.matrix(mregfor)"], 
     Print[Style[
       "Unequal row numbers in time series and mean external \
regressor", Red]]; Abort[]]
    , Print[
     Style["Input variable for mean external regressors should be a \
list with same number of rows as time series", Red]]; Abort[]]];
(*assigning variables to R*)
  RSet["nahead", nahead];
  RSet["nroll", nroll];
  RSet["p", prob];
  (*******************************************************************)
  
  mathematicaRPlotWrapper = RFunction["function(filename, plotfun){
             pdf(filename,useDingbats=F)
             plotfun()
             dev.off()
             }"];
  (*The following creates a generic R wrapper that creates the plot \
and saves it to a file,
  and defines certain parameters for the resulting image.*)
  getRPlot[plotFun_RFunction] := 
   With[{tempfile = FileNameJoin[{$TemporaryDirectory, "temp.pdf"}]}, 
    If[FileExistsQ[tempfile], DeleteFile[tempfile]];
    mathematicaRPlotWrapper[tempfile, plotFun];
    If[! FileExistsQ[tempfile], Return[$Failed]];
    Import[tempfile]];
  plotlist = {"1:Time Series Prediction (unconditional)",
     "2:Time Series Prediction (rolling)",
     "3:Sigma Prediction (unconditional)",
     "4:Sigma Prediction (rolling)"};
    (**********************************************************************)

    
    Ugarch[data, formula, "volatility", Opts]//Quiet;
      (*Checking the condition that out of sample is bigger than roll*)
  If[REvaluate["out.sample"][[1]]<nroll,
  	Print[Style["The number of rolling forecasts at position 4 must not be greater than OutSample option value",Red]];Abort[]];
  	(*R command*)
  REvaluate[
   "{forecast=ugarchforecast(fitted, n.ahead = nahead, n.roll = nroll, 
   		external.forecasts = list(mregfor = mregfor, vregfor = \
vregfor))}"];
(*trenutno dok se rlink ne apdejtuje*)
(*old code->>REvaluate["jbg=function(sta){
    datafr=NULL
    for(i in 1:(nroll+1)){
    kurac=forecast@forecast$forecasts[[i]]
    var=kurac[,sta]
    datecolumn=date[length(as.matrix(return))-out.sample-1+i]
    datafr=cbind(datafr,var)
    colnames(datafr)[i]=as.character(datecolumn)
    }
    nazivreda=NULL
    for(i in 1:(nrow(datafr))){
    nazivreda[i]=paste(\"T+\",i,sep=\"\")
    }
    rownames(datafr)=nazivreda
    as.data.frame(datafr)
    }
    "];*)
  (************************METHODS***************************)
  If[methods == "volatility",
        REvaluate["as.data.frame(sigma(forecast))"]//TableForm,
   If[methods == "fitted", 
    REvaluate["as.data.frame(fitted(forecast))"]//TableForm,
    (*REvaluate["as.data.frame(fitted(forecast)))"]//TableForm,*)
    
    (*Framed@StringJoin@Riffle[#, "\n"] &@REvaluate[
      	"capture.output(as.data.frame(forecast@forecast$forecasts))"],*)
      
    If[methods == "quantile", 
     REvaluate["as.data.frame(quantile(forecast,prob=p))"] //TableForm,
      
      If[methods=="fpm",
      	(*REvaluate[
      	"as.data.frame(fpm(forecast,summary="<>StringTake[ToString@OptionValue[UgarchForecast,Summary],1]<>"))"]//TableForm*)
      	Framed@StringJoin@Riffle[#, "\n"] &@REvaluate[
      	"capture.output(fpm(forecast,summary="<>StringTake[ToString@OptionValue[UgarchForecast,Summary],1]<>"))"
      	],
       If[methods == "plot", 
      Quiet@Panel@TabView[Table[RSet["plotnum", plotnum];plotlist[[plotnum]] ->Show[getRPlot[RFunction[
      	"function(){plot(forecast,which=plotnum,n.roll="<>ToString@OptionValue[UgarchForecast,RollFrame]<>")}"]], 
            ImageSize -> OptionValue[UgarchForecast, ImageSize], 
            PlotRange -> All], {plotnum, 1, 4}], 
         ControlPlacement -> Left],
        If[methods == "summary", 
        Framed@StringJoin@Riffle[#, "\n"] &@REvaluate["capture.output(forecast)"],
     	 If[methods=="all",{
      	REvaluate["as.data.frame(sigma(forecast))"]//TableForm,
      	REvaluate["as.data.frame(fitted(forecast))"]//TableForm,
      	REvaluate["as.data.frame(quantile(forecast,prob=p))"] //TableForm
      	},
       Print[Style[
         "Wrong input string at position 4,input should be one of:
         -\"volatility\"
         -\"fitted\"
         -\"quantile\"
         -\"plot\"
         -\"summary\"
         Assuming \"summary\"",Red]];
       Framed@StringJoin@Riffle[#, "\n"] &@
        REvaluate["capture.output(show(forecast))"]]]]]]]]
  ]
  UgarchForecast[method_String,NAhead_: 1,NRoll_: 0,OptionsPattern[]
  ] /; IntegerQ@NAhead && Positive@NAhead && IntegerQ@NRoll && NRoll >= 0:= 
   Module[
   
  {
  	nahead = NAhead, nroll = NRoll, mregfor, vregfor, methods = method,
    prob, plotlist, plotnum
    },
  REvaluate["remove(mregfor,vregfor,nroll,nahead,forecast,p,plotnum)"];
  (*Probability value for quantile method*)
  prob = OptionValue[UgarchForecast, QProb];
  If[(NumberQ@prob && prob > 0 && prob < 1) == True, RSet["p", prob],
    Print[Style["QProb should take values from 0 to 1", Red]]; 
   Abort[]];
  (*defining potencial variables for regressors*)
  mregfor = OptionValue[UgarchForecast, MExtForecast];
  vregfor = OptionValue[UgarchForecast, VExtForecast];
  If[
   vregfor === Null, REvaluate["vregfor=NULL"],
   If[ListQ@vregfor == True,
    If[Count[vregfor, _] == Count[data, _], RSet["vregfor", vregfor]; 
     REvaluate["vregfor=as.matrix(vregfor)"], 
     Print[Style[
       "Unequal row numbers in time series and variance external \
regressor", Red]]; Abort[]], 
    Print["Input variable for variance external regressors should be \
a list with same number of rows as time series"]; Abort[]]];
  If[
   mregfor === Null, REvaluate["mregfor=NULL"],
   If[ListQ@mregfor == True,
    If[Count[mregfor, _] == Count[data, _], RSet["mregfor", mregfor]; 
     REvaluate["mregfor=as.matrix(mregfor)"], 
     Print[Style[
       "Unequal row numbers in time series and mean external \
regressor", Red]]; Abort[]]
    , Print[
     Style["Input variable for mean external regressors should be a \
list with same number of rows as time series", Red]]; Abort[]]];
(*assigning variables to R*)
  RSet["nahead", nahead];
  RSet["nroll", nroll];
  RSet["p", prob];
  (*******************************************************************)
  
  mathematicaRPlotWrapper = RFunction["function(filename, plotfun){
             pdf(filename,useDingbats=F)
             plotfun()
             dev.off()
             }"];

  getRPlot[plotFun_RFunction] := 
   With[{tempfile = FileNameJoin[{$TemporaryDirectory, "temp.pdf"}]}, 
    If[FileExistsQ[tempfile], DeleteFile[tempfile]];
    mathematicaRPlotWrapper[tempfile, plotFun];
    If[! FileExistsQ[tempfile], Return[$Failed]];
    Import[tempfile]];
  plotlist = {"1:Time Series Prediction (unconditional)",
     "2:Time Series Prediction (rolling)",
     "3:Sigma Prediction (unconditional)",
     "4:Sigma Prediction (rolling)"};
    (**********************************************************************)
      (*Checking the condition that out of sample is bigger than roll*)
  If[REvaluate["out.sample"][[1]]<nroll,
  	Print[Style["The number of rolling forecasts at position 4 must not be greater than OutSample option value in Ugarch",Red]];Abort[]];
  	(*R command*)
  REvaluate[
   "{forecast=ugarchforecast(fitted, n.ahead = nahead, n.roll = nroll, 
   		external.forecasts = list(mregfor = mregfor, vregfor = \
vregfor))}"];
(*stari kod zbog nedostataka rlinka*)
(*old code->>REvaluate["jbg=function(sta){
    datafr=NULL
    for(i in 1:(nroll+1)){
    kurac=forecast@forecast$forecasts[[i]]
    var=kurac[,sta]
    datecolumn=date[length(as.matrix(return))-out.sample-1+i]
    datafr=cbind(datafr,var)
    colnames(datafr)[i]=as.character(datecolumn)
    }
    nazivreda=NULL
    for(i in 1:(nrow(datafr))){
    nazivreda[i]=paste(\"T+\",i,sep=\"\")
    }
    rownames(datafr)=nazivreda
    as.data.frame(datafr)
    }
    "];*)
  (************************METHODS***************************)
  If[methods == "volatility",
        REvaluate["as.data.frame(sigma(forecast))"]//TableForm,
   If[methods == "fitted", 
    REvaluate["as.data.frame(fitted(forecast))"]//TableForm,
    If[methods == "quantile", 
     REvaluate["as.data.frame(quantile(forecast,prob=p))"] // 
      TableForm,
      If[methods=="fpm",Framed@StringJoin@Riffle[#, "\n"] &@REvaluate[
      	"capture.output(fpm(forecast,summary="<>StringTake[ToString@OptionValue[UgarchForecast,Summary],1]<>"))"
      	],
       If[methods == "plot", 
      Quiet@Panel@TabView[Table[RSet["plotnum", plotnum];plotlist[[plotnum]] ->Show[getRPlot[RFunction[
      	"function(){plot(forecast,which=plotnum,n.roll="<>ToString@OptionValue[UgarchForecast,RollFrame]<>")}"]], 
            ImageSize -> OptionValue[UgarchForecast, ImageSize], 
            PlotRange -> All], {plotnum, 1, 4}], 
         ControlPlacement -> Left],
        If[methods == "summary", 
        Framed@StringJoin@Riffle[#, "\n"] &@REvaluate["capture.output(forecast)"],
     	 If[methods=="all",{
      	REvaluate["as.data.frame(sigma(forecast))"]//TableForm,
      	REvaluate["as.data.frame(fitted(forecast))"]//TableForm,
      	REvaluate["as.data.frame(quantile(forecast,prob=p))"] //TableForm
      	},
       Print[Style[
         "Wrong input string at position 4,input should be one of:
         -\"volatility\"
         -\"fitted\"
         -\"quantile\"
         -\"plot\"
         -\"summary\"
         Assuming \"summary\"",Red]];
       Framed@StringJoin@Riffle[#, "\n"] &@
        REvaluate["capture.output(show(forecast))"]]]]]]]]
  ]
  (*function that is being used to plot graphs from R*)
  mathematicaRPlotWrapper=RFunction["function(filename, plotfun){
		     pdf(filename,useDingbats=F)
		     plotfun()
		     dev.off()
		     }"];
		(*The following creates a generic R wrapper that creates
		 the plot and saves it to a file,and defines certain parameters 
		for the resulting image.*)
		getRPlot[plotFun_RFunction]:=With[{tempfile=FileNameJoin[{$TemporaryDirectory,"temp.pdf"}]},If[FileExistsQ[tempfile],DeleteFile[tempfile]];
		mathematicaRPlotWrapper[tempfile,plotFun];
		If[!FileExistsQ[tempfile],Return[$Failed]];
		Import[tempfile]];
End[]

EndPackage[]


