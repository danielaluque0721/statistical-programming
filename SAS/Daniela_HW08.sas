/* Daniela Luque-Sanchez */

/* QUESTION 3: Create a custom format for the gender variable */
PROC FORMAT;
VALUE genderF 0 = 'Female'
			  1 = 'Male'
			  ;
			  
/* QUESTION 1: Import Excel data into SAS */
PROC IMPORT
	DATAFILE = '/home/u64017103/sasuser.v94/ischemic.xlsx'
	OUT = data
	DBMS = XLSX
	REPLACE ;
	GETNAMES = yes;
RUN;

/* Display the structure and variable order of the dataset */
PROC CONTENTS DATA = data VARNUM;
RUN;

DATA data;
	SET data;

/* QUESTION 2: Add labels and formats to the dataset */
LABEL 
	id = 'Patient ID'
	cost = 'Total Cost of Claims ($)'
	age = 'Age of Subscriber'
	gender = 'Gender of Subscriber'
	interv = 'No. Interventions or Procedures'
	drugs = 'No. Drugs Prescribed'
	ervisit = 'No. ER Visits'
	complic = 'No. Complications'
	comorb = 'No. Comorbidities'
	dur = 'Duration of Treatment in Days'
	;

/* Apply the gender format created earlier */
FORMAT gender genderF.;
RUN;

/* QUESTION 4: Check that dataset was created correctly */
PROC PRINT DATA = data (OBS=7) LABEL;
RUN;

/* QUESTION 5Generate scatterplot matrices to explore relationships */

/* Scatterplots for cost against age, gender, and interventions */
PROC SGSCATTER DATA = data;
TITLE 'Scatterplot Matrix for Ischemic Data';
COMPARE Y = cost
X=(age gender interv);
RUN;

PROC SGSCATTER DATA = data;
TITLE 'Scatterplot Matrix for Ischemic Data';
COMPARE Y = cost
X=(drugs ervisit complic);
RUN;

PROC SGSCATTER DATA = data;
TITLE 'Scatterplot Matrix for Ischemic Data';
COMPARE Y = cost
X=(comorb dur);
RUN;

/* QUESTION 5a
There does not appear to be a linear relationship between any of the predictor variables and the response according to the scatterplots. 

/* QUESTION 5b: : Generate scatterplot matrix with histograms and kernel density estimates  */
PROC SGSCATTER DATA = data;
TITLE "Scatterplot MATRIX With
Histograms";
MATRIX age gender interv drugs ervisit complic comorb dur
/ DIAGONAL=(HISTOGRAM KERNEL);
RUN;
/* 5b: The plot does not suggest that any of the independent variables might be linearly related to each other as the data does not follow a linear trend for any pair of variables. */

/* QUESTION 6: Compute pairwise correlations */
PROC CORR DATA = data;
Var cost age gender interv drugs ervisit complic comorb dur;
RUN;

/* 6a: Variables interv, drugs, ervisit, complic, comorb, and dur have a significant correlation coefficient with cost, all with p-values < .0001. 
6b: The following variables appear to have a problem with multicollinearity since thet have a statistically significant pvalue less than .05: age with comorb and dur. gender with ervisit. interv with drugs, ervisits, comorb, and dur. drugs with ervisit and complic. ervisit with gender, interv, drugs, complic, and dur. complic with interv, ervisit, and drugs. comorb with interv and dur. dur with age, interv, ervisit, and comorb. */

/* Question 7: Fit a multiple linear regression model to predict cost */
PROC REG DATA = data;
MODEL cost=age gender interv drugs ervisit complic comorb dur / COLLIN TOL VIF;
TITLE 'Multiple Linear Regression';
RUN;
QUIT;
/* At a .05 significance level, there is evidence that the following predictors may not be important in
determining the total cost: complic and dur. They have values of .1893 and .6342 respectively. */

/* QUESTION 9 */
/* Variable SELECTION: Stepwise */
PROC REG DATA = data;
MODEL cost=age gender interv drugs ervisit complic comorb dur / SELECTION=Stepwise
SLENTRY = 0.10 SLSTAY = 0.05;
TITLE 'Multiple Linear Regression: Stepwise SELECTION';
RUN;

/* gender, interv, drugs, and er visit were not eliminated from the model as they have significant (alpha = .05) p-values of .0181, <.0001, .00358, and < .0001 respectively.
Final Regression Equation: yhat(predicted total cost) = -2090.53 - 912.80gender + 815.63interv - 374.66drugs + 416.72ervisit */

/* QUESTION 10
a) Scatterplot of predicted vs residuals of full model, and the qq plot of the residuals.
b) Normality and constant variance
c) Each graph imply the assumptions were not met. The qq plot does not follow a linear trend and therefore indicates the data is not normally distributed. The scatterplot is not randomly scattered which violates the homoscedasticity assumption. */


PROC REG DATA = data;
MODEL cost= gender interv drugs ervisit / COLLIN TOL VIF;
TITLE 'Multiple Linear Regression';
RUN;
QUIT;
/* QUESTION 11 
Using the Rsquared value of 0.5478, the model IS practically useful, however the other assumptions for linear regression were not met.  */