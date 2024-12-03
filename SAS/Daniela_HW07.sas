/* Daniela Luque-Sanchez */

/* QUESTION 1d */
/* Define custom formats for categorical variables to make the output more readable. 
   - smoke5F: Indicates whether smoking is present in the household (0 = Not Present, 1 = Present).
   - newsocioF and newsocio5F: Represent socioeconomic status categories, ranging from 'Below Poverty' to 'High'. */

PROC FORMAT;
VALUE smoke5F 0 = 'Not Present'
			  1 = 'Present'
				 ;
VALUE newsocioF 0 = 'Below Poverty'
			    1 = 'Low'
			    2 = 'Middle'
			    3 = 'High'
			   ;
VALUE newsocio5F 0 = 'Below Poverty'
			     1 = 'Low'
			     2 = 'Middle'
			     3 = 'High'
			    ;
RUN;



/* QUESTION 1a: Import data from an Excel file. */
PROC IMPORT
	DATAFILE = '/home/u64017103/sasuser.v94/btt.xlsx'
	OUT= data
	DBMS = xlsx
	REPLACE ;
	GETNAMES = yes;
RUN;

/*QUESTION 1b */

/* Create a new dataset by keeping only the relevant variables and recoding the 'socio' and 'socio5' variables 
   into numeric formats for easier analysis. */
DATA data;
SET data (KEEP = childid bweight socio smoke5 socio5);

/* Reordering socio & socio5 variable */
IF socio = ' ' THEN newsocio = .; ELSE
IF socio = 'Poverty' THEN newsocio = 0; ELSE
IF socio = 'Low' THEN newsocio = 1; ELSE
IF socio = 'Middle' THEN newsocio = 2; ELSE
IF socio = 'High' THEN newsocio = 3;

IF socio5 = ' ' THEN newsocio5 = .; ELSE
IF socio5 = 'Poverty' THEN newsocio5 = 0; ELSE
IF socio5 = 'Low' THEN newsocio5 = 1; ELSE
IF socio5 = 'Middle' THEN newsocio5 = 2; ELSE
IF socio5 = 'High' THEN newsocio5 = 3;

/*QUESTION 1c */
LABEL childid = 'Pediatric Patient ID'
	  bweight = 'Birth Weight (g)'
	  socio = 'Mother''s Socioeconomic Status at Child''s Birth'
	  smoke5 = 'Presence of Smokers in House Post 5 Years'
	  socio5 = 'Mother''s Socioeconomic Status Post 5 Years'
	  ;
FORMAT smoke5 smoke5F. newsocio newsocioF. newsocio5 newsocio5F.;
RUN;

/* Check if the recoding of socioeconomic variables was done correctly by cross-tabulating the original and recoded variables. */
PROC FREQ DATA = data;
TABLES newsocio*socio newsocio5*socio5/NOCUM NOROW NOCOL NOPERCENT MISSPRINT;
RUN;

/* QUESTION 1e */
PROC PRINT DATA = data (OBS = 7) LABEL;
VAR childid bweight socio smoke5 socio5;
RUN;

/*QUESTION 1(f)i. & ii  */
/* Perform an ANOVA test to compare birth weight across socioeconomic status groups. */
  
PROC ANOVA DATA = data;
	CLASS newsocio;
	MODEL bweight = newsocio;
	MEANS newsocio / BON LINES; 
TITLE 'ANOVA Comparing Change in Birth-weight Across Socioeconomic Statuses';
RUN;
/* There is not enough evidence to conclude that there is a difference in the birth weight of babies across the different maternal socioeconomic statuses.
Based on the ANOVA test, we fail to reject the null hypothesis at an alpha level of 0.05, with a p-value of 0.8187, indicating that the differences in birth weight across socioeconomic statuses are not statistically significant. */


/* QUESTION 1(f)iii., iv, & v. */
PROC GLM DATA = data PLOTS(UNPACK)=DIAGNOSTICS;
CLASS newsocio;
MODEL bweight = newsocio;
MEANS newsocio/BON LINES;
RUN;
/*Based on the qq plot of the residuals, the graph does not suggest that any assumptions were violated as the data follows a linear trend, indicating a normal distribution. */

/* QUESTION 1(g)i. */
/* Perform a Chi-square test to assess the association between smoking presence in the household 
   and maternal socioeconomic status at the 5-year follow-up. */
PROC FREQ DATA = data;
TABLES newsocio5*smoke5/CHISQ RELRISK;
TITLE '"Chi-square Test Comparing Smoking Presence 
vs Socioeconomic Status at 5-Year Follow-up"';
RUN;
/* There is not enough evidence to conclude that there is an association between smoking presence in the household and the mother's socioeconomic status at the 5-year follow-up.
Based on the Chi-square test, we fail to reject the null hypothesis at an alpha level of 0.05, with a p-value of 0.5229, indicating that the association is not statistically significant. */

/*QUESTION 1(g)ii. & (g)iii.*/
/* Data must be categorical and groups must be independent and unrelated which were met. */

/* QUESTION 2 */
/* Import a new dataset for analyzing fertilizer effects on corn yield. */
PROC IMPORT
	DATAFILE = '/home/u64017103/sasuser.v94/crop.xlsx'
	OUT= data2
	DBMS = xlsx
	REPLACE ;
	GETNAMES = yes;
RUN;

/* QUESTION 2a * 2b */
/* Perform ANOVA to evaluate the impact of fertilizer levels on corn yield. */
PROC ANOVA DATA = data2;
	CLASS fertilizer;
	MODEL yield = fertilizer;
	MEANS fertilizer / BON LINES; 
TITLE 'ANOVA Comparing Corn Yield Across Fertilizer Levels';
RUN;

/* 2a: There is enough evidence to conclude that fertilizer level makes a difference in corn yield.
Based on the ANOVA test, we reject the null hypothesis at an alpha level of 0.05, with a p-value of 0.0007, indicating a statistically significant effect of fertilizer level on corn yield. 
2b: Fertilizer Group 3 is different from Fertilizer group 1 & 2. Fertilizer Groups 1 & 2 are not different from each other. */

/* QUESTION 2c & 2d */
PROC GLM DATA = data2 PLOTS(UNPACK)=DIAGNOSTICS;
CLASS fertilizer;
MODEL yield = fertilizer;
MEANS fertilizer/BON LINES;
RUN;
/* The graph does not suggest any of the assumptions might be violated as the qq plot of the residuals follows a linear trend, indicating a normal distribution. */

PROC ANOVA DATA = data2;
	CLASS fertilizer;
	MODEL yield = fertilizer;
	MEANS fertilizer / TUKEY;
RUN;

/* QUESTION 3a */
/* Create a dataset for cardiovascular disease (CVD) and aspirin/placebo comparison. */
DATA data3;
	INFILE DATALINES;
    INPUT CVD $ Aspirin $ count;
    DATALINES;
Yes Aspirin 477
No Aspirin 19457
Yes Placebo 522
No Placebo 19390
;
RUN;

/* Perform a Chi-square test to compare CVD incidence between aspirin and placebo groups. */
PROC FREQ DATA = data3;
TABLES CVD*Aspirin / CHISQ RELRISK NOPERCENT NOCOL;
WEIGHT count;
TITLE 'Comparison of CVD between Aspirin and Placebo Groups';
RUN;
/* The chi-square test indicates that there is no significant difference in the percentage of cardiovascular disease (CVD) events between women randomized to aspirin and those randomized to placebo. The p-value was 0.1444, which is greater than the alpha level of 0.05, leading us to fail to reject the null hypothesis. This suggests that the use of aspirin is not associated with a statistically significant difference in the incidence of CVD events compared to placebo in this trial. */ 

/* QUESTION 3b */
/* Data must be categorical and groups must be independent and unrelated which were met. */


