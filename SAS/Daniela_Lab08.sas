/* Daniela Luque-Sanchez*/

/* Question 1f: Creating formats for drug groups, severity of headache, and presence of headache */
PROC FORMAT;
VALUE drugF 1 = 'Beta Blocker'
			2 = 'Calcium Channel Blocker'
			3 = 'Diuretic'
			4 = 'Alpha Blocker'
			5 = 'ACE Inhibitor'
			6 = 'Placebo'
			;
VALUE se12_6F 1 = 'None'
			 2 = 'Mild'
			 3 = 'Moderate'
			 4 = 'Severe'
			 ;
VALUE headacheF 0 = 'No Headache'
				1 = 'Headache'
				;
VALUE drugfmt
        1 = 'Beta Blocker'
        2 - 6 = 'Other Groups Combined'
        ;
/* Question 1a: Importing data from an Excel file */
PROC IMPORT
	DATAFILE = '/home/u64017103/tomphsp.xlsx'
	OUT = data
	DBMS = XLSX
	REPLACE ;
	GETNAMES = yes;
RUN; 

/* Question 1b */
PROC CONTENTS DATA = data VARNUM;
RUN;

/*Question 1c: Creating a subset of the data with selected variables */
DATA data;
	SET data (KEEP = ptid drug hdlbl se12_6);

/* Question 1d: Creating a binary variable for the presence of headaches */
IF se12_6 = . THEN headache = .; ELSE
IF se12_6 = 1 THEN headache = 0; ELSE
headache = 1;
/* 0 = no headache, 1 = headache */

/* Question 1e: Labeling variables for better readability */
LABEL 
	ptid = 'Patient ID'
	drug = 'Study Drug Group'
	hdlbl = 'HDL Cholesterol at Baseline'
	se12_6 = 'Severity of Headache'
	;
FORMAT se12_6 se12_6F. headache headacheF. drug drugF.; /* Formatting variables with defined formats */
RUN;

/* Question 1g: Frequency table for severity of headache and presence of headache */
PROC FREQ DATA = data;
TABLES se12_6 *headache / NOCOL NOCUM NOPERCENT NOROW;
RUN;

/* Question 1i: Verifying normality assumptions with diagnostic plots */
ODS GRAPHICS ON;
PROC GLM DATA = data PLOTS(UNPACK)=DIAGNOSTICS;
CLASS drug;
MODEL hdlbl = drug;
MEANS drug/BON LINES;
RUN;
/*QQ Plot of residuals is not linear and distribution does not look normal. A non-parametric test is required for further analysis. */

/* Question 1h: Non-parametric test for baseline HDL cholesterol levels */
PROC NPAR1WAY DATA = data WILCOXON ;
CLASS drug;
VAR hdlbl; 
TITLE 'Non-parametric Test Comparing
Groups in Baseline HDL Cholesterol Levels';
RUN;
/* There is not enough evidence to conclude that there is a difference in  mean baseline HDL levels across all drug groups. We fail to reject the null at an alpha of 0.05 and a p-value of 0.4997 for the Kruskal-Wallis test. Therefore, HDL levels at Baseline are likely not a confounder. */

/* Question 1j: Estimating pairwise comparisons using GLM */
PROC GLM DATA = data PLOTS=DIAGNOSTICS;
CLASS drug;
MODEL hdlbl = drug;
ESTIMATE 'BB vs Placebo' drug 1 0 0 0 0 -1 ;
ESTIMATE 'CCB vs Placebo' drug 0 1 0 0 0 -1 ;
ESTIMATE 'Diuretic vs Placebo' drug 0 0 1 0 0 -1 ;
ESTIMATE 'Alpha B vs Placebo' drug 0 0 0 1 0 -1 ;
ESTIMATE 'ACE v Placebo' drug 0 0 0 0 1 -1 ;
MEANS drug;
RUN;
QUIT; 
/* There is not enough evidence to conclude that there is a difference in mean baseline HDL levels between any of the drug groups and the placebo group. We fail to reject the null hypothesis at an alpha of 0.05, with an overall p-value of 0.7010. Additionally, none of the pairwise comparisons between the drug groups and placebo yielded significant results, with all p-values above 0.05. */

/* Question 1k: Chi-square test for the proportion of headaches across drug groups */
PROC FREQ DATA = data;
    FORMAT drug drugfmt.;
    TABLES drug*headache / CHISQ RELRISK;
RUN;
/* There is not enough evidence to conclude that the proportion of headaches is significantly higher for the Beta blocker group compared to all other groups combined. We fail to reject the null hypothesis at an alpha of 0.05, with a p-value of 0.6783. Therefore, the incidence of headaches in the Beta blocker group is likely not significantly different from that in the other groups.*/

/************************** Question 2a **************************/
/* Creating formats for cancer and antiulcer drug variables */
PROC FORMAT;
VALUE CancerF 1 ='Yes' 
			  2='No';
VALUE AntiulcerF 1='Yes' 
				 2='No';
/* Reading data from inline datalines */
DATA cancer;
INFILE DATALINES;
INPUT Cancer $ Antiulcer $ count;
FORMAT Cancer CancerF. Antiulcer AntiulcerF.;
DATALINES;
1 1 7
2 1 45
1 2 35
2 2 533
;
RUN;

/* Frequency table with chi-square test for cancer and antiulcer drug use */
PROC FREQ DATA = cancer;
TABLES Antiulcer*Cancer/CHISQ RELRISK NOPERCENT NOCOL;
WEIGHT COUNT;
TITLE 'Relationship between Cancer and Antiulcer Drugs';
RUN;
/*There is enough evidence to conclude that there is a relationship between the use of antiulcer drugs and cancer. We reject the null hypothesis at an alpha of 0.05, with a p-value of 0.0450. Therefore, the relationship between antiulcer drug use and cancer is statistically significant. */

/* Question 2b 
The p-value from my analysis (0.0450) is slightly higher than the p-value reported by the researchers (0.04). Differences in statistical software or specific calculation methods can result in slight variations in p-values due to rounding. */



