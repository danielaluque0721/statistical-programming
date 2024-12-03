/* Daniela Luque-Sanchez: Intro to Statistical Computing*/

/*QUESTION 5, Formats for categorical variables. */
PROC FORMAT;
VALUE profitableF 0 = 'Total'
				  1 = 'Non-Profitable'
				  2 = 'Profitable'
				  ;
/* QUESTION 1, Reading in each file*/
PROC IMPORT
	DATAFILE = '/home/u64017103/sasuser.v94/StorePurchases.xlsx'
	OUT = purchases
	DBMS = XLSX
	REPLACE ;
	GETNAMES = yes;
RUN;

PROC IMPORT 
	DATAFILE = '/home/u64017103/sasuser.v94/StoreSales.xlsx'
	OUT = sales
	DBMS = XLSX
	REPlACE ;
	GETNAMES = yes;
RUN;

/* QUESTION 2, Merging the two datasets */

PROC SORT DATA = purchases; BY ProductID;
RUN;

PROC SORT DATA = sales; BY ProductID;
RUN;

DATA all;
MERGE purchases sales;
BY ProductID;

/* QUESTION 3, Creating new variables */

 * 3a.) one that provides the numerical value of the profit/loss incurred by each product;
 return = TotSale - TotCost;
 
 * 3b.) an indicator variable that will show of the product was profitable or not;
 IF return = . THEN profitable = . ; ELSE
 IF return <= 0 THEN profitable = 1; ELSE
 IF return > 0 THEN profitable = 2; 
 /* 1 = Not profitable, 2 = Profitable */

/*QUESTION 4, Creating labels for all variables. */
LABEL ProductID = 'Product ID'
	  NProdBougth = 'No. Products Purchased'
	  ProdUnitCost = 'Product Cost Per Unit'
	  TotCost = 'Overall Cost of Products Purchased'
	  NProdSold = 'No. Products Sold'
	  ProdUnitSale = 'Sale Price Per Product Unit'
	  TotSale = 'Overall Proceeds From Products Sold'
	  return = 'Return of Each Product'
	  profitable = 'Profitable'
	  ;
FORMAT profitable profitableF.;
RUN;

/* Checking to see if variables were created correctly */
PROC MEANS DATA = all MIN MAX;
CLASS profitable;
VAR return;
RUN;

PROC PRINT DATA = all (OBS = 7) LABEL;
VAR return ProdUnitSale ProdUnitCost;
RUN;

/* QUESTION 6, Checking to see dataset was created correctly */
PROC PRINT DATA = all (OBS = 7) LABEL;
RUN;

/* QUESTION 7, Creating datasets */
DATA sevena sevenb sevenc sevend;
	SET all;
	IF return ^= . THEN OUTPUT sevena; ELSE
	IF return = . THEN OUTPUT sevenb;
	
	IF profitable = 2 THEN OUTPUT sevenc; ELSE
	IF profitable = 1 THEN OUTPUT sevend;
	RUN;

/* Checking to see datasets were created correctly */
PROC PRINT DATA = sevena (OBS = 5) LABEL;
RUN;

PROC PRINT DATA = sevenb (OBS = 5) LABEL;
RUN;

PROC PRINT DATA = sevenc (OBS = 5) LABEL;
RUN;

PROC PRINT DATA = sevend (OBS = 5) LABEL;
RUN;

/* QUESTION 8, new dataset that has the number of products, mean and standard deviation of the total product cost, sale, and profit. */

/* Computes the descriptive statistics by profitability */
PROC MEANS DATA=sevena NOPRINT NWAY;
    CLASS profitable;  
    VAR TotCost TotSale return;  
    OUTPUT OUT = q8
        N = N_Cost N_Sale N_Profit   
        MEAN = Mean_Cost Mean_Sale Mean_Profit  
        STDDEV = SD_Cost SD_Sale SD_Profit;  
RUN;

/*Calculates the totals for all products */
PROC MEANS DATA=sevena NOPRINT;
    VAR TotCost TotSale return;
    OUTPUT OUT=totalq8
        N = N_Cost N_Sale N_Profit
        MEAN = Mean_Cost Mean_Sale Mean_Profit
        STDDEV = SD_Cost SD_Sale SD_Profit;
RUN;

/*Assigns a specific value to represent 'Total' */
DATA totalq8;
    SET totalq8;
    profitable = 0;
RUN;

/* Combines datasets */
DATA final_stats;
    SET totalq8 q8; 
    
/* Concatenate N, Mean, and SD into a single column for each variable */
    Cost_Info = CATX(' ', PUT(N_Cost, 7.0), PUT(Mean_Cost, 7.1), PUT(SD_Cost, 7.1));
    Sale_Info = CATX(' ', PUT(N_Sale, 7.0), PUT(Mean_Sale, 7.1), PUT(SD_Sale, 7.1));
    Profit_Info = CATX(' ', PUT(N_Profit, 7.0), PUT(Mean_Profit, 7.1), PUT(SD_Profit, 7.1));
    
   KEEP profitable Cost_Info Sale_Info Profit_Info;
   LABEL profitable = 'Group'
   		 Cost_Info = 'Cost*No. Products Avg. SD'
   		 Sale_Info = 'Sale*No. Products Avg. SD'
         Profit_Info = 'Profit*No. Products Avg. SD'
   		 ;
RUN;

PROC PRINT DATA = final_stats NOOBS LABEL SPLIT = '*' STYLE(HEADER) = {JUST = C};
RUN;

