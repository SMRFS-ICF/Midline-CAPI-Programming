{ PEEL Feed the Future Core Field Check Tables }
{ September 11, 2018 }
{ Created by: Genevieve Dupuis }


PROC GLOBAL
  { DHS-V chapter 14, section test tables  version 1.0.0 of 05/09/07 }

  set explicit;

  numeric byteam, x, i, itot, j, jtot, jtot1, jtot2, jtot3, jmax, jmin, k;
  numeric j15, j14, j17, j18, j49, j50, j5, j6, unitx, number, hwage, hwvalue, hwsex, hwerr1, hwerr2;
  numeric yearint, cutoff, ismonth, isyear, isage, valdate;


  array interview(100);       { determine if an individual was successfully interviewed to be used for DBS collection }

  crosstab float(1) f1 Fteams+fintnum+ftotal fhresult+ftotal1
    exclude( percents, totals, specval )
    title( "FC-1: Household completion rate ", " ",
           "Percent distribution of sampled households by result of household",
           "interview and household response rate by interviewer team, Country, Year" )

  crosstab float(1) f2 Fteams+fintnum+ftotal ftotal4+fprimarym+fprimaryf+fprimary
    exclude( percents, totals, specval )
    title( "FC-2: Primary male and female decisionmakers ", " ",
           "Number of households with a completed roster and, among those households, the percentage",
           "with at least one male member age 18+, the percentage with a male decisionmaker, the percentage",
           "with at least one female member age 18+, the percentage with a female decisionmaker, and the percentage",
           "of households with at least one decisionmaker,",
           "by interviewer team, Country, Year" )

  crosstab float(1) f3 Fteams+fintnum+ftotal Fageg+Ftotal6 
    exclude( percents, totals, specval )
    title( "FC-3: Age heaping in the household roster","",
           "Percentage of household members in 5-year age groups with ages recorded as ending in 5 or 0",
           "by interviewer team, Country, Year" );
  crosstab float(1) f3w Fteams+fintnum+ftotal Fagegw+Ftotal6w noprint
    exclude( percents, totals, specval )
    title( "FC-3: Age heaping in the household roster - WORKING TABLE" );


  crosstab float(2) f4 Fteams+fintnum+ftotal Fhhmean
    exclude( percents, totals, specval )
    title( "FC-4: Eligible women per household","",
           "Mean number of eligible women ages 15-49 years per household,",
           "Country, Year" );

  crosstab float(1) f5a Fteams+fintnum+ftotal Fage+Fratio
    exclude( percents, totals, specval )
    title( "FC-5A: Female age displacement","",
           "Number of all women ages 12-17 years listed in the household",
           "roster by single years of age and age ratios,",
           "by interviewer team, Country, Year" );

  crosstab float(1) f5b Fteams+fintnum+ftotal Fage1+Fratio1
    exclude( percents, totals, specval )
    title( "FC-5B: Female age displacement","",
           "Number of all women ages 47-52 years listed in the household",
           "roster by single years of age and age ratios,",
           "by interviewer team, Country, Year" );

  crosstab float(1) f5c Fteams+fintnum+ftotal Fage2+Fratio2
    exclude( percents, totals, specval )
    title( "FC-5C: Female age displacement","",
           "Number of all women ages 15-20 years listed in the household",
           "roster by single years of age and age ratios,",
           "by interviewer team, Country, Year" );

  crosstab float(1) f5d Fteams+fintnum+ftotal Fage3+Fratio3
    exclude( percents, totals, specval )
    title( "FC-5D: Male age displacement","",
           "Number of all men age 15-20 years listed in the household",
           "roster by single years of age and age ratios,",
           "by interviewer team, Country, Year" );

  crosstab float(2) f6 Fteams+fintnum+ftotal Fhhmeanm
    exclude( percents, totals, specval )
    title( "FC-6: Eligible children per household","",
           "Mean number of eligible children younger than 6 years per household,",
           "Country, Year" );

  crosstab float(1) f7 Fteams+fintnum+ftotal Fage4+Fratio4
    exclude( percents, totals, specval )
    title( "FC-7: Child age displacement","",
           "Number of all children ages 2-8 listed in the household",
           "roster by single years of age and age ratios,",
           "by interviewer team, Country, Year" );

  crosstab float(1) f8a Fteams+fintnum+ftotal fhresult1+ftotal2
    exclude( percents, totals, specval )
    title( "FC-8A: Module 6 (Women) Women's Empowerment in Agriculture Module, eligibility and response rate ", " ",
           "Percent distribution of eligible women (primary adult female decisionmaker) by result of individual",
           "outcome, by interviewer team, Country, Year" )
           
  crosstab float(1) f8b Fteams+fintnum+ftotal fhresult1+ftotal3
    exclude( percents, totals, specval )
    title( "FC-8B: Module 6 (Men) Women's Empowerment in Agriculture Module, eligibility and response rate ", " ",
           "Percent distribution of eligible men (primary adult male decisionmaker) by result of individual",
           "outcome, by interviewer team, Country, Year" )        

  crosstab float(1) f9 Fteams+fintnum+ftotal fhresult1+ftotal2
    exclude( percents, totals, specval )
    title( "FC-9: Module 4 Results (Women's anthropometry and dietary diversity) ", " ",
           "Percent distribution of all eligible women by result of individual",
           "outcomes, by interviewer team, Country, Year" )        

  crosstab float(1) f10a Fteams+fintnum+ftotal fhresult1+ftotal1
    exclude( percents, totals, specval )
    title( "FC-10A: Module 5 Results (Children's nutrition) ", " ",
           "Percent distribution of all eligible children younger than 6 years by result of ",
           "module 5, by interviewer team, Country, Year" )        

  crosstab float(1) f10b Fteams+fintnum+ftotal fhresult5+fhresult6+ftotal5
    exclude( percents, totals, specval )
    title( "FC-10B: Module 5 Results (Children's anthropometry) ", " ",
           "Percent distribution of all eligible children younger than 6 years by result of ",
           "module 5, by interviewer team, Country, Year" )        

  crosstab float(1) f11 Fteams+fintnum+ftotal Fageg1+Ftotal7 
    exclude( percents, totals, specval )
    title( "FC-11: Age heaping in months of age","",
           "Percentage of children in module 5 in each age group with age reported as 6,12,18, or 24 months",
           "by interviewer team, Country, Year" );
  crosstab float(1) f11w Fteams+fintnum+ftotal Fageg1w+Ftotal7w noprint
    exclude( percents, totals, specval )
    title( "FC-11: Age heaping in months of age - WORKING TABLE" );

   crosstab float(1) f12 Fteams+fintnum+ftotal f12c+ftotal9
    exclude( percents, totals, specval )
    title( "FC-12: Birth date and age reporting","",
           "Percent distribution of the completeness of birth date and age information,",
           "by interviewer team, Country, Year" );

   crosstab float(1) f13 Fteams+fintnum+ftotal f13h+f13w+ftotal8
    exclude( percents, totals, specval )
    title( "FC-13: Children's anthropometry ranges","",
           "Percent distribution of  children under 6 year with heights and weights",
           "in the WHO range for gender and age, by interviewer team, Country, Year" );



PROC PEEL_FF
preproc

  ftotal   = 1;
  ftotalb  = 1;
  ftotald  = 1;
  ftotalb9 = 1;
  ftotaln  = 1;
  ftotal1  = 1;
  ftotalw  = 1;
  ftotalm  = 1;
  ftotalc2 = 1;
  ftotalc3 = 1;
  ftotalc4 = 1;
  ftotalc5 = 1;
  ftotalw2 = 1;
  ftotalm2 = 1;
  ftotalw3 = 1;
  ftotalm3 = 1;
  ftarget  = notappl;
  fratio   = notappl;
  fratiob  = notappl;
  fratiom  = notappl;
  fratiod  = notappl;
  fratio9  = notappl;

  byteam = ( sysparm()[1:1] = "T" );

  yearint = 2018;                      { !! TO DO !! Year of interview (Last year)    }

postproc


  { Table f1 }
  jtot    = tblcol( f1, ftotal1 ) - 2;
  jmax    = jtot - 1;
  { Percents }
  do j = 0 while j <= jmax
    f1[*,j] = f1[*,j] * 100 / f1[*,jtot];
  enddo;
  { Total percentage }
  f1[*,jtot+1] = tblsum( column f1[*,0:jmax] );
  { HH response rate } 
  f1[*,jtot+2] = f1[*,0] / ( f1[*,0] + f1[*,1] + f1[*,2] + f1[*,4] + f1[*,9] )*100; 

  { Table f2 }
  jtot    = 0;
  jmax    = tblcol( f2 );
  { Percents }
  do j = 1 while j <= jmax
    f2[*,j] = f2[*,j] * 100 / f2[*,jtot];
  enddo;

  { Table f3 }
  jtot    = tblcol( f3, ftotal6 );
  jmax    = jtot - 2;
  { Percents }
  do j = 0 while j <= jmax
    f3[*,j] = f3[*,j] * 100 / f3w[*,j];
  enddo;
  f3[*,jmax+1] = f3[*,jmax+1] * 100 / f3[*,jmax+2];

  { Table f4 }
  jtot1 = tblcol( f4, FHHMEAN = 2 );   { mean for urban }
  itot = tblrow(  f4 );
  { Means }
  f4[*,jtot1] = f4[*,jtot1-1] / f4[*,jtot1-2];

  { Table f5a }
  j15  = tblcol( f5a, Fage = 15 );
  j14  = tblcol( f5a, Fage = 14 );
  jtot = tblcol( f5a );
  { Ratios }
  f5a[*,jtot-1] = f5a[*,j15] / f5a[*,j14];
  f5a[*,jtot] = ( f5a[*,j15] + f5a[*,j15+1] ) / ( f5a[*,j14] + f5a[*,j14-1] );

  { Table f5b }
  j49  = tblcol( f5b, Fage1 = 49 );
  j50  = tblcol( f5b, Fage1 = 50 );
  jtot = tblcol( f5b );
  { Ratios }
  f5b[*,jtot-1] = f5b[*,j49] / f5b[*,j50];
  f5b[*,jtot] = ( f5b[*,j49] + f5b[*,j49-1] ) / ( f5b[*,j50] + f5b[*,j50+1] );

  { Table f5c }
  j18  = tblcol( f5c, Fage2 = 18 );
  j17  = tblcol( f5c, Fage2 = 17 );
  jtot = tblcol( f5c );
  { Ratios }
  f5c[*,jtot-1] = f5c[*,j18] / f5c[*,j17];
  f5c[*,jtot] = ( f5c[*,j18] + f5c[*,j18+1] ) / ( f5c[*,j17] + f5c[*,j17-1] );

  { Table f5d }
  j18  = tblcol( f5d, Fage3 = 18 );
  j17  = tblcol( f5d, Fage3 = 17 );
  jtot = tblcol( f5d );
  { Ratios }
  f5d[*,jtot-1] = f5d[*,j18] / f5d[*,j17];
  f5d[*,jtot] = ( f5d[*,j18] + f5d[*,j18+1] ) / ( f5d[*,j17] + f5d[*,j17-1] );

  { Table f6 }
  jtot1 = tblcol( f6, FHHMEANM = 2 );   { mean for urban }
  itot = tblrow(  f6 );
  { Means }
  f6[*,jtot1] = f6[*,jtot1-1] / f6[*,jtot1-2];

  { Table f7 }
  j5  = tblcol( f7, Fage4 = 5 );
  j6  = tblcol( f7, Fage4 = 6 );
  jtot = tblcol( f7 );
  { Ratios }
  f7[*,jtot-1] = f7[*,j5] / f7[*,j6];
  f7[*,jtot] = ( f7[*,j5] + f7[*,j5-1] ) / ( f7[*,j6] + f7[*,j6+1] );

  { Table f8a }
  jtot    = tblcol( f8a, ftotal2 ) - 1;
  jmax    = jtot - 1;
  { Percents }
  do j = 0 while j <= jmax
    f8a[*,j] = f8a[*,j] * 100 / f8a[*,jtot];
  enddo;
  { Total percentage }
  f8a[*,jtot-1] = tblsum( column f8a[*,0:jmax] );
  { HH response rate } 
  f8a[*,jtot+1] = f8a[*,0] / ( f8a[*,0] + f8a[*,2] )*100; 

  { Table f8b }
  jtot    = tblcol( f8b, ftotal3 ) - 1;
  jmax    = jtot - 1;
  { Percents }
  do j = 0 while j <= jmax
    f8b[*,j] = f8b[*,j] * 100 / f8b[*,jtot];
  enddo;
  { Total percentage }
  f8b[*,jtot-1] = tblsum( column f8b[*,0:jmax] );
  { HH response rate } 
  f8b[*,jtot+1] = f8b[*,0] / ( f8b[*,0] + f8b[*,2] )*100; 

  { Table f9 }
  jtot    = tblcol( f9, ftotal2 ) - 1;
  jmax    = jtot - 1;
  { Percents }
  do j = 0 while j <= jmax
    f9[*,j] = f9[*,j] * 100 / f9[*,jtot];
  enddo;
  { Total percentage }
  f9[*,jtot-1] = tblsum( column f9[*,0:jmax] );
  { HH response rate } 
  f9[*,jtot+1] = f9[*,0] / ( f9[*,0] + f9[*,2] )*100; 

  { Table f10a }
  jtot    = tblcol( f10a, ftotal1 ) - 2;
  jmax    = jtot - 1;
  { Percents }
  do j = 0 while j <= jmax
    f10a[*,j] = f10a[*,j] * 100 / f10a[*,jtot];
  enddo;
  { Total percentage }
  f10a[*,jtot+1] = tblsum( column f10a[*,0:jmax] );
  { HH response rate } 
  f10a[*,jtot+2] = f10a[*,0] / ( f10a[*,0] + f10a[*,2] )*100; 

  { Table f10b }
  jtot    = tblcol( f10b, ftotal5 ) - 1;
  jmax    = jtot - 1;
  { Percents }
  do j = 0 while j <= jmax
    f10b[*,j] = f10b[*,j] * 100 / f10b[*,jtot];
  enddo;
  { Measured for both height and weight } 
  f10b[*,jtot+1] = f10b[*,jtot+1] * 100 / f10b[*,jtot];

  { Table f11 }
  jtot    = tblcol( f11, ftotal7 );
  jmax    = jtot - 1;
  { Percents }
  do j = 0 while j <= jmax
    f11[*,j] = f11[*,j] * 100 / f11w[*,j];
  enddo;
  f11[*,jtot] = ( f11[*,0] + f11[*,1] + f11[*,2] + f11[*,3] + f11[*,4] + f11[*,5] )/6;

  { Table f12 }
  jtot    = tblcol( f12, ftotal9 );
  jmax    = jtot;
  { Percents }
  do j = 0 while j <= jmax
    f12[*,j] = f12[*,j] * 100 / f12[*,jtot];
  enddo;

  { Table f13 }
  jtot    = tblcol( f13, ftotal8 = 1 );
  jmax    = jtot - 1;
  { Percents }
  do j = 0 while j <= jmax
    f13[*,j] = f13[*,j] * 100 / f13[*,jtot];
  enddo;
  f13[*,jtot+1] = f13[*,jtot+1] * 100 / f13[*,jtot];


PROC PEEL_QUEST
preproc

  { initialize array to identify individuals interviewed }
  do i = 1 while i <= 100
    interview(i) = 0;
  enddo;

  URBRUR  = AHTYPE;
  fintnum = AHINTNUM1;  { !!!! make sure to adjust the value sets based on the actual interviewer codes for the survey }
  fteams = notappl;
  if byteam then
    { !! recode teams as necessary }
    Fteams = int( fintnum/10 );               {!!}
    fintnum = notappl;
  endif;

    fhresult = AHRESULT;
    xtab( f1 );
  { skip incomplete households }
  if AHRESULT <> 1 then skip case endif;



postproc


  { Table f2 }
  ftotal4 = 1;
  xtab( f2 );
  ftotal4 = notappl;
  if count(MOD_1_EDT where V102 = 2 & V104 in 18:95) then
    fprimaryf = 1;
    xtab( f2 );
    if V101B then
      fprimaryf = 2;
      xtab( f2 );
    endif;
  endif;
  fprimaryf = notappl;  
  
  if count(MOD_1_EDT where V102 = 1 & V104 in 18:95) then
    fprimarym = 1;
    xtab( f2 );
    if V101B then
      fprimarym = 2;
      xtab( f2 );
    endif;
  endif;
  fprimarym = notappl;  

  if V101B | V101A then
    fprimary = 1;
    xtab( f2 );
  endif;
  fprimary = notappl;
  
  { Table f4 }
  Fhhmean  = 0;        { number of complete households }
  xtab( f4 );
  Fhhmean = 1;       
  for i in MOD_1_EDT do
    if V102 = 2 & V107 <> 0 then   
      xtab( f4 );
    endif;
  enddo;

  { Table f6 }
    Fhhmeanm = 0;        { number of completed households }
    xtab( f6 );
    Fhhmeanm = 1;        
    for i in MOD_1_EDT do
      if V108 <> 0 then   
        xtab( f6 );
      endif;
    enddo;

  { Table f3, f3w, f5a, f5b, f5c, f5d, f7 }
  for i in MOD_1_EDT do
	box V104 => fageg;
	       5 => 1;
	      10 => 2;
	      15 => 3;
	      20 => 4;
	      25 => 5;
	      30 => 6;
	      35 => 7;
	      40 => 8;
	      45 => 9;
	      50 => 10;
	         => notappl;
	endbox;
	xtab( f3 );
	fageg = notappl;

	box V104  => fagegw;
	     3-7  => 1;
	    8-12  => 2;
	    13-17 => 3;
	    18-22 => 4;
	    23-27 => 5;
	    28-32 => 6;
	    33-37 => 7;
	    38-42 => 8;
	    43-47 => 9;
	    48-52 => 10;
	          => notappl;
	endbox;
	xtab( f3w );
	fagegw = notappl;

	if V104%5 = 0 then 
	  ftotal6 = 1;
	  xtab( f3 );
	endif;
    ftotal6w = 2;
	ftotal6 = 2;
	xtab( f3 );
	xtab( f3w );
    ftotal6w = notappl;
	ftotal6 = notappl;
	  	
    if V102 = 2 & V104 in 12:17 then  
      Fage = V104(i);
      xtab( f5a );
    endif;
    if V102 = 2 & V104 in 47:52 then  
      Fage1 = V104(i);
      xtab( f5b );
    endif;
    if V102 = 2 & V104 in 15:20 then  
      Fage2 = V104(i);
      xtab( f5c );
    endif;
    if V102 = 1 & V104 in 15:20 then  
      Fage3 = V104(i);
      xtab( f5d );
    endif;    
    if V104 in 2:8 then  
      Fage4 = V104(i);
      xtab( f7 );
    endif;    
  enddo;

  { Table f8a, f8b }
  if V101B = 1 then
    ftotal2 = 2;
    box V6605 => fhresult1;
            1 => 1;
            4 => 2;
            5 => 3;
           10 => 4;
           96 => 5;  
    endbox;
    xtab( f8a );
  endif;
  if V101A = 1 & V101B = 1 then
    ftotal3 = 2;
    box M6605 => fhresult1;
            1 => 1;
            4 => 2;
            5 => 3;
           10 => 4;
           96 => 5;  
    endbox;
    xtab( f8b );
  endif; 
  
  { Table f9 }
  for i in MOD_4_INF_EDT do
    ftotal2 = 2;
    box V433 => fhresult1;
            1 => 1;
            4 => 2;
            5 => 3;
           10 => 4;
           96 => 5;  
    endbox;
    xtab( f9 );
  enddo;     

  { Table f10a }
  for i in MOD_5_INF_EDT do
    ftotal1 = 1;
    box V566 => fhresult1;
            1 => 1;
            4 => 2;
            5 => 3;
           10 => 4;
           96 => 5;  
    endbox;
    xtab( f10a );
  enddo;     

  { Table f10b }
  for i in ANTHRO_CHILD_EDT do
    ftotal5 = 1;
    box AN516 => fhresult5;
   	   20-140 => 1;
   	   999.4  => 2;
   	   999.5  => 3;
  	   999.6  => 96;
    endbox;
    box AN518 => fhresult6;
   	  0.5-50  => 1;
   	   99.94  => 2;
   	   99.95  => 3;
   	   99.96  => 96;
    endbox;
    xtab( f10b );
    ftotal5 = notappl;
    
    if fhresult5 = 1 & fhresult6 = 1 then 
      ftotal5 = 2;
      fhresult5 = notappl;
      fhresult6 = notappl;
      xtab( f10b );
    endif;
    ftotal5 = notappl;
    fhresult5 = notappl;
    fhresult6 = notappl;
  enddo;   

  { f11, f11w }
  for i in MOD_5_EDT do
	box V508 => fageg1;
	       6 => 1;
	      12 => 2;
	      18 => 3;
	      24 => 4;
	      30 => 5;
	      36 => 6;
	         => notappl;
	endbox;
	xtab( f11 );
	fageg1 = notappl;

	box V508  => fageg1w;
	     4-8  => 1;
	    10-14 => 2;
	    16-20 => 3;
	    22-26 => 4;
	    28-32 => 5;
	    34-38 => 6;
	          => notappl;
	endbox;
	xtab( f11w );
	fageg1w = notappl;
  enddo;



  { Tables f11ch }
  for i in ANTHRO_CHILD_EDT do
    F13h  = notappl;
    F13w  = notappl;
    ftotal8 = 1;
    xtab( f13 );
    ftotal8 = notappl;
    if V104(V500AD) in 0:5 = 1 & AN516 in 20:140 then            { children under 5 }
      { determine if height for age z-score out of range }
        if V501(AN500X) = 1 then
          box V508(AN500X) :   AN516    => hwerr1;
               0- 2 : 36.0- 74.0 => 0;
               3- 5 : 45.0- 83.0 => 0;
               6- 8 : 51.0- 87.0 => 0;
               9-11 : 56.0- 91.0 => 0;
              12-14 : 59.0- 96.0 => 0;
              15-17 : 62.0-100.0 => 0;
              18-20 : 64.0-104.0 => 0;
              21-23 : 65.0-107.0 => 0;
              24-26 : 67.0-108.0 => 0;
              27-29 : 68.0-112.0 => 0;
              30-32 : 70.0-115.0 => 0;
              33-35 : 71.0-118.0 => 0;
              36-38 : 73.0-121.0 => 0;
              39-41 : 74.0-124.0 => 0;
              42-44 : 75.0-127.0 => 0;
              45-47 : 77.0-129.9 => 0;
              48-50 : 79.0-132.0 => 0;
              51-53 : 79.0-134.0 => 0;
              54-56 : 80.0-136.0 => 0;
              57-60 : 82.0-139.0 => 0;
              61-72 : 82.0-140.0 => 0;
                    :            => 1;
          endbox
        else
          box V508(AN500X) :   AN516    => hwerr1;
               0- 2 : 36.0- 72.0 => 0;
               3- 5 : 44.0- 80.0 => 0;
               6- 8 : 50.0- 86.0 => 0;
               9-11 : 54.0- 90.0 => 0;
              12-14 : 57.0- 95.0 => 0;
              15-17 : 60.0- 99.0 => 0;
              18-20 : 62.0-102.0 => 0;
              21-23 : 64.0-106.0 => 0;
              24-26 : 66.0-107.0 => 0;
              27-29 : 68.0-111.0 => 0;
              30-32 : 69.0-114.0 => 0;
              33-35 : 71.0-117.0 => 0;
              36-38 : 72.0-120.0 => 0;
              39-41 : 74.0-122.0 => 0;
              42-44 : 75.0-124.0 => 0;
              45-47 : 77.0-126.0 => 0;
              48-50 : 78.0-129.0 => 0;
              51-53 : 79.0-131.0 => 0;
              54-56 : 81.0-133.0 => 0;
              57-60 : 81.0-136.0 => 0;
              61-72 : 81.0-137.0 => 0;
                    :            => 1;
          endbox
        endif;
        if hwerr1 then f13h = 2; else f13h = 1; endif; xtab( f13 );
        f13h = notappl;
    endif;    
    if V104(V500D) in 0:5 = 1 & AN518 in 0.5:50 then            { children under 5 }
      { determine if height for age z-score out of range }
        if V501(AN500X) = 1 then
        box V508(AN500X) : AN518        => hwerr2;
             0- 2 :  0.5- 10.0 => 0;
             3- 5 :  1.0- 13.0 => 0;
             6- 8 :  2.0- 15.0 => 0;
             9-11 :  3.0- 16.5 => 0;
            12-14 :  4.0- 17.5 => 0;
            15-17 :  4.0- 18.5 => 0;
            18-20 :  4.0- 19.5 => 0;
            21-23 :  4.5- 20.5 => 0;
            24-26 :  4.5- 23.0 => 0;
            27-29 :  5.0- 24.0 => 0;
            30-32 :  5.0- 24.5 => 0;
            33-35 :  5.0- 25.5 => 0;
            36-38 :  5.0- 26.0 => 0;
            39-41 :  5.0- 27.0 => 0;
            42-44 :  5.0- 28.0 => 0;
            45-47 :  5.0- 29.0 => 0;
            48-50 :  5.0- 30.0 => 0;
            51-53 :  5.0- 31.0 => 0;
            54-56 :  5.5- 32.0 => 0;
            57-60 :  5.5- 33.0 => 0;
            61-72 :  5.5- 34.0 => 0;
                  :            => 1;
        endbox
      else                               { girls }
        box V508(AN500X) : AN518        => hwerr2;
             0- 2 :  0.5-  9.0 => 0;
             3- 5 :  1.0- 12.0 => 0;
             6- 8 :  2.0- 14.0 => 0;
             9-11 :  2.5- 15.5 => 0;
            12-14 :  3.0- 16.5 => 0;
            15-17 :  3.5- 17.5 => 0;
            18-20 :  3.5- 18.5 => 0;
            21-23 :  4.0- 19.5 => 0;
            24-26 :  4.5- 21.5 => 0;
            27-29 :  5.0- 23.0 => 0;
            30-32 :  5.0- 24.5 => 0;
            33-35 :  5.0- 25.5 => 0;
            36-38 :  5.0- 27.0 => 0;
            39-41 :  5.0- 28.0 => 0;
            42-44 :  5.5- 29.0 => 0;
            45-47 :  5.5- 30.0 => 0;
            48-50 :  5.5- 31.0 => 0;
            51-53 :  5.5- 32.0 => 0;
            54-56 :  6.0- 33.0 => 0;
            57-60 :  6.0- 34.5 => 0;
            61-72 :  6.0- 36.0 => 0;
                  :            => 1;
        endbox 
      endif;                                  

        if hwerr2 then f13w = 2; else f13w = 1; endif; xtab( f13 );
        f13w = notappl;
    endif;                           { end children under 6 }
	if V104(V500D) in 0:5 = 1 & AN518 in 0.5:50 & AN516 in 20:140 & !hwerr1 & !hwerr2 then ftotal8 = 2; xtab( f13 ); endif;
  enddo;

  for i in MOD_5_EDT do 
	  if V500F = 1 then
	    box V502M : V506M :     V502Y :     V506Y : V508 => f12c;
		       98 :    98 :      9998 :      9998 :  98  => 4;
		       98 :    98 :      9998 :      9998 :      => 3;
		          :       :           :           :  98  => 2;
		     1-12 :   1-12: 2012-2018 : 2012-2018 : 0-72 => 1;
	
	    endbox;
	    ftotal9 = 1;
	    xtab( f12 );
	  endif;
  enddo;
