
PROC GLOBAL
{ Feed the Future ZOI Survey, secondary editing application - April, 2018 }
{ !! TO DO - expand for country specific context and needs, including all edits/checks that aren't appropriate during CAPI data collection }

  { Counts of error messages that are not displayed }
  array kount(50);

  { --- Event table variables --------------------------------------------------------- }

  array type     (30);        { event type }
  array sflag    (30);        { date flag }
  array smonth   (30);        { if month for event was given }
  array errflag  (30);        { event error flag }
  array lcmc     (30);        { event lower CMC }
  array ucmc     (30);        { event upper CMC }
  array cmc      (30);        { event imputed CMC }
  array interv   (30);        { interval to following event }
  array cdelay   (30);        { delay for pregnancy between events }

 { --- working variables ------------------------------------------------------------------------ }

  numeric lper, lperflag, lsex, lsexflag, fsex, fsexflag, sexp, marrs;
  numeric temp, ycmc, minaiw, maxaiw;
  numeric ouse, fuse, luse, births5, birthsx, chadj, colw, calcmc, usem, muse, preg, xuse, used;
  numeric ismonth, isyear, isday, isage, flag, nEvents, LcmcB, LcmcM, UcmcB, UcmcM, xusual;
  numeric b, e, i, j, k, m, mRow, n, t, v, x, y;
  numeric ch, cy, cm, cd, dy, dm, dd, rp, rc, x1, x2, du, dn, di, xy, xm, xd, vd, vy, vm, rw, rh, eno;
  numeric mind, minai, mindbm, mindbw, minaim, minabm, minam, minamm, minau, minaum, minas, minab, mincd, minpd, lmax, lmin;
  numeric maxd, maxai, maxdbm, maxdbw, maxaim, maxabm, mindb, maxdb;
  numeric maxelig, maxevent, maxmemb, MinAsx;
  numeric xho, xhc, xhi; { # HHs, # HHs completed, # HHs incomplete }
  numeric xww, xwc, xwi, xmm, xmc, xmi, xht, xwt, cmcTerm, tryMatch;
  numeric xrelat, xsex, xlives, xslept, xage, xmother, xfather, xelig, xeligc, xeligm, xusing;
  numeric run1, doimp, dprev, ver100, ignoreY, healthy;
  numeric yage, hwage, err, rok, fline_valid, mLine_valid, hwlcmc, hwucmc, hwsex, ceb, hwerr;
  numeric ageDU, ageDN, ageD, dam, dab, head, ntwin, uMin, agew, agem;
  numeric inUnion, EverUnion;
  numeric gap, gap2, xgap, xDateMin, xDateMax, dMonths, jInterv;
  numeric goodLink, found, xdeath, discont, condom;
  numeric swrt, anew, aold, iold, aoldest, ETsuppress;
  numeric prevCL; { previous cluster # }
  numeric valf, valm, sickmem,deadmem, deadpar, chvulner, termin, xyear, xmonth, xday;
  numeric sleep;
  alpha(1)  asterisk, spaces;     { asterisks and spaces for printing purposes }
  alpha(4) strval;

  { function to check barcode for HIV }
  { barcode follows pattern: letter,digit,letter,digit,letter }
  { return: 0-No errors, 1-error in missing, 2-error in code, 3-error in check digit }
  function BarCodeError( alpha(5) barcode )
     numeric isOK = 0, checkdig = 0, base = 26, z;
    { check sequence of characteres }
    if !barcode in "99994":"99996" then
      if pos("?",barcode) & length(strip(barcode)) <> 1 then
        isOK = 1;
      elseif !pos("?",barcode) then
        do k = 1 until k > 5 by 2
          z = pos( barcode[k:1], "ABCDEFGHIJKLMNOPQRSTUVWXYZ" );
          if !z then
            isOK = 1;
            break;
          elseif k < 5 then
            checkdig = checkdig + z-1;
          endif;
          if k < 5 then
            z = pos( barcode[k+1:1], "0123456789" );
            if !z then
              isOK = 2;
              break;
            else
              checkdig = checkdig + z-1;
            endif;
          endif;
        enddo;
        { Now validate the check digit }
        if !isOK then
          while checkdig >= base do
            checkdig = int(checkdig/base)+checkdig%base;
          enddo;
          if barcode[5:1] <> "ABCDEFGHIJKLMNOPQRSTUVWXYZ"[checkdig+1:1] then
            isOK = 3;
          endif;
        endif;
      endif;
    endif;
    BarCodeError = isOK;
  end;

  { Check relationship of child's mother/father to the household head  }
  { with the child relationship to the head                            }
  function valrelat( parent, child )
    numeric isOK;
    box  parent  : child => isOK;  { MOTHER/FATHER         - CHILD              }
            1,2  :   3   => 1;     { Head/Spouse           - Son/Daughter       }
              2  :  10   => 1;     { Spouse                - Stepchild          }
            3,4  :   5   => 1;     { Son/Daughter (in-law) - Grandchild         }
              5  :   9   => 1;     { Grandchild            - Other Relative     }
              6  :   1   => 1;     { Parent                - Head               }
              7  :   2   => 1;     { Parent-in-law         - Spouse             }
            6,7  :   8   => 1;     { Parent/Parent-in-Law  - Brother/Sister     }
              7  :   9   => 1;     { Parent-in-law         - Other Relative     }
              8  :   9   => 1;     { Brother/Sister        - Other Relative : Niece/nephew-blood }
           {  ?  :  11   => 1; }   { Niece/Nephew          - Other relative     }
           {  ?  :  13   => 1; }   { Niece/Nephew-marriage - Not related        }
              9  :   9   => 1;     { Other Relative        - Other Relative     }
          10,11  :  11   => 1;     { Adopted/Not Related   - Not Related        }
      missing,98 :98,missing => 1; { Unknown             - Unknown            }
  {{POLIG}       :  11   => 1;     { Co-wife               - Not related        } {POLIG}}
                 :       => 0;     { All others are incorrect                     }
    endbox;
    valrelat = isOK;
  end;

  { valid value in a two digits variable }
  function valid( xvar );
    valid = ( !special(xvar) & xvar <= 96 )
  end;

  { valid value in a four digits digits year }
  function validyr( xvar );
    validyr = ( !special(xvar) & xvar <= 9996 )
  end;

  { convert notappl to zero }
  function NAtoZero( xvar );
    if xvar = notappl then
      xvar = 0
    endif;
    NAtoZero = xvar;
  end;

  { convert notappl to zero }
  function NSmoke( xvar );
    if xvar in 888,missing,notappl then
      xvar = 0
    endif;
    NSmoke = xvar;
  end;

  { check if two values are equal }
  function noteq( xvar, dvar );
    noteq = ( xvar <> NAtoZero(dvar) );
  end;

  { Check special answers for questions with units and numbers }
  { !!! make sure to correctly adjust the ranges of the        }
  {     questions involved, for the function to work properly  }
  function badspecial( units, number )
    numeric z = 0;
    if units  = 9 & number <> missing & number <  93 |
       units <> 9 & number <> missing & number >= 93 |
       units  > 1 & number = 0 then
      z = 1;
    endif;
    badspecial = z;
  end;

  { Function to check if a date is after the date of interview }
  function afterint( vcheckm, vchecky, IntY, IntM );
    numeric z = 0;
    if validyr(vchecky) & vchecky > IntY |
       vchecky = IntY & valid(vcheckm) & vcheckm > IntM then
      z = 1
    endif;
    afterint = z;
  end;

  { check that number of years of school according to level is correct }
  function LevelYears( xlevel, xyears )
    { Verify the maximum grade for the level }
    numeric isOK = 1, z;
    box xlevel => z;
           0   => 2; { !!! Preschool maximum }
           1   => 7; { !!! Primary maximum }
           2   => 6; { !!! Secondary maximum }
           3   => 5; { !!! Higher maximum }
    endbox;
    if valid(xyears) & xyears > z then
      isOK = 0;
    endif;
    LevelYears = isOK;
  end;

  { returns the first or second digit (decpos) of a decimal variable }
  function GetDecimal( value, decpos )
    numeric wholeval, intval, decval;
    intval   = int( value + 0.00001 ) * 100;   //to properly round the decimal part
    wholeval = int( value * 100 + 0.00001 );
    decval   = wholeval - intval;
    strval   = edit( "99", decval );
    GetDecimal = tonumber( strval[decpos:1] );
  end;

  { calculates the number of days since january 1, 1900 up to the date given in the parameters }
  { the function assumes that the day (zday) within month (zmonth) is consistent               }
  function CDCode( zyear, zmonth, zday )
    numeric z, zz, zdays1 = 0, zdays2 =0, leapday, totdays = 99999;
    if !zyear in 1900:2170 | !zmonth in 1:12 | !zday in 1:31 then
      errmsg( "Invalid date to calculate CDC Year=%04d, Month=%02d, day=%02d", zyear, zmonth, zday );
    else
      { number of days between 1900 and zyear }
      do z = 1900 while z < zyear
        zdays1 = zdays1 + 365 + (z % 4 = 0);
      enddo;
      { number of days up to the month in year zyear }
      leapday = (zyear % 4 = 0);
      do z = 1 while z < zmonth
        box z          => zz;
          1,3,5,7,8,10 => 31;
                     2 => 28+leapday;
              4,6,9,11 => 30;
        endbox;
        zdays2 = zdays2 + zz;
      enddo;
      { total days }
      totdays = zdays1 + zdays2 + zday;
    endif;
    CDCode = totdays;
  end

  { ramdomly impute a day between 1 and the maximum number of days in a month }
  function ImputeDay( yint, mint, dint, zyear, zmonth )
    numeric zz, leapday;
    { number of days up to the month in year zyear }
    leapday = (zyear % 4 = 0);
    box    zmonth     => zz;
      1,3,5,7,8,10,12 => 31;
                    2 => 28+leapday;
             4,6,9,11 => 30;
    endbox;
    if yint = zyear & mint = zmonth then
      zz = dint
    endif;
    ImputeDay = random( 1, zz );
  end

  { --------------------------------------------------------------------------- }
  { function to adjust lower bound dates based on age }
  function ndjlba( lowera, uppera, lowerb, upperb, agetofit )
    temp = lowerb - 12 * (agetofit + 1);
    if temp > uppera then
      temp = -1
    else
      if temp < uppera then
        temp = temp + 1
      endif;
      if lowera > temp then
        temp = lowera
      endif
    endif;
    ndjlba = temp
  end;

  { --------------------------------------------------------------------------- }
  { Function to get a month from a CMC }
  function cmc2m( xcmc );
    cmc2m = (xcmc-1)%12+1;
  end;

  { Function to get a 4 digits year from a CMC }
  function cmc2y( xcmc );
    cmc2y = int( (xcmc-1)/12 ) + 1900;
  end;

  { function to convert sex related variables to number of days }
  function sexdays( rvar )
    { Recode into days ago }
    box   rvar  => x;
        100-196 => (rvar-100);
        201-296 => (rvar-200)*7;
        301-396 => int((rvar-300)*365/12);
        401-496 => int((rvar-400)*365.25);
                => 9998;
    endrecode;
    sexdays = x;
  end;

{ --------------------------------------------------------------------------- }

{ -------------------------------------------------------------------------- }
PROC SECONDARYEDITING_FF
preproc

  { Initializing the application before the first questionnaire }
  seed(101);            { Seed to start the random number generator }
  CSAMPLE = 0;          { Initialize cluster number }

  { set application parameters }
  minai    = 15;        { !!! Minimum age at interview - women         }
  maxai    = 49;        { !!! Maximum age at interview - women         }
  {+MEN}
  minaiw   = 15;        { !!! Minimum age at interview - men           }
  maxaiw   = 95;        { !!! Maximum age at interview - men           }
  maxelig  = 20;        { !!! Maximum number of eligible men or women  }
  {MEN+}
  maxevent = 30;        { !!! Maximum number of events                 }
  maxmemb  = 50;        { !!! Maximum number of household members      }
  doimp    = 0;         { 1 if doing imputation - 0 if no imputation   }
  dprev    = 0;         { 1 if doing DP review at end of fieldwork     }
  ver100   = 1;         { 1 if 100% verification                       }
  ignorey  = 0;         { 1 if ignoring year when age + year = interview year
                          0 if using year when age + year = interview year  }
  healthy  = 2010;      { !!! Year of start of health section         }

  prevCL   = notappl;           { previous cluster number }

{ --------------------------------------------------------------------------- }
postproc 	{ of PROC CORE7_FF }

  if !doimp then
    CSAMPLE = HHEA;
    open( CONTROL );
    if loadcase( CONTROL, CSAMPLE ) then
      if  xho = chtotal
        & xhc = chcomp
        & xhi = chincomp
      then
        do x = 1 while x <= 3 & CEDIT(x)
          { find the first empty entry in CEDIT }
        enddo;
        CEDIT(x) = SYSDATE();
        if !writecase( CONTROL ) then
          errmsg( 0001, CSAMPLE )
        endif
      else
        errmsg( 0002, CSAMPLE,
                xho,    xhc,   xhi,    
                chtotal,chcomp,chincomp)
      endif
    endif;
    close( CONTROL );
  endif;

  if dprev then
    errmsg( 90000, 0460, kount(28) );
    errmsg( 90000, 2360, kount(1) );
    errmsg( 90000, 2362, kount(12) );
    errmsg( 90000, 2364, kount(13) );
    errmsg( 90000, 2365, kount(14) );
    errmsg( 90000, 2366, kount(15) );
    errmsg( 90000, 2367, kount(16) );
    errmsg( 90000, 2368, kount(17) );
    errmsg( 90000, 2369, kount(18) );
    errmsg( 90000, 4050, kount(2) );
    errmsg( 90000, 5152, kount(5) );
    errmsg( 90000, 5153, kount(6) );
    errmsg( 90000, 5154, kount(7) );
    errmsg( 90000, 5155, kount(8) );
    errmsg( 90000, 5156, kount(9) );
    errmsg( 90000, 5157, kount(10) );
    errmsg( 90000, 5158, kount(11) );
    errmsg( 90000, 5191, kount(24) );
    errmsg( 90000, 5192, kount(25) );
    errmsg( 90000, 5193, kount(26) );
    errmsg( 90000, 5194, kount(27) );
    errmsg( 90000, 9814, kount(19) );
    errmsg( 90000, 9815, kount(20) );
    errmsg( 90000, 9815, kount(21) );
    errmsg( 90000, 9914, kount(31) );
    errmsg( 90000, 9914, kount(32) );
    errmsg( 90000, 9915, kount(33) );
    errmsg( 90000, 9915, kount(34) );
    errmsg( 90000, 9916, kount(35) );
    errmsg( 90000, 9916, kount(36) );
    errmsg( 90000, 9917, kount(37) );
    errmsg( 90000, 9917, kount(38) );
    {+MEN}
    errmsg( 90000, 25153, kount(48) );
    errmsg( 90000, 25158, kount(49) );
    errmsg( 90000, 25191, kount(44) );
    errmsg( 90000, 25192, kount(45) );
    errmsg( 90000, 25193, kount(46) );
    errmsg( 90000, 25194, kount(47) );
    {MEN+}
  endif;



{ --------------------------------------------------------------------------- }
PROC CFTF_QUEST
preproc

  di = cmcode( AHINTM, AHINTY );

  { Check totals from previous cluster -- when running with multiple clusters }
  if CSAMPLE <> HHEA & CSAMPLE <> 0 then

    open( CONTROL );
    if loadcase( CONTROL, CSAMPLE ) then
      if  xho = chtotal
        & xhc = chcomp
        & xhi = chincomp

      then
        do x = 1 while x <= 3 & CEDIT(x)
          { find the first empty entry in CEDIT }
        enddo;
        CEDIT(x) = sysdate();
        if !writecase( CONTROL ) then
          errmsg( 0001, CSAMPLE )
        endif
      else
        errmsg( 0002, CSAMPLE,
                xho, xhc, xhi, chtotal, chcomp, chincomp)
      endif
    endif;
    close( CONTROL );
  endif;

  { Check this cluster is ready for checking }
  if HHEA <> CSAMPLE then
    CSAMPLE = HHEA;
    open( CONTROL );
    if loadcase( CONTROL, CSAMPLE ) then
        run1 = (CEDIT(1) = 0);
        xho = 0;              { Counts of questionnaires }
        xhc = 0;
        xhi = 0;
    else
      errmsg( 0003, CSAMPLE );
      stop
    endif;
    close( CONTROL );


  endif;

  { it is important to load the weights in the preproc because they are needed by the individual }
  { questionnaires.  However, weights for HIV must be assigned at the postproc because it is     }
  { necessary to know if the individual was completed, incomplete individuals are assigned  "0"  }
  if doimp then
  { !!!! uncomment at imputation time }
    AHINTC  = cmcode( AHINTM, AHINTY );
    { Following code to add weights to the data set }
    if HHEA <> prevCL then
      loadcase( WEIGHTS ); { !!! adjust this file as necessary }
    endif;
    if AHRESULT = 1 then
      WGT_HH = HHWEIGHT;         { !!! Household weight (total) in weights dictionary }
    else
      WGT_HH = 0;
    endif;
    prevCL = HHEA;   { update here }
  endif;

  { Set run1 on if doing DP review, but not yet ready for imputation }
  if dprev & !doimp then
    run1 = 1;
  endif;

  xho   = xho + 1;
  if AHRESULT = 1 then
    xhc = xhc + 1
  else
    xhi = xhi + 1
  endif;
  
  { Set up CMC for date of interview }
  di = cmcode( AHINTM, AHINTY );

    mindbm = di - (maxaiw+1)*12 + 1;     { Minimum date of birth - men }
    maxdbm = di - minaiw*12;             { Maximum date of birth - men }
    xmm   = xmm + 1;


    mindbw = di - (maxaiw+1)*12 + 1;      { Minimum date of birth - women WEAI }
    maxdbw = di - minaiw*12;              { Maximum date of birth - women WEAI }
    xww   = xww + 1;

    mindb = di - (maxai+1)*12 + 1;      { Minimum date of birth - women nutrition }
    maxdb = di - minai*12;              { Maximum date of birth - women nutrition }
{---------------------------------------------------------------------- }
{ respondent background questions }
PROC MOD_1_EDT


  { Time of interview } 
  if AHTOTVISITS = 1 then
    if valid(V100H) & valid(V112H) then
      if V100H > V112H then
        errmsg( 0070, "V100", V100H, V100M, "V112", V112H, V112M );
      elseif V100H = V112H then
        if valid(V100M) & valid(V112M) & V100M > V112M then
          errmsg( 0070, "V100", V100H, V100M, "V112", V112H, V112M );
        endif
      endif
    endif
  endif;

{ --------------------------------------------------------------------------- }

  { Only one head allowed }
  temp = count( $ where V103 = 1 );
  if temp <> 1 then
    errmsg( 0010, temp )
  endif;

  { Only one spouse, parent, parent-in-law allowed }
  if count( $ where V103 = 2 {{+POLIG} & V102 = 1 {POLIG+}} ) >= 2  { !! TO DO - uncomment if poligamy in country }
   | count( $ where V103 = 6 & V102 = 1 ) >= 2
   | count( $ where V103 = 6 & V102 = 2 ) >= 2
   | count( $ where V103 = 12 & V102 = 1 ) >= 2
   | count( $ where V103 = 12 & V102 = 2 ) >= 2 then
      if run1 & !dprev then errmsg( 0011 ) endif;
  endif;
  
	{ Primary decisionmakers should be male first, female second }
  if V101A = 1 & V101B = 1 then
    if V103(1) <> 1 | V102(1) <> 1 | V102(2) <> 2 then 
      errmsg( 0012, V101A, V101B, V102(1), V103(1), V102(2), V103(2) );
    endif;
  elseif V101A = 1  then 
    if V103(1) <> 1 | V102(1) <> 1 then 
      errmsg( 0012, V101A, V101B, V102(1), V103(1), 0, 0 );
    endif;
  elseif V101B = 1 then 
    if V103(1) <> 1 | V102(1) <> 2  then 
      errmsg( 0012, V101A, V101B, V102(1), V103(1), 0, 0 );
    endif;
  else
    if V103(1) <> 16 then 
      errmsg( 0012, V101A, V101B, V102(1), V103(1), 0, 0 );
    endif;
  endif;

  for i in $ do

    xsex    = V102;
    xrelat  = V103;
    xage    = V104;
    xslept  = V105B;
    xusual  = V105A;
    xelig   = V107;
    xeligc  = V108;

    { Check eligibility }
    { Eligible woman }
    if xsex = 2 & xage in minai:maxai then
      if xelig <> i then
        errmsg( 0013, i, xsex, xage, "V107", xelig );
      endif
    else
      if !xelig in 0,notappl then { notappl is for lines where there is no PDM }
        errmsg( 0013, i, xsex, xage, "V107", xelig );
      endif
    endif;

    { Eligible child }
    if xage in 0:5 then
      if xeligc <> i then
        errmsg( 0013, i, xsex, xage, "V108", xeligc );
      endif
    else
      if !xeligc in 0,notappl then { notappl is for lines where there is no PDM }
        errmsg( 0013, i, xsex, xage, "V108", xeligc );
      endif
    endif;


{ --------------------------------------------------------------------------- }

    { Check relationships }
    { ------------------- }

    { Check age of PDMs or spouse is at least 12 }
    if xrelat in 1,2 & xage in 0:11 then
      if run1 then errmsg( 0014, i, xage, 12 ) endif;
    endif;
    if V101A = 1 & V101B = 1 then 
      if V104(1) in 0:17 then 
        if run1 then errmsg( 0014, 1, V104(1), 17 ) endif;
      elseif V104(2) in 0:17 then
        if run1 then errmsg( 0014, 2, V104(2), 17 ) endif;
	  endif;      
    endif;

    { Check age of household head, 12 years younger than parents }
    if xrelat = 6 & xage <> missing then
      do j = 1 while j <= AHMEMBER
        if V103(j) = 1 then
          yage = V104(j);
          if yage in 0:97 & yage+12 > xage then
            if run1 then errmsg( 0015, j, yage, i, xage ) endif;
          endif;
          break;
        endif;
      enddo;
    endif;

    { Check age of spouse, 12 years younger than parents }
    if xrelat = 12 & xage <> missing then
      do j = 1 while j <= AHMEMBER
        if V103(j) = 2 then
          yage = V104(j);
          if yage in 0:97 & yage+12 > xage then
            if run1 then errmsg( 0016, j, yage, i, xage ) endif;
          endif;
          break;
        endif;
      enddo;
    endif;

    { Check spouse is opposite sex from first PDM }
    if xrelat = 2 then
      do j = 1 while j <= AHMEMBER
        if V103(j) = 1 then
          if V102(j) = xsex then
            errmsg( 0017, i, xsex, i, V102(j), j );
          endif;
          break;
        endif;
      enddo;
    endif;


  enddo; { end loop through HH members }

{ -------------------------------------------------------------------------- }
{ number of eligible women and men with cover page }
PROC MOD_1B_EDT
  
  { Existing PDMs vs eligibility }  
  if V101A = 2 & count(MOD_1_EDT where V102 = 1 & V104 in 18:95) then 
    errmsg( 0020, "male");
  endif;
  if V101B = 2 & count(MOD_1_EDT where V102 = 2 & V104 in 18:95) then 
    errmsg( 0020, "female");
  endif;
  
  
PROC MOD_2_EDT 

  { Time of interview } 
  if AHTOTVISITS = 1 then
    if valid(V200H) & valid(V244H) then
      if V200H > V244H then
        errmsg( 0070, "V7200", V200H, V200M, "V244", V244H, V244M );
      elseif V200H = V244H then
        if valid(V200M) & valid(V244M) & V200M > V244M then
          errmsg( 0070, "V7200", V200H, V200M, "V244", V244H, V244M );
        endif
      endif
    endif
  endif;


  x = count( MOD_1_EDT where V102 = 2 & V104 in minai:maxai);  
  if x <> AHWOMEN then
    errmsg( 0018, "women", AHWOMEN, x )
  endif;
  x = count( MOD_1_EDT where V104 in 0:5 );   
  if x <> AHKIDS then
    errmsg( 0018, "children", AHKIDS, x )
  endif;

{ --------------------------------------------------------------------------- }
PROC MOD_3_EDT

  { Time of interview } 
    if valid(V300H) & valid(V365H) then
      if V300H > V365H then
        errmsg( 0070, "V300", V300H, V300M, "V365", V365H, V365M );
      elseif V300H = V365H then
        if valid(V300M) & valid(V365M) & V300M > V365M then
          errmsg( 0070, "V300", V300H, V300M, "V365", V365H, V365M );
        endif
      endif
    endif;
  
  
PROC MOD_4_EDT

  for i in MOD_4_EDT do 
  { Time of interview } 
    if valid(V400H(i)) & valid(V431H(i)) then
      if V400H(i) > V431H(i) then
        errmsg( 0070, concat("V400(",edit("Z",i),")"), V400H(i), V400M(i), concat("V431(",edit("Z",i),")"), V431H(i), V431M(i) );
      elseif V400H(i) = V431H(i) then
        if valid(V400M(i)) & valid(V431M(i)) & V400M(i) > V431M(i) then
          errmsg( 0070, concat("V400(",edit("Z",i),")"), V400H(i), V400M(i), concat("V431(",edit("Z",i),")"), V431H(i), V431M(i) );
        endif
      endif
    endif;
  

  { Date of birth of the woman }

    { Check either year of birth or age given for all women }
    if V400E = 1 then
    
      if !validyr(V401Y(i)) & !valid(V402(i)) then
        if run1 then errmsg( 0040, i, V401M(i), V401Y(i), V402(i) ); endif;
    { Compare age of woman with age in household to look for typos }
      elseif valid(V402(i)) & !( (V402(i) - V104(V400D(i))) in (-2):2 ) then
        if run1 then errmsg( 0041, i, V402(i), V400D(i), V104(V400D(i)) ) endif;
      endif;
    endif;
  enddo;
    
PROC ANTHRO_WOMEN_EDT

  for i in ANTHRO_WOMEN_EDT do 
  { Time of interview } 
      if valid(V400AAH(i)) & valid(AN408H(i)) then
        if V400AAH(i) > AN408H(i) then
          errmsg( 0070, concat("V400AA(",edit("Z",i),")"), V400AAH(i), V400AAM(i), concat("V408(",edit("Z",i),")"), AN408H(i), AN408M(i) );
        elseif V400AAH(i) = AN408H(i) then
          if valid(V400AAM(i)) & valid(AN408M(i)) & V400AAM(i) > AN408M(i) then
            errmsg( 0070, concat("V400AA(",edit("Z",i),")"), V400AAH(i), V400AAM(i), concat("V408(",edit("Z",i),")"), AN408H(i), AN408M(i) );
          endif
        endif
      endif;
   enddo;


  for i in $ do
    if V400E(i) = 1 then
    xht = AN406;
    xwt = AN407;
    hwage = V104(V400AAD);

    { weight for women }
    if run1 & !xwt in 20.0:150.0,999.94:999.96,missing & AN405 <> 1  then
      errmsg( 0030, "Woman", i, V400AAD, "AN407", xwt, hwage, 2 )
    endif;

    { height for women }
    if run1 & !xht in 100.0:200.0,999.4:999.6,missing & AN405 <> 1  then
      errmsg( 0031, "Woman", i, V400AAD, "AN406", xht, hwage, 2 )
    endif;
    endif;
  enddo;


{ --------------------------------------------------------------------------- }
PROC MOD_5_EDT


  for i in MOD_5_EDT do 
  { Time of interview } 
      if valid(V500H(i)) & valid(V564H(i)) then
        if V500H(i) > V564H(i) then
          errmsg( 0070, concat("V500(",edit("Z",i),")"), V500H(i), V500M(i), concat("V564(",edit("Z",i),")"), V564H(i), V564M(i) );
        elseif V500H(i) = V564H(i) then
          if valid(V500M(i)) & valid(V564M(i)) & V500M(i) > V564M(i) then
            errmsg( 0070, concat("V500(",edit("Z",i),")"), V500H(i), V500M(i), concat("V564(",edit("Z",i),")"), V564H(i), V564M(i) );
          endif
        endif
      endif;
  enddo;
  
  
  for i in MOD_5_EDT do
    if validyr(V502Y) then 
      hwlcmc = setlb( V502M, V502Y, 0 );
      hwucmc = setub( V502M, V502Y, 9999 );
    elseif validyr(V506Y) then 
      hwlcmc = setlb( V506M, V506Y, 0 );
      hwucmc = setub( V506M, V506Y, 9999 );
    else
      hwlcmc = di-71;
      hwucmc = di;
    endif;

   if V501(i) <> V102(V500D) then 
     errmsg( 0034, V501(i), V500D, V102(V500D) )
   endif;
   if V508(i) <> V104(V500D) then 
     errmsg( 0035, V508(i), V500D, V104(V500D) )
   endif;

    hwsex = V501(i);
    hwage = di - hwlcmc;
    xht = AN516(i);                       { height }
    xwt = AN518(i);                       { weight }

    if !xwt in missing,notappl,99.94:99.96 then
      if hwsex = 1 then                  { boys }
        box hwage : xwt        => hwerr;
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
        box hwage : xwt        => hwerr;
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
      if hwerr & run1 then
        errmsg( 0030, "Child", i, V500D, "AN518", xwt, hwage, hwsex )
      endif;
      if !GetDecimal( AN518(i), 2 ) in 0,5 then
        errmsg( 0032, i, V500D, AN518(i) );
      endif;
    endif;

    if !xht in missing,notappl,999.4:999.6 then
      if hwsex = 1 then                  { boys }
        box hwage : xht        => hwerr;
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
      else                               { girls }
        box hwage : xht        => hwerr;
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
      if hwerr & run1 then
        errmsg( 0031, "Child", i, V500D, "AN516", xht, hwage, hwsex )
      endif
    endif;

    { Measured lying up to 23 months, standing from 24 months }
    if (di - hwlcmc) < 24 & AN517(i) = 2 |
       (di - hwucmc) > 23 & AN517(i) = 1 then
      if !ver100 then errmsg( 0033, i, V500D, AN517(i), di-hwlcmc ) endif;
      kount(28) = kount(28) + 1;
    endif;


  enddo;

{ --------------------------------------------------------------------------- }

PROC ANTHRO_CHILD_EDT

  for i in ANTHRO_CHILD_EDT do 
  { Time of interview } 
      if valid(V500AAH(i)) & valid(V519H(i)) then
        if V500AAH(i) > V519H(i) then
          errmsg( 0070,  concat("V500AA(",edit("Z",i),")"), V500AAH(i), V500AAM(i),  concat("V519(",edit("Z",i),")"), V519H(i), V519M(i) );
        elseif V500AAH(i) = V519H(i) then
          if valid(V500AAM(i)) & valid(V519M(i)) & V500AAM(i) > AN519M(i) then
            errmsg( 0070,  concat("V500AA(",edit("Z",i),")"), V500AAH(i), V500AAM(i),  concat("V519(",edit("Z",i),")"), V519H(i), V519M(i) );
          endif
        endif
      endif;
  enddo;

PROC MOD_61F_EDT

  { Time of interview } 
      if valid(V6100H) & valid(V6604H) then
        if V6100H > V6604H then
          errmsg( 0070, "V6100", V6100H, V6100M, "V6604", V6604H, V6604M );
        elseif V6100H = V6604H then
          if valid(V6100M) & valid(V6604M) & V6100M > V6604M then
            errmsg( 0070, "V6100", V6100H, V6100M, "V6604", V6604H, V6604M );
          endif
        endif
      endif;

  if V6100D = 1 then
          
  inUnion = ( V6105 in 1,2 ); 

  { Date of birth of the woman }

  { Check either year of birth or age given for all women }
  if !validyr(V6101Y) & !valid(V6102) then
    if run1 then errmsg( 0050, V6101M, V6101Y, V6102 ); endif;
  { Compare age of woman with age in household to look for typos }
  elseif valid(V6102) & !( (V6102 - V104(V6100C)) in (-2):2 ) then
    if run1 then errmsg( 0051, V6102, V6100C, V104(V6100C) ) endif;
  endif;

  { Set up event table entry for date of birth of woman }
  nevents = 1;
  ismonth = valid(V6101M);
  isyear  = validyr(V6101Y);
  isage   = valid(V6102);
  if ismonth then
    smonth(1) = V6101M;
  else
    smonth(1) = 0;
  endif;
  box isyear : ismonth : isage => flag;
           1 :       1 :       => 1;
             :       1 :     1 => 2;
           1 :         :     1 => 3;
           1 :         :       => 5;
             :         :     1 => 6;
             :       1 :       => 7;
             :         :       => 8;
  endbox;
  if ignorey & flag = 3 & V6101Y + V6102 = AHINTY then
    flag = 4
  endif;
  sflag(1)   = flag;
  errflag(1) = notappl;

  { Initial ranges for CMC date of birth }
  if validyr(V6101Y) & (flag <> 4 | !doimp) then
    lcmcb = setlb( V6101M, V6101Y, 0 );
    ucmcb = setub( V6101M, V6101Y, 9999 );
    if lcmcb < mindb then
      if ucmcb < mindb then
        errmsg( 0052, V6101M, V6101Y, AHINTM, AHINTY, cmc2m(mindb), cmc2y(mindb), cmc2m(maxdb), cmc2y(maxdb) );
        errflag(1) = default;
        ucmcb = mindb;
      endif;
      lcmcb = mindb;
    endif;
    if ucmcb > maxdb then
      if lcmcb > maxdb then
        errmsg( 0052, V6101M, V6101Y, AHINTM, AHINTY, cmc2m(mindb), cmc2y(mindb), cmc2m(maxdb), cmc2y(maxdb) );
        errflag(1) = default;
        lcmcb = maxdb;
      endif;
      ucmcb = maxdb;
    endif
  else
    lcmcb = mindb;
    ucmcb = maxdb;
  endif;

  type(1) = 1;
  interv(1) = 0;
  cdelay(1) = 0;

{ --------------------------------------------------------------------------- }

  { Adjust ranges for CMC date of birth based on age }
  if V6102 in minai:maxai then
    t = ndjlba( lcmcb, ucmcb, di, di, V6102 );
    if t < 0 then
      errmsg( 0053, V6101M, V6101Y, lcmcb, ucmcb, V6102, AHINTM, AHINTY, di );
      errflag(1) = default;
    else
      lcmcb = t;
    endif;
    t = adjuba( lcmcb, ucmcb, di, di, V6102 );
    if t < 0 then
      errmsg( 0053, V6101M, V6101Y, lcmcb, ucmcb, V6102, AHINTM, AHINTY, di );
      errflag(1) = default;
    else
      ucmcb = t;
    endif;
    x = di - (V6102*12 + 11);
    if lcmcb < x & x <= ucmcb then
      lcmcb = x
    endif;
  endif;

{ --------------------------------------------------------------------------- }

  { For month only without year, make adjustment }
  if ismonth & !isyear then
    x = int((lcmcb - 1)/12) * 12 + smonth(1);
    if x < lcmcb then
      x = x + 12
    endif;
    if x > ucmcb then
      if run1 then
        errmsg( 0054, smonth(1), lcmcb, ucmcb, cmc2m(lcmcb), cmc2y(lcmcb), cmc2m(ucmcb), cmc2y(ucmcb))
      endif;
    else
      lcmcb = x
    endif;
    x = int((ucmcb - 1)/12) * 12 + smonth(1);
    if x > ucmcb then
      x = x - 12
    endif;
    if x < lcmcb then
      if run1 then
        errmsg( 0054, smonth(1), lcmcb, ucmcb, cmc2m(lcmcb), cmc2y(lcmcb), cmc2m(ucmcb), cmc2y(ucmcb) )
      endif;
    else
      ucmcb = x
    endif;
  endif;

{ --------------------------------------------------------------------------- }

  { Date of marriage }
  everUnion = ( V6105 in 1,2 | V6106 in 1,2 );
  chadj = 1;                    { Adjustment for children in event table }
  if everUnion then              { added to line number of child          }
    chadj = 2;
    nevents = nevents + 1;
  endif;


{ --------------------------------------------------------------------------- }


    lcmc(2) = lcmcm;
    ucmc(2) = ucmcm;
    interv(2) = minam;
    cdelay(2) = 0;


  lcmc(1) = lcmcb;
  ucmc(1) = ucmcb;



{ --------------------------------------------------------------------------- }
{ Husband and Woman's Background, serve mainly as an anchor for imputation    }

    { Impute date of birth of respondent }
    if lcmc(1) > ucmc(1) then
      errmsg( 0090, 1, lcmc(1), ucmc(1), cmc2m(lcmc(1)), cmc2y(lcmc(1)), cmc2m(ucmc(1)), cmc2y(ucmc(1)) );
      cmc(1) = default
    elseif lcmc(1) = ucmc(1) then
      cmc(1) = lcmc(1)
    else
      cmc(1) = random(lcmc(1),ucmc(1))
    endif;
    { Fix imputed date if a month given }
    if smonth(1) & (smonth(1) <> ((cmc(1) - 1) % 12) + 1) then
      x = int((cmc(1) - 1) / 12) * 12 + smonth(1);
      if x > ucmc(1) then x = x - 12 endif;
      if x < lcmc(1) then x = x + 12 endif;
      if x <= ucmc(1) then
        cmc(1) = x
      else
        errmsg( 0091, 1, smonth(1), lcmc(1), ucmc(1), cmc2m(lcmc(1)), cmc2y(lcmc(1)),
                      cmc2m(ucmc(1)), cmc2y(ucmc(1)), cmc(1), cmc2m(cmc(1)), cmc2y(cmc(1)) );
      endif;
    endif;


    V6101C = cmc(1);                     { CMC date of birth }
    V6101F = sflag(1);                   { Flag for date of birth }
    V6102C = int( (di-V6101C)/12 );       { Computed age of respondent }

  endif; { end V6100D = 1 }


PROC MOD_62F_EDT

  for i in MOD_62F_EDT do 
    if pos("B",V6202) & !V6105 in 1,2 then
      errmsg( 0060, "V6202",V6200C, V6202, V6105 );
    endif;
  enddo;
  
PROC MOD_63AF_EDT

  for i in MOD_63AF_EDT do 
    if pos("B",V6303) & !V6105 in 1,2 then
      errmsg( 0060, "V6303",V6300AC, V6303, V6105 );
    endif;
  enddo;
PROC MOD_63BF_EDT

  for i in MOD_63BF_EDT do 
    if pos("B",V6309) & !V6105 in 1,2 then
      errmsg( 0060, "V6309",V6300BC, V6309, V6105 );
    endif;
    if pos("B",V6310) & !V6105 in 1,2 then
      errmsg( 0060, "V6310",V6300BC, V6310, V6105 );
    endif;    
  enddo;
PROC MOD_66AF_EDT

  { Sleep is in between 3 and 12 hours }
  sleep = ( count(MOD_66AF where V6601P_15 = "A") + count(MOD_66AF where V6601P_30 = "A") + count(MOD_66AF where V6601P_45 = "A") + count(MOD_66AF where V6601P_60 = "A") )/4;  
  if ( sleep < 3 ) | ( sleep > 12 ) then 
    if run1 | dprev then 
      errmsg( 0071, "WEAI(Female) A - sleeping and resting - ", sleep );
    endif;
  endif;
  
  { Traveling vs. commuting check }
  for i in MOD_66BF do 
    if V6601P_15(i) = "R" & ( V6601P_15(i-1) in "D":"H" | V6601P_15(i+1) in "D":"H" ) then 
      if run1 | dprev then 
        errmsg( 0074, "R - traveling (not for work or school) - ", V6601P_15(i-1), V6601P_15(i), V6601P_15(i+1), V6601H(i), 15);
      endif;
      break;
    endif;
    if V6601P_30(i) = "R" & ( V6601P_30(i-1) in "D":"H" | V6601P_30(i+1) in "D":"H" ) then 
      if run1 | dprev then 
        errmsg( 0074, "R - traveling (not for work or school) - ", V6601P_30(i-1), V6601P_30(i), V6601P_30(i+1), V6601H(i), 15);
      endif;
      break;
    endif;
    if V6601P_45(i) = "R" & ( V6601P_45(i-1) in "D":"H" | V6601P_45(i+1) in "D":"H" ) then 
      if run1 | dprev then 
        errmsg( 0074, "R - traveling (not for work or school) - ", V6601P_45(i-1), V6601P_45(i), V6601P_45(i+1), V6601H(i), 15);
      endif;
      break;
    endif;
    if V6601P_60(i) = "R" & ( V6601P_60(i-1) in "D":"H" | V6601P_60(i+1) in "D":"H" ) then 
      if run1 | dprev then 
        errmsg( 0074, "R - traveling (not for work or school) - ", V6601P_60(i-1), V6601P_60(i), V6601P_60(i+1), V6601H(i), 15);
      endif;
      break;
    endif;
  enddo;    

  { Outdoor work activities at night }
  for i in MOD_66BF do 
    if ( V6601P_15(i) in "G":"J" | V6601P_30(i) in "G":"J" | V6601P_45(i) in "G":"J" | V6601P_60(i) in "G":"J" ) & V6601H(i) in 0:5,19:23 then
      if run1 | dprev then
        errmsg( 0075, V6601P_15(i), V6601P_30(i), V6601P_45(i), V6601P_60(i), V6601H(i) );
      endif;
      break;
    endif;
  enddo;  
  
{ -------------------------------------------------------------------------- }

PROC MOD_61M_EDT 

  { Time of interview } 
      if valid(M6100H) & valid(M6604H) then
        if M6100H > M6604H then
          errmsg( 0070, "M6100", M6100H, M6100M, "M6604", M6604H, M6604M );
        elseif M6100H = M6604H then
          if valid(M6100M) & valid(M6604M) & M6100M > M6604M then
            errmsg( 0070, "M6100", M6100H, M6100M, "M6604", M6604H, M6604M );
          endif
        endif
      endif;

  inunion  = ( M6105 in 1,2 );

{ -------------------------------------------------------------------------- }

  { Date of birth of the man }
  { Check either year of birth or age given for all men }
  if M6100D = 1 then
  if !validyr(M6101Y) & !valid(M6102) then
    if run1 then errmsg( 1050, M6101M, M6101Y, M6102 ); endif;
  { Compare age of man with age in household to look for typos }
  elseif valid(M6102) & !( M6102 - V104(M6100C) in (-2):2 ) then
    if run1 then errmsg( 1051, M6102, M6100C, V104(M6100C) ) endif;
  endif;
  endif;

  { Set up event table entry for date of birth of man }
  nevents = 1;
  ismonth = valid(M6101M);
  isyear  = validyr(M6101Y);
  isage   = valid(M6102);
  if ismonth then
    smonth(1) = M6101M;
  else
    smonth(1) = 0;
  endif;
  box isyear : ismonth : isage => flag;
           1 :       1 :       => 1;
             :       1 :     1 => 2;
           1 :         :     1 => 3;
           1 :         :       => 5;
             :         :     1 => 6;
             :       1 :       => 7;
             :         :       => 8;
  endbox;
  if ignorey & flag = 3 & M6101Y + M6102 = AHINTY then
    flag = 4
  endif;
  sflag(1)   = flag;
  errflag(1) = notappl;

  { Initial ranges for CMC date of birth }
  if validyr(M6101Y) & ( flag <> 4 | !doimp ) then
    lcmcb = setlb( M6101M, M6101Y, 0 );
    ucmcb = setub( M6101M, M6101Y, 9999 );
    if lcmcb < mindb then
      if ucmcb < mindb then
        errmsg( 1053, M6101M, M6101Y, AHINTM, AHINTY, cmc2m(mindb),
                       cmc2y(mindb), cmc2m(maxdb), cmc2y(maxdb) );
        errflag(1) = default;
        ucmcb = mindb;
      endif;
      lcmcb = mindb;
    endif;
    if ucmcb > maxdb then
      if lcmcb > maxdb then
        errmsg( 1053, M6101M, M6101Y, AHINTM, AHINTY,
                       cmc2m(mindb), cmc2y(mindb), cmc2m(maxdb), cmc2y(maxdb) );
        errflag(1) = default;
        lcmcb = maxdb;
      endif;
      ucmcb = maxdb;
    endif
  else
    lcmcb = mindb;
    ucmcb = maxdb;
  endif;

  type(1) = 1;
  interv(1) = 0;
  cdelay(1) = 0;

{ -------------------------------------------------------------------------- }

  { Adjust ranges for CMC date of birth based on age }
  if M6102 in minaim:maxaim then
    t = ndjlba( lcmcb, ucmcb, di, di, M6102 );
    if t < 0 then
      errmsg( 1052, M6101M, M6101Y, lcmcb, ucmcb, M6102, AHINTM, AHINTY, di );
      errflag(1) = default;
    else
      lcmcb = t;
    endif;
    t = adjuba( lcmcb, ucmcb, di, di, M6102 );
    if t < 0 then
      errmsg( 1052, M6101M, M6101Y, lcmcb, ucmcb, M6102, AHINTM, AHINTY, di );
      errflag(1) = default;
    else
      ucmcb = t;
    endif;
    x = di - (M6102*12 + 11);
    if lcmcb < x & x <= ucmcb then
      lcmcb = x
    endif
  endif;

{ -------------------------------------------------------------------------- }

  { For month only without year, make adjustment }
  if ismonth & !isyear then
    x = int( (lcmcb - 1)/12 ) * 12 + smonth(1);
    if x < lcmcb then
      x = x + 12
    endif;
    if x > ucmcb then
      if run1 then errmsg( 1054, smonth(1), lcmcb, ucmcb, cmc2m(lcmcb), cmc2y(lcmcb), cmc2m(ucmcb), cmc2y(ucmcb) ) endif;
    else
      lcmcb = x
    endif;
    x = int( (ucmcb - 1)/12 ) * 12 + smonth(1);
    if x > ucmcb then
      x = x - 12
    endif;
    if x < lcmcb then
      if run1 then errmsg( 1054, smonth(1), lcmcb, ucmcb, cmc2m(lcmcb), cmc2y(lcmcb), cmc2m(ucmcb), cmc2y(ucmcb) ) endif;
    else
      ucmcb = x
    endif;
  endif;


  { For check on "Duration of current residence against age" -> see DHS5 errmsg 21030 }

{ -------------------------------------------------------------------------- }

  { Date of marriage }
  everUnion = ( M6105 in 1,2 | M6106 in 1,2 );
  chadj = 1;                     { Adjustment for children in event table }
  if everUnion then              { added to line number of child          }
    chadj = 2;
    nevents = nevents + 1;
    type(2)    = 2;
  endif;
{ -------------------------------------------------------------------------- }



{ -------------------------------------------------------------------------- }

  { Time of interview } 
    if valid(M6100H) & valid(M6604H) then
      if M6100H > M6604H then
        errmsg( 0070, "M6100", M6100H, M6100M, "M6604", M6604H, M6604M );
      elseif M6100H = M6604H then
        if valid(M6100M) & valid(M6604M) & M6100M > M6604M then
        errmsg( 0070, "M6100", M6100H, M6100M, "M6604", M6604H, M6604M );
        endif
      endif
    endif;


  { Date of interview event and open birth interval }
  nevents = nevents + 1;
  lcmc(nevents)   = di;
  ucmc(nevents)   = di;
  type(nevents)   = 6;
  sflag(nevents)  = 1;
  smonth(nevents) = 0;
  errflag(nevents)= notappl;
  interv(nevents) = 0;
  cdelay(nevents) = 0;


{ -------------------------------------------------------------------------- }


{ -------------------------------------------------------------------------- }

  { Preparation for imputation }
  if doimp then
    do i = 1 while i <= nevents-1
      do j = nevents while j > i by (-1)
        jinterv = 0;
        if j = i + 1 then
          jinterv = interv(j) + cdelay(j);
        endif;
        gap=ucmc(i)+jinterv-lcmc(j);
        if gap < 0 | lcmc(i) > lcmc(j) | ucmc(i) > ucmc(j) |
        (j = i+1 & errflag(i) = default & errflag(i+1) = default) then
          gap = 0
        endif;
        gap2 = int(gap/2);
        xgap = 0;
        if gap <> gap2*2 then xgap = 1 endif;
        ucmc(i) = ucmc(i) - gap2;
        lcmc(j) = lcmc(j) + gap2;
        if xgap = 1 then
          if ucmc(i) > lcmc(i) then
            ucmc(i) = ucmc(i) - 1
          else
            if ucmc(j) > lcmc(j) then
              lcmc(j) = lcmc(j) + 1
            else
              if !everUnion | i <> 2 | j <> 3 then
                errmsg( 0092, i, lcmc(i), ucmc(i), cmc2m(lcmc(i)), cmc2y(lcmc(i)), cmc2m(ucmc(i)),
                               cmc2y(ucmc(i)), j, lcmc(j), ucmc(j), cmc2m(lcmc(j)), cmc2y(lcmc(j)),
                               cmc2m(ucmc(j)),cmc2y(ucmc(j)), jinterv );
                errflag(i) = default
              endif
            endif
          endif
        endif;
      enddo;
    enddo;


{ -------------------------------------------------------------------------- }

    { Imputation - Random }

    { Impute date of birth of respondent }
    if lcmc(1) > ucmc(1) then
      errmsg( 0090, 1, lcmc(1), ucmc(1), cmc2m(lcmc(1)), cmc2y(lcmc(1)), cmc2m(ucmc(1)), cmc2y(ucmc(1)) );
      cmc(1) = default
    elseif lcmc(1) = ucmc(1) then
      cmc(1) = lcmc(1)
    else
      cmc(1) = random(lcmc(1),ucmc(1))
    endif;
    { Fix imputed date if a month given }
    if smonth(1) & (smonth(1) <> ((cmc(1) - 1) % 12) + 1) then
      x = int( (cmc(1) - 1) / 12 ) * 12 + smonth(1);
      if x > ucmc(1) then x = x - 12 endif;
      if x < lcmc(1) then x = x + 12 endif;
      if x <= ucmc(1) then
        cmc(1) = x
      else
        errmsg( 0091, 1, smonth(1), lcmc(1), ucmc(1), cmc2m(lcmc(1)), cmc2y(lcmc(1)), cmc2m(ucmc(1)),
                       cmc2y(ucmc(1)), cmc(1), cmc2m(cmc(1)), cmc2y(cmc(1)) );
      endif;
    endif;

{ -------------------------------------------------------------------------- }

    { Imputation - Random }

    do i = 2 while i <= nevents
      if lcmc(i) > ucmc(i) then
        { Inconsistent date }
        cmc(i) = default;
        errmsg( 0090, i, lcmc(i), ucmc(i), cmc2m(lcmc(i)), cmc2y(lcmc(i)), cmc2m(ucmc(i)), cmc2y(ucmc(i)) );
      elseif lcmc(i) = ucmc(i) then
        { Exact date }
        cmc(i) = lcmc(i)
      else
        { Imputed date }
        cmc(i) = random(lcmc(i),ucmc(i));
      endif;

      { Fix imputed date if a month given }
      if smonth(i) & (smonth(i) <> ((cmc(i) - 1) % 12) + 1) then
        x = int( (cmc(i) - 1) / 12 ) * 12 + smonth(i);
        if x > ucmc(i) then x = x - 12 endif;
        if x < lcmc(i) then x = x + 12 endif;
        if x <= ucmc(i) then
          cmc(i) = x
        else
          errmsg( 0091, i, smonth(i), lcmc(i), ucmc(i), cmc2m(lcmc(i)), cmc2y(lcmc(i)),
                         cmc2m(ucmc(i)), cmc2y(ucmc(i)), cmc(i), cmc2m(cmc(i)), cmc2y(cmc(i)) );
        endif;
      endif;
    enddo;

{ -------------------------------------------------------------------------- }

    M6101C = cmc(1);                    { CMC date of birth }
    M6101F = sflag(1);                  { Flag for date of birth }
    M6102C = int((di-M6101C)/12);       { Computed age of respondent }

  endif;      { end of imputation if no errors }
PROC MOD_62M_EDT

  for i in MOD_62M_EDT do 
    if pos("B",M6202) & !M6105 in 1,2 then
      errmsg( 0060, "M6202", M6200C, M6202, M6105 );
    endif;
  enddo;
  
PROC MOD_63AM_EDT

  for i in MOD_63AM_EDT do 
    if pos("B",M6303) & !M6105 in 1,2 then
      errmsg( 0060, "M6303",M6300AC, M6303, M6105 );
    endif;
  enddo;
PROC MOD_63BM_EDT

  for i in MOD_63BM_EDT do 
    if pos("B",M6309) & !M6105 in 1,2 then
      errmsg( 0060, "M6309", M6300BC, M6309, M6105 );
    endif;
    if pos("B",M6310) & !M6105 in 1,2 then
      errmsg( 0060, "M6310", M6300BC, M6310, M6105 );
    endif;    
  enddo;
  
PROC MOD_66AM_EDT

  { Sleep is in between 3 and 12 hours }
  sleep = ( count(MOD_66AM where M6601P_15 = "A") + count(MOD_66AM where M6601P_30 = "A") + count(MOD_66AM where M6601P_45 = "A") + count(MOD_66AM where M6601P_60 = "A") )/4;  
  if ( sleep < 3 ) | ( sleep > 12 ) then 
    if run1 | dprev then 
      errmsg( 0071, "WEAI(Male) A - sleeping and resting - ", sleep );
    endif;
  endif;
  



  { Traveling vs. commuting check }
  for i in MOD_66BF do 
    if M6601P_15(i) = "R" & ( M6601P_15(i-1) in "D":"H" | M6601P_15(i+1) in "D":"H" ) then 
      if run1 | dprev then 
        errmsg( 0074, "R - traveling (not for work or school) - ", M6601P_15(i-1), M6601P_15(i), M6601P_15(i+1), M6601H(i), 15);
      endif;
      break;
    endif;
    if M6601P_30(i) = "R" & ( M6601P_30(i-1) in "D":"H" | M6601P_30(i+1) in "D":"H" ) then 
      if run1 | dprev then 
        errmsg( 0074, "R - traveling (not for work or school) - ", M6601P_30(i-1), M6601P_30(i), M6601P_30(i+1), M6601H(i), 15);
      endif;
      break;
    endif;
    if M6601P_45(i) = "R" & ( M6601P_45(i-1) in "D":"H" | M6601P_45(i+1) in "D":"H" ) then 
      if run1 | dprev then 
        errmsg( 0074, "R - traveling (not for work or school) - ", M6601P_45(i-1), M6601P_45(i), M6601P_45(i+1), M6601H(i), 15);
      endif;
      break;
    endif;
    if M6601P_60(i) = "R" & ( M6601P_60(i-1) in "D":"H" | M6601P_60(i+1) in "D":"H" ) then 
      if run1 | dprev then 
        errmsg( 0074, "R - traveling (not for work or school) - ", M6601P_60(i-1), M6601P_60(i), M6601P_60(i+1), M6601H(i), 15);
      endif;
      break;
    endif;
  enddo;    

  { Outdoor work activities at night }
  for i in MOD_66BF do 
    if ( M6601P_15(i) in "G":"J" | M6601P_30(i) in "G":"J" | M6601P_45(i) in "G":"J" | M6601P_60(i) in "G":"J" ) & M6601H(i) in 0:5,19:23 then
      if run1 | dprev then
        errmsg( 0075, M6601P_15(i), M6601P_30(i), M6601P_45(i), M6601P_60(i), M6601H(i) );
      endif;
      break;
    endif;
  enddo;  
  
{ -------------------------------------------------------------------------- }

PROC MOD_71_EDT
  { Time of interview } 
      if valid(V7100H) & valid(V7133H) then
        if V7100H > V7133H then
          errmsg( 0070, "V7100", V7100H, V7100M, "V7133", V7133H, V7133M );
        elseif V7100H = V7133H then
          if valid(V7100M) & valid(V7133M) & V7100M > V7133M then
            errmsg( 0070, "V7100", V7100H, V7100M, "V7133", V7133H, V7133M );
          endif
        endif
      endif;

PROC MOD_72_EDT
  { Time of interview } 
      if valid(V7200H) & valid(V7234H) then
        if V7200H > V7234H then
          errmsg( 0070, "V7200", V7200H, V7200M, "V7234", V7234H, V7234M );
        elseif V7200H = V7234H then
          if valid(V7200M) & valid(V7234M) & V7200M > V7234M then
            errmsg( 0070, "V7200", V7200H, V7200M, "V7234", V7234H, V7234M );
          endif
        endif
      endif;
      
PROC MOD_73_EDT
  { Time of interview } 
      if valid(V7300H) & valid(V7326H) then
        if V7300H > V7326H then
          errmsg( 0070, "V7300", V7300H, V7300M, "V7326", V7326H, V7326M );
        elseif V7300H = V7326H then
          if valid(V7300M) & valid(V7326M) & V7300M > V7326M then
            errmsg( 0070, "V7300", V7300H, V7300M, "V7326", V7326H, V7326M );
          endif
        endif
      endif;
      
PROC MOD_8_INF_EDT
  { Time of interview } 
      if valid(V8100H) & valid(V8116H) then
        if V8100H > V8116H then
          errmsg( 0070, "V8100", V8100H, V8100M, "V8116", V8116H, V8116M );
        elseif V8100H = V8116H then
          if valid(V8100M) & valid(V8116M) & V8100M > V8116M then
            errmsg( 0070, "V8100", V8100H, V8100M, "V8116", V8116H, V8116M );
          endif
        endif
      endif;

  { Time of interview } 
      if valid(V8200H) & valid(V8204H) then
        if V8200H > V8204H then
          errmsg( 0070, "V8200", V8200H, V8200M, "V8204", V8204H, V8204M );
        elseif V8200H = V8204H then
          if valid(V8200M) & valid(V8204M) & V8200M > V8204M then
            errmsg( 0070, "V8200", V8200H, V8200M, "V8204", V8204H, V8204M );
          endif
        endif
      endif;

  { Time of interview } 
      if valid(V8300H) & valid(V8304H) then
        if V8300H > V8304H then
          errmsg( 0070, "V8300", V8300H, V8300M, "V8304", V8304H, V8304M );
        elseif V8300H = V8304H then
          if valid(V8300M) & valid(V8304M) & V8300M > V8304M then
            errmsg( 0070, "V8300", V8300H, V8300M, "V8304", V8304H, V8304M );
          endif
        endif
      endif;

  { Time of interview } 
      if valid(V8400H) & valid(V8404H) then
        if V8400H > V8404H then
          errmsg( 0070, "V8400", V8400H, V8400M, "V8404", V8404H, V8404M );
        elseif V8400H = V8404H then
          if valid(V8400M) & valid(V8404M) & V8400M > V8404M then
            errmsg( 0070, "V8400", V8400H, V8400M, "V8404", V8404H, V8404M );
          endif
        endif
      endif;

  { Time of interview } 
      if valid(V8500AH) & valid(V8504H) then
        if V8500AH > V8504H then
          errmsg( 0070, "V8500A", V8500AH, V8500AM, "V8504", V8504H, V8504M );
        elseif V8500AH = V8504H then
          if valid(V8500AM) & valid(V8504M) & V8500AM > V8504M then
            errmsg( 0070, "V8500A", V8500AH, V8500AM, "V8504", V8504H, V8504M );
          endif
        endif
      endif;
    
  { Time of interview } 
      if valid(V8500BH) & valid(V8509H) then
        if V8500BH > V8509H then
          errmsg( 0070, "V8500B", V8500BH, V8500BM, "V8509", V8509H, V8509M );
        elseif V8500BH = V8509H then
          if valid(V8500BM) & valid(V8509M) & V8500BM > V8509M then
            errmsg( 0070, "V8500B", V8500BH, V8500BM, "V8509", V8509H, V8509M );
          endif
        endif
      endif;
    
  { Time of interview } 
      if valid(V8600H) & valid(V8708H) then
        if V8600H > V8708H then
          errmsg( 0070, "V8600", V8600H, V8600M, "V8708", V8708H, V8708M );
        elseif V8600H = V8708H then
          if valid(V8600M) & valid(V8708M) & V8600M > V8708M then
            errmsg( 0070, "V8600", V8600H, V8600M, "V8708", V8708H, V8708M );
          endif
        endif
      endif;
