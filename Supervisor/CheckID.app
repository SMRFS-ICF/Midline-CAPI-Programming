PROC GLOBAL
{ CheckID - Core FTF Baseline ZOI }


  FILE      fname;                         { write file with or without errors }
  FILE      ReadMSG;                       { file used to read the error messages }
  FILE      WriteMSG;                      { file used to write the error messages }
  FILE      Listing;                       { file used to write the household listing }
  FILE 		PartialFile; 
  
  alpha     blanks;
  alpha(12) yresult, result;
  alpha(12) textsex, agef, agec;                      { textual sex for individual questionnaire }
  alpha(2)  measure;                      { biomarkers complete (Y/N) }
  alpha(30) xstring, chkfile;                      { to print addresses and names }
  alpha(90) FileHH, FileIN;               { strings to get the files for households and women }
  alpha(255) alphaMSG;                    { string used to read error messages }

  array transAG(200);                   { array to know barcodes in the transmittal that are not in questionnaires for MALARIA }


  alpha(90) transname;                   { transmittal sheet file names }
  array alpha(5) barcodeAG(200);        { stores barcodes for Ag plots }

  array arrayhh(200);                    { to check that all individuals are linked to households }
  array arraywm(200);

  numeric found, {ycluster,} fwrt, e, hhe, bioe, wme, casepart, x, i, j, aline, r;
  numeric xht, xhc, xhi, chbio, wmbio, mnbio, casefound, centoff;
  numeric hogregis, chktot, rwrthha, xlen, siblings, hhOK, newhh, bioexist;
  numeric tbcodesag, ors, ipos, calbeg, methuse;

  string reportfilename;
  
  { convert notappl to zero }
  function NAtoZero( xvar );
    if xvar = notappl then
      xvar = 0
    endif;
    NAtoZero = xvar;
  end;

function partialcs( ztype, ncluster, nnumber, nline, string FileToCheck )
    numeric FoundPart = 0, zlen;
    string  PartialRecord, findadd, findmod;
    setfile( PartialFile, concat( strip(FileToCheck), ".STS" ) );
    if ztype = 1 then
      findadd  = concat( "Pos=ADD.", edit("9999", ncluster), edit("9999",nnumber) );
      findmod  = concat( "Pos=MOD.", edit("9999", ncluster), edit("9999",nnumber) );
    else
      findadd  = concat( "Pos=ADD.", edit("9999", ncluster), edit("9999",nnumber), edit("99",nline) );
      findmod  = concat( "Pos=MOD.", edit("9999", ncluster), edit("9999",nnumber), edit("99",nline) );
    endif;
    zlen = length(strip(findadd));
    while !FoundPart do
      if !FileRead( PartialFile, PartialRecord ) then
        FoundPart = 3
      elseif pos( strip(findadd), PartialRecord[1:zlen] ) then
        FoundPart = 1
      elseif pos( strip(findmod), PartialRecord[1:zlen] ) then
        FoundPart = 2
      endif;
    enddo;
    close( PartialFile );
    if FoundPart in 1,2 then
      partialcs = FoundPart;
    else
      partialcs = 0;
    endif;
  end;
  
  function modsingle( var )
    if var = notappl then
      {ENG+} result = "Not visited"  {+ENG}
      {{FR+} result = "Pas visité"  {+FR}}
    elseif var = 5 then 
      {ENG+} result = concat( edit("9",var), "-Refused" ); {+ENG}
      {{FR+} result = concat( edit("9",var), "-Refusé" ); {+FR}}
    elseif var <> 1 then
      {ENG+} result = concat( edit("9",var), "-Incomplete" ); {+ENG}
      {{FR+} result = concat( edit("9",var), "-Partiellement rempli" ); {+FR}}
    else
      {ENG+} result = concat( edit("9",var), "-Complete" ); {+ENG}
      {{FR+} result = concat( edit("9",var), "-Rempli" ); {+FR}}
    endif;  
  end;  
  
  { function to create a file to decide if cluster is completed }
  function FileReturn( errors )
    setfile( fname, ".\TEMP\ERRORS.TXT" );
    open( fname );
    if FileExist( fname ) then
      FileDelete( fname );
    endif;
    if errors then
      FileWrite( fname, "WITH ERRORS" ) 
    else
      FileWrite( fname, "NO ERRORS" ) 
    endif;
    close( fname );
    FileWrite( WriteMSG, " " );
    FileWrite( WriteMSG, " " );
    FileWrite( WriteMSG, " " );
    close( WriteMSG );
  end;

  { function to read sequentially the error messages file and to find an error in it                      }
  { the reason to do it this way is because this is data entry application and it isn't desirable to stop }
  { stop for every message displayed on the screen.  The actual message is stored in variable alphaMSG    }
  { which in turn will be used in conjunction with a FileWrite statement instead of function errmsg       }
  function GetMessage( error )
    numeric errorno, k, l, errfound = 0;
    open( ReadMSG );
    while FileRead( ReadMSG, AlphaMSG ) do
      if length(strip(AlphaMSG)) then
        do k = 1   while AlphaMSG[k:1] =  " " enddo;
        do l = k+1 while alphaMSG[l:1] <> " " enddo;
        errorno = tonumber( AlphaMSG[k:l-k] );
        if errorno = error then
          errfound = 1;
          break;
        endif;
      endif;
    enddo;
    if !errfound then
     {ENG+}  e = errmsg( "Error number %d not found in error messages data file", error );  {+ENG}
     {{FR+}  e = errmsg( "Erreur %d non trouvé dans le fichier de données des messages d'erreur", error );  {+FR}}
    endif;
    close( ReadMSG );
  end;

PROC FL_CNULL
preproc

  ycluster = 0;
  fwrt     = 0;           { to control when to update control file }
  rwrthha  = 0;           { to control when to update household assignment file }
  centoff  = 0;           { set to 1 when running at the central office }

  newhh    = 0;           { households added }
  xht      = 0;           { households }
  xhc      = 0;
  xhi      = 0;
  e        = 0;           { control for errors in all households }

  chktot = tonumber( sysparm()[1:1] );    { if checking totals }

  { Get and set household and women file names }
  FileHH = filename( CFTF );

  { set files to read error messages (MGF) and to report errors (ERR) }

  filedelete( ".\SUPERVISOR\CheckID.txt" );
  filedelete( ".\SUPERVISOR\Listing.txt" );

  setfile( ReadMsg,  ".\SUPERVISOR\CheckID.mgf" );
  setfile( WriteMsg, ".\SUPERVISOR\CheckID.txt" );
  setfile( Listing, ".\SUPERVISOR\Listing.txt" );
//  open( WriteMsg ); { this was the original, should it be used? }

{ delete the application.pff to avoid direct calls to this application }
  filedelete(".\application.pff");


  { heading for report with write command }
  {ENG+} 
  filewrite( Listing, "                             HOUSEHOLD LISTING" );
  filewrite( Listing, " " );
  filewrite( Listing,  "CLUSTER    HH/MODULE      AGE (flag)          INTERV  RESULT       H/W (flag) " );
  { heading for report with FileWrite command }
  FileWrite( WriteMSG, "SUMMARY OF OUTSTANDING ISSUES IDENTIFIED IN THE RUN AND THAT NEED TO BE CORRECTED" );
  FileWrite( WriteMSG, "_________________________________________________________________________________" );
  FileWrite( WriteMSG, " " );
  write(  "________ __________ ____ ______________________________ __________  _____ _____ " );
 {+ENG}

  {{FR+} 
  filewrite( Listing, "                             LISTE DES MÉNAGES" );
  filewrite( Listing, " " );
  filewrite( Listing,  "GRAPPE    MNG/MODULE      AGE (flag)          INTERV  RESULTAT       H/W (flag) " );
  { heading for report with FileWrite command }
  FileWrite( WriteMSG, "SOMMAIRE DES ERREURS IDENTIFIÉES EN COURS ET QUI DOIVENT ÊTRE CORRIGÉES" );
  FileWrite( WriteMSG, "_________________________________________________________________________________" );
  FileWrite( WriteMSG, " " );
  write(  "________ __________ ____ ______________________________ __________  _____ _____ " );
 {+FR}}

  { initialize transmittal sheet arrays to control duplicates }
  do j = 1 while j <= 200
    transAG(j) = 0;
  enddo;

  { loops over every household }
  while loadcase( CFTF ) do

    { load files based on first case }
    if YCLUSTER = 0 then
      { load supervisor control to make sure that total number of cases to be collected in cluster is assigned }
      CLUSTER = CFTF.HHEA;
      if !loadcase( CLUSTERS, CLUSTER ) then
        getmessage( 50041 );
        e = FileWrite( WriteMSG, alphaMSG, CLUSTER );
        FileReturn( e );
        stop(-1);
      elseif !YTOTHH then
        getmessage( 50040 );
        e = FileWrite( WriteMSG, alphaMSG, CFTF.HHEA );
      endif;
      { load HH assigned to interviewers to check interviewer assigned to collect household }
      XCLUSTER = CFTF.HHEA;
      if !loadcase( SAMPSEL, XCLUSTER )  then
        getmessage( 50042 );
        e = FileWrite( WriteMSG, alphaMSG, XCLUSTER );
        FileReturn( e );
        stop(-1);
      endif;

      transname = filename( TRANSMIT );
      do i = length( strip(transname) ) while transname[i:1] <> "." by (-1) enddo;
      ipos = i-4;
      transname[ipos:1] = "T";
      setfile( TRANSMIT, strip(transname) );
      TCLUSTER = CFTF.HHEA;
      tbcodesAG = 0;
      if !loadcase( TRANSMIT, TCLUSTER ) then
        if chktot then
          getmessage( 50043 );
          e = FileWrite( WriteMSG, alphaMSG, "SURFACE", TCLUSTER );
        endif;
      else
        tbcodesAG = TBCODES;
      endif;
      do i = 1 while i <= tbcodesag
        barcodeAG(i) = TBARCODE(i);
      enddo;
      YCLUSTER  = CFTF.HHEA;
      
    endif;

      { load GPS data file to check collection for each household }
    GPSID = concat(edit("999",CFTF.HHEA),edit("999",CFTF.HHNUM));
    if !loadcase( HH_GPS, GPSID )  then
      getmessage( 50057 );
      e = FileWrite( WriteMSG, alphaMSG, CFTF.HHNUM );
    endif;

    chbio = 0;   { biomarkers for children in household schedule }
    wmbio = 0;   { biomarkers for women in household schedule }
    mnbio = 0;   { biomarkers for men in household schedule }
    hhe   = 0;   { define if there are household structural errors }
    bioe  = 0;   { define if there are biomarker errors }
    casepart = partialcs( 1, CFTF.HHEA, HHNUM, 0, FileHH );
    if !AHRESULT in 1,11 then
      {ENG+} yresult = concat( edit("99",AHRESULT), "-Incomplete" ) {+ENG}
      {{FR+} yresult = concat( edit("99",AHRESULT), "-Partiellement rempli" ) {+FR}}
    elseif casepart | AHRESULT = 11 then
      {ENG+} yresult = concat( edit("9",AHRESULT), "-Partial" ) {+ENG}
      {{FR+} yresult = concat( edit("9",AHRESULT), "-Partiellement rempli" ) {+FR}}
    else
      {ENG+} yresult = concat( edit("9",AHRESULT), "-Complete" ) {+ENG}
      {{FR+} yresult = concat( edit("9",AHRESULT), "-Rempli" ) {+FR}}
    endif;

    xht      = xht + 1;   { total households }

    measure = " ";
    if (casepart & AHRESULT = 1) | AHRESULT = 11 then
      { no structure checked for partial cases }
      getmessage( 50055 );
      hhe = FileWrite( WriteMSG, alphaMSG, HHNUM );
      xhi    = xhi + 1;             { partial households counted as incomplete }
    elseif AHRESULT = 1 then
      xhc    = xhc + 1;             { complete households }

      { number of records for household members }
      x = soccurs( MOD_1 ) - ( V101A <> 1 ) - ( V101B <> 1 );
      if VTYPE = 2 & x = 0 then x = 1; endif;
      if x <> AHMEMBER then
        getmessage( 50050 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, x, AHMEMBER )
      endif;

      { eligible women }
      x = count( CFTF.MOD_1 where V107 > 0 & V102 = 2 );
      if x <> AHWOMEN then
        getmessage( 50051 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, x, AHWOMEN )
      endif;

      { eligible children }
      x = count( CFTF.MOD_1 where V108 > 0 );
      if x <> AHKIDS then
        getmessage( 50052 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, x, AHKIDS )
      endif;

      { household characteristics record }
      if soccurs( MOD_2 ) <> 1 then
        getmessage( 50100 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_2) )
      endif;

      { food security }
      if soccurs( MOD_3 ) <> 1 then
        getmessage( 50100 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_3) )
      endif;

      { biomarkers for children }
	  chbio = count( CFTF.MOD_1 where V108 <> 0 );

      x = soccurs( MOD_5 );
      if x <> chbio then
        getmessage( 50054 );
        {ENG+} bioe = FileWrite( WriteMSG, alphaMSG, HHNUM, "Module 5 - child nutrition", x, chbio ); {+ENG}
        {{FR+} bioe = FileWrite( WriteMSG, alphaMSG, HHNUM, "Module 5 - Nutrition des enfants", x, chbio ); {+FR}}
        measure = "N";
      endif; 

      x = soccurs( ANTHRO_CHILD );
      if x <> chbio then
        getmessage( 50054 );
        {ENG+} bioe = FileWrite( WriteMSG, alphaMSG, HHNUM, "Module 5 - child anthro", x, chbio ); {+ENG}
        {{FR+} bioe = FileWrite( WriteMSG, alphaMSG, HHNUM, "Module 5 - Anthropométrie des enfants", x, chbio ); {+FR}}
        measure = "N";
      endif;

      { biomarkers for women }
      wmbio = count( CFTF.MOD_1 where V107 <> 0 );
      x = soccurs( MOD_4 );
      if x <> wmbio then
        getmessage( 50054 );
       {ENG+}  bioe = FileWrite( WriteMSG, alphaMSG, HHNUM, "Module 4 - women nutrition", x, wmbio ); {+ENG}
       {{FR+}  bioe = FileWrite( WriteMSG, alphaMSG, HHNUM, "Module 4 - nutrition des femmes", x, wmbio ); {+FR}}
        measure = "N";
      endif;
      
      x = soccurs( ANTHRO_WOMEN );
      if x <> wmbio then
        getmessage( 50054 );
        {ENG+} bioe = FileWrite( WriteMSG, alphaMSG, HHNUM, "Module 4 - women anthro", x, wmbio ); {+ENG}
        {{FR+} bioe = FileWrite( WriteMSG, alphaMSG, HHNUM, "Module 4 - Anthropométrie des femmes", x, wmbio ); {+FR}}
        measure = "N";
      endif;

      if soccurs( MOD_61F ) <> 1*( V101B = 1 ) | soccurs( MOD_62F ) <> 13*( V101B = 1 & V6100D = 1 ) | 
        soccurs( MOD_63AF ) <> 15*( V101B = 1 & V6100D = 1 ) | soccurs( MOD_63BF ) <> 6*( V101B = 1 & V6100D = 1 ) |
        soccurs( MOD_63CF ) <> 1*( V101B = 1 & V6100D = 1 ) | soccurs( MOD_64BF ) <> 18*( V101B = 1 & V6100D = 1 ) |
        soccurs( MOD_66AF ) <> 24*( V101B = 1 & V6100D = 1 ) | soccurs( MOD_66BF ) <> 1*( V101B = 1 ) then
        getmessage( 50100 );
        {ENG+} hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "Female WEIA Module" ); {+ENG}
        {{FR+} hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "Module WEIA Femme " ); {+FR}}
      endif;
      
      if soccurs( MOD_61M ) <> 1*( V101A = 1 & V101B = 1 ) | soccurs( MOD_62M ) <> 13*( V101A = 1 & V101B = 1 & M6100D = 1 ) | 
         soccurs( MOD_63AM ) <> 14*( V101A = 1 & V101B = 1 & M6100D = 1 ) | soccurs( MOD_63BM ) <> 6*( V101A = 1 & V101B = 1 & M6100D = 1 ) |
         soccurs( MOD_63CM ) <> 1*( V101A = 1 & V101B = 1 & M6100D = 1 ) | soccurs( MOD_64BM ) <> 16*( V101A = 1 & V101B = 1 & M6100D = 1 ) |
         soccurs( MOD_66AM ) <> 24*( V101A = 1 & V101B = 1 & M6100D = 1 ) | soccurs( MOD_66BM ) <> 1*( V101A = 1 & V101B = 1 ) then
        getmessage( 50100 );
       {ENG+}  hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "Male WEIA Module" ); {+ENG}
        {{FR+} hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "Module WEIA Homme " ); {+FR}}
      endif;


      if soccurs( MOD_790 ) <> count( MOD_1 where VFARMER <> 0 ) then 
        getmessage( 50100 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_790) )
      endif;

      if soccurs( MOD_71 ) <> NAtoZero( (V235A > 0) + (V235B > 0) + (V235C > 0) + (V235D > 0) ) then
        getmessage( 50100 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_71) )
      endif;
      if soccurs( MOD_72 ) <> NAtoZero( (V237A > 0) + (V237B > 0) + (V237C > 0) + (V237D > 0)  ) then
        getmessage( 50100 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_72) )
      endif;
      if soccurs( MOD_73 ) <> NAtoZero( (V239A > 0) + (V239B > 0) + (V239C > 0) + (V239D > 0)  ) then
        getmessage( 50100 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_73) )
      endif;

      if soccurs( MOD_791 ) <> count( MOD_1 where VFARMER <> 0 ) then 
        getmessage( 50100 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_791) )
      endif;
      if soccurs( MOD_792 ) <> count( MOD_1 where VFARMER <> 0 ) then 
        getmessage( 50100 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_792) )
      endif;


      { check if barcodes recorded in plot record are in the transmittal sheet }

      for i in record MOD_791 do

        do r = 1 while r <= noccurs(V79100M)

	        if !V79105(r) in "?","99990":"99998" & length( strip(V79105(r)) ) then
	          do j = 1 while j <= tbcodesAG
	            if V79105(r) = barcodeAG(j) then
	              { check for duplicates }
	              if transAG(j) then
	                getmessage( 50500 );
	                hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, V79105(r), strip(V101(V79100C(i))), transAG(j) );
	              { otherwise mark the barcode as found with the household number }
	              else
	                transAG(j) = HHNUM;
	              endif;
	              break;
	            endif;
	          enddo;
	          if chktot & j > tbcodesAG then
	            { barcode in questionnaire wasn't found in transmittal sheet }
	            getmessage( 50502 );
	            hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, V79105(r), strip(V101(V79100C(i))) );
	          endif;
	        endif;

        enddo;      
      enddo;

      { consumption modules }
      if V8100D = 1 & ( soccurs( MOD_8_INF ) <> 1 & soccurs( MOD_81A ) <> 186 | soccurs( MOD_81B ) <> 1 | soccurs( MOD_82 ) <> 20 | soccurs( MOD_83 ) <> 30 |
         soccurs( MOD_84 ) <> 50 | soccurs( MOD_85 ) <> 32 | soccurs( MOD_85B ) <> 3 | soccurs( MOD_86 ) <> 1 | 
         soccurs( MOD_87 ) <> 31 ) then
        getmessage( 50100 );
        {ENG+} hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "Module 8: Consumption" ) {+ENG}
        {{FR+} hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "Module 8: Consommation alimentaire" ) {+FR}}
      endif;
      if V8100D <> 1 & ( soccurs( MOD_8_INF ) <> 1 |  soccurs( MOD_81A ) | soccurs( MOD_81B ) | soccurs( MOD_82 ) | soccurs( MOD_83 ) |
         soccurs( MOD_84 ) | soccurs( MOD_85 ) | soccurs( MOD_85B ) | soccurs( MOD_86 ) | soccurs( MOD_87 ) ) then
        getmessage( 50100 );
        {ENG+} hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "Module 8: Consumption" ) {+ENG}
        {{FR+} hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "Module 8: Consommation alimentaire" ) {+FR}}
       endif;


    else
      xhi = xhi + 1;                 { incomplete households }
      if soccurs(MOD_1)  | soccurs(MOD_1B) | soccurs(MOD_2) | soccurs(MOD_3) |  soccurs(MOD_4_INF) |
         soccurs(MOD_4) | soccurs(ANTHRO_WOMEN) | soccurs(MOD_5_INF) | soccurs(MOD_5) | soccurs(ANTHRO_CHILD) | 
         soccurs(MOD_61F) | soccurs(MOD_62F) | soccurs(MOD_63AF) | soccurs(MOD_63BF) | soccurs(MOD_63CF) | 
         soccurs(MOD_64BF) | soccurs(MOD_66AF) | soccurs(MOD_66BF) | 
         soccurs(MOD_61M) | soccurs(MOD_62M) | soccurs(MOD_63AM) | soccurs(MOD_63BM) | soccurs(MOD_63CM) | 
         soccurs(MOD_64BM) | soccurs(MOD_66AM) | soccurs(MOD_66BM) | 
         soccurs(MOD_71) | soccurs(MOD_72) | soccurs(MOD_73) | 
         soccurs(MOD_74) | soccurs(MOD_75) | soccurs(MOD_76) | soccurs(MOD_77) |
         soccurs(MOD_8_INF) | soccurs(MOD_81A) | soccurs(MOD_81B) | soccurs(MOD_82) | soccurs(MOD_83) |
         soccurs(MOD_84) | soccurs(MOD_85) | soccurs(MOD_85B) | soccurs(MOD_86) | soccurs(MOD_87) then
        getmessage( 50110 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, AHRESULT );
      endif;
    endif;

    { check if household was collected by the interviewer assigned to it and synchronize file }
    found = 0;
    do i = 1 while i <= XTOTAL by 1
      if HHNUM = XNUMBER(i) then
        found = 1;
        { display error just to let the supervisor know about the change }
        if AHINTNUM1 <> XINTCODE(i) then
      //    getmessage( 50225 );
      //    FileWrite( WriteMSG, alphaMSG, HHNUM, XINTCODE(i), AHINTNUM1 );
          XINTCODE(i) = AHINTNUM1;
          rwrthha     = 1;
        endif;
        { synchronize household status in HH assignment file }
        if XRESULT(i) <> AHRESULT then
          XRESULT(i) = AHRESULT;
          rwrthha    = 1;
        endif;
        { synchronize name of PDM in HH assignment file }
        if V101A = 1 then 
          if AHRESULT = 1 & length( strip(V101(1)) ) & !pos( strip(V101(1)), XNAME(i) ) then
            XNAME(i) = strip(V101(1));
            rwrthha  = 1;
          endif;
        elseif V101B = 1 then 
          if AHRESULT = 1 & length( strip(V101(2)) ) & !pos( strip(V101(2)), XNAME(i) ) then
            XNAME(i) = strip(V101(2));
            rwrthha  = 1;
          endif;
        else
          if AHRESULT = 1 & length( strip(V101(3)) ) & !pos( strip(V101(3)), XNAME(i) ) then
            XNAME(i) = strip(V101(3));
            rwrthha  = 1;
          endif;
        endif; 
        break;
      endif;
    enddo;

    { print line for the household in the report }
    if soccurs( MOD_1 ) then
      if V101A = 1 then 
        xstring = strip(V101(1)); 
      elseif V101B = 1 then 
        xstring = strip(V101(2)); 
      else
        xstring = strip(V101(3)); 
      endif;
    else
      xstring = "";
    endif;

    FileWrite( Listing, "_________________________________________________________________________________" );
    filewrite( Listing, maketext( "  %04d     %04d-%30s%03d     %12s ",
           CFTF.HHEA, HHNUM, xstring, AHINTNUM1, yresult ) );

    { find a place in array of cases in the supervisor control file }
    casefound = 0;
    do i = 1 while i <= 35
      if HH(i) = 0 | HH(i) = HHNUM then
        casefound = i;
        break;
      endif;
    enddo;
    j = casefound;
    if casefound & hhOK then   { Only use if there is a slot and if the household is accepted }
      if HH(j) = 0 then
        HH(j) = HHNUM;
        ENUM(j) = AHINTNUM1;
        fwrt = 1;
      endif;

      { accept incomplete HH and flag all of them as true }
      if AHRESULT <> 1 & SACCEPTH(j) = 0 then
        SACCEPTH(j)  = AHRESULT;
        fwrt = 1;
      { incomplete interviews are saved as partial to come back to them in add mode }
      elseif AHRESULT = 1 & casepart then         { partial cases need to be reset, if already accepted }
        if SACCEPTH(j) then
          SACCEPTH(j)  = 0;
          fwrt = 1;
        endif;
      else
        if !hhe & ( SACCEPTH(j) <> AHRESULT | SACCEPTH(j) = 0 ) then   { household with no errors }
          SACCEPTH(j) = AHRESULT;
          fwrt = 1;
        elseif SACCEPTH(j) & hhe then                     { an error in the household was introduced }
          getmessage( 50210 );
          hhe = FileWrite( WriteMSG, alphaMSG, HHNUM );
          SACCEPTH(j)  = 0;
          fwrt = 1;
        endif;
        { check errors on biomarkers }
      {  if chbio+wmbio then
          if !bioe & !SACCEPTHW(j) then       { h/w completed & accepted for household }
            SACCEPTHW(j) = 1;
            fwrt = 1;
          elseif bioe & SACCEPTHW(j) then     { reset h/w if errors were introduced }
            SACCEPTHW(j) = 0;
            fwrt = 1;
          endif;
        elseif !SACCEPTHW(j) then             { no members eligible for h/w }
          SACCEPTHW(j) = 1;
          fwrt = 1;
        endif;}
        if !hhe & !bioe & AHRESULT <> 1 & !SACCEPTH(j) then { no structural errors in an incomplete household }
          SACCEPTF(j) = sysdate();
          fwrt = 1;
        elseif (hhe | bioe) & SACCEPTF(j) then             { structural errors introduced after being accepted }
          SACCEPTF(j) = 0;
          fwrt = 1;
        endif;
      endif;
    endif;                   { endif if slot found and HH accepted }

    if hhe | bioe then e = 1 endif;
    if !AHRESULT in 1,11 then 
      next;    { skip women and go to next household } { GD - would like to display modules for all households }
    else

  FileWrite( Listing, "---------------------------------------------------------------------------------" );
 
    modsingle( V245 );      
    {ENG+} filewrite( Listing, maketext( "              1-Roster                        %03d     %12s", V246, result ) ); {+ENG}
    {ENG+} filewrite( Listing, maketext( "              2-Dwelling                      %03d     %12s", V246, result ) ); {+ENG}
    {{FR+} filewrite( Listing, maketext( "              1-Tableau du ménage             %03d     %12s", V246, result ) ); {+FR}}
    {{FR+} filewrite( Listing, maketext( "              2-Charactéristiques             %03d     %12s", V246, result ) ); {+FR}}
        
    modsingle( V343 );      
    {ENG+} filewrite( Listing, maketext( "              3-FIES                          %03d     %12s", V344, result ) ); {+ENG}
    {{FR+} filewrite( Listing, maketext( "              3-Sécurité                      %03d     %12s", V344, result ) ); {+FR}}
  
  if AHWOMEN then
    if !soccurs(CFTF.MOD_4) | !count(CFTF.MOD_4 where V400X <> notappl) then
      {ENG+} filewrite( Listing, maketext( "              4-Women                          Not visited") ); {+ENG}
      {{FR+} filewrite( Listing, maketext( "              4-Femme                          Pas visité") ); {+FR}}

    else 
      {ENG+} filewrite( Listing, maketext( "              4-Women") ); {+ENG}
      {{FR+} filewrite( Listing, maketext( "              4-Femme") ); {+FR}}

    endif;
    for i in CFTF.MOD_4 do          
      modsingle( V433(i) );
      if V402(i) & V402(i)%5 <> 0 then
        {ENG+} agef = concat( maketext("(%d", V402(i)), "yr.)"); {+ENG}     
        {{FR+} agef = concat( maketext("(%d", V402(i)), "yr.)"); {+FR}}     
	  elseif V402(i) then
        {ENG+} agef = concat( maketext("(%d", V402(i)), "yr.-flag)");  {+ENG}       	  
        {{FR+} agef = concat( maketext("(%d", V402(i)), "yr.-flag)");  {+FR}}       	  
      endif;
      if V400X then 
        filewrite( Listing, maketext( "                %02d W-%-20s%12s%03d%12s", V400D(i), strip(V101(V400D(i))), agef, V434(i), result ) );
      endif;
    enddo;
  endif;
  
  if AHKIDS then
    if !soccurs(CFTF.MOD_5) | !count(CFTF.MOD_5 where V500X <> notappl) then
      {ENG+} filewrite( Listing, maketext( "              5-Children                      Not visited") ); {+ENG}
      {{FR+} filewrite( Listing, maketext( "              5-Enfants                       Pas visité") ); {+FR}}
    else
      {ENG+} filewrite( Listing, maketext( "              5-Children") ); {+ENG}    
      {{FR+} filewrite( Listing, maketext( "              5-Enfants") ); {+FR}}
    endif;
    for i in CFTF.MOD_5 do          
      modsingle( V566(i) );          
      if V508(i) & V508(i)%12 <> 0 then
        {ENG+} agec = concat( maketext("(%d", V508(i)), "mo.)");   {+ENG}       
        {{FR+} agec = concat( maketext("(%d", V508(i)), "mo.)");   {+FR}}       
	  elseif V508(i) then
        {ENG+} agec = concat( maketext("(%d", V508(i)), "mo.-flag)");  {+ENG}        	  
        {{FR+} agec = concat( maketext("(%d", V508(i)), "mo.-flag)");  {+FR}}        	  
      endif;
      if V500X then 
        filewrite( Listing, maketext( "                %02d C-%-20s%12s%03d%12s", V500D(i), strip(V101(V500D(i))), agec, V567(i), result ) );
      endif;
    enddo;
  endif;
 
  if V101B = 1 then 
    modsingle( V6605 ); 
    filewrite( Listing, maketext( "              6F-WEAI                         %03d     %12s", V6606, result ) ); 
  endif;

  if V101A = 1 & V101B = 1 then 
    modsingle( M6605 );
    filewrite( Listing, maketext( "              6M-WEAI                         %03d     %12s", M6606, result ) );
  endif;
  
  if V234 = 1 | V236 = 1 | V238 = 1 then
    for i in CFTF.MOD_790 do
      modsingle( V79001(i) );
      {ENG+} filewrite( Listing, maketext( "              7.90-Plot maps                  %03d     %12s", V79002, result ) ); {+ENG}
      {{FR+} filewrite( Listing, maketext( "              7.90-Cartes                      %03d     %12s", V79002, result ) ); {+FR}}
    enddo;
  endif;

  if V234 = 1 then
    for i in CFTF.MOD_71 do
      modsingle( V7134 );
      {ENG+} filewrite( Listing, maketext( "              7.1-Maize                       %03d     %12s", V7135, result ) ); {+ENG}
      {{{FR+} filewrite( Listing, maketext( "              7.1-Mais                       %03d     %12s", V7135, result ) ); {+FR}}}
    enddo;
  endif;
  
  if V236 = 1 then
    for i in CFTF.MOD_72 do 
      modsingle( V7235 );
      {ENG+} filewrite( Listing, maketext( "              7.2-Beans                       %03d     %12s", V7236, result ) ); {+ENG}
    enddo;
  endif;
  
  if V238 = 1 then
    for i in CFTF.MOD_73 do
      modsingle( V7327 );
      {ENG+} filewrite( Listing, maketext( "              7.3-Coffee                      %03d     %12s", V7328, result ) ); {+ENG}
    enddo;
  endif;

  if V234 = 1 | V236 = 1 | V238 = 1 then
    for i in CFTF.MOD_791 do
      modsingle( V79111 );
      {ENG+} filewrite( Listing, maketext( "              7.91-Plot area                   %03d     %12s", V79112, result ) ); {+ENG}
      {{FR+} filewrite( Listing, maketext( "              7.91-Surface du terrain           %03d     %12s", V79112, result ) ); {+FR}}
    enddo;
    for i in CFTF.MOD_792 do
      modsingle( V79206 );
      {ENG+} filewrite( Listing, maketext( "              7.92-Crop yield                  %03d     %12s", V79207, result ) ); {+ENG}
      {{FR+} filewrite( Listing, maketext( "              7.92-Rendement                  %03d     %12s", V79207, result ) ); {+FR}}
    enddo;
  endif;

    modsingle( V8709 );
    {ENG+} filewrite( Listing, maketext( "              8-Consumption                   %03d     %12s", V8710, result ) ); {+ENG}
    {{FR+} filewrite( Listing, maketext( "              8-Consommation alimentaire      %03d     %12s", V8710, result ) ); {+FR}}

    if wme then e = 1 endif;
  endif;    
  
  enddo;                              { end loop over all households }

  { summary of the run }
  filewrite( Listing, " " );
  {ENG+} 
  filewrite( Listing, "Cluster      HHs    Complete Incomp.  " );
  filewrite( Listing, "________ ___________________________ ________________________________________ " );
  filewrite( Listing, "  %04d   %7d%10d%8d", ycluster, xht, xhc, xhi );
   {+ENG}
  {{FR+} 
  filewrite( Listing, "Grappe      MNGs    Complet Incomp.  " );
  filewrite( Listing, "________ ___________________________ ________________________________________ " );
  filewrite( Listing, "  %04d   %7d%10d%8d", ycluster, xht, xhc, xhi );
   {+FR}}
  if newhh then
    filewrite( Listing, " " );
    {ENG+} filewrite( Listing, "Households added not originally part of the sample design %d", newhh ); {+ENG}
    {{FR+} filewrite( Listing, "Ménages ajoutés ne faisant pas partie du plan de sondage à l'origine %d", newhh ); {+FR}}
  endif;



  if chktot then   { totals checked when collapsing all data or when closing a cluster }
  
    do j = 1 while j <= tbcodesAG
      if !transAG(j) then
        getmessage( 50501 );
        e = FileWrite( WriteMSG, alphaMSG, "SURFACE", barcodeAG(j) );
      endif;
    enddo;
    
    if loadcase( CTRL_CLUSTER, CLUSTER ) then
	    { check totals with supervisor control file }
	  do i = 1 while i <= 35 & HH(i) by 1
	  enddo;
	  hogregis = i-1;
	  if xht = SHTOTAL & hogregis = xht then
	    if e then
	      getmessage( 50151 );
	      e = FileWrite( WriteMSG, alphaMSG, xht );
	    else
	      SHCOMP   = xhc;
	      SHINCOMP = xhi;
	      SFINDATE = sysdate();
	      fwrt     = 1;
	    endif;
	  else
	    getmessage( 50150 );
	    e = FileWrite( WriteMSG, alphaMSG, xht, SHTOTAL, hogregis )
      endif;
    else
      if !CLUSTER then
        getmessage( 50039 );
        e = FileWrite( WriteMSG, alphaMSG );
      else
        getmessage( 50044 );
        e = FileWrite( WriteMSG, alphaMSG, CLUSTER );
      endif;
    endif;
  endif;            { endif to check totals }

  { rewrite supervisor control file if necessary }
  if fwrt then writecase( CTRL_CLUSTER ) endif;
  close( CTRL_CLUSTER );
  { rewrite household sample file if necessary }
  if rwrthha then writecase( SAMPSEL ) endif;
  close( SAMPSEL );

  close( Listing );
  { instructions to create a file to decide if cluster is completed }
  FileReturn( e );
  
    if e then
      {ENG+} errmsg( "Cluster can't be closed as there are still pending errors" ); {+ENG}
      {{FR+} errmsg( "La grappe ne peut pas être fermée car il y a encore des erreurs à résoudre" ); {+FR}}
	  reportFilename = maketext("%sreport.html", pathname(Application));
	  fileconcat("./supervisor/report.html","./lookup/rep_header.txt","./supervisor/CheckID.txt","./supervisor/Listing.txt");
	  if getos() in 20:29 then { Android }
		execsystem(maketext("view:%s", reportFilename),wait);
	  else
		execsystem(maketext("%sexplorer.exe %s", pathname(Windows), reportFilename),wait);
	  endif;
      //DispFile( concat( strip(superv), "\CHECKID.tmp" ) );
    else
      {ENG+} errmsg( "Cluster check successful, proceed to close cluster" ); {+ENG}
      {{FR+} errmsg( "Grappe fermée avec succès" ); {+FR}}
	  reportFilename = maketext("%sreport.html", pathname(Application));
	  fileconcat("./supervisor/report.html","./lookup/rep_header.txt","./supervisor/Listing.txt");
	  if getos() in 20:29 then { Android }
		execsystem(maketext("view:%s", reportFilename),wait);
	  else
		execsystem(maketext("%sexplorer.exe %s", pathname(Windows), reportFilename),wait);
	  endif;			        
    endif;
  execpff("./Supervisor.pff",stop);	
  
  { totally quit application }
  stop(-1);
