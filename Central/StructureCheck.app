PROC GLOBAL


  FILE      fname;                         { write file with or without errors }
  FILE      ReadMSG;                       { file used to read the error messages }
  FILE      WriteMSG;                      { file used to write the error messages }
  FILE 		PartialFile; 
  
  alpha     blanks;
  alpha(13) yresult;
  alpha(12) textsex;                      { textual sex for individual questionnaire }
  alpha(2)  measure;                      { biomarkers complete (Y/N) }
  alpha(30) xstring;                      { to print addresses and names }
  alpha(90) FileHH;               { strings to get the files for households and women }
  alpha(255) alphaMSG;                    { string used to read error messages }
  alpha(6) ystring;					
  alpha(6) zstring;					

  numeric found, ycluster, zcluster, fwrt, e, hhe, bioe, casepart, x, i, j;
  numeric xht, xhc, xhi, xit, xic, xii, chbio, wmbio, casefound, centoff;
  numeric hogregis, chktot, rwrthha, xlen, siblings, hhOK, newhh, bioexist;
  numeric ipos, aline, measured, vcc;


function partialcs( ncluster, nnumber, string FileToCheck ) { This function is mainly used in the field for closing clusters }
    numeric FoundPart = 0, zlen;
    string  PartialRecord, findadd, findmod;
    setfile( PartialFile, concat( strip(FileToCheck), ".STS" ) );
    findadd  = concat( "Pos=ADD.", edit("999", ncluster), edit("999",nnumber) );
    findmod  = concat( "Pos=MOD.", edit("999", ncluster), edit("999",nnumber) );
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
  
  { function to create a file to decide if cluster is completed }
  function FileReturn( errors )
    setfile( fname, ".\ERRORS.TXT" );
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

  function updtctrl( xtype )
  { assign field supervisor to cluster in control file }
    open( CONTROL );
    CSAMPLE = HHEA;
    if loadcase( CONTROL, CSAMPLE ) then
      if xtype = 1 then
        CSUPERV = AHSUPERV;
      elseif xtype = 2 then
        CHTOTAL = xht;
        CHCOMP = xhc;
        CHINCOMP = xhi;
      endif;
      writecase( CONTROL );
    endif;
    close( CONTROL );
  end;
  
  { function to read sequentially the error messages file and to find an error in it                      }
  { the reason to do it this way is because this is data entry application and it isn't desirable to      }
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
      e = errmsg( "Error number %d not found in error messages data file", error );
    endif;
    close( ReadMSG );
  end;

PROC FL_CNULL
preproc

  ycluster = 0;
  fwrt = 0;           { to control when to update control file }
  rwrthha = 0;           { to control when to update household assignment file }
  centoff = 0;           { set to 1 when running at the central office }

  newhh = 0;           { households added }
  xht = 0;           { households }
  xhc = 0;
  xhi = 0;
  e = 0;           { control for errors in all households }

  chktot = tonumber( sysparm()[1:1] );    { if checking totals }
  zcluster = tonumber( sysparm()[2:3] );   { getting cluster number for report title }

  { Get and set file names }
  FileHH = filename( CFTF );

  { set files to read error messages (MGF) and to report errors (ERR) }
  setfile( ReadMsg,  ".\StructureCheck.mgf" );
  setfile( WriteMsg, ".\StructureCheck.txt" );
  open( WriteMsg );

  write( " " );
  write(  "Cluster  HH/Module     AGE (flag)   INTERV  RESULT                H/W (flag) " );
  { heading for report with FileWrite command }
  FileWrite( WriteMSG, "SUMMARY OF OUTSTANDING ISSUES IDENTIFIED IN CLUSTER %03d", zcluster );
  FileWrite( WriteMSG, "========================================================================================================" );
  FileWrite( WriteMSG, " " );
  write(  "________________________________________________________________________ " );

  { loops over every household }
  while loadcase( CFTF ) do
	updtctrl( 1 );
    { load files based on first case }
    if ycluster = 0 then
      { load supervisor control to make sure that total number of cases to be collected in cluster is assigned }
{      CLUSTER = HHEA;
      if !loadcase( CTRL_CLUSTER, CLUSTER ) then
        getmessage( 001 );
        e = FileWrite( WriteMSG, alphaMSG, CLUSTER );
        FileReturn( e );
        stop(-1);
      elseif !CHEXPECT then
        getmessage( 002 );
        e = FileWrite( WriteMSG, alphaMSG, HHEA );
      endif;}
      { load HH assigned to interviewers to check interviewer assigned to collect household }
      XCLUSTER = HHEA;
      if !loadcase( SAMPSEL, XCLUSTER )  then
        getmessage( 003 );
        e = FileWrite( WriteMSG, alphaMSG, XCLUSTER );
        FileReturn( e );
        stop(-1);
      endif;
      ycluster  = HHEA;
    endif;

    chbio = 0;   { biomarkers for children in household schedule }
    wmbio = 0;   { biomarkers for women in household schedule }
    hhe   = 0;   { define if there are household structural errors }
    bioe  = 0;   { define if there are biomarker errors }
    casepart = partialcs( HHEA, HHNUM, FileHH );
    {+US}
    if AHRESULT <> 1 then
      yresult = concat( edit("Z9",AHRESULT), "-Incomplete" )
    elseif casepart then
      yresult = concat( edit("Z9",AHRESULT), "-In progress" )
    else
      yresult = concat( edit("Z9",AHRESULT), "-Complete" )
    endif;
    {US+}

    xht = xht + 1;   { total households }

    measure = " ";
    if casepart & AHRESULT = 1 then
      { no structure checked for partial cases }
      getmessage( 004 );
      hhe = FileWrite( WriteMSG, alphaMSG, HHNUM );
      xhi    = xhi + 1;             { partial households counted as incomplete }
    elseif AHRESULT = 1 then
      xhc    = xhc + 1;             { complete households }

      { number of records for household members }
      x = soccurs( MOD_1 );
      if x <> AHMEMBER then
        getmessage( 005 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, x, AHMEMBER )
      endif;
      
      { eligible women }
      x = count( CFTF.MOD_1 where V107 > 0 & V102 = 2 );
      if x <> AHWOMEN then
        getmessage( 006 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, x, AHWOMEN )
      endif;

      { eligible children }
      x = count( CFTF.MOD_1 where V108 > 0 );
      if x <> AHKIDS then
        getmessage( 007 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, x, AHKIDS )
      endif;

      if soccurs( MOD_1B ) <> 1 then
        getmessage( 008 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_1B) )
      endif;

      { household characteristics record }
      if soccurs( MOD_2 ) <> 1 then
        getmessage( 008 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_2) )
      endif;

      { food security }
      if soccurs( MOD_3 ) <> 1 then
        getmessage( 008 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_3) )
      endif;

      { biomarkers for women }
      wmbio = count( CFTF.MOD_1 where V107 > 0 );
      x = soccurs( MOD_4 );
      if x <> wmbio then
        getmessage( 010 );
        bioe = FileWrite( WriteMSG, alphaMSG, HHNUM, "nutrition", "women", x, wmbio );
      endif;
      
      x = soccurs( ANTHRO_WOMEN );
      if x <> wmbio then
        getmessage( 010 );
        bioe = FileWrite( WriteMSG, alphaMSG, HHNUM, "anthropometry", "women", x, wmbio );
      endif;

      { nutrition/anthro for children }
	  chbio = count( CFTF.MOD_1 where V108 > 0 );
      x = soccurs( MOD_5 );
      if x <> chbio then
        getmessage( 010 );
        bioe = FileWrite( WriteMSG, alphaMSG, HHNUM, "nutrition", "children", x, chbio );
      endif; 

      x = soccurs( ANTHRO_CHILD );
      if x <> chbio then
        getmessage( 010 );
        bioe = FileWrite( WriteMSG, alphaMSG, HHNUM, "anthropometry", "children", x, chbio );
      endif;

      { WEAI - women }
      if soccurs( MOD_61F ) <> ( V101B = 1 ) then
        getmessage( 009 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "female", GetLabel(MOD_61F) )
      endif;
      if soccurs( MOD_62F ) <> ( V101B = 1 ) then
        getmessage( 009 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "female", GetLabel(MOD_62F) )
      endif;
      if soccurs( MOD_63AF ) <> ( V101B = 1 ) then
        getmessage( 009 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "female", GetLabel(MOD_63AF) )
      endif;
      if soccurs( MOD_63BF ) <> ( V101B = 1 ) then
        getmessage( 009 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "female", GetLabel(MOD_63BF) )
      endif;
      if soccurs( MOD_63CF ) <> ( V101B = 1 ) then
        getmessage( 009 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "female", GetLabel(MOD_63CF) )
      endif;
      if soccurs( MOD_64BF ) <> ( V101B = 1 ) then
        getmessage( 009 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "female", GetLabel(MOD_64BF) )
      endif;
      if soccurs( MOD_66AF ) <> ( V101B = 1 ) then
        getmessage( 009 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "female", GetLabel(MOD_66AF) )
      endif;
      if soccurs( MOD_66BF ) <> ( V101B = 1 ) then
        getmessage( 009 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "female", GetLabel(MOD_66BF) )
      endif;


      { WEAI - men }
      if soccurs( MOD_61M ) <> ( V101A = 1 ) then
        getmessage( 009 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "male", GetLabel(MOD_61M) )
      endif;
      if soccurs( MOD_62M ) <> ( V101A = 1 ) then
        getmessage( 009 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "male", GetLabel(MOD_62M) )
      endif;
      if soccurs( MOD_63AM ) <> ( V101A = 1 ) then
        getmessage( 009 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "male", GetLabel(MOD_63AM) )
      endif;
      if soccurs( MOD_63BM ) <> ( V101A = 1 ) then
        getmessage( 009 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "male", GetLabel(MOD_63BM) )
      endif;
      if soccurs( MOD_63CM ) <> ( V101A = 1 ) then
        getmessage( 009 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "male", GetLabel(MOD_63CM) )
      endif;
      if soccurs( MOD_64BM ) <> ( V101A = 1 ) then
        getmessage( 009 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "male", GetLabel(MOD_64BM) )
      endif;
      if soccurs( MOD_66AM ) <> ( V101A = 1 ) then
        getmessage( 009 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "male", GetLabel(MOD_66AM) )
      endif;
      if soccurs( MOD_66BM ) <> ( V101A = 1 ) then
        getmessage( 009 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, "male", GetLabel(MOD_66BM) )
      endif;

      { VCCs } { !!TO DO - must be modified for checking all L-VCC and C-VCC according to appropriate filters in module 2 }
      vcc = ( V234 = 1 ) + ( V236 = 1 ) + ( V238 = 1 );
     { if soccurs( MOD_7_INF ) <> ( vcc > 1 ) then
        getmessage( 008 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_7_INF) )
      endif;}

      if soccurs( MOD_71 ) <> ( V234 = 1 ) then
        getmessage( 008 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_71) )
      endif;

      if soccurs( MOD_72 ) <> ( V236 = 1 ) then
        getmessage( 008 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_72) )
      endif;

      if soccurs( MOD_73 ) <> ( V238 = 1 ) then
        getmessage( 008 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_73) )
      endif;

      { food security }
      if soccurs( MOD_8_INF ) <> 1 then
        getmessage( 008 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_8_INF) )
      endif;
      if soccurs( MOD_81A ) <> 1 then
        getmessage( 008 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_81A) )
      endif;
      if soccurs( MOD_81B ) <> 1 then
        getmessage( 008 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_81B) )
      endif;
      if soccurs( MOD_82 ) <> 1 then
        getmessage( 008 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_82) )
      endif;
      if soccurs( MOD_83 ) <> 1 then
        getmessage( 008 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_83) )
      endif;
      if soccurs( MOD_84 ) <> 1 then
        getmessage( 008 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_84) )
      endif;
      if soccurs( MOD_85 ) <> 1 then
        getmessage( 008 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_85) )
      endif;
      if soccurs( MOD_85B ) <> 1 then
        getmessage( 008 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_85B) )
      endif;
      if soccurs( MOD_86 ) <> 1 then
        getmessage( 008 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_86) )
      endif;
      if soccurs( MOD_87 ) <> 1 then
        getmessage( 008 );
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, GetLabel(MOD_87) )
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
         soccurs(MOD_8_INF) | soccurs(MOD_81A) | soccurs(MOD_81B) | soccurs(MOD_82) | soccurs(MOD_83) | 
         soccurs(MOD_84) | soccurs(MOD_85) | soccurs(MOD_85B) | soccurs(MOD_86) | soccurs(MOD_87) then
        getmessage( 011 );
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
          getmessage( 012 );
          FileWrite( WriteMSG, alphaMSG, HHNUM, XINTCODE(i), AHINTNUM1 );
          XINTCODE(i) = AHINTNUM1;
          rwrthha     = 1;
        endif;
        { synchronize household status in HH assignment file }
        if XRESULT(i) <> AHRESULT then
          XRESULT(i) = AHRESULT;
          rwrthha    = 1;
        endif;
        { synchronize name of household head in HH assignment file }
        if AHRESULT = 1 & length( strip(V101(1)) ) &
           !pos( strip(V101(1)), XNAME(i) ) then
          XNAME(i) = strip(V101(1));
          rwrthha  = 1;
        endif;
        break;
      endif;
    enddo;
    { if household was not found it means that it was added by an interviewer }
    { supervisor then needs to accept it as part of the sample                }
    hhOK = 1;
    if !found then
      errmsg( 013, HHNUM );
      getmessage( 013 );
      FileWrite( WriteMSG, alphaMSG, HHNUM );
      hhOK = ( accept( maketext( "Confirm that household %d is accepted", HHNUM ), "Yes", "No, cancel" ) = 1 );

      if hhOK then
        XTOTAL = XTOTAL + 1;
        i = XTOTAL;
        XNUMBER(i)  = HHNUM;
        XNAME(i)    = V101(1);
        XINTCODE(i) = AHINTNUM1;
        XINTDATE(i) = AHINTM*1000000 + AHINTD*10000 + AHINTY;
        XRESULT(i)  = AHRESULT;
        rwrthha     = 1;
        newhh = newhh + 1;
      else
        hhe = FileWrite( WriteMSG, alphaMSG, HHNUM, AHINTNUM1 );
      endif;
    endif;

    { print line for the household in the report }
    if soccurs( MOD_1 ) then
      xstring = strip(V101(1));
    endif;
    write( "  %03d    %03d                         %03d    %12s",
           HHEA, HHNUM,  AHINTNUM1, yresult );
    { Result lines for each module }
    { Module 2 }           
    box V245 => xstring;
           1 => "Complete";
           4 => "Postponed";
           5 => "Refused";
       10,96 => "Other Inc.";
             => "";
    endbox;       
    write(  "           2-Dwelling                 %03d     %2d-%12s", V246, V245, xstring ); 
    { Module 3 }           
    box V319 => xstring;
           1 => "Complete";
           4 => "Postponed";
           5 => "Refused";
       10,96 => "Other Inc.";
             => "";
    endbox;       
    write(  "           3-FIES                     %03d     %2d-%12s", V320, V319, xstring ); 
    { Module 4 }
    box V433(i) => xstring;
           1 => "Complete";
           4 => "Postponed";
           5 => "Refused";
       10,96 => "Other Inc.";
             => "";
    endbox;       
    write(  "           4-Women                    %03d     %2d-%12s", V434(i), V433(i), xstring ); 

    for i in record ANTHRO_WOMEN do
      box AN406 : AN407       => xstring;
    999.4 :             => "4-Not present";
                : 999.4 => "4-Not present";
    999.5 :             => "5-Refused";
                : 999.5 => "5-Refused";
    999.6 :             => "6-Other";
                : 999.6 => "6-Other";
        notappl : notappl     => "Not visited";
                :       => "1-Measured";
      endbox;       
      if !AN406 in 100.0:200.0,999.4:999.6 | !AN407 in 20.0:150.0,999.94:999.96 then
        zstring = "(flag)";
      else
        zstring = "";
      endif;
      if V104(V400AAD(i))%5 = 0 then
        ystring = "-flag)";
      else
        ystring = ")";
      endif;
      write(  "            %02d-%-10s (%dyr.%s        %13s     %3.1fcm/%3.1fkg  %s", V400AAD(i), strip(V400ADN(i)), V104(V400AAD(i)), strip(ystring), strip(xstring), AN406, AN407, zstring ); 
    enddo;
    
    
    { Module 5 }           
    box V566(i) => xstring;
           1 => "Complete";
           4 => "Postponed";
           5 => "Refused";
       10,96 => "Other Inc.";
             => "";
    endbox;       
    write(  "           5-Children                  %03d     %2d-%12s", V567(i), V566(i), xstring ); 
    
    for i in record ANTHRO_CHILD do
      box AN516 : AN518       => xstring;
    999.4 :             => "4-Not present";
                : 99.94 => "4-Not present";
    999.5 :             => "5-Refused";
                : 99.95 => "5-Refused";
    999.6 :             => "6-Other";
                : 99.96 => "6-Other";
        notappl : notappl     => "Not visited";
                :       => "1-Measured";
      endbox;       
      if !AN516 in 36.0:140.0,999.94:999.96 | !AN518 in 0.5:36.0,999.4:999.6 then
        zstring = "(flag)";
      else
        zstring = "";
      endif;
      if V104(V500AD(i))%5 = 0 then
        ystring = "-flag)";
      else
        ystring = ")";
      endif;
      write(  "            %02d-%-10s (%-dmo.%s            %14s     %3.1fcm/%3.1fkg  %s", V500AD(i), strip(AN500FN(i)), V508(V500AD(i)), strip(ystring), strip(xstring), AN516, AN518, zstring ); 
    enddo;
    
  
    { Module 6F }           
    box V6605 => xstring;
            1 => "Complete";
            4 => "Postponed";
            5 => "Refused";
        10,96 => "Other Inc.";
              => "";
    endbox;       
    write(  "           6F-WEAI                    %03d     %2d-%12s", V6606, V6605, xstring ); 
    { Module 6M }           
    box M6605 => xstring;
            1 => "Complete";
            4 => "Postponed";
            5 => "Refused";
        10,96 => "Other Inc.";
              => "";
    endbox;       
    write(  "           6M-WEAI                    %03d     %2d-%12s", M6606, M6605, xstring ); 
 
    { Module 7.1 }           
    box V7134 => xstring;
            1 => "Complete";
            4 => "Postponed";
            5 => "Refused";
        10,96 => "Other Inc.";
              => "";
    endbox;       
    write(  "           7.1-VCC1                   %03d     %2d-%12s", V7135, V7134, xstring ); { !! - TO DO - replace VCC1 with crop name - maintain total spacing }
    { Module 7.2 }           
    box V7235 => xstring;
            1 => "Complete";
            4 => "Postponed";
            5 => "Refused";
        10,96 => "Other Inc.";
              => "";
    endbox;       
    write(  "           7.2-VCC2                   %03d     %2d-%12s", V7236, V7235, xstring ); { !! - TO DO - replace VCC2 with crop name  - maintain total spacing }
    { Module 7.3 }           
    box V7327 => xstring;
            1 => "Complete";
            4 => "Postponed";
            5 => "Refused";
        10,96 => "Other Inc.";
              => "";
    endbox;       
    write(  "           7.3-VCC3                   %03d     %2d-%12s", V7328, V7327, xstring ); { !! - TO DO - replace VCC3 with crop name  - maintain total spacing }
    { Module 8 }           
    box V8709 => xstring;
            1 => "Complete";
            4 => "Postponed";
            5 => "Refused";
        10,96 => "Other Inc.";
              => "";
    endbox;       
    write(  "           8-Consumption              %03d     %2d-%12s", V8710, V8709, xstring ); 

    { find a place in array of cases in the supervisor control file }
    casefound = 0;
    do i = 1 while i <= 50
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

    if AHRESULT <> 1 | casepart then next endif;  { skip women and go to next household }
    

    if hhe then e = 1 endif;
  enddo;                              { end loop over all households }

  { summary of the run }
  write(  " " );
  write(  "Cluster      HHs    Complete Incomp. Indiv complete Incomp. " );
  write(  "________ ___________________________ ________________________________________ " );
  write(  "  %03d   %7d%10d%8d%7d%8d%8d", ycluster, xht, xhc, xhi, xit, xic, xii );
  if newhh then
    write(  " " );
    write(  "Households added not originally part of the sample design %d", newhh );
  endif;

  { update control file with household totals }	
  updtctrl( 2 );
	
  if chktot then   { totals checked when collapsing all data or when closing a cluster }

    { check totals with supervisor control file }
    do i = 1 while i <= 50 & HH(i) by 1
    enddo;
    hogregis = i-1;
    if xht = SHTOTAL & hogregis = xht then
      if e then
        getmessage( 014 );
        e = FileWrite( WriteMSG, alphaMSG, xht );
      else
        SHCOMP   = xhc;
        SHINCOMP = xhi;
        SFINDATE = sysdate();
        fwrt     = 1;
      endif;
    else
      getmessage( 015 );
      e = FileWrite( WriteMSG, alphaMSG, xht, SHTOTAL, hogregis )
    endif;
  endif;            { endif to check totals }

  { rewrite supervisor control file if necessary }
  if fwrt then writecase( CTRL_CLUSTER ) endif;
  close( CTRL_CLUSTER );
  { rewrite household sample file if necessary }
  if rwrthha then writecase( SAMPSEL ) endif;
  close( SAMPSEL );

  { instructions to create a file to decide if cluster is completed }
  FileReturn( e );
  { totally quit application }
  stop(-1);


  
