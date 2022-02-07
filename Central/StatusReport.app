PROC GLOBAL

  set explicit;

  numeric lines, xreceiv, xaccept, xedit, yedit, xhtotal, xfinal, xcluster;

  alpha( 140 ) line5, line4, line1, line2, line3;
  alpha( 8 )   xmuser,  xvuser;

PROC CONTROL_FF
preproc
  lines = 1;
  line1 = "       	FEED THE FUTURE - ZOI SURVEYS - COUNTRY YYYY-YY - Data Processing Progress Report			"; { TO DO !! - modify country name and year of survey }									
  line2 = "        																								";
  line3 = "        Received                                      Accepted     Sec. Edit   Households    Closed";
  line4 = "Cluster (yymmdd)        Supervisor                    (yymmdd)        (n)         (n)       (yymmdd) ";
  line5 = "=====================================================================================================";
  write( "%s", line1 );
  write( "%s", line2 );
  write( "%s", line3 );
  write( "%s", line4 );
  write( "%s", line5 );
  xreceiv = 0;
  xaccept = 0;
  xedit   = 0;
  xhtotal = 0;
  xfinal  = 0;

postproc
    write( "%s", line5 );
    INAME = " ";
    write( "  %3d    %6d      %s    %6d     %6d      %6d         %6d",
          xcluster, xreceiv, INAME, xaccept, xedit, xhtotal, xfinal );
    xreceiv  = 100 * xreceiv  / xcluster;
    xaccept  = 100 * xaccept  / xcluster;
    xedit    = 100 * xedit    / xcluster;
    xfinal   = 100 * xfinal   / xcluster;
    xcluster = 100 * xcluster / xcluster;
    write( "  %3d%   %6d%      %s   %6d%    %6d%                    %6d%",
          xcluster, xreceiv, INAME, xaccept, xedit, xfinal );

PROC LEVEL_1
preproc
  xcluster = xcluster + 1;
  xreceiv  = xreceiv + ( CDATEBEG <> 0 );
  xaccept  = xaccept + ( CACCEPT <> 0 );
  xfinal   = xfinal  + ( CFINDATE <> 0 );
  yedit    = count( CEDIT where CEDIT <> 0 );
  xedit    = xedit + ( yedit > 0 );
  xhtotal  = xhtotal + CHTOTAL;
  if CDATEBEG then
    INAME = " ";
    loadcase( INTERV, CSUPERV ); { !! TO DO GD - make sure CSUPERV is getting set during accepting stage }
    write( "  %3d    %6d      %s    %6d     %6d      %6d         %6d",
          CSAMPLE, CDATEBEG, INAME, CACCEPT, yedit, CHTOTAL, CFINDATE );
    lines   = lines + 1;
    if lines >= 77 then           { 63 for A4 paper }
      lines = 1;
      write( "%s", line1 );
      write( "%s", line2 );
      write( "%s", line3 );
    endif;
  endif;
