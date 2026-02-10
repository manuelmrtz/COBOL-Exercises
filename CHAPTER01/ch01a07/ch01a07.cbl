       >>SOURCE FORMAT IS FIXED
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CH01A07.
      *AUTHOR. MANUEL A. MARTINEZ.
      *DATE-WRITTEN. 02-09-2026.
      *REMARKS.
      ******************************************************************
      *    Speeds used in aeronautical or maritime application are     *
      *    ussualy given in knots, or nauticale miles per hour, rather *
      *    than in statute miles per hour, which is the commonly used  *
      *    unit on land. Code an interactive program in its entirely   *
      *    that will accept a value in knots, convert it to miles per  *
      *    hour, and then display the converted value (1 knot = 1.15   *
      *    statute miles per hour).                                    *
      ******************************************************************

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  KNOTS-IN PIC 999.
       01  MILES-OUT PIC 999.
       
       PROCEDURE DIVISION.
       001-MAIN.
           DISPLAY 'Enter Speed in Knots: '
           ACCEPT KNOTS-IN
           COMPUTE MILES-OUT = KNOTS-IN * 1.15
           DISPLAY 'Speed in Miles per Hour: ' MILES-OUT
           STOP RUN
           .
