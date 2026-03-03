       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CHAP01A01.
      *AUTHOR.      Manuel A. Martinez.
      *DATE-WRITTEN. 2026-03-02.
      *DATE-COMPILED.
      *INSTALLATION. Manuel Martinez Development Lab.
      *SECURITY.     Internal / Confidential / Public.
      *ENVIRONMENT.  Linux ARM64 GnuCOBOL 3_2.

      *****************************************************************
      * PROGRAM NAME : CHAP01A01                                      *
      *                                                               *
      * PROGRAM TYPE : Batch                                          *
      *                                                               *
      * DESCRIPTION :                                                 *
      *    The chain Video Trap: Movies for Less needs to create two  *
      *    mailing list for each customer record on file. One mailing *
      *    list will be for video rentals and the other for video     *
      *    sales.                                                     *
      *        a. Print two mailing labels per individual, stacked on *
      *           top of the other.                                   *
      *        b. Now asume that the printer contains perforated      *
      *           stick-on labels that can be printed side by side.   *
      *                                                               *
      * BUSINESS PURPOSE :                                            *
      *   Educational                                                 *
      *                                                               *
      * INPUT :                                                       *
      *   <Files / Tables / Parameters>                               *
      *                                                               *
      * OUTPUT :                                                      *
      *   <Files / Tables / Reports>                                  *
      *                                                               *
      * ERROR HANDLING STRATEGY :                                     *
      *   - File status validated after each I/O                      *
      *                                                               *
      * RETURN CODES :                                                *
      *   NONE                                                        *
      *                                                               *
      * SPECIAL NOTES :                                               *
      *   NONE                                                        *
      *                                                               *
      * REVISION HISTORY :                                            *
      *   2026-03-02  MAM  Initial version                            *
      *   YYYY-MM-DD  MAM  Description of change                      *
      *                                                               *
      ***************************************************************** 
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
           SELECT F-CUSTOMER-FILE ASSIGN TO 'data/customer.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
      *****************************************************************
      *     VLABELS - VERTICAL LABELS, THIS FORMAT WILL PRINT ONE     *
      *               ON TOP OF THE OTHER.                            *
      *                                                               *
      *     HLABELS - HORIZONTAL LABELS, THIS FORMAT WILL PRINT SIDE  *
      *               BY SIDE.                                        *
      *****************************************************************                        
           SELECT F-VLABELS-FILE ASSIGN TO 'data/vlabels.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT F-HLABELS-FILE ASSIGN TO 'data/hlabels.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
       FD  F-CUSTOMER-FILE.
       01  CUSTOMER-REC.
           05 CUSTOMER-NAME-IN PIC X(20).
           05 STREET-ADDR-IN   PIC X(20).
           05 CITY-ADDR-IN     PIC X(10).
           05 STATE-ADDR-IN    PIC X(3).
           05 ZIP-CODE-ADDR-IN PIC X(5).
           
       FD  F-VLABELS-FILE.
       01  VLABELS-REC.
           05 dummy pic x(1).
       
       FD  F-HLABELS-FILE.
       01  HLABELS-REC.
           05 dummy pic x(1).
       
       WORKING-STORAGE SECTION.
       
       
       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           STOP RUN
           .
       