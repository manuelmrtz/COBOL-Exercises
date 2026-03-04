       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CHAP04A01.
      *AUTHOR.      Manuel A. Martinez.
      *DATE-WRITTEN. 2026-03-02.
      *DATE-COMPILED.
      *INSTALLATION. Manuel Martinez Development Lab.
      *SECURITY.     Internal / Confidential / Public.
      *ENVIRONMENT.  Linux ARM64 GnuCOBOL 3_2.

      *****************************************************************
      * PROGRAM NAME : CHAP04A01                                      *
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
      *    F-CUSTOMER-FILE = data/customer.dat                        *
      *                                                               *
      * OUTPUT :                                                      *
      *    F-VLABELS-FILE  = data/vlabels.prt                         *
      *    F-HLABELS-FILE  = data/hlabels.prt                         *
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
       
           SELECT F-CUSTOMER-FILE ASSIGN TO "data/customer.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CUSTOMER-STATUS.
               
      *****************************************************************
      *     VLABELS - VERTICAL LABELS, THIS FORMAT WILL PRINT ONE     *
      *               ON TOP OF THE OTHER.                            *
      *                                                               *
      *     HLABELS - HORIZONTAL LABELS, THIS FORMAT WILL PRINT SIDE  *
      *               BY SIDE.                                        *
      *****************************************************************                        
           
           SELECT F-VLABELS-FILE ASSIGN TO "data/vlabels.prt"
               ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS IS WS-VLABELS-STATUS.
                         
           SELECT F-HLABELS-FILE ASSIGN TO "data/hlabels.prt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-HLABELS-STATUS.
           
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
           05 ADDRESS-LINE-01 PIC X(20).
       
       FD  F-HLABELS-FILE.
       01  HLABELS-REC.
           05 ADDRESS-LINE-02A PIC X(20).
           05 DUMMY            PIC X(15).
           05 ADDRESS-LINE-02B PIC X(20).
       
       WORKING-STORAGE SECTION.
       01  WS-EOF              PIC X      VALUE 'N'.
       01  WS-CUSTOMER-STATUS    PIC XX.
       01  WS-VLABELS-STATUS     PIC XX.
       01  WS-HLABELS-STATUS     PIC XX.
       
       PROCEDURE DIVISION.
       100-MAIN-MODULE.
      *    VERTICAL LABELS ROUTINE
           PERFORM 200-OPEN-FILES
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ F-CUSTOMER-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END     
                       PERFORM 400-PROCESS-VLABEL-RTN
                       PERFORM 500-PROCESS-HLABEL-RTN
               END-READ
               
      *    Check for read errors (excluding normal end-of-file)
               IF WS-CUSTOMER-STATUS NOT = '00' AND 
                  WS-CUSTOMER-STATUS NOT = '10'
                   DISPLAY 'ERROR: Reading CUSTOMER file. Status: ' 
                           WS-CUSTOMER-STATUS
                   PERFORM 300-CLOSE-FILES
                   STOP RUN
               END-IF
       
           END-PERFORM
           PERFORM 300-CLOSE-FILES
           
           
           
           STOP RUN
           .
       
       200-OPEN-FILES.
           OPEN INPUT  F-CUSTOMER-FILE
                OUTPUT F-VLABELS-FILE
                OUTPUT F-HLABELS-FILE
                
           IF WS-CUSTOMER-STATUS NOT = '00'
               DISPLAY 'ERROR: Cannot open CUSTOMER file. Status: ' 
                   WS-CUSTOMER-STATUS
               STOP RUN
           END-IF
       
           IF WS-VLABELS-STATUS NOT = '00'
               DISPLAY 'ERROR: Cannot open VLABELS file. Status: ' 
                       WS-VLABELS-STATUS
               STOP RUN
           END-IF
       
           IF WS-HLABELS-STATUS NOT = '00'
               DISPLAY 'ERROR: Cannot open HLABELS file. Status: ' 
                       WS-HLABELS-STATUS
               STOP RUN
           END-IF
           .
       300-CLOSE-FILES.
           CLOSE F-CUSTOMER-FILE
                 F-VLABELS-FILE
                 F-HLABELS-FILE
           
           IF WS-CUSTOMER-STATUS NOT = '00'
               DISPLAY 'WARNING: Error closing CUSTOMER file. Status: ' 
                       WS-CUSTOMER-STATUS
           END-IF
       
           IF WS-VLABELS-STATUS NOT = '00'
               DISPLAY 'WARNING: Error closing VLABELS file. Status: ' 
                       WS-VLABELS-STATUS
           END-IF
       
           IF WS-HLABELS-STATUS NOT = '00'
               DISPLAY 'WARNING: Error closing HLABELS file. Status: ' 
                       WS-HLABELS-STATUS
           END-IF
           .
       
       400-PROCESS-VLABEL-RTN.
           PERFORM 2 TIMES
               MOVE SPACES TO ADDRESS-LINE-01
               MOVE ALL '-' TO ADDRESS-LINE-01
               PERFORM 410-WRITE-VLABELS
               
               MOVE SPACES TO ADDRESS-LINE-01
               MOVE CUSTOMER-NAME-IN TO ADDRESS-LINE-01
               PERFORM 410-WRITE-VLABELS
               
               MOVE SPACES TO ADDRESS-LINE-01
               MOVE STREET-ADDR-IN   TO ADDRESS-LINE-01
               PERFORM 410-WRITE-VLABELS
                          
               MOVE SPACES TO ADDRESS-LINE-01           
               STRING FUNCTION TRIM(CITY-ADDR-IN) ", "
                      FUNCTION TRIM(STATE-ADDR-IN) " "
                      FUNCTION TRIM(ZIP-CODE-ADDR-IN)
                      DELIMITED BY SIZE
                      INTO ADDRESS-LINE-01
               END-STRING
               PERFORM 410-WRITE-VLABELS
           END-PERFORM
           .
           
       410-WRITE-VLABELS.
           WRITE VLABELS-REC
           IF WS-VLABELS-STATUS NOT = '00'
               DISPLAY 'ERROR: Writing to VLABELS file. Status: ' 
                       WS-VLABELS-STATUS
               PERFORM 300-CLOSE-FILES
               STOP RUN
           END-IF
           .      
           
       500-PROCESS-HLABEL-RTN.
           MOVE SPACES TO HLABELS-REC
           MOVE ALL '-' TO HLABELS-REC
           PERFORM 510-WRITE-HLABELS 
              
           MOVE SPACES TO HLABELS-REC
           MOVE CUSTOMER-NAME-IN TO ADDRESS-LINE-02A
           MOVE CUSTOMER-NAME-IN TO ADDRESS-LINE-02B           
           PERFORM 510-WRITE-HLABELS
           
           MOVE SPACES TO HLABELS-REC
           MOVE STREET-ADDR-IN   TO ADDRESS-LINE-02A
           MOVE STREET-ADDR-IN   TO ADDRESS-LINE-02B
           PERFORM 510-WRITE-HLABELS
                          
           MOVE SPACES TO HLABELS-REC
           STRING FUNCTION TRIM(CITY-ADDR-IN) ", "
                  FUNCTION TRIM(STATE-ADDR-IN) " "
                  FUNCTION TRIM(ZIP-CODE-ADDR-IN)
                  DELIMITED BY SIZE
                  INTO ADDRESS-LINE-02A
           END-STRING
           MOVE ADDRESS-LINE-02A TO ADDRESS-LINE-02B
           PERFORM 510-WRITE-HLABELS
           .
       510-WRITE-HLABELS.
           WRITE HLABELS-REC
           IF WS-HLABELS-STATUS NOT = '00'
               DISPLAY 'ERROR: Writing to HLABELS file. Status: ' 
                       WS-HLABELS-STATUS
               PERFORM 300-CLOSE-FILES
               STOP RUN
           END-IF
           .          
           