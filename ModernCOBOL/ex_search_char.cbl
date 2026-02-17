       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'EX_SEARCH_CHAR'.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  THE-CHARS VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
           05  CHAR-SET OCCURS 26 TIMES INDEXED BY SUBS.
               10  A-CHAR PIC X.
               
       01  ZCHAR PIC X.
                  
       PROCEDURE DIVISION.
       100-MAIN.
           DISPLAY "Subject: [" THE-CHARS "]"
           SET SUBS TO 1
           MOVE 'N' TO ZCHAR
           SEARCH CHAR-SET
               AT END
                   DISPLAY ZCHAR ' Bummer!'
               WHEN ZCHAR = A-CHAR(SUBS)
                   DISPLAY ' Gotcha: ' A-CHAR(SUBS) ' @ LOC: ' SUBS
           GOBACK
           .
           