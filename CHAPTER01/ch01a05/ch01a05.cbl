       >>SOURCE FORMAT FIXED
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CH01A05.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALES-FILE ASSIGN TO "DATA/SALES.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT PRINT-FILE ASSIGN TO "DATA/PRINTER.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  SALES-FILE.
       01  SALES-RECORD.
           05  NAME-IN               PICTURE X(15).
           05  AMOUNT-OF-SALES-IN    PICTURE 999V99.
           
       FD  PRINT-FILE.
       01  PRINT-REC.
           05  FILLER                PICTURE X(20).
           05  NAME-OUT              PICTURE X(15).
           05  FILLER                PICTURE X(20).
           05  AMT-COMMISSION-OUT    PICTURE 99.99.
           05  FILLER                PICTURE X(72).
           
       WORKING-STORAGE SECTION.
       01  ARE-THERE-MORE-RECORDS    PIC XXX VALUE 'YES'.
       01  AMT-COMMISSION-NUM        PIC 99V99.
               
       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           OPEN INPUT SALES-FILE
                OUTPUT PRINT-FILE
           
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO '
             READ SALES-FILE
               AT END
                 MOVE 'NO ' TO ARE-THERE-MORE-RECORDS
               NOT AT END
                 PERFORM 200-COMMISSION-RTN      
             END-READ
           END-PERFORM
           CLOSE SALES-FILE
                 PRINT-FILE                
           STOP RUN
           .
       200-COMMISSION-RTN.
           MOVE SPACES TO PRINT-REC
           MOVE NAME-IN TO NAME-OUT
           IF AMOUNT-OF-SALES-IN IS GREATER THAN 100.00
              MULTIPLY 0.03 BY AMOUNT-OF-SALES-IN
                GIVING AMT-COMMISSION-NUM
           ELSE
              MULTIPLY 0.02 BY AMOUNT-OF-SALES-IN
                GIVING AMT-COMMISSION-NUM
           END-IF
           MOVE AMT-COMMISSION-NUM TO AMT-COMMISSION-OUT
           WRITE PRINT-REC
           .
