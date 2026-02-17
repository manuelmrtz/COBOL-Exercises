       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX-PARAGS-04.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JUSTA-FILE ASSIGN TO 'ex_parags_04.cbl'
           ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
       FD JUSTA-FILE.
       01  FILE-REC.
           05  FULL-LINE PICTURE X(132).
       
       WORKING-STORAGE SECTION.
       
       01  A-LINE    PIC X(132) VALUE SPACES.
       01  MORE-RECORDS PIC X VALUE 'Y'.
            
       PROCEDURE DIVISION.
           OPEN INPUT JUSTA-FILE.
           PERFORM UNTIL MORE-RECORDS = 'N'
               READ JUSTA-FILE INTO A-LINE
               AT END
                   DISPLAY 'DONE!'
                   MOVE 'N' TO MORE-RECORDS
               NOT AT END
                   DISPLAY '[' A-LINE ']'
           END-PERFORM
           
           CLOSE JUSTA-FILE
           
           DISPLAY 'Normal End-of Job?'
           
           GOBACK
           .
           