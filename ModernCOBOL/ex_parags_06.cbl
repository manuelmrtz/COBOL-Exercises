       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX-PARAGS-06.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JUSTA-FILE ASSIGN TO 'ex_parags_05.cbl'
           ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
       FD JUSTA-FILE.
       01  FILE-REC.
           05  FULL-LINE PICTURE X(132).
       
       WORKING-STORAGE SECTION.
       
       01  A-LINE    PIC X(132) VALUE SPACES.
       01  WS-EOF    PIC X VALUE 'N'.
           88  AT-END     VALUE 'Y'.
            
       PROCEDURE DIVISION.
           OPEN INPUT JUSTA-FILE.
           PERFORM UNTIL AT-END
               READ JUSTA-FILE INTO A-LINE
               AT END
                   DISPLAY 'DONE!'
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   DISPLAY '[' A-LINE ']'
           END-PERFORM
           
           CLOSE JUSTA-FILE
           
           DISPLAY 'Normal End-of Job?'
           
           GOBACK
           .
           