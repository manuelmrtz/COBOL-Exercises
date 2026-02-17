       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX-PARAGS-02.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       PROCEDURE DIVISION.
           PERFORM CLEAR-SCREEN
           DISPLAY "Welcome, Manuel!"
           PERFORM 3 TIMES
               DISPLAY "   Welcome to modern COBOL!"
           END-PERFORM
           GOBACK.
           
       CLEAR-SCREEN.
           PERFORM 30 TIMES
               DISPLAY SPACE
           END-PERFORM
           .
