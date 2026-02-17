       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX-PARAGS-03.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  WS-YOUR-NAME    PIC X(6) VALUE "MANUEL".
       01  WS-LINE-COUNT   PIC 9(2) VALUE 30.
       
       PROCEDURE DIVISION.
           PERFORM CLEAR-SCREEN
           DISPLAY "Welcome, <" WS-YOUR-NAME ">!"
           PERFORM 3 TIMES
               DISPLAY "   Welcome to modern COBOL!"
           END-PERFORM
           
           MOVE 5 TO WS-LINE-COUNT
           GO TO CLEAR-SCREEN
           GOBACK
           .
           
           
       CLEAR-SCREEN.
           PERFORM WS-LINE-COUNT TIMES
               DISPLAY SPACE WS-LINE-COUNT
           END-PERFORM
           .
