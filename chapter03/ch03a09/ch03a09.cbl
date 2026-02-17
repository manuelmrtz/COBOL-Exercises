       IDENTIFICATION DIVISION.
       PROGRAM-ID. CH03A09.
      *AUTHOR. MANUEL A. MARTINEZ.
      *DATE-WRITTEN. 02-17-2026.
       
      ******************************************************************
      * Write an interactive COBOL program to key in a CUSTOMER-NAME   *
      * and AMT-OF-PURCHASE. For each set of variables, DISPLAY the    *
      * CUSTOMER-NAME and BALANCE-DUE, where BALANCE-DUE is the        *
      * AMT-OF-PURCHASE less a 10 percent discount.                    *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CUSTOMER-NAME       PIC X(30)     VALUE SPACES.
       01  AMT-OF-PURCHASE     PIC 9(4)V99   VALUE ZERO.
       01  AMT-OF-DISCOUNT     PIC 9(4)v99   VALUE ZERO.
       01  DISCOUNT-PERCENT    PIC 9(2)V99   VALUE 0.10.
       01  BALANCE-DUE         PIC 9(4)V99   VALUE ZERO.
       01  BALANCE-DUE-OUT     PIC $$,$$9.99 VALUE ZERO.
       
       
       PROCEDURE DIVISION.
       100-MAIN.
           DISPLAY " "
           DISPLAY "ENTER CUSTOMER NAME :"
           ACCEPT CUSTOMER-NAME
           DISPLAY "ENTER AMOUNT OF PURCHASE :"
           ACCEPT AMT-OF-PURCHASE
           
           MULTIPLY AMT-OF-PURCHASE BY DISCOUNT-PERCENT 
              GIVING AMT-OF-DISCOUNT
           
           COMPUTE BALANCE-DUE = AMT-OF-PURCHASE - AMT-OF-DISCOUNT    
           
           MOVE BALANCE-DUE TO BALANCE-DUE-OUT
           DISPLAY " "
           DISPLAY "CUSTOMER NAME : " CUSTOMER-NAME
           DISPLAY "BALANCE DUE : " BALANCE-DUE-OUT
           GOBACK
           .
           