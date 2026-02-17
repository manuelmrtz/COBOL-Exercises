       IDENTIFICATION DIVISION.
       PROGRAM-ID. CH03A08.
      *AUTHOR. MANUEL A. MARTINEZ.
      *DATE-WRITTEN. 02-16-2026.
       
      ******************************************************************
      * Write an interactive COBOL program to key in an                *
      * INVENTORY-PART-NO, QUANTITY-OF-ITEMS-ON-HAND, and UNIT-PRICE   *
      * per item. For each set of variable, DISPLAY the                * 
      * INVENTORY-PART-NO and its TOTAL-VALUE                          *
      * (UNIT-PRICE X QUANTITY-OF-ITEMS-ON-HAND).                      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  INVENTORY-PART-NO           PIC X(15)         VALUE SPACES.
       01  QUANTITY-OF-ITEMS-ON-HAND   PIC 99            VALUE ZERO.
       01  UNIT-PRICE                  PIC 9(4)V99       VALUE ZERO.
       01  TOTAL-VALUE                 PIC 9(10)V99      VALUE ZERO.
       01  TOTAL-VALUE-OUT             PIC $$,$$$,$$$,$$9.99 VALUE ZERO.
       
       
       PROCEDURE DIVISION.
       100-MAIN.
           DISPLAY " "
           DISPLAY "ENTER ITEM NO. : "
           ACCEPT INVENTORY-PART-NO
           DISPLAY "ENTER Q-O-H : "
           ACCEPT QUANTITY-OF-ITEMS-ON-HAND
           DISPLAY "ENTER UNIT PRICE : "
           ACCEPT UNIT-PRICE
           
           MULTIPLY QUANTITY-OF-ITEMS-ON-HAND BY UNIT-PRICE
               GIVING TOTAL-VALUE
           
           MOVE TOTAL-VALUE TO TOTAL-VALUE-OUT
           DISPLAY " "
           DISPLAY "ITEM NO. : " INVENTORY-PART-NO
           DISPLAY "TOTAL VALUE : " TOTAL-VALUE-OUT
           GOBACK
           .
           