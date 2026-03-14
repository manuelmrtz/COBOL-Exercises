       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CHAP04A03.
       
      *****************************************************************
      * PROGRAM NAME : LIGHT-EM UP                                    *
      * PROGRAM ID   : CHAP04A03                                      *
      *                                                               *
      * AUTHOR       : Manuel A. Martinez                             *
      * DATE WRITTEN : 2026-03-04                                     *
      * DATE COMPILED:                                                *
      *                                                               *
      * INSTALLATION : Manuel Martinez Development Lab                *
      * ENVIRONMENT  : Linux ARM64 GnuCOBOL 3.2                       *
      *                                                               *
      * PROGRAM TYPE : Batch                                          *
      *                                                               *
      *===============================================================*
      *                                                               *
      * DESCRIPTION :                                                 *
      *   THE LIGHT-EM UP UTILITY COMPANY HAS MASTER DISK RECORDS,    *
      *   EACH OF WHICH WILL BE USED TO CREATE AN ELECTRIC BILL       *
      *   RECORD TO BE STORED ON AN ELEC-BILL-FILE AND A GAS BILL     *
      *   RECORD TO BE STORED ON A GAS-BILL-FILE. NOTE THAT FOR EACH  *
      *   INPUT RECORD, THE PROGRAM WILL CREATE TWO DISK RECORDS, ONE *
      *   ON THE ELEC-BILL-FILE AND ONE ON THE GAS-BILL-FILE.         *
      *                                                               *
      *===============================================================*
      *                                                               *
      * BUSINESS PURPOSE :                                            *
      *   Educational                                                 *
      *                                                               *
      *===============================================================*
      *                                                               *
      * INPUT FILES :                                                 *
      *   ---------------------- ------------------------------------ *
      *   | File Name           | Description                       | *
      *   ---------------------- ------------------------------------ *
      *   | account-master.dat  | Customer master data              | *
      *   ---------------------- -----------------------------------| *
      *                                                               *
      * OUTPUT FILES :                                                *
      *   ---------------------- ------------------------------------ *
      *   | File Name           | Description                       | *
      *   ---------------------- ------------------------------------ *
      *   | elec-bill-file.dat  | Electric bill data                | *
      *   | gas-bill-file.dat   | Gas bill Data                     | * 
      *   ---------------------- ------------------------------------ *
      *                                                               *
      *===============================================================*
      *                                                               *
      * ERROR HANDLING :                                              *
      *   - File status validated after every I/O operation           *
      *   - Program aborts with message on critical errors            *
      *   - Warning messages for non-critical issues                  *
      *                                                               *
      * RETURN CODES :                                                *
      *   - 0 : Normal completion                                     *
      *   - 8 : Error encountered                                     *
      *                                                               *
      *===============================================================*
      *                                                               *
      * SPECIAL NOTES :                                               *
      *   [Any special considerations, dependencies, or notes]        *
      *                                                               *
      *===============================================================*
      *                                                               *
      * REVISION HISTORY :                                            *
      *   2026-03-12  MAM  Initial version                            *
      *   YYYY-MM-DD  MAM  [Description of change]                    *
      *                                                               *
      *****************************************************************
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-ACCOUNT-MASTER ASSIGN TO 'data/account-master.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-MASTER-STATUS.
               
           SELECT F-ELEC-BILL ASSIGN TO 'data/elec-bill.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ELEC-STATUS.
               
           SELECT F-GAS-BILL ASSIGN TO 'data/gas-bill.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-GAS-STATUS.
           
       DATA DIVISION.
       FILE SECTION.
       FD  F-ACCOUNT-MASTER.
       01  MASTER-REC.
           05 ACCOUNT-NO-IN     PIC X(5).
           05 CUSTOMER-NAME-IN  PIC X(20).
           05 ADDRESS-IN        PIC X(20).
           05 KILO-HRS-USED-IN  PIC X(5).
           05 GAS-USED-IN       PIC X(5).
           05 ELEC-BILL-IN      PIC X(5).
           05 GAS-BILL-IN       PIC X(5).
           
       FD  F-ELEC-BILL.
       01  ELEC-REC.
           05 EACCOUNT-NO-OUT    PIC X(5).
           05 ECUSTOMER-NAME-OUT PIC X(20).
           05 EADDRESS-OUT       PIC X(20).
           05 EKILO-HRS-USED-OUT PIC X(5).
           05 EELEC-BILL-OUT     PIC X(5).
           
       
       FD  F-GAS-BILL.
       01  GAS-REC.
           05 GACCOUNT-NO-OUT    PIC X(5).
           05 GCUSTOMER-NAME-OUT PIC X(20).
           05 GADDRESS-OUT       PIC X(20).
           05 GGAS-USED-OUT      PIC X(5).
           05 GGAS-BILL-OUT      PIC X(5).
           
       WORKING-STORAGE SECTION.
       01  WS-MASTER-STATUS     PIC XX.
       01  WS-GAS-STATUS        PIC XX.
       01  WS-ELEC-STATUS       PIC XX.
       01  WS-EOF               PIC X  VALUE 'N'.

       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           PERFORM 200-OPEN-FILES
           
           PERFORM UNTIL WS-EOF = 'Y'
           
              READ F-ACCOUNT-MASTER
                 AT END
                    MOVE 'Y' TO WS-EOF
                 NOT AT END
                    PERFORM 310-ELEC-RTN
                    PERFORM 320-GAS-RTN
              END-READ
              IF WS-MASTER-STATUS NOT = '00' AND 
                       WS-MASTER-STATUS NOT = '10'
                 DISPLAY 'ERROR reading master file, status=' 
                       WS-MASTER-STATUS
                 PERFORM 400-CLOSE-FILES
                 STOP RUN
              END-IF
           END-PERFORM
           
           PERFORM 400-CLOSE-FILES
           STOP RUN
           .
       200-OPEN-FILES.
           OPEN INPUT  F-ACCOUNT-MASTER
                OUTPUT F-ELEC-BILL
                OUTPUT F-GAS-BILL
                
           IF WS-MASTER-STATUS NOT = '00'
               DISPLAY 'ERROR: Cannot open MASTER file. Status: ' 
                   WS-MASTER-STATUS
               PERFORM 400-CLOSE-FILES
               STOP RUN
           END-IF
       
           IF WS-ELEC-STATUS NOT = '00'
               DISPLAY 'ERROR: Cannot open ELEC-BILL. Status: ' 
                       WS-ELEC-STATUS
               PERFORM 400-CLOSE-FILES
               STOP RUN
           END-IF
           
           IF WS-GAS-STATUS NOT = '00'
               DISPLAY 'ERROR: Cannot open GAS-BILL. Status: ' 
                       WS-GAS-STATUS
               PERFORM 400-CLOSE-FILES
               STOP RUN
           END-IF
           .

       310-ELEC-RTN.
           MOVE SPACES           TO ELEC-REC
           MOVE ACCOUNT-NO-IN    TO EACCOUNT-NO-OUT
           MOVE CUSTOMER-NAME-IN TO ECUSTOMER-NAME-OUT
           MOVE ADDRESS-IN       TO EADDRESS-OUT
           MOVE KILO-HRS-USED-IN TO EKILO-HRS-USED-OUT
           MOVE ELEC-BILL-IN     TO EELEC-BILL-OUT
           WRITE ELEC-REC           
           .

       320-GAS-RTN.
           MOVE SPACES           TO GAS-REC
           MOVE ACCOUNT-NO-IN    TO GACCOUNT-NO-OUT
           MOVE CUSTOMER-NAME-IN TO GCUSTOMER-NAME-OUT
           MOVE ADDRESS-IN       TO GADDRESS-OUT
           MOVE GAS-USED-IN      TO GGAS-USED-OUT
           MOVE GAS-BILL-IN      TO GGAS-BILL-OUT
           WRITE GAS-REC
           .
           
       400-CLOSE-FILES.
           CLOSE F-ACCOUNT-MASTER
                 F-ELEC-BILL
                 F-GAS-BILL
                 
           IF WS-MASTER-STATUS NOT = '00'
               DISPLAY 'WARNING: Error closing MASTER file. Status: ' 
                   WS-MASTER-STATUS
               STOP RUN
           END-IF
       
           IF WS-ELEC-STATUS NOT = '00'
               DISPLAY 'WARNING: Error closing ELEC-BILL. Status: ' 
                       WS-ELEC-STATUS
               STOP RUN
           END-IF
           
           IF WS-GAS-STATUS NOT = '00'
               DISPLAY 'WARNING: Error closing GAS-BILL. Status: ' 
                       WS-GAS-STATUS
               STOP RUN
           END-IF
           .
           