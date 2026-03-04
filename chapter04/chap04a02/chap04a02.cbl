       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CHAP04A02.
       
      *****************************************************************
      * PROGRAM NAME : Assignment 02 FROM CHAPTER 04                  *
      * PROGRAM ID   : CHAP04A02                                      *
      *                                                               *
      * AUTHOR       : Manuel A. Martinez                             *
      * DATE WRITTEN : 2026-03-03                                     *
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
      *   WRITE A POGRAM TO PRINT ALL INFORMATION FROM PAYROLL        *
      *   RECORDS FOR EMPLOYEES OF THE INTERNATIONAL CHERRY MACHINE   *
      *   COMPANY (ICM). THE PROBLEM DEFINITION IS SHOWN IN FIGURE    *
      *   4.6.                                                        *
      *                                                               *
      *===============================================================*
      *                                                               *
      * BUSINESS PURPOSE :                                            *
      *   Educational                                                 *
      *                                                               *
      *===============================================================*
      *                                                               *
      * INPUT FILES :                                                 *
      *   -------------------- -------------------------------------- *
      *   | File Name       | Description                           | *
      *   -------------------- -------------------------------------- *
      *   | prmaster.dat    | Payroll Master Data                   | *
      *   -------------------- -------------------------------------- *
      *                                                               *
      * OUTPUT FILES :                                                *
      *   -------------------- -------------------------------------- *
      *   | File Name       | Description                           | *
      *   -------------------- -------------------------------------- *
      *   | prlist.prt      | Payroll List                          | *
      *   -------------------- -------------------------------------- *
      *                                                               *
      *===============================================================*
      *                                                               *
      * ERROR HANDLING :                                              *
      *   - File status validated after every I/O operation           *
      *                                                               *
      *===============================================================*
      *                                                               *
      * SPECIAL NOTES :                                               *
      *   [Any special considerations, dependencies, or notes]        *
      *                                                               *
      *===============================================================*
      *                                                               *
      * REVISION HISTORY :                                            *
      *   2026-03-03  MAM  Initial version                            *
      *   YYYY-MM-DD  MAM  [Description of change]                    *
      *                                                               *
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-PR-MASTER ASSIGN TO "data/prmast.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS WS-MASTER-STATUS.
               
           SELECT F-PR-LIST ASSIGN TO "data/prlist.prt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS WS-LIST-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  F-PR-MASTER.
       01  MASTER-REC.
           05 EMPLOYEE-NO-IN         PIC X(5).
           05 EMPLOYEE-NAME-IN       PIC X(20).
           05 LOCATION-CODE-IN.
               10 TERRITORY-IN       PIC X(2).
               10 OFFICE-NO-IN       PIC X(2).
           05 ANNUAL-SALARY-IN       PIC X(6).
           05 SOCIAL-SECURITY-NO-IN  PIC X(9).
           05 NO-OF-DEPENDENTS-IN    PIC X(2).
           05 JOB-CLASSIFICATION-IN  PIC X(2).
           
       FD  F-PR-LIST.
       01  REPORT-HEADER-LINE.
           05 HEADER-TEXT            PIC X(80).
       
       01  REPORT-DETAIL.
           05 FILLER                 PIC X(5).
           05 EMPLOYEE-NO-OUT        PIC X(5).
           05 FILLER                 PIC X(2).
           05 EMPLOYEE-NAME-OUT      PIC X(20).
           05 FILLER                 PIC X(1).
           05 LOCATION-CODE-OUT.
               10 TERRITORY-OUT      PIC X(2).
               10 FILLER             PIC X(3).
               10 OFFICE-NO-OUT      PIC X(2).
           05 FILLER                 PIC X(2).
           05 ANNUAL-SALARY-OUT      PIC X(6).
           05 FILLER                 PIC X(2).
           05 SOCIAL-SECURITY-NO-OUT PIC X(9).
           05 FILLER                 PIC X(2).
           05 NO-OF-DEPENDENTS-OUT   PIC X(2).
           05 FILLER                 PIC X(2).
           05 JOB-CLASSIFICATION-OUT PIC X(2).
           05 FILLER                 PIC X(39).
       
       WORKING-STORAGE SECTION.
       01  WS-MASTER-STATUS          PIC XX.
       01  WS-LIST-STATUS            PIC XX.
       01  WS-EOF                    PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           PERFORM 200-OPEN-FILES
           
           PERFORM 300-PRINT-REPORT-RTN      
       
           PERFORM 210-CLOSE-FILES
           
           STOP RUN
           .
           
       200-OPEN-FILES.
           OPEN INPUT  F-PR-MASTER
                OUTPUT F-PR-LIST
       
           IF WS-MASTER-STATUS NOT = '00'
               DISPLAY 'ERROR: Cannot open MASTER file. Status: ' 
                   WS-MASTER-STATUS
               PERFORM 210-CLOSE-FILES
               STOP RUN
           END-IF
       
           IF WS-LIST-STATUS NOT = '00'
               DISPLAY 'ERROR: Cannot open PRLIST file. Status: ' 
                       WS-LIST-STATUS
               PERFORM 210-CLOSE-FILES
               STOP RUN
           END-IF
           .
           
       210-CLOSE-FILES.
           CLOSE F-PR-MASTER
                 F-PR-LIST
           
           IF WS-MASTER-STATUS NOT = '00'
               DISPLAY 'WARNING: Error closing PRMASTER file. Status: ' 
                       WS-MASTER-STATUS
           END-IF
       
           IF WS-LIST-STATUS NOT = '00'
               DISPLAY 'WARNING: Error closing PRLIST file. Status: ' 
                       WS-LIST-STATUS
           END-IF
           .

       300-PRINT-REPORT-RTN.
           PERFORM UNTIL WS-EOF = 'Y'
               READ F-PR-MASTER
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE SPACES TO REPORT-DETAIL
                       MOVE EMPLOYEE-NO-IN TO EMPLOYEE-NO-OUT
                       MOVE EMPLOYEE-NAME-IN TO EMPLOYEE-NAME-OUT
                       MOVE TERRITORY-IN TO TERRITORY-OUT
                       MOVE OFFICE-NO-IN TO OFFICE-NO-OUT
                       MOVE ANNUAL-SALARY-IN TO ANNUAL-SALARY-OUT
                       MOVE SOCIAL-SECURITY-NO-IN TO 
                            SOCIAL-SECURITY-NO-OUT
                       MOVE NO-OF-DEPENDENTS-IN TO NO-OF-DEPENDENTS-OUT
                       MOVE JOB-CLASSIFICATION-IN TO 
                            JOB-CLASSIFICATION-OUT
                       WRITE REPORT-DETAIL
               END-READ
               IF WS-MASTER-STATUS NOT = '00' AND 
                  WS-MASTER-STATUS NOT = '10'
                   DISPLAY 'ERROR reading master file, status=' 
                            WS-MASTER-STATUS
                   PERFORM 210-CLOSE-FILES
                   STOP RUN
               END-IF
           END-PERFORM
           .
