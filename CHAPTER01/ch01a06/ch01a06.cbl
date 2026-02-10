       >>SOURCE FORMAT FIXED
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CH01A06.
      *AUTHOR. MANUEL A. MARTINEZ.
      *DATE-WRITTEN. 02-09-2026.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYROLL-IN ASSIGN TO "DATA/DISK1.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
               
           SELECT PAYROLL-OUT ASSIGN TO "DATA/DISK2.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
         
       DATA DIVISION.
       FILE SECTION.
       FD  PAYROLL-IN.
       01  PAYROLL-REC.
           05  EMPLOYEE-NUMBER-IN          PIC 9(5).
           05  EMPLOYEE-NAME-IN            PIC X(20).
           05  LOCATION-CODE-IN            PIC 9999.
           05  ANNUAL-SALARY-IN            PIC 9(6).
           
       FD  PAYROLL-OUT.
       01  RECORD-OUT.
           05 EMPLOYEE-NUMBER-OUT         PIC 9(5).
           05  EMPLOYEE-NAME-OUT           PIC X(20).
           05  ANNUAL-SALARY-OUT           PIC 9(6).
       
       WORKING-STORAGE SECTION.
       01  ARE-THERE-MORE-RECORDS          PICTURE x(3) VALUE 'YES'.
       
       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           OPEN INPUT PAYROLL-IN
                OUTPUT PAYROLL-OUT
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO '
             READ PAYROLL-IN
               AT END
                 MOVE 'NO ' TO ARE-THERE-MORE-RECORDS
               NOT AT END
                 PERFORM 200-WAGE-ROUTINE
             END-READ
           END-PERFORM
           CLOSE PAYROLL-IN
                 PAYROLL-OUT
           STOP RUN
           .
           
       200-WAGE-ROUTINE.
           MOVE EMPLOYEE-NUMBER-IN TO EMPLOYEE-NUMBER-OUT
           MOVE EMPLOYEE-NAME-IN TO EMPLOYEE-NAME-OUT
           ADD 1000, ANNUAL-SALARY-IN
               GIVING ANNUAL-SALARY-OUT
           WRITE RECORD-OUT
           .
