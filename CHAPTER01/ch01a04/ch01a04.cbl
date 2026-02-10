       >>SOURCE FORMAT FIXED
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CH01Q04.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-DATA ASSIGN TO "DATA\EMPLOYEE.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
              
           SELECT PAYROLL-LISTING ASSIGN TO "DATA\PRINTER.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-DATA.
       01  EMPLOYEE-RECORD.
           05  EMPLOYEE-NAME-IN    PIC X(20).
           05  HOURS-WORKED-IN     PIC 9(2).
           05  HOURLY-RATE-IN      PIC 9V99.
       
       FD  PAYROLL-LISTING.
       01  PRINT-REC.
           05                      PIC X(20).
           05  NAME-OUT            PIC X(20).
           05                      PIC X(10).
           05  HOURS-OUT           PIC 9(2).
           05                      PIC X(8).
           05  RATE-OUT            PIC 9.99.
           05                      PIC X(6).
           05  WEEKLY-WAGES-OUT    PIC 999.99.
           
       WORKING-STORAGE SECTION.
       01  ARE-THERE-MORE-RECORDS  PIC XXX VALUE 'YES'.           
       
       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           OPEN INPUT EMPLOYEE-DATA
                OUTPUT PAYROLL-LISTING
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO '
             READ EMPLOYEE-DATA
               AT END
                 MOVE 'NO ' TO ARE-THERE-MORE-RECORDS
               NOT AT END
                 PERFORM 200-WAGE-ROUTINE      
             END-READ
           END-PERFORM
           CLOSE EMPLOYEE-DATA
                 PAYROLL-LISTING
           STOP RUN
           .
       200-WAGE-ROUTINE.
           MOVE SPACES TO PRINT-REC
           MOVE EMPLOYEE-NAME-IN TO NAME-OUT
           MOVE HOURS-WORKED-IN TO HOURS-OUT
           MOVE HOURLY-RATE-IN TO RATE-OUT
           MULTIPLY HOURS-WORKED-IN BY HOURLY-RATE-IN 
                    GIVING WEEKLY-WAGES-OUT
           WRITE PRINT-REC
           .
