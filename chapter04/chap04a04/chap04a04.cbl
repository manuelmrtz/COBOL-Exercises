       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CHAP04A04.
       
      *****************************************************************
      * PROGRAM NAME : [Program Name]                                 *
      * PROGRAM ID   : CHAP04A04                                      *
      *                                                               *
      * AUTHOR       : Manuel A. Martinez                             *
      * DATE WRITTEN : 2026-03-18                                     *
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
      *   The Video Trap has one input file containing data on video  *
      * tapes for rent and one input file containing data on video    *
      * tapes for sale. Create a single master file where each record *
      * contains data from each file.                                 *
      *                                                               *
      *   Both files have exactly the same item numbers in the same   *
      * sequence.                                                     *
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
      *   | File Name         | Description                         | *
      *   -------------------- -------------------------------------- *
      *   | rentals.dat       | Video tapes for rent                | *
      *   | sales.dat         | Video tapes for sale                | *
      *   -------------------- -------------------------------------- *
      *                                                               *
      * OUTPUT FILES :                                                *
      *   -------------------- -------------------------------------- *
      *   | File Name         | Description                         | *
      *   -------------------- -------------------------------------- *
      *   | master.dat        | Video tape master file              | *
      *   -------------------- -------------------------------------- *
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
      *   2026-03-18  MAM  Initial version                            *
      *                                                               *
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RENTALS-FILE ASSIGN TO 'data/rentals.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS  IS WS-RENTAL-STATUS.
               
           SELECT SALES-FILE ASSIGN TO 'data/sales.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS  IS WS-SALES-STATUS.
           
           SELECT MASTER-FILE ASSIGN TO 'data/master.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS  IS WS-MASTER-STATUS.
           
               
       DATA DIVISION.
       FILE SECTION.
       FD RENTALS-FILE.
       01  RENTALS-REC.
           05 ITEM-NO-IN    PIC X(3).
           05 VIDEO-NAME-IN PIC X(17).
           05 RENTAL-OH-IN  PIC X(3).
           
       FD  SALES-FILE.
       01  SALES-REC.
           05 ITEM-NO-IN    PIC X(3).
           05 VIDEO-NAME-IN PIC X(17).
           05 SALES-OH-IN  PIC X(3).
       
       FD MASTER-FILE.
       01  MASTER-REC.
           05 ITEM-NO    PIC X(3).
           05 VIDEO-NAME PIC X(17).
           05 RENTAL-OH  PIC X(3).
           05 SALES-OH  PIC X(3).
           
       WORKING-STORAGE SECTION.
       01  WS-MASTER-STATUS PIC XX.
       01  WS-SALES-STATUS  PIC XX.
       01  WS-RENTAL-STATUS PIC XX.
       01  WS-EOF           PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       100-MAIN.
           PERFORM 200-OPEN-FILES
           
           PERFORM UNTIL WS-EOF = 'Y'
               READ RENTALS-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE SPACES TO MASTER-REC
                       MOVE ITEM-NO-IN OF RENTALS-REC TO ITEM-NO
                       MOVE VIDEO-NAME-IN OF RENTALS-REC TO VIDEO-NAME
                       MOVE RENTAL-OH-IN OF RENTALS-REC TO RENTAL-OH
               END-READ
               
               READ SALES-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE SALES-OH-IN OF SALES-REC TO SALES-OH
                       WRITE MASTER-REC
               END-READ
               
           END-PERFORM
           
           PERFORM 210-CLOSE-FILES
           
           STOP RUN
           .
           
       200-OPEN-FILES.
           OPEN INPUT  RENTALS-FILE
                INPUT  SALES-FILE
                OUTPUT MASTER-FILE
           .
           
       210-CLOSE-FILES.
           CLOSE RENTALS-FILE
                 SALES-FILE
                 MASTER-FILE
           .          
           
