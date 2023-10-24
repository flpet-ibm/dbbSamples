       IDENTIFICATION DIVISION.
       PROGRAM-ID. IDISCBL1
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  FILLER                 PIC X(20)  VALUE 'WORKING-STORAGE'.
       01  NUMBERX PIC 999999 COMP-3.
       01  ERROR-FLD.
           05  ERROR-COUNT PIC 999999 COMP-3.
           05  FLDY REDEFINES ERROR-COUNT.
               07 FLDZ PIC XXXX.
       01  BAD-RESULT PIC 99 COMP-3 Value 42.

       01  XDUMP-TBL.
           05  XDUMPELEM OCCURS 1000 TIMES INDEXED BY XDUMP-IDX.
              10 XDUMPDATA   PIC X(80).

       PROCEDURE DIVISION.
       MAIN SECTION.
           DISPLAY '*** IDISCBL1 - START OF PROGRAM'.
           INITIALIZE XDUMP-TBL.
       LOOP SECTION.
       START000.
           MOVE 3 TO ERROR-COUNT.
           ADD 986885 TO ERROR-COUNT GIVING NUMBERX.
           MOVE 'ABCD' TO FLDZ.
           IF NUMBERX > 0 THEN PERFORM CLEAR.
           DISPLAY '*** IDISCBL1 - END OF PROGRAM'.
           GOBACK.
       CLEAR SECTION.
       START001.
           DIVIDE NUMBERX BY ERROR-COUNT GIVING BAD-RESULT.
           EXIT.
       END PROGRAM IDISCBL1.