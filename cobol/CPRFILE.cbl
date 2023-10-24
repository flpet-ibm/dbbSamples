       ID DIVISION.
       PROGRAM-ID. CPRFILE.
      *
      *
      *    (C) 2019 IBM FLEMMING PETERSEN
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO FILEIN
               FILE STATUS IS FILEIN-STATUS
               ORGANIZATION IS SEQUENTIAL.

           SELECT AMOUNTIN ASSIGN TO AMOUNTIN
               FILE STATUS IS AMOUNTIN-STATUS
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD FILEIN RECORDING MODE F.
       01 IN-RECORD.
       COPY CPRRECOR.

       FD AMOUNTIN RECORDING MODE F.
       01 AMOUNT-RECORD.
          05 AMOUNT1  PIC ZZZ.ZZZ.ZZ9,99.
          05 FILLER   PIC X(66).

       WORKING-STORAGE SECTION.

       01 FI-MARKER         PIC X     VALUE '0'.
          88 FI-EOF                   VALUE '1'.
       01 FILEIN-STATUS  PIC 99.
       01 AMOUNTIN-STATUS  PIC 99.

      *
       01 WS-CPR            PIC X(10).
       01 WS-AGE            PIC S9(4) BINARY
                                      VALUE 0.
       01 WS-AGE-FORMAT     PIC ZZZ.ZZ9,999 DISPLAY.
       01 WS-DISP-AGE       PIC ZZZ.ZZ9,999 DISPLAY.
       01 WS-GENDER         PIC X.
       01 WS-AGE2           PIC S9(4) BINARY.
       01 WS-RC             PIC X     VALUE '0'.
       01 WS-MODULE         PIC X(8)  VALUE 'CPRCHECK'.

       01 WS-NUMBER         PIC 9(10)v99.
       01 ws-atenth         PIC ZZZ.ZZZ.ZZ9,99.

       PROCEDURE DIVISION.
      *
       MAIN SECTION.
       MAIN1.
           OPEN INPUT AMOUNTIN.
           IF AMOUNTIN-STATUS NOT = 0 THEN
              DISPLAY 'FILE STATUS AT OPEN AMOUNTIN' AMOUNTIN-STATUS
              MOVE 16 to RETURN-CODE
              STOP RUN
           END-IF.

           READ AMOUNTIN.

           DISPLAY 'AMOUNT READ FROM FILE ' AMOUNT1.
           MOVE AMOUNT1 TO WS-NUMBER.
           DIVIDE 10 INTO WS-NUMBER.
           MOVE WS-NUMBER TO WS-ATENTH.
           DISPLAY 'A TENTH OF IT IS      ' ws-atenth                   1

           OPEN INPUT FILEIN.
           IF FILEIN-STATUS NOT = 0 THEN
              DISPLAY 'FILE STATUS AT OPEN FILEIN' FILEIN-STATUS
              MOVE 16 to RETURN-CODE
              STOP RUN
           END-IF.

           READ FILEIN
                AT END SET FI-EOF TO TRUE
           END-READ.
           DISPLAY 'CPRFILE. Read from filein. Status: '  FILEIN-STATUS
           DISPLAY 'CPRFILE. Read from filein. EOF: '  FI-MARKER

           PERFORM TEST BEFORE until FI-EOF
              MOVE IN-FDATO TO WS-CPR(1:6)
              MOVE IN-CHECKDIGIT TO WS-CPR(7:4)
              DISPLAY 'CPRFILE. Read from file: ' WS-CPR
              CALL WS-MODULE USING  WS-CPR WS-AGE WS-GENDER
                                     WS-RC
      *
              PERFORM DISPLAY-RESULTS
              READ FILEIN
                   AT END SET FI-EOF TO TRUE
              END-READ
              DISPLAY 'CPRFILE. Read from filein. Status: '
                      FILEIN-STATUS
              DISPLAY 'CPRFILE. Read from filein. EOF: '
                      FI-MARKER
           END-PERFORM.

           CLOSE FILEIN .


           GOBACK.


       DISPLAY-RESULTS SECTION.
           MOVE WS-AGE TO WS-AGE-FORMAT.
           DISPLAY 'CALLED CPRCHECK WITH ' WS-CPR
                                   '. AGE=' WS-AGE-FORMAT
                                   '. RC=' WS-RC.
           COMPUTE WS-AGE2 = WS-AGE / 10.
           MOVE WS-AGE2 TO WS-AGE-FORMAT.
           DISPLAY '  AGE DIVIDED BY 10  ' WS-AGE-FORMAT.

           EXIT SECTION.

       END PROGRAM CPRFILE.