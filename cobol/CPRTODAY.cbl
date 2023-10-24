       ID DIVISION.
       PROGRAM-ID. CPRTODAY.
      *    THIS IS A CALLED PROGRAM EXAMPLE FOR DEMONSTRATION
      *
      *
      *    (C) 2019 IBM FLEMMING PETERSEN
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-DATE        PIC 9(8) value 0.

       LINKAGE SECTION.
      *
       01  LS-TODAY        PIC 9(8).

       PROCEDURE DIVISION USING LS-TODAY.
      *
       MAIN SECTION.
       MAIN1.
           if ws-date = zeroes then
              ACCEPT WS-DATE FROM DATE YYYYMMDD
           else
              add 1 to ws-date
           end-if.

           display 'CPRTODAY was called.'
           MOVE WS-DATE TO LS-TODAY.
           display 'CPRTODAY returning today as : ' LS-TODAY
           GOBACK.

           EXIT PROGRAM.

       END PROGRAM CPRTODAY.