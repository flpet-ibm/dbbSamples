       PROCESS NODLL,NODYNAM,TEST(NOSEP),NOCICS,NOSQL,PGMN(LU)
      *+---------------------------------------------------------------+
      *| TEBUD01                                                       |
      *| PRODUCT: IBM DEVELOPER FOR Z/OS                               |
      *| COMPONENT: IBM Z/OS AUTOMATED UNIT TESTING FRAMEWORK (ZUNIT)  |
      *|   FOR ENTERPRISE COBOL AND PL/I                               |
      *| PROGRAM: ENTERPRISE COBOL ZUNIT TEST CASE FOR DYNAMIC RUNNER  |
      *| TEST CASE VERSION: 101                                        |
      *| DATE GENERATED: 11/22/2023 14:35                              |
      *| ID: 10cd2390-f74a-47c2-bcda-5b59583ab429                      |
      *+---------------------------------------------------------------+
      *+---------------------------------------------------------------+
      *| ZUNIT TEST_TEST1                                              |
      *|     THIS PROGRAM IS FOR TEST TEST1                            |
      *| TEST CASE VERSION: 101                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TEST_TEST1'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'EBUD01'.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM TEST CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 BZUGETEP          PIC X(8) VALUE 'BZUGETEP'.
       01 AZ-EP-PTR         USAGE IS POINTER.
       01 AZ-TRACE-PTR      POINTER.
       01 ASSERT-ST.
         03 ASSERT-RC PIC 9(9) BINARY VALUE 4.
         03 ASSERT-TEXT PIC 9(4) BINARY VALUE 0.
       01 AZ-TEST-NAME-LEN       PIC S9(9) COMP-5.
       01 AZ-RC-WORK             PIC S9(4) USAGE BINARY.
       01 AZ-SUB-GETARG     PIC X(8)  VALUE 'BZUGTARG'.
       01 AZ-SUB-PROGRAM    PIC X(8)  VALUE 'EBUD01'.
       01 AZ-SUB-CSECT      PIC X(72) VALUE SPACES.
       01 AZ-SUB-ARG-LIST   USAGE POINTER.
       LOCAL-STORAGE SECTION.
      *  *** INTERFACE-AREA : ZUT00000013
       1 ZUT00000013.
      *    *** L-INPUT-DATE : ZUT00000014
         5 ZUT00000014.
      *    *** L-CCYY : ZUT00000015
         10 ZUT00000015 PIC X(4).
      *    *** L-MM : ZUT00000016
         10 ZUT00000016 PIC X(2).
      *    *** L-DD : ZUT00000017
         10 ZUT00000017 PIC X(2).
      *    *** DAYS-DIFF : ZUT00000018
         5 ZUT00000018 PIC 9(8) COMP.
      *    *** RETIREMENT-DATE : ZUT00000019
         5 ZUT00000019 PIC X(80).
      *    *** RETC : ZUT0000001A
         5 ZUT0000001A PIC S9(4) COMP.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-ARG-LIST.
         03 ARG-LENGTH PIC 9(4) COMP-4.
         03 ARG-DATA PIC X(256).
       01 AZ-PROC-PTR       USAGE IS PROCEDURE-POINTER.
       01  AZ-SUB-PGM-LIST.
         03  AZ-SUB-PGM-COUNT       PIC 9(4) COMP-4.
         03  AZ-SUB-PGM-ADDRS  OCCURS 1 TO 100 TIMES
                       DEPENDING ON AZ-SUB-PGM-COUNT.
           05  AZ-SUB-PGM-ADDR      USAGE POINTER.
           05  AZ-SUB-PGM-LGTH      PIC 9(8) COMP-4.
       01  AZ-LINKPARM1             PIC X(32768).
       01  AZ-LINKPARM2             PIC X(32768).
       PROCEDURE DIVISION USING AZ-TEST AZ-ARG-LIST.
      * START
           DISPLAY 'AZU0000I TEST_TEST1 STARTED...'
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * INITIALIZE PARAMETER
           PERFORM INITIALIZE-PARM
      * SET AREA ADDRESS TO POINTER
      * GET SUB PROGRAM ARGUMENT
           DISPLAY 'AZU0000I CALL BZUGTARG FOR EBUD01'
           CALL AZ-SUB-GETARG USING AZ-SUB-PROGRAM AZ-SUB-CSECT
             RETURNING AZ-SUB-ARG-LIST
           IF AZ-SUB-ARG-LIST = NULL
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             STRING 'SUB PROGRAM EBUD01 NOT FOUND.'
               DELIMITED BY SIZE
               INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           ELSE
              SET ADDRESS OF AZ-SUB-PGM-LIST TO AZ-SUB-ARG-LIST
              IF AZ-SUB-PGM-COUNT NOT EQUAL 1
                MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
                STRING 'SUB PROGRAM ARGUMENT COUNT DOES NOT MATCH.'
                  DELIMITED BY SIZE
                  INTO MESSAGE-TXT OF BZ-ASSERT
                  WITH POINTER MESSAGE-LEN OF BZ-ASSERT
                END-STRING
                SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
                PERFORM THROW-ASSERTION-M
              END-IF
              SET ADDRESS OF AZ-LINKPARM1 TO AZ-SUB-PGM-ADDR(1)
              SET ADDRESS OF AZ-LINKPARM2 TO ADDRESS OF ZUT00000013
              MOVE AZ-LINKPARM1(1:AZ-SUB-PGM-LGTH(1)) TO
                AZ-LINKPARM2(1:AZ-SUB-PGM-LGTH(1))
           END-IF.
      * SET INPUT VALUE
           MOVE 0 TO RETURN-CODE.
      * CALL TEST PROGRAM
           DISPLAY 'AZU0000I CALL EBUD01'
           CALL PROGRAM-NAME
           USING ZUT00000013
           .
      * EVALUATE OUTPUT VALUE
           MOVE 4 TO RETURN-CODE
      * END
           DISPLAY 'AZU0000I TEST_TEST1 END.'
           GOBACK.
       INITIALIZE-PARM.
           INITIALIZE ZUT00000013
           EXIT.
       THROW-ASSERTION-M.
           DISPLAY 'AZU0000I *******************************************
      -    '*************************************'
           DISPLAY 'AZU2001W THE TEST "' AZ-TEST(1:AZ-TEST-NAME-LEN) '"
      -    'FAILED DUE TO AN ASSERTION.'
           DISPLAY 'AZU1101I ' MESSAGE-TXT OF BZ-ASSERT(1:MESSAGE-LEN
           OF BZ-ASSERT)
           DISPLAY 'AZU0000I *******************************************
      -    '*************************************'
           CALL BZUASSRT USING BZ-P1 BZ-P2 BZ-P3 BZ-ASSERT
           EXIT.
       END PROGRAM TEST_TEST1.
      *+---------------------------------------------------------------+
      *| ZUNIT TEST_TEST2                                              |
      *|     THIS PROGRAM IS FOR TEST TEST2                            |
      *| TEST CASE VERSION: 101                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TEST_TEST2'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'EBUD01'.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM TEST CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 BZUGETEP          PIC X(8) VALUE 'BZUGETEP'.
       01 AZ-EP-PTR         USAGE IS POINTER.
       01 AZ-TRACE-PTR      POINTER.
       01 ASSERT-ST.
         03 ASSERT-RC PIC 9(9) BINARY VALUE 4.
         03 ASSERT-TEXT PIC 9(4) BINARY VALUE 0.
       01 AZ-TEST-NAME-LEN       PIC S9(9) COMP-5.
       01 AZ-RC-WORK             PIC S9(4) USAGE BINARY.
       01 AZ-SUB-GETARG     PIC X(8)  VALUE 'BZUGTARG'.
       01 AZ-SUB-PROGRAM    PIC X(8)  VALUE 'EBUD01'.
       01 AZ-SUB-CSECT      PIC X(72) VALUE SPACES.
       01 AZ-SUB-ARG-LIST   USAGE POINTER.
       LOCAL-STORAGE SECTION.
      *  *** INTERFACE-AREA : ZUT00000013
       1 ZUT00000013.
      *    *** L-INPUT-DATE : ZUT00000014
         5 ZUT00000014.
      *    *** L-CCYY : ZUT00000015
         10 ZUT00000015 PIC X(4).
      *    *** L-MM : ZUT00000016
         10 ZUT00000016 PIC X(2).
      *    *** L-DD : ZUT00000017
         10 ZUT00000017 PIC X(2).
      *    *** DAYS-DIFF : ZUT00000018
         5 ZUT00000018 PIC 9(8) COMP.
      *    *** RETIREMENT-DATE : ZUT00000019
         5 ZUT00000019 PIC X(80).
      *    *** RETC : ZUT0000001A
         5 ZUT0000001A PIC S9(4) COMP.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-ARG-LIST.
         03 ARG-LENGTH PIC 9(4) COMP-4.
         03 ARG-DATA PIC X(256).
       01 AZ-PROC-PTR       USAGE IS PROCEDURE-POINTER.
       01  AZ-SUB-PGM-LIST.
         03  AZ-SUB-PGM-COUNT       PIC 9(4) COMP-4.
         03  AZ-SUB-PGM-ADDRS  OCCURS 1 TO 100 TIMES
                       DEPENDING ON AZ-SUB-PGM-COUNT.
           05  AZ-SUB-PGM-ADDR      USAGE POINTER.
           05  AZ-SUB-PGM-LGTH      PIC 9(8) COMP-4.
       01  AZ-LINKPARM1             PIC X(32768).
       01  AZ-LINKPARM2             PIC X(32768).
       PROCEDURE DIVISION USING AZ-TEST AZ-ARG-LIST.
      * START
           DISPLAY 'AZU0000I TEST_TEST2 STARTED...'
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * INITIALIZE PARAMETER
           PERFORM INITIALIZE-PARM
      * SET AREA ADDRESS TO POINTER
      * GET SUB PROGRAM ARGUMENT
           DISPLAY 'AZU0000I CALL BZUGTARG FOR EBUD01'
           CALL AZ-SUB-GETARG USING AZ-SUB-PROGRAM AZ-SUB-CSECT
             RETURNING AZ-SUB-ARG-LIST
           IF AZ-SUB-ARG-LIST = NULL
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             STRING 'SUB PROGRAM EBUD01 NOT FOUND.'
               DELIMITED BY SIZE
               INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           ELSE
              SET ADDRESS OF AZ-SUB-PGM-LIST TO AZ-SUB-ARG-LIST
              IF AZ-SUB-PGM-COUNT NOT EQUAL 1
                MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
                STRING 'SUB PROGRAM ARGUMENT COUNT DOES NOT MATCH.'
                  DELIMITED BY SIZE
                  INTO MESSAGE-TXT OF BZ-ASSERT
                  WITH POINTER MESSAGE-LEN OF BZ-ASSERT
                END-STRING
                SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
                PERFORM THROW-ASSERTION-M
              END-IF
              SET ADDRESS OF AZ-LINKPARM1 TO AZ-SUB-PGM-ADDR(1)
              SET ADDRESS OF AZ-LINKPARM2 TO ADDRESS OF ZUT00000013
              MOVE AZ-LINKPARM1(1:AZ-SUB-PGM-LGTH(1)) TO
                AZ-LINKPARM2(1:AZ-SUB-PGM-LGTH(1))
           END-IF.
      * SET INPUT VALUE
           MOVE 0 TO RETURN-CODE.
      * CALL TEST PROGRAM
           DISPLAY 'AZU0000I CALL EBUD01'
           CALL PROGRAM-NAME
           USING ZUT00000013
           .
      * EVALUATE OUTPUT VALUE
           MOVE 4 TO RETURN-CODE
      * END
           DISPLAY 'AZU0000I TEST_TEST2 END.'
           GOBACK.
       INITIALIZE-PARM.
           INITIALIZE ZUT00000013
           EXIT.
       THROW-ASSERTION-M.
           DISPLAY 'AZU0000I *******************************************
      -    '*************************************'
           DISPLAY 'AZU2001W THE TEST "' AZ-TEST(1:AZ-TEST-NAME-LEN) '"
      -    'FAILED DUE TO AN ASSERTION.'
           DISPLAY 'AZU1101I ' MESSAGE-TXT OF BZ-ASSERT(1:MESSAGE-LEN
           OF BZ-ASSERT)
           DISPLAY 'AZU0000I *******************************************
      -    '*************************************'
           CALL BZUASSRT USING BZ-P1 BZ-P2 BZ-P3 BZ-ASSERT
           EXIT.
       END PROGRAM TEST_TEST2.
      *+---------------------------------------------------------------+
      *| ZUNIT BZU_TEST                                                |
      *|     THIS PROGRAM IS CALLBACK DEFINITION FOR TEST              |
      *| TEST CASE VERSION: 101                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_TEST'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'EBUD01'.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM TEST CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR      POINTER.
       01 ASSERT-ST.
         03 ASSERT-RC PIC 9(9) BINARY VALUE 4.
         03 ASSERT-TEXT PIC 9(4) BINARY VALUE 0.
       01 AZ-TEST-NAME-LEN       PIC S9(9) COMP-5.
       01 AZ-GRP-INDEX        PIC 9(8).
       01 AZ-FLAG-IN          PIC 9(1).
       01 AZ-RECORD-PTR       POINTER.
       01 AZ-RC-WORK          PIC S9(4) USAGE BINARY.
       01 AZ-OUTPUT-COUNT-STR PIC X(5).
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY BZUITERC.
       01 AZ-ARG-LIST.
         03 ARG-LENGTH PIC 9(4) COMP-4.
         03 ARG-DATA PIC X(256).
      *  *** INTERFACE-AREA : ZUT00000013
       1 ZUT00000013.
      *    *** L-INPUT-DATE : ZUT00000014
         5 ZUT00000014.
      *    *** L-CCYY : ZUT00000015
         10 ZUT00000015 PIC X(4).
      *    *** L-MM : ZUT00000016
         10 ZUT00000016 PIC X(2).
      *    *** L-DD : ZUT00000017
         10 ZUT00000017 PIC X(2).
      *    *** DAYS-DIFF : ZUT00000018
         5 ZUT00000018 PIC 9(8) COMP.
      *    *** RETIREMENT-DATE : ZUT00000019
         5 ZUT00000019 PIC X(80).
      *    *** RETC : ZUT0000001A
         5 ZUT0000001A PIC S9(4) COMP.
       01 AZ-RECORD-COUNT     PIC 9(5) COMP-5.
       PROCEDURE DIVISION.
      * SET INPUT VALUE
           ENTRY "PGM_INPT_EBUD01" USING AZ-TEST AZ-INFO-BLOCK
           ZUT00000013.
           DISPLAY 'AZU0000I PGM_INPT_EBUD01 INPUT VALUES...'.
           MOVE 0 TO RETURN-CODE.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR CHARACTERS
             BEFORE INITIAL SPACE.
           EVALUATE AZ-TEST(1:AZ-TEST-NAME-LEN)
           WHEN SPACE
             CONTINUE
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
      * EVALUATE OUTPUT VALUE
           ENTRY "PGM_OUTP_EBUD01" USING AZ-TEST AZ-INFO-BLOCK
           ZUT00000013.
           DISPLAY 'AZU0000I PGM_OUTP_EBUD01 CHECK VALUES...'.
           MOVE 4 TO RETURN-CODE.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR CHARACTERS
             BEFORE INITIAL SPACE.
           EVALUATE AZ-TEST(1:AZ-TEST-NAME-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM CHECK-REC-TEST1
             MOVE 4 TO RETURN-CODE
           WHEN 'TEST2'
             PERFORM CHECK-REC-TEST2
             MOVE 4 TO RETURN-CODE
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
       TEARDOWN.
           DISPLAY 'AZU0000I BZU_TEST END.'
           GOBACK.
       CHECK-REC-TEST1.
      * CHECK RECORD COUNT FOR TEST1
      * FOR EBUD02
           MOVE 1 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           IF AZ-RECORD-COUNT < 1 THEN
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             MOVE AZ-RECORD-COUNT TO AZ-OUTPUT-COUNT-STR
             STRING
               'EXPECTED MAX RECORD COUNT IS ''1''. '
               'BUT REAL RECORD COUNT IS ''' AZ-OUTPUT-COUNT-STR ''''
               ' IN EBUD02.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
      * FOR EBUD03
           MOVE 2 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           IF AZ-RECORD-COUNT < 1 THEN
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             MOVE AZ-RECORD-COUNT TO AZ-OUTPUT-COUNT-STR
             STRING
               'EXPECTED MAX RECORD COUNT IS ''1''. '
               'BUT REAL RECORD COUNT IS ''' AZ-OUTPUT-COUNT-STR ''''
               ' IN EBUD03.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
           EXIT.
       CHECK-REC-TEST2.
      * CHECK RECORD COUNT FOR TEST2
      * FOR EBUD02
           MOVE 1 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           IF AZ-RECORD-COUNT < 1 THEN
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             MOVE AZ-RECORD-COUNT TO AZ-OUTPUT-COUNT-STR
             STRING
               'EXPECTED MAX RECORD COUNT IS ''1''. '
               'BUT REAL RECORD COUNT IS ''' AZ-OUTPUT-COUNT-STR ''''
               ' IN EBUD02.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
      * FOR EBUD03
           MOVE 2 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           IF AZ-RECORD-COUNT < 1 THEN
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             MOVE AZ-RECORD-COUNT TO AZ-OUTPUT-COUNT-STR
             STRING
               'EXPECTED MAX RECORD COUNT IS ''1''. '
               'BUT REAL RECORD COUNT IS ''' AZ-OUTPUT-COUNT-STR ''''
               ' IN EBUD03.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
           EXIT.
       THROW-ASSERTION-M.
           DISPLAY 'AZU0000I *******************************************
      -    '*************************************'
           DISPLAY 'AZU2001W THE TEST "' AZ-TEST(1:AZ-TEST-NAME-LEN) '"
      -    'FAILED DUE TO AN ASSERTION.'
           DISPLAY 'AZU1101I ' MESSAGE-TXT OF BZ-ASSERT(1:MESSAGE-LEN
           OF BZ-ASSERT)
           DISPLAY 'AZU0000I *******************************************
      -    '*************************************'
           CALL BZUASSRT USING BZ-P1 BZ-P2 BZ-P3 BZ-ASSERT
           EXIT.
       END PROGRAM BZU_TEST.
      *+---------------------------------------------------------------+
      *| ZUNIT BZU_INIT                                                |
      *|     INITIAL PROCEDURE                                         |
      *| TEST CASE VERSION: 101                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_INIT'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 AZ-TEST-NAME-LEN      PIC S9(9) COMP-5.
       01 AZ-TESTCASE-ID        PIC X(36)
           VALUE '10cd2390-f74a-47c2-bcda-5b59583ab429'.
       LINKAGE SECTION.
       01 AZ-TEST               PIC X(80).
       01 AZ-TEST-ID            PIC X(80).
       PROCEDURE DIVISION USING AZ-TEST AZ-TEST-ID.
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           DISPLAY 'AZU0000I BZU_INIT: ' AZ-TEST(1:AZ-TEST-NAME-LEN)
           DISPLAY 'AZU0000I TEST CASE VERSION: 101'
           DISPLAY 'AZU0000I FOR TEST RUNNER: <no_value> (NOT FOR:CICSCO
      -    'UNT)'
           MOVE AZ-TESTCASE-ID TO AZ-TEST-ID
           GOBACK.
       END PROGRAM BZU_INIT.
      *+---------------------------------------------------------------+
      *| ZUNIT BZU_TERM                                                |
      *|     TERMINATION PROCEDURE                                     |
      *| TEST CASE VERSION: 101                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_TERM'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 AZ-TEST-NAME-LEN      PIC S9(9) COMP-5.
       LINKAGE SECTION.
       01 AZ-TEST               PIC X(80).
       01 AZ-INFO-BLOCK.
           COPY BZUITERC.
       PROCEDURE DIVISION USING AZ-TEST
                                AZ-INFO-BLOCK.
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           DISPLAY 'AZU0000I BZU_TERM: ' AZ-TEST(1:AZ-TEST-NAME-LEN)
           GOBACK.
       END PROGRAM BZU_TERM.
      *+---------------------------------------------------------------+
      *| ZUNIT PROGRAM FOR EBUD02                                      |
      *| TEST CASE VERSION: 101                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'PGM_EBUD02'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM STUB CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR      POINTER.
       01 AZ-TEST-LEN       PIC S9(8) COMP.
       01 AZ-RECORD.
         03 AZ-RECORD-COUNT-OT PIC 9(5) COMP-5 VALUE 0.
         03 AZ-RECORD-COUNT-IN PIC 9(5) COMP-5 VALUE 0.
       01 AZ-GRP-INDEX      PIC 9(8).
       01 AZ-FLAG-IN        PIC 9(1).
       01 AZ-RECORD-PTR     POINTER.
       01 AZ-RC-WORK        PIC S9(4) USAGE BINARY.
       01 AZ-COMPARE.
         03 AZ-COMPARE-ITEM-NAME-PTR  POINTER.
         03 AZ-COMPARE-ITEM-NAME-LEN  PIC S9(9) COMP-5.
         03 AZ-COMPARE-ITEM-VALUE-PTR POINTER.
         03 AZ-COMPARE-ITEM-VALUE-LEN PIC S9(9) COMP-5.
         03 AZ-COMPARE-ITEM-EXP-VALUE-PTR POINTER.
         03 AZ-COMPARE-ITEM-EXP-VALUE-LEN PIC S9(9) COMP-5.
         03 AZ-ITEM-NAME-S            PIC S9(8) COMP.
         03 AZ-ITEM-NAME-LEN          PIC S9(8) COMP.
       LOCAL-STORAGE SECTION.
       1 AZ-COMPARE-ITEM-NAMES.
         3 AZU00000000.
            5 PIC X(20) DISPLAY VALUE 'W-CCYY OF W-INPUT-DA'.
            5 PIC X(20) DISPLAY VALUE 'TE OF W-EBUD02-LINKA'.
            5 PIC X(7) DISPLAY VALUE 'GE-AREA'.
       1 AZ-COMPARE-WORK-ITEMS.
          3 AZU00000001 PIC 9(4) OCCURS 2.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY BZUITERC.
       01 AZ-WK-RECORD-COUNT PIC 9(5) COMP-5.
       01 AZ-COMPARE-ITEM-NAME      PIC X(1000).
       01 AZ-COMPARE-ITEM-VALUE     PIC X(254).
       01 AZ-COMPARE-ITEM-EXP-VALUE PIC X(254).
      *  *** W-EBUD02-LINKAGE-AREA : ZUT00000002
       1 ZUT00000002.
      *    *** W-INPUT-DATE : ZUT00000003
         5 ZUT00000003.
      *    *** W-CCYY : ZUT00000004
         10 ZUT00000004 PIC 9(4).
      *    *** W-MM : ZUT00000005
         10 ZUT00000005 PIC 9(2).
      *    *** W-DD : ZUT00000006
         10 ZUT00000006 PIC 9(2).
      *    *** W-DAY-DIFFERENCE : ZUT00000007
         5 ZUT00000007 PIC 9(9).
      *    *** W-EBUD02-PROGRAM-RETCODE : ZUT00000008
         5 ZUT00000008 PIC 9(4).
      *    *** W-EBUD02-REQUEST-SUCCESS : ZUT00000009
         88 ZUT00000009 VALUE 0.
      *
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
           ENTRY "PGM_INPT_EBUD02" USING
              AZ-TEST AZ-INFO-BLOCK
           ZUT00000002.
           DISPLAY 'AZU0000I PGM_INPT_EBUD02 CHECK VALUES...'.
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * SET AREA ADDRESS TO POINTER
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-RECORD-COUNT-OT
           MOVE 1 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-WK-RECORD-COUNT
           EVALUATE AZ-TEST(1:AZ-TEST-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM P-OUTPUT-TEST1
           WHEN 'TEST2'
             PERFORM P-OUTPUT-TEST2
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
      * SET INPUT VALUE
           ENTRY "PGM_OUTP_EBUD02" USING
              AZ-TEST AZ-INFO-BLOCK
           ZUT00000002.
           DISPLAY 'AZU0000I PGM_OUTP_EBUD02 INPUT VALUES...'.
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * SET AREA ADDRESS TO POINTER
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-RECORD-COUNT-IN
           MOVE 1 TO AZ-GRP-INDEX
           MOVE 1 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-WK-RECORD-COUNT
           EVALUATE AZ-TEST(1:AZ-TEST-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM P-INPUT-TEST1
           WHEN 'TEST2'
             PERFORM P-INPUT-TEST2
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
       TEARDOWN.
           DISPLAY 'AZU0000I PGM_EBUD02 END.'
           GOBACK.
       P-OUTPUT-TEST1.
           INITIALIZE AZ-COMPARE-WORK-ITEMS
           IF AZ-RECORD-COUNT-OT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       P-OUTPUT-TEST2.
           INITIALIZE AZ-COMPARE-WORK-ITEMS
           IF AZ-RECORD-COUNT-OT = 0 THEN
             CONTINUE
           ELSE IF AZ-RECORD-COUNT-OT = 1
             MOVE 0 TO RETURN-CODE
             IF (ZUT00000004 OF ZUT00000003 OF ZUT00000002 IS NUMERIC)
                 AND (ZUT00000004 OF ZUT00000003 OF ZUT00000002 =
           1978) THEN
               CONTINUE
             ELSE
               MOVE ZUT00000004 OF ZUT00000003 OF ZUT00000002 TO
           AZU00000001(1)
               MOVE 1978 TO AZU00000001(2)
               SET AZ-COMPARE-ITEM-NAME-PTR TO ADDRESS OF AZU00000000
               MOVE LENGTH OF AZU00000000 TO AZ-COMPARE-ITEM-NAME-LEN
               SET AZ-COMPARE-ITEM-VALUE-PTR TO ADDRESS OF
           AZU00000001(1)
               MOVE 4 TO AZ-COMPARE-ITEM-VALUE-LEN
               SET AZ-COMPARE-ITEM-EXP-VALUE-PTR TO ADDRESS OF
           AZU00000001(2)
               MOVE 4 TO AZ-COMPARE-ITEM-EXP-VALUE-LEN
               MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
               STRING
                 'COMPARE FAILED AT RECORD 1 '
                 'IN EBUD02.'
                 DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
                 WITH POINTER MESSAGE-LEN OF BZ-ASSERT
               END-STRING
               SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
               PERFORM THROW-ASSERTION
             END-IF
           ELSE
             CONTINUE
               END-IF
           END-IF.
           EXIT.
       P-INPUT-TEST1.
           IF AZ-RECORD-COUNT-IN = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       P-INPUT-TEST2.
           IF AZ-RECORD-COUNT-IN = 0 THEN
             CONTINUE
           ELSE IF AZ-RECORD-COUNT-IN = 1
             MOVE 1978 TO ZUT00000004 OF ZUT00000003 OF ZUT00000002
             MOVE 20415 TO ZUT00000007 OF ZUT00000002
           ELSE
             CONTINUE
           END-IF
           END-IF.
           EXIT.
       THROW-ASSERTION.
           SET ADDRESS OF AZ-COMPARE-ITEM-NAME TO
           AZ-COMPARE-ITEM-NAME-PTR.
           SET ADDRESS OF AZ-COMPARE-ITEM-VALUE TO
           AZ-COMPARE-ITEM-VALUE-PTR.
           SET ADDRESS OF AZ-COMPARE-ITEM-EXP-VALUE TO
           AZ-COMPARE-ITEM-EXP-VALUE-PTR.
      *    DISPLAY ERROR MESSAGE AND ENDS TEST
           DISPLAY 'AZU0000I *******************************************
      -    '*************************************'
           DISPLAY 'AZU2001W THE TEST "' AZ-TEST(1:AZ-TEST-LEN)
           '" FAILED DUE TO AN ASSERTION.'
           DISPLAY 'AZU1101I ' MESSAGE-TXT OF BZ-ASSERT(1:MESSAGE-LEN
           OF BZ-ASSERT)
           DISPLAY 'AZU0000I  DATA ITEM NAME : '
           AZ-COMPARE-ITEM-NAME(1:AZ-COMPARE-ITEM-NAME-LEN)
           DISPLAY 'AZU0000I   VALUE         : '
           AZ-COMPARE-ITEM-VALUE(1:AZ-COMPARE-ITEM-VALUE-LEN)
           DISPLAY 'AZU0000I   EXPECTED VALUE: '
           AZ-COMPARE-ITEM-EXP-VALUE(1:AZ-COMPARE-ITEM-EXP-VALUE-LEN)
           DISPLAY 'AZU0000I *******************************************
      -    '*************************************'
           CALL BZUASSRT USING BZ-P1 BZ-P2 BZ-P3 BZ-ASSERT
           MOVE 1 TO TRACE-LEN OF BZ-TRACE
           MOVE 1 TO AZ-ITEM-NAME-S
           PERFORM UNTIL AZ-ITEM-NAME-S > AZ-COMPARE-ITEM-NAME-LEN
             MOVE 206 TO AZ-ITEM-NAME-LEN
             IF AZ-ITEM-NAME-S + 206 > AZ-COMPARE-ITEM-NAME-LEN THEN
               MOVE AZ-COMPARE-ITEM-NAME-LEN TO AZ-ITEM-NAME-LEN
               SUBTRACT AZ-ITEM-NAME-S FROM AZ-ITEM-NAME-LEN
               ADD 1 TO AZ-ITEM-NAME-LEN
             END-IF
             STRING 'ITEM NAME='
             AZ-COMPARE-ITEM-NAME(AZ-ITEM-NAME-S:AZ-ITEM-NAME-LEN)
               DELIMITED BY SIZE INTO TRACE-TXT OF BZ-TRACE
               WITH POINTER TRACE-LEN OF BZ-TRACE
             END-STRING
             SUBTRACT 1 FROM TRACE-LEN OF BZ-TRACE
             SET AZ-TRACE-PTR TO ADDRESS OF TRACE-TXT OF BZ-TRACE
             CALL BZUTRACE USING BZ-TRACE
             MOVE 1 TO TRACE-LEN OF BZ-TRACE
             ADD AZ-ITEM-NAME-LEN TO AZ-ITEM-NAME-S
           END-PERFORM
           STRING 'VALUE   ='
           AZ-COMPARE-ITEM-VALUE(1:AZ-COMPARE-ITEM-VALUE-LEN)
               DELIMITED BY SIZE INTO TRACE-TXT OF BZ-TRACE
               WITH POINTER TRACE-LEN OF BZ-TRACE
             END-STRING
           SUBTRACT 1 FROM TRACE-LEN OF BZ-TRACE
           CALL BZUTRACE USING BZ-TRACE
           MOVE 1 TO TRACE-LEN OF BZ-TRACE
           STRING 'EXPECTED='
           AZ-COMPARE-ITEM-EXP-VALUE(1:AZ-COMPARE-ITEM-EXP-VALUE-LEN)
               DELIMITED BY SIZE INTO TRACE-TXT OF BZ-TRACE
               WITH POINTER TRACE-LEN OF BZ-TRACE
             END-STRING
           SUBTRACT 1 FROM TRACE-LEN OF BZ-TRACE
           CALL BZUTRACE USING BZ-TRACE
           EXIT.
       END PROGRAM 'PGM_EBUD02'.
      *+---------------------------------------------------------------+
      *| ZUNIT PROGRAM FOR EBUD03                                      |
      *| TEST CASE VERSION: 101                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'PGM_EBUD03'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM STUB CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR      POINTER.
       01 AZ-TEST-LEN       PIC S9(8) COMP.
       01 AZ-RECORD.
         03 AZ-RECORD-COUNT-OT PIC 9(5) COMP-5 VALUE 0.
         03 AZ-RECORD-COUNT-IN PIC 9(5) COMP-5 VALUE 0.
       01 AZ-GRP-INDEX      PIC 9(8).
       01 AZ-FLAG-IN        PIC 9(1).
       01 AZ-RECORD-PTR     POINTER.
       01 AZ-RC-WORK        PIC S9(4) USAGE BINARY.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY BZUITERC.
       01 AZ-WK-RECORD-COUNT PIC 9(5) COMP-5.
      *  *** W-EBUD03-LINKAGE-AREA : ZUT0000000A
       1 ZUT0000000A.
      *    *** W-RETIREMENT-DATE-IN : ZUT0000000B
         5 ZUT0000000B.
      *    *** W-RET-YYYY : ZUT0000000C
         10 ZUT0000000C PIC X(4).
      *    *** FILLLER : ZUT0000000D
         10 ZUT0000000D PIC X(1).
      *    *** W-RET-MM : ZUT0000000E
         10 ZUT0000000E PIC X(2).
      *    *** W-RET-DD : ZUT0000000F
         10 ZUT0000000F PIC X(2).
      *    *** W-RETIREMENT-DATE : ZUT00000010
         5 ZUT00000010 PIC X(80).
      *    *** W-EBUD03-PROGRAM-RETCODE : ZUT00000011
         5 ZUT00000011 PIC 9(4).
      *    *** W-EBUD03-REQUEST-SUCCESS : ZUT00000012
         88 ZUT00000012 VALUE 0.
      *
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
           ENTRY "PGM_INPT_EBUD03" USING
              AZ-TEST AZ-INFO-BLOCK
           ZUT0000000A.
           DISPLAY 'AZU0000I PGM_INPT_EBUD03 CHECK VALUES...'.
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * SET AREA ADDRESS TO POINTER
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-RECORD-COUNT-OT
           MOVE 2 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-WK-RECORD-COUNT
           EVALUATE AZ-TEST(1:AZ-TEST-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM P-OUTPUT-TEST1
           WHEN 'TEST2'
             PERFORM P-OUTPUT-TEST2
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
      * SET INPUT VALUE
           ENTRY "PGM_OUTP_EBUD03" USING
              AZ-TEST AZ-INFO-BLOCK
           ZUT0000000A.
           DISPLAY 'AZU0000I PGM_OUTP_EBUD03 INPUT VALUES...'.
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * SET AREA ADDRESS TO POINTER
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-RECORD-COUNT-IN
           MOVE 2 TO AZ-GRP-INDEX
           MOVE 1 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-WK-RECORD-COUNT
           EVALUATE AZ-TEST(1:AZ-TEST-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM P-INPUT-TEST1
           WHEN 'TEST2'
             PERFORM P-INPUT-TEST2
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
       TEARDOWN.
           DISPLAY 'AZU0000I PGM_EBUD03 END.'
           GOBACK.
       P-OUTPUT-TEST1.
           IF AZ-RECORD-COUNT-OT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       P-OUTPUT-TEST2.
           IF AZ-RECORD-COUNT-OT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       P-INPUT-TEST1.
           IF AZ-RECORD-COUNT-IN = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       P-INPUT-TEST2.
           IF AZ-RECORD-COUNT-IN = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       END PROGRAM 'PGM_EBUD03'.
      *+---------------------------------------------------------------+
      *| ZUNIT GTMEMRC                                                 |
      *|     GET DATA AREA FOR RECORD COUNT OF SUBSYSTEM GROUP         |
      *| TEST CASE VERSION: 101                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'GTMEMRC'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZUGTMEM            PIC X(8) VALUE 'BZUGTMEM'.
       01 DATA-SIZE           PIC 9(8) COMP-4.
       LINKAGE SECTION.
       01 AZ-TC-WORK-AREA        PIC X(256).
       01 AZ-GRP-INDEX        PIC 9(8).
       01 AZ-FLAG-IN          PIC 9(1).
       01 AZ-RECORD-PTR       POINTER.
       01 AZ-RECORD-PTR-VALUE
            REDEFINES AZ-RECORD-PTR  PIC S9(9) COMP-5.
       01 DATA-PTR            POINTER.
       01 DATA-PTR-VALUE
            REDEFINES DATA-PTR  PIC S9(9) COMP-5.
       01 DATA-AREA.
         03 RECORD-COUNT-IO OCCURS 2.
           05 RECORD-COUNT-OT PIC 9(5) COMP-5.
           05 RECORD-COUNT-IN PIC 9(5) COMP-5.
       01 WK-RECORD-COUNT     PIC 9(5) COMP-5.
       01 AZ-TEST             PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY BZUITERC.
       PROCEDURE DIVISION USING AZ-TC-WORK-AREA AZ-GRP-INDEX AZ-FLAG-IN
           AZ-RECORD-PTR.
       MAINPROC SECTION.
      * GET DATA AREA
           SET ADDRESS OF DATA-PTR TO ADDRESS OF AZ-TC-WORK-AREA.
           IF DATA-PTR-VALUE = 0 THEN
             COMPUTE DATA-SIZE = LENGTH OF WK-RECORD-COUNT * 2 * 2
             CALL BZUGTMEM USING DATA-SIZE RETURNING DATA-PTR
             SET ADDRESS OF DATA-AREA TO DATA-PTR
             DISPLAY 'AZU0000I AREA ALLOCATED FOR RECORD COUNT:'
           DATA-SIZE
           END-IF
           SET AZ-RECORD-PTR TO DATA-PTR
           COMPUTE AZ-RECORD-PTR-VALUE = AZ-RECORD-PTR-VALUE +
                 LENGTH OF WK-RECORD-COUNT * 2 * (AZ-GRP-INDEX - 1)
           IF AZ-FLAG-IN = 1 THEN
             ADD LENGTH OF WK-RECORD-COUNT TO AZ-RECORD-PTR-VALUE
           END-IF
           SET ADDRESS OF WK-RECORD-COUNT TO AZ-RECORD-PTR
           GOBACK.
       CB-ENTRY.
      * ENTRY FOR CALLBACK
           ENTRY "PGM_INPT_GTMEMRC" USING AZ-TEST AZ-INFO-BLOCK
             AZ-TC-WORK-AREA AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR.
           PERFORM MAINPROC.
           EXIT.
       END PROGRAM 'GTMEMRC'.
