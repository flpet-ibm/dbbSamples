       IDENTIFICATION DIVISION.                                         00000100
       PROGRAM-ID. FSPPROCI.                                            00000200
      ********************************************************@SCPYRT** 00000201
      *                                                               * 00000202
      *  Licensed Materials - Property of IBM                         * 00000203
      *                                                               * 00000204
      *  5635-A06                                                     * 00000205
      *                                                               * 00000206
      *      Copyright IBM Corp. 1991,1998 All Rights Reserved        * 00000207
      *                                                               * 00000208
      *  US Government Users Restricted Rights - Use, duplication or  * 00000209
      *  disclosure restricted by GSA ADP Schedule contract with      * 00000210
      *  IBM Corp.                                                      00000211
                                                                        00000212
      *                                                               * 00000213
      * This sample is a refactored version of the IMS sample         * 00000214
      * IVP application DFSIVA64. The purpose of this refactored      * 00000215
      * version is to demonstrate shift left testing with IDz's zUnit * 00000216
      * and application integration testing with IBM Z Virtual Test   * 00000217
      * Platform                                                        00000218
      *                                                                 00000219
      *     Flemming Skovgaard Petersen, flemming.petersen@dk.ibm.com   00000220
      *         Copyright IBM Corp, 2021                              * 00000221
      ********************************************************@ECPYRT** 00000222
       DATE-WRITTEN. 11/09/2021.                                        00000300
                                                                        00000400
       DATA DIVISION.                                                   00000500
       WORKING-STORAGE SECTION.                                         00000600
      * Generated program name variable                                 00000700
       77 FSPISRTO PIC X(8) VALUE 'FSPISRTO'.                           00000800
                                                                        00000900
                                                                        00001000
      * MESSAGES                                                        00001100
                                                                        00001200
       77  MDEL    PIC X(40) VALUE 'ENTRY WAS DELETED'.                 00001300
       77  MADD    PIC X(40) VALUE 'ENTRY WAS ADDED'.                   00001400
       77  MEND    PIC X(40) VALUE 'BMP/DLI PGM HAS ENDED'.             00001500
       77  MDIS    PIC X(40) VALUE 'ENTRY WAS DISPLAYED'.               00001600
       77  MUPD1   PIC X(40) VALUE 'ENTRY WAS UPDATED'.                 00001700
       77  MTEST   PIC X(40) VALUE 'TEST REQUEST WAS ENDED'.            00001800
       77  MMORE   PIC X(40) VALUE 'DATA IS NOT ENOUGH'.                00001900
       77  MINV    PIC X(40) VALUE 'PROCESS CODE IS NOT VALID'.         00002000
       77  MUPD0   PIC X(40) VALUE 'PLEASE UPDATE ENTRY'.               00002100
       77  MNODATA PIC X(40) VALUE 'NO DATA WAS ENTERED'.               00002200
       77  MNONAME PIC X(40) VALUE 'LAST NAME WAS NOT SPECIFIED'.       00002300
       77  MNOENT  PIC X(40) VALUE 'SPECIFIED PERSON WAS NOT FOUND'.    00002400
       77  MISRTE  PIC X(40) VALUE 'ADDITION OF ENTRY HAS FAILED'.      00002500
       77  MDLETE  PIC X(40) VALUE 'DELETION OF ENTRY HAS FAILED'.      00002600
       77  MREPLE  PIC X(40) VALUE 'UPDATE OF ENTRY HAS FAILED'.        00002700
                                                                        00002800
       77  GET-UNIQUE      PIC  X(4)  VALUE 'GU  '.                     00002900
       77  GET-HOLD-UNIQUE PIC  X(4)  VALUE 'GHU '.                     00003000
       77  GET-NEXT        PIC  X(4)  VALUE 'GN  '.                     00003100
       77  ISRT            PIC  X(4)  VALUE 'ISRT'.                     00003200
       77  DLET            PIC  X(4)  VALUE 'DLET'.                     00003300
       77  REPL            PIC  X(4)  VALUE 'REPL'.                     00003400
                                                                        00003500
      * COUNTERS                                                        00003600
                                                                        00003700
       01 COUNTERS.                                                     00003800
          02  L-SPACE-CTR    PIC   9(2) COMP VALUE 0.                   00003900
                                                                        00004000
       77  TEMP-ONE   PICTURE X(8) VALUE SPACES.                        00004100
       77  TEMP-TWO   PICTURE X(8) VALUE SPACES.                        00004200
       77  REPLY      PICTURE X(16).                                    00004300
                                                                        00004400
       77  SSA1            PIC X(9)  VALUE 'A1111111 '.                 00004500
                                                                        00004600
      * FLAGS                                                           00004700
                                                                        00004800
       01 FLAGS.                                                        00004900
          02  SET-DATA-FLAG  PIC X VALUE '0'.                           00005000
             88  NO-SET-DATA       VALUE '1'.                           00005100
          02  TADD-FLAG      PIC X VALUE '0'.                           00005200
             88  PROCESS-TADD      VALUE '1'.                           00005300
      * DATA AREA OUTPUT                                                00005400
                                                                        00005500
       01  OUTPUT-AREA.                                                 00005600
           02  OUT-BLANK  PIC  X(85) VALUE SPACES.                      00005700
           02  OUT-TEXT REDEFINES OUT-BLANK.                            00005800
               03  OUT-MESSAGE   PIC  X(40).                            00005900
               03  OUT-COMMAND   PIC  X(8).                             00006000
               03  OUT-DATA.                                            00006100
                   04  OUT-LAST-NAME   PIC  X(10).                      00006200
                   04  OUT-FIRST-NAME  PIC  X(10).                      00006300
                   04  OUT-EXTENSION   PIC  X(10).                      00006400
                   04  OUT-ZIP-CODE    PIC  X(7).                       00006500
           02  OUT-SEGMENT-NO    PIC  9(4).                             00006600
           02  OUT-FILL          PIC  X(32) VALUE SPACES.               00006700
                                                                        00006800
      * DC TEXT FOR ERROR CALL                                          00006900
                                                                        00007000
       01 DC-TEXT.                                                      00007100
          02  DC-TEXT1         PIC  X(7) VALUE 'STATUS '.               00007200
          02  DC-ERROR-STATUS  PIC  X(2).                               00007300
          02  DC-TEXT2         PIC  X(12) VALUE 'DLI  CALL = '.         00007400
          02  DC-ERROR-CALL    PIC  X(4).                               00007500
                                                                        00007600
      * SEGMENT SEARCH ARGUMENT                                         00007700
                                                                        00007800
       01 SSA.                                                          00007900
          02  SEGMENT-NAME  PIC X(8)  VALUE 'A1111111'.                 00008000
          02  SEG-KEY-NAME  PIC X(11) VALUE '(A1111111 ='.              00008100
          02  SSA-KEY       PIC X(10).                                  00008200
          02  FILLER        PIC X VALUE ')'.                            00008300
                                                                        00008400
       LINKAGE SECTION.                                                 00008500
                                                                        00008600
                                                                        00008700
       01  INPUT-AREA.                                                  00008800
           02  IN-BLANK  PIC  X(80).                                    00008900
           02  IN-TEXT REDEFINES IN-BLANK.                              00009000
               03  IN-COMMAND    PIC  X(8).                             00009100
               03  TEMP-COMMAND REDEFINES IN-COMMAND.                   00009200
                   04  TEMP-IOCMD PIC  X(3).                            00009300
                   04  FILLER     PIC  X(5).                            00009400
               03  IN-LAST-NAME  PIC  X(10).                            00009500
               03  IN-FIRST-NAME PIC  X(10).                            00009600
               03  IN-EXTENSION  PIC  X(10).                            00009700
               03  IN-ZIP-CODE   PIC  X(7).                             00009800
               03  INFILL        PIC  X(35).                            00009900
       01  IOAREA.                                                      00010000
           02  IO-BLANK  PIC  X(37).                                    00010100
           02  IO-DATA REDEFINES IO-BLANK.                              00010200
               03  IO-LAST-NAME   PIC  X(10).                           00010300
               03  IO-FIRST-NAME  PIC  X(10).                           00010400
               03  IO-EXTENSION   PIC  X(10).                           00010500
               03  IO-ZIP-CODE    PIC  X(7).                            00010600
           02  IO-FILLER    PIC  X(3).                                  00010700
           02  IO-COMMAND   PIC  X(8).                                  00010800
       01  DBPCB.                                                       00010900
           02  DBD-NAME        PIC  X(8).                               00011000
           02  SEG-LEVEL       PIC  X(2).                               00011100
           02  DBSTATUS        PIC  X(2).                               00011200
           02  PROC-OPTIONS    PIC  X(4).                               00011300
           02  RESERVE-DLI     PIC  X(4).                               00011400
           02  SEG-NAME-FB     PIC  X(8).                               00011500
           02  LENGTH-FB-KEY   PIC  9(4).                               00011600
           02  NUMB-SENS-SEGS  PIC  9(4).                               00011700
           02  KEY-FB-AREA     PIC  X(17).                              00011800
       01  GIPCB.                                                       00011900
           02  DBD-NAME        PIC  X(8).                               00012000
           02  SEG-LEVEL       PIC  X(2).                               00012100
           02  GI-STATUS       PIC  X(2).                               00012200
           02  PROC-OPTIONS    PIC  X(4).                               00012300
           02  RESERVE-DLI     PIC  X(4).                               00012400
           02  SEG-NAME-FB     PIC  X(8).                               00012500
           02  LENGTH-FB-KEY   PIC  9(4).                               00012600
           02  NUMB-SENS-SEGS  PIC  9(4).                               00012700
           02  KEY-FB-AREA     PIC  X(17).                              00012800
       01  GOPCB.                                                       00012900
           02  DBD-NAME        PIC  X(8).                               00013000
           02  SEG-LEVEL       PIC  X(2).                               00013100
           02  GO-STATUS       PIC  X(2).                               00013200
           02  PROC-OPTIONS    PIC  X(4).                               00013300
           02  RESERVE-DLI     PIC  x(4).                               00013400
           02  SEG-NAME-FB     PIC  X(8).                               00013500
           02  LENGTH-FB-KEY   PIC  9(4).                               00013600
           02  NUMB-SENS-SEGS  PIC  9(4).                               00013700
           02  KEY-FB-AREA     PIC  X(17).                              00013800
                                                                        00013900
                                                                        00014000
       PROCEDURE DIVISION USING INPUT-AREA, IOAREA,                     00014100
                                DBPCB, GIPCB, GOPCB.                    00014200
       MAIN SECTION.                                                    00014300
                                                                        00014400
           MOVE SPACES TO OUT-BLANK.                                    00014500
           MOVE SPACES TO IO-BLANK.                                     00014600
                                                                        00014700
      *    CHECK THE LEADING SPACE IN INPUT COMMAND AND TRIM IT OFF     00014800
                                                                        00014900
           INSPECT IN-COMMAND TALLYING L-SPACE-CTR FOR LEADING SPACE    00015000
             REPLACING LEADING SPACE BY '*'.                            00015100
           IF L-SPACE-CTR > 0                                           00015200
             UNSTRING IN-COMMAND DELIMITED BY ALL '*'                   00015300
                 INTO TEMP-ONE TEMP-TWO                                 00015400
             MOVE TEMP-TWO TO IN-COMMAND                                00015500
             MOVE 0 TO L-SPACE-CTR                                      00015600
             MOVE SPACES TO TEMP-TWO                                    00015700
           END-IF.                                                      00015800
                                                                        00015900
      *    CHECK THE LEADING SPACE IN INPUT LAST NAME AND TRIM IT OFF   00016000
                                                                        00016100
           INSPECT IN-LAST-NAME TALLYING L-SPACE-CTR FOR LEADING        00016200
             SPACE REPLACING LEADING SPACE BY '*'.                      00016300
           IF L-SPACE-CTR > 0                                           00016400
             UNSTRING IN-LAST-NAME DELIMITED BY ALL '*' INTO TEMP-ONE   00016500
               TEMP-TWO                                                 00016600
             MOVE TEMP-TWO TO IN-LAST-NAME                              00016700
             MOVE 0 TO L-SPACE-CTR                                      00016800
             MOVE SPACES TO TEMP-TWO.                                   00016900
                                                                        00017000
      *    CHECK THE LEADING SPACE IN INPUT FIRST NAME AND TRIM IT OFF  00017100
                                                                        00017200
           INSPECT IN-FIRST-NAME TALLYING L-SPACE-CTR FOR LEADING       00017300
             SPACE REPLACING LEADING SPACE BY '*'.                      00017400
           IF L-SPACE-CTR > 0                                           00017500
             UNSTRING IN-FIRST-NAME DELIMITED BY ALL '*' INTO TEMP-ONE  00017600
               TEMP-TWO                                                 00017700
             MOVE TEMP-TWO TO IN-FIRST-NAME                             00017800
             MOVE 0 TO L-SPACE-CTR                                      00017900
             MOVE SPACES TO TEMP-TWO.                                   00018000
                                                                        00018100
      *    CHECK THE LEADING SPACE IN INPUT EXTENSION AND TRIM IT OFF   00018200
                                                                        00018300
           INSPECT IN-EXTENSION TALLYING L-SPACE-CTR FOR LEADING        00018400
             SPACE REPLACING LEADING SPACE BY '*'.                      00018500
           IF L-SPACE-CTR > 0                                           00018600
             UNSTRING IN-EXTENSION DELIMITED BY ALL '*' INTO TEMP-ONE   00018700
               TEMP-TWO                                                 00018800
             MOVE TEMP-TWO TO IN-EXTENSION                              00018900
             MOVE 0 TO L-SPACE-CTR                                      00019000
             MOVE SPACES TO TEMP-TWO.                                   00019100
                                                                        00019200
      *    CHECK THE LEADING SPACE IN INPUT ZIP CODE AND TRIM IT OFF    00019300
                                                                        00019400
           INSPECT IN-ZIP-CODE TALLYING L-SPACE-CTR FOR LEADING SPACE   00019500
             REPLACING LEADING SPACE BY '*'.                            00019600
           IF L-SPACE-CTR > 0                                           00019700
             UNSTRING IN-ZIP-CODE DELIMITED BY ALL '*' INTO TEMP-ONE    00019800
               TEMP-TWO                                                 00019900
             MOVE TEMP-TWO TO IN-ZIP-CODE                               00020000
             MOVE 0 TO L-SPACE-CTR                                      00020100
             MOVE SPACES TO TEMP-TWO                                    00020200
           END-IF.                                                      00020300
      *                                                                 00020400
           MOVE IN-LAST-NAME TO IO-LAST-NAME.                           00020500
           MOVE IN-COMMAND TO IO-COMMAND.                               00020600
                                                                        00020700
           IF IO-COMMAND EQUAL SPACES                                   00020800
           THEN MOVE MINV TO OUT-MESSAGE                                00020900
                PERFORM PRINT-OUTPUT                                    00021000
           ELSE IF IO-LAST-NAME EQUAL SPACES THEN                       00021100
                MOVE MNONAME TO OUT-MESSAGE                             00021200
                PERFORM PRINT-OUTPUT                                    00021300
           ELSE IF TEMP-IOCMD EQUAL 'ADD' THEN                          00021400
                PERFORM TO-ADD                                          00021500
           ELSE IF TEMP-IOCMD EQUAL 'TAD' THEN                          00021600
                MOVE 1 TO TADD-FLAG                                     00021700
                PERFORM TO-ADD                                          00021800
           ELSE IF TEMP-IOCMD EQUAL 'UPD' THEN                          00021900
                PERFORM TO-UPD                                          00022000
           ELSE IF TEMP-IOCMD EQUAL 'DEL' THEN                          00022100
                PERFORM TO-DEL                                          00022200
           ELSE IF TEMP-IOCMD EQUAL 'DIS' THEN                          00022300
                PERFORM TO-DIS                                          00022400
           ELSE                                                         00022500
               MOVE IN-COMMAND TO OUT-COMMAND                           00022600
               MOVE IN-LAST-NAME TO OUT-LAST-NAME                       00022700
               MOVE MINV TO OUT-MESSAGE                                 00022800
               PERFORM PRINT-OUTPUT                                     00022900
           END-IF.                                                      00023000
           EXIT.                                                        00023100
                                                                        00023200
                                                                        00023300
           GOBACK.                                                      00023400
                                                                        00023500
       TO-ADD.                                                          00023600
           MOVE IN-FIRST-NAME TO IO-FIRST-NAME.                         00023700
      *    MOVE 'zUnit' TO IO-FIRST-NAME.
           MOVE IN-EXTENSION  TO IO-EXTENSION.                          00023800
           MOVE IN-ZIP-CODE   TO IO-ZIP-CODE.                           00023900
           MOVE IO-DATA       TO OUT-DATA.                              00024000
           MOVE IO-COMMAND    TO OUT-COMMAND.                           00024100
           IF IN-FIRST-NAME EQUAL SPACES OR                             00024200
              IN-EXTENSION EQUAL SPACES OR                              00024300
              IN-ZIP-CODE EQUAL SPACES                                  00024400
           THEN                                                         00024500
              MOVE MMORE TO OUT-MESSAGE                                 00024600
              PERFORM PRINT-OUTPUT                                      00024700
           ELSE                                                         00024800
              PERFORM ISRT-DB                                           00024900
           END-IF.                                                      00025000
           EXIT.                                                        00025100
                                                                        00025200
       TO-UPD.                                                          00025300
           MOVE 0 TO SET-DATA-FLAG.                                     00025400
           MOVE IN-COMMAND TO OUT-COMMAND.                              00025500
           MOVE IN-LAST-NAME TO OUT-LAST-NAME.                          00025600
           MOVE IO-LAST-NAME TO SSA-KEY.                                00025700
           PERFORM GET-HOLD-UNIQUE-DB.                                  00025800
           IF DBSTATUS = SPACES THEN                                    00025900
              IF IN-FIRST-NAME NOT = SPACES                             00026000
                 MOVE 1 TO SET-DATA-FLAG                                00026100
                 MOVE IN-FIRST-NAME TO IO-FIRST-NAME                    00026200
              END-IF                                                    00026300
              IF IN-EXTENSION  NOT = SPACES                             00026400
                 MOVE 1 TO SET-DATA-FLAG                                00026500
                 MOVE IN-EXTENSION  TO IO-EXTENSION                     00026600
              END-IF                                                    00026700
              IF IN-ZIP-CODE NOT = SPACES THEN                          00026800
                 MOVE 1 TO SET-DATA-FLAG                                00026900
                 MOVE IN-ZIP-CODE   TO IO-ZIP-CODE                      00027000
              END-IF                                                    00027100
           END-IF.                                                      00027200
                                                                        00027300
           MOVE IO-DATA    TO OUT-DATA.                                 00027400
           MOVE IO-COMMAND TO OUT-COMMAND                               00027500
           IF NO-SET-DATA THEN                                          00027600
              PERFORM REPL-DB                                           00027700
           ELSE                                                         00027800
              MOVE MNODATA TO OUT-MESSAGE                               00027900
              PERFORM PRINT-OUTPUT                                      00028000
           END-IF.                                                      00028100
                                                                        00028200
           EXIT.                                                        00028300
                                                                        00028400
       TO-DEL.                                                          00028500
           MOVE IO-LAST-NAME TO SSA-KEY.                                00028600
           PERFORM GET-HOLD-UNIQUE-DB.                                  00028700
           IF DBSTATUS = SPACES                                         00028800
           THEN                                                         00028900
              MOVE IO-DATA TO OUT-DATA                                  00029000
              MOVE IO-COMMAND TO OUT-COMMAND                            00029100
              PERFORM DLET-DB                                           00029200
           END-IF.                                                      00029300
           EXIT.                                                        00029400
                                                                        00029500
       TO-DIS.                                                          00029600
           MOVE IN-COMMAND TO OUT-COMMAND.                              00029700
           MOVE IN-LAST-NAME TO OUT-LAST-NAME.                          00029800
           MOVE IO-LAST-NAME TO SSA-KEY.                                00029900
           PERFORM GET-UNIQUE-DB.                                       00030000
           IF DBSTATUS = SPACES THEN                                    00030100
              MOVE IO-DATA TO OUT-DATA                                  00030200
              MOVE IO-COMMAND TO OUT-COMMAND                            00030300
             MOVE MDIS TO OUT-MESSAGE                                   00030400
             MOVE MEND TO OUT-MESSAGE                                   00030401
              PERFORM PRINT-OUTPUT                                      00030500
           END-IF.                                                      00030600
           EXIT.                                                        00030700
                                                                        00030800
       ISRT-DB.                                                         00030900
           MOVE ISRT TO DC-ERROR-CALL.                                  00031000
           CALL 'CBLTDLI' USING ISRT, DBPCB, IOAREA, SSA1               00031100
           IF DBSTATUS   = SPACES THEN                                  00031200
              IF PROCESS-TADD                                           00031300
                 DISPLAY 'INSERT IS DONE, REPLY' UPON CONSOLE           00031400
                 ACCEPT REPLY FROM CONSOLE                              00031500
                 MOVE 0 TO TADD-FLAG                                    00031600
              END-IF                                                    00031700
              MOVE MADD TO OUT-MESSAGE                                  00031800
              PERFORM PRINT-OUTPUT                                      00031900
           ELSE                                                         00032000
              MOVE MISRTE TO OUT-MESSAGE                                00032100
              MOVE DBSTATUS TO DC-ERROR-STATUS                          00032200
              PERFORM PRINT-OUTPUT                                      00032300
           END-IF.                                                      00032400
           EXIT.                                                        00032500
                                                                        00032600
       GET-UNIQUE-DB.                                                   00032700
           MOVE GET-UNIQUE TO DC-ERROR-CALL.                            00032800
           CALL 'CBLTDLI' USING GET-UNIQUE, DBPCB, IOAREA, SSA.         00032900
           IF DBSTATUS NOT = SPACES                                     00033000
           THEN                                                         00033100
              MOVE MNOENT TO OUT-MESSAGE                                00033200
              MOVE DBSTATUS TO DC-ERROR-STATUS                          00033300
              PERFORM PRINT-OUTPUT                                      00033400
           END-IF.                                                      00033500
           EXIT.                                                        00033600
                                                                        00033700
       GET-HOLD-UNIQUE-DB.                                              00033800
           MOVE GET-HOLD-UNIQUE TO DC-ERROR-CALL.                       00033900
           CALL 'CBLTDLI' USING GET-HOLD-UNIQUE, DBPCB, IOAREA, SSA.    00034000
           IF DBSTATUS NOT = SPACES THEN                                00034100
              MOVE MNOENT   TO OUT-MESSAGE                              00034200
              MOVE DBSTATUS TO DC-ERROR-STATUS                          00034300
              PERFORM PRINT-OUTPUT                                      00034400
           END-IF.                                                      00034500
           EXIT.                                                        00034600
                                                                        00034700
       REPL-DB.                                                         00034800
           MOVE REPL TO DC-ERROR-CALL.                                  00034900
           CALL 'CBLTDLI' USING REPL, DBPCB, IOAREA.                    00035000
           IF DBSTATUS = SPACES                                         00035100
           THEN                                                         00035200
              MOVE MUPD1 TO OUT-MESSAGE                                 00035300
              PERFORM PRINT-OUTPUT                                      00035400
           ELSE                                                         00035500
              MOVE MREPLE TO OUT-MESSAGE                                00035600
              MOVE DBSTATUS TO DC-ERROR-STATUS                          00035700
              PERFORM PRINT-OUTPUT                                      00035800
           END-IF.                                                      00035900
           EXIT.                                                        00036000
                                                                        00036100
       DLET-DB.                                                         00036200
           MOVE DLET TO DC-ERROR-CALL.                                  00036300
           CALL 'CBLTDLI' USING DLET, DBPCB, IOAREA.                    00036400
           IF DBSTATUS = SPACES                                         00036500
           THEN                                                         00036600
              MOVE MDEL TO OUT-MESSAGE                                  00036700
              PERFORM PRINT-OUTPUT                                      00036800
           ELSE                                                         00036900
              MOVE MDLETE TO OUT-MESSAGE                                00037000
              MOVE DBSTATUS TO DC-ERROR-STATUS                          00037100
              PERFORM PRINT-OUTPUT                                      00037200
           EXIT.                                                        00037300
                                                                        00037400
       PRINT-OUTPUT.                                                    00037500
           CALL FSPISRTO USING OUTPUT-AREA,                             00037600
                               GIPCB, GOPCB.                            00037700
           EXIT.                                                        00037800
                                                                        00037900
       END PROGRAM FSPPROCI.                                            00038000