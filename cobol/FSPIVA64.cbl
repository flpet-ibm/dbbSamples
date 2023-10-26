 CBL  APOST,NODYNAM                                                     00000100
       IDENTIFICATION DIVISION.                                         00000200
       PROGRAM-ID.  FSPIVA64                                            00000300
      *                                                                 00000400
      ********************************************************@SCPYRT** 00000500
      *                                                               * 00000600
      *  Licensed Materials - Property of IBM                         * 00000700
      *                                                               * 00000800
      *  5635-A06                                                     * 00000900
      *                                                               * 00001000
      *      Copyright IBM Corp. 1991,1998 All Rights Reserved        * 00001100
      *                                                               * 00001200
      *  US Government Users Restricted Rights - Use, duplication or  * 00001300
      *  disclosure restricted by GSA ADP Schedule contract with      * 00001400
      *  IBM Corp.                                                      00001500
                                                                        00001600
      *                                                               * 00001700
      * This sample is a refactored version of the IMS sample         * 00001800
      * IVP application DFSIVA64. The purpose of this refactored      * 00001900
      * version is to demonstrate shift left testing with IDz's zUnit * 00002000
      * and application integration testing with IBM Z Virtual Test   * 00002100
      * Platform                                                        00002200
      *                                                                 00002300
      *     Flemming Skovgaard Petersen, flemming.petersen@dk.ibm.com   00002400
      *         Copyright IBM Corp, 2021                              * 00002500
      ********************************************************@ECPYRT** 00002600
      *                                                                 00002700
      * APPLICATION  :  BMP DL/I PROGRAM                                00002800
      * TRANSACTION  :  NONE (BMP/DLI)                                  00002900
      * PSB          :  DFSIVP64                                        00003000
      * DATABASE     :  DFSIVD1                                         00003100
      * INPUT:                                                          00003200
      *                                                                 00003300
      *        TELEPHONE DIRECTORY SYSTEM                               00003400
      *        PROCESS CODE : CCCCCCCC                                  00003500
      *        LAST NAME    : XXXXXXXXXX                                00003600
      *        FIRST NAME   : XXXXXXXXXX                                00003700
      *        EXTENSION#   : N-NNN-NNNN                                00003800
      *        INTERNAL ZIP : XXX/XXX                                   00003900
      *                                                                 00004000
      * CCCCCCCC = COMMAND                                              00004100
      *        ADD     = INSERT ENTRY IN DB                             00004200
      *        DELETE  = DELETE ENTRY FROM DB                           00004300
      *        UPDATE  = UPDATE ENTRY FROM DB                           00004400
      *        DISPLAY = DISPLAY ENTRY                                  00004500
      *        TADD    = SAME AS ADD, BUT WRITE TO OPERATOR             00004600
      *                                                                 00004700
      *       CHANGES:  THIS MODULE IS NEW IN IMS/ESA 3.2               00004800
      *  APAR...  ID  PREREQ.  DATE....  DESCRIPTION................... 00004900
      *  KNQ0115  01           11/17/91  Add COBOL lang version         00005000
      *                                                                 00005100
       ENVIRONMENT DIVISION.                                            00005200
       CONFIGURATION SECTION.                                           00005300
       SOURCE-COMPUTER.  IBM-370.                                       00005400
       OBJECT-COMPUTER.  IBM-370.                                       00005500
      *                                                                 00005600
       DATA DIVISION.                                                   00005700
       WORKING-STORAGE SECTION.                                         00005800
      * Generated program name variable                                 00005900
       77 FSPPROCI PIC X(8) VALUE 'FSPPROCI'.                           00006000
       77 FSPPROCA PIC X(8) VALUE 'FSPPROCA'.
       77 WS-STATUS PIC X(40).
                                                                        00006100
                                                                        00006200
      * DL/I FUNCTION CODES                                             00006300
                                                                        00006400
       77  GET-NEXT        PIC  X(4)  VALUE 'GN  '.                     00006500
                                                                        00006600
      * DL/I CALL STATUS CODE                                           00006700
                                                                        00006800
       77  END-OF-DATABASE PIC  X(4)  VALUE 'GB'.                       00006900
                                                                        00007000
      * FLAGS                                                           00007100
                                                                        00007200
       01 FLAGS.                                                        00007300
          02  SET-DATA-FLAG  PIC X VALUE '0'.                           00007400
             88  NO-SET-DATA       VALUE '1'.                           00007500
          02  TADD-FLAG      PIC X VALUE '0'.                           00007600
             88  PROCESS-TADD      VALUE '1'.                           00007700
                                                                        00007800
                                                                        00007900
      * DATA AREA FOR TERMINAL INPUT                                    00008000
                                                                        00008100
       01  INPUT-AREA.                                                  00008200
           02  IN-BLANK  PIC  X(80) VALUE SPACES.                       00008300
           02  IN-TEXT REDEFINES IN-BLANK.                              00008400
               03  IN-COMMAND    PIC  X(8).                             00008500
               03  TEMP-COMMAND REDEFINES IN-COMMAND.                   00008600
                   04  TEMP-IOCMD PIC  X(3).                            00008700
                   04  FILLER     PIC  X(5).                            00008800
               03  IN-LAST-NAME  PIC  X(10).                            00008900
               03  IN-FIRST-NAME PIC  X(10).                            00009000
               03  IN-EXTENSION  PIC  X(10).                            00009100
               03  IN-ZIP-CODE   PIC  X(7).                             00009200
               03  INFILL        PIC  X(35).                            00009300
                                                                        00009400
                                                                        00009500
      * I/O AREA FOR DATACASE HANDLING                                  00009600
                                                                        00009700
       01  IOAREA.                                                      00009800
           02  IO-BLANK  PIC  X(37) VALUE SPACES.                       00009900
           02  IO-DATA REDEFINES IO-BLANK.                              00010000
               03  IO-LAST-NAME   PIC  X(10).                           00010100
               03  IO-FIRST-NAME  PIC  X(10).                           00010200
               03  IO-EXTENSION   PIC  X(10).                           00010300
               03  IO-ZIP-CODE    PIC  X(7).                            00010400
           02  IO-FILLER    PIC  X(3) VALUE SPACES.                     00010500
           02  IO-COMMAND   PIC  X(8) VALUE SPACES.                     00010600
                                                                        00010700
      * GSAM TEXT FOR ERROR CALL                                        00010800
                                                                        00010900
       01  GS-TEXT.                                                     00011000
           02  GS-TEXT1           PIC  X(7)   VALUE 'STATUS '.          00011100
           02  GS-ERROR-STATUS    PIC  X(2).                            00011200
           02  GS-TEXT2           PIC  X(12)  VALUE 'GSAM CALL = '.     00011300
           02  GS-ERROR-CALL      PIC  X(4).                            00011400
                                                                        00011500
                                                                        00011600
                                                                        00011700
       LINKAGE SECTION.                                                 00011800
                                                                        00011900
       01  IOPCB.                                                       00012000
           02  LTERM-NAME      PIC  X(8).                               00012100
           02  IO-RESERVE-IMS  PIC  X(2).                               00012200
           02  IO-STATUS       PIC  X(2).                               00012300
           02  CURR-DATE       PIC  X(4).                               00012400
           02  CURR-TIME       PIC  X(4).                               00012500
           02  IN-MSN          PIC  X(4).                               00012600
           02  MODNAME         PIC  X(8).                               00012700
           02  USERID          PIC  X(8).                               00012800
       01  DBPCB.                                                       00012900
           02  DBD-NAME        PIC  X(8).                               00013000
           02  SEG-LEVEL       PIC  X(2).                               00013100
           02  DBSTATUS        PIC  X(2).                               00013200
           02  PROC-OPTIONS    PIC  X(4).                               00013300
           02  RESERVE-DLI     PIC  X(4).                               00013400
           02  SEG-NAME-FB     PIC  X(8).                               00013500
           02  LENGTH-FB-KEY   PIC  9(4).                               00013600
           02  NUMB-SENS-SEGS  PIC  9(4).                               00013700
           02  KEY-FB-AREA     PIC  X(17).                              00013800
       01  GIPCB.                                                       00013900
           02  DBD-NAME        PIC  X(8).                               00014000
           02  SEG-LEVEL       PIC  X(2).                               00014100
           02  GI-STATUS       PIC  X(2).                               00014200
           02  PROC-OPTIONS    PIC  X(4).                               00014300
           02  RESERVE-DLI     PIC  X(4).                               00014400
           02  SEG-NAME-FB     PIC  X(8).                               00014500
           02  LENGTH-FB-KEY   PIC  9(4).                               00014600
           02  NUMB-SENS-SEGS  PIC  9(4).                               00014700
           02  KEY-FB-AREA     PIC  X(17).                              00014800
       01  GOPCB.                                                       00014900
           02  DBD-NAME        PIC  X(8).                               00015000
           02  SEG-LEVEL       PIC  X(2).                               00015100
           02  GO-STATUS       PIC  X(2).                               00015200
           02  PROC-OPTIONS    PIC  X(4).                               00015300
           02  RESERVE-DLI     PIC  x(4).                               00015400
           02  SEG-NAME-FB     PIC  X(8).                               00015500
           02  LENGTH-FB-KEY   PIC  9(4).                               00015600
           02  NUMB-SENS-SEGS  PIC  9(4).                               00015700
           02  KEY-FB-AREA     PIC  X(17).                              00015800
                                                                        00015900
       PROCEDURE DIVISION USING IOPCB, DBPCB, GIPCB, GOPCB.             00016000
                                                                        00016100
      * ON ENTRY IMS PASSES ADDRESSES FOR IOPCB, DBPCB, GIPCB AND GOPCB 00016200
                                                                        00016300
       MAIN-RTN.                                                        00016400
           MOVE 0 TO SET-DATA-FLAG.                                     00016500
           MOVE 0 TO TADD-FLAG.                                         00016600
           MOVE GET-NEXT TO GS-ERROR-CALL.                              00016700
           CALL 'CBLTDLI' USING GET-NEXT, GIPCB, INPUT-AREA.            00016800
                                                                        00016900
           PERFORM WITH TEST BEFORE UNTIL GI-STATUS = END-OF-DATABASE   00017000
              IF GI-STATUS EQUAL SPACES                                 00017100
                 PERFORM PROCESS-INPUT                                  00017200
              ELSE                                                      00017300
                 PERFORM GSAM-ERROR                                     00017400
              END-IF                                                    00017500
              MOVE GET-NEXT TO GS-ERROR-CALL                            00017600
              CALL 'CBLTDLI' USING GET-NEXT, GIPCB, INPUT-AREA          00017700
           END-PERFORM.                                                 00017800
           GOBACK.                                                      00017900
                                                                        00018000
      * PROCEDURE PROCESS-INPUT                                         00018100
                                                                        00018200
       PROCESS-INPUT.                                                   00018300
                                                                        00018400
           CALL FSPPROCI USING INPUT-AREA, IOAREA,
      *    CALL 'FSPPROCI' USING INPUT-AREA, IOAREA,
                    DBPCB, GIPCB, GOPCB WS-STATUS.
           DISPLAY 'FSPIVA64 status from FSPPROCI: ' ws-status.
      *    CALL FSPPROCA USING INPUT-AREA, IOAREA,                      00018500
      *               DBPCB, GIPCB, GOPCB WS-STATUS.
      *    DISPLAY 'FSPIVA64 status from FSPPROCA: ' ws-status.
                                                                        00018700
                                                                        00018800
       GSAM-ERROR.                                                      00018900
           MOVE GI-STATUS TO GS-ERROR-STATUS.                           00019000
           DISPLAY GS-TEXT1, GS-ERROR-STATUS, GS-TEXT2,                 00019100
                   GS-ERROR-CALL UPON CONSOLE                           00019200
           GOBACK.                                                      00019300
                                                                        00019400
                                                                        00019500
                                                                        00019600
                                                                        00019700