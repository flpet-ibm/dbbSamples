 CBL  APOST                                                             00010000
       IDENTIFICATION DIVISION.                                         00020000
       PROGRAM-ID.  DFSIVA64                                            00030000
      *                                                                 00040000
      ********************************************************@SCPYRT** 00050000
      *                                                               * 00060000
      *  Licensed Materials - Property of IBM                         * 00070000
      *                                                               * 00080000
      *  5635-A06                                                     * 00090000
      *                                                               * 00100000
      *      Copyright IBM Corp. 1991,1998 All Rights Reserved        * 00110000
      *                                                               * 00120000
      *  US Government Users Restricted Rights - Use, duplication or  * 00130000
      *  disclosure restricted by GSA ADP Schedule contract with      * 00140000
      *  IBM Corp.                                                    * 00150000
      *                                                               * 00160000
      ********************************************************@ECPYRT** 00170000
      *                                                                 00180000
      * APPLICATION  :  BMP DL/I PROGRAM                                00190000
      * TRANSACTION  :  NONE (BMP/DLI)                                  00200000
      * PSB          :  DFSIVP64                                        00210000
      * DATABASE     :  DFSIVD1                                         00220000
      * INPUT:                                                          00230000
      *                                                                 00240000
      *        TELEPHONE DIRECTORY SYSTEM                               00250000
      *        PROCESS CODE : CCCCCCCC                                  00260000
      *        LAST NAME    : XXXXXXXXXX                                00270000
      *        FIRST NAME   : XXXXXXXXXX                                00280000
      *        EXTENSION#   : N-NNN-NNNN                                00290000
      *        INTERNAL ZIP : XXX/XXX                                   00300000
      *                                                                 00310000
      * CCCCCCCC = COMMAND                                              00320000
      *        ADD     = INSERT ENTRY IN DB                             00330000
      *        DELETE  = DELETE ENTRY FROM DB                           00340000
      *        UPDATE  = UPDATE ENTRY FROM DB                           00350000
      *        DISPLAY = DISPLAY ENTRY                                  00360000
      *        TADD    = SAME AS ADD, BUT WRITE TO OPERATOR             00370000
      *                                                                 00380000
      *       CHANGES:  THIS MODULE IS NEW IN IMS/ESA 3.2               00390000
      *  APAR...  ID  PREREQ.  DATE....  DESCRIPTION................... 00400000
      *  KNQ0115  01           11/17/91  Add COBOL lang version         00410000
      *                                                                 00420000
       ENVIRONMENT DIVISION.                                            00430000
       CONFIGURATION SECTION.                                           00440000
       SOURCE-COMPUTER.  IBM-370.                                       00450000
       OBJECT-COMPUTER.  IBM-370.                                       00460000
      *                                                                 00470000
       DATA DIVISION.                                                   00480000
       WORKING-STORAGE SECTION.                                         00490000
                                                                        00500000
      * DL/I FUNCTION CODES

       01  i pic s9(8) binary.                                          00510000
                                                                        00520000
       77  GET-UNIQUE      PIC  X(4)  VALUE 'GU  '.                     00530000
       77  GET-HOLD-UNIQUE PIC  X(4)  VALUE 'GHU '.                     00540000
       77  GET-NEXT        PIC  X(4)  VALUE 'GN  '.                     00550000
       77  ISRT            PIC  X(4)  VALUE 'ISRT'.                     00560000
       77  DLET            PIC  X(4)  VALUE 'DLET'.                     00570000
       77  REPL            PIC  X(4)  VALUE 'REPL'.                     00580000
                                                                        00590000
      * DL/I CALL STATUS CODE                                           00600000
                                                                        00610000
       77  END-OF-DATABASE PIC  X(4)  VALUE 'GB'.                       00620000
                                                                        00630000
      * MESSAGES                                                        00640000
                                                                        00650000
       77  MDEL    PIC  X(40)                                           00660000
                   VALUE 'ENTRY WAS DELETED                       '.    00670000
       77  MADD    PIC  X(40)                                           00680000
                   VALUE 'ENTRY WAS ADDED                         '.    00690000
       77  MEND    PIC  X(40)                                           00700000
                   VALUE 'BMP/DLI PGM HAS ENDED                   '.    00710000
       77  MDIS    PIC  X(40)                                           00720000
                   VALUE 'ENTRY WAS DISPLAYED                     '.    00730000
       77  MUPD1   PIC  X(40)                                           00740000
                   VALUE 'ENTRY WAS UPDATED                       '.    00750000
       77  MTEST   PIC  X(40)                                           00760000
                   VALUE 'TEST REQUEST WAS ENDED                  '.    00770000
       77  MMORE   PIC  X(40)                                           00780000
                   VALUE 'DATA IS NOT ENOUGH                      '.    00790000
       77  MINV    PIC  X(40)                                           00800000
                   VALUE 'PROCESS CODE IS NOT VALID               '.    00810000
       77  MUPD0   PIC  X(40)                                           00820000
                   VALUE 'PLEASE UPDATE ENTRY                     '.    00830000
       77  MNODATA PIC  X(40)                                           00840000
                   VALUE 'NO DATA WAS ENTERED                     '.    00850000
       77  MNONAME PIC  X(40)                                           00860000
                   VALUE 'LAST NAME WAS NOT SPECIFIED             '.    00870000
       77  MNOENT  PIC  X(40)                                           00880000
                   VALUE 'SPECIFIED PERSON WAS NOT FOUND          '.    00890000
       77  MISRTE  PIC  X(40)                                           00900000
                   VALUE 'ADDITION OF ENTRY HAS FAILED            '.    00910000
       77  MDLETE  PIC  X(40)                                           00920000
                   VALUE 'DELETION OF ENTRY HAS FAILED            '.    00930000
       77  MREPLE  PIC  X(40)                                           00940000
                   VALUE 'UPDATE OF ENTRY HAS FAILED              '.    00950000
                                                                        00960000
      * VARIABLES                                                       00970000
                                                                        00980000
       77  TEMP-ONE   PICTURE X(8) VALUE SPACES.                        00990000
       77  TEMP-TWO   PICTURE X(8) VALUE SPACES.                        01000000
       77  REPLY      PICTURE X(16).                                    01010000
                                                                        01020000
      * CONSTANTS                                                       01030000
                                                                        01040000
       77  HEADER-BLOCK    PIC X(50)                                    01050000
           VALUE '**************************************************'.  01060000
       77  HEADER-NAME     PIC X(50)                                    01070000
           VALUE '*  IMS INSTALLATION VERIFICATION PROCEDURE       *'.  01080000
       77  CONSTANT1       PIC X(24)                                    01090000
           VALUE   'PROCESS  CODE  (*1) :   '.                          01100000
       77  CONSTANT2       PIC X(24)                                    01110000
           VALUE   'LAST  NAME          :   '.                          01120000
       77  CONSTANT3       PIC X(24)                                    01130000
           VALUE   'FIRST NAME          :   '.                          01140000
       77  CONSTANT4       PIC X(24)                                    01150000
           VALUE   'EXTENSION  NUMBER   :   '.                          01160000
       77  CONSTANT5       PIC X(24)                                    01170000
           VALUE   'INTERNAL  ZIP CODE  :   '.                          01180000
       77  CONSTANT6       PIC X(17)                                    01190000
           VALUE   '(*1) PROCESS CODE'.                                 01200000
       77  CONSTANT7       PIC X(7)                                     01210000
           VALUE   'ADD    '.                                           01220000
       77  CONSTANT8       PIC X(7)                                     01230000
           VALUE   'DELETE '.                                           01240000
       77  CONSTANT9       PIC X(7)                                     01250000
           VALUE   'UPDATE '.                                           01260000
       77  CONSTANT10      PIC X(7)                                     01270000
           VALUE   'DISPLAY'.                                           01280000
       77  CONSTANT11      PIC X(7)                                     01290000
           VALUE   'TADD   '.                                           01300000
       77  SSA1            PIC X(9)  VALUE 'A1111111 '.                 01310000
                                                                        01320000
                                                                        01330000
      * FLAGS                                                           01340000
                                                                        01350000
       01 FLAGS.                                                        01360000
          02  SET-DATA-FLAG  PIC X VALUE '0'.                           01370000
             88  NO-SET-DATA       VALUE '1'.                           01380000
          02  TADD-FLAG      PIC X VALUE '0'.                           01390000
             88  PROCESS-TADD      VALUE '1'.                           01400000
                                                                        01410000
      * COUNTERS                                                        01420000
                                                                        01430000
       01 COUNTERS.                                                     01440000
          02  L-SPACE-CTR    PIC   9(2) COMP VALUE 0.                   01450000
                                                                        01460000
      * OUTLINE FORMAT                                                  01470000
                                                                        01480000
       01  BLANKLINE.                                                   01490000
           02  ANSI     PIC  X.                                         01500000
           02  HFILLER  PIC  X(24)  VALUE SPACES.                       01510000
           02  LEDGE    PIC  X(1)   VALUE '|'.                          01520000
           02  FILLER   PIC  X(80)  VALUE SPACES.                       01530000
           02  REDGE    PIC  X(1)   VALUE '|'.                          01540000
           02  FILLER   PIC  X(14)  VALUE SPACES.                       01550000
       01  OUTLINE1.                                                    01560000
           02  O-ANSI   PIC  X.                                         01570000
           02  HFILLER  PIC  X(24)  VALUE SPACES.                       01580000
           02  OUTLN1A  PIC  X(40)                                      01590000
               VALUE '*---------------------------------------'.        01600000
           02  OUTLN1B  PIC  X(42)                                      01610000
               VALUE '-----------------------------------------*'.      01620000
           02  FILLER   PIC  X(14)  VALUE SPACES.                       01630000
       01  OUTLINE2.                                                    01640000
           02  ANSI     PIC  X.                                         01650000
           02  HFILLER  PIC  X(24)  VALUE SPACES.                       01660000
           02  LEDGE    PIC  X(1)   VALUE '|'.                          01670000
           02  FILLER   PIC  X(15)  VALUE SPACES.                       01680000
           02  HDRLN    PIC  X(50)  VALUE SPACES.                       01690000
           02  FILLER   PIC  X(15)  VALUE SPACES.                       01700000
           02  REDGE    PIC  X(1)   VALUE '|'.                          01710000
           02  FILLER   PIC  X(14)  VALUE SPACES.                       01720000
       01  OUTLINE3.                                                    01730000
           02  ANSI     PIC  X.                                         01740000
           02  HFILLER  PIC  X(24)  VALUE SPACES.                       01750000
           02  LEDGE    PIC  X(1)   VALUE '|'.                          01760000
           02  FILLER   PIC  X(40)  VALUE SPACES.                       01770000
           02  D1LN     PIC  X(27)                                      01780000
               VALUE 'TRANSACTION TYPE : BMP/DLI '.                     01790000
           02  D2LN     PIC  X(10)  VALUE '(HIDAM DB)'.                 01800000
           02  FILLER   PIC  X(3)   VALUE SPACES.                       01810000
           02  REDGE    PIC  X(1)   VALUE '|'.                          01820000
           02  FILLER   PIC  X(14)  VALUE SPACES.                       01830000
       01  OUTLINE4.                                                    01840000
           02  ANSI     PIC  X.                                         01850000
           02  HFILLER  PIC  X(24)  VALUE SPACES.                       01860000
           02  LEDGE    PIC  X(1)   VALUE '|'.                          01870000
           02  FILLER   PIC  X(40)  VALUE SPACES.                       01880000
           02  D1CON    PIC  X(19)  VALUE 'DATE      :'.                01890000
           02  D1VAR    PIC  X(8)   VALUE '  /  /  '.                   01900000
           02  TEMP-DATE REDEFINES D1VAR.                               01910000
               04  MM          PIC  X(2).                               01920000
               04  DATE-FILL1  PIC  X.                                  01930000
               04  DD          PIC  X(2).                               01940000
               04  DATE-FILL2  PIC  X.                                  01950000
               04  YY          PIC  X(2).                               01960000
           02  FILLER   PIC  X(13)  VALUE SPACES.                       01970000
           02  REDGE    PIC  X(1)   VALUE '|'.                          01980000
           02  FILLER   PIC  X(14)  VALUE SPACES.                       01990000
       01  OUTLINE5.                                                    02000000
           02  ANSI     PIC  X.                                         02010000
           02  HFILLER  PIC  X(24)  VALUE SPACES.                       02020000
           02  LEDGE    PIC  X(1)   VALUE '|'.                          02030000
           02  DFILL2   PIC  X(10)  VALUE SPACES.                       02040000
           02  D2CON1   PIC  X(24).                                     02050000
           02  D2VAR    PIC  X(10).                                     02060000
           02  DFILL2A  PIC  X(23)  VALUE SPACES.                       02070000
           02  D2CON2   PIC  X(7)   VALUE SPACES.                       02080000
           02  FILLER   PIC  X(6)   VALUE SPACES.                       02090000
           02  REDGE    PIC  X(1)   VALUE '|'.                          02100000
           02  FILLER   PIC  X(14)  VALUE SPACES.                       02110000
       01  OUTLINE6.                                                    02120000
           02  ANSI     PIC  X.                                         02130000
           02  HFILLER  PIC  X(24)  VALUE SPACES.                       02140000
           02  LEDGE    PIC  X(1)   VALUE '|'.                          02150000
           02  DFILL3   PIC  X(59)  VALUE SPACES.                       02160000
           02  D3CON    PIC  X(17).                                     02170000
           02  FILLER   PIC  X(4)   VALUE SPACES.                       02180000
           02  REDGE    PIC  X(1)   VALUE '|'.                          02190000
           02  FILLER   PIC  X(14)  VALUE SPACES.                       02200000
       01  OUTLINE7.                                                    02210000
           02  ANSI     PIC  X.                                         02220000
           02  HFILLER  PIC  X(24)  VALUE SPACES.                       02230000
           02  LEDGE    PIC  X(1)   VALUE '|'.                          02240000
           02  DFILL4   PIC  X(10)  VALUE SPACES.                       02250000
           02  D4VAR1   PIC  X(40).                                     02260000
           02  DFILL4A  PIC  X(10)  VALUE SPACES.                       02270000
           02  D4CON    PIC  X(12)  VALUE 'SEGMENT# :  '.               02280000
           02  D4VAR2   PIC  X(4).                                      02290000
           02  FILLER   PIC  X(4)   VALUE SPACES.                       02300000
           02  REDGE    PIC  X(1)   VALUE '|'.                          02310000
           02  FILLER   PIC  X(14)  VALUE SPACES.                       02320000
                                                                        02330000
      * DATA AREA FOR TERMINAL INPUT                                    02340000
                                                                        02350000
       01  INPUT-AREA.                                                  02360000
           02  IN-BLANK  PIC  X(80) VALUE SPACES.                       02370000
           02  IN-TEXT REDEFINES IN-BLANK.                              02380000
               03  IN-COMMAND    PIC  X(8).                             02390000
               03  TEMP-COMMAND REDEFINES IN-COMMAND.                   02400000
                   04  TEMP-IOCMD PIC  X(3).                            02410000
                   04  FILLER     PIC  X(5).                            02420000
               03  IN-LAST-NAME  PIC  X(10).                            02430000
               03  IN-FIRST-NAME PIC  X(10).                            02440000
               03  IN-EXTENSION  PIC  X(10).                            02450000
               03  IN-ZIP-CODE   PIC  X(7).                             02460000
               03  INFILL        PIC  X(35).                            02470000
                                                                        02480000
      * DATA AREA OUTPUT                                                02490000
                                                                        02500000
       01  OUTPUT-AREA.                                                 02510000
           02  OUT-BLANK  PIC  X(85) VALUE SPACES.                      02520000
           02  OUT-TEXT REDEFINES OUT-BLANK.                            02530000
               03  OUT-MESSAGE   PIC  X(40).                            02540000
               03  OUT-COMMAND   PIC  X(8).                             02550000
               03  OUT-DATA.                                            02560000
                   04  OUT-LAST-NAME   PIC  X(10).                      02570000
                   04  OUT-FIRST-NAME  PIC  X(10).                      02580000
                   04  OUT-EXTENSION   PIC  X(10).                      02590000
                   04  OUT-ZIP-CODE    PIC  X(7).                       02600000
           02  OUT-SEGMENT-NO    PIC  9(4).                             02610000
           02  OUT-FILL          PIC  X(32) VALUE SPACES.               02620000
                                                                        02630000
      * I/O AREA FOR DATACASE HANDLING                                  02640000
                                                                        02650000
       01  IOAREA.                                                      02660000
           02  IO-BLANK  PIC  X(37) VALUE SPACES.                       02670000
           02  IO-DATA REDEFINES IO-BLANK.                              02680000
               03  IO-LAST-NAME   PIC  X(10).                           02690000
               03  IO-FIRST-NAME  PIC  X(10).                           02700000
               03  IO-EXTENSION   PIC  X(10).                           02710000
               03  IO-ZIP-CODE    PIC  X(7).                            02720000
           02  IO-FILLER    PIC  X(3) VALUE SPACES.                     02730000
           02  IO-COMMAND   PIC  X(8) VALUE SPACES.                     02740000
                                                                        02750000
      * GSAM TEXT FOR ERROR CALL                                        02760000
                                                                        02770000
       01  GS-TEXT.                                                     02780000
           02  GS-TEXT1           PIC  X(7)   VALUE 'STATUS '.          02790000
           02  GS-ERROR-STATUS    PIC  X(2).                            02800000
           02  GS-TEXT2           PIC  X(12)  VALUE 'GSAM CALL = '.     02810000
           02  GS-ERROR-CALL      PIC  X(4).                            02820000
                                                                        02830000
      * DC TEXT FOR ERROR CALL                                          02840000
                                                                        02850000
       01 DC-TEXT.                                                      02860000
          02  DC-TEXT1         PIC  X(7) VALUE 'STATUS '.               02870000
          02  DC-ERROR-STATUS  PIC  X(2).                               02880000
          02  DC-TEXT2         PIC  X(12) VALUE 'DLI  CALL = '.         02890000
          02  DC-ERROR-CALL    PIC  X(4).                               02900000
                                                                        02910000
       01  TEMPDATE.                                                    02920000
           02  TYY      PIC  99.                                        02930000
           02  TMM      PIC  99.                                        02940000
           02  TDD      PIC  99.                                        02950000
                                                                        02960000
      * SEGMENT SEARCH ARGUMENT                                         02970000
                                                                        02980000
       01 SSA.                                                          02990000
          02  SEGMENT-NAME  PIC X(8)  VALUE 'A1111111'.                 03000000
          02  SEG-KEY-NAME  PIC X(11) VALUE '(A1111111 ='.              03010000
          02  SSA-KEY       PIC X(10).                                  03020000
          02  FILLER        PIC X VALUE ')'.                            03030000
                                                                        03040000
       LINKAGE SECTION.                                                 03050000
                                                                        03060000
       01  IOPCB.                                                       03070000
           02  LTERM-NAME      PIC  X(8).                               03080000
           02  IO-RESERVE-IMS  PIC  X(2).                               03090000
           02  IO-STATUS       PIC  X(2).                               03100000
           02  CURR-DATE       PIC  X(4).                               03110000
           02  CURR-TIME       PIC  X(4).                               03120000
           02  IN-MSN          PIC  X(4).                               03130000
           02  MODNAME         PIC  X(8).                               03140000
           02  USERID          PIC  X(8).                               03150000
       01  DBPCB.                                                       03160000
           02  DBD-NAME        PIC  X(8).                               03170000
           02  SEG-LEVEL       PIC  X(2).                               03180000
           02  DBSTATUS        PIC  X(2).                               03190000
           02  PROC-OPTIONS    PIC  X(4).                               03200000
           02  RESERVE-DLI     PIC  X(4).                               03210000
           02  SEG-NAME-FB     PIC  X(8).                               03220000
           02  LENGTH-FB-KEY   PIC  9(4).                               03230000
           02  NUMB-SENS-SEGS  PIC  9(4).                               03240000
           02  KEY-FB-AREA     PIC  X(17).                              03250000
       01  GIPCB.                                                       03260000
           02  DBD-NAME        PIC  X(8).                               03270000
           02  SEG-LEVEL       PIC  X(2).                               03280000
           02  GI-STATUS       PIC  X(2).                               03290000
           02  PROC-OPTIONS    PIC  X(4).                               03300000
           02  RESERVE-DLI     PIC  X(4).                               03310000
           02  SEG-NAME-FB     PIC  X(8).                               03320000
           02  LENGTH-FB-KEY   PIC  9(4).                               03330000
           02  NUMB-SENS-SEGS  PIC  9(4).                               03340000
           02  KEY-FB-AREA     PIC  X(17).                              03350000
       01  GOPCB.                                                       03360000
           02  DBD-NAME        PIC  X(8).                               03370000
           02  SEG-LEVEL       PIC  X(2).                               03380000
           02  GO-STATUS       PIC  X(2).                               03390000
           02  PROC-OPTIONS    PIC  X(4).                               03400000
           02  RESERVE-DLI     PIC  x(4).                               03410000
           02  SEG-NAME-FB     PIC  X(8).                               03420000
           02  LENGTH-FB-KEY   PIC  9(4).                               03430000
           02  NUMB-SENS-SEGS  PIC  9(4).                               03440000
           02  KEY-FB-AREA     PIC  X(17).                              03450000
                                                                        03460000
       PROCEDURE DIVISION USING IOPCB, DBPCB, GIPCB, GOPCB.             03470000
                                                                        03480000
      * ON ENTRY IMS PASSES ADDRESSES FOR IOPCB, DBPCB, GIPCB AND GOPCB 03490000
                                                                        03500000
       MAIN-RTN.                                                        03510000
           MOVE 0 TO SET-DATA-FLAG.                                     03520000
           MOVE 0 TO TADD-FLAG.                                         03530000
           MOVE GET-NEXT TO GS-ERROR-CALL.                              03540000
           CALL 'CBLTDLI' USING GET-NEXT, GIPCB, INPUT-AREA.

           compute i = 40 / i.
                                                                        03550000
       READ-INPUT.                                                      03560000
           IF GI-STATUS = END-OF-DATABASE GOBACK.                       03570000
           IF GI-STATUS NOT EQUAL SPACES                                03580000
              PERFORM GSAM-ERROR                                        03590000
           ELSE                                                         03600000
              PERFORM PROCESS-INPUT THRU PROCESS-INPUT-END.             03610000
           MOVE GET-NEXT TO GS-ERROR-CALL.                              03620000
           CALL 'CBLTDLI' USING GET-NEXT, GIPCB, INPUT-AREA.            03630000
           GO TO READ-INPUT.                                            03640000
                                                                        03650000
      * PROCEDURE PROCESS-INPUT                                         03660000
                                                                        03670000
       PROCESS-INPUT.                                                   03680000
                                                                        03690000
           MOVE SPACES TO OUT-BLANK.                                    03700000
           MOVE SPACES TO IO-BLANK.                                     03710000
                                                                        03720000
      *    CHECK THE LEADING SPACE IN INPUT COMMAND AND TRIM IT OFF     03730000
                                                                        03740000
           INSPECT IN-COMMAND TALLYING L-SPACE-CTR FOR LEADING SPACE    03750000
             REPLACING LEADING SPACE BY '*'.                            03760000
           IF L-SPACE-CTR > 0                                           03770000
             UNSTRING IN-COMMAND DELIMITED BY ALL '*' INTO TEMP-ONE     03780000
               TEMP-TWO                                                 03790000
             MOVE TEMP-TWO TO IN-COMMAND                                03800000
             MOVE 0 TO L-SPACE-CTR                                      03810000
             MOVE SPACES TO TEMP-TWO.                                   03820000
                                                                        03830000
      *    CHECK THE LEADING SPACE IN INPUT LAST NAME AND TRIM IT OFF   03840000
                                                                        03850000
           INSPECT IN-LAST-NAME TALLYING L-SPACE-CTR FOR LEADING        03860000
             SPACE REPLACING LEADING SPACE BY '*'.                      03870000
           IF L-SPACE-CTR > 0                                           03880000
             UNSTRING IN-LAST-NAME DELIMITED BY ALL '*' INTO TEMP-ONE   03890000
               TEMP-TWO                                                 03900000
             MOVE TEMP-TWO TO IN-LAST-NAME                              03910000
             MOVE 0 TO L-SPACE-CTR                                      03920000
             MOVE SPACES TO TEMP-TWO.                                   03930000
                                                                        03940000
      *    CHECK THE LEADING SPACE IN INPUT FIRST NAME AND TRIM IT OFF  03950000
                                                                        03960000
           INSPECT IN-FIRST-NAME TALLYING L-SPACE-CTR FOR LEADING       03970000
             SPACE REPLACING LEADING SPACE BY '*'.                      03980000
           IF L-SPACE-CTR > 0                                           03990000
             UNSTRING IN-FIRST-NAME DELIMITED BY ALL '*' INTO TEMP-ONE  04000000
               TEMP-TWO                                                 04010000
             MOVE TEMP-TWO TO IN-FIRST-NAME                             04020000
             MOVE 0 TO L-SPACE-CTR                                      04030000
             MOVE SPACES TO TEMP-TWO.                                   04040000
                                                                        04050000
      *    CHECK THE LEADING SPACE IN INPUT EXTENSION AND TRIM IT OFF   04060000
                                                                        04070000
           INSPECT IN-EXTENSION TALLYING L-SPACE-CTR FOR LEADING        04080000
             SPACE REPLACING LEADING SPACE BY '*'.                      04090000
           IF L-SPACE-CTR > 0                                           04100000
             UNSTRING IN-EXTENSION DELIMITED BY ALL '*' INTO TEMP-ONE   04110000
               TEMP-TWO                                                 04120000
             MOVE TEMP-TWO TO IN-EXTENSION                              04130000
             MOVE 0 TO L-SPACE-CTR                                      04140000
             MOVE SPACES TO TEMP-TWO.                                   04150000
                                                                        04160000
      *    CHECK THE LEADING SPACE IN INPUT ZIP CODE AND TRIM IT OFF    04170000
                                                                        04180000
           INSPECT IN-ZIP-CODE TALLYING L-SPACE-CTR FOR LEADING SPACE   04190000
             REPLACING LEADING SPACE BY '*'.                            04200000
           IF L-SPACE-CTR > 0                                           04210000
             UNSTRING IN-ZIP-CODE DELIMITED BY ALL '*' INTO TEMP-ONE    04220000
               TEMP-TWO                                                 04230000
             MOVE TEMP-TWO TO IN-ZIP-CODE                               04240000
             MOVE 0 TO L-SPACE-CTR                                      04250000
             MOVE SPACES TO TEMP-TWO.                                   04260000
      *                                                                 04270000
           MOVE IN-LAST-NAME TO IO-LAST-NAME.                           04280000
           MOVE IN-COMMAND TO IO-COMMAND.                               04290000
                                                                        04300000
           IF IO-COMMAND EQUAL SPACES                                   04310000
           THEN MOVE MINV TO OUT-MESSAGE                                04320000
                PERFORM ISRT-IO THRU ISRT-IO-END                        04330000
           ELSE IF IO-LAST-NAME EQUAL SPACES                            04340000
                THEN MOVE MNONAME TO OUT-MESSAGE                        04350000
                    PERFORM ISRT-IO THRU ISRT-IO-END                    04360000
           ELSE IF TEMP-IOCMD EQUAL 'ADD'                               04370000
                THEN PERFORM TO-ADD THRU TO-ADD-END                     04380000
           ELSE IF TEMP-IOCMD EQUAL 'TAD'                               04390000
                THEN MOVE 1 TO TADD-FLAG                                04400000
                    PERFORM TO-ADD THRU TO-ADD-END                      04410000
           ELSE IF TEMP-IOCMD EQUAL 'UPD'                               04420000
                THEN PERFORM TO-UPD THRU TO-UPD-END                     04430000
           ELSE IF TEMP-IOCMD EQUAL 'DEL'                               04440000
                THEN PERFORM TO-DEL THRU TO-DEL-END                     04450000
           ELSE IF TEMP-IOCMD EQUAL 'DIS'                               04460000
                THEN PERFORM TO-DIS THRU TO-DIS-END                     04470000
           ELSE                                                         04480000
               MOVE IN-COMMAND TO OUT-COMMAND                           04490000
               MOVE IN-LAST-NAME TO OUT-LAST-NAME                       04500000
               MOVE MINV TO OUT-MESSAGE                                 04510000
               PERFORM ISRT-IO THRU ISRT-IO-END.                        04520000
       PROCESS-INPUT-END.                                               04530000
           EXIT.                                                        04540000
                                                                        04550000
      * PROCEDURE GSAM-ERROR                                            04560000
                                                                        04570000
       GSAM-ERROR.                                                      04580000
           MOVE GI-STATUS TO GS-ERROR-STATUS.                           04590000
           DISPLAY GS-TEXT1, GS-ERROR-STATUS, GS-TEXT2,                 04600000
                   GS-ERROR-CALL UPON CONSOLE                           04610000
           GOBACK.                                                      04620000
                                                                        04630000
      * PROCEDURE TO-ADD : ADDITION REQUEST HANDLER                     04640000
                                                                        04650000
       TO-ADD.                                                          04660000
           MOVE IN-FIRST-NAME TO IO-FIRST-NAME.                         04670000
           MOVE IN-EXTENSION  TO IO-EXTENSION.                          04680000
           MOVE IN-ZIP-CODE   TO IO-ZIP-CODE.                           04690000
           MOVE IO-DATA       TO OUT-DATA.                              04700000
           MOVE IO-COMMAND    TO OUT-COMMAND.                           04710000
           IF IN-FIRST-NAME EQUAL SPACES OR                             04720000
              IN-EXTENSION EQUAL SPACES OR                              04730000
              IN-ZIP-CODE EQUAL SPACES                                  04740000
           THEN                                                         04750000
              MOVE MMORE TO OUT-MESSAGE                                 04760000
              PERFORM ISRT-IO THRU ISRT-IO-END                          04770000
           ELSE                                                         04780000
              PERFORM ISRT-DB THRU ISRT-DB-END.                         04790000
       TO-ADD-END.                                                      04800000
           EXIT.                                                        04810000
                                                                        04820000
      * PROCEDURE TO-UPD : UPDATE REQUEST HANDLER                       04830000
                                                                        04840000
       TO-UPD.                                                          04850000
           MOVE 0 TO SET-DATA-FLAG.                                     04860000
           MOVE IN-COMMAND TO OUT-COMMAND.                              04870000
           MOVE IN-LAST-NAME TO OUT-LAST-NAME.                          04880000
           MOVE IO-LAST-NAME TO SSA-KEY.                                04890000
           PERFORM GET-HOLD-UNIQUE-DB THRU GET-HOLD-UNIQUE-DB-END.      04900000
           IF DBSTATUS = SPACES                                         04910000
           THEN                                                         04920000
             IF IN-FIRST-NAME NOT = SPACES                              04930000
               MOVE 1 TO SET-DATA-FLAG                                  04940000
               MOVE IN-FIRST-NAME TO IO-FIRST-NAME                      04950000
             END-IF                                                     04960000
             IF IN-EXTENSION  NOT = SPACES                              04970000
               MOVE 1 TO SET-DATA-FLAG                                  04980000
               MOVE IN-EXTENSION  TO IO-EXTENSION                       04990000
             END-IF                                                     05000000
             IF IN-ZIP-CODE   NOT = SPACES                              05010000
               MOVE 1 TO SET-DATA-FLAG                                  05020000
               MOVE IN-ZIP-CODE   TO IO-ZIP-CODE                        05030000
             END-IF                                                     05040000
             MOVE IO-DATA TO OUT-DATA.                                  05050000
             MOVE IO-COMMAND TO OUT-COMMAND.                            05060000
             IF NO-SET-DATA                                             05070000
             THEN                                                       05080000
               PERFORM REPL-DB THRU REPL-DB-END                         05090000
             ELSE                                                       05100000
               MOVE MNODATA TO OUT-MESSAGE                              05110000
               PERFORM ISRT-IO THRU ISRT-IO-END.                        05120000
       TO-UPD-END.                                                      05130000
           EXIT.                                                        05140000
                                                                        05150000
      * PROCEDURE TO-DEL : DELETE REQUEST HANDLER                       05160000
                                                                        05170000
       TO-DEL.                                                          05180000
           MOVE IO-LAST-NAME TO SSA-KEY.                                05190000
           PERFORM GET-HOLD-UNIQUE-DB THRU GET-HOLD-UNIQUE-DB-END.      05200000
           IF DBSTATUS = SPACES                                         05210000
           THEN                                                         05220000
              MOVE IO-DATA TO OUT-DATA                                  05230000
              MOVE IO-COMMAND TO OUT-COMMAND                            05240000
              PERFORM DLET-DB THRU DLET-DB-END.                         05250000
       TO-DEL-END.                                                      05260000
           EXIT.                                                        05270000
                                                                        05280000
      * PROCEDURE TO-DIS : DISPLAY REQUEST HANDLER                      05290000
                                                                        05300000
       TO-DIS.                                                          05310000
           MOVE IN-COMMAND TO OUT-COMMAND.                              05320000
           MOVE IN-LAST-NAME TO OUT-LAST-NAME.                          05330000
           MOVE IO-LAST-NAME TO SSA-KEY.                                05340000
           PERFORM GET-UNIQUE-DB THRU GET-UNIQUE-DB-END.                05350000
           IF DBSTATUS = SPACES                                         05360000
           THEN                                                         05370000
              MOVE IO-DATA TO OUT-DATA                                  05380000
              MOVE IO-COMMAND TO OUT-COMMAND                            05390000
              MOVE MDIS TO OUT-MESSAGE                                  05400000
              PERFORM ISRT-IO THRU ISRT-IO-END.                         05410000
       TO-DIS-END.                                                      05420000
           EXIT.                                                        05430000
                                                                        05440000
      * PROCEDURE ISRT-DB : DATA BASE SEGMENT INSERT REQUEST HANDLER    05450000
                                                                        05460000
       ISRT-DB.                                                         05470000
           MOVE ISRT TO DC-ERROR-CALL.                                  05480000
           CALL 'CBLTDLI' USING ISRT, DBPCB, IOAREA, SSA1               05490000
           IF DBSTATUS   = SPACES                                       05500000
           THEN                                                         05510000
              IF PROCESS-TADD                                           05520000
                 DISPLAY 'INSERT IS DONE, REPLY' UPON CONSOLE           05530000
                 ACCEPT REPLY FROM CONSOLE                              05540000
                 MOVE 0 TO TADD-FLAG                                    05550000
              END-IF                                                    05560000
              MOVE MADD TO OUT-MESSAGE                                  05570000
              PERFORM ISRT-IO THRU ISRT-IO-END                          05580000
           ELSE                                                         05590000
              MOVE MISRTE TO OUT-MESSAGE                                05600000
              MOVE DBSTATUS TO DC-ERROR-STATUS                          05610000
              PERFORM ISRT-IO THRU ISRT-IO-END.                         05620000
       ISRT-DB-END.                                                     05630000
           EXIT.                                                        05640000
                                                                        05650000
      * PROCEDURE GET-UNIQUE-DB                                         05660000
      *    DATA BASE SEGMENT GET-UNIQUE-DB REQUEST HANDLER              05670000
                                                                        05680000
       GET-UNIQUE-DB.                                                   05690000
           MOVE GET-UNIQUE TO DC-ERROR-CALL.                            05700000
           CALL 'CBLTDLI' USING GET-UNIQUE, DBPCB, IOAREA, SSA.         05710000
           IF DBSTATUS NOT = SPACES                                     05720000
           THEN                                                         05730000
              MOVE MNOENT TO OUT-MESSAGE                                05740000
              MOVE DBSTATUS TO DC-ERROR-STATUS                          05750000
              PERFORM ISRT-IO THRU ISRT-IO-END.                         05760000
       GET-UNIQUE-DB-END.                                               05770000
           EXIT.                                                        05780000
                                                                        05790000
      * PROCEDURE GET-HOLD-UNIQUE-DB                                    05800000
      *    DATA BASE SEGMENT GET-HOLD-UNIQUE-DB REQUEST HANDLER         05810000
                                                                        05820000
       GET-HOLD-UNIQUE-DB.                                              05830000
           MOVE GET-HOLD-UNIQUE TO DC-ERROR-CALL.                       05840000
           CALL 'CBLTDLI' USING GET-HOLD-UNIQUE, DBPCB, IOAREA, SSA.    05850000
           IF DBSTATUS NOT = SPACES                                     05860000
           THEN                                                         05870000
              MOVE MNOENT TO OUT-MESSAGE                                05880000
              MOVE DBSTATUS TO DC-ERROR-STATUS                          05890000
              PERFORM ISRT-IO THRU ISRT-IO-END.                         05900000
       GET-HOLD-UNIQUE-DB-END.                                          05910000
           EXIT.                                                        05920000
                                                                        05930000
      * PROCEDURE REPL-DB : DATA BASE SEGMENT REPLACE REQUEST HANDLER   05940000
                                                                        05950000
       REPL-DB.                                                         05960000
           MOVE REPL TO DC-ERROR-CALL.                                  05970000
           CALL 'CBLTDLI' USING REPL, DBPCB, IOAREA.                    05980000
           IF DBSTATUS = SPACES                                         05990000
           THEN                                                         06000000
              MOVE MUPD1 TO OUT-MESSAGE                                 06010000
              PERFORM ISRT-IO THRU ISRT-IO-END                          06020000
           ELSE                                                         06030000
              MOVE MREPLE TO OUT-MESSAGE                                06040000
              MOVE DBSTATUS TO DC-ERROR-STATUS                          06050000
              PERFORM ISRT-IO THRU ISRT-IO-END.                         06060000
       REPL-DB-END.                                                     06070000
           EXIT.                                                        06080000
                                                                        06090000
      * PROCEDURE DLET-DB : DATA BASE SEGMENT DELETE REQUEST HANDLER    06100000
                                                                        06110000
       DLET-DB.                                                         06120000
           MOVE DLET TO DC-ERROR-CALL.                                  06130000
           CALL 'CBLTDLI' USING DLET, DBPCB, IOAREA.                    06140000
           IF DBSTATUS = SPACES                                         06150000
           THEN                                                         06160000
              MOVE MDEL TO OUT-MESSAGE                                  06170000
              PERFORM ISRT-IO THRU ISRT-IO-END                          06180000
           ELSE                                                         06190000
              MOVE MDLETE TO OUT-MESSAGE                                06200000
              MOVE DBSTATUS TO DC-ERROR-STATUS                          06210000
              PERFORM ISRT-IO THRU ISRT-IO-END.                         06220000
       DLET-DB-END.                                                     06230000
           EXIT.                                                        06240000
                                                                        06250000
                                                                        06260000
      * PROCEDURE ISRT-IO : FORMAT AND PRINT OUTPUT PAGE                06270000
                                                                        06280000
       ISRT-IO.                                                         06290000
           MOVE ISRT  TO GS-ERROR-CALL.                                 06300000
           ADD +1  TO OUT-SEGMENT-NO.                                   06310000
           ACCEPT TEMPDATE FROM DATE.                                   06320000
           PERFORM SETDATE.                                             06330000
                                                                        06340000
           MOVE 1 TO O-ANSI.                                            06350000
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE1.                  06360000
           IF GO-STATUS NOT EQUAL SPACES                                06370000
              PERFORM GSAM-ERROR.                                       06380000
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE2.                  06390000
           IF GO-STATUS NOT EQUAL SPACES                                06400000
              PERFORM GSAM-ERROR.                                       06410000
                                                                        06420000
           MOVE HEADER-BLOCK TO HDRLN.                                  06430000
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE2.                  06440000
           IF GO-STATUS NOT EQUAL SPACES                                06450000
              PERFORM GSAM-ERROR.                                       06460000
           MOVE SPACES TO HDRLN.                                        06470000
                                                                        06480000
           MOVE HEADER-NAME TO HDRLN.                                   06490000
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE2.                  06500000
           IF GO-STATUS NOT EQUAL SPACES                                06510000
              PERFORM GSAM-ERROR.                                       06520000
           MOVE SPACES TO HDRLN.                                        06530000
                                                                        06540000
           MOVE HEADER-BLOCK TO HDRLN.                                  06550000
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE2.                  06560000
           IF GO-STATUS NOT EQUAL SPACES                                06570000
              PERFORM GSAM-ERROR.                                       06580000
           MOVE SPACES TO HDRLN.                                        06590000
                                                                        06600000
           CALL 'CBLTDLI' USING ISRT, GOPCB, BLANKLINE.                 06610000
           IF GO-STATUS NOT EQUAL SPACES                                06620000
              PERFORM GSAM-ERROR.                                       06630000
                                                                        06640000
           CALL 'CBLTDLI' USING ISRT, GOPCB, BLANKLINE.                 06650000
           IF GO-STATUS NOT EQUAL SPACES                                06660000
              PERFORM GSAM-ERROR.                                       06670000
                                                                        06680000
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE3.                  06690000
           IF GO-STATUS NOT EQUAL SPACES                                06700000
              PERFORM GSAM-ERROR.                                       06710000
                                                                        06720000
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE4.                  06730000
           IF GO-STATUS NOT EQUAL SPACES                                06740000
              PERFORM GSAM-ERROR.                                       06750000
                                                                        06760000
           CALL 'CBLTDLI' USING ISRT, GOPCB, BLANKLINE.                 06770000
           IF GO-STATUS NOT EQUAL SPACES                                06780000
              PERFORM GSAM-ERROR.                                       06790000
           MOVE CONSTANT1 TO D2CON1.                                    06800000
           MOVE OUT-COMMAND TO D2VAR.                                   06810000
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE5.                  06820000
           IF GO-STATUS NOT EQUAL SPACES                                06830000
              PERFORM GSAM-ERROR.                                       06840000
           MOVE SPACES TO D2CON1.                                       06850000
           MOVE SPACES TO D2VAR.                                        06860000
                                                                        06870000
           MOVE CONSTANT6 TO D3CON.                                     06880000
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE6.                  06890000
           IF GO-STATUS NOT EQUAL SPACES                                06900000
              PERFORM GSAM-ERROR.                                       06910000
                                                                        06920000
           MOVE CONSTANT2 TO D2CON1.                                    06930000
           MOVE OUT-LAST-NAME TO D2VAR.                                 06940000
           MOVE CONSTANT7  TO D2CON2.                                   06950000
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE5.                  06960000
           IF GO-STATUS NOT EQUAL SPACES                                06970000
              PERFORM GSAM-ERROR.                                       06980000
           MOVE SPACES TO D2CON1.                                       06990000
           MOVE SPACES TO D2VAR.                                        07000000
           MOVE SPACES TO D2CON2.                                       07010000
                                                                        07020000
           MOVE CONSTANT8 TO D2CON2.                                    07030000
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE5.                  07040000
           IF GO-STATUS NOT EQUAL SPACES                                07050000
              PERFORM GSAM-ERROR.                                       07060000
           MOVE SPACES TO D2CON2.                                       07070000
                                                                        07080000
           MOVE CONSTANT3 TO D2CON1.                                    07090000
           MOVE OUT-FIRST-NAME TO D2VAR.                                07100000
           MOVE CONSTANT9 TO D2CON2.                                    07110000
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE5.                  07120000
           IF GO-STATUS NOT EQUAL SPACES                                07130000
              PERFORM GSAM-ERROR.                                       07140000
           MOVE SPACES TO D2CON1.                                       07150000
           MOVE SPACES TO D2VAR.                                        07160000
           MOVE SPACES TO D2CON2.                                       07170000
                                                                        07180000
           MOVE CONSTANT10 TO D2CON2.                                   07190000
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE5.                  07200000
           IF GO-STATUS NOT EQUAL SPACES                                07210000
              PERFORM GSAM-ERROR.                                       07220000
           MOVE SPACES TO D2CON2.                                       07230000
                                                                        07240000
           MOVE CONSTANT4 TO D2CON1.                                    07250000
           MOVE OUT-EXTENSION TO D2VAR.                                 07260000
           MOVE CONSTANT11 TO D2CON2.                                   07270000
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE5.                  07280000
           IF GO-STATUS NOT EQUAL SPACES                                07290000
              PERFORM GSAM-ERROR.                                       07300000
           MOVE SPACES TO D2CON1.                                       07310000
           MOVE SPACES TO D2VAR.                                        07320000
           MOVE SPACES TO D2CON2.                                       07330000
                                                                        07340000
           CALL 'CBLTDLI' USING ISRT, GOPCB, BLANKLINE.                 07350000
           IF GO-STATUS NOT EQUAL SPACES                                07360000
              PERFORM GSAM-ERROR.                                       07370000
                                                                        07380000
           MOVE CONSTANT5 TO D2CON1.                                    07390000
           MOVE OUT-ZIP-CODE TO D2VAR.                                  07400000
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE5.                  07410000
           IF GO-STATUS NOT EQUAL SPACES                                07420000
              PERFORM GSAM-ERROR.                                       07430000
           MOVE SPACES TO D2CON1.                                       07440000
           MOVE SPACES TO D2VAR.                                        07450000
                                                                        07460000
           CALL 'CBLTDLI' USING ISRT, GOPCB, BLANKLINE.                 07470000
           IF GO-STATUS NOT EQUAL SPACES                                07480000
              PERFORM GSAM-ERROR.                                       07490000
                                                                        07500000
           CALL 'CBLTDLI' USING ISRT, GOPCB, BLANKLINE.                 07510000
           IF GO-STATUS NOT EQUAL SPACES                                07520000
              PERFORM GSAM-ERROR.                                       07530000
                                                                        07540000
           MOVE OUT-MESSAGE TO D4VAR1.                                  07550000
           MOVE OUT-SEGMENT-NO TO D4VAR2.                               07560000
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE7.                  07570000
           IF GO-STATUS NOT EQUAL SPACES                                07580000
              PERFORM GSAM-ERROR.                                       07590000
                                                                        07600000
           CALL 'CBLTDLI' USING ISRT, GOPCB, BLANKLINE.                 07610000
           IF GO-STATUS NOT EQUAL SPACES                                07620000
              PERFORM GSAM-ERROR.                                       07630000
                                                                        07640000
           CALL 'CBLTDLI' USING ISRT, GOPCB, BLANKLINE.                 07650000
           IF GO-STATUS NOT EQUAL SPACES                                07660000
              PERFORM GSAM-ERROR.                                       07670000
                                                                        07680000
           CALL 'CBLTDLI' USING ISRT, GOPCB, BLANKLINE.                 07690000
           IF GO-STATUS NOT EQUAL SPACES                                07700000
              PERFORM GSAM-ERROR.                                       07710000
                                                                        07720000
           MOVE 0 TO O-ANSI.                                            07730000
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE1.                  07740000
           IF GO-STATUS NOT EQUAL SPACES                                07750000
              PERFORM GSAM-ERROR.                                       07760000
       ISRT-IO-END.                                                     07770000
           EXIT.                                                        07780000
                                                                        07790000
      * PROCEDURE SETDATE : SET THE DATE                                07800000
                                                                        07810000
       SETDATE.                                                         07820000
           MOVE TYY TO YY.                                              07830000
           MOVE TMM TO MM.                                              07840000
           MOVE TDD TO DD.                                              07850000
           EXIT.                                                        07860000
                                                                        07870000
                                                                        07880000