 CBL  APOST
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FSPISRTO.
      ********************************************************@SCPYRT**
      *                                                               *
      *  Licensed Materials - Property of IBM                         *
      *                                                               *
      *  5635-A06                                                     *
      *                                                               *
      *      Copyright IBM Corp. 1991,1998 All Rights Reserved        *
      *                                                               *
      *  US Government Users Restricted Rights - Use, duplication or  *
      *  disclosure restricted by GSA ADP Schedule contract with      *
      *  IBM Corp.

      *                                                               *
      * This sample is a refactored version of the IMS sample         *
      * IVP application DFSIVA64. The purpose of this refactored      *
      * version is to demonstrate shift left testing with IDz's zUnit *
      * and application integration testing with IBM Z Virtual Test   *
      * Platform
      *
      *     Flemming Skovgaard Petersen, flemming.petersen@dk.ibm.com
      *         Copyright IBM Corp, 2021                              *
      ********************************************************@ECPYRT**
       DATE-WRITTEN. 11/08/2021.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * CONSTANTS                                                       00000103
                                                                        00000104
       77  HEADER-BLOCK    PIC X(50)                                    00000105
           VALUE '**************************************************'.  00000106
       77  HEADER-NAME     PIC X(50)                                    00000107
           VALUE '*  IMS INSTALLATION VERIFICATION PROCEDURE       *'.  00000108
       77  CONSTANT1       PIC X(24)                                    00000109
           VALUE   'PROCESS  CODE  (*1) :   '.                          00000110
       77  CONSTANT2       PIC X(24)                                    00000111
           VALUE   'LAST  NAME          :   '.                          00000112
       77  CONSTANT3       PIC X(24)                                    00000113
           VALUE   'FIRST NAME          :   '.                          00000114
       77  CONSTANT4       PIC X(24)                                    00000115
           VALUE   'EXTENSION  NUMBER   :   '.                          00000116
       77  CONSTANT5       PIC X(24)                                    00000117
           VALUE   'INTERNAL  ZIP CODE  :   '.                          00000118
       77  CONSTANT6       PIC X(17)                                    00000119
           VALUE   '(*1) PROCESS CODE'.                                 00000120
       77  CONSTANT7       PIC X(7)                                     00000121
           VALUE   'ADD    '.                                           00000122
       77  CONSTANT8       PIC X(7)                                     00000123
           VALUE   'DELETE '.                                           00000124
       77  CONSTANT9       PIC X(7)                                     00000125
           VALUE   'UPDATE '.                                           00000126
       77  CONSTANT10      PIC X(7)                                     00000127
           VALUE   'DISPLAY'.                                           00000128
       77  CONSTANT11      PIC X(7)                                     00000129
           VALUE   'TADD   '.

                                                                        00000130
       77  ISRT            PIC  X(4)  VALUE 'ISRT'.

       01  BLANKLINE.                                                   00000149
           02  ANSI     PIC  X.                                         00000150
           02  HFILLER  PIC  X(24)  VALUE SPACES.                       00000151
           02  LEDGE    PIC  X(1)   VALUE '|'.                          00000152
           02  FILLER   PIC  X(80)  VALUE SPACES.                       00000153
           02  REDGE    PIC  X(1)   VALUE '|'.                          00000154
           02  FILLER   PIC  X(14)  VALUE SPACES.                       00000155

       01  TEMPDATE.                                                    00000292
           02  TYY      PIC  99.                                        00000293
           02  TMM      PIC  99.                                        00000294
           02  TDD      PIC  99.                                        00000295

      * OUTLINE FORMAT                                                  00000147
                                                                        00000148
       01  OUTLINE1.                                                    00000156
           02  O-ANSI   PIC  X.                                         00000157
           02  HFILLER  PIC  X(24)  VALUE SPACES.                       00000158
           02  OUTLN1A  PIC  X(40)                                      00000159
               VALUE '*---------------------------------------'.        00000160
           02  OUTLN1B  PIC  X(42)                                      00000161
               VALUE '-----------------------------------------*'.      00000162
           02  FILLER   PIC  X(14)  VALUE SPACES.                       00000163
       01  OUTLINE2.                                                    00000164
           02  ANSI     PIC  X.                                         00000165
           02  HFILLER  PIC  X(24)  VALUE SPACES.                       00000166
           02  LEDGE    PIC  X(1)   VALUE '|'.                          00000167
           02  FILLER   PIC  X(15)  VALUE SPACES.                       00000168
           02  HDRLN    PIC  X(50)  VALUE SPACES.                       00000169
           02  FILLER   PIC  X(15)  VALUE SPACES.                       00000170
           02  REDGE    PIC  X(1)   VALUE '|'.                          00000171
           02  FILLER   PIC  X(14)  VALUE SPACES.                       00000172
       01  OUTLINE3.                                                    00000173
           02  ANSI     PIC  X.                                         00000174
           02  HFILLER  PIC  X(24)  VALUE SPACES.                       00000175
           02  LEDGE    PIC  X(1)   VALUE '|'.                          00000176
           02  FILLER   PIC  X(40)  VALUE SPACES.                       00000177
           02  D1LN     PIC  X(27)                                      00000178
               VALUE 'TRANSACTION TYPE : BMP/DLI '.                     00000179
           02  D2LN     PIC  X(10)  VALUE '(HIDAM DB)'.                 00000180
           02  FILLER   PIC  X(3)   VALUE SPACES.                       00000181
           02  REDGE    PIC  X(1)   VALUE '|'.                          00000182
           02  FILLER   PIC  X(14)  VALUE SPACES.                       00000183
       01  OUTLINE4.                                                    00000184
           02  ANSI     PIC  X.                                         00000185
           02  HFILLER  PIC  X(24)  VALUE SPACES.                       00000186
           02  LEDGE    PIC  X(1)   VALUE '|'.                          00000187
           02  FILLER   PIC  X(40)  VALUE SPACES.                       00000188
           02  D1CON    PIC  X(19)  VALUE 'DATE      :'.                00000189
           02  D1VAR    PIC  X(8)   VALUE '  /  /  '.                   00000190
           02  TEMP-DATE REDEFINES D1VAR.                               00000191
               04  MM          PIC  X(2).                               00000192
               04  DATE-FILL1  PIC  X.                                  00000193
               04  DD          PIC  X(2).                               00000194
               04  DATE-FILL2  PIC  X.                                  00000195
               04  YY          PIC  X(2).                               00000196
           02  FILLER   PIC  X(13)  VALUE SPACES.                       00000197
           02  REDGE    PIC  X(1)   VALUE '|'.                          00000198
           02  FILLER   PIC  X(14)  VALUE SPACES.                       00000199
       01  OUTLINE5.                                                    00000200
           02  ANSI     PIC  X.                                         00000201
           02  HFILLER  PIC  X(24)  VALUE SPACES.                       00000202
           02  LEDGE    PIC  X(1)   VALUE '|'.                          00000203
           02  DFILL2   PIC  X(10)  VALUE SPACES.                       00000204
           02  D2CON1   PIC  X(24).                                     00000205
           02  D2VAR    PIC  X(10).                                     00000206
           02  DFILL2A  PIC  X(23)  VALUE SPACES.                       00000207
           02  D2CON2   PIC  X(7)   VALUE SPACES.                       00000208
           02  FILLER   PIC  X(6)   VALUE SPACES.                       00000209
           02  REDGE    PIC  X(1)   VALUE '|'.                          00000210
           02  FILLER   PIC  X(14)  VALUE SPACES.                       00000211
       01  OUTLINE6.                                                    00000212
           02  ANSI     PIC  X.                                         00000213
           02  HFILLER  PIC  X(24)  VALUE SPACES.                       00000214
           02  LEDGE    PIC  X(1)   VALUE '|'.                          00000215
           02  DFILL3   PIC  X(59)  VALUE SPACES.                       00000216
           02  D3CON    PIC  X(17).                                     00000217
           02  FILLER   PIC  X(4)   VALUE SPACES.                       00000218
           02  REDGE    PIC  X(1)   VALUE '|'.                          00000219
           02  FILLER   PIC  X(14)  VALUE SPACES.                       00000220
       01  OUTLINE7.                                                    00000221
           02  ANSI     PIC  X.                                         00000222
           02  HFILLER  PIC  X(24)  VALUE SPACES.                       00000223
           02  LEDGE    PIC  X(1)   VALUE '|'.                          00000224
           02  DFILL4   PIC  X(10)  VALUE SPACES.                       00000225
           02  D4VAR1   PIC  X(40).                                     00000226
           02  DFILL4A  PIC  X(10)  VALUE SPACES.                       00000227
           02  D4CON    PIC  X(12)  VALUE 'SEGMENT# :  '.               00000228
           02  D4VAR2   PIC  X(4).                                      00000229
           02  FILLER   PIC  X(4)   VALUE SPACES.                       00000230
           02  REDGE    PIC  X(1)   VALUE '|'.                          00000231
           02  FILLER   PIC  X(14)  VALUE SPACES.                       00000232

       01  GS-TEXT.                                                     00000278
           02  GS-TEXT1           PIC  X(7).                            00000279
           02  GS-ERROR-STATUS    PIC  X(2).                            00000280
           02  GS-TEXT2           PIC  X(12).                           00000281
           02  GS-ERROR-CALL      PIC  X(4).                            00000282

       LINKAGE SECTION.
                                                                        00000232
       01  OUTPUT-AREA.                                                 00000251
           02  OUT-BLANK  PIC  X(85).                                   00000252
           02  OUT-TEXT REDEFINES OUT-BLANK.                            00000253
               03  OUT-MESSAGE   PIC  X(40).                            00000254
               03  OUT-COMMAND   PIC  X(8).                             00000255
               03  OUT-DATA.                                            00000256
                   04  OUT-LAST-NAME   PIC  X(10).                      00000257
                   04  OUT-FIRST-NAME  PIC  X(10).                      00000258
                   04  OUT-EXTENSION   PIC  X(10).                      00000259
                   04  OUT-ZIP-CODE    PIC  X(7).                       00000260
           02  OUT-SEGMENT-NO    PIC  9(4).                             00000261
           02  OUT-FILL          PIC  X(32).                            00000262
       01  GIPCB.                                                       00000326
           02  DBD-NAME        PIC  X(8).                               00000327
           02  SEG-LEVEL       PIC  X(2).                               00000328
           02  GI-STATUS       PIC  X(2).                               00000329
           02  PROC-OPTIONS    PIC  X(4).                               00000330
           02  RESERVE-DLI     PIC  X(4).                               00000331
           02  SEG-NAME-FB     PIC  X(8).                               00000332
           02  LENGTH-FB-KEY   PIC  9(4).                               00000333
           02  NUMB-SENS-SEGS  PIC  9(4).                               00000334
           02  KEY-FB-AREA     PIC  X(17).                              00000335
       01  GOPCB.                                                       00000336
           02  DBD-NAME        PIC  X(8).                               00000337
           02  SEG-LEVEL       PIC  X(2).                               00000338
           02  GO-STATUS       PIC  X(2).                               00000339
           02  PROC-OPTIONS    PIC  X(4).                               00000340
           02  RESERVE-DLI     PIC  x(4).                               00000341
           02  SEG-NAME-FB     PIC  X(8).                               00000342
           02  LENGTH-FB-KEY   PIC  9(4).                               00000343
           02  NUMB-SENS-SEGS  PIC  9(4).                               00000344
           02  KEY-FB-AREA     PIC  X(17).                              00000345


       PROCEDURE DIVISION USING OUTPUT-AREA,
            GIPCB, GOPCB.
       MAIN SECTION.
           MOVE ISRT  TO GS-ERROR-CALL.                                 00000630
           ADD +1  TO OUT-SEGMENT-NO.                                   00000631
           ACCEPT TEMPDATE FROM DATE.                                   00000632
           PERFORM SETDATE.                                             00000633
                                                                        00000634
           MOVE 1 TO O-ANSI.                                            00000635
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE1.                  00000636
           IF GO-STATUS NOT EQUAL SPACES                                00000637
              PERFORM GSAM-ERROR.                                       00000638
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE2.                  00000639
           IF GO-STATUS NOT EQUAL SPACES                                00000640
              PERFORM GSAM-ERROR.                                       00000641
                                                                        00000642
           MOVE HEADER-BLOCK TO HDRLN.                                  00000643
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE2.                  00000644
           IF GO-STATUS NOT EQUAL SPACES                                00000645
              PERFORM GSAM-ERROR.                                       00000646
           MOVE SPACES TO HDRLN.                                        00000647
                                                                        00000648
           MOVE HEADER-NAME TO HDRLN.                                   00000649
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE2.                  00000650
           IF GO-STATUS NOT EQUAL SPACES                                00000651
              PERFORM GSAM-ERROR.                                       00000652
           MOVE SPACES TO HDRLN.                                        00000653
                                                                        00000654
           MOVE HEADER-BLOCK TO HDRLN.                                  00000655
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE2.                  00000656
           IF GO-STATUS NOT EQUAL SPACES                                00000657
              PERFORM GSAM-ERROR.                                       00000658
           MOVE SPACES TO HDRLN.                                        00000659
                                                                        00000660
           CALL 'CBLTDLI' USING ISRT, GOPCB, BLANKLINE.                 00000661
           IF GO-STATUS NOT EQUAL SPACES                                00000662
              PERFORM GSAM-ERROR.                                       00000663
                                                                        00000664
           CALL 'CBLTDLI' USING ISRT, GOPCB, BLANKLINE.                 00000665
           IF GO-STATUS NOT EQUAL SPACES                                00000666
              PERFORM GSAM-ERROR.                                       00000667
                                                                        00000668
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE3.                  00000669
           IF GO-STATUS NOT EQUAL SPACES                                00000670
              PERFORM GSAM-ERROR.                                       00000671
                                                                        00000672
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE4.                  00000673
           IF GO-STATUS NOT EQUAL SPACES                                00000674
              PERFORM GSAM-ERROR.                                       00000675
                                                                        00000676
           CALL 'CBLTDLI' USING ISRT, GOPCB, BLANKLINE.                 00000677
           IF GO-STATUS NOT EQUAL SPACES                                00000678
              PERFORM GSAM-ERROR.                                       00000679
           MOVE CONSTANT1 TO D2CON1.                                    00000680
           MOVE OUT-COMMAND TO D2VAR.                                   00000681
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE5.                  00000682
           IF GO-STATUS NOT EQUAL SPACES                                00000683
              PERFORM GSAM-ERROR.                                       00000684
           MOVE SPACES TO D2CON1.                                       00000685
           MOVE SPACES TO D2VAR.                                        00000686
                                                                        00000687
           MOVE CONSTANT6 TO D3CON.                                     00000688
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE6.                  00000689
           IF GO-STATUS NOT EQUAL SPACES                                00000690
              PERFORM GSAM-ERROR.                                       00000691
                                                                        00000692
           MOVE CONSTANT2 TO D2CON1.                                    00000693
           MOVE OUT-LAST-NAME TO D2VAR.                                 00000694
           MOVE CONSTANT7  TO D2CON2.                                   00000695
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE5.                  00000696
           IF GO-STATUS NOT EQUAL SPACES                                00000697
              PERFORM GSAM-ERROR.                                       00000698
           MOVE SPACES TO D2CON1.                                       00000699
           MOVE SPACES TO D2VAR.                                        00000700
           MOVE SPACES TO D2CON2.                                       00000701
                                                                        00000702
           MOVE CONSTANT8 TO D2CON2.                                    00000703
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE5.                  00000704
           IF GO-STATUS NOT EQUAL SPACES                                00000705
              PERFORM GSAM-ERROR.                                       00000706
           MOVE SPACES TO D2CON2.                                       00000707
                                                                        00000708
           MOVE CONSTANT3 TO D2CON1.                                    00000709
           MOVE OUT-FIRST-NAME TO D2VAR.                                00000710
           MOVE CONSTANT9 TO D2CON2.                                    00000711
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE5.                  00000712
           IF GO-STATUS NOT EQUAL SPACES                                00000713
              PERFORM GSAM-ERROR.                                       00000714
           MOVE SPACES TO D2CON1.                                       00000715
           MOVE SPACES TO D2VAR.                                        00000716
           MOVE SPACES TO D2CON2.                                       00000717
                                                                        00000718
           MOVE CONSTANT10 TO D2CON2.                                   00000719
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE5.                  00000720
           IF GO-STATUS NOT EQUAL SPACES                                00000721
              PERFORM GSAM-ERROR.                                       00000722
           MOVE SPACES TO D2CON2.                                       00000723
                                                                        00000724
           MOVE CONSTANT4 TO D2CON1.                                    00000725
           MOVE OUT-EXTENSION TO D2VAR.                                 00000726
           MOVE CONSTANT11 TO D2CON2.                                   00000727
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE5.                  00000728
           IF GO-STATUS NOT EQUAL SPACES                                00000729
              PERFORM GSAM-ERROR.                                       00000730
           MOVE SPACES TO D2CON1.                                       00000731
           MOVE SPACES TO D2VAR.                                        00000732
           MOVE SPACES TO D2CON2.                                       00000733
                                                                        00000734
           CALL 'CBLTDLI' USING ISRT, GOPCB, BLANKLINE.                 00000735
           IF GO-STATUS NOT EQUAL SPACES                                00000736
              PERFORM GSAM-ERROR.                                       00000737
                                                                        00000738
           MOVE CONSTANT5 TO D2CON1.                                    00000739
           MOVE OUT-ZIP-CODE TO D2VAR.                                  00000740
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE5.                  00000741
           IF GO-STATUS NOT EQUAL SPACES                                00000742
              PERFORM GSAM-ERROR.                                       00000743
           MOVE SPACES TO D2CON1.                                       00000744
           MOVE SPACES TO D2VAR.                                        00000745
                                                                        00000746
           CALL 'CBLTDLI' USING ISRT, GOPCB, BLANKLINE.                 00000747
           IF GO-STATUS NOT EQUAL SPACES                                00000748
              PERFORM GSAM-ERROR.                                       00000749
                                                                        00000750
           CALL 'CBLTDLI' USING ISRT, GOPCB, BLANKLINE.                 00000751
           IF GO-STATUS NOT EQUAL SPACES                                00000752
              PERFORM GSAM-ERROR.                                       00000753
                                                                        00000754
           MOVE OUT-MESSAGE TO D4VAR1.                                  00000755
           MOVE OUT-SEGMENT-NO TO D4VAR2.                               00000756
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE7.                  00000757
           IF GO-STATUS NOT EQUAL SPACES                                00000758
              PERFORM GSAM-ERROR.                                       00000759
                                                                        00000760
           CALL 'CBLTDLI' USING ISRT, GOPCB, BLANKLINE.                 00000761
           IF GO-STATUS NOT EQUAL SPACES                                00000762
              PERFORM GSAM-ERROR.                                       00000763
                                                                        00000764
           CALL 'CBLTDLI' USING ISRT, GOPCB, BLANKLINE.                 00000765
           IF GO-STATUS NOT EQUAL SPACES                                00000766
              PERFORM GSAM-ERROR.                                       00000767
                                                                        00000768
           CALL 'CBLTDLI' USING ISRT, GOPCB, BLANKLINE.                 00000769
           IF GO-STATUS NOT EQUAL SPACES                                00000770
              PERFORM GSAM-ERROR.                                       00000771
                                                                        00000772
           MOVE 0 TO O-ANSI.                                            00000773
           CALL 'CBLTDLI' USING ISRT, GOPCB, OUTLINE1.                  00000774
           IF GO-STATUS NOT EQUAL SPACES                                00000775
              PERFORM GSAM-ERROR.                                       00000776

           GOBACK.

       GSAM-ERROR.                                                      00000458
           MOVE GI-STATUS TO GS-ERROR-STATUS.                           00000459
           DISPLAY GS-TEXT1, GS-ERROR-STATUS, GS-TEXT2,                 00000460
                   GS-ERROR-CALL UPON CONSOLE                           00000461
           GOBACK.                                                      00000462


       SETDATE.                                                         00000782
           MOVE TYY TO YY.                                              00000783
           MOVE TMM TO MM.                                              00000784
           MOVE TDD TO DD.                                              00000785
           EXIT.                                                        00000786


       END PROGRAM FSPISRTO.