      *-------------------------*                                       00011241
       IDENTIFICATION DIVISION.                                         00011826
      *-------------------------*                                       00011941
       PROGRAM-ID. PCREA.                                               00012030
       AUTHOR. JEAN MARC C.
      *----------------------------------------------------------*      00012243
      * Auteur: Jean Marc C.                                            00012344
      *                                                                 00012444
      * But: Creer un nouveau segment TREATMNT dans IMS DB              00012543
      *      pour un patient defini dans le fichier en entree           00012643
      *                                                                 00012743
      * Fichier entree: contient le patient a mettre a jour             00012843
      *      et les donnees a creer pour le segment TREATMNT            00012943
      *----------------------------------------------------------*      00013043
      *--------------------*                                            00013143
       ENVIRONMENT DIVISION.                                            00013243
      *--------------------*                                            00013343
       CONFIGURATION SECTION.                                           00013443
       OBJECT-COMPUTER.                                                 00013543
       SOURCE-COMPUTER.                                                 00013643
            IBM-SYSTEM WITH DEBUGGING MODE.                             00013743
       INPUT-OUTPUT SECTION.                                            00013843
       FILE-CONTROL.                                                    00013943
           SELECT FI01-IN ASSIGN TO FI01IN                              00014043
           ORGANIZATION IS  SEQUENTIAL                                  00014143
           FILE STATUS IS WS-FI01-FS.                                   00014243
      *----------------*                                                00014441
       DATA DIVISION.                                                   00014526
      *----------------*                                                00014641
       FILE SECTION.                                                    00014726
                                                                        00014826
       FD  FI01-IN RECORDING MODE F.                                    00014926
       01  FI01-IN-DATA                 PIC X(80).                      00015026
                                                                        00015126
      *-----------------------*                                         00015226
       WORKING-STORAGE SECTION.                                         00015326
      *-----------------------*                                         00015426
       01 WS-COUNTERS.                                                  00015526
           05 WS-NO-READ-FI01              PIC 9(8).                    00015628
                                                                        00015733
       01 WS-FILE-STATUS.                                               00016026
          05 WS-FI01-FS                PIC X(2).                        00016132
                                                                        00016233
       01 WS-FI01-END-OF-FILE           PIC X(5) VALUE 'FALSE'.         00016426
          88 WS-FI01-EOF                VALUE 'TRUE'.                   00016526
          88 WS-FI01-NOT-EOF            VALUE 'FALSE'.                  00016626
                                                                        00016728
      * ce fichier contient les records a creer dans IMS DB             00016843
      * -> segment patient et donnees du segment treatmnt               00016943
       01 WS-REC-FI01.                                                  00017028
          05 FI01-SEGMENT-TYPE         PIC X(08).                       00017132
          05 FILLER                    PIC X(01).                       00017233
          05 FI01-DATA                 PIC X(72).                       00017333
          05 FI01-DATA-PATIENT  REDEFINES FI01-DATA.                    00017433
             10 FI01-DATA-PATIENID     PIC X(03).                       00017533
             10 FILLER                 PIC X(69).                       00017633
          05 FI01-DATA-TREATMNT REDEFINES FI01-DATA.                    00017733
             10 FI01-DATA-TRTNAME      PIC X(20).                       00017833
             10 FI01-DATA-DOCTOR       PIC X(20).                       00017933
             10 FILLER                 PIC X(32).                       00018033
                                                                        00018133
       01 WS-SAVE-PATIENID             PIC X(03).                       00018233
                                                                        00018333
       01 QUAL-SSA-PATIENT.                                             00018433
           05  SEGNAME     PIC X(08) VALUE 'PATIENT'.                   00018533
           05  FILLER      PIC X(01) VALUE '('.                         00018633
           05  FIELD       PIC X(08) VALUE 'PATIENID'.                  00018733
           05  OPER        PIC X(02) VALUE 'EQ'.                        00019032
           05  FIELD-VAL   PIC X(03) VALUE SPACE.                       00019132
           05  FILLER      PIC X(01) VALUE ')'.                         00019232
                                                                        00019332
       01 QUAL-SSA-TREATMNT.                                            00019432
           05  SEGNAME     PIC X(8) VALUE 'TREATMNT'.                   00019532
           05  FILLER      PIC X(1) VALUE '('.                          00019632
           05  FIELD-NAME  PIC X(8) VALUE 'TRDATE'.                     00019732
           05  OPER        PIC X(2) VALUE 'EQ'.                         00019832
           05  FIELD-VAL   PIC X(6) VALUE SPACE.                        00019932
           05  FILLER      PIC X(1) VALUE ')'.                          00020032
                                                                        00020132
       01 UNQUAL-SSA-PATIENT.                                           00020232
           05 SEGMENT-NAME PIC X(8) VALUE 'PATIENT'.                    00020332
           05 FILLER  PIC X VALUE SPACE.                                00020432
                                                                        00020532
       01 UNQUAL-SSA-TREATMNT.                                          00020632
           05 SEGMENT-NAME PIC X(8) VALUE 'TREATMNT'.                   00020732
           05 FILLER PIC X VALUE SPACE.                                 00020832
                                                                        00020932
       01 DLI-FUNCTIONS.                                                00021829
        05 DLI-GU   PIC X(4) VALUE 'GU '.                               00021934
        05 DLI-GHU  PIC X(4) VALUE 'GHU '.                              00022034
        05 DLI-GN   PIC X(4) VALUE 'GN '.                               00022134
        05 DLI-GHN  PIC X(4) VALUE 'GHN '.                              00022234
        05 DLI-GNP  PIC X(4) VALUE 'GNP '.                              00022334
        05 DLI-GHNP PIC X(4) VALUE 'GHNP'.                              00022429
        05 DLI-ISRT PIC X(4) VALUE 'ISRT'.                              00022529
        05 DLI-DLET PIC X(4) VALUE 'DLET'.                              00022629
        05 DLI-REPL PIC X(4) VALUE 'REPL'.                              00022729
        05 DLI-CHKP PIC X(4) VALUE 'CHKP'.                              00022829
        05 DLI-XRST PIC X(4) VALUE 'XRST'.                              00022929
        05 DLI-PCB  PIC X(4) VALUE 'PCB '.                              00023034
                                                                        00023234
       01 SEG-IO-AREA     PIC X(60).                                    00023334
       01 WS-DLI-FUNCTION PIC X(4).                                     00023434
                                                                        00023534
       01 WS-TREATMNT-SEG.                                              00023636
          05 WS-TRTNAME   PIC X(20).                                    00023736
          05 WS-DOCTOR    PIC X(20).                                    00023836
                                                                        00023940
      *-----------------------*                                         00026140
       LINKAGE SECTION.                                                 00026240
      *-----------------------*                                         00026340
                                                                        00026440
      * psb to get and insert                                           00026540
        01 PCB-MASK-GI.                                                 00026640
           03 DBD-NAME        PIC X(8).                                 00026740
           03 SEG-LEVEL       PIC XX.                                   00026840
           03 STATUS-CODE     PIC XX.                                   00026940
           03 PROC-OPT        PIC X(4).                                 00027040
           03 FILLER          PIC X(4).                                 00027140
           03 SEG-NAME        PIC X(8).                                 00027240
           03 KEY-FDBK        PIC S9(5) COMP.                           00027340
           03 NUM-SENSEG      PIC S9(5) COMP.                           00027440
           03 KEY-FDBK-AREA.                                            00027540
              05 PATIENT-KEY    PIC X(3).                               00027640
              05 MEDICAL-KEY    PIC X(6).                               00027740
              05 DRUG-KEY       PIC X(8).                               00027840
              05 BILLING-KEY    PIC X(8).                               00027940
                                                                        00028040
                                                                        00028140
      *-----------------------*                                         00028240
       PROCEDURE DIVISION.                                              00028340
      *-----------------------*                                         00028440
                                                                        00028540
           INITIALIZE PCB-MASK-GI.                                      00028640
           ENTRY 'DLITCBL' USING PCB-MASK-GI.                           00028740
                                                                        00028840
           DISPLAY "***** DEBUT PROCEDURE DIVISION *****".              00029143
                                                                        00029340
           pERFORM 1000-INIT                                            00029442
              THRU 1000-INIT-END.                                       00029540
                                                                        00029640
           DISPLAY '1-DBD-NAME      :'    DBD-NAME.                     00030140
           DISPLAY '1-SEG-LEVEL     :'    SEG-LEVEL.                    00030240
           DISPLAY '1-STATUS-CODE   :'    STATUS-CODE.                  00030340
           DISPLAY '1-PROC-OPT      :'    PROC-OPT.                     00030440
           DISPLAY '1-SEG-NAME      :'    SEG-NAME.                     00030540
           DISPLAY '1-KEY-FDBK      :'    KEY-FDBK.                     00030640
           DISPLAY '1-NUM-SENSEG    :'    NUM-SENSEG.                   00030740
           DISPLAY '1-KEY-FDBK-AREA :' KEY-FDBK-AREA.                   00030840
           DISPLAY '*------------------------*'.                        00030940
                                                                        00031040
           DISPLAY '1_GET PATIENT'.                                     00031140
           MOVE DLI-GHU  TO WS-DLI-FUNCTION.                            00031243
           MOVE WS-SAVE-PATIENID TO FIELD-VAL OF QUAL-SSA-PATIENT.      00031340
           PERFORM 4200-GET-A-PATIENT                                   00031440
              THRU 4200-GET-A-PATIENT-END.                              00031540
                                                                        00031640
           DISPLAY '2_GET ALL TREATMNT FOR THIS PATIENT'.               00031743
           MOVE DLI-GN  TO WS-DLI-FUNCTION.                             00031840
           PERFORM 4300-GET-TREATMNT                                    00031940
              THRU 4300-GET-TREATMNT-END                                00032040
              UNTIL STATUS-CODE NOT = SPACE.                            00032140
                                                                        00032240
      *    then read file again to get first treatmnt                   00032340
           PERFORM 8100-READ-FI01                                       00032440
              THRU 8100-READ-FI01-END.                                  00032540
                                                                        00034040
      *    create segment until end of input file                       00034140
           DISPLAY '3_____CREATE TREATMNT'.                             00034240
           PERFORM 5000-CREATE-TREATMNT                                 00034340
              THRU 5000-CREATE-TREATMNT-END                             00034440
              UNTIL WS-FI01-EOF.                                        00034540
                                                                        00034640
      *    we have to reposition on the patient                         00034740
           DISPLAY '4_____GET PATIENT'.                                 00034840
           MOVE DLI-GU  TO WS-DLI-FUNCTION.                             00034940
           MOVE WS-SAVE-PATIENID TO FIELD-VAL OF QUAL-SSA-PATIENT.      00035040
           PERFORM 4200-GET-A-PATIENT                                   00035140
              THRU 4200-GET-A-PATIENT-END.                              00035240
                                                                        00035340
           DISPLAY '5_____GET ALL TREATMNT'.                            00035440
           MOVE DLI-GN  TO WS-DLI-FUNCTION.                             00035540
           PERFORM 4300-GET-TREATMNT                                    00035640
              THRU 4300-GET-TREATMNT-END                                00035740
              UNTIL STATUS-CODE NOT = SPACE.                            00035840
                                                                        00035940
           DISPLAY "***** FIN PROCEDURE DIVISION *****".                00036043
                                                                        00036143
           GOBACK.                                                      00036240
                                                                        00036340
      *------------*                                                    00036441
       1000-INIT.                                                       00036540
      *------------*                                                    00036641
           DISPLAY "***** INIT PROCESS *****".                          00036740
                                                                        00036840
           MOVE SPACE TO WS-FILE-STATUS.                                00036940
           MOVE ZEROES TO WS-COUNTERS.                                  00037040
                                                                        00037140
           OPEN INPUT  FI01-IN.                                         00037240
                                                                        00037340
           IF WS-FI01-FS NOT = "00"                                     00037440
      D      DISPLAY "ERROR OPEN FILE FI01-IN: " WS-FI01-FS             00037540
             PERFORM 9999-ABEND                                         00037640
                THRU 9999-ABEND-END                                     00037740
           ELSE                                                         00037840
      D      DISPLAY "OPEN FI01-IN IS OK"                               00037940
           END-IF.                                                      00038040
                                                                        00038140
      *    first read of the input file                                 00038240
           PERFORM 8100-READ-FI01                                       00038340
              THRU 8100-READ-FI01-END.                                  00038440
                                                                        00038540
           MOVE FI01-DATA-PATIENID TO WS-SAVE-PATIENID.                 00038640
      D    DISPLAY 'Patient ID to update: ' WS-SAVE-PATIENID.           00038740
                                                                        00038843
           DISPLAY "***** FIN INIT PROCESS *****".                      00038943
                                                                        00039040
      *----------------*                                                00039141
       1000-INIT-END.                                                   00039240
      *----------------*                                                00039341
           EXIT.                                                        00039440
                                                                        00039540
      *---------------------*                                           00039641
       4200-GET-A-PATIENT.                                              00039740
      *---------------------*                                           00039841
                                                                        00039940
           INITIALIZE SEG-IO-AREA,                                      00040040
                                                                        00040140
           CALL 'CBLTDLI' USING WS-DLI-FUNCTION,                        00040240
                                PCB-MASK-GI,                            00040340
                                SEG-IO-AREA,                            00040440
                                QUAL-SSA-PATIENT.                       00040540
                                                                        00040640
           IF STATUS-CODE = '  '                                        00040740
                DISPLAY 'SUCCESSFUL GET: '  SEG-IO-AREA                 00040840
           ELSE                                                         00040940
                DISPLAY 'ERROR IN FETCH :' STATUS-CODE                  00041040
                DISPLAY 'SEG-IO-AREA    :' SEG-IO-AREA                  00041140
                DISPLAY 'DBD-NAME       :'     DBD-NAME                 00041240
                DISPLAY 'SEG-LEVEL      :'    SEG-LEVEL                 00041340
                DISPLAY 'STATUS-CODE    :'   STATUS-CODE                00041440
                DISPLAY 'PROC-OPT       :'    PROC-OPT                  00041540
                DISPLAY 'SEG-NAME       :'    SEG-NAME                  00041640
                DISPLAY 'KEY-FDBK       :'    KEY-FDBK                  00041740
                DISPLAY 'NUM-SENSEG     :'   NUM-SENSEG                 00041840
                DISPLAY 'KEY-FDBK-AREA  :' KEY-FDBK-AREA                00041940
           END-IF.                                                      00042040
                                                                        00042140
      *----------------------*                                          00042240
       4200-GET-A-PATIENT-END.                                          00042340
      *----------------------*                                          00042440
           EXIT.                                                        00042540
                                                                        00042640
      *----------------------*                                          00042740
       4300-GET-TREATMNT.                                               00042840
      *----------------------*                                          00042940
                                                                        00043040
           INITIALIZE SEG-IO-AREA,                                      00043140
                                                                        00043240
           CALL 'CBLTDLI' USING WS-DLI-FUNCTION,                        00043340
                                PCB-MASK-GI,                            00043440
                                SEG-IO-AREA,                            00043540
                                QUAL-SSA-PATIENT,                       00043640
                                UNQUAL-SSA-TREATMNT.                    00043740
                                                                        00043840
           IF STATUS-CODE = '  '                                        00043940
                DISPLAY 'SUCCESSFUL GET: '  SEG-IO-AREA                 00044040
           ELSE                                                         00044140
                DISPLAY 'ERROR IN FETCH :' STATUS-CODE                  00044240
                DISPLAY 'SEG-IO-AREA    :' SEG-IO-AREA                  00044340
                DISPLAY 'DBD-NAME       :'     DBD-NAME                 00044440
                DISPLAY 'SEG-LEVEL      :'    SEG-LEVEL                 00044540
                DISPLAY 'STATUS-CODE    :'   STATUS-CODE                00044640
                DISPLAY 'PROC-OPT       :'    PROC-OPT                  00044740
                DISPLAY 'SEG-NAME       :'    SEG-NAME                  00044840
                DISPLAY 'KEY-FDBK       :'    KEY-FDBK                  00044940
                DISPLAY 'NUM-SENSEG     :'   NUM-SENSEG                 00045040
                DISPLAY 'KEY-FDBK-AREA  :' KEY-FDBK-AREA                00045140
           END-IF.                                                      00045240
                                                                        00045340
      *----------------------*                                          00045440
       4300-GET-TREATMNT-END.                                           00045540
      *----------------------*                                          00045640
           EXIT.                                                        00045740
                                                                        00045840
      *-----------------------*                                         00045940
       5000-CREATE-TREATMNT.                                            00046040
      *-----------------------*                                         00046140
                                                                        00046240
           EVALUATE FI01-SEGMENT-TYPE                                   00046340
             WHEN 'TREATMNT'                                            00046440
      D        DISPLAY "WE ARE ON A TREATMNT SEGMENT"                   00046540
      D        DISPLAY "WS-REC-FI01      : " WS-REC-FI01                00046640
      D        DISPLAY "FI01-DATA-TRTNAME: " FI01-DATA-TRTNAME          00046740
      D        DISPLAY "FI01-DATA-DOCTOR : " FI01-DATA-DOCTOR           00046840
             WHEN OTHER                                                 00046940
      D        DISPLAY "UNKNOWN SEGMENT: " FI01-SEGMENT-TYPE            00047040
           END-EVALUATE.                                                00047140
                                                                        00047240
           INITIALIZE WS-TREATMNT-SEG.                                  00047340
           MOVE FI01-DATA-TRTNAME    TO WS-TRTNAME.                     00047440
           MOVE FI01-DATA-DOCTOR     TO WS-DOCTOR.                      00047540
           MOVE WS-TREATMNT-SEG      TO SEG-IO-AREA.                    00047640
                                                                        00047740
           CALL 'CBLTDLI' USING DLI-ISRT,                               00047840
                                PCB-MASK-GI,                            00047940
                                SEG-IO-AREA,                            00048040
                                QUAL-SSA-PATIENT,                       00048140
                                UNQUAL-SSA-TREATMNT.                    00048240
                                                                        00048340
           IF STATUS-CODE = '  '                                        00048440
              DISPLAY 'CREATE IS OK'                                    00048540
              DISPLAY 'SEG-IO : ' SEG-IO-AREA                           00048640
           ELSE                                                         00049029
              DISPLAY 'ERROR IN FETCH :' STATUS-CODE                    00050029
              DISPLAY 'SEG-IO-AREA    :' SEG-IO-AREA                    00060029
              DISPLAY 'DBD-NAME       :'       DBD-NAME                 00070029
              DISPLAY 'SEG-LEVEL      :'      SEG-LEVEL                 00080029
              DISPLAY 'STATUS-CODE    :'     STATUS-CODE                00090029
              DISPLAY 'PROC-OPT       :'      PROC-OPT                  00100029
              DISPLAY 'SEG-NAME       :'      SEG-NAME                  00110029
              DISPLAY 'KEY-FDBK       :'      KEY-FDBK                  00120029
              DISPLAY 'NUM-SENSEG     :'     NUM-SENSEG                 00130029
              DISPLAY 'KEY-FDBK-AREA  :' KEY-FDBK-AREA                  00140029
           END-IF.                                                      00150029
                                                                        00150129
           IF NOT WS-FI01-EOF                                           00150337
              PERFORM 8100-READ-FI01                                    00151037
                 THRU 8100-READ-FI01-END                                00152037
           END-IF.                                                      00152137
                                                                        00152233
      *-----------------------*                                         00170026
       5000-CREATE-TREATMNT-END.                                        00180033
      *-----------------------*                                         00190026
           EXIT.                                                        00200008
                                                                        00640127
      *---------------*                                                 00641027
       8100-READ-FI01.                                                  00642027
      *---------------*                                                 00643027
                                                                        00643127
           INITIALIZE WS-REC-FI01.                                      00643228
                                                                        00643327
           READ FI01-IN INTO WS-REC-FI01                                00643428
           END-READ.                                                    00643527
                                                                        00643627
           EVALUATE TRUE                                                00643727
                                                                        00643827
             WHEN WS-FI01-FS = '00'                                     00643927
               ADD 1 TO WS-NO-READ-FI01                                 00644128
      D        DISPLAY "READ FILE OK: " WS-REC-FI01                     00644237
               CONTINUE                                                 00644327
             WHEN WS-FI01-FS = '10'                                     00644427
               SET WS-FI01-EOF TO TRUE                                  00644527
      D        DISPLAY "WS-FI01-END-OF-FILE " WS-FI01-END-OF-FILE       00644627
             WHEN OTHER                                                 00644727
      D        DISPLAY "ERROR READ FILE FI01 !!!: " WS-FI01-FS          00644827
               PERFORM 9999-ABEND                                       00644927
                  THRU 9999-ABEND-END                                   00645027
                                                                        00645127
           END-EVALUATE.                                                00645227
                                                                        00645327
      *-------------------*                                             00645427
       8100-READ-FI01-END.                                              00645527
      *-------------------*                                             00645627
           EXIT.                                                        00646027
      *-------------------*                                             00647041
       9999-ABEND.                                                      00648027
      *-------------------*                                             00649041
      D    DISPLAY "WE ARE IN ABEND".                                   00649127
      *    WE FORCE AN ABEND                                            00649427
      *>      MOVE +40                TO WS-USER-ABEND-CODE             00649527
      *>      CALL 'ILBOABN0'      USING WS-USER-ABEND-CODE             00649627
           GOBACK.                                                      00649727
      *-------------------*                                             00649841
       9999-ABEND-END.                                                  00649927
      *-------------------*                                             00650041
           EXIT.                                                        00651027
                                                                        00660018

