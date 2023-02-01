      *-------------*                                                   00010046
       ID DIVISION.                                                     00011800
      *-------------*                                                   00011946
       PROGRAM-ID. PREAD.                                               00012045
      *----------------------------------------------------------*      00012154
      * Auteur: Jean Marc C.                                            00012254
      *                                                                 00012354
      * But: LECTURE de la base IMS DB pour differents segments         00012454
      *----------------------------------------------------------*      00012554
      *----------------------*                                          00012655
       ENVIRONMENT DIVISION.                                            00012700
      *----------------------*                                          00012855
      *----------------------*                                          00012956
       DATA DIVISION.                                                   00013056
      *----------------------*                                          00013156
      *----------------------*                                          00013346
       WORKING-STORAGE SECTION.                                         00013400
      *----------------------*                                          00013546
       01 QUAL-SSA-PATIENT.                                             00013641
           05  SEGNAME     PIC X(08) VALUE 'PATIENT'.                   00013739
           05  FILLER      PIC X(01) VALUE '('.                         00014000
           05  FIELD       PIC X(08) VALUE 'PATIENID'.                  00014141
           05  OPER        PIC X(02) VALUE 'EQ'.                        00014200
           05  FIELD-VAL   PIC X(03) VALUE SPACE.                       00014344
           05  FILLER      PIC X(01) VALUE ')'.                         00014400
                                                                        00015605
       01 QUAL-SSA-TREATMNT.                                            00017946
           05  SEGNAME     PIC X(8) VALUE 'TREATMNT'.                   00018046
           05  FILLER      PIC X(1) VALUE '('.                          00018125
           05  FIELD-NAME  PIC X(8) VALUE 'TRDATE'.                     00018225
           05  OPER        PIC X(2) VALUE 'EQ'.                         00018325
           05  FIELD-VAL   PIC X(6) VALUE SPACE.                        00018428
           05  FILLER      PIC X(1) VALUE ')'.                          00018525
                                                                        00020839
       01 UNQUAL-SSA-PATIENT.                                           00020944
           05 SEGMENT-NAME PIC X(8) VALUE 'PATIENT'.                    00021044
           05 FILLER  PIC X VALUE SPACE.                                00021139
                                                                        00021244
       01 UNQUAL-SSA-TREATMNT.                                          00021646
           05 SEGMENT-NAME PIC X(8) VALUE 'TREATMNT'.                   00021746
           05 FILLER PIC X VALUE SPACE.                                 00021839
                                                                        00021939
       01 UNQUAL-SSA-BILLING.                                           00022047
           05 SEGMENT-NAME PIC X(8) VALUE 'BILLING'.                    00022147
           05 FILLER PIC X VALUE SPACE.                                 00022247
                                                                        00022447
       01 SEG-IO-AREA     PIC X(60).                                    00022547
                                                                        00022647
       01 WS-TREATMNT-SEG.                                              00022747
          05 WS-TRDATE  PIC X(06).                                      00022847
          05 WS-TRTTYPE PIC X(20).                                      00022947
                                                                        00023047
       01 DLI-FUNCTIONS.                                                00023147
        05 DLI-GU PIC X(4) VALUE 'GU '.                                 00023247
        05 DLI-GHU PIC X(4) VALUE 'GHU '.                               00023347
        05 DLI-GN PIC X(4) VALUE 'GN '.                                 00023447
        05 DLI-GHN PIC X(4) VALUE 'GHN '.                               00023547
        05 DLI-GNP PIC X(4) VALUE 'GNP '.                               00023647
        05 DLI-GHNP PIC X(4) VALUE 'GHNP'.                              00023747
        05 DLI-ISRT PIC X(4) VALUE 'ISRT'.                              00023847
        05 DLI-DLET PIC X(4) VALUE 'DLET'.                              00023947
        05 DLI-REPL PIC X(4) VALUE 'REPL'.                              00024047
        05 DLI-CHKP PIC X(4) VALUE 'CHKP'.                              00024147
        05 DLI-XRST PIC X(4) VALUE 'XRST'.                              00024247
        05 DLI-PCB PIC X(4) VALUE 'PCB '.                               00024347
                                                                        00024447
       01 WS-DLI-FUNCTION  PIC X(4).                                    00024547
                                                                        00024647
      *----------------------*                                          00024747
       LINKAGE SECTION.                                                 00024847
      *----------------------*                                          00024947
                                                                        00025047
      * psb to get and insert                                           00025147
       01 PCB-MASK-GI.                                                  00025247
           03 DBD-NAME        PIC X(8).                                 00025347
           03 SEG-LEVEL       PIC XX.                                   00025447
           03 STATUS-CODE     PIC XX.                                   00025547
           03 PROC-OPT        PIC X(4).                                 00025647
           03 FILLER          PIC X(4).                                 00025747
           03 SEG-NAME        PIC X(8).                                 00025847
           03 KEY-FDBK        PIC S9(5) COMP.                           00025947
           03 NUM-SENSEG      PIC S9(5) COMP.                           00026047
           03 KEY-FDBK-AREA.                                            00026147
              05 PATIENT-KEY    PIC X(3).                               00026248
              05 MEDICAL-KEY    PIC X(6).                               00026348
              05 DRUG-KEY       PIC X(8).                               00026448
              05 BILLING-KEY    PIC X(8).                               00026548
                                                                        00026747
      *----------------------*                                          00026847
       PROCEDURE DIVISION.                                              00026947
      *----------------------*                                          00027047
                                                                        00027147
           INITIALIZE PCB-MASK-GI.                                      00027247
           ENTRY 'DLITCBL' USING PCB-MASK-GI.                           00027347
                                                                        00027447
           DISPLAY '*------------------------*'.                        00027547
           DISPLAY ' *** BEGIN PROG PREAD ***'.                         00027647
           DISPLAY '*------------------------*'.                        00027747
           DISPLAY '1-DBD-NAME      :'    DBD-NAME.                     00027847
           DISPLAY '1-SEG-LEVEL     :'    SEG-LEVEL.                    00027947
           DISPLAY '1-STATUS-CODE   :'    STATUS-CODE.                  00028047
           DISPLAY '1-PROC-OPT      :'    PROC-OPT.                     00028147
           DISPLAY '1-SEG-NAME      :'    SEG-NAME.                     00028247
           DISPLAY '1-KEY-FDBK      :'    KEY-FDBK.                     00028347
           DISPLAY '1-NUM-SENSEG    :'    NUM-SENSEG.                   00028447
           DISPLAY '1-KEY-FDBK-AREA :' KEY-FDBK-AREA.                   00028547
           DISPLAY '*------------------------*'.                        00028647
                                                                        00028747
           DISPLAY '1_GET ALL THE PATIENTS OF DB___'.                   00028850
           MOVE DLI-GN  TO WS-DLI-FUNCTION.                             00028947
           PERFORM 4100-GET-PATIENT                                     00029047
              THRU 4100-GET-PATIENT-END                                 00029147
              UNTIL STATUS-CODE NOT = SPACE.                            00029247
                                                                        00029347
           DISPLAY '2_GET A SINGLE PATIENT__________'.                  00029450
           MOVE DLI-GU  TO WS-DLI-FUNCTION.                             00029547
           MOVE '003'   TO FIELD-VAL OF QUAL-SSA-PATIENT.               00029647
           PERFORM 4200-GET-A-PATIENT                                   00029747
              THRU 4200-GET-A-PATIENT-END.                              00029847
                                                                        00030918
           DISPLAY '3_GET ALL BILLINGS FOR A PATIENT_'.                 00031050
           MOVE DLI-GN  TO WS-DLI-FUNCTION.                             00031147
           PERFORM 4300-GET-BILLING                                     00031347
              THRU 4300-GET-BILLING-END                                 00031447
              UNTIL STATUS-CODE NOT = SPACE.                            00031547
                                                                        00031647
           DISPLAY '4_____GET ALL TREATMNT'.                            00031750
           MOVE DLI-GN  TO WS-DLI-FUNCTION.                             00031850
           INITIALIZE STATUS-CODE.                                      00031951
           PERFORM 4400-GET-TREATMNT                                    00032050
              THRU 4400-GET-TREATMNT-END                                00032150
              UNTIL STATUS-CODE NOT = SPACE.                            00032250
                                                                        00032349
           GOBACK.                                                      00032449
                                                                        00032549
      *----------------------*                                          00032649
       4100-GET-PATIENT.                                                00032749
      *----------------------*                                          00032849
                                                                        00032949
           INITIALIZE SEG-IO-AREA,                                      00033049
                                                                        00033149
           CALL 'CBLTDLI' USING WS-DLI-FUNCTION,                        00033249
                                PCB-MASK-GI,                            00033349
                                SEG-IO-AREA,                            00033449
                                UNQUAL-SSA-PATIENT.                     00033549
                                                                        00034018
           IF STATUS-CODE = '  '                                        00034218
                DISPLAY 'SUCCESSFUL GET: '  SEG-IO-AREA                 00034346
           ELSE                                                         00035018
                DISPLAY 'ERROR IN FETCH :' STATUS-CODE                  00035118
                DISPLAY 'SEG-IO-AREA    :' SEG-IO-AREA                  00035218
                DISPLAY 'DBD-NAME       :'     DBD-NAME                 00035318
                DISPLAY 'SEG-LEVEL      :'    SEG-LEVEL                 00035418
                DISPLAY 'STATUS-CODE    :'   STATUS-CODE                00035518
                DISPLAY 'PROC-OPT       :'    PROC-OPT                  00035618
                DISPLAY 'SEG-NAME       :'    SEG-NAME                  00035718
                DISPLAY 'KEY-FDBK       :'    KEY-FDBK                  00035818
                DISPLAY 'NUM-SENSEG     :'   NUM-SENSEG                 00035933
                DISPLAY 'KEY-FDBK-AREA  :' KEY-FDBK-AREA                00036033
           END-IF.                                                      00036118
                                                                        00036218
      *----------------------*                                          00036346
       4100-GET-PATIENT-END.                                            00036446
      *----------------------*                                          00036546
           EXIT.                                                        00036618
                                                                        00036746
      *----------------------*                                          00036846
       4200-GET-A-PATIENT.                                              00036946
      *----------------------*                                          00037046
                                                                        00037146
           INITIALIZE SEG-IO-AREA,                                      00037247
                                                                        00037347
           CALL 'CBLTDLI' USING WS-DLI-FUNCTION,                        00038046
                                PCB-MASK-GI,                            00039046
                                SEG-IO-AREA,                            00040046
                                QUAL-SSA-PATIENT.                       00050046
                                                                        00060046
           IF STATUS-CODE = '  '                                        00070046
                DISPLAY 'SUCCESSFUL GET: '  SEG-IO-AREA                 00080046
           ELSE                                                         00090046
                DISPLAY 'ERROR IN FETCH :' STATUS-CODE                  00100046
                DISPLAY 'SEG-IO-AREA    :' SEG-IO-AREA                  00110046
                DISPLAY 'DBD-NAME       :'     DBD-NAME                 00120046
                DISPLAY 'SEG-LEVEL      :'    SEG-LEVEL                 00130046
                DISPLAY 'STATUS-CODE    :'   STATUS-CODE                00140046
                DISPLAY 'PROC-OPT       :'    PROC-OPT                  00150046
                DISPLAY 'SEG-NAME       :'    SEG-NAME                  00160046
                DISPLAY 'KEY-FDBK       :'    KEY-FDBK                  00170046
                DISPLAY 'NUM-SENSEG     :'   NUM-SENSEG                 00180046
                DISPLAY 'KEY-FDBK-AREA  :' KEY-FDBK-AREA                00190046
           END-IF.                                                      00200046
                                                                        00210046
      *----------------------*                                          00220046
       4200-GET-A-PATIENT-END.                                          00230046
      *----------------------*                                          00240046
           EXIT.                                                        00250046
                                                                        00251047
      *----------------------*                                          00260047
       4300-GET-BILLING.                                                00270047
      *----------------------*                                          00280047
                                                                        00290047
           INITIALIZE SEG-IO-AREA,                                      00291047
                                                                        00292047
           CALL 'CBLTDLI' USING WS-DLI-FUNCTION,                        00300047
                                PCB-MASK-GI,                            00310047
                                SEG-IO-AREA,                            00320047
                                UNQUAL-SSA-BILLING.                     00330047
                                                                        00340047
           IF STATUS-CODE = '  '                                        00350047
                DISPLAY 'SUCCESSFUL GET: '  SEG-IO-AREA                 00360047
           ELSE                                                         00370047
                DISPLAY 'ERROR IN FETCH :' STATUS-CODE                  00380047
                DISPLAY 'SEG-IO-AREA    :' SEG-IO-AREA                  00390047
                DISPLAY 'DBD-NAME       :'     DBD-NAME                 00400047
                DISPLAY 'SEG-LEVEL      :'    SEG-LEVEL                 00410047
                DISPLAY 'STATUS-CODE    :'   STATUS-CODE                00420047
                DISPLAY 'PROC-OPT       :'    PROC-OPT                  00430047
                DISPLAY 'SEG-NAME       :'    SEG-NAME                  00440047
                DISPLAY 'KEY-FDBK       :'    KEY-FDBK                  00450047
                DISPLAY 'NUM-SENSEG     :'   NUM-SENSEG                 00460047
                DISPLAY 'KEY-FDBK-AREA  :' KEY-FDBK-AREA                00470047
           END-IF.                                                      00480047
                                                                        00490047
      *----------------------*                                          00500047
       4300-GET-BILLING-END.                                            00510047
      *----------------------*                                          00520047
           EXIT.                                                        00530047
                                                                        00530150
      *----------------------*                                          00531050
       4400-GET-TREATMNT.                                               00532050
      *----------------------*                                          00533050
                                                                        00534050
           INITIALIZE SEG-IO-AREA,                                      00535050
                                                                        00536050
           CALL 'CBLTDLI' USING WS-DLI-FUNCTION,                        00537050
                                PCB-MASK-GI,                            00538050
                                SEG-IO-AREA,                            00539050
                                QUAL-SSA-PATIENT,                       00539150
                                UNQUAL-SSA-TREATMNT.                    00539250
                                                                        00539350
           IF STATUS-CODE = '  '                                        00539450
                DISPLAY 'SUCCESSFUL GET: '  SEG-IO-AREA                 00539550
           ELSE                                                         00539650
                DISPLAY 'ERROR IN FETCH :' STATUS-CODE                  00539750
                DISPLAY 'SEG-IO-AREA    :' SEG-IO-AREA                  00539850
                DISPLAY 'DBD-NAME       :'     DBD-NAME                 00539950
                DISPLAY 'SEG-LEVEL      :'    SEG-LEVEL                 00540050
                DISPLAY 'STATUS-CODE    :'   STATUS-CODE                00541050
                DISPLAY 'PROC-OPT       :'    PROC-OPT                  00542050
                DISPLAY 'SEG-NAME       :'    SEG-NAME                  00543050
                DISPLAY 'KEY-FDBK       :'    KEY-FDBK                  00544050
                DISPLAY 'NUM-SENSEG     :'   NUM-SENSEG                 00545050
                DISPLAY 'KEY-FDBK-AREA  :' KEY-FDBK-AREA                00546050
           END-IF.                                                      00547050
                                                                        00548050
      *----------------------*                                          00549050
       4400-GET-TREATMNT-END.                                           00549150
      *----------------------*                                          00549250
           EXIT.                                                        00549350
                                                                        00549450
                                                                        00550047
