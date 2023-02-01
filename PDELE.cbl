      *--------------------*                                            00011238
       IDENTIFICATION DIVISION.                                         00011338
      *--------------------*                                            00011438
       PROGRAM-ID. PDELE.   
       AUTHOR. JEAN MARC C.
      *----------------------------------------------------------*      00011743
      * Auteur: Jean Marc C.                                            00011845
      *                                                                 00011945
      * But: supprimer segment TREATMNT dans IMS DB                     00012043
      *      pour un patient defini dans le fichier en entree           00012143
      *                                                                 00012243
      * Fichier entree: contient le patient a mettre a jour             00012343
      *      et les donnees du segment a supprimer                      00012443
      *----------------------------------------------------------*      00012543
      *--------------------*                                            00012638
       ENVIRONMENT DIVISION.                                            00012738
      *--------------------*                                            00012838
       CONFIGURATION SECTION.                                           00012938
       OBJECT-COMPUTER.                                                 00013038
       SOURCE-COMPUTER.                                                 00014038
            IBM-SYSTEM WITH DEBUGGING MODE.                             00015038
       INPUT-OUTPUT SECTION.                                            00016038
       FILE-CONTROL.                                                    00017038
           SELECT FI01-IN ASSIGN TO FI01IN                              00018038
           ORGANIZATION IS  SEQUENTIAL                                  00019038
           FILE STATUS IS WS-FI01-FS.                                   00020038
      *-------------------*                                             00030042
       DATA DIVISION.                                                   00040038
      *-------------------*                                             00050042
       FILE SECTION.                                                    00060038
                                                                        00070038
       FD  FI01-IN RECORDING MODE F.                                    00080038
       01  FI01-IN-DATA                 PIC X(80).                      00090038
                                                                        00100038
      *-----------------------*                                         00110038
       WORKING-STORAGE SECTION.                                         00120038
      *-----------------------*                                         00130038
       01 WS-COUNTERS.                                                  00140038
           05 WS-NO-READ-FI01              PIC 9(8).                    00150038
                                                                        00160038
       01 WS-FILE-STATUS.                                               00170038
          05 WS-FI01-FS                PIC X(2).                        00180038
                                                                        00190038
       01 WS-FI01-END-OF-FILE           PIC X(5) VALUE 'FALSE'.         00200038
          88 WS-FI01-EOF                VALUE 'TRUE'.                   00210038
          88 WS-FI01-NOT-EOF            VALUE 'FALSE'.                  00220038
                                                                        00230038
      * THIS FILE contains records to delete in IMS                     00240038
       01 WS-REC-FI01.                                                  00250038
          05 FI01-SEGMENT-TYPE         PIC X(08).                       00260038
          05 FILLER                    PIC X(01).                       00270038
          05 FI01-DATA                 PIC X(72).                       00280038
          05 FI01-DATA-PATIENT  REDEFINES FI01-DATA.                    00290038
             10 FI01-DATA-PATIENID     PIC X(03).                       00300038
             10 FILLER                 PIC X(69).                       00310038
          05 FI01-DATA-TREATMNT REDEFINES FI01-DATA.                    00320038
             10 FI01-DATA-TRTNAME      PIC X(20).                       00330038
             10 FI01-DATA-DOCTOR       PIC X(20).                       00340038
             10 FILLER                 PIC X(32).                       00350038
                                                                        00360038
       01 WS-SAVE-PATIENID             PIC X(03).                       00370038
                                                                        00380038
       01 QUAL-SSA-PATIENT.                                             00390038
           05  SEGNAME     PIC X(08) VALUE 'PATIENT'.                   00400038
           05  FILLER      PIC X(01) VALUE '('.                         00410038
           05  FIELD       PIC X(08) VALUE 'PATIENID'.                  00420038
           05  OPER        PIC X(02) VALUE 'EQ'.                        00430038
           05  FIELD-VAL   PIC X(03) VALUE SPACE.                       00440038
           05  FILLER      PIC X(01) VALUE ')'.                         00450038
                                                                        00460038
       01 QUAL-SSA-TREATMNT.                                            00470038
           05  SEGNAME     PIC X(08) VALUE 'TREATMNT'.                  00480039
           05  FILLER      PIC X(01) VALUE '('.                         00490039
           05  FIELD-NAME  PIC X(08) VALUE 'TRTNAME'.                   00500039
           05  OPER        PIC X(02) VALUE 'EQ'.                        00510039
           05  FIELD-VAL   PIC X(20) VALUE SPACE.                       00520039
           05  FILLER      PIC X(01) VALUE ')'.                         00530039
                                                                        00540038
       01 UNQUAL-SSA-PATIENT.                                           00550038
           05 SEGMENT-NAME PIC X(8) VALUE 'PATIENT'.                    00560038
           05 FILLER  PIC X VALUE SPACE.                                00570038
                                                                        00580038
       01 UNQUAL-SSA-TREATMNT.                                          00590038
           05 SEGMENT-NAME PIC X(8) VALUE 'TREATMNT'.                   00600038
           05 FILLER PIC X VALUE SPACE.                                 00610038
                                                                        00620038
       01 UNQUAL-SSA-BILLING.                                           00630038
           05 SEGMENT-NAME PIC X(8) VALUE 'BILLING'.                    00640038
           05 FILLER PIC X VALUE SPACE.                                 00650038
                                                                        00660038
       01 DLI-FUNCTIONS.                                                00670038
        05 DLI-GU   PIC X(4) VALUE 'GU '.                               00680038
        05 DLI-GHU  PIC X(4) VALUE 'GHU '.                              00690038
        05 DLI-GN   PIC X(4) VALUE 'GN '.                               00700038
        05 DLI-GHN  PIC X(4) VALUE 'GHN '.                              00710038
        05 DLI-GNP  PIC X(4) VALUE 'GNP '.                              00720038
        05 DLI-GHNP PIC X(4) VALUE 'GHNP'.                              00730038
        05 DLI-ISRT PIC X(4) VALUE 'ISRT'.                              00740038
        05 DLI-DLET PIC X(4) VALUE 'DLET'.                              00750038
        05 DLI-REPL PIC X(4) VALUE 'REPL'.                              00760038
        05 DLI-CHKP PIC X(4) VALUE 'CHKP'.                              00770038
        05 DLI-XRST PIC X(4) VALUE 'XRST'.                              00780038
        05 DLI-PCB  PIC X(4) VALUE 'PCB '.                              00790038
                                                                        00800038
       01 SEG-IO-AREA     PIC X(60).                                    00810038
       01 WS-DLI-FUNCTION PIC X(4).                                     00820038
                                                                        00830038
       01 WS-TREATMNT-SEG.                                              00840038
          05 WS-TRTNAME   PIC X(20).                                    00850038
          05 WS-DOCTOR    PIC X(20).                                    00860038
                                                                        00861041
       01 WS-NO-OCCURS    PIC 9(6) VALUE ZERO.                          00862041
                                                                        00870038
      *-----------------------*                                         00880038
       LINKAGE SECTION.                                                 00890038
      *-----------------------*                                         00900038
                                                                        00910038
      * psb to get and insert                                           00920038
        01 PCB-MASK-GI.                                                 00930038
           03 DBD-NAME        PIC X(8).                                 00940038
           03 SEG-LEVEL       PIC XX.                                   00950038
           03 STATUS-CODE     PIC XX.                                   00960038
           03 PROC-OPT        PIC X(4).                                 00970038
           03 FILLER          PIC X(4).                                 00980038
           03 SEG-NAME        PIC X(8).                                 00990038
           03 KEY-FDBK        PIC S9(5) COMP.                           01000038
           03 NUM-SENSEG      PIC S9(5) COMP.                           01010038
           03 KEY-FDBK-AREA.                                            01020038
              05 PATIENT-KEY    PIC X(3).                               01030038
              05 MEDICAL-KEY    PIC X(6).                               01040038
              05 DRUG-KEY       PIC X(8).                               01050038
              05 BILLING-KEY    PIC X(8).                               01060038
                                                                        01070038
                                                                        01080038
      *-----------------------*                                         01090038
       PROCEDURE DIVISION.                                              01100038
      *-----------------------*                                         01110038
                                                                        01120038
           INITIALIZE PCB-MASK-GI.                                      01130038
           ENTRY 'DLITCBL' USING PCB-MASK-GI.                           01140038
                                                                        01150038
           DISPLAY '*------------------------*'.                        01160038
           DISPLAY ' *** BEGIN PROG BY JMC ***'.                        01170038
           DISPLAY '*------------------------*'.                        01180038
                                                                        01190038
           PERFORM 1000-INIT                                            01200038
              THRU 1000-INIT-END.                                       01210038
                                                                        01220038
           DISPLAY '1-DBD-NAME      :'    DBD-NAME.                     01230038
           DISPLAY '1-SEG-LEVEL     :'    SEG-LEVEL.                    01240038
           DISPLAY '1-STATUS-CODE   :'    STATUS-CODE.                  01250038
           DISPLAY '1-PROC-OPT      :'    PROC-OPT.                     01260038
           DISPLAY '1-SEG-NAME      :'    SEG-NAME.                     01270038
           DISPLAY '1-KEY-FDBK      :'    KEY-FDBK.                     01280038
           DISPLAY '1-NUM-SENSEG    :'    NUM-SENSEG.                   01290038
           DISPLAY '1-KEY-FDBK-AREA :' KEY-FDBK-AREA.                   01300038
           DISPLAY '*------------------------*'.                        01310038
                                                                        01320038
           DISPLAY '1_GET PATIENT'.                                     01330038
           MOVE DLI-GHU  TO WS-DLI-FUNCTION.                            01340039
           MOVE WS-SAVE-PATIENID TO FIELD-VAL OF QUAL-SSA-PATIENT.      01350038
           PERFORM 4200-GET-A-PATIENT                                   01360038
              THRU 4200-GET-A-PATIENT-END.                              01370038
                                                                        01380038
           DISPLAY '2_____GET ALL TREATMNT FOR THIS PATIENT'.           01390038
           MOVE ZERO TO WS-NO-OCCURS.                                   01391041
           MOVE DLI-GHN  TO WS-DLI-FUNCTION.                            01400039
           PERFORM 4300-GET-TREATMNT                                    01410038
              THRU 4300-GET-TREATMNT-END                                01420038
              UNTIL STATUS-CODE NOT = SPACE.                            01430038
           DISPLAY '__WS-NO-OCCURS: ' WS-NO-OCCURS.                     01431041
                                                                        01440038
      *    then read file again to get first treatmnt                   01450038
           PERFORM 8100-READ-FI01                                       01460038
              THRU 8100-READ-FI01-END.                                  01470038
                                                                        01480038
      *    delete segment until end of input file                       01490038
           DISPLAY '3_____DELETE TREATMNT'.                             01500038
           PERFORM 5000-DELETE-TREATMNT                                 01510038
              THRU 5000-DELETE-TREATMNT-END                             01520038
              UNTIL WS-FI01-EOF.                                        01530038
                                                                        01540038
      *    we have to reposition on the patient                         01550038
           DISPLAY '4_____GET PATIENT'.                                 01560038
           MOVE DLI-GU  TO WS-DLI-FUNCTION.                             01570038
           MOVE WS-SAVE-PATIENID TO FIELD-VAL OF QUAL-SSA-PATIENT.      01580038
           PERFORM 4200-GET-A-PATIENT                                   01590038
              THRU 4200-GET-A-PATIENT-END.                              01600038
                                                                        01610038
           DISPLAY '5_____GET ALL TREATMNT'.                            01620038
           MOVE ZERO TO WS-NO-OCCURS.                                   01621041
           MOVE DLI-GN  TO WS-DLI-FUNCTION.                             01630038
           PERFORM 4300-GET-TREATMNT                                    01640038
              THRU 4300-GET-TREATMNT-END                                01650038
              UNTIL STATUS-CODE NOT = SPACE.                            01660038
           DISPLAY '__WS-NO-OCCURS: ' WS-NO-OCCURS.                     01661041
                                                                        01670038
           GOBACK.                                                      01680038
                                                                        01690038
      *-----------------------*                                         01700038
       1000-INIT.                                                       01710038
      *-----------------------*                                         01720038
           DISPLAY "***** INIT PROCESS *****".                          01730038
                                                                        01740038
           MOVE SPACE TO WS-FILE-STATUS.                                01750038
           MOVE ZEROES TO WS-COUNTERS.                                  01760038
                                                                        01770038
           OPEN INPUT  FI01-IN.                                         01780038
                                                                        01790038
           IF WS-FI01-FS NOT = "00"                                     01800038
      D      DISPLAY "ERROR OPEN FILE FI01-IN: " WS-FI01-FS             01810038
             PERFORM 9999-ABEND                                         01820038
                THRU 9999-ABEND-END                                     01830038
           ELSE                                                         01840038
      D      DISPLAY "OPEN FI01-IN IS OK"                               01850038
           END-IF.                                                      01860038
                                                                        01870038
      *    first read of the input file                                 01880038
           PERFORM 8100-READ-FI01                                       01890038
              THRU 8100-READ-FI01-END.                                  01900038
                                                                        01910038
           MOVE FI01-DATA-PATIENID TO WS-SAVE-PATIENID.                 01920038
      D    DISPLAY 'Patient ID to update: ' WS-SAVE-PATIENID.           01930038
                                                                        01940038
      *-----------------------*                                         01950038
       1000-INIT-END.                                                   01960038
      *-----------------------*                                         01970038
           EXIT.                                                        01980038
                                                                        01990038
      *----------------------*                                          02000038
       4200-GET-A-PATIENT.                                              02010038
      *----------------------*                                          02020038
                                                                        02030038
           INITIALIZE SEG-IO-AREA,                                      02040038
                                                                        02050038
           CALL 'CBLTDLI' USING WS-DLI-FUNCTION,                        02060038
                                PCB-MASK-GI,                            02070038
                                SEG-IO-AREA,                            02080038
                                QUAL-SSA-PATIENT.                       02090038
                                                                        02100038
           IF STATUS-CODE = '  '                                        02110038
                DISPLAY 'SUCCESSFUL GET: '  SEG-IO-AREA                 02120038
           ELSE                                                         02130038
                DISPLAY 'ERROR IN FETCH :' STATUS-CODE                  02140038
                DISPLAY 'SEG-IO-AREA    :' SEG-IO-AREA                  02150038
                DISPLAY 'DBD-NAME       :'     DBD-NAME                 02160038
                DISPLAY 'SEG-LEVEL      :'    SEG-LEVEL                 02170038
                DISPLAY 'STATUS-CODE    :'   STATUS-CODE                02180038
                DISPLAY 'PROC-OPT       :'    PROC-OPT                  02190038
                DISPLAY 'SEG-NAME       :'    SEG-NAME                  02200038
                DISPLAY 'KEY-FDBK       :'    KEY-FDBK                  02210038
                DISPLAY 'NUM-SENSEG     :'   NUM-SENSEG                 02220038
                DISPLAY 'KEY-FDBK-AREA  :' KEY-FDBK-AREA                02230038
           END-IF.                                                      02240038
                                                                        02250038
      *----------------------*                                          02260038
       4200-GET-A-PATIENT-END.                                          02270038
      *----------------------*                                          02280038
           EXIT.                                                        02290038
                                                                        02300038
      *----------------------*                                          02310038
       4300-GET-TREATMNT.                                               02320038
      *----------------------*                                          02330038
                                                                        02340038
           INITIALIZE SEG-IO-AREA,                                      02350038
                                                                        02360038
           CALL 'CBLTDLI' USING WS-DLI-FUNCTION,                        02370038
                                PCB-MASK-GI,                            02380038
                                SEG-IO-AREA,                            02390038
                                QUAL-SSA-PATIENT,                       02400038
                                UNQUAL-SSA-TREATMNT.                    02410038
                                                                        02420038
           IF STATUS-CODE = '  '                                        02430038
                DISPLAY 'SUCCESSFUL GET: '  SEG-IO-AREA                 02440038
                ADD 1 to WS-NO-OCCURS                                   02441041
           ELSE                                                         02450038
                DISPLAY 'ERROR IN FETCH :' STATUS-CODE                  02460038
                DISPLAY 'SEG-IO-AREA    :' SEG-IO-AREA                  02470038
                DISPLAY 'DBD-NAME       :'     DBD-NAME                 02480038
                DISPLAY 'SEG-LEVEL      :'    SEG-LEVEL                 02490038
                DISPLAY 'STATUS-CODE    :'   STATUS-CODE                02500038
                DISPLAY 'PROC-OPT       :'    PROC-OPT                  02510038
                DISPLAY 'SEG-NAME       :'    SEG-NAME                  02520038
                DISPLAY 'KEY-FDBK       :'    KEY-FDBK                  02530038
                DISPLAY 'NUM-SENSEG     :'   NUM-SENSEG                 02540038
                DISPLAY 'KEY-FDBK-AREA  :' KEY-FDBK-AREA                02550038
           END-IF.                                                      02560038
                                                                        02570038
      *----------------------*                                          02580038
       4300-GET-TREATMNT-END.                                           02590038
      *----------------------*                                          02600038
           EXIT.                                                        02610038
                                                                        02620038
      *-----------------------*                                         02630038
       5000-DELETE-TREATMNT.                                            02640038
      *-----------------------*                                         02650038
                                                                        02660038
           EVALUATE FI01-SEGMENT-TYPE                                   02670038
             WHEN 'TREATMNT'                                            02680038
      D        DISPLAY "WE ARE ON A TREATMNT SEGMENT"                   02690038
      D        DISPLAY "WS-REC-FI01      : " WS-REC-FI01                02700038
      D        DISPLAY "FI01-DATA-TRTNAME: " FI01-DATA-TRTNAME          02710038
      D        DISPLAY "FI01-DATA-DOCTOR : " FI01-DATA-DOCTOR           02720038
             WHEN OTHER                                                 02730038
      D        DISPLAY "UNKNOWN SEGMENT: " FI01-SEGMENT-TYPE            02740038
           END-EVALUATE.                                                02750038
                                                                        02760038
           INITIALIZE WS-TREATMNT-SEG.                                  02770038
           MOVE FI01-DATA-TRTNAME    TO WS-TRTNAME.                     02780038
           MOVE FI01-DATA-DOCTOR     TO WS-DOCTOR.                      02790038
           MOVE WS-TREATMNT-SEG      TO SEG-IO-AREA.                    02800038
           MOVE FI01-DATA-TRTNAME    TO FIELD-VAL OF QUAL-SSA-TREATMNT. 02801039
                                                                        02810038
           CALL 'CBLTDLI' USING DLI-GHU,                                02820039
                                PCB-MASK-GI,                            02830038
                                SEG-IO-AREA,                            02840038
                                QUAL-SSA-PATIENT,                       02850038
                                QUAL-SSA-TREATMNT.                      02860039
                                                                        02870038
           IF STATUS-CODE = '  '                                        02880038
              DISPLAY 'GHU FOR DELETE IS OK: ' SEG-IO-AREA              02890039
           ELSE                                                         02910038
              DISPLAY 'ERROR IN FETCH :' STATUS-CODE                    02920038
              DISPLAY 'SEG-IO-AREA    :' SEG-IO-AREA                    02930038
              DISPLAY 'DBD-NAME       :'       DBD-NAME                 02940038
              DISPLAY 'SEG-LEVEL      :'      SEG-LEVEL                 02950038
              DISPLAY 'STATUS-CODE    :'     STATUS-CODE                02960038
              DISPLAY 'PROC-OPT       :'      PROC-OPT                  02970038
              DISPLAY 'SEG-NAME       :'      SEG-NAME                  02980038
              DISPLAY 'KEY-FDBK       :'      KEY-FDBK                  02990038
              DISPLAY 'NUM-SENSEG     :'     NUM-SENSEG                 03000038
              DISPLAY 'KEY-FDBK-AREA  :' KEY-FDBK-AREA                  03010038
           END-IF.                                                      03020038
                                                                        03030038
           CALL 'CBLTDLI' USING DLI-DLET,                               03031039
                                PCB-MASK-GI,                            03032039
                                SEG-IO-AREA.                            03033039
                                                                        03034039
           IF STATUS-CODE = '  '                                        03035039
              DISPLAY 'DELETE IS OK: ' SEG-IO-AREA                      03036039
           ELSE                                                         03038039
              DISPLAY 'ERROR IN DELETE:' STATUS-CODE                    03039039
              DISPLAY 'SEG-IO-AREA    :' SEG-IO-AREA                    03039139
              DISPLAY 'DBD-NAME       :'       DBD-NAME                 03039239
              DISPLAY 'SEG-LEVEL      :'      SEG-LEVEL                 03039339
              DISPLAY 'STATUS-CODE    :'     STATUS-CODE                03039439
              DISPLAY 'PROC-OPT       :'      PROC-OPT                  03039539
              DISPLAY 'SEG-NAME       :'      SEG-NAME                  03039639
              DISPLAY 'KEY-FDBK       :'      KEY-FDBK                  03039739
              DISPLAY 'NUM-SENSEG     :'     NUM-SENSEG                 03039839
              DISPLAY 'KEY-FDBK-AREA  :' KEY-FDBK-AREA                  03039939
           END-IF.                                                      03040039
                                                                        03040139
           IF NOT WS-FI01-EOF                                           03041038
              PERFORM 8100-READ-FI01                                    03050038
                 THRU 8100-READ-FI01-END                                03060038
           END-IF.                                                      03070038
                                                                        03080038
      *-----------------------*                                         03090038
       5000-DELETE-TREATMNT-END.                                        03100038
      *-----------------------*                                         03110038
           EXIT.                                                        03120038
                                                                        03450038
      *---------------*                                                 03460038
       8100-READ-FI01.                                                  03470038
      *---------------*                                                 03480038
                                                                        03490038
           INITIALIZE WS-REC-FI01.                                      03500038
                                                                        03510038
           READ FI01-IN INTO WS-REC-FI01                                03520038
           END-READ.                                                    03530038
                                                                        03540038
           EVALUATE TRUE                                                03550038
                                                                        03560038
             WHEN WS-FI01-FS = '00'                                     03570038
               ADD 1 TO WS-NO-READ-FI01                                 03580038
      D        DISPLAY "READ FILE OK: " WS-REC-FI01                     03590038
               CONTINUE                                                 03600038
             WHEN WS-FI01-FS = '10'                                     03610038
               SET WS-FI01-EOF TO TRUE                                  03620038
      D        DISPLAY "WS-FI01-END-OF-FILE " WS-FI01-END-OF-FILE       03630038
             WHEN OTHER                                                 03640038
      D        DISPLAY "ERROR READ FILE FI01 !!!: " WS-FI01-FS          03650038
               PERFORM 9999-ABEND                                       03660038
                  THRU 9999-ABEND-END                                   03670038
                                                                        03680038
           END-EVALUATE.                                                03690038
                                                                        03700038
      *-------------------*                                             03710038
       8100-READ-FI01-END.                                              03720038
      *-------------------*                                             03730038
           EXIT.                                                        03740038
      *-------------------*                                             03750042
       9999-ABEND.                                                      03760038
      *-------------------*                                             03770042
      D    DISPLAY "WE ARE IN ABEND".                                   03780038
      *    WE FORCE AN ABEND                                            03790038
      *>      MOVE +40                TO WS-USER-ABEND-CODE             03800038
      *>      CALL 'ILBOABN0'      USING WS-USER-ABEND-CODE             03810038
           GOBACK.                                                      03820038
      *-------------------*                                             03830042
       9999-ABEND-END.                                                  03840038
      *-------------------*                                             03850042
           EXIT.                                                        03860038
                                                                        03870038

