      *------------------------*                                        00013039
       IDENTIFICATION DIVISION.                                         00014036
      *------------------------*                                        00015039
       PROGRAM-ID. PUPDA.                                               00016036
       AUTHOR. JEAN MARC C.
      *----------------------------------------------------------*      00017140
      * Auteur: Jean Marc C.                                            00017242
      *                                                                 00017342
      * But: mettre a jour segement TREATMNT dans IMS DB                00017440
      *      pour un patient defini dans le fichier en entree           00017540
      *                                                                 00017640
      * Fichier entree: contient les donnees du segment patient         00017740
      *      a mettre a jour et les donnees du segment TREATMNT         00017840
      *----------------------------------------------------------*      00017940
      *--------------------*                                            00018036
       ENVIRONMENT DIVISION.                                            00019036
      *--------------------*                                            00020036
       CONFIGURATION SECTION.                                           00030036
       OBJECT-COMPUTER.                                                 00040036
       SOURCE-COMPUTER.                                                 00050036
            IBM-SYSTEM WITH DEBUGGING MODE.                             00060036
       INPUT-OUTPUT SECTION.                                            00070036
       FILE-CONTROL.                                                    00080036
           SELECT FI01-IN ASSIGN TO FI01IN                              00090036
           ORGANIZATION IS  SEQUENTIAL                                  00100036
           FILE STATUS IS WS-FI01-FS.                                   00110036
      *-------------*                                                   00120039
       DATA DIVISION.                                                   00130036
      *-------------*                                                   00140039
       FILE SECTION.                                                    00150036
                                                                        00160036
       FD  FI01-IN RECORDING MODE F.                                    00170036
       01  FI01-IN-DATA                 PIC X(80).                      00180036
                                                                        00190036
      *-----------------------*                                         00200036
       WORKING-STORAGE SECTION.                                         00210036
      *-----------------------*                                         00220036
       01 WS-COUNTERS.                                                  00230036
           05 WS-NO-READ-FI01              PIC 9(8).                    00240036
                                                                        00250036
       01 WS-FILE-STATUS.                                               00260036
          05 WS-FI01-FS                PIC X(2).                        00270036
                                                                        00280036
       01 WS-FI01-END-OF-FILE           PIC X(5) VALUE 'FALSE'.         00290036
          88 WS-FI01-EOF                VALUE 'TRUE'.                   00300036
          88 WS-FI01-NOT-EOF            VALUE 'FALSE'.                  00310036
                                                                        00320036
      * THIS FILE contains records to delete in IMS                     00330036
       01 WS-REC-FI01.                                                  00340036
          05 FI01-SEGMENT-TYPE         PIC X(08).                       00350036
          05 FILLER                    PIC X(01).                       00360036
          05 FI01-DATA                 PIC X(72).                       00370036
          05 FI01-DATA-PATIENT  REDEFINES FI01-DATA.                    00380036
             10 FI01-DATA-PATIENID     PIC X(03).                       00390036
             10 FILLER                 PIC X(69).                       00400036
          05 FI01-DATA-TREATMNT REDEFINES FI01-DATA.                    00410036
             10 FI01-DATA-TRTNAME      PIC X(20).                       00420036
             10 FI01-DATA-DOCTOR       PIC X(20).                       00430036
             10 FILLER                 PIC X(32).                       00440036
                                                                        00450036
       01 WS-SAVE-PATIENID             PIC X(03).                       00460036
                                                                        00470036
       01 QUAL-SSA-PATIENT.                                             00480036
           05  SEGNAME     PIC X(08) VALUE 'PATIENT'.                   00490036
           05  FILLER      PIC X(01) VALUE '('.                         00500036
           05  FIELD       PIC X(08) VALUE 'PATIENID'.                  00510036
           05  OPER        PIC X(02) VALUE 'EQ'.                        00520036
           05  FIELD-VAL   PIC X(03) VALUE SPACE.                       00530036
           05  FILLER      PIC X(01) VALUE ')'.                         00540036
                                                                        00550036
       01 QUAL-SSA-TREATMNT.                                            00560036
           05  SEGNAME     PIC X(08) VALUE 'TREATMNT'.                  00570036
           05  FILLER      PIC X(01) VALUE '('.                         00580036
           05  FIELD-NAME  PIC X(08) VALUE 'TRTNAME'.                   00590036
           05  OPER        PIC X(02) VALUE 'EQ'.                        00600036
           05  FIELD-VAL   PIC X(20) VALUE SPACE.                       00610036
           05  FILLER      PIC X(01) VALUE ')'.                         00620036
                                                                        00630036
       01 UNQUAL-SSA-PATIENT.                                           00640036
           05 SEGMENT-NAME PIC X(8) VALUE 'PATIENT'.                    00650036
           05 FILLER  PIC X VALUE SPACE.                                00660036
                                                                        00670036
       01 UNQUAL-SSA-TREATMNT.                                          00680036
           05 SEGMENT-NAME PIC X(8) VALUE 'TREATMNT'.                   00690036
           05 FILLER PIC X VALUE SPACE.                                 00700036
                                                                        00710036
       01 UNQUAL-SSA-BILLING.                                           00720036
           05 SEGMENT-NAME PIC X(8) VALUE 'BILLING'.                    00730036
           05 FILLER PIC X VALUE SPACE.                                 00740036
                                                                        00750036
       01 DLI-FUNCTIONS.                                                00760036
        05 DLI-GU   PIC X(4) VALUE 'GU '.                               00770036
        05 DLI-GHU  PIC X(4) VALUE 'GHU '.                              00780036
        05 DLI-GN   PIC X(4) VALUE 'GN '.                               00790036
        05 DLI-GHN  PIC X(4) VALUE 'GHN '.                              00800036
        05 DLI-GNP  PIC X(4) VALUE 'GNP '.                              00810036
        05 DLI-GHNP PIC X(4) VALUE 'GHNP'.                              00820036
        05 DLI-ISRT PIC X(4) VALUE 'ISRT'.                              00830036
        05 DLI-DLET PIC X(4) VALUE 'DLET'.                              00840036
        05 DLI-REPL PIC X(4) VALUE 'REPL'.                              00850036
        05 DLI-CHKP PIC X(4) VALUE 'CHKP'.                              00860036
        05 DLI-XRST PIC X(4) VALUE 'XRST'.                              00870036
        05 DLI-PCB  PIC X(4) VALUE 'PCB '.                              00880036
                                                                        00890036
       01 SEG-IO-AREA     PIC X(60).                                    00900036
       01 WS-DLI-FUNCTION PIC X(4).                                     00910036
                                                                        00920036
       01 WS-TREATMNT-SEG.                                              00930036
          05 WS-TRTNAME   PIC X(20).                                    00940036
          05 WS-DOCTOR    PIC X(20).                                    00950036
                                                                        00960036
      *-----------------------*                                         00970036
       LINKAGE SECTION.                                                 00980036
      *-----------------------*                                         00990036
                                                                        01000036
      * psb to get and insert                                           01010036
        01 PCB-MASK-GI.                                                 01020036
           03 DBD-NAME        PIC X(8).                                 01030036
           03 SEG-LEVEL       PIC XX.                                   01040036
           03 STATUS-CODE     PIC XX.                                   01050036
           03 PROC-OPT        PIC X(4).                                 01060036
           03 FILLER          PIC X(4).                                 01070036
           03 SEG-NAME        PIC X(8).                                 01080036
           03 KEY-FDBK        PIC S9(5) COMP.                           01090036
           03 NUM-SENSEG      PIC S9(5) COMP.                           01100036
           03 KEY-FDBK-AREA.                                            01110036
              05 PATIENT-KEY    PIC X(3).                               01120036
              05 MEDICAL-KEY    PIC X(6).                               01130036
              05 DRUG-KEY       PIC X(8).                               01140036
              05 BILLING-KEY    PIC X(8).                               01150036
                                                                        01170036
      *-----------------------*                                         01180036
       PROCEDURE DIVISION.                                              01190036
      *-----------------------*                                         01200036
                                                                        01210036
           INITIALIZE PCB-MASK-GI.                                      01220036
           ENTRY 'DLITCBL' USING PCB-MASK-GI.                           01230036
                                                                        01240036
           DISPLAY '*------------------------*'.                        01250036
           DISPLAY ' *** BEGIN PROG BY JMC ***'.                        01260036
           DISPLAY '*------------------------*'.                        01270036
                                                                        01280036
           PERFORM 1000-INIT                                            01290036
              THRU 1000-INIT-END.                                       01300036
                                                                        01310036
           DISPLAY '1-DBD-NAME      :'    DBD-NAME.                     01320036
           DISPLAY '1-SEG-LEVEL     :'    SEG-LEVEL.                    01330036
           DISPLAY '1-STATUS-CODE   :'    STATUS-CODE.                  01340036
           DISPLAY '1-PROC-OPT      :'    PROC-OPT.                     01350036
           DISPLAY '1-SEG-NAME      :'    SEG-NAME.                     01360036
           DISPLAY '1-KEY-FDBK      :'    KEY-FDBK.                     01370036
           DISPLAY '1-NUM-SENSEG    :'    NUM-SENSEG.                   01380036
           DISPLAY '1-KEY-FDBK-AREA :' KEY-FDBK-AREA.                   01390036
           DISPLAY '*------------------------*'.                        01400036
                                                                        01410036
           DISPLAY '1_GET PATIENT'.                                     01420036
           MOVE DLI-GHU  TO WS-DLI-FUNCTION.                            01430036
           MOVE WS-SAVE-PATIENID TO FIELD-VAL OF QUAL-SSA-PATIENT.      01440036
           PERFORM 4200-GET-A-PATIENT                                   01450036
              THRU 4200-GET-A-PATIENT-END.                              01460036
                                                                        01470036
           DISPLAY '2_GET ALL TREATMNT FOR THIS PATIENT'.               01480038
           MOVE DLI-GHN  TO WS-DLI-FUNCTION.                            01490036
           PERFORM 4300-GET-TREATMNT                                    01500036
              THRU 4300-GET-TREATMNT-END                                01510036
              UNTIL STATUS-CODE NOT = SPACE.                            01520036
                                                                        01530036
      *    then read file again to get first treatmnt                   01540036
           PERFORM 8100-READ-FI01                                       01550036
              THRU 8100-READ-FI01-END.                                  01560036
                                                                        01570036
      *    update segment until end of input file                       01580036
           DISPLAY '3_UPDATE TREATMNT'.                                 01590038
           PERFORM 5000-UPDATE-TREATMNT                                 01600038
              THRU 5000-UPDATE-TREATMNT-END                             01610038
               UNTIL WS-FI01-EOF.                                       01620036
                                                                        01630036
      *    we have to reposition on the patient                         01640036
           DISPLAY '4_GET PATIENT'.                                     01650038
           MOVE DLI-GU  TO WS-DLI-FUNCTION.                             01660036
           MOVE WS-SAVE-PATIENID TO FIELD-VAL OF QUAL-SSA-PATIENT.      01670036
           PERFORM 4200-GET-A-PATIENT                                   01680036
              THRU 4200-GET-A-PATIENT-END.                              01690036
                                                                        01700036
           DISPLAY '5_GET ALL TREATMNT'.                                01710038
           MOVE DLI-GN  TO WS-DLI-FUNCTION.                             01720036
           PERFORM 4300-GET-TREATMNT                                    01730036
              THRU 4300-GET-TREATMNT-END                                01740036
              UNTIL STATUS-CODE NOT = SPACE.                            01750036
                                                                        01760036
           GOBACK.                                                      01770036
                                                                        01780036
      *-------------*                                                   01790039
       1000-INIT.                                                       01800036
      *-------------*                                                   01810039
           DISPLAY "***** INIT PROCESS *****".                          01820036
                                                                        01830036
           MOVE SPACE TO WS-FILE-STATUS.                                01840036
           MOVE ZEROES TO WS-COUNTERS.                                  01850036
                                                                        01860036
           OPEN INPUT  FI01-IN.                                         01870036
                                                                        01880036
           IF WS-FI01-FS NOT = "00"                                     01890036
      D      DISPLAY "ERROR OPEN FILE FI01-IN: " WS-FI01-FS             01900036
             PERFORM 9999-ABEND                                         01910036
                THRU 9999-ABEND-END                                     01920036
           ELSE                                                         01930036
      D      DISPLAY "OPEN FI01-IN IS OK"                               01940036
           END-IF.                                                      01950036
                                                                        01960036
      *    first read of the input file                                 01970036
           PERFORM 8100-READ-FI01                                       01980036
              THRU 8100-READ-FI01-END.                                  01990036
                                                                        02000036
           MOVE FI01-DATA-PATIENID TO WS-SAVE-PATIENID.                 02010036
      D    DISPLAY 'Patient ID to update: ' WS-SAVE-PATIENID.           02020036
                                                                        02030036
      *-----------------*                                               02040039
       1000-INIT-END.                                                   02050036
      *-----------------*                                               02060039
           EXIT.                                                        02070036
                                                                        02080036
      *---------------------*                                           02090039
       4200-GET-A-PATIENT.                                              02100036
      *---------------------*                                           02110039
                                                                        02120036
           INITIALIZE SEG-IO-AREA,                                      02130036
                                                                        02140036
           CALL 'CBLTDLI' USING WS-DLI-FUNCTION,                        02150036
                                PCB-MASK-GI,                            02160036
                                SEG-IO-AREA,                            02170036
                                QUAL-SSA-PATIENT.                       02180036
                                                                        02190036
           IF STATUS-CODE = '  '                                        02200036
                DISPLAY 'SUCCESSFUL GET: '  SEG-IO-AREA                 02210036
           ELSE                                                         02220036
                DISPLAY 'ERROR IN FETCH :' STATUS-CODE                  02230036
                DISPLAY 'SEG-IO-AREA    :' SEG-IO-AREA                  02240036
                DISPLAY 'DBD-NAME       :'     DBD-NAME                 02250036
                DISPLAY 'SEG-LEVEL      :'    SEG-LEVEL                 02260036
                DISPLAY 'STATUS-CODE    :'   STATUS-CODE                02270036
                DISPLAY 'PROC-OPT       :'    PROC-OPT                  02280036
                DISPLAY 'SEG-NAME       :'    SEG-NAME                  02290036
                DISPLAY 'KEY-FDBK       :'    KEY-FDBK                  02300036
                DISPLAY 'NUM-SENSEG     :'   NUM-SENSEG                 02310036
                DISPLAY 'KEY-FDBK-AREA  :' KEY-FDBK-AREA                02320036
           END-IF.                                                      02330036
                                                                        02340036
      *----------------------*                                          02350036
       4200-GET-A-PATIENT-END.                                          02360036
      *----------------------*                                          02370036
           EXIT.                                                        02380036
                                                                        02390036
      *--------------------*                                            02400039
       4300-GET-TREATMNT.                                               02410036
      *--------------------*                                            02420039
                                                                        02430036
           INITIALIZE SEG-IO-AREA,                                      02440036
                                                                        02450036
           CALL 'CBLTDLI' USING WS-DLI-FUNCTION,                        02460036
                                PCB-MASK-GI,                            02470036
                                SEG-IO-AREA,                            02480036
                                QUAL-SSA-PATIENT,                       02490036
                                UNQUAL-SSA-TREATMNT.                    02500036
                                                                        02510036
           IF STATUS-CODE = '  '                                        02520036
                DISPLAY 'SUCCESSFUL GET: '  SEG-IO-AREA                 02530036
           ELSE                                                         02540036
                DISPLAY 'ERROR IN FETCH :' STATUS-CODE                  02550036
                DISPLAY 'SEG-IO-AREA    :' SEG-IO-AREA                  02560036
                DISPLAY 'DBD-NAME       :'     DBD-NAME                 02570036
                DISPLAY 'SEG-LEVEL      :'    SEG-LEVEL                 02580036
                DISPLAY 'STATUS-CODE    :'   STATUS-CODE                02590036
                DISPLAY 'PROC-OPT       :'    PROC-OPT                  02600036
                DISPLAY 'SEG-NAME       :'    SEG-NAME                  02610036
                DISPLAY 'KEY-FDBK       :'    KEY-FDBK                  02620036
                DISPLAY 'NUM-SENSEG     :'   NUM-SENSEG                 02630036
                DISPLAY 'KEY-FDBK-AREA  :' KEY-FDBK-AREA                02640036
           END-IF.                                                      02650036
                                                                        02660036
      *----------------------*                                          02670036
       4300-GET-TREATMNT-END.                                           02680036
      *----------------------*                                          02690036
           EXIT.                                                        02700036
                                                                        02710036
      *----------------------*                                          02720039
       5000-UPDATE-TREATMNT.                                            02730038
      *----------------------*                                          02740039
                                                                        02750036
           EVALUATE FI01-SEGMENT-TYPE                                   02760036
             WHEN "TREATMNT"                                            02770038
      D        DISPLAY "WE ARE ON A TREATMNT SEGMENT"                   02780038
      D        DISPLAY "WS-REC-FI01: " WS-REC-FI01                      02790036
      D        DISPLAY "FI01-DATA-TRTNAME: " FI01-DATA-TRTNAME          02800038
      D        DISPLAY "FI01-DATA-DOCTOR: " FI01-DATA-DOCTOR            02810038
             WHEN OTHER                                                 02820036
      D        DISPLAY "UNKNOWN SEGMENT: " FI01-SEGMENT-TYPE            02830036
           END-EVALUATE.                                                02840036
                                                                        02850036
           MOVE FI01-DATA-TRTNAME    TO FIELD-VAL OF QUAL-SSA-TREATMNT. 02882037
                                                                        02883037
           CALL 'CBLTDLI' USING DLI-GHU,                                02890036
                                PCB-MASK-GI,                            02900036
                                SEG-IO-AREA,                            02910036
                                QUAL-SSA-PATIENT,                       02920037
                                QUAL-SSA-TREATMNT.                      02921037
                                                                        02930036
           IF STATUS-CODE = '  '                                        02940036
              DISPLAY 'GHU TREATMNT is ok'                              02950038
              DISPLAY 'SEG-IO : ' SEG-IO-AREA                           02960036
           ELSE                                                         02970036
              DISPLAY 'ERROR GHU UPDATE:' STATUS-CODE                   02980036
              DISPLAY 'SEG-IO-AREA    :' SEG-IO-AREA                    02990036
              DISPLAY 'DBD-NAME       :'       DBD-NAME                 03000036
              DISPLAY 'SEG-LEVEL      :'      SEG-LEVEL                 03010036
              DISPLAY 'STATUS-CODE    :'     STATUS-CODE                03020036
              DISPLAY 'PROC-OPT       :'      PROC-OPT                  03030036
              DISPLAY 'SEG-NAME       :'      SEG-NAME                  03040036
              DISPLAY 'KEY-FDBK       :'      KEY-FDBK                  03050036
              DISPLAY 'NUM-SENSEG     :'     NUM-SENSEG                 03060036
              DISPLAY 'KEY-FDBK-AREA  :' KEY-FDBK-AREA                  03070036
           END-IF.                                                      03080036
                                                                        03090036
           INITIALIZE WS-TREATMNT-SEG.                                  03100039
           MOVE FI01-DATA-TRTNAME    TO WS-TRTNAME.                     03110039
           MOVE FI01-DATA-DOCTOR     TO WS-DOCTOR.                      03120039
                                                                        03122039
           CALL 'CBLTDLI' USING DLI-REPL,                               03130036
                                PCB-MASK-GI,                            03140036
                                WS-TREATMNT-SEG.                        03150039
                                                                        03160039
           IF STATUS-CODE = '  '                                        03170036
              DISPLAY 'UPDATE IS OK'                                    03180036
              DISPLAY 'SEG-IO : ' SEG-IO-AREA                           03190036
           ELSE                                                         03200036
              DISPLAY 'ERROR UPDATE   :' STATUS-CODE                    03210036
              DISPLAY 'SEG-IO-AREA    :' SEG-IO-AREA                    03220036
              DISPLAY 'DBD-NAME       :'       DBD-NAME                 03230036
              DISPLAY 'SEG-LEVEL      :'      SEG-LEVEL                 03240036
              DISPLAY 'STATUS-CODE    :'     STATUS-CODE                03250036
              DISPLAY 'PROC-OPT       :'      PROC-OPT                  03260036
              DISPLAY 'SEG-NAME       :'      SEG-NAME                  03270036
              DISPLAY 'KEY-FDBK       :'      KEY-FDBK                  03280036
              DISPLAY 'NUM-SENSEG     :'     NUM-SENSEG                 03290036
              DISPLAY 'KEY-FDBK-AREA  :' KEY-FDBK-AREA                  03300036
           END-IF.                                                      03310036
                                                                        03320036
           PERFORM 8100-READ-FI01                                       03330036
              THRU 8100-READ-FI01-END.                                  03340036
                                                                        03350036
      *----------------------*                                          03360039
       5000-UPDATE-TREATMNT-END.                                        03370038
      *----------------------*                                          03380039
           EXIT.                                                        03390036
                                                                        03730036
      *---------------*                                                 03740036
       8100-READ-FI01.                                                  03750036
      *---------------*                                                 03760036
                                                                        03770036
           INITIALIZE WS-REC-FI01.                                      03780036
                                                                        03790036
           READ FI01-IN INTO WS-REC-FI01                                03800036
           END-READ.                                                    03810036
                                                                        03820036
           EVALUATE TRUE                                                03830036
                                                                        03840036
             WHEN WS-FI01-FS = '00'                                     03850036
               ADD 1 TO WS-NO-READ-FI01                                 03860036
      D        DISPLAY "READ FILE OK: " WS-REC-FI01                     03870036
               CONTINUE                                                 03880036
             WHEN WS-FI01-FS = '10'                                     03890036
               SET WS-FI01-EOF TO TRUE                                  03900036
      D        DISPLAY "WS-FI01-END-OF-FILE " WS-FI01-END-OF-FILE       03910036
             WHEN OTHER                                                 03920036
      D        DISPLAY "ERROR READ FILE FI01 !!!: " WS-FI01-FS          03930036
               PERFORM 9999-ABEND                                       03940036
                  THRU 9999-ABEND-END                                   03950036
                                                                        03960036
           END-EVALUATE.                                                03970036
                                                                        03980036
      *-------------------*                                             03990036
       8100-READ-FI01-END.                                              04000036
      *-------------------*                                             04010036
           EXIT.                                                        04020036
      *-------------------                                              04030036
       9999-ABEND.                                                      04040036
      *-------------------                                              04050036
      D    DISPLAY "WE ARE IN ABEND".                                   04060036
      *    WE FORCE AN ABEND                                            04070036
      *>      MOVE +40                TO WS-USER-ABEND-CODE             04080036
      *>      CALL 'ILBOABN0'      USING WS-USER-ABEND-CODE             04090036
           GOBACK.                                                      04100036
      *-------------------                                              04110036
       9999-ABEND-END.                                                  04120036
      *-------------------                                              04130036
           EXIT.                                                        04140036
                                                                        04150036

