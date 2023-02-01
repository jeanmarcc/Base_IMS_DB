       ID DIVISION.                                                     00011800
       PROGRAM-ID. INSRT.                                               00011900
       AUTHOR. JEAN MARC C.
      *----------------------------------------------------------*      00012002
      * Auteur: Jean Marc C.                                            00012103
      *                                                                 00012203
      * But: Charger en masse la base ims db a partir d'un fichier      00012303
      *      en entree (Initial Loading)                                00012403
      *                                                                 00012503
      * Fichier Entree: contient tous les segments a inserer            00012603
      *      dans la base IMS DB                                        00012703
      *----------------------------------------------------------*      00012803
       ENVIRONMENT DIVISION.                                            00012903
       INPUT-OUTPUT SECTION.                                            00013003
       FILE-CONTROL.                                                    00013103
           SELECT INFILE ASSIGN TO INDD.                                00013203
       DATA DIVISION.                                                   00013303
       FILE SECTION.                                                    00013403
       FD INFILE.                                                       00013503
       01 INSRT-REC.                                                    00013603
          05 SSA         PIC X(09).                                     00013703
          05 SEG-IO-AREA PIC X(61).                                     00013803
       WORKING-STORAGE SECTION.                                         00013903
        01 WS-EOF           PIC X VALUE 'N'.                            00014003
           88 EOF  VALUE 'Y'.                                           00014103
        01 SW-EOF           PIC X.                                      00014203
           88 FILE-END      VALUE 'Y'.                                  00014303
           88 FILE-OPEN     VALUE 'N'.                                  00014403
       01 DLI-FUNCN  PIC X(4) VALUE 'ISRT'.                             00014503
       LINKAGE SECTION.                                                 00014603
        01 PCB-MASK.                                                    00014703
           03 DBD-NAME        PIC X(8).                                 00014803
           03 SEG-LEVEL       PIC XX.                                   00014903
           03 STATUS-CODE     PIC XX.                                   00015003
           03 PROC-OPT        PIC X(4).                                 00015103
           03 FILLER          PIC X(4).                                 00015203
           03 SEG-NAME        PIC X(8).                                 00015303
           03 KEY-FDBK        PIC S9(5) COMP.                           00015403
           03 NUM-SENSEG      PIC S9(5) COMP.                           00015503
           03 KEY-FDBK-AREA.                                            00015603
              05 PATIENID-KEY PIC X(3).                                 00015703
              05 MEDICID-KEY  PIC X(6).                                 00015803
              05 DRUGID-KEY   PIC X(8).                                 00015903
       PROCEDURE DIVISION.                                              00016003
                 INITIALIZE PCB-MASK                                    00016103
             ENTRY 'DLITCBL' USING PCB-MASK.                            00016203
                     DISPLAY 'ERROR IN INSERT :' STATUS-CODE            00016303
                     DISPLAY 'DBD-NAME :'     DBD-NAME                  00016403
                     DISPLAY 'SEG-LEVEL:'     SEG-LEVEL                 00016503
                     DISPLAY 'STATUS-CODE:'   STATUS-CODE               00016603
                     DISPLAY 'PROC-OPT :'     PROC-OPT                  00016703
                     DISPLAY 'SEG-NAME :'     SEG-NAME                  00016803
                     DISPLAY 'KEY-FDBK :'     KEY-FDBK                  00016903
                     DISPLAY 'NUM-SENSEG:'    NUM-SENSEG                00017003
                     DISPLAY 'KEY-FDBK-AREA:' KEY-FDBK-AREA             00017103
             DISPLAY '*****PROGRAM START*******'.                       00017203
             OPEN INPUT INFILE.                                         00017303
             DISPLAY 'OPEN'.                                            00017403
             PERFORM 1000-READ-FILE UNTIL WS-EOF = 'Y'                  00017503
             GOBACK.                                                    00017603
       1000-READ-FILE.                                                  00017703
               DISPLAY 'READ'.                                          00017803
               READ INFILE AT END MOVE 'Y' TO WS-EOF                    00017903
               NOT AT END                                               00018003
               IF WS-EOF = 'N'                                          00018103
                  CALL 'CBLTDLI' USING ,                                00018203
                                      DLI-FUNCN,                        00018303
                                      PCB-MASK,                         00018403
                                      SEG-IO-AREA,                      00018503
                                      SSA                               00018603
                 DISPLAY 'DLIFN :' DLI-FUNCN                            00018703
                 DISPLAY 'SSA   :' SSA                                  00018803
                 DISPLAY 'PCB-MASK :' PCB-MASK                          00018903
                  IF STATUS-CODE = '  '                                 00019003
                     DISPLAY 'SUCCESSFUL INSRT-REC:' SEG-IO-AREA        00019103
                  ELSE                                                  00019203
                     DISPLAY 'SEG-IO-AREA     :' SEG-IO-AREA            00019303
                     DISPLAY 'ERROR IN INSERT1:' STATUS-CODE            00019403
                     DISPLAY 'DBD-NAME1:'     DBD-NAME                  00019503
                     DISPLAY 'SEG-LEVEL1:'    SEG-LEVEL                 00019603
                     DISPLAY 'STATUS-CODE:'   STATUS-CODE               00019703
                     DISPLAY 'PROC-OPT1 :'    PROC-OPT                  00019803
                     DISPLAY 'SEG-NAME1 :'    SEG-NAME                  00019903
                     DISPLAY 'KEY-FDBK1 :'    KEY-FDBK                  00020003
                     DISPLAY 'NUM-SENSEG1:'   NUM-SENSEG                00020103
                     DISPLAY 'KEY-FDBK-AREA1:' KEY-FDBK-AREA            00020203
                  END-IF                                                00020303
               ELSE                                                     00020403
                  MOVE 'Y'  TO WS-EOF                                   00020503
                  PERFORM 3000-CLOSE-PARA                               00020603
               END-IF.                                                  00020703
       3000-CLOSE-PARA.                                                 00020803
               CLOSE INFILE.                                            00020903
               GOBACK.                                                  00021003

