//MATEBFRU JOB MSGLEVEL=(1,1),NOTIFY=&SYSUID                            00001000
//*                                                                     00004207
//*----------------------------------------------------------*          00004307
//* BUT: CHARGEMENT EN MASSE DE LA BASE IMS (INITIAL LOADING)           00004407
//*----------------------------------------------------------*          00004507
//*                                                                     00004607
//PLIB    JCLLIB ORDER=MATE1.IMS.PROCLIB                                00005000
//RUN     EXEC IMSCOBGO,                                                00007500
//             MBR=INITLOAD,                 <= COBOL PROGRAM NAME      00007601
//             LOADLIB=MATEBF.IMS.LOADLIB.MYPROJ,  <= LOAD LIBRARY      00007701
//             PSB=DENTPSB,                        <= PSB NAME          00007801
//             PSBLIB=MATEBF.IMS.PSBLIB.MYPROJ,    <= PSB LIBRARY       00007901
//             DBDLIB=MATEBF.IMS.DBDLIB.MYPROJ     <= DBD LIBRARY       00008001
//*                                                                     00008100
//** FLAT FILES IF ANY  ***********************                         00009000
//GO.INDD    DD DSN=MATEBF.IMS.TEST.DATA.MYPROJ,DISP=SHR                00020002
//*                                                                     00040000
//** IMS DATABASES (VSAM) *********************                         00050000
//GO.DENTET  DD DSN=MATEBF.IMS.KSDS.MYPROJ,DISP=SHR                     00070006
//GO.DENTFLW DD DSN=MATEBF.IMS.ESDS.MYPROJ,DISP=SHR                     00080006
