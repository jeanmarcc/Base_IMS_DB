//MATEBFR2 JOB MSGLEVEL=(1,1),NOTIFY=&SYSUID                            00010000
//*                                                                     00010115
//***********************************                                   00011014
//* COMPILE IMS COBOL PROGRAM                                           00012016
//***********************************                                   00013114
//PLIB    JCLLIB ORDER=MATE1.IMS.PROCLIB                                00014002
//CL      EXEC IMSCOBCL,                                                00015002
//             MBR=PCREA,                        <= COBOL PROGRAM NAME  00016010
//             SRCLIB=MATEBF.IMS.SRCLIB.MYPROJ,  <= COBOL SOURCE LIBRARY00017010
//             COPYLIB=MATEBF.COPYLIB,           <= COPY BOOK LIBRARY   00018010
//             LOADLIB=MATEBF.IMS.LOADLIB.MYPROJ <= LOAD LIBRARY        00019010
//***********************************                                   00020014
//* RUN IMS COBOL PROGRAM                                               00030015
//***********************************                                   00040014
//RUN     EXEC IMSCOBGO,                                                00070000
//             MBR=PCREA,                    <= COBOL PROGRAM NAME      00080009
//             LOADLIB=MATEBF.IMS.LOADLIB.MYPROJ, <= LOAD LIBRARY       00090011
//             PSB=DENTPSBA,                      <= PSB NAME           00100011
//             PSBLIB=MATEBF.IMS.PSBLIB.MYPROJ,   <= PSB LIBRARY        00110011
//             DBDLIB=MATEBF.IMS.DBDLIB.MYPROJ    <= DBD LIBRARY        00120011
//*                                                                     00130000
//** IMS DATABASES (VSAM) *********************                         00170000
//GO.DENTET  DD DSN=MATEBF.IMS.KSDS.MYPROJ,DISP=SHR                     00180013
//GO.DENTFLW DD DSN=MATEBF.IMS.ESDS.MYPROJ,DISP=SHR                     00190013
//***********************************                                   00190114
//* INPUT DATA FILE                                                     00190214
//***********************************                                   00190314
//FI01IN     DD DSN=MATEBF.IMS.PCREA.FI01IN.MYPROJ,DISP=SHR             00191012
//SYSOUT     DD SYSOUT=*                                                00200001
