//MATEBFR2 JOB MSGLEVEL=(1,1),NOTIFY=&SYSUID                            00010000
//*                                                                     00011002
//*******************************                                       00011114
//* COMPILE IMS COBOL PROGRAM                                           00012015
//*******************************                                       00013014
//PLIB    JCLLIB ORDER=MATE1.IMS.PROCLIB                                00014002
//CL      EXEC IMSCOBCL,                                                00015002
//             MBR=PREAD,                         <= COBOL PROGRAM NAME 00016013
//             SRCLIB=MATEBF.IMS.SRCLIB.MYPROJ,    <= COBOL SOURCE      00017011
//             COPYLIB=MATEBF.COPYLIB,             <= COPY BOOK LIBRARY 00018011
//             LOADLIB=MATEBF.IMS.LOADLIB.MYPROJ   <= LOAD LIBRARY      00019011
//*******************************                                       00020014
//* RUN IMS COBOL PROGRAM                                               00030015
//*******************************                                       00040014
//*LIB    JCLLIB ORDER=MATE1.IMS.PROCLIB                                00050002
//RUN     EXEC IMSCOBGO,                                                00070000
//             MBR=PREAD,                        <= COBOL PROGRAM NAME  00080013
//             LOADLIB=MATEBF.IMS.LOADLIB.MYPROJ, <= LOAD LIBRARY       00090011
//             PSB=DENTPSBA,                      <= PSB NAME           00100012
//             PSBLIB=MATEBF.IMS.PSBLIB.MYPROJ,   <= PSB LIBRARY        00110011
//             DBDLIB=MATEBF.IMS.DBDLIB.MYPROJ    <= DBD LIBRARY        00120011
//*                                                                     00130000
//** IMS DATABASES (VSAM) *********************                         00170000
//GO.DENTET  DD DSN=MATEBF.IMS.KSDS.MYPROJ,DISP=SHR                     00180011
//GO.DENTFLW DD DSN=MATEBF.IMS.ESDS.MYPROJ,DISP=SHR                     00190011
//SYSOUT     DD SYSOUT=*                                                00200001
