//MATEBFR2 JOB MSGLEVEL=(1,1),NOTIFY=&SYSUID                            00010000
//*                                                                     00011002
//********************************                                      00011115
//* COMPILE IMS COBOL PROGRAM                                           00012016
//********************************                                      00013015
//PLIB    JCLLIB ORDER=MATE1.IMS.PROCLIB                                00014002
//CL      EXEC IMSCOBCL,                                                00015002
//             MBR=PDELE,                        <= COBOL PROGRAM NAME  00016014
//             SRCLIB=MATEBF.IMS.SRCLIB.MYPROJ,  <= COBOL SOURCE LIBRARY00017010
//             COPYLIB=MATEBF.COPYLIB,           <= COPY BOOK LIBRARY   00018010
//             LOADLIB=MATEBF.IMS.LOADLIB.MYPROJ <= LOAD LIBRARY        00019010
//********************************                                      00020015
//* RUN IMS COBOL PROGRAM                                               00030016
//********************************                                      00040015
//*LIB    JCLLIB ORDER=MATE1.IMS.PROCLIB                                00050002
//RUN     EXEC IMSCOBGO,                                                00070000
//             MBR=PDELE,                    <= COBOL PROGRAM NAME      00080014
//             LOADLIB=MATEBF.IMS.LOADLIB.MYPROJ, <= LOAD LIBRARY       00090011
//             PSB=DENTPSBA,                      <= PSB NAME           00100011
//             PSBLIB=MATEBF.IMS.PSBLIB.MYPROJ,   <= PSB LIBRARY        00110011
//             DBDLIB=MATEBF.IMS.DBDLIB.MYPROJ    <= DBD LIBRARY        00120011
//*                                                                     00130000
//** IMS DATABASES (VSAM) *********************                         00170000
//GO.DENTET  DD DSN=MATEBF.IMS.KSDS.MYPROJ,DISP=SHR                     00180013
//GO.DENTFLW DD DSN=MATEBF.IMS.ESDS.MYPROJ,DISP=SHR                     00190013
//***********************************                                   00190115
//* INPUT DATA FILE                                                     00190215
//***********************************                                   00190315
//FI01IN     DD DSN=MATEBF.IMS.PDELE.FI01IN.MYPROJ,DISP=SHR             00191014
//SYSOUT     DD SYSOUT=*                                                00200001
