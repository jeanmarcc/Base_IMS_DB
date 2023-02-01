//MATEBFR2 JOB MSGLEVEL=(1,1),NOTIFY=&SYSUID                            00010011
//*                                                                     00020011
//*******************************                                       00021012
//* COMPILE IMS COBOL PROGRAM                                           00030013
//*******************************                                       00040012
//PLIB    JCLLIB ORDER=MATE1.IMS.PROCLIB                                00050011
//CL      EXEC IMSCOBCL,                                                00060011
//             MBR=PUPDA,                        <= COBOL PROGRAM NAME  00070011
//             SRCLIB=MATEBF.IMS.SRCLIB.MYPROJ,  <= COBOL SOURCE LIBRARY00080011
//             COPYLIB=MATEBF.COPYLIB,           <= COPY BOOK LIBRARY   00090011
//             LOADLIB=MATEBF.IMS.LOADLIB.MYPROJ <= LOAD LIBRARY        00100011
//*******************************                                       00110012
//* RUN IMS COBOL PROGRAM                                               00120013
//*******************************                                       00130012
//*LIB    JCLLIB ORDER=MATE1.IMS.PROCLIB                                00140011
//RUN     EXEC IMSCOBGO,                                                00150011
//             MBR=PUPDA,                    <= COBOL PROGRAM NAME      00160011
//             LOADLIB=MATEBF.IMS.LOADLIB.MYPROJ, <= LOAD LIBRARY       00170011
//             PSB=DENTPSBA,                      <= PSB NAME           00180011
//             PSBLIB=MATEBF.IMS.PSBLIB.MYPROJ,   <= PSB LIBRARY        00190011
//             DBDLIB=MATEBF.IMS.DBDLIB.MYPROJ    <= DBD LIBRARY        00200011
//*                                                                     00210011
//** IMS DATABASES (VSAM) *********************                         00220011
//GO.DENTET  DD DSN=MATEBF.IMS.KSDS.MYPROJ,DISP=SHR                     00230011
//GO.DENTFLW DD DSN=MATEBF.IMS.ESDS.MYPROJ,DISP=SHR                     00240011
//***********************************                                   00241012
//* INPUT DATA FILE                                                     00242012
//***********************************                                   00243012
//FI01IN     DD DSN=MATEBF.IMS.PUPDA.FI01IN.MYPROJ,DISP=SHR             00250011
//SYSOUT     DD SYSOUT=*                                                00260011
