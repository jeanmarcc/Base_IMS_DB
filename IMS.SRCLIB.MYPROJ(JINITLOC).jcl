//MATEBFD JOB MSGLEVEL=(1,1),NOTIFY=&SYSUID                             00001000
//*----------------------------------------------------------*          00004202
//* BUT: COMPILATION PROGRAMME                                          00004302
//*----------------------------------------------------------*          00004402
//*                                                                     00004502
//PLIB    JCLLIB ORDER=MATE1.IMS.PROCLIB                                00005000
//CL      EXEC IMSCOBCL,                                                00007500
//             MBR=INITLOAD,                     <= COBOL PROGRAM NAME  00007600
//             SRCLIB=MATEBF.IMS.SRCLIB.MYPROJ,  <= COBOL SOURCE LIBRARY00007800
//             COPYLIB=MATEBF.COPYLIB,           <= COPY BOOK LIBRARY   00007900
//             LOADLIB=MATEBF.IMS.LOADLIB.MYPROJ <=LOAD LIBRARY         00008000
