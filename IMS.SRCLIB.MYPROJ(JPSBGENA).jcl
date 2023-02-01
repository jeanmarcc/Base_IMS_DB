//MATEBFP2 JOB MSGLEVEL=(1,1),NOTIFY=&SYSUID                            00001000
//*                                                                     00002104
//*----------------------------------------------------------*          00002204
//* BUT: GENERATION PSB                                                 00002304
//*----------------------------------------------------------*          00002404
//*                                                                     00002504
//PLIB    JCLLIB ORDER=MATE1.IMS.PROCLIB                                00003000
//PGEN    EXEC PSBGEN,                                                  00005000
//             MEMBER=DENTPSBA,                        <= PSB SOURCE    00006003
//             SRCLIB=MATEBF.IMS.SRCLIB.MYPROJ,        <= PSB SOURCE    00007003
//             PSBLIB=MATEBF.IMS.PSBLIB.MYPROJ         <= PSB LIBRARY   00008003
//*                                                                     00009000
