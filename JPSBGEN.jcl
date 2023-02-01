//MATEBFD JOB MSGLEVEL=(1,1),NOTIFY=&SYSUID                             00001000
//*                                                                     00002000
//*----------------------------------------------------------*          00002202
//* BUT: GENERATION PSB                                                 00002302
//*----------------------------------------------------------*          00002402
//*                                                                     00002502
//PLIB    JCLLIB ORDER=MATE1.IMS.PROCLIB                                00003000
//PGEN    EXEC PSBGEN,                                                  00007500
//             MEMBER=DENTPSB,               <= PSB SOURCE MEMBER       00007600
//             SRCLIB=MATEBF.IMS.SRCLIB.MYPROJ,   <= PSB SOURCE LIBRARY 00007901
//             PSBLIB=MATEBF.IMS.PSBLIB.MYPROJ    <= PSB LIBRARY        00008001
//*                                                                     00009000
