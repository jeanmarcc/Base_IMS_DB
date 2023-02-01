//MATEBFG JOB MSGLEVEL=(1,1),NOTIFY=&SYSUID                             00001001
//*                                                                     00002000
//**----------------------------------------------------------*         00002102
//** BUT: GENERER DBD                                                   00002202
//**----------------------------------------------------------*         00002702
//*                                                                     00002802
//PLIB    JCLLIB ORDER=MATE1.IMS.PROCLIB <= DONT CHANGE IT              00003000
//DGEN    EXEC DBDGEN,                                                  00007500
//             MEMBER=DENTDBD,                  <= DBD SOURCE MEMBER    00007602
//             SRCLIB=MATEBF.IMS.SRCLIB.MYPROJ, <= DBD SOURCE LIBRARY   00007901
//             DBDLIB=MATEBF.IMS.DBDLIB.MYPROJ  <= DBD LIBRARY          00008001
//*                                                                     00009000
