//MATEBFD JOB MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*
//*----------------------------------------------------------*
//* BUT: GENERER FICHIERS VSAM QUI VONT CONTENIR LA BASE IMS
//*----------------------------------------------------------*
//*
//VKDEF    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD  *
  DELETE MATEBF.IMS.KSDS.MYPROJ CLUSTER PURGE
  SET MAXCC = 0
  DEFINE CLUSTER(NAME(MATEBF.IMS.KSDS.MYPROJ) -
                 INDEXED KEYS (3,6)              -
                 RECORDSIZE(72,72)               -
                 TRACKS(15,15)                   -
                 CISZ(2048)                      -
                 VOLUMES(DEVHD4)                 -
                )                                -
         DATA(NAME(MATEBF.IMS.KSDS.MYPROJ.DATA)) -
         INDEX (NAME(MATEBF.IMS.KSDS.MYPROJ.INDEX))
/*
//*
//************************************************************
//* INITIALIZE THE VSAM FILE TO PLACE EOF MARK
//************************************************************
//VKINIT   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INF      DD  DUMMY
//OUTF     DD  DSN=MATEBF.IMS.KSDS.MYPROJ,DISP=SHR
//SYSIN    DD  *
  REPRO INFILE(INF) OUTFILE(OUTF)
/*
//************************************************************
//* DEFINE VSAM ESDS CLUSTER FOR HISAM OVERFLOW DATA SET
//************************************************************
//VODEF    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSIN    DD  *
  DELETE MATEBF.IMS.ESDS.MYPROJ CLUSTER PURGE
  SET MAXCC = 0
  DEFINE CLUSTER(NAME(MATEBF.IMS.ESDS.MYPROJ) -
                 NONINDEXED                      -
                 RECORDSIZE(72,72)               -
                 TRACKS(15,15)                   -
                 CISZ(2048)                      -
                 VOLUMES(DEVHD4)                 -
                )                                -
         DATA(NAME(MATEBF.IMS.ESDS.MYPROJ.DATA))
/*
