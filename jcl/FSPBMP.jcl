//IBMUSERP JOB ,MSGCLASS=H,CLASS=A,NOTIFY=&SYSUID,REGION=144M,
//   TIME=(10)
//PROCS JCLLIB ORDER=(DFSF10.PROCLIB)
//* Action: Run Test Case...
//*
//RUNNER EXEC PROC=IMSBATCH,MBR=FSPIVA64,PSB=DFSIVP64
//*RUNNER EXEC PROC=IMSBATCH,MBR=FSPIVA66,PSB=FSPIVP99
//STEPLIB  DD DSN=TEST.ZAPPBUIL.LOAD,DISP=SHR
//         DD DSN=DFSF10.SDFSRESL,DISP=SHR
//         DD DSN=DFSF10.PGMLIB,DISP=SHR
//*         DD DISP=SHR,DSN=IBMUSER.DLAYDBGB.LOAD
//*         DD DISP=SHR,DSN=FELF00.SEQAMOD       Debugg
//PROCLIB  DD DSN=DFSF10.PROCLIB,DISP=SHR
//IMS      DD DISP=SHR,DSN=DFSF10.PSBLIB
//         DD DISP=SHR,DSN=DFSF10.DBDLIB
//CEEOPTS  DD  *
  TRAP(OFF),STORAGE(00,NONE,00),
  STACK(4K,4080,ANYWHERE,KEEP,4K,4080)
  NOTEST
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//IMSERR   DD SYSOUT=*
//DFSIVD5I DD *
DIS     PETERSEN
//* ADD     HANSEN    JENS      x12345    2605
//DFSIVD5O DD SYSOUT=*,DCB=(LRECL=121,RECFM=FA)
//
  TRAP(OFF),STORAGE(EE,NONE,00),
