//IBMUSERC JOB ,
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(10),REGION=144M,COND=(16,LT)
//*
//STP0000 EXEC PROC=ELAXFCOC,
// CICS=,
// DB2=,
// COMP=,
// PARM.COBOL=('LIB,MAP,TEST')
//COBOL.SYSLIN DD DISP=SHR,
//        DSN=IBMUSER.COBOL.OBJ(IDISCBL1)
//COBOL.SYSLIB DD DISP=SHR,DSN=IBMUSER.COBOL.COPYLIB
//COBOL.SYSXMLSD DD DUMMY
//COBOL.SYSIN DD DISP=SHR,
//        DSN=IBMUSER.COBOL.SOURCE.COBOL(IDISCBL1)
//*
//******* ADDITIONAL JCL FOR COMPILE HERE ******
//LKED EXEC PROC=ELAXFLNK,PARM='REUS=RENT'
//LINK.OBJ0000 DD DISP=SHR,DSN=IBMUSER.COBOL.OBJ(IDISCBL1)
//LINK.SYSLIN DD *
     INCLUDE OBJ0000
/*
//LINK.SYSLMOD   DD  DISP=SHR,DSN=IBMUSER.LOAD(IDISCBL1)
//*
//RUN EXEC PGM=IDISCBL1
//STEPLIB DD DSN=IBMUSER.LOAD,DISP=SHR
//*        DD DISP=SHR,DSN=ISM400.SEQAMOD       Debugg
//SYSPRINT DD SYSOUT=*
//XEEOPTS DD *
   TER(UADUMP)
/*