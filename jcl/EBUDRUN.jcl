//EBUDRUN JOB (ACCT),'RETIRE',CLASS=A,MSGCLASS=T
//         INCLUDE MEMBER=ELAXF
//RUN      EXEC PGM=EBUD0RUN,COND=(4,LT),REGION=0M
//STEPLIB  DD DISP=SHR,DSN=&CEE..SCEERUN       LE
//         DD DISP=SHR,DSN=&FEL..SFELLOAD      error feedback
//*         DD DISP=SHR,DSN=IBMUSER.DLAYDBGB.LOAD       debugger
//         DD DISP=SHR,DSN=&EQA..SEQAMOD       debugger
//         DD DISP=SHR,DSN=DEV.ZAPPBUIL.LOAD
//SYSPRINT DD SYSOUT=*
//SYSIN DD *
1968
01
01
//CEEOPTS DD *
NOTEST
/*