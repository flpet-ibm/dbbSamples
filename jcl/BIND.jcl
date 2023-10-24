//ZZPKBIND JOB 241901,'BIND PROGRAMS',NOTIFY=&SYSUID,CLASS=A,MSGCLASS=H
//*
//* LICENSED MATERIALS - PROPERTY OF IBM
//*
//* "RESTRICTED MATERIALS OF IBM"
//*
//* CB12
//*
//* (C) COPYRIGHT IBM CORP. 2011, 2013 ALL RIGHTS RESERVED
//*
//*  US GOVERNMENT USERS RESTRICTED RIGHTS - USE, DUPLICATION,
//*  OR DISCLOSURE RESTRICTED BY GSA ADP SCHEDULE
//*  CONTRACT WITH IBM CORPORATION
//*
//BIND    EXEC PGM=IKJEFT01,DYNAMNBR=20
//STEPLIB  DD  DSN=DSNC10.SDSNLOAD,DISP=SHR
//DBRMLIB  DD  DSN=IBMUSER.UBUILV15.DBRM,DISP=SHR
//         DD  DSN=DEV.ZAPPBV15.DBRM,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*
//SYSIN  DD *
/*
//SYSTSIN DD *
DSN SYSTEM(DBCG)
BIND PACKAGE (GENASA1)                                    -
     ISO(CS)                                              -
     CURRENTDATA(NO)                                      -
     MEMBER(ZZ5CHD02)                                     -
     DEGREE(1)                                            -
     DYNAMICRULES(BIND)                                   -
     ACTION (REPLACE)                                     -
     EXPLAIN(NO)                                          -
     OWNER(IBMUSER)                                       -
     QUALIFIER(GENASA1)                                   -
     ENABLE(BATCH,CICS)                                   -
     REL(DEALLOCATE)                                      -
     VALIDATE(BIND)

BIND PACKAGE (GENASA1)                                    -
     ISO(CS)                                              -
     CURRENTDATA(NO)                                      -
     MEMBER(ZZ5CHD03)                                     -
     DEGREE(1)                                            -
     DYNAMICRULES(BIND)                                   -
     ACTION (REPLACE)                                     -
     EXPLAIN(NO)                                          -
     OWNER(IBMUSER)                                       -
     QUALIFIER(GENASA1)                                   -
     ENABLE(BATCH,CICS)                                   -
     REL(DEALLOCATE)                                      -
     VALIDATE(BIND)

BIND PACKAGE (GENASA1)                                    -
     ISO(CS)                                              -
     CURRENTDATA(NO)                                      -
     MEMBER(ZZ5CHD04)                                     -
     DEGREE(1)                                            -
     DYNAMICRULES(BIND)                                   -
     ACTION (REPLACE)                                     -
     EXPLAIN(NO)                                          -
     OWNER(IBMUSER)                                       -
     QUALIFIER(GENASA1)                                   -
     ENABLE(BATCH,CICS)                                   -
     REL(DEALLOCATE)                                      -
     VALIDATE(BIND)

BIND PACKAGE (GENASA1)                                    -
     ISO(CS)                                              -
     CURRENTDATA(NO)                                      -
     MEMBER(ZZ5CHD05)                                     -
     DEGREE(1)                                            -
     DYNAMICRULES(BIND)                                   -
     ACTION (REPLACE)                                     -
     EXPLAIN(NO)                                          -
     OWNER(IBMUSER)                                       -
     QUALIFIER(GENASA1)                                   -
     ENABLE(BATCH,CICS)                                   -
     REL(DEALLOCATE)                                      -
     VALIDATE(BIND)

BIND PACKAGE (GENASA1)                                    -
     ISO(CS)                                              -
     CURRENTDATA(NO)                                      -
     MEMBER(ZZ5CHD02)                                     -
     DEGREE(1)                                            -
     DYNAMICRULES(BIND)                                   -
     ACTION (REPLACE)                                     -
     EXPLAIN(NO)                                          -
     OWNER(IBMUSER)                                       -
     QUALIFIER(GENASA1)                                   -
     ENABLE(BATCH,CICS)                                   -
     REL(DEALLOCATE)                                      -
     VALIDATE(BIND)

BIND PACKAGE (GENASA1)                                    -
     ISO(CS)                                              -
     CURRENTDATA(NO)                                      -
     MEMBER(ZZ5CHD02)                                     -
     DEGREE(1)                                            -
     DYNAMICRULES(BIND)                                   -
     ACTION (REPLACE)                                     -
     EXPLAIN(NO)                                          -
     OWNER(IBMUSER)                                       -
     QUALIFIER(GENASA1)                                   -
     ENABLE(BATCH,CICS)                                   -
     REL(DEALLOCATE)                                      -
     VALIDATE(BIND)


/*