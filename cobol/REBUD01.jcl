//EBUDRUN JOB (ACCT),'RETIRE',CLASS=A,MSGCLASS=T
//         INCLUDE MEMBER=ELAXF
//RUN EXEC PGM=BZUBCP,COND=(4,LT),REGION=0M,PARM='EBUD0RUN='
//*//RUN      EXEC PGM=EBUD0RUN,COND=(4,LT),REGION=0M
//STEPLIB  DD DSN=ISM330.SBZULLEP,DISP=SHR
//         DD DSN=ISM330.SBZULOAD,DISP=SHR
//         DD DSN=ISM330.SBZURESL,DISP=SHR
//         DD DISP=SHR,DSN=&CEE..SCEERUN       LE
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
//BZUMSG DD SYSOUT=*
//BZUPLAY DD DSN=IBMUSER.ZUNIT.PB.AZUPLAY.TEBUD01,DISP=SHR
//BZUCFG DD *
<?xml version="1.0" encoding="UTF-8"?>
<runner:RunnerConfiguration
 xmlns:runner="http://www.ibm.com/zUnit/3.0.0.0/TestRunner"
 id="30816279-60cf-46d4-bbe9-686e6e410d36">
  <runner:options contOnTestCaseError="false" contOnTestCaseFail="true"
 contOnTestError="false" contOnTestFail="true" fileIOCapture="enable"/>
  <runner:intercept module="EBUD01" stub="false" lengths="94"
 retcode="true" exist="false"/>
  <runner:intercept module="EBUD01" stub="false" csect="EBUD02"
 lengths="21" retcode="false" exist="false"/>
  <runner:intercept module="EBUD02" stub="false" lengths="21"
 retcode="false" exist="false"/>
  <runner:intercept module="EBUD01" stub="false" csect="EBUD03"
 lengths="93" retcode="false" exist="false"/>
  <runner:intercept module="EBUD03" stub="false" lengths="93"
 retcode="false" exist="false"/>
</runner:RunnerConfiguration>

/*
NOTEST
/*