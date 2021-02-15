//IS198CPY JOB (PYGM-TEST-001),'PYGMENTS TEST JOB',
//  CLASS=L,MSGCLASS=X,TIME=(00,10)
//* Copy 'OLDFILE' to 'NEWFILE'.
//COPY01   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=OLDFILE,DISP=SHR
//SYSUT2   DD DSN=NEWFILE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(40,5),RLSE),      Some comment
//            DCB=(LRECL=115,BLKSIZE=1150)
//SYSIN    DD DUMMY
/*
//* Test line continuation in strings.
//CONT01   EXEC PGM=IEFBR14,PARM='THIS IS A LONG PARAMETER WITHIN APOST
//           ROPHES, CONTINUED IN COLUMN 15 OF THE NEXT RECORD'
//* Sort a couple of lines and show the result in the job log.
//SORT01   EXEC PGM=IEFBR14
//SORTIN   DD *
spam
eggs
ham
/*
//SORTOUT  DD SYSOUT=*
/*
//* Test line continuation with comment at end of line continued by a
//* character at column 72 (in this case 'X').
//STP4     EXEC PROC=BILLING,COND.PAID=((20,LT),EVEN),
//         COND.LATE=(60,GT,FIND),
//         COND.BILL=((20,GE),(30,LT,CHGE))  THIS STATEMENT CALLS THE X
//         BILLING PROCEDURE AND SPECIFIES RETURN CODE TESTS FOR THREEX
//         PROCEDURE STEPS.
