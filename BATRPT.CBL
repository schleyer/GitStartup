      *UNIconX Ver:UNIVER 02.00.04 20100714 Date:10-08-14 Time:12:21:54
       $SET SOURCEFORMAT"FIXED", DEFAULTBYTE(32), SPZERO
 
       IDENTIFICATION DIVISION.
 
       PROGRAM-ID. BATRPT.
 
       DATE-WRITTEN.  November, 1988.  Version 5.0.
 
           THIS PROGRAM LOCATES THE NEXT SPECIFICATION TO PROCESS AND
           THEN CALLS THE APPROPRIATE PRINTING PROGRAM.
 
           TRADE SECRETS NOTICE: ALL RIGHTS RESERVED.  This material
           contains the valuable properties and trade secrets of
           RealWorld Corporation ("RWC"), a New Hampshire corporation,
           embodying substantial creative efforts and confidential
           information, ideas and expressions, no part of which may
           be reproduced or transmitted in any form or by any means,
           electronic, mechanical, or otherwise, including photographic
           and recording, or in connection with any information storage
           or retrieval system without written permission from RWC.
 
           COPYRIGHT NOTICE: COPYRIGHT (C) General Ledger, 1980, 1982,
           1983, 1984, 1986, 1988 by RealWorld Corporation.  All rights
           reserved. An unpublished work.
 
       ENVIRONMENT DIVISION.
 
       CONFIGURATION SECTION.
 
       SOURCE-COMPUTER. VAX-11.
       OBJECT-COMPUTER. VAX-11.
 
       INPUT-OUTPUT SECTION.
 
       FILE-CONTROL.
 
 
 
       DATA DIVISION.
 
       FILE SECTION.
 
       WORKING-STORAGE SECTION.
       COPY "WSUCTDBIOMF.COB".
       COPY "COMPFIFD.COB".
 
 
       COPY "SABATFSFD.COB".
 
 
      / *** WORKING STORAGE SECTION ***
 
 
       01  BATCH-JOB                           PIC X EXTERNAL.
       01 PGM-ID.
           03 SYS-ID                           PIC X(2) VALUE "SA".
           03 PGM-NAM                          PIC X(6) VALUE "BATRPT".
           03 CALLED-THRU-NAM                  PIC X(8) VALUE SPACES.
 
       01 SCRN-ID.
           03 SCRN-COMP-ID                     PIC X(2).
           03 SCRN-SYS-ID                      PIC X(2) VALUE "SA".
           03 SCRN-PGM-NAM                     PIC X(6) VALUE "BATRPT".
 
       01 DAYS-IN-MONTH                        PIC 9(2).
       01 LEAP-YR-DIVIDED-NO                   PIC 9(2)V9999.
       01 LEAP-YR-DIVIDED REDEFINES LEAP-YR-DIVIDED-NO.
           03 FILLER                           PIC 9(2).
           03 LEAP-YR-DIVIDED-NO-DEC           PIC 9999.
 
       01 TEMP-SYS-DAT                         PIC 9(8).
 
       01 MON-END-DAYS                         PIC X(24) VALUE
              "312831303130313130313031".
 
       01 MON-END-TABLE REDEFINES MON-END-DAYS.
           03 MON-END-DAY OCCURS 12 TIMES PIC 99.
 
       01 PGM-NAMS.
           03 FILLER                           PIC X(36) VALUE
              "ORDERSOrders by customer".
           03 FILLER                           PIC X(36) VALUE
              "ORDERAOrders by area".
           03 FILLER                           PIC X(36) VALUE
              "ORDERCOrders by category".
           03 FILLER                           PIC X(36) VALUE
              "ORDERDOrders by department".
           03 FILLER                           PIC X(36) VALUE
              "SALESSInvoices by customer".
           03 FILLER                           PIC X(36) VALUE
              "SALESAInvoices by area".
           03 FILLER                           PIC X(36) VALUE
              "SALESCInvoices by category".
           03 FILLER                           PIC X(36) VALUE
              "SALESDInvoices by department".
           03 FILLER                           PIC X(36) VALUE
              "ORDITMItems ordered for a customer".
           03 FILLER                           PIC X(36) VALUE
              "CUSITMItems invoiced for a customer".
           03 FILLER                           PIC X(36) VALUE
              "ITMORDCustomers ordered for an item".
           03 FILLER                           PIC X(36) VALUE
              "ITMCUSCustomers invoiced for an item".
           03 FILLER                           PIC X(36) VALUE
              "CUHRPTInvoice history by customer".
           03 FILLER                           PIC X(36) VALUE
              "ORDEDTOrder edit list".
           03 FILLER                           PIC X(36) VALUE
              "SHPDATShip date list".
           03 FILLER                           PIC X(36) VALUE
              "QPARMSQuote report".
           03 FILLER                           PIC X(36) VALUE
              "FOLSHPFollow-up past due shipments".
           03 FILLER                           PIC X(36) VALUE
              "PURADVPurchase advice".
           03 FILLER                           PIC X(36) VALUE
              "VALRPTValuation report".
           03 FILLER                           PIC X(36) VALUE
              "INVRPTInvoice history list".
           03 FILLER                           PIC X(36) VALUE
              "COLSLSCollections report".
           03 FILLER                           PIC X(36) VALUE
              "SHPERLOrders shipped early".
           03 FILLER                           PIC X(36) VALUE
              "STKWTHStock withdrawals".
           03 FILLER                           PIC X(36) VALUE
              "LICRPTLicense due report".
 
       01 PGM-NAMS-TABLE REDEFINES PGM-NAMS.
           03 PGM-FIL-ENTRY OCCURS 24 TIMES.
               05 PGM-FIL-NAM                  PIC X(6).
               05 PGM-FIL-TITLE                PIC X(30).
 
       01 PERIOD-DAT.
           03 PRD-START-DAT.
               05 PRD-START-YR                 PIC 9(4).
               05 PRD-START-MON                PIC 9(2).
               05 PRD-START-DAY                PIC 9(2).
 
           03 PRD-END-DAT.
               05 PRD-END-YR                   PIC 9(4).
               05 PRD-END-MON                  PIC 9(2).
               05 PRD-END-DAY                  PIC 9(2).
 
      *01 WK-BASE                              PIC 9(6) VALUE 34493.
      *   THIS REPRESENTS SATURDAY, JULY 2 1994; USE THIS FOR WEEK
      *   RANGING FROM SUNDAY TO SATURDAY (NOT USED - SEE BELOW).
 
       01 WK-BASE                              PIC 9(6) VALUE 727993.
      *   THIS REPRESENTS SUNDAY, JULY 3 1994; USE THIS FOR WEEK
      *   RANGING FROM MONDAY TO SUNDAY.  THE VALUE SHOULD BE 34494,
      *   BUT HAS BEEN ADJUSTED FOR LEAP YEAR 1996 TO BE 34493.
      *   ADDED 727810 FOR Y2K
       01 WK-LST-DAYS                          PIC 9(6).
       01 WK-SYS-DAYS                          PIC 9(6).
       01 WK-SYS-NO-OF-NEW-DAYS                PIC S9(6).
 
       01 WK-DIVIDED-NO                        PIC 9(4)V9999.
       01 WK-DIVIDED REDEFINES WK-DIVIDED-NO.
           03 FILLER                           PIC 9(4).
           03 WK-DIVIDED-NO-DEC                PIC V9999.
 
       01 SAV-MENU-SEL                         PIC X(1).
 
       01 VAL-SPEC-FND-FLG                     PIC X(1).
           88 VAL-SPEC-FND                     VALUE "Y".
 
           COPY "DATEWS.COB".
 
 
           COPY "COMPFIWS.COB".
 
 
           COPY "MLTCMPWS.COB".
 
 
           COPY "SABATFSWS.COB".
 
 
           COPY "FILERRWS.COB".
 
 
           COPY "FILESWS.COB".
 
 
           COPY "GETPTHWS.COB".
 
 
           COPY "MONTHSWS.COB".
 
 
           COPY "NEWDATWS.COB".
 
 
           COPY "SCREENWS.COB".
 
 
           COPY "SYSPASWS.COB".
 
 
           03 PASS-DATA.
               05 FILLER                       PIC X(599).
               05 PASS-BATCH-FLG               PIC X(1).
                   88 BATCH-PROCESS            VALUE "Y".
 
       01 PASS-BATCH-CTL-VALUS.
 
           03 PASS-PREV-BATCH-ALT-KEY.
               05 PASS-PREV-BATCH-PGM-NO       PIC 9(2).
               05 PASS-PREV-BATCH-FREQ         PIC X(1).
               05 PASS-PREV-BATCH-PERIODS      PIC X(64).
               05 PASS-PREV-ALT-SPEC-NO        PIC 9(3).
 
           03 PASS-BATCH-PRTR-SEL              PIC 9(2).
           03 PASS-VAL-BATCH-FLG               PIC X(1).
 
       01 PASS-AGING-FLDS.
           03 CUTOFF-DAT                       PIC 9(8).
           03 AGING-METH                       PIC X(1).
           03 CUST-ORD                         PIC 9(1).
           03 START-CUST-NAM                   PIC X(25).
           03 START-CUST-NAM-DSPLY-FLG         PIC X(1).
           03 START-CUST-NO                    PIC X(6).
           03 END-CUST-NAM                     PIC X(25).
           03 END-CUST-NAM-DSPLY-FLG           PIC X(1).
           03 END-CUST-NO                      PIC X(6).
           03 LST-PMT-CUTOFF-DAT               PIC 9(8).
           03 GRP-BY-SLS-REP-SEL               PIC X(1).
           03 SLS-REP                          PIC X(3).
           03 CUTOFF-BAL-DUE                   PIC S9(10).
           03 CUTOFF-AGING-PRD                 PIC 9(1).
           03 ALL-PRDS-SEL-FLG                 PIC X(1).
           03 PRT-DR-OR-CR-ACCTS-SEL           PIC X(1).
           03 SHOW-ZERO-BAL-CUSTS-SEL          PIC X(1).
           03 RPT-TYP                          PIC X(1).
           03 AGE-OPN-ITEM-METH                PIC X(1).
           03 SHOW-INVC-BAL-ONLY-SEL           PIC X(1).
           03 PRT-PO-NO-OR-REF-SEL             PIC X(1).
           03 AGING-PRDS-BY-SEL                PIC X(1).
 
           03 AGING-PRDS.
               05 AGING-DAYS-PRD-1             PIC 9(3).
               05 DESC-PRD-1-LIN-1             PIC X(13).
               05 DESC-PRD-1-LIN-2             PIC X(13).
               05 PRD-1-CUTOFF-DAT             PIC 9(8).
 
               05 AGING-DAYS-PRD-2             PIC 9(3).
               05 DESC-PRD-2-LIN-1             PIC X(13).
               05 DESC-PRD-2-LIN-2             PIC X(13).
               05 PRD-2-CUTOFF-DAT             PIC 9(8).
 
               05 AGING-DAYS-PRD-3             PIC 9(3).
               05 DESC-PRD-3-LIN-1             PIC X(13).
               05 DESC-PRD-3-LIN-2             PIC X(13).
               05 PRD-3-CUTOFF-DAT             PIC 9(8).
 
               05 AGING-DAYS-PRD-4             PIC 9(3).
               05 DESC-PRD-4-LIN-1             PIC X(13).
               05 DESC-PRD-4-LIN-2             PIC X(13).
               05 PRD-4-CUTOFF-DAT             PIC 9(8).
 
      / *** PROCEDURE DIVISION ***
       01 LOG-MESSAGE                          PIC X(80).
       01 WRITELOG          PIC X(8) VALUE "WRITELOG".
       PROCEDURE DIVISION.
 
       DECLARATIVES.
 
           COPY "FILERRDS.COB".
 
 
       END DECLARATIVES.
 
      / *** LEVEL 1 - PROGRAM MAIN LOGIC ***
 
       LEVEL-1 SECTION.
 
       COPY "PDUCTSTART.COB".
       MAIN-LOGIC.
 
 
           INITIALIZE SYS-PASS-VALUS.
           MOVE "N" TO WAIT-03-FLG.
           MOVE DSPLY-OFF TO PASS-DSPLY-MOD.
           PERFORM SET-DSPLY-ON.
           MOVE "Y" TO PASS-BATCH-FLG
                       BATCH-JOB.
           PERFORM DSPLY-STARTING-BATCH.
 
           DISPLAY " "
                   UPON CONSOLE.
           DISPLAY "I'M IN"
                   UPON CONSOLE.
           DISPLAY "Today's date", "  ", PASS-SYS-DAT
                   UPON CONSOLE.
 
           PERFORM BEG-START-UP.
 
           MOVE "Y" TO RUNNING-MULTI-COMP-FLG,
                      PASS-MULTI-COMP-FLG.
           MOVE "00" TO PASS-COMP-ID.
 
           PERFORM GET-COMP-REC.
 
      *    IF PASS-ACCESS-ACTIV = "LOAD"
      *        MOVE PGM-NAM TO PASS-ACCESS-LOAD-PGM-NAM,
      *    ELSE
      *        MOVE CALLED-THRU-NAM TO PASS-ACCESS-CALLED-THRU-NAM,
      *        MOVE PGM-NAM TO PASS-ACCESS-CURR-PGM-NAM.
 
           IF COMP-FIL-NOT-FND
               PERFORM COMP-FIL-NOT-FND-MSG,
               GO TO MAIN-LOGIC-EXIT,
           ELSE
               PERFORM SET-DSPLY-ON,
               PERFORM TURN-OFF-AUTO-FUNC-KEYS.
 
           IF NOT FILS-ARE-OPN
               PERFORM OPENING-PROC,
               IF OPENING-PROC-SUCCESS
                   MOVE LOW-VALUES TO SA-BATCH-ALT-KEY,
                                      PASS-PREV-BATCH-ALT-KEY,
      *             PERFORM START-NOT-LT-SA-BATCH-ALT-KEY,
                   IF SA-BATCH-FIL-STAT = I-O-OK
                       PERFORM PROC-NXT-SPEC
                            UNTIL SA-BATCH-FIL-STAT NOT = I-O-OK.
 
       MAIN-LOGIC-EXIT.
 
      *    MOVE PASS-DSPLY-MOD TO SAV-DSPLY-MOD.
           IF NXT-PGM-ID = "FILERR" OR
              NXT-PGM-NAM = "FILERR"
               CALL "FILERR" USING SYS-PASS-VALUS.
 
           MOVE "END" TO NXT-PGM-ID,
                         NXT-PGM-NAM.
           PERFORM CLOSING-PROC.
           PERFORM SET-DSPLY-ON.
           PERFORM DSPLY-ENDING-BATCH.
           PERFORM SET-DSPLY-OFF.
           .
           EXIT PROGRAM
           STOP RUN.
 
      / *** LEVEL 2 ROUTINES ***
 
       LEVEL-2 SECTION.
 
       BEG-START-UP.
 
      *    PERFORM TURN-ON-HELP-KEY.
 
      *    MOVE 1 TO DSPLY-MOD-FLG.
      *    MOVE DSPLY-OFF TO SAV-DSPLY-MOD.
      *    MOVE DSPLY-ON TO PASS-DSPLY-MOD.
 
      *    MOVE "Y" TO COMP-FIL-FND-FLG.
           MOVE "N" TO PASS-COMPRS-PRT-FLG (1).
           MOVE "$PRINTER" TO PASS-PRTR-DEV-NAM (1).
 
      *    MOVE SPACES TO PASS-ACCESS-TITLE.
 
       TURN-OFF-AUTO-FUNC-KEYS.
 
           MOVE "KS,12,0" TO SCRN-PARAM.
           PERFORM SCRN-CALL.
 
           MOVE "KS,20,0" TO SCRN-PARAM.
           PERFORM SCRN-CALL.
 
           MOVE "KS,21,0" TO SCRN-PARAM.
           PERFORM SCRN-CALL.
 
           MOVE "KS,31,0" TO SCRN-PARAM.
           PERFORM TURN-ON-OR-OFF-FUNC-KEY-GRP 5 TIMES.
 
           MOVE "KS,71,0" TO SCRN-PARAM.
           PERFORM TURN-ON-OR-OFF-FUNC-KEY-GRP 26 TIMES.
 
       GET-COMP-REC.
 
           MOVE "N" TO ALLOW-INVAL-OPN-FLG,
                       COMP-FIL-FND-FLG.
           MOVE SPACES TO COMP-REC.
 
           PERFORM OPN-INPUT-COMP-FIL.
           IF COMP-FIL-STAT = I-O-OK
               MOVE FIL-IS-OPN TO COMP-FIL-OPN-STAT,
               PERFORM RD-COMP-REC,
               IF COMP-FIL-STAT = I-O-OK
                   MOVE "Y" TO COMP-FIL-FND-FLG,
                   PERFORM SET-UP-PASS-DATA.
 
           IF COMP-FIL-IS-OPN
               PERFORM CLS-COMP-FIL,
               MOVE FIL-IS-CLSD TO COMP-FIL-OPN-STAT.
 
       OPENING-PROC.
 
           MOVE ZERO TO OPENING-PROC-STAT.
           PERFORM OPN-SA-BATCH-FIL.
           IF SA-BATCH-FIL-STAT = I-O-OK
               MOVE FIL-IS-OPN TO SA-BATCH-FIL-OPN-STAT
           ELSE
               MOVE UNSUCCESSFUL TO OPENING-PROC-STAT.
 
           MOVE OPN TO FILS-OPN-STAT.
           IF NOT OPENING-PROC-SUCCESS
               MOVE 3 TO PASS-CALL-STAT.
 
       PROC-NXT-SPEC.
 
           MOVE "N" TO VAL-SPEC-FND-FLG,
                       WAIT-03-FLG.
           MOVE "Y" TO PASS-VAL-BATCH-FLG.
           PERFORM RD-NXT-SA-BATCH-REC.
           IF SA-BATCH-FIL-STAT = I-O-OK
               IF SA-BATCH-FREQ = SPACE
                   MOVE "Y" TO VAL-SPEC-FND-FLG,
               ELSE
                   PERFORM PROC-FREQ-SEL.
 
 
           DISPLAY " "
                   UPON CONSOLE.
           DISPLAY "Program requested printer", " ",
                 SA-BATCH-PRTR-SEL
                   UPON CONSOLE.
 
           IF NOT VAL-SPEC-FND
               MOVE "N" TO PASS-VAL-BATCH-FLG,
           ELSE
               IF NOT PRT-THIS-RPT
                   MOVE "N" TO VAL-SPEC-FND-FLG,
               ELSE
                   IF SA-BATCH-PRTR-SEL = "M"
                       MOVE 4 TO PASS-BATCH-PRTR-SEL,
                   ELSE
                       IF SA-BATCH-PRTR-SEL = "D"
                           MOVE 12 TO PASS-BATCH-PRTR-SEL,
                       ELSE
                           MOVE 13 TO PASS-BATCH-PRTR-SEL.
                           
           MOVE SA-BATCH-PRTR-SEL TO PASS-BATCH-PRTR-SPEC.
           
           DISPLAY " "
                   UPON CONSOLE.
           DISPLAY "Selected Printer", " ",
                PASS-BATCH-PRTR-SEL
                   UPON CONSOLE.
 
           IF VAL-SPEC-FND
               DISPLAY " ",
                       UPON SYSERR
               DISPLAY "Valid rec-fnd yes",
                       UPON SYSERR
               PERFORM CALL-NXT-BATCH-PGM.
 
           IF PASS-VAL-BATCH-FLG = "Y"
               PERFORM RD-SA-BATCH-REC-WITH-LCK,
               MOVE PASS-SYS-DAT TO SA-BATCH-LST-DAT,
               PERFORM REWRT-SA-BATCH-REC,
               MOVE SA-BATCH-ALT-KEY TO PASS-PREV-BATCH-ALT-KEY.
               PERFORM START-GT-SA-BATCH-KEY.
      *         PERFORM RD-NXT-SA-BATCH-REC.
 
       CLOSING-PROC.
 
           IF SA-BATCH-FIL-IS-OPN
               PERFORM CLS-SA-BATCH-FIL,
               MOVE FIL-IS-CLSD TO SA-BATCH-FIL-OPN-STAT.
 
           MOVE FIL-IS-CLSD TO FILS-OPN-STAT.
 
      / *** LEVEL 3 ROUTINES ***
 
       LEVEL-3 SECTION.
 
       TURN-ON-OR-OFF-FUNC-KEY-GRP.
 
           PERFORM SCRN-CALL.
           ADD 1 TO SCRN-ROW.
 
       SET-UP-PASS-DATA.
 
           IF PASS-COMP-IS-CONSOL-FLG = SPACE
               MOVE "N" TO PASS-COMP-IS-CONSOL-FLG.
 
           MOVE "H" TO PASS-HELP-TYP,
                       PASS-HELP-SIZ.
 
           MOVE "S" TO PASS-FLG.
           MOVE COMP-RPT-NAM TO PASS-COMP-RPT-NAM.
           MOVE COMP-DSPLY-NAM TO PASS-COMP-DSPLY-NAM.
           MOVE COMP-MAIN-ACCT-SIZ TO PASS-COMP-MAIN-ACCT-SIZ.
           MOVE COMP-SUB-ACCT-SIZ TO PASS-COMP-SUB-ACCT-SIZ.
           MOVE "Y" TO PASS-MU-FIL-PROT-USAGE-FLG.
           MOVE COMP-PASSWORD-USAGE-FLG TO PASS-COMP-PASSWORD-USAGE-FLG.
           MOVE COMP-CHNG-LOG-USAGE-FLG TO PASS-COMP-CHNG-LOG-USAGE-FLG.
           MOVE COMP-ALLOW-PROT-CHNG-FLG
               TO PASS-COMP-ALLOW-PROT-CHNG-FLG.
           MOVE COMP-MENU-TYP TO PASS-MENU-TYP.
           MOVE COMP-USE-LIN-25 TO PASS-USE-LIN-25.
 
           ACCEPT TEMP-SYS-DAT FROM DATE.
           MOVE ZERO TO PASS-SYS-DAT.
           MOVE TEMP-SYS-DAT TO PASS-SYS-DAT.
           ADD 2000 TO PASS-SYS-FULL-YR.
      *     ADD 8 TO PASS-SYS-FULL-YR.
 
      *     ACCEPT PASS-SYS-DAT FROM DATE.
      *     ADD 8 TO PASS-SYS-YR.
 
           MOVE COMP-PRTRS TO PASS-COMP-PRTRS.
      *    MOVE COMP-MENU TO MAIN-MENU-TABLE,
           MOVE COMP-MENU TO PASS-COMP-MENU.
           MOVE COMP-DSPLY-MOD TO DSPLY-MOD,
                                  PASS-DSPLY-MOD.
           IF COMP-DSPLY-MOD = DSPLY-ON
               MOVE 1 TO SAV-DSPLY-MOD,
           ELSE
               MOVE ZERO TO SAV-DSPLY-MOD.
 
      *     IF PASS-MENU-TYP = 2
      *         PERFORM SET-MENU-BAR-SYS.
 
      *    MOVE MAIN-MENU-ENTRY-FOR (PO) TO PO-IS-ON-MAIN-MENU-FLG.
 
       PROC-FREQ-SEL.
 
           IF SA-BATCH-PRD-START-DAT (2) > ZERO
               MOVE SA-BATCH-PRD-START-DAT (2) TO PRD-START-DAT,
               MOVE SA-BATCH-PRD-END-DAT (2) TO PRD-END-DAT,
           ELSE
               MOVE SA-BATCH-PRD-START-DAT (1) TO PRD-START-DAT,
               MOVE SA-BATCH-PRD-END-DAT (1) TO PRD-END-DAT.
 
           IF YEARLY-RPT
               PERFORM PROC-YR-SEL THRU PROC-YR-EXIT,
           ELSE
               IF QUARTERLY-RPT
                   PERFORM PROC-QTR-SEL THRU PROC-QTR-EXIT,
               ELSE
                   IF MONTHLY-RPT
                       PERFORM PROC-MON-SEL THRU PROC-MON-EXIT,
                   ELSE
                       IF WEEKLY-RPT
                           PERFORM PROC-WEEK-SEL THRU PROC-WEEK-EXIT,
                       ELSE
                           PERFORM PROC-DAY-SEL THRU PROC-DAY-EXIT.
 
           MOVE PRD-START-DAT TO SA-BATCH-PRD-START-DAT (1).
           MOVE PRD-END-DAT TO SA-BATCH-PRD-END-DAT (1).
           MOVE ZEROS TO SA-BATCH-PRD-START-DAT (2),
                         SA-BATCH-PRD-END-DAT (2),
                         SA-BATCH-PRD-START-DAT (3),
                         SA-BATCH-PRD-END-DAT (3),
                         SA-BATCH-PRD-START-DAT (4),
                         SA-BATCH-PRD-END-DAT (4).
           MOVE "N" TO SA-BATCH-MULT-PRDS-FLG.
 
           IF QUARTERLY-RPT OR
              MONTHLY-RPT
               IF SA-BATCH-PGM-NO < 9
                   PERFORM PROC-MULT-PRDS-SEL THRU PROC-MULT-PRDS-EXIT.
 
       CALL-NXT-BATCH-PGM.
 
           PERFORM SET-DSPLY-OFF.
           MOVE ZERO TO PASS-CALL-STAT.
           IF SA-BATCH-PGM-NO = 15
               IF SA-BATCH-DAYS-PLUS = 999
                   MOVE 1 TO SA-BATCH-PRD-START-DAT (1),
                   MOVE 99999999 TO SA-BATCH-PRD-END-DAT (1),
               ELSE
                   MOVE SA-BATCH-PRD-START-DAT (1) TO FORMAT-YYMMDD-DAT,
                   ADD SA-BATCH-DAYS-PLUS TO FORMAT-YYMMDD-DAY,
                   PERFORM GET-DAYS-IN-MONTH,
                   PERFORM ADD-PLUS-DAY.
 
           IF SA-BATCH-PGM-NO = 17
               MOVE SA-BATCH-PRD-START-DAT (1) TO FORMAT-YYMMDD-DAT,
               ADD SA-BATCH-DAYS-PLUS TO FORMAT-YYMMDD-DAY,
               PERFORM GET-DAYS-IN-MONTH,
               PERFORM ADD-PLUS-DAY.
 
           IF SA-BATCH-PGM-NO < 13
               MOVE "SA" TO PASS-SYS-ID,
                            PASS-DRIVER-ID,
           ELSE
               IF SA-BATCH-PGM-NO = 16
                   MOVE "PI" TO PASS-SYS-ID,
                                PASS-DRIVER-ID,
               ELSE
                   IF SA-BATCH-PGM-NO = 17
                       MOVE "PO" TO PASS-SYS-ID,
                                    PASS-DRIVER-ID,
                   ELSE
                       IF SA-BATCH-PGM-NO = 18 OR
                          SA-BATCH-PGM-NO = 19 OR
                          SA-BATCH-PGM-NO = 23
                           MOVE "IC" TO PASS-SYS-ID,
                                        PASS-DRIVER-ID,
                       ELSE
                           IF SA-BATCH-PGM-NO = 21
                               MOVE "AR" TO PASS-SYS-ID,
                                            PASS-DRIVER-ID,
                           ELSE
                               MOVE "OE" TO PASS-SYS-ID,
                                            PASS-DRIVER-ID.
 
           MOVE PGM-FIL-NAM (SA-BATCH-PGM-NO) TO NXT-PGM-NAM,
                                                 NXT-PGM-ID,
                                                 PATH-NAM-FIL.
 
           IF SA-BATCH-PGM-NO = 16
               IF SA-BATCH-SEL-FORMAT = "B"
                    MOVE "QTSLST" TO NXT-PGM-NAM,
                                     NXT-PGM-ID,
                                     PATH-NAM-FIL,
               ELSE
                   IF SA-BATCH-SEL-FORMAT = "F"
                        MOVE "QTELST" TO NXT-PGM-NAM,
                                         NXT-PGM-ID,
                                         PATH-NAM-FIL,
                    ELSE
                        MOVE "OPNINQ" TO NXT-PGM-NAM,
                                         NXT-PGM-ID,
                                         PATH-NAM-FIL.
 
           PERFORM GET-PGM-PATH.
           MOVE PATH TO NXT-PGM-NAM.
      *    PERFORM CALL-PATH.
           IF SA-BATCH-PGM-NO NOT = 21
               CALL NXT-PGM-NAM
                   USING SYS-PASS-VALUS,
                         PASS-BATCH-CTL-VALUS,
                         SA-BATCH-REC,
           ELSE
               DISPLAY " ",
                       UPON CONSOLE
               DISPLAY "Program Name", " ", NXT-PGM-NAM ,
                       UPON CONSOLE
               CALL NXT-PGM-NAM
                   USING SYS-PASS-VALUS,
                         PASS-AGING-FLDS,
                         PASS-BATCH-CTL-VALUS,
                         SA-BATCH-REC.
 
           PERFORM SET-DSPLY-ON.
 
 
       ADD-PLUS-DAY.
 
           IF FORMAT-YYMMDD-DAY NOT > DAYS-IN-MONTH
               MOVE FORMAT-YYMMDD-DAT TO SA-BATCH-PRD-START-DAT (1),
                                         SA-BATCH-PRD-END-DAT (1),
           ELSE
               SUBTRACT DAYS-IN-MONTH FROM FORMAT-YYMMDD-DAY,
               ADD 1 TO FORMAT-YYMMDD-MON,
               IF FORMAT-YYMMDD-MON < 13
                   MOVE FORMAT-YYMMDD-DAT TO SA-BATCH-PRD-START-DAT (1),
                                             SA-BATCH-PRD-END-DAT (1),
               ELSE
                   MOVE 1 TO FORMAT-YYMMDD-MON,
                   ADD 1 TO FORMAT-YYMMDD-YR,
                   MOVE FORMAT-YYMMDD-DAT TO SA-BATCH-PRD-START-DAT (1),
                                             SA-BATCH-PRD-END-DAT (1).
 
           PERFORM GET-DAYS-IN-MONTH.
           IF FORMAT-YYMMDD-DAY > DAYS-IN-MONTH
               GO TO ADD-PLUS-DAY.
 
       GET-DAYS-IN-MONTH.
 
           IF FORMAT-YYMMDD-MON = 1
               MOVE 31 TO DAYS-IN-MONTH.
 
           IF FORMAT-YYMMDD-MON = 2
               MOVE 28 TO DAYS-IN-MONTH.
 
           IF FORMAT-YYMMDD-MON = 3
               MOVE 31 TO DAYS-IN-MONTH.
 
           IF FORMAT-YYMMDD-MON = 4
               MOVE 30 TO DAYS-IN-MONTH.
 
           IF FORMAT-YYMMDD-MON = 5
               MOVE 31 TO DAYS-IN-MONTH.
 
           IF FORMAT-YYMMDD-MON = 6
               MOVE 30 TO DAYS-IN-MONTH.
 
           IF FORMAT-YYMMDD-MON = 7
               MOVE 31 TO DAYS-IN-MONTH.
 
           IF FORMAT-YYMMDD-MON = 8
               MOVE 31 TO DAYS-IN-MONTH.
 
           IF FORMAT-YYMMDD-MON = 9
               MOVE 30 TO DAYS-IN-MONTH.
 
           IF FORMAT-YYMMDD-MON = 10
               MOVE 31 TO DAYS-IN-MONTH.
 
           IF FORMAT-YYMMDD-MON = 11
               MOVE 30 TO DAYS-IN-MONTH.
 
           IF FORMAT-YYMMDD-MON = 12
               MOVE 31 TO DAYS-IN-MONTH.
 
      / *** LEVEL 4 ROUTINES ***
 
       LEVEL-4 SECTION.
 
       PROC-YR-SEL.
 
           IF PASS-SYS-MO > 9
               IF PASS-SYS-FULL-YR > SA-BATCH-LST-YR
                   MOVE 10 TO PRD-START-MON,
                   MOVE 1 TO PRD-START-DAY,
                   SUBTRACT 1 FROM PASS-SYS-FULL-YR GIVING PRD-START-YR,
                   MOVE 9 TO PRD-END-MON,
                   MOVE 30 TO PRD-END-DAY,
                   MOVE PASS-SYS-FULL-YR TO PRD-END-YR,
                   MOVE "Y" TO VAL-SPEC-FND-FLG.
 
       PROC-YR-EXIT.
           EXIT.
 
       PROC-QTR-SEL.
 
           IF PASS-SYS-MO > 9
               IF PASS-SYS-FULL-YR > SA-BATCH-LST-YR
                   GO TO PROC-QTR-3,
               ELSE
                   IF PASS-SYS-FULL-YR = SA-BATCH-LST-YR
                       IF SA-BATCH-LST-MON > 9
                           GO TO PROC-QTR-EXIT,
                       ELSE
                           GO TO PROC-QTR-3.
 
           IF PASS-SYS-MO > 6
               IF PASS-SYS-FULL-YR > SA-BATCH-LST-YR
                   GO TO PROC-QTR-2,
               ELSE
                   IF PASS-SYS-FULL-YR = SA-BATCH-LST-YR
                       IF SA-BATCH-LST-MON > 6
                           GO TO PROC-QTR-EXIT,
                       ELSE
                           GO TO PROC-QTR-2.
 
           IF PASS-SYS-MO > 3
               IF PASS-SYS-FULL-YR > SA-BATCH-LST-YR
                   GO TO PROC-QTR-1,
               ELSE
                   IF PASS-SYS-FULL-YR = SA-BATCH-LST-YR
                       IF SA-BATCH-LST-MON > 3
                           GO TO PROC-QTR-EXIT,
                       ELSE
                           GO TO PROC-QTR-1.
 
           IF PASS-SYS-FULL-YR = SA-BATCH-LST-YR
               GO TO PROC-QTR-EXIT,
           ELSE
               MOVE 10 TO PRD-START-MON,
               MOVE 1 TO PRD-START-DAY,
               SUBTRACT 1 FROM PASS-SYS-FULL-YR GIVING PRD-START-YR,
                                                       PRD-END-YR,
               MOVE 12 TO PRD-END-MON,
               MOVE 31 TO PRD-END-DAY,
               MOVE "Y" TO VAL-SPEC-FND-FLG,
               GO TO PROC-QTR-EXIT.
 
       PROC-QTR-3.
 
           MOVE 7 TO PRD-START-MON.
           MOVE 1 TO PRD-START-DAY.
           MOVE PASS-SYS-FULL-YR TO PRD-START-YR,
                                    PRD-END-YR.
           MOVE 9 TO PRD-END-MON.
           MOVE 30 TO PRD-END-DAY.
           MOVE "Y" TO VAL-SPEC-FND-FLG.
           GO TO PROC-QTR-EXIT.
 
       PROC-QTR-2.
 
           MOVE 4 TO PRD-START-MON.
           MOVE 1 TO PRD-START-DAY.
           MOVE PASS-SYS-FULL-YR TO PRD-START-YR,
                                    PRD-END-YR.
           MOVE 6 TO PRD-END-MON.
           MOVE 30 TO PRD-END-DAY.
           MOVE "Y" TO VAL-SPEC-FND-FLG.
           GO TO PROC-QTR-EXIT.
 
       PROC-QTR-1.
 
           MOVE 1 TO PRD-START-MON.
           MOVE 1 TO PRD-START-DAY.
           MOVE PASS-SYS-FULL-YR TO PRD-START-YR,
                                    PRD-END-YR.
           MOVE 3 TO PRD-END-MON.
           MOVE 31 TO PRD-END-DAY.
           MOVE "Y" TO VAL-SPEC-FND-FLG.
 
       PROC-QTR-EXIT.
           EXIT.
 
       PROC-MON-SEL.
 
           IF PASS-SYS-FULL-YR > SA-BATCH-LST-YR
               IF PASS-SYS-MO = 1
                   MOVE 12 TO PRD-START-MON,
                              PRD-END-MON,
                   MOVE 1 TO PRD-START-DAY,
                   MOVE 31 TO PRD-END-DAY,
                   SUBTRACT 1 FROM PASS-SYS-FULL-YR GIVING PRD-START-YR,
                                                           PRD-END-YR,
                   MOVE "Y" TO VAL-SPEC-FND-FLG,
               ELSE
                   SUBTRACT 1 FROM PASS-SYS-MO GIVING PRD-START-MON,
                                                      PRD-END-MON,
                   MOVE 1 TO PRD-START-DAY,
                   MOVE MON-END-DAY (PRD-END-MON) TO PRD-END-DAY,
                   MOVE PASS-SYS-FULL-YR TO PRD-START-YR,
                                            PRD-END-YR,
                   PERFORM CHK-FOR-LEAP-YR,
                   MOVE "Y" TO VAL-SPEC-FND-FLG.
 
           IF PASS-SYS-FULL-YR = SA-BATCH-LST-YR
               IF PASS-SYS-MO NOT > SA-BATCH-LST-MON
                   GO TO PROC-MON-EXIT,
               ELSE
                   SUBTRACT 1 FROM PASS-SYS-MO GIVING PRD-START-MON,
                                                      PRD-END-MON,
                   MOVE 1 TO PRD-START-DAY,
                   MOVE MON-END-DAY (PRD-END-MON) TO PRD-END-DAY,
                   MOVE PASS-SYS-FULL-YR TO PRD-START-YR,
                                            PRD-END-YR,
                   PERFORM CHK-FOR-LEAP-YR,
                   MOVE "Y" TO VAL-SPEC-FND-FLG.
 
       PROC-MON-EXIT.
           EXIT.
 
       PROC-WEEK-SEL.
 
           MOVE PASS-SYS-DAT TO OLD-DAT.
           MOVE ZERO TO NO-OF-NEW-DAYS.
           PERFORM CALC-NEW-DAT.
           MOVE TOT-DAYS TO WK-SYS-DAYS.
 
           SUBTRACT WK-BASE FROM WK-SYS-DAYS.
           DIVIDE WK-SYS-DAYS BY 7 GIVING WK-DIVIDED-NO.
           IF WK-DIVIDED-NO-DEC = ZERO
               GO TO PROC-WEEK-EXIT.
 
           MULTIPLY WK-DIVIDED-NO-DEC BY 7 GIVING REMAIN-DAYS ROUNDED.
           MOVE REMAIN-DAYS TO WK-SYS-NO-OF-NEW-DAYS.
           SUBTRACT REMAIN-DAYS FROM WK-SYS-DAYS.
 
           IF SA-BATCH-LST-DAT = ZERO
               GO TO PROC-WEEK-PRD,
           ELSE
               MOVE SA-BATCH-LST-DAT TO OLD-DAT,
               PERFORM CALC-NEW-DAT,
               MOVE TOT-DAYS TO WK-LST-DAYS.
 
           SUBTRACT WK-BASE FROM WK-LST-DAYS.
           DIVIDE WK-LST-DAYS BY 7 GIVING WK-DIVIDED-NO.
           MULTIPLY WK-DIVIDED-NO-DEC BY 7 GIVING REMAIN-DAYS ROUNDED.
           SUBTRACT REMAIN-DAYS FROM WK-LST-DAYS.
           IF WK-LST-DAYS + 7 > WK-SYS-DAYS
               GO TO PROC-WEEK-EXIT.
 
       PROC-WEEK-PRD.
 
           MOVE PASS-SYS-DAT TO OLD-DAT.
           MULTIPLY WK-SYS-NO-OF-NEW-DAYS BY -1 GIVING NO-OF-NEW-DAYS.
           PERFORM CALC-NEW-DAT.
           MOVE NEW-DAT TO PRD-END-DAT.
 
           SUBTRACT 6 FROM NO-OF-NEW-DAYS.
           PERFORM CALC-NEW-DAT.
           MOVE NEW-DAT TO PRD-START-DAT.
           MOVE "Y" TO VAL-SPEC-FND-FLG.
 
       PROC-WEEK-EXIT.
           EXIT.
 
 
       PROC-DAY-SEL.
 
           IF PASS-SYS-DAT > SA-BATCH-LST-DAT
               MOVE PASS-SYS-DAT TO OLD-DAT,
               MOVE -1 TO NO-OF-NEW-DAYS,
               PERFORM CALC-NEW-DAT,
               MOVE NEW-DAT TO PRD-START-DAT,
                               PRD-END-DAT,
               MOVE "Y" TO VAL-SPEC-FND-FLG,
           ELSE
               DISPLAY " ",
                       UPON CONSOLE
               DISPLAY "Program to run next", " ",
                       SA-BATCH-LST-DAT
                       UPON CONSOLE.
 
      *
       PROC-DAY-EXIT.
           EXIT.
 
       PROC-MULT-PRDS-SEL.
 
           IF PRD-START-MON = 10
               GO TO PROC-MULT-PRDS-EXIT.
 
           MOVE PRD-START-DAT TO SA-BATCH-PRD-START-DAT (2).
           MOVE PRD-END-DAT TO SA-BATCH-PRD-END-DAT (2).
           MOVE "Y" TO SA-BATCH-MULT-PRDS-FLG.
           IF PRD-START-MON = 1
               MOVE 12 TO PRD-END-MON,
               MOVE 31 TO PRD-END-DAY,
               SUBTRACT 1 FROM PRD-END-YR,
           ELSE
               SUBTRACT 1 FROM PRD-START-MON GIVING PRD-END-MON,
               MOVE MON-END-DAY (PRD-END-MON) TO PRD-END-DAY,
               PERFORM CHK-FOR-LEAP-YR.
 
           IF PRD-START-MON < 10
               SUBTRACT 1 FROM PRD-START-YR.
 
           MOVE 10 TO PRD-START-MON.
           MOVE 1 TO PRD-START-DAY.
 
           MOVE PRD-START-DAT TO SA-BATCH-PRD-START-DAT (1).
           MOVE PRD-END-DAT TO SA-BATCH-PRD-END-DAT (1).
 
       PROC-MULT-PRDS-EXIT.
           EXIT.
 
       CHK-FOR-LEAP-YR.
 
           IF PRD-END-MON = 2
               DIVIDE PRD-END-YR BY 4 GIVING LEAP-YR-DIVIDED-NO,
               IF LEAP-YR-DIVIDED-NO-DEC = ZEROS
                   MOVE 29 TO PRD-END-DAY.
 
      / *** I-O ROUTINES ***
 
       I-O-ROUTINES SECTION.
 
       OPN-INPUT-COMP-FIL.
           MOVE COMP-FIL-ID TO PATH-NAM-FIL.
           PERFORM GET-DATA-FIL-PATH.
           MOVE PATH TO COMP-FIL-NAM.
      *    OPEN INPUT COMP-FIL ALLOWING ALL
           MOVE "OP" TO DB-IO
           PERFORM UCT-DBIO-PARAGRAPH
           MOVE DB-FILE-STATUS TO COMP-FIL-STAT.
 
 
       RD-COMP-REC.
           MOVE 1 TO COMP-FIL-KEY.
           MOVE SPACES TO DB-SEARCH-ITEM
           MOVE "RE" TO DB-IO
           MOVE "COMPFI" TO DB-TABLE
           MOVE COMP-REC TO DB-BUFFER
           PERFORM UCT-DBIO-PARAGRAPH
           MOVE DB-BUFFER TO COMP-REC
           MOVE DB-FILE-STATUS TO COMP-FIL-STAT.
      *    READ COMP-FIL        REGARDLESS
 
 
       CLS-COMP-FIL.
           MOVE "CL" TO DB-IO
           MOVE "COMPFI" TO DB-TABLE
           PERFORM UCT-DBIO-PARAGRAPH.
      *    CLOSE COMP-FIL
 
 
       OPN-SA-BATCH-FIL.
           MOVE SA-BATCH-FIL-ID TO PATH-NAM-FIL.
           PERFORM GET-DATA-FIL-PATH.
           MOVE PATH TO SA-BATCH-FIL-NAM.
      *    OPEN I-O SA-BATCH-FIL ALLOWING ALL
           MOVE "OP" TO DB-IO
           PERFORM UCT-DBIO-PARAGRAPH
           MOVE DB-FILE-STATUS TO SA-BATCH-FIL-STAT.
 
 
       START-NOT-LT-SA-BATCH-ALT-KEY.
           MOVE "KEY01" TO DB-SEARCH-ITEM
           MOVE "SGE" TO DB-IO
           MOVE "SABATF" TO DB-TABLE
           MOVE SA-BATCH-REC TO DB-BUFFER
           PERFORM UCT-DBIO-PARAGRAPH
           MOVE DB-FILE-STATUS TO SA-BATCH-FIL-STAT.
      *    START SA-BATCH-FIL KEY NOT <
      *        SA-BATCH-ALT-KEY
      *^*^* 3 lines to be deleted by later macro
           IF SA-BATCH-FIL-STAT = I-O-OK
               MOVE "UN" TO DB-IO
               MOVE "SABATF" TO DB-TABLE
               PERFORM UCT-DBIO-PARAGRAPH.
      *               UNLOCK SA-BATCH-FIL
 
           IF SA-BATCH-FIL-STAT = REC-LCKD
               GO TO START-NOT-LT-SA-BATCH-ALT-KEY.
 
       START-GT-SA-BATCH-ALT-KEY.
           MOVE "KEY01" TO DB-SEARCH-ITEM
           MOVE "SL" TO DB-IO
           MOVE "SABATF" TO DB-TABLE
           MOVE SA-BATCH-REC TO DB-BUFFER
           PERFORM UCT-DBIO-PARAGRAPH
           MOVE DB-FILE-STATUS TO SA-BATCH-FIL-STAT.
      *    START SA-BATCH-FIL KEY <
      *        SA-BATCH-ALt-KEY
      *^*^* 3 lines to be deleted by later macro
           IF SA-BATCH-FIL-STAT = I-O-OK
               MOVE "UN" TO DB-IO
               MOVE "SABATF" TO DB-TABLE
               PERFORM UCT-DBIO-PARAGRAPH.
      *               UNLOCK SA-BATCH-FIL
 
           IF SA-BATCH-FIL-STAT = REC-LCKD
               GO TO START-GT-SA-BATCH-ALT-KEY.
 
       START-GT-SA-BATCH-KEY.
           MOVE "KEY00" TO DB-SEARCH-ITEM
           MOVE "SG" TO DB-IO
           MOVE "SABATF" TO DB-TABLE
           MOVE SA-BATCH-REC TO DB-BUFFER
           PERFORM UCT-DBIO-PARAGRAPH
           MOVE DB-FILE-STATUS TO SA-BATCH-FIL-STAT.
      *    START SA-BATCH-FIL KEY >
      *        SA-BATCH-KEY
      *^*^* 3 lines to be deleted by later macro
           IF SA-BATCH-FIL-STAT = I-O-OK
               MOVE "UN" TO DB-IO
               MOVE "SABATF" TO DB-TABLE
               PERFORM UCT-DBIO-PARAGRAPH.
      *               UNLOCK SA-BATCH-FIL
 
           IF SA-BATCH-FIL-STAT = REC-LCKD
               GO TO START-GT-SA-BATCH-ALT-KEY.
 
       RD-NXT-SA-BATCH-REC.
           MOVE SPACES TO DB-SEARCH-ITEM
           MOVE "RN" TO DB-IO
           MOVE "SABATF" TO DB-TABLE
           MOVE SA-BATCH-REC TO DB-BUFFER
           PERFORM UCT-DBIO-PARAGRAPH
           MOVE DB-BUFFER TO SA-BATCH-REC
           MOVE DB-FILE-STATUS TO SA-BATCH-FIL-STAT.
      *    READ SA-BATCH-FIL NEXT        REGARDLESS
 
           IF SA-BATCH-FIL-STAT = I-O-OK
               MOVE "UN" TO DB-IO
               MOVE "SABATF" TO DB-TABLE
               PERFORM UCT-DBIO-PARAGRAPH.
      *               UNLOCK SA-BATCH-FIL
 
           DISPLAY " "
                   UPON SYSERR.
           DISPLAY "Specification name", " ",
               SA-BATCH-SPEC-NO
                   UPON SYSERR.
           DISPLAY " "
                   UPON SYSERR.
           DISPLAY "Program number ", " ",
                   SA-BATCH-PGM-NO
                   UPON SYSERR.
      *
       RD-SA-BATCH-REC-WITH-LCK.
           MOVE "KEY01" TO DB-SEARCH-ITEM
           MOVE "RKH" TO DB-IO
           MOVE "SABATF" TO DB-TABLE
           MOVE SA-BATCH-REC TO DB-BUFFER
           PERFORM UCT-DBIO-PARAGRAPH
           MOVE DB-BUFFER TO SA-BATCH-REC
           MOVE DB-FILE-STATUS TO SA-BATCH-FIL-STAT.
      *    READ SA-BATCH-FIL        ALLOWING NO
      *        KEY SA-BATCH-ALT-KEY
 
 
           IF SA-BATCH-FIL-STAT = REC-LCKD
               GO TO RD-SA-BATCH-REC-WITH-LCK.
 
       REWRT-SA-BATCH-REC.
           MOVE "UP" TO DB-IO
           MOVE SA-BATCH-REC TO DB-BUFFER
           MOVE "SABATF" TO DB-TABLE
           PERFORM UCT-DBIO-PARAGRAPH
           MOVE DB-FILE-STATUS TO SA-BATCH-FIL-STAT.
      *UCT REWRITE SA-BATCH-REC ALLOWING NO
           IF SA-BATCH-FIL-STAT = I-O-OK
               MOVE "UN" TO DB-IO
               MOVE "SABATF" TO DB-TABLE
               PERFORM UCT-DBIO-PARAGRAPH.
      *               UNLOCK SA-BATCH-FIL
 
       CLS-SA-BATCH-FIL.
           MOVE "CL" TO DB-IO
           MOVE "SABATF" TO DB-TABLE
           PERFORM UCT-DBIO-PARAGRAPH.
      *    CLOSE SA-BATCH-FIL
 
 
      / *** MESSAGE ROUTINES ***
 
       MSG-ROUTINES SECTION.
 
       COMP-FIL-NOT-FND-MSG.
 
           MOVE "Dm,99" TO SCRN-PARAM.
           MOVE "Company file not found" TO SCRN-LIT.
      *    PERFORM MSG.
           PERFORM DSPLY.
 
       CALL-PATH.
 
           MOVE ZERO TO PASS-CALL-STAT.
           MOVE PASS-MENU-SEL TO SAV-MENU-SEL.
           PERFORM GET-PGM-PATH.
           CALL PATH USING SYS-PASS-VALUS
               ON OVERFLOW
                   MOVE 2 TO PASS-CALL-STAT.
 
           CANCEL PATH.
           IF NXT-PGM-NAM = "FILERR" OR
              NXT-PGM-NAM = "END"
               PERFORM EXIT-THIS-PGM.
 
           IF PASS-TEMP-ACCESS NOT = "Y" AND
              PASS-MENU-TYP = 2
               IF PASS-MENU-SEL NOT = SAV-MENU-SEL OR
                  (NXT-PGM-ID NOT = PGM-NAM AND
                  NXT-PGM-ID NOT = CALLED-THRU-NAM AND
                  NXT-PGM-ID NOT = PASS-ACCESS-TOP-PGM-NAM)
                   PERFORM SET-DSPLY-OFF,
                   PERFORM EXIT-THIS-PGM.
 
           IF PASS-ACCESS-ACTIV NOT = "LOAD"
               IF PASS-CALL-STAT = 1 OR
                  PASS-CALL-STAT = 2
      *            PERFORM CHK-IF-LOAD-OK,
                   PERFORM MAIN-LOGIC-EXIT.
 
      / *** UTILITY ROUTINES ***
 
       UTILITY-ROUTINES SECTION.
 
       ABORT-PGM-LOGIC.
 
           EXIT.
 
           COPY "GETPTHPL.COB".
 
 
           COPY "NEWDATPL.COB".
 
 
           COPY "NXTPGMPL.COB".
 
 
      *** SCREEN ROUTINE ***
 
       DSPLY-STARTING-BATCH.
 
           MOVE "CS,01,01" TO SCRN-PARAM.
           PERFORM DSPLY.
 
           MOVE "DL,01,01,31,H" TO SCRN-PARAM.
           MOVE "Starting Batch Program ..." TO SCRN-LIT.
           PERFORM DSPLY.
 
       DSPLY-ENDING-BATCH.
 
           MOVE "CS,01,01" TO SCRN-PARAM.
           PERFORM DSPLY.
 
           MOVE "DL,20,01,31,H" TO SCRN-PARAM.
           MOVE "Ending Batch Program ..." TO SCRN-LIT.
           PERFORM DSPLY.
 
           MOVE "DL,22,01,01" TO SCRN-PARAM.
           MOVE SPACE TO SCRN-LIT.
           PERFORM DSPLY.
 
           MOVE "EX" TO SCRN-PARAM.
           PERFORM DSPLY.
 
       DSPLY.
           MOVE PASS-DSPLY-MOD TO DSPLY-MOD.
           PERFORM SCRN-CALL.
 
       MSG.
           PERFORM SCRN-CALL.
 
       SCRN-CALL.
           CALL "SCREEN" USING SCRN-CTL,
                               WIN-CTL,
                               SCRN-ID.
 
           IF HELP-KEY OR
              PRT-KEY
               GO TO SCRN-CALL.
 
       SET-DSPLY-ON.
           IF DSPLY-MOD-OFF
               MOVE 1 TO DSPLY-MOD-FLG,
               MOVE PASS-DSPLY-MOD TO SAV-DSPLY-MOD,
               MOVE DSPLY-ON TO PASS-DSPLY-MOD.
 
       SET-DSPLY-OFF.
           IF DSPLY-MOD-ON
               MOVE ZERO TO DSPLY-MOD-FLG,
               MOVE SAV-DSPLY-MOD TO PASS-DSPLY-MOD.
 
       INIT-FUNC-KEY.
           MOVE ZERO TO FUNC-KEY-VALU.
       COPY "PDUCTDBIOMF.COB".
