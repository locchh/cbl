      *****************************************************************
      * BANKING-TRANSACTION-SYSTEM.cbl                                *
      *                                                               *
      * This program processes daily banking transactions for a       *
      * financial institution. It handles account debits, credits,    *
      * transfers, and generates account statements and reports.      *
      *                                                               *
      * Author: Cascade AI                                            *
      * Date: 2025-03-14                                              *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKING-TRANSACTION-SYSTEM.
       AUTHOR. CASCADE-AI.
       DATE-WRITTEN. 2025-03-14.
       DATE-COMPILED. 2025-03-14.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-MASTER-FILE ASSIGN TO CUSTMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CM-CUSTOMER-ID
               FILE STATUS IS CUSTOMER-FILE-STATUS.
               
           SELECT ACCOUNT-MASTER-FILE ASSIGN TO ACCTMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AM-ACCOUNT-NUMBER
               ALTERNATE RECORD KEY IS AM-CUSTOMER-ID
                   WITH DUPLICATES
               FILE STATUS IS ACCOUNT-FILE-STATUS.
               
           SELECT TRANSACTION-FILE ASSIGN TO TRANFILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS TRANS-FILE-STATUS.
               
           SELECT STATEMENT-FILE ASSIGN TO STMTFILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS STMT-FILE-STATUS.
               
           SELECT DAILY-REPORT-FILE ASSIGN TO DAYREPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS REPORT-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       
       FD  CUSTOMER-MASTER-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 200 CHARACTERS.
       01  CUSTOMER-MASTER-RECORD.
           05  CM-CUSTOMER-ID             PIC X(10).
           05  CM-CUSTOMER-NAME.
               10  CM-LAST-NAME           PIC X(20).
               10  CM-FIRST-NAME          PIC X(15).
               10  CM-MIDDLE-INIT         PIC X.
           05  CM-ADDRESS.
               10  CM-STREET              PIC X(30).
               10  CM-CITY                PIC X(20).
               10  CM-STATE               PIC X(2).
               10  CM-ZIP-CODE            PIC X(10).
           05  CM-PHONE-NUMBER            PIC X(15).
           05  CM-EMAIL                   PIC X(30).
           05  CM-TAX-ID                  PIC X(11).
           05  CM-DATE-OF-BIRTH.
               10  CM-DOB-YEAR            PIC 9(4).
               10  CM-DOB-MONTH           PIC 9(2).
               10  CM-DOB-DAY             PIC 9(2).
           05  CM-CUSTOMER-SINCE.
               10  CM-CUST-SINCE-YEAR     PIC 9(4).
               10  CM-CUST-SINCE-MONTH    PIC 9(2).
               10  CM-CUST-SINCE-DAY      PIC 9(2).
           05  CM-CUSTOMER-STATUS         PIC X.
               88  CM-ACTIVE              VALUE 'A'.
               88  CM-INACTIVE            VALUE 'I'.
               88  CM-CLOSED              VALUE 'C'.
           05  CM-CUSTOMER-TYPE           PIC X.
               88  CM-INDIVIDUAL          VALUE 'I'.
               88  CM-BUSINESS            VALUE 'B'.
           05  CM-CREDIT-SCORE            PIC 9(3).
           05  CM-FILLER                  PIC X(20).
           
       FD  ACCOUNT-MASTER-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 250 CHARACTERS.
       01  ACCOUNT-MASTER-RECORD.
           05  AM-ACCOUNT-NUMBER          PIC X(12).
           05  AM-CUSTOMER-ID             PIC X(10).
           05  AM-ACCOUNT-TYPE            PIC X(2).
               88  AM-CHECKING            VALUE 'CK'.
               88  AM-SAVINGS             VALUE 'SV'.
               88  AM-MONEY-MARKET        VALUE 'MM'.
               88  AM-CERTIFICATE         VALUE 'CD'.
               88  AM-LOAN                VALUE 'LN'.
               88  AM-CREDIT-CARD         VALUE 'CC'.
           05  AM-ACCOUNT-STATUS          PIC X.
               88  AM-ACTIVE              VALUE 'A'.
               88  AM-DORMANT             VALUE 'D'.
               88  AM-FROZEN              VALUE 'F'.
               88  AM-CLOSED              VALUE 'C'.
           05  AM-CURRENT-BALANCE         PIC S9(11)V99 COMP-3.
           05  AM-AVAILABLE-BALANCE       PIC S9(11)V99 COMP-3.
           05  AM-ACCRUED-INTEREST        PIC S9(7)V99 COMP-3.
           05  AM-INTEREST-RATE           PIC S9(3)V9(6) COMP-3.
           05  AM-INTEREST-YTD            PIC S9(9)V99 COMP-3.
           05  AM-DATE-OPENED.
               10  AM-OPEN-YEAR           PIC 9(4).
               10  AM-OPEN-MONTH          PIC 9(2).
               10  AM-OPEN-DAY            PIC 9(2).
           05  AM-LAST-ACTIVITY-DATE.
               10  AM-LAST-ACT-YEAR       PIC 9(4).
               10  AM-LAST-ACT-MONTH      PIC 9(2).
               10  AM-LAST-ACT-DAY        PIC 9(2).
           05  AM-LAST-STATEMENT-DATE.
               10  AM-LAST-STMT-YEAR      PIC 9(4).
               10  AM-LAST-STMT-MONTH     PIC 9(2).
               10  AM-LAST-STMT-DAY       PIC 9(2).
           05  AM-OVERDRAFT-LIMIT         PIC S9(7)V99 COMP-3.
           05  AM-OVERDRAFT-FEE           PIC S9(3)V99 COMP-3.
           05  AM-MINIMUM-BALANCE         PIC S9(7)V99 COMP-3.
           05  AM-SERVICE-CHARGE-YTD      PIC S9(5)V99 COMP-3.
           05  AM-TRANSACTIONS-MTD        PIC S9(5) COMP-3.
           05  AM-MATURITY-DATE.
               10  AM-MAT-YEAR            PIC 9(4).
               10  AM-MAT-MONTH           PIC 9(2).
               10  AM-MAT-DAY             PIC 9(2).
           05  AM-FILLER                  PIC X(140).
           
       FD  TRANSACTION-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 100 CHARACTERS.
       01  TRANSACTION-RECORD.
           05  TR-TRANSACTION-ID          PIC X(15).
           05  TR-TRANSACTION-TYPE        PIC X(2).
               88  TR-DEPOSIT             VALUE 'DP'.
               88  TR-WITHDRAWAL          VALUE 'WD'.
               88  TR-TRANSFER            VALUE 'TR'.
               88  TR-PAYMENT             VALUE 'PY'.
               88  TR-FEE                 VALUE 'FE'.
               88  TR-INTEREST            VALUE 'IN'.
               88  TR-ADJUSTMENT          VALUE 'AD'.
           05  TR-TRANSACTION-DATE.
               10  TR-TRANS-YEAR          PIC 9(4).
               10  TR-TRANS-MONTH         PIC 9(2).
               10  TR-TRANS-DAY           PIC 9(2).
           05  TR-TRANSACTION-TIME.
               10  TR-TRANS-HOUR          PIC 9(2).
               10  TR-TRANS-MINUTE        PIC 9(2).
               10  TR-TRANS-SECOND        PIC 9(2).
           05  TR-ACCOUNT-NUMBER          PIC X(12).
           05  TR-TO-ACCOUNT-NUMBER       PIC X(12).
           05  TR-AMOUNT                  PIC S9(9)V99 COMP-3.
           05  TR-TRANSACTION-CHANNEL     PIC X(2).
               88  TR-BRANCH              VALUE 'BR'.
               88  TR-ATM                 VALUE 'AT'.
               88  TR-ONLINE              VALUE 'OL'.
               88  TR-MOBILE              VALUE 'MB'.
               88  TR-ACH                 VALUE 'AC'.
               88  TR-WIRE                VALUE 'WR'.
               88  TR-BATCH               VALUE 'BT'.
           05  TR-DESCRIPTION             PIC X(30).
           05  TR-TELLER-ID               PIC X(5).
           05  TR-FILLER                  PIC X(10).
           
       FD  STATEMENT-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS.
       01  STATEMENT-RECORD               PIC X(132).
       
       FD  DAILY-REPORT-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS.
       01  DAILY-REPORT-RECORD            PIC X(132).
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  CUSTOMER-FILE-STATUS       PIC X(2).
               88  CUSTOMER-FILE-SUCCESS  VALUE '00'.
           05  ACCOUNT-FILE-STATUS        PIC X(2).
               88  ACCOUNT-FILE-SUCCESS   VALUE '00'.
               88  ACCOUNT-REC-NOT-FOUND  VALUE '23'.
           05  TRANS-FILE-STATUS          PIC X(2).
               88  TRANS-FILE-SUCCESS     VALUE '00'.
               88  TRANS-FILE-EOF         VALUE '10'.
           05  STMT-FILE-STATUS           PIC X(2).
               88  STMT-FILE-SUCCESS      VALUE '00'.
           05  REPORT-FILE-STATUS         PIC X(2).
               88  REPORT-FILE-SUCCESS    VALUE '00'.
               
       01  WS-COUNTERS.
           05  WS-TRANS-READ              PIC 9(7) VALUE ZEROES.
           05  WS-TRANS-PROCESSED         PIC 9(7) VALUE ZEROES.
           05  WS-TRANS-ERRORS            PIC 9(7) VALUE ZEROES.
           05  WS-DEPOSITS-COUNT          PIC 9(7) VALUE ZEROES.
           05  WS-WITHDRAWALS-COUNT       PIC 9(7) VALUE ZEROES.
           05  WS-TRANSFERS-COUNT         PIC 9(7) VALUE ZEROES.
           05  WS-PAYMENTS-COUNT          PIC 9(7) VALUE ZEROES.
           05  WS-FEES-COUNT              PIC 9(7) VALUE ZEROES.
           05  WS-INTEREST-COUNT          PIC 9(7) VALUE ZEROES.
           05  WS-ADJUSTMENTS-COUNT       PIC 9(7) VALUE ZEROES.
           
       01  WS-AMOUNT-FIELDS.
           05  WS-DEPOSITS-TOTAL          PIC S9(11)V99 VALUE ZEROES.
           05  WS-WITHDRAWALS-TOTAL       PIC S9(11)V99 VALUE ZEROES.
           05  WS-TRANSFERS-TOTAL         PIC S9(11)V99 VALUE ZEROES.
           05  WS-PAYMENTS-TOTAL          PIC S9(11)V99 VALUE ZEROES.
           05  WS-FEES-TOTAL              PIC S9(11)V99 VALUE ZEROES.
           05  WS-INTEREST-TOTAL          PIC S9(11)V99 VALUE ZEROES.
           05  WS-ADJUSTMENTS-TOTAL       PIC S9(11)V99 VALUE ZEROES.
           05  WS-NEW-BALANCE             PIC S9(11)V99 VALUE ZEROES.
           
       01  WS-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR        PIC 9(4).
               10  WS-CURRENT-MONTH       PIC 9(2).
               10  WS-CURRENT-DAY         PIC 9(2).
           05  WS-FORMATTED-DATE          PIC X(10).
           
       01  WS-ERROR-FLAG                  PIC X VALUE 'N'.
           88  WS-ERROR-FOUND             VALUE 'Y'.
           88  WS-NO-ERROR                VALUE 'N'.
           
       01  WS-REPORT-HEADER1.
           05  FILLER                     PIC X(30) VALUE 
                                          'DAILY TRANSACTION REPORT'.
           05  FILLER                     PIC X(40) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE 'DATE:'.
           05  WS-HEADER-DATE             PIC X(10).
           05  FILLER                     PIC X(47) VALUE SPACES.
           
       01  WS-REPORT-HEADER2.
           05  FILLER                     PIC X(15) VALUE 'TRANSACTION TYPE'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'COUNT'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(15) VALUE 'TOTAL AMOUNT'.
           05  FILLER                     PIC X(86) VALUE SPACES.
           
       01  WS-REPORT-DETAIL.
           05  WS-RPT-TRANS-TYPE          PIC X(15).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RPT-COUNT               PIC Z(6)9.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RPT-AMOUNT              PIC $$$,$$$,$$$,$$9.99-.
           05  FILLER                     PIC X(86) VALUE SPACES.
           
       01  WS-REPORT-TOTAL.
           05  FILLER                     PIC X(15) VALUE 'TOTAL'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RPT-TOTAL-COUNT         PIC Z(6)9.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RPT-TOTAL-AMOUNT        PIC $$$,$$$,$$$,$$9.99-.
           05  FILLER                     PIC X(86) VALUE SPACES.
           
       01  WS-STATEMENT-HEADER1.
           05  FILLER                     PIC X(20) VALUE 'ACCOUNT STATEMENT'.
           05  FILLER                     PIC X(50) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE 'DATE:'.
           05  WS-STMT-HEADER-DATE        PIC X(10).
           05  FILLER                     PIC X(47) VALUE SPACES.
           
       01  WS-STATEMENT-HEADER2.
           05  FILLER                     PIC X(12) VALUE 'ACCOUNT NO:'.
           05  WS-STMT-ACCOUNT-NO         PIC X(12).
           05  FILLER                     PIC X(5) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'CUSTOMER:'.
           05  WS-STMT-CUSTOMER-NAME      PIC X(30).
           05  FILLER                     PIC X(63) VALUE SPACES.
           
       01  WS-STATEMENT-HEADER3.
           05  FILLER                     PIC X(10) VALUE 'DATE'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'TRANS ID'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(30) VALUE 'DESCRIPTION'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(15) VALUE 'AMOUNT'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(15) VALUE 'BALANCE'.
           05  FILLER                     PIC X(40) VALUE SPACES.
           
       01  WS-STATEMENT-DETAIL.
           05  WS-STMT-TRANS-DATE         PIC X(10).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-STMT-TRANS-ID           PIC X(10).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-STMT-DESCRIPTION        PIC X(30).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-STMT-AMOUNT             PIC $$$,$$$,$$9.99-.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-STMT-BALANCE            PIC $$$,$$$,$$9.99-.
           05  FILLER                     PIC X(40) VALUE SPACES.
           
       01  WS-STATEMENT-SUMMARY.
           05  FILLER                     PIC X(30) VALUE 
                                          'CURRENT BALANCE:'.
           05  WS-STMT-CURRENT-BALANCE    PIC $$$,$$$,$$9.99-.
           05  FILLER                     PIC X(85) VALUE SPACES.
           
       01  WS-SUMMARY-REPORT.
           05  FILLER                     PIC X(30) 
                                          VALUE 'PROCESSING SUMMARY'.
           05  FILLER                     PIC X(102) VALUE SPACES.
           
       01  WS-SUMMARY-DETAIL1.
           05  FILLER                     PIC X(25) VALUE 
                                          'TRANSACTIONS READ:'.
           05  WS-SUM-TRANS-READ          PIC ZZ,ZZ9.
           05  FILLER                     PIC X(100) VALUE SPACES.
           
       01  WS-SUMMARY-DETAIL2.
           05  FILLER                     PIC X(25) VALUE 
                                          'TRANSACTIONS PROCESSED:'.
           05  WS-SUM-TRANS-PROCESSED     PIC ZZ,ZZ9.
           05  FILLER                     PIC X(100) VALUE SPACES.
           
       01  WS-SUMMARY-DETAIL3.
           05  FILLER                     PIC X(25) VALUE 
                                          'TRANSACTIONS IN ERROR:'.
           05  WS-SUM-TRANS-ERRORS        PIC ZZ,ZZ9.
           05  FILLER                     PIC X(100) VALUE SPACES.
           
       PROCEDURE DIVISION.
       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZATION
           PERFORM 2000-PROCESS-TRANSACTIONS
               UNTIL TRANS-FILE-EOF
           PERFORM 3000-GENERATE-DAILY-REPORT
           PERFORM 4000-TERMINATION
           STOP RUN
           .
           
       1000-INITIALIZATION.
           OPEN INPUT TRANSACTION-FILE
                      CUSTOMER-MASTER-FILE
                I-O   ACCOUNT-MASTER-FILE
                OUTPUT STATEMENT-FILE
                       DAILY-REPORT-FILE
                       
           IF NOT ACCOUNT-FILE-SUCCESS
              DISPLAY 'ERROR OPENING ACCOUNT MASTER FILE: ' 
                      ACCOUNT-FILE-STATUS
              PERFORM 4000-TERMINATION
              STOP RUN
           END-IF
           
           IF NOT CUSTOMER-FILE-SUCCESS
              DISPLAY 'ERROR OPENING CUSTOMER MASTER FILE: ' 
                      CUSTOMER-FILE-STATUS
              PERFORM 4000-TERMINATION
              STOP RUN
           END-IF
           
           IF NOT TRANS-FILE-SUCCESS
              DISPLAY 'ERROR OPENING TRANSACTION FILE: ' 
                      TRANS-FILE-STATUS
              PERFORM 4000-TERMINATION
              STOP RUN
           END-IF
           
           IF NOT STMT-FILE-SUCCESS
              DISPLAY 'ERROR OPENING STATEMENT FILE: ' 
                      STMT-FILE-STATUS
              PERFORM 4000-TERMINATION
              STOP RUN
           END-IF
           
           IF NOT REPORT-FILE-SUCCESS
              DISPLAY 'ERROR OPENING DAILY REPORT FILE: ' 
                      REPORT-FILE-STATUS
              PERFORM 4000-TERMINATION
              STOP RUN
           END-IF
           
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           
           MOVE WS-CURRENT-YEAR TO WS-FORMATTED-DATE(1:4)
           MOVE '-' TO WS-FORMATTED-DATE(5:1)
           MOVE WS-CURRENT-MONTH TO WS-FORMATTED-DATE(6:2)
           MOVE '-' TO WS-FORMATTED-DATE(8:1)
           MOVE WS-CURRENT-DAY TO WS-FORMATTED-DATE(9:2)
           
           MOVE WS-FORMATTED-DATE TO WS-HEADER-DATE
           MOVE WS-FORMATTED-DATE TO WS-STMT-HEADER-DATE
           
           READ TRANSACTION-FILE
               AT END SET TRANS-FILE-EOF TO TRUE
           END-READ
           
           IF TRANS-FILE-SUCCESS
              ADD 1 TO WS-TRANS-READ
           END-IF
           .
           
       2000-PROCESS-TRANSACTIONS.
           MOVE 'N' TO WS-ERROR-FLAG
           
           MOVE TR-ACCOUNT-NUMBER TO AM-ACCOUNT-NUMBER
           
           READ ACCOUNT-MASTER-FILE
               INVALID KEY
                   MOVE 'Y' TO WS-ERROR-FLAG
                   ADD 1 TO WS-TRANS-ERRORS
           END-READ
           
           IF ACCOUNT-FILE-SUCCESS AND WS-NO-ERROR
              EVALUATE TRUE
                  WHEN TR-DEPOSIT
                      ADD TR-AMOUNT TO AM-CURRENT-BALANCE
                      ADD TR-AMOUNT TO AM-AVAILABLE-BALANCE
                      ADD 1 TO WS-DEPOSITS-COUNT
                      ADD TR-AMOUNT TO WS-DEPOSITS-TOTAL
                      
                  WHEN TR-WITHDRAWAL
                      IF TR-AMOUNT > AM-AVAILABLE-BALANCE AND
                         TR-AMOUNT > (AM-AVAILABLE-BALANCE + AM-OVERDRAFT-LIMIT)
                         MOVE 'Y' TO WS-ERROR-FLAG
                         ADD 1 TO WS-TRANS-ERRORS
                      ELSE
                         SUBTRACT TR-AMOUNT FROM AM-CURRENT-BALANCE
                         SUBTRACT TR-AMOUNT FROM AM-AVAILABLE-BALANCE
                         ADD 1 TO WS-WITHDRAWALS-COUNT
                         ADD TR-AMOUNT TO WS-WITHDRAWALS-TOTAL
                         
                         IF AM-AVAILABLE-BALANCE < 0 AND
                            AM-OVERDRAFT-FEE > 0
                            SUBTRACT AM-OVERDRAFT-FEE FROM AM-CURRENT-BALANCE
                            SUBTRACT AM-OVERDRAFT-FEE FROM AM-AVAILABLE-BALANCE
                            ADD AM-OVERDRAFT-FEE TO AM-SERVICE-CHARGE-YTD
                            ADD 1 TO WS-FEES-COUNT
                            ADD AM-OVERDRAFT-FEE TO WS-FEES-TOTAL
                         END-IF
                      END-IF
                      
                  WHEN TR-TRANSFER
                      IF TR-AMOUNT > AM-AVAILABLE-BALANCE
                         MOVE 'Y' TO WS-ERROR-FLAG
                         ADD 1 TO WS-TRANS-ERRORS
                      ELSE
                         SUBTRACT TR-AMOUNT FROM AM-CURRENT-BALANCE
                         SUBTRACT TR-AMOUNT FROM AM-AVAILABLE-BALANCE
                         
                         MOVE TR-TO-ACCOUNT-NUMBER TO AM-ACCOUNT-NUMBER
                         
                         READ ACCOUNT-MASTER-FILE
                             INVALID KEY
                                 MOVE 'Y' TO WS-ERROR-FLAG
                                 ADD 1 TO WS-TRANS-ERRORS
                         END-READ
                         
                         IF ACCOUNT-FILE-SUCCESS AND WS-NO-ERROR
                            ADD TR-AMOUNT TO AM-CURRENT-BALANCE
                            ADD TR-AMOUNT TO AM-AVAILABLE-BALANCE
                            
                            REWRITE ACCOUNT-MASTER-RECORD
                            
                            MOVE TR-ACCOUNT-NUMBER TO AM-ACCOUNT-NUMBER
                            
                            READ ACCOUNT-MASTER-FILE
                            
                            ADD 1 TO WS-TRANSFERS-COUNT
                            ADD TR-AMOUNT TO WS-TRANSFERS-TOTAL
                         END-IF
                      END-IF
                      
                  WHEN TR-PAYMENT
                      SUBTRACT TR-AMOUNT FROM AM-CURRENT-BALANCE
                      SUBTRACT TR-AMOUNT FROM AM-AVAILABLE-BALANCE
                      ADD 1 TO WS-PAYMENTS-COUNT
                      ADD TR-AMOUNT TO WS-PAYMENTS-TOTAL
                      
                  WHEN TR-FEE
                      SUBTRACT TR-AMOUNT FROM AM-CURRENT-BALANCE
                      SUBTRACT TR-AMOUNT FROM AM-AVAILABLE-BALANCE
                      ADD TR-AMOUNT TO AM-SERVICE-CHARGE-YTD
                      ADD 1 TO WS-FEES-COUNT
                      ADD TR-AMOUNT TO WS-FEES-TOTAL
                      
                  WHEN TR-INTEREST
                      ADD TR-AMOUNT TO AM-CURRENT-BALANCE
                      ADD TR-AMOUNT TO AM-AVAILABLE-BALANCE
                      ADD TR-AMOUNT TO AM-INTEREST-YTD
                      ADD 1 TO WS-INTEREST-COUNT
                      ADD TR-AMOUNT TO WS-INTEREST-TOTAL
                      
                  WHEN TR-ADJUSTMENT
                      ADD TR-AMOUNT TO AM-CURRENT-BALANCE
                      ADD TR-AMOUNT TO AM-AVAILABLE-BALANCE
                      ADD 1 TO WS-ADJUSTMENTS-COUNT
                      ADD TR-AMOUNT TO WS-ADJUSTMENTS-TOTAL
              END-EVALUATE
              
              IF WS-NO-ERROR
                 MOVE TR-TRANSACTION-DATE TO AM-LAST-ACTIVITY-DATE
                 ADD 1 TO AM-TRANSACTIONS-MTD
                 
                 REWRITE ACCOUNT-MASTER-RECORD
                 
                 PERFORM 2100-GENERATE-STATEMENT-ENTRY
                 
                 ADD 1 TO WS-TRANS-PROCESSED
              END-IF
           ELSE
              ADD 1 TO WS-TRANS-ERRORS
           END-IF
           
           READ TRANSACTION-FILE
               AT END SET TRANS-FILE-EOF TO TRUE
           END-READ
           
           IF TRANS-FILE-SUCCESS
              ADD 1 TO WS-TRANS-READ
           END-IF
           .
           
       2100-GENERATE-STATEMENT-ENTRY.
           MOVE AM-ACCOUNT-NUMBER TO WS-STMT-ACCOUNT-NO
           
           MOVE AM-CUSTOMER-ID TO CM-CUSTOMER-ID
           
           READ CUSTOMER-MASTER-FILE
               INVALID KEY
                   MOVE SPACES TO WS-STMT-CUSTOMER-NAME
               NOT INVALID KEY
                   STRING CM-LAST-NAME DELIMITED BY SPACE
                          ', ' DELIMITED BY SIZE
                          CM-FIRST-NAME DELIMITED BY SPACE
                          ' ' DELIMITED BY SIZE
                          CM-MIDDLE-INIT DELIMITED BY SPACE
                          INTO WS-STMT-CUSTOMER-NAME
           END-READ
           
           STRING TR-TRANS-YEAR DELIMITED BY SIZE
                  '-' DELIMITED BY SIZE
                  TR-TRANS-MONTH DELIMITED BY SIZE
                  '-' DELIMITED BY SIZE
                  TR-TRANS-DAY DELIMITED BY SIZE
                  INTO WS-STMT-TRANS-DATE
           
           MOVE TR-TRANSACTION-ID(1:10) TO WS-STMT-TRANS-ID
           MOVE TR-DESCRIPTION TO WS-STMT-DESCRIPTION
           MOVE TR-AMOUNT TO WS-STMT-AMOUNT
           MOVE AM-CURRENT-BALANCE TO WS-STMT-BALANCE
           
           WRITE STATEMENT-RECORD FROM WS-STATEMENT-DETAIL
           .
           
       3000-GENERATE-DAILY-REPORT.
           WRITE DAILY-REPORT-RECORD FROM WS-REPORT-HEADER1
           WRITE DAILY-REPORT-RECORD FROM WS-REPORT-HEADER2
           
           MOVE 'DEPOSITS' TO WS-RPT-TRANS-TYPE
           MOVE WS-DEPOSITS-COUNT TO WS-RPT-COUNT
           MOVE WS-DEPOSITS-TOTAL TO WS-RPT-AMOUNT
           WRITE DAILY-REPORT-RECORD FROM WS-REPORT-DETAIL
           
           MOVE 'WITHDRAWALS' TO WS-RPT-TRANS-TYPE
           MOVE WS-WITHDRAWALS-COUNT TO WS-RPT-COUNT
           MOVE WS-WITHDRAWALS-TOTAL TO WS-RPT-AMOUNT
           WRITE DAILY-REPORT-RECORD FROM WS-REPORT-DETAIL
           
           MOVE 'TRANSFERS' TO WS-RPT-TRANS-TYPE
           MOVE WS-TRANSFERS-COUNT TO WS-RPT-COUNT
           MOVE WS-TRANSFERS-TOTAL TO WS-RPT-AMOUNT
           WRITE DAILY-REPORT-RECORD FROM WS-REPORT-DETAIL
           
           MOVE 'PAYMENTS' TO WS-RPT-TRANS-TYPE
           MOVE WS-PAYMENTS-COUNT TO WS-RPT-COUNT
           MOVE WS-PAYMENTS-TOTAL TO WS-RPT-AMOUNT
           WRITE DAILY-REPORT-RECORD FROM WS-REPORT-DETAIL
           
           MOVE 'FEES' TO WS-RPT-TRANS-TYPE
           MOVE WS-FEES-COUNT TO WS-RPT-COUNT
           MOVE WS-FEES-TOTAL TO WS-RPT-AMOUNT
           WRITE DAILY-REPORT-RECORD FROM WS-REPORT-DETAIL
           
           MOVE 'INTEREST' TO WS-RPT-TRANS-TYPE
           MOVE WS-INTEREST-COUNT TO WS-RPT-COUNT
           MOVE WS-INTEREST-TOTAL TO WS-RPT-AMOUNT
           WRITE DAILY-REPORT-RECORD FROM WS-REPORT-DETAIL
           
           MOVE 'ADJUSTMENTS' TO WS-RPT-TRANS-TYPE
           MOVE WS-ADJUSTMENTS-COUNT TO WS-RPT-COUNT
           MOVE WS-ADJUSTMENTS-TOTAL TO WS-RPT-AMOUNT
           WRITE DAILY