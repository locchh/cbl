      *****************************************************************
      * INVENTORY-MANAGEMENT.cbl                                      *
      *                                                               *
      * This program manages inventory for a manufacturing company.   *
      * It processes inventory transactions, updates stock levels,    *
      * generates reorder reports, and maintains production records.  *
      *                                                               *
      * Author: Cascade AI                                            *
      * Date: 2025-03-14                                              *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVENTORY-MANAGEMENT.
       AUTHOR. CASCADE-AI.
       DATE-WRITTEN. 2025-03-14.
       DATE-COMPILED. 2025-03-14.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENTORY-MASTER-FILE ASSIGN TO INVMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS IM-ITEM-ID
               FILE STATUS IS INVENTORY-FILE-STATUS.
               
           SELECT TRANSACTION-FILE ASSIGN TO TRANFILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS TRANS-FILE-STATUS.
               
           SELECT SUPPLIER-FILE ASSIGN TO SUPPFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS SF-SUPPLIER-ID
               FILE STATUS IS SUPPLIER-FILE-STATUS.
               
           SELECT REORDER-REPORT-FILE ASSIGN TO REORDRPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS REORDER-FILE-STATUS.
               
           SELECT TRANSACTION-LOG-FILE ASSIGN TO TRANSLOG
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS LOG-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       
       FD  INVENTORY-MASTER-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 200 CHARACTERS.
       01  INVENTORY-MASTER-RECORD.
           05  IM-ITEM-ID                 PIC X(10).
           05  IM-ITEM-DESCRIPTION        PIC X(50).
           05  IM-CATEGORY                PIC X(15).
           05  IM-LOCATION.
               10  IM-WAREHOUSE           PIC X(5).
               10  IM-AISLE               PIC X(3).
               10  IM-BIN                 PIC X(5).
           05  IM-QUANTITY-ON-HAND        PIC S9(7) COMP-3.
           05  IM-QUANTITY-ALLOCATED      PIC S9(7) COMP-3.
           05  IM-REORDER-POINT           PIC S9(7) COMP-3.
           05  IM-REORDER-QUANTITY        PIC S9(7) COMP-3.
           05  IM-UNIT-COST               PIC S9(7)V99 COMP-3.
           05  IM-LAST-ORDER-DATE.
               10  IM-LAST-ORDER-YEAR     PIC 9(4).
               10  IM-LAST-ORDER-MONTH    PIC 9(2).
               10  IM-LAST-ORDER-DAY      PIC 9(2).
           05  IM-PREFERRED-SUPPLIER-ID   PIC X(10).
           05  IM-LEAD-TIME-DAYS          PIC S9(3) COMP-3.
           05  IM-ABC-CLASS               PIC X.
               88  IM-CLASS-A             VALUE 'A'.
               88  IM-CLASS-B             VALUE 'B'.
               88  IM-CLASS-C             VALUE 'C'.
           05  IM-STATUS                  PIC X.
               88  IM-ACTIVE              VALUE 'A'.
               88  IM-DISCONTINUED        VALUE 'D'.
               88  IM-PENDING             VALUE 'P'.
           05  IM-LAST-CYCLE-COUNT-DATE.
               10  IM-CYCLE-COUNT-YEAR    PIC 9(4).
               10  IM-CYCLE-COUNT-MONTH   PIC 9(2).
               10  IM-CYCLE-COUNT-DAY     PIC 9(2).
           05  IM-FILLER                  PIC X(60).
           
       FD  TRANSACTION-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 100 CHARACTERS.
       01  TRANSACTION-RECORD.
           05  TR-TRANSACTION-ID          PIC X(12).
           05  TR-TRANSACTION-TYPE        PIC X(2).
               88  TR-RECEIPT             VALUE 'RC'.
               88  TR-ISSUE               VALUE 'IS'.
               88  TR-ADJUSTMENT          VALUE 'AD'.
               88  TR-TRANSFER            VALUE 'TR'.
               88  TR-CYCLE-COUNT         VALUE 'CC'.
           05  TR-TRANSACTION-DATE.
               10  TR-TRANS-YEAR          PIC 9(4).
               10  TR-TRANS-MONTH         PIC 9(2).
               10  TR-TRANS-DAY           PIC 9(2).
           05  TR-ITEM-ID                 PIC X(10).
           05  TR-QUANTITY                PIC S9(7) COMP-3.
           05  TR-UNIT-COST               PIC S9(7)V99 COMP-3.
           05  TR-SOURCE-LOCATION         PIC X(13).
           05  TR-DESTINATION-LOCATION    PIC X(13).
           05  TR-REFERENCE-NUMBER        PIC X(15).
           05  TR-USER-ID                 PIC X(8).
           05  TR-REASON-CODE             PIC X(3).
           05  TR-FILLER                  PIC X(15).
           
       FD  SUPPLIER-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 150 CHARACTERS.
       01  SUPPLIER-RECORD.
           05  SF-SUPPLIER-ID             PIC X(10).
           05  SF-SUPPLIER-NAME           PIC X(40).
           05  SF-CONTACT-NAME            PIC X(30).
           05  SF-PHONE-NUMBER            PIC X(15).
           05  SF-EMAIL                   PIC X(30).
           05  SF-LEAD-TIME-DAYS          PIC S9(3) COMP-3.
           05  SF-PERFORMANCE-RATING      PIC 9(1).
           05  SF-FILLER                  PIC X(20).
           
       FD  REORDER-REPORT-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS.
       01  REORDER-REPORT-RECORD          PIC X(132).
       
       FD  TRANSACTION-LOG-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS.
       01  TRANSACTION-LOG-RECORD         PIC X(132).
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  INVENTORY-FILE-STATUS      PIC X(2).
               88  INVENTORY-FILE-SUCCESS VALUE '00'.
           05  TRANS-FILE-STATUS          PIC X(2).
               88  TRANS-FILE-SUCCESS     VALUE '00'.
               88  TRANS-FILE-EOF         VALUE '10'.
           05  SUPPLIER-FILE-STATUS       PIC X(2).
               88  SUPPLIER-FILE-SUCCESS  VALUE '00'.
           05  REORDER-FILE-STATUS        PIC X(2).
               88  REORDER-FILE-SUCCESS   VALUE '00'.
           05  LOG-FILE-STATUS            PIC X(2).
               88  LOG-FILE-SUCCESS       VALUE '00'.
               
       01  WS-COUNTERS.
           05  WS-TRANS-READ              PIC 9(7) VALUE ZEROES.
           05  WS-TRANS-PROCESSED         PIC 9(7) VALUE ZEROES.
           05  WS-TRANS-ERRORS            PIC 9(7) VALUE ZEROES.
           05  WS-REORDER-COUNT           PIC 9(5) VALUE ZEROES.
           
       01  WS-CALCULATION-FIELDS.
           05  WS-NEW-QUANTITY            PIC S9(7) VALUE ZEROES.
           05  WS-AVAILABLE-QUANTITY      PIC S9(7) VALUE ZEROES.
           05  WS-INVENTORY-VALUE         PIC S9(9)V99 VALUE ZEROES.
           
       01  WS-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR        PIC 9(4).
               10  WS-CURRENT-MONTH       PIC 9(2).
               10  WS-CURRENT-DAY         PIC 9(2).
           05  WS-FORMATTED-DATE          PIC X(10).
           
       01  WS-ERROR-FLAG                  PIC X VALUE 'N'.
           88  WS-ERROR-FOUND             VALUE 'Y'.
           88  WS-NO-ERROR                VALUE 'N'.
           
       01  WS-REORDER-HEADER1.
           05  FILLER                     PIC X(25) VALUE 'INVENTORY REORDER REPORT'.
           05  FILLER                     PIC X(45) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE 'DATE:'.
           05  WS-HEADER-DATE             PIC X(10).
           05  FILLER                     PIC X(47) VALUE SPACES.
           
       01  WS-REORDER-HEADER2.
           05  FILLER                     PIC X(10) VALUE 'ITEM ID'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(30) VALUE 'DESCRIPTION'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'ON HAND'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(15) VALUE 'REORDER POINT'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(15) VALUE 'REORDER QTY'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(15) VALUE 'SUPPLIER'.
           05  FILLER                     PIC X(22) VALUE SPACES.
           
       01  WS-REORDER-DETAIL.
           05  WS-RO-ITEM-ID              PIC X(10).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RO-DESCRIPTION          PIC X(30).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RO-QUANTITY-ON-HAND     PIC Z(6)9.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RO-REORDER-POINT        PIC Z(6)9.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RO-REORDER-QUANTITY     PIC Z(6)9.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RO-SUPPLIER-NAME        PIC X(15).
           05  FILLER                     PIC X(22) VALUE SPACES.
           
       01  WS-TRANSACTION-LOG-HEADER1.
           05  FILLER                     PIC X(25) VALUE 'TRANSACTION LOG REPORT'.
           05  FILLER                     PIC X(45) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE 'DATE:'.
           05  WS-LOG-HEADER-DATE         PIC X(10).
           05  FILLER                     PIC X(47) VALUE SPACES.
           
       01  WS-TRANSACTION-LOG-HEADER2.
           05  FILLER                     PIC X(12) VALUE 'TRANS ID'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'ITEM ID'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(4) VALUE 'TYPE'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'DATE'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'QUANTITY'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(15) VALUE 'REFERENCE'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(8) VALUE 'USER ID'.
           05  FILLER                     PIC X(45) VALUE SPACES.
           
       01  WS-TRANSACTION-LOG-DETAIL.
           05  WS-LOG-TRANS-ID            PIC X(12).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-LOG-ITEM-ID             PIC X(10).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-LOG-TRANS-TYPE          PIC X(4).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-LOG-TRANS-DATE          PIC X(10).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-LOG-QUANTITY            PIC Z(6)9-.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-LOG-REFERENCE           PIC X(15).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-LOG-USER-ID             PIC X(8).
           05  FILLER                     PIC X(45) VALUE SPACES.
           
       01  WS-SUMMARY-REPORT.
           05  FILLER                     PIC X(30) 
                                          VALUE 'TRANSACTION PROCESSING SUMMARY'.
           05  FILLER                     PIC X(102) VALUE SPACES.
           
       01  WS-SUMMARY-DETAIL1.
           05  FILLER                     PIC X(25) VALUE 'TRANSACTIONS READ:'.
           05  WS-SUM-TRANS-READ          PIC ZZ,ZZ9.
           05  FILLER                     PIC X(100) VALUE SPACES.
           
       01  WS-SUMMARY-DETAIL2.
           05  FILLER                     PIC X(25) VALUE 'TRANSACTIONS PROCESSED:'.
           05  WS-SUM-TRANS-PROCESSED     PIC ZZ,ZZ9.
           05  FILLER                     PIC X(100) VALUE SPACES.
           
       01  WS-SUMMARY-DETAIL3.
           05  FILLER                     PIC X(25) VALUE 'TRANSACTIONS IN ERROR:'.
           05  WS-SUM-TRANS-ERRORS        PIC ZZ,ZZ9.
           05  FILLER                     PIC X(100) VALUE SPACES.
           
       01  WS-SUMMARY-DETAIL4.
           05  FILLER                     PIC X(25) VALUE 'ITEMS FLAGGED FOR REORDER:'.
           05  WS-SUM-REORDER-COUNT       PIC ZZ,ZZ9.
           05  FILLER                     PIC X(100) VALUE SPACES.
           
       PROCEDURE DIVISION.
       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZATION
           PERFORM 2000-PROCESS-TRANSACTIONS
               UNTIL TRANS-FILE-EOF
           PERFORM 3000-GENERATE-REORDER-REPORT
           PERFORM 4000-TERMINATION
           STOP RUN
           .
           
       1000-INITIALIZATION.
           OPEN INPUT TRANSACTION-FILE
                      SUPPLIER-FILE
                I-O   INVENTORY-MASTER-FILE
                OUTPUT REORDER-REPORT-FILE
                       TRANSACTION-LOG-FILE
                       
           IF NOT INVENTORY-FILE-SUCCESS
              DISPLAY 'ERROR OPENING INVENTORY FILE: ' 
                      INVENTORY-FILE-STATUS
              PERFORM 4000-TERMINATION
              STOP RUN
           END-IF
           
           IF NOT TRANS-FILE-SUCCESS
              DISPLAY 'ERROR OPENING TRANSACTION FILE: ' 
                      TRANS-FILE-STATUS
              PERFORM 4000-TERMINATION
              STOP RUN
           END-IF
           
           IF NOT SUPPLIER-FILE-SUCCESS
              DISPLAY 'ERROR OPENING SUPPLIER FILE: ' 
                      SUPPLIER-FILE-STATUS
              PERFORM 4000-TERMINATION
              STOP RUN
           END-IF
           
           IF NOT REORDER-FILE-SUCCESS
              DISPLAY 'ERROR OPENING REORDER REPORT FILE: ' 
                      REORDER-FILE-STATUS
              PERFORM 4000-TERMINATION
              STOP RUN
           END-IF
           
           IF NOT LOG-FILE-SUCCESS
              DISPLAY 'ERROR OPENING TRANSACTION LOG FILE: ' 
                      LOG-FILE-STATUS
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
           MOVE WS-FORMATTED-DATE TO WS-LOG-HEADER-DATE
           
           WRITE TRANSACTION-LOG-RECORD FROM WS-TRANSACTION-LOG-HEADER1
           WRITE TRANSACTION-LOG-RECORD FROM WS-TRANSACTION-LOG-HEADER2
           
           READ TRANSACTION-FILE
               AT END SET TRANS-FILE-EOF TO TRUE
           END-READ
           
           IF TRANS-FILE-SUCCESS
              ADD 1 TO WS-TRANS-READ
           END-IF
           .
           
       2000-PROCESS-TRANSACTIONS.
           MOVE 'N' TO WS-ERROR-FLAG
           
           PERFORM 2100-VALIDATE-TRANSACTION
           
           IF WS-NO-ERROR
              PERFORM 2200-UPDATE-INVENTORY
              PERFORM 2300-LOG-TRANSACTION
              ADD 1 TO WS-TRANS-PROCESSED
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
           
       2100-VALIDATE-TRANSACTION.
           MOVE TR-ITEM-ID TO IM-ITEM-ID
           
           READ INVENTORY-MASTER-FILE
               INVALID KEY
                   MOVE 'Y' TO WS-ERROR-FLAG
                   MOVE TR-TRANSACTION-ID TO WS-LOG-TRANS-ID
                   MOVE TR-ITEM-ID TO WS-LOG-ITEM-ID
                   MOVE 'ERR' TO WS-LOG-TRANS-TYPE
                   
                   STRING TR-TRANS-YEAR DELIMITED BY SIZE
                          '-' DELIMITED BY SIZE
                          TR-TRANS-MONTH DELIMITED BY SIZE
                          '-' DELIMITED BY SIZE
                          TR-TRANS-DAY DELIMITED BY SIZE
                          INTO WS-LOG-TRANS-DATE
                   
                   MOVE TR-QUANTITY TO WS-LOG-QUANTITY
                   MOVE 'INVALID ITEM ID' TO WS-LOG-REFERENCE
                   MOVE TR-USER-ID TO WS-LOG-USER-ID
                   
                   WRITE TRANSACTION-LOG-RECORD 
                         FROM WS-TRANSACTION-LOG-DETAIL
           END-READ
           
           IF INVENTORY-FILE-SUCCESS AND WS-NO-ERROR
              IF TR-ISSUE AND TR-QUANTITY > IM-QUANTITY-ON-HAND
                 MOVE 'Y' TO WS-ERROR-FLAG
                 MOVE TR-TRANSACTION-ID TO WS-LOG-TRANS-ID
                 MOVE TR-ITEM-ID TO WS-LOG-ITEM-ID
                 MOVE 'ERR' TO WS-LOG-TRANS-TYPE
                 
                 STRING TR-TRANS-YEAR DELIMITED BY SIZE
                        '-' DELIMITED BY SIZE
                        TR-TRANS-MONTH DELIMITED BY SIZE
                        '-' DELIMITED BY SIZE
                        TR-TRANS-DAY DELIMITED BY SIZE
                        INTO WS-LOG-TRANS-DATE
                 
                 MOVE TR-QUANTITY TO WS-LOG-QUANTITY
                 MOVE 'INSUFFICIENT QTY' TO WS-LOG-REFERENCE
                 MOVE TR-USER-ID TO WS-LOG-USER-ID
                 
                 WRITE TRANSACTION-LOG-RECORD 
                       FROM WS-TRANSACTION-LOG-DETAIL
              END-IF
              
              IF NOT IM-ACTIVE
                 MOVE 'Y' TO WS-ERROR-FLAG
                 MOVE TR-TRANSACTION-ID TO WS-LOG-TRANS-ID
                 MOVE TR-ITEM-ID TO WS-LOG-ITEM-ID
                 MOVE 'ERR' TO WS-LOG-TRANS-TYPE
                 
                 STRING TR-TRANS-YEAR DELIMITED BY SIZE
                        '-' DELIMITED BY SIZE
                        TR-TRANS-MONTH DELIMITED BY SIZE
                        '-' DELIMITED BY SIZE
                        TR-TRANS-DAY DELIMITED BY SIZE
                        INTO WS-LOG-TRANS-DATE
                 
                 MOVE TR-QUANTITY TO WS-LOG-QUANTITY
                 MOVE 'INACTIVE ITEM' TO WS-LOG-REFERENCE
                 MOVE TR-USER-ID TO WS-LOG-USER-ID
                 
                 WRITE TRANSACTION-LOG-RECORD 
                       FROM WS-TRANSACTION-LOG-DETAIL
              END-IF
           END-IF
           .
           
       2200-UPDATE-INVENTORY.
           EVALUATE TRUE
               WHEN TR-RECEIPT
                   ADD TR-QUANTITY TO IM-QUANTITY-ON-HAND
                   MOVE TR-TRANSACTION-DATE TO IM-LAST-ORDER-DATE
                   
                   IF TR-UNIT-COST > 0
                      MOVE TR-UNIT-COST TO IM-UNIT-COST
                   END-IF
                   
               WHEN TR-ISSUE
                   SUBTRACT TR-QUANTITY FROM IM-QUANTITY-ON-HAND
                   
               WHEN TR-ADJUSTMENT
                   ADD TR-QUANTITY TO IM-QUANTITY-ON-HAND
                   
               WHEN TR-CYCLE-COUNT
                   MOVE TR-QUANTITY TO IM-QUANTITY-ON-HAND
                   MOVE TR-TRANSACTION-DATE TO IM-LAST-CYCLE-COUNT-DATE
                   
               WHEN TR-TRANSFER
                   IF TR-SOURCE-LOCATION = IM-LOCATION
                      SUBTRACT TR-QUANTITY FROM IM-QUANTITY-ON-HAND
                   END-IF
                   
                   IF TR-DESTINATION-LOCATION = IM-LOCATION
                      ADD TR-QUANTITY TO IM-QUANTITY-ON-HAND
                   END-IF
           END-EVALUATE
           
           REWRITE INVENTORY-MASTER-RECORD
           .
           
       2300-LOG-TRANSACTION.
           MOVE TR-TRANSACTION-ID TO WS-LOG-TRANS-ID
           MOVE TR-ITEM-ID TO WS-LOG-ITEM-ID
           
           EVALUATE TRUE
               WHEN TR-RECEIPT
                   MOVE 'RCPT' TO WS-LOG-TRANS-TYPE
               WHEN TR-ISSUE
                   MOVE 'ISSU' TO WS-LOG-TRANS-TYPE
               WHEN TR-ADJUSTMENT
                   MOVE 'ADJT' TO WS-LOG-TRANS-TYPE
               WHEN TR-TRANSFER
                   MOVE 'TRAN' TO WS-LOG-TRANS-TYPE
               WHEN TR-CYCLE-COUNT
                   MOVE 'CYCL' TO WS-LOG-TRANS-TYPE
           END-EVALUATE
           
           STRING TR-TRANS-YEAR DELIMITED BY SIZE
                  '-' DELIMITED BY SIZE
                  TR-TRANS-MONTH DELIMITED BY SIZE
                  '-' DELIMITED BY SIZE
                  TR-TRANS-DAY DELIMITED BY SIZE
                  INTO WS-LOG-TRANS-DATE
           
           MOVE TR-QUANTITY TO WS-LOG-QUANTITY
           MOVE TR-REFERENCE-NUMBER TO WS-LOG-REFERENCE
           MOVE TR-USER-ID TO WS-LOG-USER-ID
           
           WRITE TRANSACTION-LOG-RECORD FROM WS-TRANSACTION-LOG-DETAIL
           .
           
       3000-GENERATE-REORDER-REPORT.
           WRITE REORDER-REPORT-RECORD FROM WS-REORDER-HEADER1
           WRITE REORDER-REPORT-RECORD FROM WS-REORDER-HEADER2
           
           MOVE LOW-VALUES TO IM-ITEM-ID
           
           START INVENTORY-MASTER-FILE KEY >= IM-ITEM-ID
           
           READ INVENTORY-MASTER-FILE NEXT
               AT END MOVE HIGH-VALUES TO IM-ITEM-ID
           END-READ
           
           PERFORM UNTIL IM-ITEM-ID = HIGH-VALUES
              IF IM-ACTIVE AND 
                 IM-QUANTITY-ON-HAND <= IM-REORDER-POINT
                 
                 MOVE IM-ITEM-ID TO WS-RO-ITEM-ID
                 MOVE IM-ITEM-DESCRIPTION TO WS-RO-DESCRIPTION
                 MOVE IM-QUANTITY-ON-HAND TO WS-RO-QUANTITY-ON-HAND
                 MOVE IM-REORDER-POINT TO WS-RO-REORDER-POINT
                 MOVE IM-REORDER-QUANTITY TO WS-RO-REORDER-QUANTITY
                 
                 MOVE IM-PREFERRED-SUPPLIER-ID TO SF-SUPPLIER-ID
                 
                 READ SUPPLIER-FILE
                     INVALID KEY
                         MOVE 'UNKNOWN' TO WS-RO-SUPPLIER-NAME
                     NOT INVALID KEY
                         MOVE SF-SUPPLIER-NAME(1:15) TO WS-RO-SUPPLIER-NAME
                 END-READ
                 
                 WRITE REORDER-REPORT-RECORD FROM WS-REORDER-DETAIL
                 
                 ADD 1 TO WS-REORDER-COUNT
              END-IF
              
              READ INVENTORY-MASTER-FILE NEXT
                  AT END MOVE HIGH-VALUES TO IM-ITEM-ID
              END-READ
           END-PERFORM
           .
           
       4000-TERMINATION.
           MOVE WS-TRANS-READ TO WS-SUM-TRANS-READ
           MOVE WS-TRANS-PROCESSED TO WS-SUM-TRANS-PROCESSED
           MOVE WS-TRANS-ERRORS TO WS-SUM-TRANS-ERRORS
           MOVE WS-REORDER-COUNT TO WS-SUM-REORDER-COUNT
           
           WRITE TRANSACTION-LOG-RECORD FROM SPACES
           WRITE TRANSACTION-LOG-RECORD FROM WS-SUMMARY-REPORT
           WRITE TRANSACTION-LOG-RECORD FROM WS-SUMMARY-DETAIL1
           WRITE TRANSACTION-LOG-RECORD FROM WS-SUMMARY-DETAIL2
           WRITE TRANSACTION-LOG-RECORD FROM WS-SUMMARY-DETAIL3
           WRITE TRANSACTION-LOG-RECORD FROM WS-SUMMARY-DETAIL4
           
           CLOSE INVENTORY-MASTER-FILE
                 TRANSACTION-FILE
                 SUPPLIER-FILE
                 REORDER-REPORT-FILE
                 TRANSACTION-LOG-FILE
           .
