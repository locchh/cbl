      *****************************************************************
      * AUTOMOTIVE-PARTS-SYSTEM.cbl                                   *
      *                                                               *
      * This program manages automotive parts inventory, pricing,     *
      * and sales for a dealership or auto parts retailer.            *
      * It processes parts transactions, tracks inventory levels,     *
      * and generates reports for management.                         *
      *                                                               *
      * Author: Cascade AI                                            *
      * Date: 2025-03-14                                              *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AUTOMOTIVE-PARTS-SYSTEM.
       AUTHOR. CASCADE-AI.
       DATE-WRITTEN. 2025-03-14.
       DATE-COMPILED. 2025-03-14.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARTS-MASTER-FILE ASSIGN TO PARTMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PM-PART-NUMBER
               ALTERNATE RECORD KEY IS PM-INTERCHANGE-NUMBER
                   WITH DUPLICATES
               FILE STATUS IS PARTS-FILE-STATUS.
               
           SELECT VEHICLE-MASTER-FILE ASSIGN TO VEHMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS VM-VEHICLE-ID
               FILE STATUS IS VEHICLE-FILE-STATUS.
               
           SELECT TRANSACTION-FILE ASSIGN TO TRANFILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS TRANS-FILE-STATUS.
               
           SELECT PARTS-REPORT-FILE ASSIGN TO PARTREPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS REPORT-FILE-STATUS.
               
           SELECT BACKORDER-REPORT-FILE ASSIGN TO BACKREPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS BACKORD-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       
       FD  PARTS-MASTER-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 250 CHARACTERS.
       01  PARTS-MASTER-RECORD.
           05  PM-PART-NUMBER             PIC X(15).
           05  PM-PART-DESCRIPTION        PIC X(40).
           05  PM-INTERCHANGE-NUMBER      PIC X(15).
           05  PM-MANUFACTURER            PIC X(20).
           05  PM-CATEGORY                PIC X(10).
               88  PM-ENGINE              VALUE 'ENGINE'.
               88  PM-TRANSMISSION        VALUE 'TRANSMISS'.
               88  PM-BRAKE               VALUE 'BRAKE'.
               88  PM-SUSPENSION          VALUE 'SUSPENSN'.
               88  PM-ELECTRICAL          VALUE 'ELECTRIC'.
               88  PM-BODY                VALUE 'BODY'.
               88  PM-HVAC                VALUE 'HVAC'.
           05  PM-LOCATION.
               10  PM-WAREHOUSE           PIC X(3).
               10  PM-AISLE               PIC X(3).
               10  PM-BIN                 PIC X(4).
           05  PM-QUANTITY-ON-HAND        PIC S9(5) COMP-3.
           05  PM-QUANTITY-ALLOCATED      PIC S9(5) COMP-3.
           05  PM-REORDER-POINT           PIC S9(5) COMP-3.
           05  PM-REORDER-QUANTITY        PIC S9(5) COMP-3.
           05  PM-COST-PRICE              PIC S9(5)V99 COMP-3.
           05  PM-RETAIL-PRICE            PIC S9(5)V99 COMP-3.
           05  PM-DEALER-PRICE            PIC S9(5)V99 COMP-3.
           05  PM-LAST-SALE-DATE.
               10  PM-LAST-SALE-YEAR      PIC 9(4).
               10  PM-LAST-SALE-MONTH     PIC 9(2).
               10  PM-LAST-SALE-DAY       PIC 9(2).
           05  PM-YTD-SALES-QUANTITY      PIC S9(7) COMP-3.
           05  PM-YTD-SALES-AMOUNT        PIC S9(7)V99 COMP-3.
           05  PM-VEHICLE-APPLICATIONS    OCCURS 5 TIMES.
               10  PM-VEHICLE-MAKE        PIC X(10).
               10  PM-VEHICLE-MODEL       PIC X(10).
               10  PM-YEAR-FROM           PIC 9(4).
               10  PM-YEAR-TO             PIC 9(4).
           05  PM-WARRANTY-MONTHS         PIC 9(2).
           05  PM-CORE-CHARGE             PIC S9(5)V99 COMP-3.
           05  PM-STATUS                  PIC X.
               88  PM-ACTIVE              VALUE 'A'.
               88  PM-DISCONTINUED        VALUE 'D'.
               88  PM-SEASONAL            VALUE 'S'.
           05  PM-FILLER                  PIC X(10).
           
       FD  VEHICLE-MASTER-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 150 CHARACTERS.
       01  VEHICLE-MASTER-RECORD.
           05  VM-VEHICLE-ID              PIC X(17).
           05  VM-MAKE                    PIC X(15).
           05  VM-MODEL                   PIC X(20).
           05  VM-YEAR                    PIC 9(4).
           05  VM-ENGINE-TYPE             PIC X(15).
           05  VM-TRANSMISSION-TYPE       PIC X(10).
           05  VM-BODY-STYLE              PIC X(10).
           05  VM-TRIM-LEVEL              PIC X(10).
           05  VM-VIN-PATTERN             PIC X(17).
           05  VM-FILLER                  PIC X(32).
           
       FD  TRANSACTION-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 100 CHARACTERS.
       01  TRANSACTION-RECORD.
           05  TR-TRANSACTION-CODE        PIC X.
               88  TR-SALE                VALUE 'S'.
               88  TR-RETURN              VALUE 'R'.
               88  TR-RECEIPT             VALUE 'P'.
               88  TR-ADJUSTMENT          VALUE 'A'.
               88  TR-BACKORDER           VALUE 'B'.
           05  TR-PART-NUMBER             PIC X(15).
           05  TR-TRANSACTION-DATE.
               10  TR-TRANS-YEAR          PIC 9(4).
               10  TR-TRANS-MONTH         PIC 9(2).
               10  TR-TRANS-DAY           PIC 9(2).
           05  TR-QUANTITY                PIC S9(5) COMP-3.
           05  TR-UNIT-PRICE              PIC S9(5)V99 COMP-3.
           05  TR-CUSTOMER-TYPE           PIC X.
               88  TR-RETAIL              VALUE 'R'.
               88  TR-DEALER              VALUE 'D'.
               88  TR-WHOLESALE           VALUE 'W'.
           05  TR-INVOICE-NUMBER          PIC X(10).
           05  TR-VEHICLE-ID              PIC X(17).
           05  TR-EMPLOYEE-ID             PIC X(8).
           05  TR-REASON-CODE             PIC X(3).
           05  TR-FILLER                  PIC X(25).
           
       FD  PARTS-REPORT-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS.
       01  PARTS-REPORT-RECORD            PIC X(132).
       
       FD  BACKORDER-REPORT-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS.
       01  BACKORDER-REPORT-RECORD        PIC X(132).
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  PARTS-FILE-STATUS          PIC X(2).
               88  PARTS-FILE-SUCCESS     VALUE '00'.
               88  PARTS-REC-NOT-FOUND    VALUE '23'.
           05  VEHICLE-FILE-STATUS        PIC X(2).
               88  VEHICLE-FILE-SUCCESS   VALUE '00'.
           05  TRANS-FILE-STATUS          PIC X(2).
               88  TRANS-FILE-SUCCESS     VALUE '00'.
               88  TRANS-FILE-EOF         VALUE '10'.
           05  REPORT-FILE-STATUS         PIC X(2).
               88  REPORT-FILE-SUCCESS    VALUE '00'.
           05  BACKORD-FILE-STATUS        PIC X(2).
               88  BACKORD-FILE-SUCCESS   VALUE '00'.
               
       01  WS-COUNTERS.
           05  WS-TRANS-READ              PIC 9(7) VALUE ZEROES.
           05  WS-TRANS-PROCESSED         PIC 9(7) VALUE ZEROES.
           05  WS-TRANS-ERRORS            PIC 9(7) VALUE ZEROES.
           05  WS-BACKORDERS              PIC 9(5) VALUE ZEROES.
           05  WS-PARTS-BELOW-REORDER     PIC 9(5) VALUE ZEROES.
           
       01  WS-CALCULATION-FIELDS.
           05  WS-NEW-QUANTITY            PIC S9(5) VALUE ZEROES.
           05  WS-AVAILABLE-QUANTITY      PIC S9(5) VALUE ZEROES.
           05  WS-INVENTORY-VALUE         PIC S9(9)V99 VALUE ZEROES.
           05  WS-SALE-AMOUNT             PIC S9(7)V99 VALUE ZEROES.
           
       01  WS-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR        PIC 9(4).
               10  WS-CURRENT-MONTH       PIC 9(2).
               10  WS-CURRENT-DAY         PIC 9(2).
           05  WS-FORMATTED-DATE          PIC X(10).
           
       01  WS-ERROR-FLAG                  PIC X VALUE 'N'.
           88  WS-ERROR-FOUND             VALUE 'Y'.
           88  WS-NO-ERROR                VALUE 'N'.
           
       01  WS-PARTS-HEADER1.
           05  FILLER                     PIC X(30) VALUE 
                                          'AUTOMOTIVE PARTS INVENTORY REPORT'.
           05  FILLER                     PIC X(40) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE 'DATE:'.
           05  WS-HEADER-DATE             PIC X(10).
           05  FILLER                     PIC X(47) VALUE SPACES.
           
       01  WS-PARTS-HEADER2.
           05  FILLER                     PIC X(15) VALUE 'PART NUMBER'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(20) VALUE 'DESCRIPTION'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'CATEGORY'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'ON HAND'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'ALLOCATED'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'AVAILABLE'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(12) VALUE 'RETAIL PRICE'.
           05  FILLER                     PIC X(27) VALUE SPACES.
           
       01  WS-PARTS-DETAIL.
           05  WS-PRT-PART-NUMBER         PIC X(15).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-PRT-DESCRIPTION         PIC X(20).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-PRT-CATEGORY            PIC X(10).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-PRT-ON-HAND             PIC Z(4)9.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-PRT-ALLOCATED           PIC Z(4)9.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-PRT-AVAILABLE           PIC Z(4)9.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-PRT-RETAIL-PRICE        PIC $$$,$$9.99.
           05  FILLER                     PIC X(27) VALUE SPACES.
           
       01  WS-BACKORDER-HEADER1.
           05  FILLER                     PIC X(30) VALUE 
                                          'AUTOMOTIVE PARTS BACKORDER REPORT'.
           05  FILLER                     PIC X(40) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE 'DATE:'.
           05  WS-BO-HEADER-DATE          PIC X(10).
           05  FILLER                     PIC X(47) VALUE SPACES.
           
       01  WS-BACKORDER-HEADER2.
           05  FILLER                     PIC X(15) VALUE 'PART NUMBER'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(20) VALUE 'DESCRIPTION'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'QUANTITY'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'DATE'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'INVOICE'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'VEHICLE ID'.
           05  FILLER                     PIC X(42) VALUE SPACES.
           
       01  WS-BACKORDER-DETAIL.
           05  WS-BO-PART-NUMBER          PIC X(15).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-BO-DESCRIPTION          PIC X(20).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-BO-QUANTITY             PIC Z(4)9.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-BO-DATE                 PIC X(10).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-BO-INVOICE              PIC X(10).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-BO-VEHICLE-ID           PIC X(17).
           05  FILLER                     PIC X(35) VALUE SPACES.
           
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
           
       01  WS-SUMMARY-DETAIL4.
           05  FILLER                     PIC X(25) VALUE 
                                          'BACKORDERS CREATED:'.
           05  WS-SUM-BACKORDERS          PIC ZZ,ZZ9.
           05  FILLER                     PIC X(100) VALUE SPACES.
           
       01  WS-SUMMARY-DETAIL5.
           05  FILLER                     PIC X(25) VALUE 
                                          'PARTS BELOW REORDER:'.
           05  WS-SUM-BELOW-REORDER       PIC ZZ,ZZ9.
           05  FILLER                     PIC X(100) VALUE SPACES.
           
       PROCEDURE DIVISION.
       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZATION
           PERFORM 2000-PROCESS-TRANSACTIONS
               UNTIL TRANS-FILE-EOF
           PERFORM 3000-GENERATE-PARTS-REPORT
           PERFORM 4000-TERMINATION
           STOP RUN
           .
           
       1000-INITIALIZATION.
           OPEN INPUT TRANSACTION-FILE
                      VEHICLE-MASTER-FILE
                I-O   PARTS-MASTER-FILE
                OUTPUT PARTS-REPORT-FILE
                       BACKORDER-REPORT-FILE
                       
           IF NOT PARTS-FILE-SUCCESS
              DISPLAY 'ERROR OPENING PARTS MASTER FILE: ' 
                      PARTS-FILE-STATUS
              PERFORM 4000-TERMINATION
              STOP RUN
           END-IF
           
           IF NOT VEHICLE-FILE-SUCCESS
              DISPLAY 'ERROR OPENING VEHICLE MASTER FILE: ' 
                      VEHICLE-FILE-STATUS
              PERFORM 4000-TERMINATION
              STOP RUN
           END-IF
           
           IF NOT TRANS-FILE-SUCCESS
              DISPLAY 'ERROR OPENING TRANSACTION FILE: ' 
                      TRANS-FILE-STATUS
              PERFORM 4000-TERMINATION
              STOP RUN
           END-IF
           
           IF NOT REPORT-FILE-SUCCESS
              DISPLAY 'ERROR OPENING PARTS REPORT FILE: ' 
                      REPORT-FILE-STATUS
              PERFORM 4000-TERMINATION
              STOP RUN
           END-IF
           
           IF NOT BACKORD-FILE-SUCCESS
              DISPLAY 'ERROR OPENING BACKORDER REPORT FILE: ' 
                      BACKORD-FILE-STATUS
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
           MOVE WS-FORMATTED-DATE TO WS-BO-HEADER-DATE
           
           WRITE BACKORDER-REPORT-RECORD FROM WS-BACKORDER-HEADER1
           WRITE BACKORDER-REPORT-RECORD FROM WS-BACKORDER-HEADER2
           
           READ TRANSACTION-FILE
               AT END SET TRANS-FILE-EOF TO TRUE
           END-READ
           
           IF TRANS-FILE-SUCCESS
              ADD 1 TO WS-TRANS-READ
           END-IF
           .
           
       2000-PROCESS-TRANSACTIONS.
           MOVE 'N' TO WS-ERROR-FLAG
           
           MOVE TR-PART-NUMBER TO PM-PART-NUMBER
           
           READ PARTS-MASTER-FILE
               INVALID KEY
                   MOVE 'Y' TO WS-ERROR-FLAG
                   ADD 1 TO WS-TRANS-ERRORS
           END-READ
           
           IF PARTS-FILE-SUCCESS AND WS-NO-ERROR
              EVALUATE TRUE
                  WHEN TR-SALE
                      IF TR-QUANTITY > PM-QUANTITY-ON-HAND
                         IF TR-BACKORDER
                            PERFORM 2100-CREATE-BACKORDER
                         ELSE
                            MOVE 'Y' TO WS-ERROR-FLAG
                            ADD 1 TO WS-TRANS-ERRORS
                         END-IF
                      ELSE
                         SUBTRACT TR-QUANTITY FROM PM-QUANTITY-ON-HAND
                         ADD TR-QUANTITY TO PM-QUANTITY-ALLOCATED
                         
                         EVALUATE TRUE
                             WHEN TR-RETAIL
                                 COMPUTE WS-SALE-AMOUNT = 
                                         TR-QUANTITY * PM-RETAIL-PRICE
                             WHEN TR-DEALER
                                 COMPUTE WS-SALE-AMOUNT = 
                                         TR-QUANTITY * PM-DEALER-PRICE
                             WHEN TR-WHOLESALE
                                 COMPUTE WS-SALE-AMOUNT = 
                                         TR-QUANTITY * PM-COST-PRICE * 1.15
                         END-EVALUATE
                         
                         ADD TR-QUANTITY TO PM-YTD-SALES-QUANTITY
                         ADD WS-SALE-AMOUNT TO PM-YTD-SALES-AMOUNT
                         MOVE TR-TRANSACTION-DATE TO PM-LAST-SALE-DATE
                      END-IF
                      
                  WHEN TR-RETURN
                      ADD TR-QUANTITY TO PM-QUANTITY-ON-HAND
                      
                  WHEN TR-RECEIPT
                      ADD TR-QUANTITY TO PM-QUANTITY-ON-HAND
                      
                      IF TR-UNIT-PRICE > 0
                         MOVE TR-UNIT-PRICE TO PM-COST-PRICE
                         COMPUTE PM-RETAIL-PRICE = TR-UNIT-PRICE * 1.5
                         COMPUTE PM-DEALER-PRICE = TR-UNIT-PRICE * 1.25
                      END-IF
                      
                  WHEN TR-ADJUSTMENT
                      ADD TR-QUANTITY TO PM-QUANTITY-ON-HAND
              END-EVALUATE
              
              IF WS-NO-ERROR
                 REWRITE PARTS-MASTER-RECORD
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
           
       2100-CREATE-BACKORDER.
           MOVE PM-PART-NUMBER TO WS-BO-PART-NUMBER
           MOVE PM-PART-DESCRIPTION(1:20) TO WS-BO-DESCRIPTION
           MOVE TR-QUANTITY TO WS-BO-QUANTITY
           
           STRING TR-TRANS-YEAR DELIMITED BY SIZE
                  '-' DELIMITED BY SIZE
                  TR-TRANS-MONTH DELIMITED BY SIZE
                  '-' DELIMITED BY SIZE
                  TR-TRANS-DAY DELIMITED BY SIZE
                  INTO WS-BO-DATE
           
           MOVE TR-INVOICE-NUMBER TO WS-BO-INVOICE
           MOVE TR-VEHICLE-ID TO WS-BO-VEHICLE-ID
           
           WRITE BACKORDER-REPORT-RECORD FROM WS-BACKORDER-DETAIL
           
           ADD 1 TO WS-BACKORDERS
           ADD 1 TO WS-TRANS-PROCESSED
           .
           
       3000-GENERATE-PARTS-REPORT.
           MOVE ZEROES TO WS-PARTS-BELOW-REORDER
           
           WRITE PARTS-REPORT-RECORD FROM WS-PARTS-HEADER1
           WRITE PARTS-REPORT-RECORD FROM WS-PARTS-HEADER2
           
           MOVE LOW-VALUES TO PM-PART-NUMBER
           
           START PARTS-MASTER-FILE KEY >= PM-PART-NUMBER
           
           READ PARTS-MASTER-FILE NEXT
               AT END MOVE HIGH-VALUES TO PM-PART-NUMBER
           END-READ
           
           PERFORM UNTIL PM-PART-NUMBER = HIGH-VALUES
              MOVE PM-PART-NUMBER TO WS-PRT-PART-NUMBER
              MOVE PM-PART-DESCRIPTION(1:20) TO WS-PRT-DESCRIPTION
              MOVE PM-CATEGORY TO WS-PRT-CATEGORY
              MOVE PM-QUANTITY-ON-HAND TO WS-PRT-ON-HAND
              MOVE PM-QUANTITY-ALLOCATED TO WS-PRT-ALLOCATED
              
              COMPUTE WS-AVAILABLE-QUANTITY = 
                      PM-QUANTITY-ON-HAND - PM-QUANTITY-ALLOCATED
              
              MOVE WS-AVAILABLE-QUANTITY TO WS-PRT-AVAILABLE
              MOVE PM-RETAIL-PRICE TO WS-PRT-RETAIL-PRICE
              
              WRITE PARTS-REPORT-RECORD FROM WS-PARTS-DETAIL
              
              IF PM-ACTIVE AND 
                 PM-QUANTITY-ON-HAND <= PM-REORDER-POINT
                 ADD 1 TO WS-PARTS-BELOW-REORDER
              END-IF
              
              READ PARTS-MASTER-FILE NEXT
                  AT END MOVE HIGH-VALUES TO PM-PART-NUMBER
              END-READ
           END-PERFORM
           .
           
       4000-TERMINATION.
           MOVE WS-TRANS-READ TO WS-SUM-TRANS-READ
           MOVE WS-TRANS-PROCESSED TO WS-SUM-TRANS-PROCESSED
           MOVE WS-TRANS-ERRORS TO WS-SUM-TRANS-ERRORS
           MOVE WS-BACKORDERS TO WS-SUM-BACKORDERS
           MOVE WS-PARTS-BELOW-REORDER TO WS-SUM-BELOW-REORDER
           
           WRITE PARTS-REPORT-RECORD FROM SPACES
           WRITE PARTS-REPORT-RECORD FROM WS-SUMMARY-REPORT
           WRITE PARTS-REPORT-RECORD FROM WS-SUMMARY-DETAIL1
           WRITE PARTS-REPORT-RECORD FROM WS-SUMMARY-DETAIL2
           WRITE PARTS-REPORT-RECORD FROM WS-SUMMARY-DETAIL3
           WRITE PARTS-REPORT-RECORD FROM WS-SUMMARY-DETAIL4
           WRITE PARTS-REPORT-RECORD FROM WS-SUMMARY-DETAIL5
           
           CLOSE PARTS-MASTER-FILE
                 VEHICLE-MASTER-FILE
                 TRANSACTION-FILE
                 PARTS-REPORT-FILE
                 BACKORDER-REPORT-FILE
           .
