      *****************************************************************
      * RETAIL-POS-SYSTEM.cbl                                         *
      *                                                               *
      * This program implements a retail point-of-sale system that    *
      * processes sales transactions, manages inventory, and          *
      * generates sales reports for a retail store.                   *
      *                                                               *
      * Author: Cascade AI                                            *
      * Date: 2025-03-14                                              *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RETAIL-POS-SYSTEM.
       AUTHOR. CASCADE-AI.
       DATE-WRITTEN. 2025-03-14.
       DATE-COMPILED. 2025-03-14.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRODUCT-MASTER-FILE ASSIGN TO PRODMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PM-PRODUCT-ID
               ALTERNATE RECORD KEY IS PM-UPC-CODE
                   WITH DUPLICATES
               FILE STATUS IS PRODUCT-FILE-STATUS.
               
           SELECT SALES-TRANSACTION-FILE ASSIGN TO SALEFILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS SALES-FILE-STATUS.
               
           SELECT DAILY-SALES-REPORT-FILE ASSIGN TO SALEREPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS REPORT-FILE-STATUS.
               
           SELECT INVENTORY-UPDATE-FILE ASSIGN TO INVUPDT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS INVUPDT-FILE-STATUS.
               
           SELECT CUSTOMER-MASTER-FILE ASSIGN TO CUSTMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CM-CUSTOMER-ID
               FILE STATUS IS CUSTOMER-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       
       FD  PRODUCT-MASTER-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 250 CHARACTERS.
       01  PRODUCT-MASTER-RECORD.
           05  PM-PRODUCT-ID              PIC X(10).
           05  PM-UPC-CODE                PIC X(13).
           05  PM-PRODUCT-DESCRIPTION     PIC X(40).
           05  PM-DEPARTMENT              PIC X(3).
           05  PM-CATEGORY                PIC X(5).
           05  PM-SUB-CATEGORY            PIC X(5).
           05  PM-VENDOR-ID               PIC X(6).
           05  PM-COST-PRICE              PIC S9(5)V99 COMP-3.
           05  PM-RETAIL-PRICE            PIC S9(5)V99 COMP-3.
           05  PM-SALE-PRICE              PIC S9(5)V99 COMP-3.
           05  PM-TAX-RATE                PIC SV999 COMP-3.
           05  PM-QUANTITY-ON-HAND        PIC S9(5) COMP-3.
           05  PM-REORDER-POINT           PIC S9(5) COMP-3.
           05  PM-REORDER-QUANTITY        PIC S9(5) COMP-3.
           05  PM-LOCATION.
               10  PM-AISLE               PIC X(3).
               10  PM-SHELF               PIC X(3).
               10  PM-BIN                 PIC X(3).
           05  PM-LAST-RESTOCK-DATE.
               10  PM-RESTOCK-YEAR        PIC 9(4).
               10  PM-RESTOCK-MONTH       PIC 9(2).
               10  PM-RESTOCK-DAY         PIC 9(2).
           05  PM-LAST-SALE-DATE.
               10  PM-LAST-SALE-YEAR      PIC 9(4).
               10  PM-LAST-SALE-MONTH     PIC 9(2).
               10  PM-LAST-SALE-DAY       PIC 9(2).
           05  PM-YTD-SALES-QUANTITY      PIC S9(7) COMP-3.
           05  PM-YTD-SALES-AMOUNT        PIC S9(7)V99 COMP-3.
           05  PM-MTD-SALES-QUANTITY      PIC S9(7) COMP-3.
           05  PM-MTD-SALES-AMOUNT        PIC S9(7)V99 COMP-3.
           05  PM-PRODUCT-STATUS          PIC X.
               88  PM-ACTIVE              VALUE 'A'.
               88  PM-DISCONTINUED        VALUE 'D'.
               88  PM-SEASONAL            VALUE 'S'.
           05  PM-TAXABLE-FLAG            PIC X.
               88  PM-TAXABLE             VALUE 'Y'.
               88  PM-NON-TAXABLE         VALUE 'N'.
           05  PM-DISCOUNTABLE-FLAG       PIC X.
               88  PM-DISCOUNTABLE        VALUE 'Y'.
               88  PM-NON-DISCOUNTABLE    VALUE 'N'.
           05  PM-SALE-START-DATE.
               10  PM-SALE-START-YEAR     PIC 9(4).
               10  PM-SALE-START-MONTH    PIC 9(2).
               10  PM-SALE-START-DAY      PIC 9(2).
           05  PM-SALE-END-DATE.
               10  PM-SALE-END-YEAR       PIC 9(4).
               10  PM-SALE-END-MONTH      PIC 9(2).
               10  PM-SALE-END-DAY        PIC 9(2).
           05  PM-FILLER                  PIC X(90).
           
       FD  SALES-TRANSACTION-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 100 CHARACTERS.
       01  SALES-TRANSACTION-RECORD.
           05  ST-TRANSACTION-ID          PIC X(12).
           05  ST-REGISTER-NUMBER         PIC X(3).
           05  ST-CASHIER-ID              PIC X(5).
           05  ST-TRANSACTION-DATE.
               10  ST-TRANS-YEAR          PIC 9(4).
               10  ST-TRANS-MONTH         PIC 9(2).
               10  ST-TRANS-DAY           PIC 9(2).
           05  ST-TRANSACTION-TIME.
               10  ST-TRANS-HOUR          PIC 9(2).
               10  ST-TRANS-MINUTE        PIC 9(2).
               10  ST-TRANS-SECOND        PIC 9(2).
           05  ST-PRODUCT-ID              PIC X(10).
           05  ST-UPC-CODE                PIC X(13).
           05  ST-QUANTITY                PIC S9(3) COMP-3.
           05  ST-UNIT-PRICE              PIC S9(5)V99 COMP-3.
           05  ST-DISCOUNT-PERCENT        PIC SV99 COMP-3.
           05  ST-EXTENDED-PRICE          PIC S9(7)V99 COMP-3.
           05  ST-TAX-AMOUNT              PIC S9(5)V99 COMP-3.
           05  ST-CUSTOMER-ID             PIC X(10).
           05  ST-PAYMENT-METHOD          PIC X(2).
               88  ST-CASH                VALUE 'CA'.
               88  ST-CREDIT              VALUE 'CR'.
               88  ST-DEBIT               VALUE 'DB'.
               88  ST-GIFT-CARD           VALUE 'GC'.
               88  ST-STORE-CREDIT        VALUE 'SC'.
           05  ST-FILLER                  PIC X(10).
           
       FD  DAILY-SALES-REPORT-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS.
       01  DAILY-SALES-REPORT-RECORD      PIC X(132).
       
       FD  INVENTORY-UPDATE-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 50 CHARACTERS.
       01  INVENTORY-UPDATE-RECORD.
           05  IU-PRODUCT-ID              PIC X(10).
           05  IU-UPDATE-TYPE             PIC X.
               88  IU-SALE                VALUE 'S'.
               88  IU-RETURN              VALUE 'R'.
               88  IU-ADJUSTMENT          VALUE 'A'.
               88  IU-RESTOCK             VALUE 'T'.
           05  IU-QUANTITY                PIC S9(5) COMP-3.
           05  IU-TRANSACTION-DATE.
               10  IU-TRANS-YEAR          PIC 9(4).
               10  IU-TRANS-MONTH         PIC 9(2).
               10  IU-TRANS-DAY           PIC 9(2).
           05  IU-TRANSACTION-ID          PIC X(12).
           05  IU-FILLER                  PIC X(12).
           
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
           05  CM-LOYALTY-POINTS          PIC 9(7) COMP-3.
           05  CM-YTD-PURCHASES           PIC S9(7)V99 COMP-3.
           05  CM-CUSTOMER-SINCE.
               10  CM-CUST-SINCE-YEAR     PIC 9(4).
               10  CM-CUST-SINCE-MONTH    PIC 9(2).
               10  CM-CUST-SINCE-DAY      PIC 9(2).
           05  CM-LAST-PURCHASE-DATE.
               10  CM-LAST-PURCH-YEAR     PIC 9(4).
               10  CM-LAST-PURCH-MONTH    PIC 9(2).
               10  CM-LAST-PURCH-DAY      PIC 9(2).
           05  CM-CUSTOMER-TYPE           PIC X.
               88  CM-REGULAR             VALUE 'R'.
               88  CM-GOLD                VALUE 'G'.
               88  CM-PLATINUM            VALUE 'P'.
           05  CM-FILLER                  PIC X(20).
           
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  PRODUCT-FILE-STATUS        PIC X(2).
               88  PRODUCT-FILE-SUCCESS   VALUE '00'.
               88  PRODUCT-REC-NOT-FOUND  VALUE '23'.
           05  SALES-FILE-STATUS          PIC X(2).
               88  SALES-FILE-SUCCESS     VALUE '00'.
               88  SALES-FILE-EOF         VALUE '10'.
           05  REPORT-FILE-STATUS         PIC X(2).
               88  REPORT-FILE-SUCCESS    VALUE '00'.
           05  INVUPDT-FILE-STATUS        PIC X(2).
               88  INVUPDT-FILE-SUCCESS   VALUE '00'.
           05  CUSTOMER-FILE-STATUS       PIC X(2).
               88  CUSTOMER-FILE-SUCCESS  VALUE '00'.
               
       01  WS-COUNTERS.
           05  WS-TRANS-READ              PIC 9(7) VALUE ZEROES.
           05  WS-TRANS-PROCESSED         PIC 9(7) VALUE ZEROES.
           05  WS-TRANS-ERRORS            PIC 9(7) VALUE ZEROES.
           05  WS-ITEMS-SOLD              PIC 9(7) VALUE ZEROES.
           05  WS-ITEMS-BELOW-REORDER     PIC 9(5) VALUE ZEROES.
           
       01  WS-CALCULATION-FIELDS.
           05  WS-TOTAL-SALES-AMOUNT      PIC S9(9)V99 VALUE ZEROES.
           05  WS-TOTAL-TAX-AMOUNT        PIC S9(7)V99 VALUE ZEROES.
           05  WS-TOTAL-DISCOUNT-AMOUNT   PIC S9(7)V99 VALUE ZEROES.
           05  WS-NET-SALES-AMOUNT        PIC S9(9)V99 VALUE ZEROES.
           05  WS-EXTENDED-PRICE          PIC S9(7)V99 VALUE ZEROES.
           05  WS-DISCOUNT-AMOUNT         PIC S9(5)V99 VALUE ZEROES.
           05  WS-TAXABLE-AMOUNT          PIC S9(7)V99 VALUE ZEROES.
           05  WS-TAX-AMOUNT              PIC S9(5)V99 VALUE ZEROES.
           
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
                                          'DAILY SALES REPORT'.
           05  FILLER                     PIC X(40) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE 'DATE:'.
           05  WS-HEADER-DATE             PIC X(10).
           05  FILLER                     PIC X(47) VALUE SPACES.
           
       01  WS-REPORT-HEADER2.
           05  FILLER                     PIC X(10) VALUE 'PRODUCT ID'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(20) VALUE 'DESCRIPTION'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'QUANTITY'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(12) VALUE 'UNIT PRICE'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(12) VALUE 'TOTAL SALES'.
           05  FILLER                     PIC X(56) VALUE SPACES.
           
       01  WS-REPORT-DETAIL.
           05  WS-RPT-PRODUCT-ID          PIC X(10).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RPT-DESCRIPTION         PIC X(20).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RPT-QUANTITY            PIC Z(4)9.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RPT-UNIT-PRICE          PIC $$$,$$9.99.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RPT-TOTAL-SALES         PIC $$$,$$$,$$9.99.
           05  FILLER                     PIC X(56) VALUE SPACES.
           
       01  WS-REPORT-TOTAL.
           05  FILLER                     PIC X(36) VALUE SPACES.
           05  FILLER                     PIC X(15) VALUE 'TOTAL SALES:'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RPT-TOTAL-AMOUNT        PIC $$$,$$$,$$9.99.
           05  FILLER                     PIC X(56) VALUE SPACES.
           
       01  WS-REPORT-TAX.
           05  FILLER                     PIC X(36) VALUE SPACES.
           05  FILLER                     PIC X(15) VALUE 'TOTAL TAX:'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RPT-TOTAL-TAX           PIC $$$,$$$,$$9.99.
           05  FILLER                     PIC X(56) VALUE SPACES.
           
       01  WS-REPORT-NET.
           05  FILLER                     PIC X(36) VALUE SPACES.
           05  FILLER                     PIC X(15) VALUE 'NET SALES:'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RPT-NET-SALES           PIC $$$,$$$,$$9.99.
           05  FILLER                     PIC X(56) VALUE SPACES.
           
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
                                          'ITEMS SOLD:'.
           05  WS-SUM-ITEMS-SOLD          PIC ZZ,ZZ9.
           05  FILLER                     PIC X(100) VALUE SPACES.
           
       01  WS-SUMMARY-DETAIL5.
           05  FILLER                     PIC X(25) VALUE 
                                          'ITEMS BELOW REORDER:'.
           05  WS-SUM-BELOW-REORDER       PIC ZZ,ZZ9.
           05  FILLER                     PIC X(100) VALUE SPACES.
           
       PROCEDURE DIVISION.
       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZATION
           PERFORM 2000-PROCESS-TRANSACTIONS
               UNTIL SALES-FILE-EOF
           PERFORM 3000-GENERATE-SALES-REPORT
           PERFORM 4000-TERMINATION
           STOP RUN
           .
           
       1000-INITIALIZATION.
           OPEN INPUT SALES-TRANSACTION-FILE
                      CUSTOMER-MASTER-FILE
                I-O   PRODUCT-MASTER-FILE
                OUTPUT DAILY-SALES-REPORT-FILE
                       INVENTORY-UPDATE-FILE
                       
           IF NOT PRODUCT-FILE-SUCCESS
              DISPLAY 'ERROR OPENING PRODUCT MASTER FILE: ' 
                      PRODUCT-FILE-STATUS
              PERFORM 4000-TERMINATION
              STOP RUN
           END-IF
           
           IF NOT CUSTOMER-FILE-SUCCESS
              DISPLAY 'ERROR OPENING CUSTOMER MASTER FILE: ' 
                      CUSTOMER-FILE-STATUS
              PERFORM 4000-TERMINATION
              STOP RUN
           END-IF
           
           IF NOT SALES-FILE-SUCCESS
              DISPLAY 'ERROR OPENING SALES TRANSACTION FILE: ' 
                      SALES-FILE-STATUS
              PERFORM 4000-TERMINATION
              STOP RUN
           END-IF
           
           IF NOT REPORT-FILE-SUCCESS
              DISPLAY 'ERROR OPENING DAILY SALES REPORT FILE: ' 
                      REPORT-FILE-STATUS
              PERFORM 4000-TERMINATION
              STOP RUN
           END-IF
           
           IF NOT INVUPDT-FILE-SUCCESS
              DISPLAY 'ERROR OPENING INVENTORY UPDATE FILE: ' 
                      INVUPDT-FILE-STATUS
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
           
           WRITE DAILY-SALES-REPORT-RECORD FROM WS-REPORT-HEADER1
           WRITE DAILY-SALES-REPORT-RECORD FROM WS-REPORT-HEADER2
           
           READ SALES-TRANSACTION-FILE
               AT END SET SALES-FILE-EOF TO TRUE
           END-READ
           
           IF SALES-FILE-SUCCESS
              ADD 1 TO WS-TRANS-READ
           END-IF
           .
           
       2000-PROCESS-TRANSACTIONS.
           MOVE 'N' TO WS-ERROR-FLAG
           
           MOVE ST-PRODUCT-ID TO PM-PRODUCT-ID
           
           READ PRODUCT-MASTER-FILE
               INVALID KEY
                   MOVE 'Y' TO WS-ERROR-FLAG
                   ADD 1 TO WS-TRANS-ERRORS
           END-READ
           
           IF PRODUCT-FILE-SUCCESS AND WS-NO-ERROR
              COMPUTE WS-EXTENDED-PRICE = 
                      ST-QUANTITY * ST-UNIT-PRICE
                      
              IF ST-DISCOUNT-PERCENT > ZEROES
                 COMPUTE WS-DISCOUNT-AMOUNT = 
                         WS-EXTENDED-PRICE * ST-DISCOUNT-PERCENT
                 SUBTRACT WS-DISCOUNT-AMOUNT FROM WS-EXTENDED-PRICE
                 ADD WS-DISCOUNT-AMOUNT TO WS-TOTAL-DISCOUNT-AMOUNT
              END-IF
              
              IF PM-TAXABLE
                 COMPUTE WS-TAX-AMOUNT = 
                         WS-EXTENDED-PRICE * PM-TAX-RATE
                 ADD WS-TAX-AMOUNT TO WS-TOTAL-TAX-AMOUNT
              ELSE
                 MOVE ZEROES TO WS-TAX-AMOUNT
              END-IF
              
              MOVE WS-EXTENDED-PRICE TO ST-EXTENDED-PRICE
              MOVE WS-TAX-AMOUNT TO ST-TAX-AMOUNT
              
              SUBTRACT ST-QUANTITY FROM PM-QUANTITY-ON-HAND
              ADD ST-QUANTITY TO PM-YTD-SALES-QUANTITY
              ADD ST-QUANTITY TO PM-MTD-SALES-QUANTITY
              ADD ST-EXTENDED-PRICE TO PM-YTD-SALES-AMOUNT
              ADD ST-EXTENDED-PRICE TO PM-MTD-SALES-AMOUNT
              
              MOVE ST-TRANSACTION-DATE TO PM-LAST-SALE-DATE
              
              REWRITE PRODUCT-MASTER-RECORD
              
              MOVE PM-PRODUCT-ID TO IU-PRODUCT-ID
              MOVE 'S' TO IU-UPDATE-TYPE
              MOVE ST-QUANTITY TO IU-QUANTITY
              MOVE ST-TRANSACTION-DATE TO IU-TRANSACTION-DATE
              MOVE ST-TRANSACTION-ID TO IU-TRANSACTION-ID
              
              WRITE INVENTORY-UPDATE-RECORD
              
              IF ST-CUSTOMER-ID NOT = SPACES
                 MOVE ST-CUSTOMER-ID TO CM-CUSTOMER-ID
                 
                 READ CUSTOMER-MASTER-FILE
                     INVALID KEY
                         CONTINUE
                     NOT INVALID KEY
                         ADD ST-EXTENDED-PRICE TO CM-YTD-PURCHASES
                         MOVE ST-TRANSACTION-DATE TO CM-LAST-PURCHASE-DATE
                         
                         EVALUATE TRUE
                             WHEN CM-REGULAR
                                 ADD ST-EXTENDED-PRICE TO CM-LOYALTY-POINTS
                             WHEN CM-GOLD
                                 COMPUTE CM-LOYALTY-POINTS = 
                                         CM-LOYALTY-POINTS + 
                                         (ST-EXTENDED-PRICE * 1.5)
                             WHEN CM-PLATINUM
                                 COMPUTE CM-LOYALTY-POINTS = 
                                         CM-LOYALTY-POINTS + 
                                         (ST-EXTENDED-PRICE * 2)
                         END-EVALUATE
                         
                         REWRITE CUSTOMER-MASTER-RECORD
                 END-READ
              END-IF
              
              ADD ST-EXTENDED-PRICE TO WS-TOTAL-SALES-AMOUNT
              ADD ST-QUANTITY TO WS-ITEMS-SOLD
              ADD 1 TO WS-TRANS-PROCESSED
           ELSE
              ADD 1 TO WS-TRANS-ERRORS
           END-IF
           
           READ SALES-TRANSACTION-FILE
               AT END SET SALES-FILE-EOF TO TRUE
           END-READ
           
           IF SALES-FILE-SUCCESS
              ADD 1 TO WS-TRANS-READ
           END-IF
           .
           
       3000-GENERATE-SALES-REPORT.
           MOVE ZEROES TO WS-ITEMS-BELOW-REORDER
           
           MOVE LOW-VALUES TO PM-PRODUCT-ID
           
           START PRODUCT-MASTER-FILE KEY >= PM-PRODUCT-ID
           
           READ PRODUCT-MASTER-FILE NEXT
               AT END MOVE HIGH-VALUES TO PM-PRODUCT-ID
           END-READ
           
           PERFORM UNTIL PM-PRODUCT-ID = HIGH-VALUES
              IF PM-MTD-SALES-QUANTITY > ZEROES
                 MOVE PM-PRODUCT-ID TO WS-RPT-PRODUCT-ID
                 MOVE PM-PRODUCT-DESCRIPTION(1:20) TO WS-RPT-DESCRIPTION
                 MOVE PM-MTD-SALES-QUANTITY TO WS-RPT-QUANTITY
                 MOVE PM-RETAIL-PRICE TO WS-RPT-UNIT-PRICE
                 MOVE PM-MTD-SALES-AMOUNT TO WS-RPT-TOTAL-SALES
                 
                 WRITE DAILY-SALES-REPORT-RECORD FROM WS-REPORT-DETAIL
              END-IF
              
              IF PM-ACTIVE AND 
                 PM-QUANTITY-ON-HAND <= PM-REORDER-POINT
                 ADD 1 TO WS-ITEMS-BELOW-REORDER
              END-IF
              
              READ PRODUCT-MASTER-FILE NEXT
                  AT END MOVE HIGH-VALUES TO PM-PRODUCT-ID
              END-READ
           END-PERFORM
           
           COMPUTE WS-NET-SALES-AMOUNT = 
                   WS-TOTAL-SALES-AMOUNT + WS-TOTAL-TAX-AMOUNT
                   
           MOVE WS-TOTAL-SALES-AMOUNT TO WS-RPT-TOTAL-AMOUNT
           MOVE WS-TOTAL-TAX-AMOUNT TO WS-RPT-TOTAL-TAX
           MOVE WS-NET-SALES-AMOUNT TO WS-RPT-NET-SALES
           
           WRITE DAILY-SALES-REPORT-RECORD FROM SPACES
           WRITE DAILY-SALES-REPORT-RECORD FROM WS-REPORT-TOTAL
           WRITE DAILY-SALES-REPORT-RECORD FROM WS-REPORT-TAX
           WRITE DAILY-SALES-REPORT-RECORD FROM WS-REPORT-NET
           .
           
       4000-TERMINATION.
           MOVE WS-TRANS-READ TO WS-SUM-TRANS-READ
           MOVE WS-TRANS-PROCESSED TO WS-SUM-TRANS-PROCESSED
           MOVE WS-TRANS-ERRORS TO WS-SUM-TRANS-ERRORS
           MOVE WS-ITEMS-SOLD TO WS-SUM-ITEMS-SOLD
           MOVE WS-ITEMS-BELOW-REORDER TO WS-SUM-BELOW-REORDER
           
           WRITE DAILY-SALES-REPORT-RECORD FROM SPACES
           WRITE DAILY-SALES-REPORT-RECORD FROM WS-SUMMARY-REPORT
           WRITE DAILY-SALES-REPORT-RECORD FROM WS-SUMMARY-DETAIL1
           WRITE DAILY-SALES-REPORT-RECORD FROM WS-SUMMARY-DETAIL2
           WRITE DAILY-SALES-REPORT-RECORD FROM WS-SUMMARY-DETAIL3
           WRITE DAILY-SALES-REPORT-RECORD FROM WS-SUMMARY-DETAIL4
           WRITE DAILY-SALES-REPORT-RECORD FROM WS-SUMMARY-DETAIL5
           
           CLOSE PRODUCT-MASTER-FILE
                 CUSTOMER-MASTER-FILE
                 SALES-TRANSACTION-FILE
                 DAILY-SALES-REPORT-FILE
                 INVENTORY-UPDATE-FILE
           .
