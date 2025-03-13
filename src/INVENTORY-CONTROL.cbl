      *****************************************************************
      * INVENTORY-CONTROL.cbl                                         *
      *                                                               *
      * This program manages inventory for a retail/wholesale         *
      * business. It processes daily inventory transactions,          *
      * maintains stock levels, and produces inventory reports.       *
      *                                                               *
      * Author: Cascade AI                                            *
      * Date: 2025-03-14                                              *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVENTORY-CONTROL.
       AUTHOR. CASCADE-AI.
       DATE-WRITTEN. 2025-03-14.
       DATE-COMPILED. 2025-03-14.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENTORY-FILE ASSIGN TO INVFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS INV-ITEM-NUMBER
               ALTERNATE RECORD KEY IS INV-ITEM-DESCRIPTION
                   WITH DUPLICATES
               FILE STATUS IS INV-FILE-STATUS.
               
           SELECT TRANSACTION-FILE ASSIGN TO TRANFILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS TRANS-FILE-STATUS.
               
           SELECT INVENTORY-REPORT-FILE ASSIGN TO INVREPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS REPORT-FILE-STATUS.
               
           SELECT ERROR-REPORT-FILE ASSIGN TO ERRREPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS ERROR-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       
       FD  INVENTORY-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 150 CHARACTERS.
       01  INVENTORY-RECORD.
           05  INV-ITEM-NUMBER            PIC X(10).
           05  INV-ITEM-DESCRIPTION       PIC X(40).
           05  INV-WAREHOUSE-LOCATION     PIC X(10).
           05  INV-QUANTITY-ON-HAND       PIC S9(7) COMP-3.
           05  INV-QUANTITY-ALLOCATED     PIC S9(7) COMP-3.
           05  INV-REORDER-POINT          PIC S9(7) COMP-3.
           05  INV-REORDER-QUANTITY       PIC S9(7) COMP-3.
           05  INV-COST-PRICE             PIC S9(7)V99 COMP-3.
           05  INV-SELLING-PRICE          PIC S9(7)V99 COMP-3.
           05  INV-LAST-ACTIVITY-DATE.
               10  INV-LAST-ACT-YEAR      PIC 9(4).
               10  INV-LAST-ACT-MONTH     PIC 9(2).
               10  INV-LAST-ACT-DAY       PIC 9(2).
           05  INV-YTD-SALES-QUANTITY     PIC S9(9) COMP-3.
           05  INV-YTD-SALES-AMOUNT       PIC S9(9)V99 COMP-3.
           05  INV-SUPPLIER-ID            PIC X(10).
           05  INV-STATUS                 PIC X.
               88  INV-ACTIVE             VALUE 'A'.
               88  INV-DISCONTINUED       VALUE 'D'.
               88  INV-SEASONAL           VALUE 'S'.
           05  INV-CATEGORY               PIC X(10).
           05  INV-FILLER                 PIC X(20).
           
       FD  TRANSACTION-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS.
       01  TRANSACTION-RECORD.
           05  TR-TRANSACTION-CODE        PIC X.
               88  TR-RECEIPT             VALUE 'R'.
               88  TR-ISSUE               VALUE 'I'.
               88  TR-ADJUSTMENT          VALUE 'A'.
           05  TR-ITEM-NUMBER             PIC X(10).
           05  TR-TRANSACTION-QUANTITY    PIC S9(7) COMP-3.
           05  TR-TRANSACTION-DATE.
               10  TR-TRANS-YEAR          PIC 9(4).
               10  TR-TRANS-MONTH         PIC 9(2).
               10  TR-TRANS-DAY           PIC 9(2).
           05  TR-REFERENCE-NUMBER        PIC X(10).
           05  TR-UNIT-COST               PIC S9(7)V99 COMP-3.
           05  TR-UNIT-PRICE              PIC S9(7)V99 COMP-3.
           05  TR-USER-ID                 PIC X(8).
           05  TR-REASON-CODE             PIC X(3).
           05  TR-FILLER                  PIC X(25).
           
       FD  INVENTORY-REPORT-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS.
       01  INVENTORY-REPORT-RECORD        PIC X(132).
       
       FD  ERROR-REPORT-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS.
       01  ERROR-REPORT-RECORD            PIC X(132).
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  INV-FILE-STATUS            PIC X(2).
               88  INV-FILE-SUCCESS       VALUE '00'.
               88  INV-REC-NOT-FOUND      VALUE '23'.
           05  TRANS-FILE-STATUS          PIC X(2).
               88  TRANS-FILE-SUCCESS     VALUE '00'.
               88  TRANS-FILE-EOF         VALUE '10'.
           05  REPORT-FILE-STATUS         PIC X(2).
               88  REPORT-FILE-SUCCESS    VALUE '00'.
           05  ERROR-FILE-STATUS          PIC X(2).
               88  ERROR-FILE-SUCCESS     VALUE '00'.
               
       01  WS-COUNTERS.
           05  WS-TRANS-READ              PIC 9(7) VALUE ZEROES.
           05  WS-TRANS-PROCESSED         PIC 9(7) VALUE ZEROES.
           05  WS-TRANS-ERRORS            PIC 9(7) VALUE ZEROES.
           05  WS-ITEMS-BELOW-REORDER     PIC 9(5) VALUE ZEROES.
           
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
           
       01  WS-INVENTORY-HEADER1.
           05  FILLER                     PIC X(30) VALUE 
                                          'INVENTORY STATUS REPORT'.
           05  FILLER                     PIC X(40) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE 'DATE:'.
           05  WS-HEADER-DATE             PIC X(10).
           05  FILLER                     PIC X(47) VALUE SPACES.
           
       01  WS-INVENTORY-HEADER2.
           05  FILLER                     PIC X(10) VALUE 'ITEM NO'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(25) VALUE 'DESCRIPTION'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'LOCATION'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'ON HAND'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'ALLOCATED'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'AVAILABLE'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'COST VALUE'.
           05  FILLER                     PIC X(29) VALUE SPACES.
           
       01  WS-INVENTORY-DETAIL.
           05  WS-INV-ITEM-NUMBER         PIC X(10).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-INV-DESCRIPTION         PIC X(25).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-INV-LOCATION            PIC X(10).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-INV-ON-HAND             PIC Z(6)9-.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-INV-ALLOCATED           PIC Z(6)9-.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-INV-AVAILABLE           PIC Z(6)9-.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-INV-COST-VALUE          PIC $$$,$$$,$$9.99-.
           05  FILLER                     PIC X(29) VALUE SPACES.
           
       01  WS-ERROR-HEADER1.
           05  FILLER                     PIC X(30) VALUE 
                                          'TRANSACTION ERROR REPORT'.
           05  FILLER                     PIC X(40) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE 'DATE:'.
           05  WS-ERR-HEADER-DATE         PIC X(10).
           05  FILLER                     PIC X(47) VALUE SPACES.
           
       01  WS-ERROR-HEADER2.
           05  FILLER                     PIC X(10) VALUE 'ITEM NO'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE 'TRANS'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'REFERENCE'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'QUANTITY'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(30) VALUE 'ERROR DESCRIPTION'.
           05  FILLER                     PIC X(55) VALUE SPACES.
           
       01  WS-ERROR-DETAIL.
           05  WS-ERR-ITEM-NUMBER         PIC X(10).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-ERR-TRANS-CODE          PIC X(5).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-ERR-REFERENCE           PIC X(10).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-ERR-QUANTITY            PIC Z(6)9-.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-ERR-DESCRIPTION         PIC X(30).
           05  FILLER                     PIC X(55) VALUE SPACES.
           
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
                                          'ITEMS BELOW REORDER:'.
           05  WS-SUM-BELOW-REORDER       PIC ZZ,ZZ9.
           05  FILLER                     PIC X(100) VALUE SPACES.
           
       01  WS-SUMMARY-DETAIL5.
           05  FILLER                     PIC X(25) VALUE 
                                          'TOTAL INVENTORY VALUE:'.
           05  WS-SUM-INVENTORY-VALUE     PIC $$$,$$$,$$$,$$9.99.
           05  FILLER                     PIC X(90) VALUE SPACES.
           
       PROCEDURE DIVISION.
       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZATION
           PERFORM 2000-PROCESS-TRANSACTIONS
               UNTIL TRANS-FILE-EOF
           PERFORM 3000-GENERATE-INVENTORY-REPORT
           PERFORM 4000-TERMINATION
           STOP RUN
           .
           
       1000-INITIALIZATION.
           OPEN INPUT TRANSACTION-FILE
                I-O   INVENTORY-FILE
                OUTPUT INVENTORY-REPORT-FILE
                       ERROR-REPORT-FILE
                       
           IF NOT INV-FILE-SUCCESS
              DISPLAY 'ERROR OPENING INVENTORY FILE: ' INV-FILE-STATUS
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
              DISPLAY 'ERROR OPENING INVENTORY REPORT FILE: ' 
                      REPORT-FILE-STATUS
              PERFORM 4000-TERMINATION
              STOP RUN
           END-IF
           
           IF NOT ERROR-FILE-SUCCESS
              DISPLAY 'ERROR OPENING ERROR REPORT FILE: ' 
                      ERROR-FILE-STATUS
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
           MOVE WS-FORMATTED-DATE TO WS-ERR-HEADER-DATE
           
           WRITE ERROR-REPORT-RECORD FROM WS-ERROR-HEADER1
           WRITE ERROR-REPORT-RECORD FROM WS-ERROR-HEADER2
           
           READ TRANSACTION-FILE
               AT END SET TRANS-FILE-EOF TO TRUE
           END-READ
           
           IF TRANS-FILE-SUCCESS
              ADD 1 TO WS-TRANS-READ
           END-IF
           .
           
       2000-PROCESS-TRANSACTIONS.
           MOVE 'N' TO WS-ERROR-FLAG
           
           MOVE TR-ITEM-NUMBER TO INV-ITEM-NUMBER
           
           READ INVENTORY-FILE
               INVALID KEY
                   MOVE 'Y' TO WS-ERROR-FLAG
                   MOVE TR-ITEM-NUMBER TO WS-ERR-ITEM-NUMBER
                   
                   EVALUATE TRUE
                       WHEN TR-RECEIPT
                           MOVE 'RCPT' TO WS-ERR-TRANS-CODE
                       WHEN TR-ISSUE
                           MOVE 'ISSUE' TO WS-ERR-TRANS-CODE
                       WHEN TR-ADJUSTMENT
                           MOVE 'ADJST' TO WS-ERR-TRANS-CODE
                       WHEN OTHER
                           MOVE 'UNKN' TO WS-ERR-TRANS-CODE
                   END-EVALUATE
                   
                   MOVE TR-REFERENCE-NUMBER TO WS-ERR-REFERENCE
                   MOVE TR-TRANSACTION-QUANTITY TO WS-ERR-QUANTITY
                   MOVE 'ITEM NOT FOUND IN INVENTORY' TO WS-ERR-DESCRIPTION
                   WRITE ERROR-REPORT-RECORD FROM WS-ERROR-DETAIL
           END-READ
           
           IF INV-FILE-SUCCESS AND WS-NO-ERROR
              EVALUATE TRUE
                  WHEN TR-RECEIPT
                      ADD TR-TRANSACTION-QUANTITY TO INV-QUANTITY-ON-HAND
                      
                      IF TR-UNIT-COST > 0
                         MOVE TR-UNIT-COST TO INV-COST-PRICE
                      END-IF
                      
                      IF TR-UNIT-PRICE > 0
                         MOVE TR-UNIT-PRICE TO INV-SELLING-PRICE
                      END-IF
                      
                  WHEN TR-ISSUE
                      IF TR-TRANSACTION-QUANTITY > INV-QUANTITY-ON-HAND
                         MOVE 'Y' TO WS-ERROR-FLAG
                         MOVE TR-ITEM-NUMBER TO WS-ERR-ITEM-NUMBER
                         MOVE 'ISSUE' TO WS-ERR-TRANS-CODE
                         MOVE TR-REFERENCE-NUMBER TO WS-ERR-REFERENCE
                         MOVE TR-TRANSACTION-QUANTITY TO WS-ERR-QUANTITY
                         MOVE 'INSUFFICIENT QUANTITY ON HAND' 
                              TO WS-ERR-DESCRIPTION
                         WRITE ERROR-REPORT-RECORD FROM WS-ERROR-DETAIL
                      ELSE
                         SUBTRACT TR-TRANSACTION-QUANTITY 
                                  FROM INV-QUANTITY-ON-HAND
                         
                         ADD TR-TRANSACTION-QUANTITY TO INV-YTD-SALES-QUANTITY
                         
                         COMPUTE INV-YTD-SALES-AMOUNT = 
                                 INV-YTD-SALES-AMOUNT + 
                                 (TR-TRANSACTION-QUANTITY * INV-SELLING-PRICE)
                      END-IF
                      
                  WHEN TR-ADJUSTMENT
                      ADD TR-TRANSACTION-QUANTITY TO INV-QUANTITY-ON-HAND
              END-EVALUATE
              
              IF WS-NO-ERROR
                 MOVE TR-TRANSACTION-DATE TO INV-LAST-ACTIVITY-DATE
                 
                 REWRITE INVENTORY-RECORD
                 
                 ADD 1 TO WS-TRANS-PROCESSED
              ELSE
                 ADD 1 TO WS-TRANS-ERRORS
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
           
       3000-GENERATE-INVENTORY-REPORT.
           MOVE ZEROES TO WS-INVENTORY-VALUE
           MOVE ZEROES TO WS-ITEMS-BELOW-REORDER
           
           WRITE INVENTORY-REPORT-RECORD FROM WS-INVENTORY-HEADER1
           WRITE INVENTORY-REPORT-RECORD FROM WS-INVENTORY-HEADER2
           
           MOVE LOW-VALUES TO INV-ITEM-NUMBER
           
           START INVENTORY-FILE KEY >= INV-ITEM-NUMBER
           
           READ INVENTORY-FILE NEXT
               AT END MOVE HIGH-VALUES TO INV-ITEM-NUMBER
           END-READ
           
           PERFORM UNTIL INV-ITEM-NUMBER = HIGH-VALUES
              MOVE INV-ITEM-NUMBER TO WS-INV-ITEM-NUMBER
              MOVE INV-ITEM-DESCRIPTION(1:25) TO WS-INV-DESCRIPTION
              MOVE INV-WAREHOUSE-LOCATION TO WS-INV-LOCATION
              MOVE INV-QUANTITY-ON-HAND TO WS-INV-ON-HAND
              MOVE INV-QUANTITY-ALLOCATED TO WS-INV-ALLOCATED
              
              COMPUTE WS-AVAILABLE-QUANTITY = 
                      INV-QUANTITY-ON-HAND - INV-QUANTITY-ALLOCATED
              
              MOVE WS-AVAILABLE-QUANTITY TO WS-INV-AVAILABLE
              
              COMPUTE WS-INV-COST-VALUE = 
                      INV-QUANTITY-ON-HAND * INV-COST-PRICE
              
              ADD WS-INV-COST-VALUE TO WS-INVENTORY-VALUE
              
              WRITE INVENTORY-REPORT-RECORD FROM WS-INVENTORY-DETAIL
              
              IF INV-ACTIVE AND 
                 INV-QUANTITY-ON-HAND <= INV-REORDER-POINT
                 ADD 1 TO WS-ITEMS-BELOW-REORDER
              END-IF
              
              READ INVENTORY-FILE NEXT
                  AT END MOVE HIGH-VALUES TO INV-ITEM-NUMBER
              END-READ
           END-PERFORM
           .
           
       4000-TERMINATION.
           MOVE WS-TRANS-READ TO WS-SUM-TRANS-READ
           MOVE WS-TRANS-PROCESSED TO WS-SUM-TRANS-PROCESSED
           MOVE WS-TRANS-ERRORS TO WS-SUM-TRANS-ERRORS
           MOVE WS-ITEMS-BELOW-REORDER TO WS-SUM-BELOW-REORDER
           MOVE WS-INVENTORY-VALUE TO WS-SUM-INVENTORY-VALUE
           
           WRITE INVENTORY-REPORT-RECORD FROM SPACES
           WRITE INVENTORY-REPORT-RECORD FROM WS-SUMMARY-REPORT
           WRITE INVENTORY-REPORT-RECORD FROM WS-SUMMARY-DETAIL1
           WRITE INVENTORY-REPORT-RECORD FROM WS-SUMMARY-DETAIL2
           WRITE INVENTORY-REPORT-RECORD FROM WS-SUMMARY-DETAIL3
           WRITE INVENTORY-REPORT-RECORD FROM WS-SUMMARY-DETAIL4
           WRITE INVENTORY-REPORT-RECORD FROM WS-SUMMARY-DETAIL5
           
           CLOSE INVENTORY-FILE
                 TRANSACTION-FILE
                 INVENTORY-REPORT-FILE
                 ERROR-REPORT-FILE
           .
