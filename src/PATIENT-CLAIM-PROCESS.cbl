      *****************************************************************
      * PATIENT-CLAIM-PROCESS.cbl                                    *
      *                                                               *
      * This program processes patient insurance claims for a         *
      * healthcare provider. It reads patient claim data, validates   *
      * the information, calculates reimbursement amounts based on    *
      * procedure codes, and generates claim reports.                 *
      *                                                               *
      * Author: Cascade AI                                            *
      * Date: 2025-03-14                                              *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PATIENT-CLAIM-PROCESS.
       AUTHOR. CASCADE-AI.
       DATE-WRITTEN. 2025-03-14.
       DATE-COMPILED. 2025-03-14.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PATIENT-MASTER-FILE ASSIGN TO PATMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PM-PATIENT-ID
               FILE STATUS IS PATIENT-FILE-STATUS.
               
           SELECT CLAIM-INPUT-FILE ASSIGN TO CLAIMINP
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS CLAIM-IN-STATUS.
               
           SELECT PROCEDURE-CODE-FILE ASSIGN TO PROCCODE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PC-PROCEDURE-CODE
               FILE STATUS IS PROC-FILE-STATUS.
               
           SELECT CLAIM-REPORT-FILE ASSIGN TO CLAIMRPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS REPORT-FILE-STATUS.
               
           SELECT ERROR-REPORT-FILE ASSIGN TO ERRORRPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS ERROR-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       
       FD  PATIENT-MASTER-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 200 CHARACTERS.
       01  PATIENT-MASTER-RECORD.
           05  PM-PATIENT-ID              PIC X(10).
           05  PM-PATIENT-NAME.
               10  PM-LAST-NAME           PIC X(20).
               10  PM-FIRST-NAME          PIC X(15).
               10  PM-MIDDLE-INIT         PIC X.
           05  PM-DATE-OF-BIRTH.
               10  PM-DOB-YEAR            PIC 9(4).
               10  PM-DOB-MONTH           PIC 9(2).
               10  PM-DOB-DAY             PIC 9(2).
           05  PM-GENDER                  PIC X.
               88  PM-MALE                VALUE 'M'.
               88  PM-FEMALE              VALUE 'F'.
               88  PM-OTHER               VALUE 'O'.
           05  PM-ADDRESS.
               10  PM-STREET              PIC X(30).
               10  PM-CITY                PIC X(20).
               10  PM-STATE               PIC X(2).
               10  PM-ZIP-CODE            PIC X(10).
           05  PM-PHONE-NUMBER            PIC X(15).
           05  PM-INSURANCE-INFO.
               10  PM-PRIMARY-INS-ID      PIC X(15).
               10  PM-PRIMARY-INS-NAME    PIC X(30).
               10  PM-POLICY-NUMBER       PIC X(20).
               10  PM-GROUP-NUMBER        PIC X(15).
           05  PM-FILLER                  PIC X(15).
           
       FD  CLAIM-INPUT-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 150 CHARACTERS.
       01  CLAIM-INPUT-RECORD.
           05  CI-CLAIM-ID                PIC X(12).
           05  CI-PATIENT-ID              PIC X(10).
           05  CI-SERVICE-DATE.
               10  CI-SERV-YEAR           PIC 9(4).
               10  CI-SERV-MONTH          PIC 9(2).
               10  CI-SERV-DAY            PIC 9(2).
           05  CI-PROVIDER-ID             PIC X(10).
           05  CI-PROCEDURE-CODES         OCCURS 5 TIMES.
               10  CI-PROCEDURE-CODE      PIC X(8).
               10  CI-PROCEDURE-MODIFIER  PIC X(2).
           05  CI-DIAGNOSIS-CODES         OCCURS 4 TIMES.
               10  CI-DIAGNOSIS-CODE      PIC X(8).
           05  CI-TOTAL-CHARGE            PIC 9(7)V99.
           05  CI-FILLER                  PIC X(10).
           
       FD  PROCEDURE-CODE-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 100 CHARACTERS.
       01  PROCEDURE-CODE-RECORD.
           05  PC-PROCEDURE-CODE          PIC X(8).
           05  PC-PROCEDURE-DESC          PIC X(50).
           05  PC-ALLOWED-AMOUNT          PIC 9(7)V99.
           05  PC-COVERAGE-PERCENT        PIC 9(3).
           05  PC-FILLER                  PIC X(30).
           
       FD  CLAIM-REPORT-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS.
       01  CLAIM-REPORT-RECORD            PIC X(132).
       
       FD  ERROR-REPORT-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS.
       01  ERROR-REPORT-RECORD            PIC X(132).
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  PATIENT-FILE-STATUS        PIC X(2).
               88  PATIENT-FILE-SUCCESS   VALUE '00'.
           05  CLAIM-IN-STATUS            PIC X(2).
               88  CLAIM-IN-SUCCESS       VALUE '00'.
               88  CLAIM-IN-EOF           VALUE '10'.
           05  PROC-FILE-STATUS           PIC X(2).
               88  PROC-FILE-SUCCESS      VALUE '00'.
           05  REPORT-FILE-STATUS         PIC X(2).
               88  REPORT-FILE-SUCCESS    VALUE '00'.
           05  ERROR-FILE-STATUS          PIC X(2).
               88  ERROR-FILE-SUCCESS     VALUE '00'.
               
       01  WS-COUNTERS.
           05  WS-CLAIMS-READ             PIC 9(7) VALUE ZEROES.
           05  WS-CLAIMS-PROCESSED        PIC 9(7) VALUE ZEROES.
           05  WS-CLAIMS-IN-ERROR         PIC 9(7) VALUE ZEROES.
           05  WS-PROC-CODE-IDX           PIC 9 VALUE 1.
           
       01  WS-CALCULATION-FIELDS.
           05  WS-TOTAL-ALLOWED           PIC 9(7)V99 VALUE ZEROES.
           05  WS-TOTAL-COVERED           PIC 9(7)V99 VALUE ZEROES.
           05  WS-PATIENT-RESPONSIBILITY  PIC 9(7)V99 VALUE ZEROES.
           05  WS-PROCEDURE-ALLOWED       PIC 9(7)V99 VALUE ZEROES.
           05  WS-PROCEDURE-COVERED       PIC 9(7)V99 VALUE ZEROES.
           
       01  WS-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR        PIC 9(4).
               10  WS-CURRENT-MONTH       PIC 9(2).
               10  WS-CURRENT-DAY         PIC 9(2).
           05  WS-FORMATTED-DATE          PIC X(10).
           
       01  WS-ERROR-FLAG                  PIC X VALUE 'N'.
           88  WS-ERROR-FOUND             VALUE 'Y'.
           88  WS-NO-ERROR                VALUE 'N'.
           
       01  WS-CLAIM-REPORT-HEADER1.
           05  FILLER                     PIC X(20) VALUE 'HEALTHCARE CLAIMS REP'.
           05  FILLER                     PIC X(3) VALUE 'ORT'.
           05  FILLER                     PIC X(45) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE 'DATE:'.
           05  WS-HEADER-DATE             PIC X(10).
           05  FILLER                     PIC X(49) VALUE SPACES.
           
       01  WS-CLAIM-REPORT-HEADER2.
           05  FILLER                     PIC X(12) VALUE 'CLAIM ID'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'PATIENT ID'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(20) VALUE 'PATIENT NAME'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'SERV DATE'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(15) VALUE 'TOTAL ALLOWED'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(15) VALUE 'TOTAL COVERED'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(15) VALUE 'PATIENT RESP'.
           05  FILLER                     PIC X(17) VALUE SPACES.
           
       01  WS-CLAIM-REPORT-DETAIL.
           05  WS-RPT-CLAIM-ID            PIC X(12).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RPT-PATIENT-ID          PIC X(10).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RPT-PATIENT-NAME        PIC X(20).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RPT-SERVICE-DATE        PIC X(10).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RPT-TOTAL-ALLOWED       PIC $$$,$$$,$$9.99.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RPT-TOTAL-COVERED       PIC $$$,$$$,$$9.99.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-RPT-PATIENT-RESP        PIC $$$,$$$,$$9.99.
           05  FILLER                     PIC X(17) VALUE SPACES.
           
       01  WS-ERROR-REPORT-HEADER1.
           05  FILLER                     PIC X(20) VALUE 'CLAIMS ERROR REPORT'.
           05  FILLER                     PIC X(45) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE 'DATE:'.
           05  WS-ERR-HEADER-DATE         PIC X(10).
           05  FILLER                     PIC X(52) VALUE SPACES.
           
       01  WS-ERROR-REPORT-HEADER2.
           05  FILLER                     PIC X(12) VALUE 'CLAIM ID'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE 'PATIENT ID'.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(50) VALUE 'ERROR DESCRIPTION'.
           05  FILLER                     PIC X(54) VALUE SPACES.
           
       01  WS-ERROR-REPORT-DETAIL.
           05  WS-ERR-CLAIM-ID            PIC X(12).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-ERR-PATIENT-ID          PIC X(10).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-ERR-DESCRIPTION         PIC X(50).
           05  FILLER                     PIC X(54) VALUE SPACES.
           
       01  WS-SUMMARY-REPORT.
           05  FILLER                     PIC X(30) VALUE 'CLAIMS PROCESSING SUMMARY'.
           05  FILLER                     PIC X(102) VALUE SPACES.
           
       01  WS-SUMMARY-DETAIL1.
           05  FILLER                     PIC X(25) VALUE 'TOTAL CLAIMS READ:'.
           05  WS-SUM-CLAIMS-READ         PIC ZZ,ZZ9.
           05  FILLER                     PIC X(100) VALUE SPACES.
           
       01  WS-SUMMARY-DETAIL2.
           05  FILLER                     PIC X(25) VALUE 'CLAIMS PROCESSED:'.
           05  WS-SUM-CLAIMS-PROCESSED    PIC ZZ,ZZ9.
           05  FILLER                     PIC X(100) VALUE SPACES.
           
       01  WS-SUMMARY-DETAIL3.
           05  FILLER                     PIC X(25) VALUE 'CLAIMS IN ERROR:'.
           05  WS-SUM-CLAIMS-ERROR        PIC ZZ,ZZ9.
           05  FILLER                     PIC X(100) VALUE SPACES.
           
       PROCEDURE DIVISION.
       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZATION
           PERFORM 2000-PROCESS-CLAIMS
               UNTIL CLAIM-IN-EOF
           PERFORM 3000-TERMINATION
           STOP RUN
           .
           
       1000-INITIALIZATION.
           OPEN INPUT PATIENT-MASTER-FILE
                      CLAIM-INPUT-FILE
                      PROCEDURE-CODE-FILE
                OUTPUT CLAIM-REPORT-FILE
                       ERROR-REPORT-FILE
                       
           IF NOT PATIENT-FILE-SUCCESS
              DISPLAY 'ERROR OPENING PATIENT FILE: ' PATIENT-FILE-STATUS
              PERFORM 3000-TERMINATION
              STOP RUN
           END-IF
           
           IF NOT CLAIM-IN-SUCCESS
              DISPLAY 'ERROR OPENING CLAIM INPUT FILE: ' CLAIM-IN-STATUS
              PERFORM 3000-TERMINATION
              STOP RUN
           END-IF
           
           IF NOT PROC-FILE-SUCCESS
              DISPLAY 'ERROR OPENING PROCEDURE CODE FILE: ' 
                      PROC-FILE-STATUS
              PERFORM 3000-TERMINATION
              STOP RUN
           END-IF
           
           IF NOT REPORT-FILE-SUCCESS
              DISPLAY 'ERROR OPENING CLAIM REPORT FILE: ' 
                      REPORT-FILE-STATUS
              PERFORM 3000-TERMINATION
              STOP RUN
           END-IF
           
           IF NOT ERROR-FILE-SUCCESS
              DISPLAY 'ERROR OPENING ERROR REPORT FILE: ' 
                      ERROR-FILE-STATUS
              PERFORM 3000-TERMINATION
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
           
           WRITE CLAIM-REPORT-RECORD FROM WS-CLAIM-REPORT-HEADER1
           WRITE CLAIM-REPORT-RECORD FROM WS-CLAIM-REPORT-HEADER2
           WRITE ERROR-REPORT-RECORD FROM WS-ERROR-REPORT-HEADER1
           WRITE ERROR-REPORT-RECORD FROM WS-ERROR-REPORT-HEADER2
           
           READ CLAIM-INPUT-FILE
               AT END SET CLAIM-IN-EOF TO TRUE
           END-READ
           
           IF CLAIM-IN-SUCCESS
              ADD 1 TO WS-CLAIMS-READ
           END-IF
           .
           
       2000-PROCESS-CLAIMS.
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE ZEROES TO WS-TOTAL-ALLOWED
                          WS-TOTAL-COVERED
                          WS-PATIENT-RESPONSIBILITY
           
           PERFORM 2100-VALIDATE-CLAIM
           
           IF WS-NO-ERROR
              PERFORM 2200-PROCESS-PROCEDURES
              PERFORM 2300-CALCULATE-TOTALS
              PERFORM 2400-WRITE-CLAIM-REPORT
              ADD 1 TO WS-CLAIMS-PROCESSED
           ELSE
              ADD 1 TO WS-CLAIMS-IN-ERROR
           END-IF
           
           READ CLAIM-INPUT-FILE
               AT END SET CLAIM-IN-EOF TO TRUE
           END-READ
           
           IF CLAIM-IN-SUCCESS
              ADD 1 TO WS-CLAIMS-READ
           END-IF
           .
           
       2100-VALIDATE-CLAIM.
           MOVE CI-PATIENT-ID TO PM-PATIENT-ID
           
           READ PATIENT-MASTER-FILE
               INVALID KEY
                   MOVE 'Y' TO WS-ERROR-FLAG
                   MOVE CI-CLAIM-ID TO WS-ERR-CLAIM-ID
                   MOVE CI-PATIENT-ID TO WS-ERR-PATIENT-ID
                   MOVE 'PATIENT ID NOT FOUND IN MASTER FILE' 
                        TO WS-ERR-DESCRIPTION
                   WRITE ERROR-REPORT-RECORD FROM WS-ERROR-REPORT-DETAIL
           END-READ
           
           IF PATIENT-FILE-SUCCESS AND WS-NO-ERROR
              IF CI-SERV-YEAR < 2000 OR CI-SERV-YEAR > WS-CURRENT-YEAR
                 MOVE 'Y' TO WS-ERROR-FLAG
                 MOVE CI-CLAIM-ID TO WS-ERR-CLAIM-ID
                 MOVE CI-PATIENT-ID TO WS-ERR-PATIENT-ID
                 MOVE 'INVALID SERVICE DATE' TO WS-ERR-DESCRIPTION
                 WRITE ERROR-REPORT-RECORD FROM WS-ERROR-REPORT-DETAIL
              END-IF
              
              IF CI-SERV-MONTH < 1 OR CI-SERV-MONTH > 12
                 MOVE 'Y' TO WS-ERROR-FLAG
                 MOVE CI-CLAIM-ID TO WS-ERR-CLAIM-ID
                 MOVE CI-PATIENT-ID TO WS-ERR-PATIENT-ID
                 MOVE 'INVALID SERVICE MONTH' TO WS-ERR-DESCRIPTION
                 WRITE ERROR-REPORT-RECORD FROM WS-ERROR-REPORT-DETAIL
              END-IF
              
              IF CI-SERV-DAY < 1 OR CI-SERV-DAY > 31
                 MOVE 'Y' TO WS-ERROR-FLAG
                 MOVE CI-CLAIM-ID TO WS-ERR-CLAIM-ID
                 MOVE CI-PATIENT-ID TO WS-ERR-PATIENT-ID
                 MOVE 'INVALID SERVICE DAY' TO WS-ERR-DESCRIPTION
                 WRITE ERROR-REPORT-RECORD FROM WS-ERROR-REPORT-DETAIL
              END-IF
              
              IF CI-TOTAL-CHARGE <= 0
                 MOVE 'Y' TO WS-ERROR-FLAG
                 MOVE CI-CLAIM-ID TO WS-ERR-CLAIM-ID
                 MOVE CI-PATIENT-ID TO WS-ERR-PATIENT-ID
                 MOVE 'INVALID TOTAL CHARGE AMOUNT' TO WS-ERR-DESCRIPTION
                 WRITE ERROR-REPORT-RECORD FROM WS-ERROR-REPORT-DETAIL
              END-IF
           END-IF
           .
           
       2200-PROCESS-PROCEDURES.
           MOVE 1 TO WS-PROC-CODE-IDX
           
           PERFORM VARYING WS-PROC-CODE-IDX FROM 1 BY 1
                   UNTIL WS-PROC-CODE-IDX > 5
              
              IF CI-PROCEDURE-CODE(WS-PROC-CODE-IDX) NOT = SPACES
                 MOVE CI-PROCEDURE-CODE(WS-PROC-CODE-IDX) 
                      TO PC-PROCEDURE-CODE
                 
                 READ PROCEDURE-CODE-FILE
                     INVALID KEY
                         MOVE 'Y' TO WS-ERROR-FLAG
                         MOVE CI-CLAIM-ID TO WS-ERR-CLAIM-ID
                         MOVE CI-PATIENT-ID TO WS-ERR-PATIENT-ID
                         MOVE 'INVALID PROCEDURE CODE' TO WS-ERR-DESCRIPTION
                         WRITE ERROR-REPORT-RECORD 
                               FROM WS-ERROR-REPORT-DETAIL
                 END-READ
                 
                 IF PROC-FILE-SUCCESS AND WS-NO-ERROR
                    COMPUTE WS-PROCEDURE-ALLOWED = PC-ALLOWED-AMOUNT
                    COMPUTE WS-PROCEDURE-COVERED = 
                            (PC-ALLOWED-AMOUNT * PC-COVERAGE-PERCENT) / 100
                    
                    ADD WS-PROCEDURE-ALLOWED TO WS-TOTAL-ALLOWED
                    ADD WS-PROCEDURE-COVERED TO WS-TOTAL-COVERED
                 END-IF
              END-IF
           END-PERFORM
           .
           
       2300-CALCULATE-TOTALS.
           COMPUTE WS-PATIENT-RESPONSIBILITY = 
                   WS-TOTAL-ALLOWED - WS-TOTAL-COVERED
           
           IF WS-PATIENT-RESPONSIBILITY < 0
              MOVE 0 TO WS-PATIENT-RESPONSIBILITY
           END-IF
           .
           
       2400-WRITE-CLAIM-REPORT.
           MOVE CI-CLAIM-ID TO WS-RPT-CLAIM-ID
           MOVE CI-PATIENT-ID TO WS-RPT-PATIENT-ID
           
           STRING PM-LAST-NAME DELIMITED BY SPACE
                  ', ' DELIMITED BY SIZE
                  PM-FIRST-NAME DELIMITED BY SPACE
                  ' ' DELIMITED BY SIZE
                  PM-MIDDLE-INIT DELIMITED BY SPACE
                  INTO WS-RPT-PATIENT-NAME
           
           STRING CI-SERV-YEAR DELIMITED BY SIZE
                  '-' DELIMITED BY SIZE
                  CI-SERV-MONTH DELIMITED BY SIZE
                  '-' DELIMITED BY SIZE
                  CI-SERV-DAY DELIMITED BY SIZE
                  INTO WS-RPT-SERVICE-DATE
           
           MOVE WS-TOTAL-ALLOWED TO WS-RPT-TOTAL-ALLOWED
           MOVE WS-TOTAL-COVERED TO WS-RPT-TOTAL-COVERED
           MOVE WS-PATIENT-RESPONSIBILITY TO WS-RPT-PATIENT-RESP
           
           WRITE CLAIM-REPORT-RECORD FROM WS-CLAIM-REPORT-DETAIL
           .
           
       3000-TERMINATION.
           MOVE WS-CLAIMS-READ TO WS-SUM-CLAIMS-READ
           MOVE WS-CLAIMS-PROCESSED TO WS-SUM-CLAIMS-PROCESSED
           MOVE WS-CLAIMS-IN-ERROR TO WS-SUM-CLAIMS-ERROR
           
           WRITE CLAIM-REPORT-RECORD FROM SPACES
           WRITE CLAIM-REPORT-RECORD FROM WS-SUMMARY-REPORT
           WRITE CLAIM-REPORT-RECORD FROM WS-SUMMARY-DETAIL1
           WRITE CLAIM-REPORT-RECORD FROM WS-SUMMARY-DETAIL2
           WRITE CLAIM-REPORT-RECORD FROM WS-SUMMARY-DETAIL3
           
           CLOSE PATIENT-MASTER-FILE
                 CLAIM-INPUT-FILE
                 PROCEDURE-CODE-FILE
                 CLAIM-REPORT-FILE
                 ERROR-REPORT-FILE
           .
