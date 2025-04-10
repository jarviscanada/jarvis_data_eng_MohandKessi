       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGV0001.
       AUTHOR. Mohand Kessi.
       
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-SEQ-FILE ASSIGN TO 'STUDENT.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-SEQ-STATUS.
               
           SELECT STUDENT-VSAM-FILE ASSIGN TO 'STUDENT.VSAM'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS VSAM-STUDENT-ID
               FILE STATUS IS WS-VSAM-STATUS.
               
       DATA DIVISION.
       FILE SECTION.
       FD  STUDENT-SEQ-FILE
           RECORD CONTAINS 120 CHARACTERS.
       01  SEQ-STUDENT-RECORD.
           05  SEQ-STUDENT-ID           PIC 9(6).
           05  SEQ-STUDENT-NAME         PIC X(30).
           05  SEQ-STUDENT-ADDRESS      PIC X(40).
           05  SEQ-STUDENT-PHONE        PIC X(15).
           05  SEQ-STUDENT-EMAIL        PIC X(20).
           05  SEQ-STUDENT-COURSE       PIC X(4).
           05  SEQ-INCLUSION-DATE       PIC 9(8).
           
       FD  STUDENT-VSAM-FILE
           RECORD CONTAINS 120 CHARACTERS.
       01  VSAM-STUDENT-RECORD.
           05  VSAM-STUDENT-ID          PIC 9(6).
           05  VSAM-STUDENT-NAME        PIC X(30).
           05  VSAM-STUDENT-ADDRESS     PIC X(40).
           05  VSAM-STUDENT-PHONE       PIC X(15).
           05  VSAM-STUDENT-EMAIL       PIC X(20).
           05  VSAM-STUDENT-COURSE      PIC X(4).
           05  VSAM-INCLUSION-DATE      PIC 9(8).
           
       WORKING-STORAGE SECTION.
       01  WS-SEQ-STATUS               PIC XX VALUE SPACES.
       01  WS-VSAM-STATUS              PIC XX VALUE SPACES.
       01  WS-END-OF-FILE              PIC X VALUE 'N'.
       01  WS-RECORD-COUNTER           PIC 9(6) VALUE ZEROES.
       01  WS-DISPLAY-COUNTER          PIC Z(5)9.
       
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-PROCESS-RECORDS UNTIL WS-END-OF-FILE = 'Y'.
           PERFORM 3000-TERMINATE.
           GOBACK.
       
       1000-INIT.
           OPEN INPUT STUDENT-SEQ-FILE.
           IF WS-SEQ-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING SEQUENTIAL FILE: ' WS-SEQ-STATUS
               MOVE 'Y' TO WS-END-OF-FILE
           ELSE
               OPEN OUTPUT STUDENT-VSAM-FILE
               IF WS-VSAM-STATUS NOT = '00'
                   DISPLAY 'ERROR OPENING VSAM FILE: ' WS-VSAM-STATUS
                   MOVE 'Y' TO WS-END-OF-FILE
               END-IF
           END-IF.
           
       2000-PROCESS-RECORDS.
           READ STUDENT-SEQ-FILE
               AT END
                   MOVE 'Y' TO WS-END-OF-FILE
               NOT AT END
                   MOVE SEQ-STUDENT-ID TO VSAM-STUDENT-ID
                   MOVE SEQ-STUDENT-NAME TO VSAM-STUDENT-NAME
                   MOVE SEQ-STUDENT-ADDRESS TO VSAM-STUDENT-ADDRESS
                   MOVE SEQ-STUDENT-PHONE TO VSAM-STUDENT-PHONE
                   MOVE SEQ-STUDENT-EMAIL TO VSAM-STUDENT-EMAIL
                   MOVE SEQ-STUDENT-COURSE TO VSAM-STUDENT-COURSE
                   MOVE SEQ-INCLUSION-DATE TO VSAM-INCLUSION-DATE
                   
                   WRITE VSAM-STUDENT-RECORD
                       INVALID KEY
                           DISPLAY 'ERROR WRITING RECORD: ' 
                                   VSAM-STUDENT-ID
                       NOT INVALID KEY
                           ADD 1 TO WS-RECORD-COUNTER
                   END-WRITE
           END-READ.
           
       3000-TERMINATE.
           CLOSE STUDENT-SEQ-FILE
                 STUDENT-VSAM-FILE.
           MOVE WS-RECORD-COUNTER TO WS-DISPLAY-COUNTER.
           DISPLAY WS-DISPLAY-COUNTER ' RECORDS CONVERTED'.
           DISPLAY 'PRESS ENTER TO CONTINUE...'.
           ACCEPT WS-END-OF-FILE.
