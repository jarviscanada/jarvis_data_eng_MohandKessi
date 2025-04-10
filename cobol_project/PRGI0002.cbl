       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGI0002.
       AUTHOR. Mohand Kessi.
       
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO 'STUDENT.VSAM'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS STUDENT-ID
               FILE STATUS IS WS-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  STUDENT-FILE
           RECORD CONTAINS 120 CHARACTERS.
       01  STUDENT-RECORD.
           05  STUDENT-ID              PIC 9(6).
           05  STUDENT-NAME            PIC X(30).
           05  STUDENT-ADDRESS         PIC X(40).
           05  STUDENT-PHONE           PIC X(15).
           05  STUDENT-EMAIL           PIC X(20).
           05  STUDENT-COURSE          PIC X(4).
           05  INCLUSION-DATE          PIC 9(8).
           
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS              PIC XX VALUE SPACES.
       01  WS-CONTINUE                 PIC X VALUE 'Y'.
       01  WS-CONFIRMATION             PIC X VALUE SPACES.
       01  WS-TEMP-ID                  PIC 9(6).
       
       01  WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR         PIC 9(4).
           05  WS-CURRENT-MONTH        PIC 9(2).
           05  WS-CURRENT-DAY          PIC 9(2).
           05  WS-CURRENT-HOURS        PIC 9(2).
           05  WS-CURRENT-MINUTES      PIC 9(2).
           05  WS-CURRENT-SECONDS      PIC 9(2).
           05  WS-CURRENT-MSECS        PIC 9(2).
           
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-PROCESS UNTIL WS-CONTINUE = 'N'.
           PERFORM 3000-TERMINATE.
           GOBACK.
           
       1000-INIT.
           OPEN I-O STUDENT-FILE.
           IF WS-FILE-STATUS NOT = '00' AND 
              WS-FILE-STATUS NOT = '05'
               DISPLAY 'ERROR OPENING STUDENT FILE: ' WS-FILE-STATUS
               MOVE 'N' TO WS-CONTINUE
           END-IF.
           
       2000-PROCESS.
           PERFORM 1100-CLEAR-SCREEN.
           DISPLAY '----------------------------------------'.
           DISPLAY '           ADD NEW STUDENT              '.
           DISPLAY '----------------------------------------'.
           DISPLAY ' '.
           
           DISPLAY 'ENTER STUDENT ID (6 DIGITS) OR 0 TO EXIT: '.
           ACCEPT STUDENT-ID.
           
           IF STUDENT-ID = 0
               MOVE 'N' TO WS-CONTINUE
           ELSE
               MOVE STUDENT-ID TO WS-TEMP-ID
               READ STUDENT-FILE
                   INVALID KEY
                       PERFORM 2100-GET-STUDENT-DATA
                   NOT INVALID KEY
                       DISPLAY 'STUDENT ID ALREADY EXISTS: ' STUDENT-ID
                       DISPLAY 'PRESS ENTER TO CONTINUE...'
                       ACCEPT WS-CONFIRMATION
               END-READ
           END-IF.
           
       1100-CLEAR-SCREEN.
           DISPLAY SPACE.
           DISPLAY SPACE.
           DISPLAY SPACE.
           DISPLAY SPACE.
           DISPLAY SPACE.
           
       2100-GET-STUDENT-DATA.
           MOVE WS-TEMP-ID TO STUDENT-ID.
           
           DISPLAY 'ENTER STUDENT NAME: '.
           ACCEPT STUDENT-NAME.
           
           DISPLAY 'ENTER STUDENT ADDRESS: '.
           ACCEPT STUDENT-ADDRESS.
           
           DISPLAY 'ENTER STUDENT PHONE: '.
           ACCEPT STUDENT-PHONE.
           
           DISPLAY 'ENTER STUDENT EMAIL: '.
           ACCEPT STUDENT-EMAIL.
           
           DISPLAY 'ENTER STUDENT COURSE CODE: '.
           ACCEPT STUDENT-COURSE.
           
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE.
           STRING WS-CURRENT-YEAR 
                  WS-CURRENT-MONTH 
                  WS-CURRENT-DAY 
                  DELIMITED BY SIZE
                  INTO INCLUSION-DATE.
           
           DISPLAY ' '.
           DISPLAY 'CONFIRM DATA (Y/N)? '.
           ACCEPT WS-CONFIRMATION.
           
           IF WS-CONFIRMATION = 'Y' OR WS-CONFIRMATION = 'y'
               WRITE STUDENT-RECORD
                   INVALID KEY
                       DISPLAY 'ERROR WRITING RECORD: ' WS-FILE-STATUS
                   NOT INVALID KEY
                       DISPLAY 'STUDENT SUCCESSFULLY ADDED!'
               END-WRITE
           ELSE
               DISPLAY 'OPERATION CANCELLED'
           END-IF.
           
           DISPLAY 'PRESS ENTER TO CONTINUE...'.
           ACCEPT WS-CONFIRMATION.
           
       3000-TERMINATE.
           CLOSE STUDENT-FILE.
