       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGU0003.
       AUTHOR. Mohand Kessi.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       
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
       01  WS-TEMP-RECORD.
           05  WS-TEMP-ID              PIC 9(6).
           05  WS-TEMP-NAME            PIC X(30).
           05  WS-TEMP-ADDRESS         PIC X(40).
           05  WS-TEMP-PHONE           PIC X(15).
           05  WS-TEMP-EMAIL           PIC X(20).
           05  WS-TEMP-COURSE          PIC X(4).
           05  WS-TEMP-INCLUSION-DATE  PIC 9(8).
       
       01  WS-FORMATTED-DATE.
           05  WS-FORMATTED-YEAR       PIC 9(4).
           05  FILLER                  PIC X VALUE '/'.
           05  WS-FORMATTED-MONTH      PIC 9(2).
           05  FILLER                  PIC X VALUE '/'.
           05  WS-FORMATTED-DAY        PIC 9(2).
           
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-PROCESS UNTIL WS-CONTINUE = 'N'.
           PERFORM 3000-TERMINATE.
           GOBACK.
           
       1000-INIT.
           OPEN I-O STUDENT-FILE.
           IF WS-FILE-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING STUDENT FILE: ' WS-FILE-STATUS
               MOVE 'N' TO WS-CONTINUE
           END-IF.
           
       2000-PROCESS.
           PERFORM 1100-CLEAR-SCREEN.
           DISPLAY '----------------------------------------'.
           DISPLAY '           UPDATE STUDENT               '.
           DISPLAY '----------------------------------------'.
           DISPLAY ' '.
           
           DISPLAY 'ENTER STUDENT ID TO UPDATE (OR 0 TO EXIT): '.
           ACCEPT STUDENT-ID.
           
           IF STUDENT-ID = 0
               MOVE 'N' TO WS-CONTINUE
           ELSE
               READ STUDENT-FILE
                   INVALID KEY
                       DISPLAY 'STUDENT ID NOT FOUND: ' STUDENT-ID
                       DISPLAY 'PRESS ENTER TO CONTINUE...'
                       ACCEPT WS-CONFIRMATION
                   NOT INVALID KEY
                       PERFORM 2100-FORMAT-DATE
                       MOVE STUDENT-RECORD TO WS-TEMP-RECORD
                       PERFORM 2200-DISPLAY-CURRENT
                       PERFORM 2300-UPDATE-FIELDS
               END-READ
           END-IF.
           
       1100-CLEAR-SCREEN.
           DISPLAY SPACE.
           DISPLAY SPACE.
           DISPLAY SPACE.
           DISPLAY SPACE.
           DISPLAY SPACE.
           
       2100-FORMAT-DATE.
           MOVE INCLUSION-DATE(1:4) TO WS-FORMATTED-YEAR.
           MOVE INCLUSION-DATE(5:2) TO WS-FORMATTED-MONTH.
           MOVE INCLUSION-DATE(7:2) TO WS-FORMATTED-DAY.
           
       2200-DISPLAY-CURRENT.
           DISPLAY 'CURRENT STUDENT INFORMATION:'.
           DISPLAY 'ID      : ' STUDENT-ID.
           DISPLAY 'NAME    : ' STUDENT-NAME.
           DISPLAY 'ADDRESS : ' STUDENT-ADDRESS.
           DISPLAY 'PHONE   : ' STUDENT-PHONE.
           DISPLAY 'EMAIL   : ' STUDENT-EMAIL.
           DISPLAY 'COURSE  : ' STUDENT-COURSE.
           DISPLAY 'DATE    : ' WS-FORMATTED-DATE.
           DISPLAY ' '.
           
       2300-UPDATE-FIELDS.
           DISPLAY 'ENTER NEW NAME (OR PRESS ENTER TO KEEP CURRENT): '.
           ACCEPT WS-TEMP-NAME.
           IF WS-TEMP-NAME NOT = SPACES
               MOVE WS-TEMP-NAME TO STUDENT-NAME
           END-IF.
           
           DISPLAY 'ENTER NEW ADDRESS (OR PRESS ENTER TO KEEP CURRENT): '.
           ACCEPT WS-TEMP-ADDRESS.
           IF WS-TEMP-ADDRESS NOT = SPACES
               MOVE WS-TEMP-ADDRESS TO STUDENT-ADDRESS
           END-IF.
           
           DISPLAY 'ENTER NEW PHONE (OR PRESS ENTER TO KEEP CURRENT): '.
           ACCEPT WS-TEMP-PHONE.
           IF WS-TEMP-PHONE NOT = SPACES
               MOVE WS-TEMP-PHONE TO STUDENT-PHONE
           END-IF.
           
           DISPLAY 'ENTER NEW EMAIL (OR PRESS ENTER TO KEEP CURRENT): '.
           ACCEPT WS-TEMP-EMAIL.
           IF WS-TEMP-EMAIL NOT = SPACES
               MOVE WS-TEMP-EMAIL TO STUDENT-EMAIL
           END-IF.
           
           DISPLAY 'ENTER NEW COURSE CODE (OR PRESS ENTER TO KEEP CURRENT): '.
           ACCEPT WS-TEMP-COURSE.
           IF WS-TEMP-COURSE NOT = SPACES
               MOVE WS-TEMP-COURSE TO STUDENT-COURSE
           END-IF.
           
           DISPLAY ' '.
           DISPLAY 'CONFIRM UPDATE (Y/N)? '.
           ACCEPT WS-CONFIRMATION.
           
           IF WS-CONFIRMATION = 'Y' OR WS-CONFIRMATION = 'y'
               REWRITE STUDENT-RECORD
                   INVALID KEY
                       DISPLAY 'ERROR UPDATING RECORD: ' WS-FILE-STATUS
                   NOT INVALID KEY
                       DISPLAY 'STUDENT SUCCESSFULLY UPDATED!'
               END-REWRITE
           ELSE
               DISPLAY 'UPDATE CANCELLED'
           END-IF.
           
           DISPLAY 'PRESS ENTER TO CONTINUE...'.
           ACCEPT WS-CONFIRMATION.
           
       3000-TERMINATE.
           CLOSE STUDENT-FILE.