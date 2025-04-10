       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGD0004.
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
           DISPLAY '           DELETE STUDENT               '.
           DISPLAY '----------------------------------------'.
           DISPLAY ' '.
           
           DISPLAY 'ENTER STUDENT ID TO DELETE (OR 0 TO EXIT): '.
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
                       PERFORM 2200-DISPLAY-STUDENT
                       PERFORM 2300-CONFIRM-DELETE
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
           
       2200-DISPLAY-STUDENT.
           DISPLAY 'STUDENT INFORMATION:'.
           DISPLAY 'ID      : ' STUDENT-ID.
           DISPLAY 'NAME    : ' STUDENT-NAME.
           DISPLAY 'ADDRESS : ' STUDENT-ADDRESS.
           DISPLAY 'PHONE   : ' STUDENT-PHONE.
           DISPLAY 'EMAIL   : ' STUDENT-EMAIL.
           DISPLAY 'COURSE  : ' STUDENT-