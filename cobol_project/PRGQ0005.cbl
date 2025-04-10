IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGQ0005.
       AUTHOR. Mohand Kessi.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO 'STUDENT.VSAM'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
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
       01  WS-END-OF-FILE              PIC X VALUE 'N'.
       01  WS-RECORD-COUNTER           PIC 9(6) VALUE ZEROES.
       01  WS-PAGE-COUNTER             PIC 9(3) VALUE 1.
       01  WS-LINES-PER-PAGE           PIC 9(2) VALUE 20.
       01  WS-LINE-COUNTER             PIC 9(2) VALUE ZEROES.
       01  WS-CONTINUE                 PIC X VALUE SPACES.
       
       01  WS-FORMATTED-DATE.
           05  WS-FORMATTED-YEAR       PIC 9(4).
           05  FILLER                  PIC X VALUE '/'.
           05  WS-FORMATTED-MONTH      PIC 9(2).
           05  FILLER                  PIC X VALUE '/'.
           05  WS-FORMATTED-DAY        PIC 9(2).
           
       01  WS-HEADER.
           05  FILLER                  PIC X(6) VALUE 'ID'.
           05  FILLER                  PIC X(4) VALUE SPACES.
           05  FILLER                  PIC X(4) VALUE 'NAME'.
           05  FILLER                  PIC X(30) VALUE SPACES.
           05  FILLER                  PIC X(6) VALUE 'COURSE'.
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  FILLER                  PIC X(4) VALUE 'DATE'.
           
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-PROCESS-RECORDS UNTIL WS-END-OF-FILE = 'Y'.
           PERFORM 3000-TERMINATE.
           GOBACK.
           
       1000-INIT.
           OPEN INPUT STUDENT-FILE.
           IF WS-FILE-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING STUDENT FILE: ' WS-FILE-STATUS
               MOVE 'Y' TO WS-END-OF-FILE
           ELSE
               PERFORM 1100-PRINT-HEADER
           END-IF.
           
       1100-PRINT-HEADER.
           PERFORM 1110-CLEAR-SCREEN.
           DISPLAY '----------------------------------------'.
           DISPLAY '           LIST OF ALL STUDENTS         '.
           DISPLAY '----------------------------------------'.
           DISPLAY ' '.
           DISPLAY WS-HEADER.
           DISPLAY '----------------------------------------'.
           ADD 5 TO WS-LINE-COUNTER.
           
       1110-CLEAR-SCREEN.
           DISPLAY SPACE.
           DISPLAY SPACE.
           DISPLAY SPACE.
           DISPLAY SPACE.
           DISPLAY SPACE.
           
       2000-PROCESS-RECORDS.
           READ STUDENT-FILE NEXT
               AT END
                   MOVE 'Y' TO WS-END-OF-FILE
               NOT AT END
                   PERFORM 2100-DISPLAY-RECORD
           END-READ.
           
       2100-DISPLAY-RECORD.
           ADD 1 TO WS-RECORD-COUNTER.
           ADD 1 TO WS-LINE-COUNTER.
           
           IF WS-LINE-COUNTER > WS-LINES-PER-PAGE
               ADD 1 TO WS-PAGE-COUNTER
               DISPLAY 'Press ENTER to continue...'
               ACCEPT WS-CONTINUE
               PERFORM 1100-PRINT-HEADER
           END-IF.
           
           PERFORM 2110-FORMAT-DATE.
           
           DISPLAY STUDENT-ID '  ' STUDENT-NAME(1:30) '  ' 
                  STUDENT-COURSE '     ' WS-FORMATTED-DATE.
                  
       2110-FORMAT-DATE.
           MOVE INCLUSION-DATE(1:4) TO WS-FORMATTED-YEAR.
           MOVE INCLUSION-DATE(5:2) TO WS-FORMATTED-MONTH.
           MOVE INCLUSION-DATE(7:2) TO WS-FORMATTED-DAY.
           
       3000-TERMINATE.
           CLOSE STUDENT-FILE.
           DISPLAY ' '.
           DISPLAY '----------------------------------------'.
           DISPLAY 'TOTAL RECORDS: ' WS-RECORD-COUNTER.
           DISPLAY 'Press ENTER to return to menu...'.
           ACCEPT WS-CONTINUE.
