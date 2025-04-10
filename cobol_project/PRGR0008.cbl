       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGR0008.
       AUTHOR. Mohand Kessi.
       
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO 'STUDENT.VSAM'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS STUDENT-ID
               FILE STATUS IS WS-FILE-STATUS.
               
           SELECT REPORT-FILE ASSIGN TO 'COURSE-REPORT.TXT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-REPORT-STATUS.
       
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
           
       FD  REPORT-FILE
           RECORD CONTAINS 132 CHARACTERS.
       01  REPORT-LINE                 PIC X(132).
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS              PIC XX VALUE SPACES.
       01  WS-REPORT-STATUS            PIC XX VALUE SPACES.
       01  WS-END-OF-FILE              PIC X VALUE 'N'.
       01  WS-CONTINUE                 PIC X VALUE SPACES.
       01  WS-CURRENT-COURSE           PIC X(4) VALUE SPACES.
       01  WS-PREV-COURSE              PIC X(4) VALUE SPACES.
       01  WS-COURSE-COUNTER           PIC 9(4) VALUE ZEROES.
       01  WS-TOTAL-STUDENTS           PIC 9(6) VALUE ZEROES.
       01  WS-PAGE-NUMBER              PIC 9(3) VALUE 1.
       01  WS-LINE-COUNT               PIC 9(3) VALUE 0.
       01  WS-LINES-PER-PAGE           PIC 9(3) VALUE 50.
       
       01  WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR         PIC 9(4).
           05  WS-CURRENT-MONTH        PIC 9(2).
           05  WS-CURRENT-DAY          PIC 9(2).
       
       01  WS-FORMATTED-DATE.
           05  WS-FORMATTED-MONTH      PIC 9(2).
           05  FILLER                  PIC X VALUE '/'.
           05  WS-FORMATTED-DAY        PIC 9(2).
           05  FILLER                  PIC X VALUE '/'.
           05  WS-FORMATTED-YEAR       PIC 9(4).
       
       01  WS-HEADING-1.
           05  FILLER                  PIC X(28) VALUE SPACES.
           05  FILLER                  PIC X(26) 
                VALUE 'STUDENT REGISTRATION SYSTEM'.
           05  FILLER                  PIC X(20) VALUE SPACES.
           05  FILLER                  PIC X(5) VALUE 'DATE:'.
           05  FILLER                  PIC X VALUE SPACE.
           05  WS-HEADING-DATE         PIC X(10).
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  FILLER                  PIC X(5) VALUE 'PAGE:'.
           05  FILLER                  PIC X VALUE SPACE.
           05  WS-HEADING-PAGE         PIC ZZ9.
           
       01  WS-HEADING-2.
           05  FILLER                  PIC X(30) VALUE SPACES.
           05  FILLER                  PIC X(22) 
                VALUE 'STUDENT COURSE REPORT'.
           05  FILLER                  PIC X(80) VALUE SPACES.
            
       01  WS-HEADING-3.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  FILLER                  PIC X(6) VALUE 'ID'.
           05  FILLER                  PIC X(4) VALUE SPACES.
           05  FILLER                  PIC X(4) VALUE 'NAME'.
           05  FILLER                  PIC X(27) VALUE SPACES.
           05  FILLER                  PIC X(6) VALUE 'COURSE'.
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  FILLER                  PIC X(5) VALUE 'DATE'.
           05  FILLER                  PIC X(72) VALUE SPACES.
           
       01  WS-HEADING-4.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  FILLER                  PIC X(65) 
                VALUE ALL '-'.
           05  FILLER                  PIC X(64) VALUE SPACES.
       
       01  WS-DETAIL-LINE.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  WS-DET-STUDENT-ID       PIC 9(6).
           05  FILLER                  PIC X(4) VALUE SPACES.
           05  WS-DET-STUDENT-NAME     PIC X(30).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  WS-DET-STUDENT-COURSE   PIC X(4).
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  WS-DET-INCLUSION-DATE   PIC X(10).
           05  FILLER                  PIC X(68) VALUE SPACES.
       
       01  WS-COURSE-HEADER.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  FILLER                  PIC X(7) VALUE 'COURSE:'.
           05  FILLER                  PIC X VALUE SPACE.
           05  WS-HEAD-COURSE-CODE     PIC X(4).
           05  FILLER                  PIC X(117) VALUE SPACES.
           
       01  WS-COURSE-FOOTER.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  FILLER                  PIC X(21) 
                VALUE 'TOTAL STUDENTS IN COURSE'.
           05  FILLER                  PIC X VALUE SPACE.
           05  WS-FOOT-COURSE-CODE     PIC X(4).
           05  FILLER                  PIC X(2) VALUE ':'.
           05  FILLER                  PIC X VALUE SPACE.
           05  WS-FOOT-COURSE-COUNT    PIC Z,ZZ9.
           05  FILLER                  PIC X(95) VALUE SPACES.
           
       01  WS-TOTAL-LINE.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  FILLER                  PIC X(37) 
                VALUE 'TOTAL STUDENTS IN ALL COURSES:'.
           05  FILLER                  PIC X VALUE SPACE.
           05  WS-TOTAL-COUNT          PIC Z,ZZ9.
           05  FILLER                  PIC X(86) VALUE SPACES.
           
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-PROCESS-RECORDS UNTIL WS-END-OF-FILE = 'Y'.
           PERFORM 3000-PRINT-TOTAL.
           PERFORM 4000-TERMINATE.
           GOBACK.
           
       1000-INIT.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE WS-CURRENT-MONTH TO WS-FORMATTED-MONTH.
           MOVE WS-CURRENT-DAY TO WS-FORMATTED-DAY.
           MOVE WS-CURRENT-YEAR TO WS-FORMATTED-YEAR.
           MOVE WS-FORMATTED-DATE TO WS-HEADING-DATE.
           
           OPEN INPUT STUDENT-FILE.
           IF WS-FILE-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING STUDENT FILE: ' WS-FILE-STATUS
               MOVE 'Y' TO WS-END-OF-FILE
           ELSE
               OPEN OUTPUT REPORT-FILE
               IF WS-REPORT-STATUS NOT = '00'
                   DISPLAY 'ERROR OPENING REPORT FILE: ' WS-REPORT-STATUS
                   MOVE 'Y' TO WS-END-OF-FILE
               ELSE
                   PERFORM 1100-PRINT-HEADERS
               END-IF
           END-IF.
           
       1100-PRINT-HEADERS.
           MOVE WS-PAGE-NUMBER TO WS-HEADING-PAGE.
           WRITE REPORT-LINE FROM WS-HEADING-1.
           WRITE REPORT-LINE FROM WS-HEADING-2.
           WRITE REPORT-LINE FROM SPACES.
           WRITE REPORT-LINE FROM WS-HEADING-3.
           WRITE REPORT-LINE FROM WS-HEADING-4.
           MOVE 5 TO WS-LINE-COUNT.
           
       2000-PROCESS-RECORDS.
           READ STUDENT-FILE NEXT
               AT END
                   MOVE 'Y' TO WS-END-OF-FILE
                   IF WS-PREV-COURSE NOT = SPACES
                       PERFORM 2300-PRINT-COURSE-FOOTER
                   END-IF
               NOT AT END
                   MOVE STUDENT-COURSE TO WS-CURRENT-COURSE
                   IF WS-CURRENT-COURSE NOT = WS-PREV-COURSE
                       IF WS-PREV-COURSE NOT = SPACES
                           PERFORM 2300-PRINT-COURSE-FOOTER
                       END-IF
                       PERFORM 2100-PRINT-COURSE-HEADER
                   END-IF
                   PERFORM 2200-PRINT-STUDENT-DETAIL
                   MOVE WS-CURRENT-COURSE TO WS-PREV-COURSE
           END-READ.
           
       2100-PRINT-COURSE-HEADER.
           IF WS-LINE-COUNT > (WS-LINES-PER-PAGE - 5)
               PERFORM 2400-NEW-PAGE
           END-IF.
           
           MOVE WS-CURRENT-COURSE TO WS-HEAD-COURSE-CODE.
           WRITE REPORT-LINE FROM SPACES.
           WRITE REPORT-LINE FROM WS-COURSE-HEADER.
           WRITE REPORT-LINE FROM SPACES.
           ADD 3 TO WS-LINE-COUNT.
           MOVE 0 TO WS-COURSE-COUNTER.
           
       2200-PRINT-STUDENT-DETAIL.
           IF WS-LINE-COUNT > (WS-LINES-PER-PAGE - 2)
               PERFORM 2400-NEW-PAGE
           END-IF.
           
           MOVE STUDENT-ID TO WS-DET-STUDENT-ID.
           MOVE STUDENT-NAME TO WS-DET-STUDENT-NAME.
           MOVE STUDENT-COURSE TO WS-DET-STUDENT-COURSE.
           
           STRING INCLUSION-DATE(5:2) '/'
                  INCLUSION-DATE(7:2) '/'
                  INCLUSION-DATE(1:4)
                  DELIMITED BY SIZE
                  INTO WS-DET-INCLUSION-DATE.
                  
           WRITE REPORT-LINE FROM WS-DETAIL-LINE.
           ADD 1 TO WS-LINE-COUNT.
           ADD 1 TO WS-COURSE-COUNTER.
           ADD 1 TO WS-TOTAL-STUDENTS.
           
       2300-PRINT-COURSE-FOOTER.
           IF WS-LINE-COUNT > (WS-LINES-PER-PAGE - 3)
               PERFORM 2400-NEW-PAGE
           END-IF.
           
           MOVE WS-PREV-COURSE TO WS-FOOT-COURSE-CODE.
           MOVE WS-COURSE-COUNTER TO WS-FOOT-COURSE-COUNT.
           WRITE REPORT-LINE FROM SPACES.
           WRITE REPORT-LINE FROM WS-COURSE-FOOTER.
           WRITE REPORT-LINE FROM SPACES.
           ADD 3 TO WS-LINE-COUNT.
           
       2400-NEW-PAGE.
           ADD 1 TO WS-PAGE-NUMBER.
           MOVE WS-PAGE-NUMBER TO WS-HEADING-PAGE.
           WRITE REPORT-LINE FROM SPACES AFTER PAGE.
           WRITE REPORT-LINE FROM WS-HEADING-1.
           WRITE REPORT-LINE FROM WS-HEADING-2.
           WRITE REPORT-LINE FROM SPACES.
           WRITE REPORT-LINE FROM WS-HEADING-3.
           WRITE REPORT-LINE FROM WS-HEADING-4.
           MOVE 5 TO WS-LINE-COUNT.
           
       3000-PRINT-TOTAL.
           IF NOT WS-END-OF-FILE = 'Y'
               GO TO 3000-EXIT
           END-IF.
           
           IF WS-LINE-COUNT > (WS-LINES-PER-PAGE - 4)
               PERFORM 2400-NEW-PAGE
           END-IF.
           
           MOVE WS-TOTAL-STUDENTS TO WS-TOTAL-COUNT.
           WRITE REPORT-LINE FROM SPACES.
           WRITE REPORT-LINE FROM WS-HEADING-4.
           WRITE REPORT-LINE FROM WS-TOTAL-LINE.
           
       3000-EXIT.
           EXIT.
           
       4000-TERMINATE.
           CLOSE STUDENT-FILE
                 REPORT-FILE.
                 
           DISPLAY 'REPORT GENERATED: COURSE-REPORT.TXT'.
           DISPLAY 'TOTAL STUDENTS PROCESSED: ' WS-TOTAL-STUDENTS.
           DISPLAY 'PRESS ENTER TO RETURN TO MENU...'.
           ACCEPT WS-CONTINUE.