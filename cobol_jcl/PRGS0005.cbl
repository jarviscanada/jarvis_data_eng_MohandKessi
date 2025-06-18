       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGR0005.
       AUTHOR. Mohand Kessi.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORT-FILE ASSIGN TO 'REPORT_ALL.DAT'.
       
       DATA DIVISION.
       FILE SECTION.
       FD REPORT-FILE.
       01 REPORT-RECORD       PIC X(100).
       
       WORKING-STORAGE SECTION.
       EXEC SQL INCLUDE SQLCA END-EXEC.
       
       01 WS-STUDENT-ID        PIC X(04).
       01 WS-STUDENT-NAME      PIC X(25).
       01 WS-STUDENT-DOB       PIC X(08).
       01 WS-STUDENT-COURSE    PIC X(15).
       01 WS-STUDENT-INS-DATE  PIC X(08).
       01 WS-STUDENT-UPD-DATE  PIC X(08).
       01 WS-LINE              PIC X(100).
       01 SQL-END              PIC X VALUE 'N'.
       
       EXEC SQL
           DECLARE C1 CURSOR FOR
               SELECT STUD_ID, STUD_NAME, STUD_DOB, STUD_COURSE,
                      STUD_INS_DATE, STUD_UPDT_DATE
               FROM STUDENT
       END-EXEC.
       
       PROCEDURE DIVISION.
           OPEN OUTPUT REPORT-FILE
       
           EXEC SQL
               OPEN C1
           END-EXEC
       
           PERFORM UNTIL SQL-END = 'Y'
               EXEC SQL
                   FETCH C1 INTO :WS-STUDENT-ID, :WS-STUDENT-NAME,
                                   :WS-STUDENT-DOB, :WS-STUDENT-COURSE,
                                   :WS-STUDENT-INS-DATE, :WS-STUDENT-UPD-DATE
               END-EXEC
       
               IF SQLCODE = 0
                   STRING WS-STUDENT-ID DELIMITED BY SIZE
                          ' '             DELIMITED BY SIZE
                          WS-STUDENT-NAME DELIMITED BY SIZE
                          ' '             DELIMITED BY SIZE
                          WS-STUDENT-DOB DELIMITED BY SIZE
                          ' '             DELIMITED BY SIZE
                          WS-STUDENT-COURSE DELIMITED BY SIZE
                          ' '             DELIMITED BY SIZE
                          WS-STUDENT-INS-DATE DELIMITED BY SIZE
                          ' '             DELIMITED BY SIZE
                          WS-STUDENT-UPD-DATE DELIMITED BY SIZE
                          INTO WS-LINE
                   WRITE REPORT-RECORD FROM WS-LINE
               ELSE
                   MOVE 'Y' TO SQL-END
               END-IF
           END-PERFORM
       
           EXEC SQL
               CLOSE C1
           END-EXEC
       
           CLOSE REPORT-FILE
           STOP RUN.
       