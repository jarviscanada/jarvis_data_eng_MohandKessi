       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGD0004.
       AUTHOR. Mohand Kessi.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DELETE-FILE ASSIGN TO 'DELETE.DAT'.
           SELECT ERROR-FILE  ASSIGN TO 'ERROR.LOG'.
       
       DATA DIVISION.
       FILE SECTION.
       FD DELETE-FILE.
       01 DELETE-RECORD.
          05 D-STUD-ID       PIC X(04).
          05 FILLER          PIC X(65).
       
       FD ERROR-FILE.
       01 ERROR-RECORD       PIC X(80).
       
       WORKING-STORAGE SECTION.
       01 WS-EOF             PIC X VALUE 'N'.
       01 WS-MESSAGE         PIC X(80).
       01 WS-PROGRAM-ID      PIC X(08) VALUE 'PRGD0004'.
       01 WS-ERROR-TEXT      PIC X(20).
       
       EXEC SQL INCLUDE SQLCA END-EXEC.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT DELETE-FILE
                OUTPUT ERROR-FILE
       
           PERFORM UNTIL WS-EOF = 'Y'
               READ DELETE-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM DELETE-STUDENT
               END-READ
           END-PERFORM
       
           CLOSE DELETE-FILE
                 ERROR-FILE
           STOP RUN.
       
       DELETE-STUDENT.
           EXEC SQL
               DELETE FROM STUDENT
               WHERE STUD_ID = :D-STUD-ID
           END-EXEC
       
           IF SQLCODE NOT = 0
               MOVE 'DELETE FAILED' TO WS-ERROR-TEXT
               STRING WS-PROGRAM-ID DELIMITED BY SIZE
                      ' | ' DELIMITED BY SIZE
                      WS-ERROR-TEXT DELIMITED BY SIZE
                      ' | ID: ' DELIMITED BY SIZE
                      D-STUD-ID DELIMITED BY SIZE
                      INTO WS-MESSAGE
               WRITE ERROR-RECORD FROM WS-MESSAGE
           END-IF.
       