       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGI0002.
       AUTHOR. Mohand Kessi.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INSERT-FILE ASSIGN TO 'INSERT.DAT'.
       
       DATA DIVISION.
       FILE SECTION.
       FD INSERT-FILE.
       01 INSERT-RECORD.
          05 F-STUD-ID        PIC X(04).
          05 F-STUD-NAME      PIC X(25).
          05 F-STUD-DOB       PIC X(08).
          05 F-STUD-COURSE    PIC X(15).
          05 F-STUD-INS-DATE  PIC X(08).
          05 F-STUD-UPDT-DATE PIC X(08).
       
       WORKING-STORAGE SECTION.
       01 WS-EOF             PIC X VALUE 'N'.
       01 WS-NEXT-ID         PIC 9(04).
       01 WS-ID-CHAR         PIC X(04).
       
       EXEC SQL INCLUDE SQLCA END-EXEC.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT INSERT-FILE
       
           PERFORM UNTIL WS-EOF = 'Y'
               READ INSERT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM GENERATE-ID
                       PERFORM INSERT-STUDENT
               END-READ
           END-PERFORM
       
           CLOSE INSERT-FILE
           STOP RUN.
       
       GENERATE-ID.
           EXEC SQL
               SELECT MAX(STUD_ID) INTO :WS-ID-CHAR FROM STUDENT
           END-EXEC
       
           IF SQLCODE = 0
               COMPUTE WS-NEXT-ID = FUNCTION NUMVAL(WS-ID-CHAR) + 1
               MOVE WS-NEXT-ID TO F-STUD-ID
           ELSE
               MOVE '0001' TO F-STUD-ID
           END-IF.
       
       INSERT-STUDENT.
           EXEC SQL
               INSERT INTO STUDENT
               (STUD_ID, STUD_NAME, STUD_DOB, STUD_COURSE,
                STUD_INS_DATE, STUD_UPDT_DATE)
               VALUES
               (:F-STUD-ID, :F-STUD-NAME, :F-STUD-DOB,
                :F-STUD-COURSE, :F-STUD-INS-DATE, :F-STUD-UPDT-DATE)
           END-EXEC.
       
           IF SQLCODE NOT = 0
               DISPLAY 'INSERT FAILED FOR ID: ' F-STUD-ID
           END-IF.
       