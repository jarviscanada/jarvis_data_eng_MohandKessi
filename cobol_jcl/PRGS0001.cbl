       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGS0001.
       AUTHOR. Mohand Kessi.


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTROL-FILE ASSIGN TO 'CONTROL.DAT'.
           SELECT INSERT-FILE  ASSIGN TO 'INSERT.DAT'.
           SELECT UPDATE-FILE  ASSIGN TO 'UPDATE.DAT'.
           SELECT DELETE-FILE  ASSIGN TO 'DELETE.DAT'.
       DATA DIVISION.
       FILE SECTION.
       FD CONTROL-FILE.
       01 CONTROL-RECORD.
          05 OP-CODE          PIC X(01).
          05 STUD-ID          PIC X(04).
          05 STUD-NAME        PIC X(25).
          05 STUD-DOB         PIC X(08).
          05 STUD-COURSE      PIC X(15).
          05 STUD-INS-DATE    PIC X(08).
          05 STUD-UPDT-DATE   PIC X(08).
       FD INSERT-FILE.
       01 INSERT-RECORD       PIC X(69).
       FD UPDATE-FILE.
       01 UPDATE-RECORD       PIC X(69).
       FD DELETE-FILE.
       01 DELETE-RECORD       PIC X(69).
       WORKING-STORAGE SECTION.
       01 EOF-FLAG            PIC X VALUE 'N'.
       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT CONTROL-FILE
                OUTPUT INSERT-FILE
                OUTPUT UPDATE-FILE
                OUTPUT DELETE-FILE
           PERFORM UNTIL EOF-FLAG = 'Y'
               READ CONTROL-FILE
                   AT END MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       EVALUATE OP-CODE
                           WHEN 'I' WRITE INSERT-RECORD FROM CONTROL-RECORD
                           WHEN 'U' WRITE UPDATE-RECORD FROM CONTROL-RECORD
                           WHEN 'D' WRITE DELETE-RECORD FROM CONTROL-RECORD
                       END-EVALUATE
               END-READ
           END-PERFORM
           CLOSE CONTROL-FILE INSERT-FILE UPDATE-FILE DELETE-FILE
           STOP RUN.
      