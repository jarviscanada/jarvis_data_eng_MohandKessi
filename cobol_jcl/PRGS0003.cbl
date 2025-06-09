       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGS0003.
       AUTHOR. Mohand Kessi.       
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ERROR-FILE ASSIGN TO 'ERROR.LOG'.
       
       DATA DIVISION.
       FILE SECTION.
       FD ERROR-FILE.
       01 ERROR-RECORD    PIC X(80).
       
       WORKING-STORAGE SECTION.
       01 WS-PROGRAM-ID       PIC X(08) VALUE SPACES.
       01 WS-ERROR-TYPE       PIC X(20) VALUE SPACES.
       01 WS-STUDENT-ID       PIC X(04) VALUE SPACES.
       01 WS-MESSAGE          PIC X(80) VALUE SPACES.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN OUTPUT ERROR-FILE
       
           MOVE 'PRGD0004'       TO WS-PROGRAM-ID
           MOVE 'DELETE FAILED'  TO WS-ERROR-TYPE
           MOVE '0005'           TO WS-STUDENT-ID
       
           STRING WS-PROGRAM-ID DELIMITED BY SIZE
                  ' | '         DELIMITED BY SIZE
                  WS-ERROR-TYPE DELIMITED BY SIZE
                  ' | ID: '     DELIMITED BY SIZE
                  WS-STUDENT-ID DELIMITED BY SIZE
                  INTO WS-MESSAGE
       
           WRITE ERROR-RECORD FROM WS-MESSAGE
       
           CLOSE ERROR-FILE
           STOP RUN.