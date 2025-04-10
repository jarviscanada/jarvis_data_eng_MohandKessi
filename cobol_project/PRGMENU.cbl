       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGMENU.
       AUTHOR. Mohand Kessi.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-OPTION                     PIC 9(2).
       01  WS-ERROR-MESSAGE              PIC X(50).
       01  WS-CONTINUE                   PIC X VALUE 'Y'.
       
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM UNTIL WS-CONTINUE = 'N'
               PERFORM 1000-DISPLAY-MENU
               ACCEPT WS-OPTION
               EVALUATE WS-OPTION
                   WHEN 1
                       CALL 'PRGV0001'
                   WHEN 2
                       CALL 'PRGQ0005'
                   WHEN 3
                       CALL 'PRGI0002'
                   WHEN 4
                       CALL 'PRGU0003'
                   WHEN 5
                       CALL 'PRGD0004'
                   WHEN 6
                       CALL 'PRGQ0006'
                   WHEN 7
                       CALL 'PRGQ0007'
                   WHEN 8
                       CALL 'PRGR0008'
                   WHEN 9
                       MOVE 'N' TO WS-CONTINUE
                   WHEN OTHER
                       MOVE 'Invalid option. Please try again.' 
                         TO WS-ERROR-MESSAGE
                       DISPLAY WS-ERROR-MESSAGE
               END-EVALUATE
           END-PERFORM.
           
           STOP RUN.
           
       1000-DISPLAY-MENU.
           PERFORM 1100-CLEAR-SCREEN.
           DISPLAY '----------------------------------------'.
           DISPLAY '      STUDENT REGISTRATION SYSTEM       '.
           DISPLAY '----------------------------------------'.
           DISPLAY ' '.
           DISPLAY ' 1. Convert File to VSAM'.
           DISPLAY ' 2. Query All Students'.
           DISPLAY ' 3. Add New Student'.
           DISPLAY ' 4. Update Student'.
           DISPLAY ' 5. Delete Student'.
           DISPLAY ' 6. Query Student by ID'.
           DISPLAY ' 7. Query Student by Inclusion Date'.
           DISPLAY ' 8. Generate Report with Course Break'.
           DISPLAY ' 9. Exit'.
           DISPLAY ' '.
           DISPLAY 'Please enter your choice (1-9): '.
       
       1100-CLEAR-SCREEN.
           DISPLAY SPACE.
           DISPLAY SPACE.
           DISPLAY SPACE.
           DISPLAY SPACE.
           DISPLAY SPACE.