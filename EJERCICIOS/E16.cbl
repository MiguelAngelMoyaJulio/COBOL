      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-06
      * Desarrollar un programa tal que, dado un valor entero positivo 
      * que se ingresa por teclado indique 
      * si se trata de un número primo o no.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E16.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-VAR. 
           02 WS-NUM                 PIC 9(03) VALUE ZEROS. 
           02 WS-J                   PIC 9(03) VALUE ZEROS. 
           02 WS-RES                 PIC 9(03) VALUE ZEROS. 
           02 WS-MOD                 PIC 9(03) VALUE ZEROS. 
           02 WS-CANT                PIC 9(03) VALUE ZEROS. 
       PROCEDURE DIVISION.
           PERFORM 10-LOAD
              THRU 10-LOAD-F

           PERFORM 20-PROCESS
              THRU 20-PROCESS-F
              
           PERFORM 30-TOTAL
           THRU    30-TOTAL-F   
           .
           STOP RUN.
       10-LOAD.
           DISPLAY "ENTER A NUMER "
           ACCEPT WS-NUM 
           . 
       10-LOAD-F.

       20-PROCESS.
           PERFORM VARYING WS-J FROM 1 BY 1
           UNTIL WS-J > WS-NUM
               DIVIDE WS-NUM BY WS-J GIVING WS-RES REMAINDER WS-MOD 
               IF WS-MOD = 0
                   ADD 1 TO WS-CANT
               END-IF
           END-PERFORM
           .
       20-PROCESS-F.

       30-TOTAL.
           IF WS-CANT = 2
               DISPLAY "ES NUMERO PRIMO"
           ELSE
               DISPLAY "NO ES NUMERO PRIMO"
           END-IF
           .           
       30-TOTAL-F.
       END PROGRAM E16.
