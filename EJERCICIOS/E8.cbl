      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-06
      * Se ingresa un valor numérico entero n, se pide desarrollar un 
      * algoritmo que muestre por consola 
      * los primeros n números naturales.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E8.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-N PIC 9(04). 
       77 WS-I PIC 9(04). 
       PROCEDURE DIVISION.
           PERFORM 100-LOAD
              THRU 100-LOAD-F
           
           PERFORM 200-PROCESS
              THRU 200-PROCESS-F
           .
           STOP RUN.
       100-LOAD.
           DISPLAY "ENTER A NUMBER "
           ACCEPT WS-N
           .
       100-LOAD-F.

       200-PROCESS.
           PERFORM VARYING WS-I FROM 1
           BY 1 UNTIL WS-I > WS-N
            DISPLAY "NUMBER " WS-I
           END-PERFORM
           .
       200-PROCESS-F.
       END PROGRAM E8.
