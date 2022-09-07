      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-06
      * Se ingresa un valor numérico entero, se pide calcular 
      * e informar su factorial.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E10.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-A PIC 9(04). 
       77 WS-I PIC 9(04). 
       77 WS-FACTORIAL PIC 9(04). 
       PROCEDURE DIVISION.
           PERFORM 100-LOAD
              THRU 100-LOAD-F
           
           PERFORM 200-PROCESS
              THRU 200-PROCESS-F
           .
           STOP RUN.
       100-LOAD.
           DISPLAY "ENTER A NUMBER "
           ACCEPT WS-A
           .
       100-LOAD-F.

       200-PROCESS.
           ADD 1 TO WS-FACTORIAL
           PERFORM VARYING WS-I FROM 1
           BY 1 UNTIL WS-I > WS-A
            COMPUTE WS-FACTORIAL = WS-FACTORIAL * WS-I
           END-PERFORM

           DISPLAY "FACTORIAL OF " WS-A " IS : " WS-FACTORIAL
           .
       200-PROCESS-F.
       END PROGRAM E10.
