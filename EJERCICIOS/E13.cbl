      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-07
      * Dado un valor numérico entero m, determinar e imprimir un 
      * listado con los m primeros 
      * múltiplos de 3 que no sean múltiples de 5
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E13.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-VAR.
          02 WS-NUM        PIC 9(03). 
          02 WS-I          PIC 9(03). 
          02 WS-X          PIC 9(03). 
          02 WS-MOD-3      PIC 9(03). 
          02 WS-RES-3      PIC 9(03). 
          02 WS-RES-5      PIC 9(03). 
          02 WS-MOD-5      PIC 9(03). 
       PROCEDURE DIVISION.
           PERFORM 10-LOAD
              THRU 10-LOAD-F
           
           PERFORM 20-PROCESS
              THRU 20-PROCESS-F
           .
           STOP RUN.
       10-LOAD.
           DISPLAY "ENTER A NUMBER "
           ACCEPT WS-NUM
           .
       10-LOAD-F.

       20-PROCESS.
           PERFORM 30-CALCULO
           THRU 30-CALCULO-F 
           UNTIL WS-I = WS-NUM
           .
       20-PROCESS-F.
       30-CALCULO.
           ADD 1 TO WS-X
           DIVIDE WS-X BY 3 GIVING WS-RES-3 REMAINDER WS-MOD-3
           DIVIDE WS-X BY 5 GIVING WS-RES-5 REMAINDER WS-MOD-5
               
           IF WS-MOD-3 = 0 AND WS-MOD-5 <> 0
               DISPLAY "NUMBER " WS-X 
               ADD 1 TO WS-I
           END-IF
           .
       30-CALCULO-F.

       END PROGRAM E13.
