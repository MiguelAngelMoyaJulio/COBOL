      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-06
      * Dados dos valores numéricos enteros a y b, calcular e informar
      * la suma: a+b, la diferencia: a-b, y el producto: a*b entre dichos 
      * valores.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E1.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-A PIC 9(03).
       77 WS-B PIC 9(03).
       77 WS-SUM PIC 9(04).
       77 WS-SUB PIC 9(04).
       77 WS-MUL PIC 9(06).
       77 WS-DIV PIC 9(04).
       77 WS-REMAINDER PIC 9(04).
       PROCEDURE DIVISION.
            PERFORM 100-LOAD
               THRU 100-LOAD-F
           
            PERFORM 200-PROCESS
               THRU 200-PROCESS-F
           .
            STOP RUN.
       100-LOAD.
           DISPLAY "ENTER THE FIRST NUMBER "
           ACCEPT WS-A
           DISPLAY "ENTER THE SECOND NUMBER "
           ACCEPT WS-B
               .
       100-LOAD-F.

       200-PROCESS.
           COMPUTE WS-SUM = WS-A + WS-B
           COMPUTE WS-SUB = WS-A - WS-B
           COMPUTE WS-MUL = WS-A * WS-B
           DIVIDE WS-A BY WS-B GIVING WS-DIV REMAINDER WS-REMAINDER
           DISPLAY "SUM " WS-A " + " WS-B " : " WS-SUM
           DISPLAY "SUBSTRACTION " WS-A " - " WS-B " : " WS-SUB
           DISPLAY "MULTIPLICATION " WS-A " * " WS-B " : " WS-MUL
           DISPLAY "DIVISION " WS-A " / " WS-B " : " WS-DIV
           DISPLAY "REMAINDER " WS-REMAINDER
           .
       200-PROCESS-F.
       END PROGRAM E1.
