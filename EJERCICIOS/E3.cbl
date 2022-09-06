      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-06
      * Dado valor numérico entero que se ingresa por teclado, 
      * se pide informar:
      * •	La quinta parte de dicho valor,
      * •	el resto de la división por 5 y
      * •	la séptima parte de la quinta parte.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E3.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-A PIC 9(03).
       77 WS-B PIC 9(03).
       77 WS-Q PIC 9(04).
       77 WS-DIV PIC 9(04).
       77 WS-BY-FIVE PIC 9(04).
       77 WS-S PIC 9(04)V9(02).
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
           .
       100-LOAD-F.

       200-PROCESS.
           DIVIDE WS-A BY 5 GIVING WS-DIV REMAINDER WS-REMAINDER
           DIVIDE WS-DIV BY 7 GIVING WS-S
           COMPUTE WS-BY-FIVE = WS-REMAINDER * 5
           DISPLAY "DIVISION BY FIVE " WS-A " / 5 : " WS-DIV
           DISPLAY "RAMAINDER BY FIVE " WS-A " * 5 : " WS-BY-FIVE
           DISPLAY "DIVIDING THE FIRST DIVISION BY SEVEN " WS-S 
           .
       200-PROCESS-F.
       END PROGRAM E3.
