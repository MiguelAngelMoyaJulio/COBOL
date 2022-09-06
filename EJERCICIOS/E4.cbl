      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-06
      * Dados dos valores numéricos, informar cual es el mayor y 
      * cual es el menor o, si ambos valores 
      * son iguales emitir un mensaje.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E3.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-A PIC 9(03).
       77 WS-B PIC 9(03).
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
           IF WS-A > WS-B THEN
               DISPLAY WS-A " IS GREATER"
           ELSE
               IF WS-B > WS-A THEN
                   DISPLAY WS-B " IS GREATER"
               ELSE
                   DISPLAY " ITS THE SAME NUMBER"
               END-IF 
           END-IF
           .
       200-PROCESS-F.
       END PROGRAM E3.
