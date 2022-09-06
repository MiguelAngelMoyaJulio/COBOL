      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-06
      * Dado un triángulo representado por sus lados lado1, lado2 y 
      * lado3, determinar e indicar según corresponda: 
      * “equilátero”, “isósceles” o “escálenos”.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E6.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-L1     PIC 9(02). 
       77 WS-L2     PIC 9(02). 
       77 WS-L3     PIC 9(02). 
       PROCEDURE DIVISION.
           PERFORM 100-LOAD
              THRU 100-LOAD-F
           
           PERFORM 200-PROCESS
              THRU 200-PROCESS-F
           .
           STOP RUN.
       100-LOAD.
           DISPLAY "ENTER THE FIRST SIDE "
           ACCEPT WS-L1
           DISPLAY "ENTER THE SECOND SIDE "
           ACCEPT WS-L2
           DISPLAY "ENTER THE THIRD SIDE "
           ACCEPT WS-L3
           .
       100-LOAD-F.

       200-PROCESS.
           IF WS-L1 = WS-L2 AND WS-L3 = WS-L2 
               DISPLAY "EQUILATERAL"
           ELSE
               IF WS-L1 = WS-L2 OR WS-L3 = WS-L2 OR WS-L1 = WS-L3
                   DISPLAY "ISOSCELES"
               ELSE
                   DISPLAY "SCALENE"
               END-IF
           END-IF  
           .
       200-PROCESS-F.
       END PROGRAM E6.
