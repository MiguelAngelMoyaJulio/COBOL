      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-06
      * Dados dos valores numéricos enteros, calcular e 
      * informar su producto mediante sumas sucesivas
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E9.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-A PIC 9(04). 
       77 WS-B PIC 9(04). 
       77 WS-I PIC 9(04). 
       77 WS-TOTAL PIC 9(04). 
       PROCEDURE DIVISION.
           PERFORM 100-LOAD
              THRU 100-LOAD-F
           
           PERFORM 200-PROCESS
              THRU 200-PROCESS-F
           .
           STOP RUN.
       100-LOAD.
           DISPLAY "ENTER TWO NUMBERS "
           ACCEPT WS-A
           ACCEPT WS-B
           .
       100-LOAD-F.

       200-PROCESS.
           PERFORM VARYING WS-I FROM 1
           BY 1 UNTIL WS-I > WS-A
            COMPUTE WS-TOTAL = WS-TOTAL + WS-B
           END-PERFORM

           DISPLAY "MULTIPLCATION BETWEEN " WS-A " AND " WS-B " IS "
                   WS-TOTAL  
           .
       200-PROCESS-F.
       END PROGRAM E9.
