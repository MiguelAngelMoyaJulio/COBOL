      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-07
      * Dados 100 números enteros, informar el promedio de los mayores 
      * que 100 y la suma de los que están entre que 30 y 60.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E11.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT OPTIONAL DATOS
       ASSIGN TO "DAT.txt"
       ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD DATOS.
           01 REG-DATOS.
               05 REG-NUM PIC 9(03).
           
       WORKING-STORAGE SECTION.
           01 WS-ENTRADA PIC X(01).
           01 SI-NO PIC X(01).
           77 WS-ID PIC 9(03) VALUE ZERO.
           77 WS-STATUS PIC X(01).
           77 WS-PROM-100  PIC 9(05).
           77 WS-SUM-100   PIC 9(05).
           77 WS-TOT-100   PIC 9(05).
           77 WS-SUM-INT    PIC 9(05).
           
       PROCEDURE DIVISION.
       OPEN INPUT DATOS
       
       PERFORM 20-LEER
       THRU 20-LEER-F

       PERFORM 30-CALCULO
       THRU 30-CALCULO-F
       UNTIL WS-STATUS = "F"
       
       CLOSE DATOS

       PERFORM 40-TOTALES
          THRU 40-TOTALES-F
       . 
       STOP RUN.

       20-LEER. 
       READ DATOS NEXT RECORD
       AT END
       MOVE "F" TO  WS-STATUS
       .            
       20-LEER-F.

       30-CALCULO. 
       IF REG-NUM > 100
           ADD 1 TO WS-TOT-100
           COMPUTE WS-SUM-100 = WS-SUM-100 + REG-NUM 
       END-IF
       
       IF REG-NUM > 30 AND REG-NUM < 60
          COMPUTE WS-SUM-INT = WS-SUM-INT + REG-NUM 
       END-IF
       PERFORM 20-LEER
       THRU 20-LEER-F
       .
       30-CALCULO-F.  

       40-TOTALES. 
           COMPUTE WS-PROM-100 = WS-SUM-100 / WS-TOT-100
           DISPLAY " AVERAGE OF NUMBERS GREATER THAN 100 " WS-PROM-100           
           DISPLAY " SUMMATION OF NUMBERS BETWEEN 30-60 " WS-SUM-INT
           .           
       40-TOTALES-F.            
       END PROGRAM E11.
