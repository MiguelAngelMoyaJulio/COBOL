      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-07
      * Un buque de carga traslada 100 contenedores a 3 diferentes puer-
      * tos del paísidentificados con los números 1, 2 y 3.
      * Por cada uno de los contenedores trasladados por el buque se 
      * registran los siguientes datos:
      *•	Identificación del contenedor: idCont.
      *•	Peso del contenedor en (en kilos): peso.
      *•	Puerto de arribo (un valor de 1 a 3): idPuerto. 
      * Se pide calcular e informar:
      *•	El peso total que el buque debe trasladar.
      *•	El contenedor de mayor peso.
      *•	La cantidad de contenedores que se trasladarán a cada puerto
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E14.
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
               05 REG-ID-CONT  PIC 9(05).
               05 REG-ID-PORT  PIC 9(01).
               05 REG-WEIGHT   PIC 9(02)V9.
           
       WORKING-STORAGE SECTION.
           77 WS-STATUS                 PIC X(01).
           77 WS-AMOUNT-P1              PIC 9(05).
           77 WS-AMOUNT-P2              PIC 9(05).
           77 WS-AMOUNT-P3              PIC 9(05).
           77 WS-ID-MAX-WEIGHT          PIC 9(05).
           77 WS-PA-MAX-WEIGHT          PIC 9(02)V9.
           77 WS-TOTAL-WEIGHT           PIC 9(04)V9.
           
       PROCEDURE DIVISION.
           OPEN INPUT DATOS
       
           PERFORM 20-LEER
           THRU 20-LEER-F
       
           MOVE REG-WEIGHT TO  WS-PA-MAX-WEIGHT
           MOVE REG-ID-CONT TO WS-ID-MAX-WEIGHT

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
       20-LEER-F. EXIT.

       30-CALCULO.

           COMPUTE WS-TOTAL-WEIGHT = WS-TOTAL-WEIGHT + REG-WEIGHT
   
           IF REG-WEIGHT > WS-PA-MAX-WEIGHT
               MOVE REG-WEIGHT  TO WS-PA-MAX-WEIGHT
               MOVE REG-ID-CONT TO WS-ID-MAX-WEIGHT
           END-IF
   
           EVALUATE TRUE
           WHEN REG-ID-PORT = 1
                ADD 1 TO WS-AMOUNT-P1
           WHEN REG-ID-PORT = 2
                ADD 1 TO WS-AMOUNT-P2
           WHEN REG-ID-PORT = 3
                ADD 1 TO WS-AMOUNT-P3
           END-EVALUATE

           PERFORM 20-LEER
           THRU 20-LEER-F
           .
       30-CALCULO-F. EXIT. 

       40-TOTALES. 
           DISPLAY "TOTAL WEIGHT - TON "        WS-TOTAL-WEIGHT           
           DISPLAY "MAX WEIGHT - ID CONTAINER " WS-ID-MAX-WEIGHT           
           DISPLAY "AMOUNT OF CONTS TO PORT 1 " WS-AMOUNT-P1           
           DISPLAY "AMOUNT OF CONTS TO PORT 2 " WS-AMOUNT-P2           
           DISPLAY "AMOUNT OF CONTS TO PORT 3 " WS-AMOUNT-P3           
           .           
       40-TOTALES-F. EXIT.
       END PROGRAM E14.