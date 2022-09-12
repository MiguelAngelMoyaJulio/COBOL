      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-07
      * De un censo realizado en una población se conocen los siguientes
      * datos:
      *1.	Día de nacimiento (2 dígitos)
      *2.	Mes (2 dígitos)
      *3.	Año (4 dígitos)
      *4.	Sexo ('M'=masculino, 'F'=femenino)
      *Con estos datos de cada habitante se forma un lote, 
      *finalizado su ingreso con un día igual a 0.
      * Se pide desarrollar el programa que determine e imprima:
      *1.	Cuántos nacimientos hubo en el mes de octubre de 
      *todos los años.
      *2.	Cuántos nacimientos hubo antes del 9 de julio de 1990.
      *3.	Cuántos nacimientos de mujeres hubo en la primavera del 1982
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E18.
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
               05 REG-DIA       PIC 9(02).
               05 REG-MES       PIC 9(02).
               05 REG-ANIO      PIC 9(04).
               05 REG-SEXO      PIC X(01).
       WORKING-STORAGE SECTION.
           01 WS-FECHA.
               05 WS-D                 PIC 9(02).
               05 WS-M                 PIC 9(02).
               05 WS-A                 PIC 9(04).
           77 WS-STATUS                PIC X(01).
           77 WS-OLDER-PERSON          PIC X(01).
           77 WS-KK          PIC 9(05).
           77 WS-AMOUNT-OCTOBER        PIC 9(05).
           77 WS-AMOUNT-SPECIAL        PIC 9(05).
           77 WS-AMOUNT-SPRING         PIC 9(05).
           77 WS-ID-MAX-WEIGHT         PIC 9(05).
           77 WS-PA-MAX-WEIGHT         PIC 9(02)V9.
           77 WS-TOTAL-WEIGHT          PIC 9(04)V9.
       PROCEDURE DIVISION.
           OPEN INPUT DATOS
       
           PERFORM 20-LEER
           THRU 20-LEER-F
           MOVE "Y" TO WS-STATUS
           
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
           ADD 1 TO WS-KK
           DISPLAY WS-KK
           IF REG-MES = 10
               ADD 1 TO WS-AMOUNT-OCTOBER
           END-IF
           
           IF REG-ANIO < 1990
               ADD 1 TO WS-AMOUNT-SPECIAL
           ELSE        
               IF REG-ANIO = 1990 AND (REG-MES >= 1 
                   AND REG-MES <= 6) 
                   ADD 1 TO WS-AMOUNT-SPECIAL
               ELSE
                   IF REG-ANIO = 1990 AND REG-MES = 7 AND
                      REG-DIA >= 1 AND REG-DIA <= 9                    
                       ADD 1 TO WS-AMOUNT-SPECIAL
                   END-IF
               END-IF
           END-IF
           
           IF REG-ANIO = 1982 AND REG-SEXO = "F"
               IF REG-MES = 9 AND REG-DIA >= 21 AND REG-DIA <= 30
                   ADD 1 TO WS-AMOUNT-SPRING
               ELSE
                   IF REG-MES > 9 AND REG-MES < 12
                       ADD 1 TO WS-AMOUNT-SPRING
                   ELSE
                       IF REG-MES = 12 AND REG-DIA >= 1 
                           AND REG-DIA <= 21
                               ADD 1 TO WS-AMOUNT-SPRING
                       END-IF        
                   END-IF
               END-IF
           END-IF
           
           PERFORM 20-LEER
           THRU 20-LEER-F
           .
       30-CALCULO-F. EXIT. 

       40-TOTALES. 
           DISPLAY "AMOUNT OF BIRTHS ON OCTOBER " WS-AMOUNT-OCTOBER                  
           DISPLAY "AMOUNT SPECIAL " WS-AMOUNT-SPECIAL                  
           DISPLAY "AMOUNT ON SPRING " WS-AMOUNT-SPRING                  
           .           
       40-TOTALES-F. EXIT.
       END PROGRAM E18.