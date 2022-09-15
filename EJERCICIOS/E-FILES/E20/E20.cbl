      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-07
      *Crear un de VEC de 25 componentes. Si la
      *suma de las componentes resulta mayor que 900 imprimir 
      *las de índice par, sino las de índice impar.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E20.
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
               05 REG-NUM              PIC 9(03).
       WORKING-STORAGE SECTION.
           77 WS-STATUS                PIC X(01).
           01 WS-VAR.
               02 WS-I                     PIC 9(03).
               02 WS-SUM-TOTAL             PIC 9(06).
               02 WS-REMAINING             PIC 9(06).
               02 WS-RESULT                PIC 9(06).
           01 WS-VECTOR OCCURS 25 TIMES.
               02 WS-NUM               PIC 9(03).                
              
       PROCEDURE DIVISION.
           OPEN INPUT DATOS
           
           PERFORM 20-LEER
           THRU 20-LEER-F
           MOVE "Y" TO WS-STATUS
           
           PERFORM 30-CALCULO
           THRU 30-CALCULO-F
           UNTIL WS-STATUS = "F"
                      
           CLOSE DATOS

           PERFORM 50-CALCULAR-SUMA
              THRU 50-CALCULAR-SUMA-F
           DISPLAY "SUM OF ALL NUMBERS IN THE VECTOR " WS-SUM-TOTAL
           PERFORM 60-MOSTRAR-PAR-IMPAR
              THRU 60-MOSTRAR-PAR-IMPAR-F
           .
            STOP RUN.

       20-LEER.
           READ DATOS NEXT RECORD
           AT END
           MOVE "F" TO  WS-STATUS
           .
       20-LEER-F. EXIT.

       30-CALCULO.
           
           PERFORM 40-LLENAR-VECTOR
              THRU 40-LLENAR-VECTOR-F
           
           PERFORM 20-LEER
           THRU 20-LEER-F
           .
       30-CALCULO-F. EXIT.
       40-LLENAR-VECTOR.
           ADD 1 TO WS-I     
           MOVE REG-NUM TO WS-NUM(WS-I)
           .
       40-LLENAR-VECTOR-F. EXIT. 
       
       50-CALCULAR-SUMA.
           PERFORM VARYING WS-I FROM 1 
           BY 1 UNTIL WS-I > 25
               COMPUTE WS-SUM-TOTAL = WS-SUM-TOTAL + WS-NUM(WS-I) 
           END-PERFORM
           .
       50-CALCULAR-SUMA-F. EXIT.
       60-MOSTRAR-PAR-IMPAR.
           IF WS-SUM-TOTAL > 900
               PERFORM VARYING WS-I FROM 1 
               BY 1 UNTIL WS-I > 25
                   DIVIDE WS-NUM(WS-I) BY 2 GIVING WS-RESULT REMAINDER
                          WS-REMAINING  
                   IF WS-REMAINING = 0
                       DISPLAY WS-NUM(WS-I)
                   END-IF    
               END-PERFORM     
           ELSE
               PERFORM VARYING WS-I FROM 1 
               BY 1 UNTIL WS-I > 25
                   DIVIDE WS-NUM(WS-I) BY 2 GIVING WS-RESULT REMAINDER
                          WS-REMAINING  
                   IF WS-REMAINING NOT EQUAL ZEROS
                       DISPLAY WS-NUM(WS-I)
                   END-IF    
               END-PERFORM
           END-IF
           .
       60-MOSTRAR-PAR-IMPAR-F. EXIT.
       END PROGRAM E20.