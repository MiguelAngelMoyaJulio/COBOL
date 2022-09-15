      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-14
      *Ingresar un conjunto VEC de 40 elementos.
      *Determinar e imprimir el valor máximo y la posición del 
      *mismo dentro del conjunto. Si el máximo no es
      *único, imprimir todas las posiciones en que se encuentra.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E21.
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
               05 REG-NUM              PIC 9(02).
       WORKING-STORAGE SECTION.
           77 WS-STATUS                PIC X(01).
           01 WS-VAR.
               02 WS-I                     PIC 9(03).
               02 WS-MAX                   PIC 9(02).
           01 WS-VECTOR OCCURS 40 TIMES.
               02 WS-NUM               PIC 9(02).                
       PROCEDURE DIVISION.
           OPEN INPUT DATOS
           
           PERFORM 20-LEER
           THRU 20-LEER-F
           MOVE "Y" TO WS-STATUS
           
           PERFORM 30-CALCULO
           THRU 30-CALCULO-F
           UNTIL WS-STATUS = "F"
                      
           CLOSE DATOS

           PERFORM 50-CALCULAR-MAX
              THRU 50-CALCULAR-MAX-F
              
           PERFORM 60-POSICION-MAXIMO
              THRU 60-POSICION-MAXIMO-F
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
       50-CALCULAR-MAX.
           MOVE ZEROS TO WS-MAX
           PERFORM VARYING WS-I FROM 1 
           BY 1 UNTIL WS-I > 40

               IF WS-NUM(WS-I) > WS-MAX
                   MOVE WS-NUM(WS-I) TO WS-MAX
               END-IF 
           
           END-PERFORM

           DISPLAY "MAX'S NUMBER : " WS-MAX
           .
       50-CALCULAR-MAX-F. EXIT.
       60-POSICION-MAXIMO.
           DISPLAY "MAX'S POSITION/S "

           PERFORM VARYING WS-I FROM 1 
           BY 1 UNTIL WS-I > 40
               IF WS-MAX = WS-NUM(WS-I)
                   DISPLAY WS-I
               END-IF    
           END-PERFORM     
           .
       60-POSICION-MAXIMO-F. EXIT.
       END PROGRAM E21.