      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-15
      *Ingresar un valor entero N (< 40). A continuación ingresar 
      *un conjunto A y luego otro conjunto B
      *ambos de N elementos. Generar un arreglo C donde 
      *cada elemento se forme de la siguiente forma:
      *C[1] ← A[1]+B[N] C[2] ← A[2]+B[N-1] ..........................
      *C[N] ← A[N]+B[1]
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E23.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT OPTIONAL DATOS1
       ASSIGN TO "DAT1.txt"
       ORGANIZATION IS LINE SEQUENTIAL.
       
       SELECT OPTIONAL DATOS2
       ASSIGN TO "DAT2.txt"
       ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD DATOS1.
           01 REG-DATOS1.
               05 REG-NUM1                 PIC 9(02).
       
       FD DATOS2.
           01 REG-DATOS2.
               05 REG-NUM2                 PIC 9(02).
       WORKING-STORAGE SECTION.
           77 WS-STATUS                    PIC X(01).
           77 WS-STATUS2                   PIC X(01).
           01 WS-VAR.
               02 WS-I                     PIC 9(03).
               02 WS-J                     PIC 9(03).
               02 WS-X                     PIC 9(03).
               02 WS-SUMA                  PIC 9(03).
           01 WS-V1 OCCURS 40 TIMES.
               02 WS-NUM1                  PIC 9(02).                
           
           01 WS-V2 OCCURS 40 TIMES.
               02 WS-NUM2                  PIC 9(02).                
           
           01 WS-V3 OCCURS 40 TIMES.
               02 WS-NUM3                  PIC 9(03).   
           01 WS-TITULO.
               02 WS-T1                    PIC  X(04) VALUE "V1 [".                 
               02 WS-I1                    PIC  9(02).                  
               02 WS-F1                    PIC  X(05) VALUE "] -> ".                 
               02 WS-VALOR1                PIC  X(02).                 
               02 WS-T2                    PIC  X(07) VALUE " + V2 [".                 
               02 WS-I2                    PIC  9(02).                  
               02 WS-F2                    PIC  X(05) VALUE "] -> ".                 
               02 WS-VALOR2                PIC  9(02).                 
               02 WS-OP                    PIC  X(03) VALUE " = ".                 
               02 WS-T-SUMA                PIC  9(03).                  
       PROCEDURE DIVISION.
           OPEN INPUT DATOS1
           
           PERFORM 20-LEER1
              THRU 20-LEER1-F
           
           MOVE "Y" TO WS-STATUS
           
           PERFORM 30-CARGAR-DATOS1
           THRU 30-CARGAR-DATOS1
           UNTIL WS-STATUS = "F" 
                      
           CLOSE DATOS1
           
           MOVE ZEROS TO WS-I
           OPEN INPUT DATOS2

           PERFORM 25-LEER2
              THRU 25-LEER2-F
           
           MOVE "Y" TO WS-STATUS2
           
           PERFORM 35-CARGAR-DATOS2
              THRU 35-CARGAR-DATOS2
              UNTIL WS-STATUS2 = "F" 
                      
           CLOSE DATOS2
           
           PERFORM 40-SUMAR-VECTORES
               THRU 40-SUMAR-VECTORES-F

           PERFORM 50-MOSTRAR-VECTOR
              THRU 50-MOSTRAR-VECTOR-F
           .
            STOP RUN.
       20-LEER1.
           READ DATOS1 NEXT RECORD
           AT END
           MOVE "F" TO  WS-STATUS
           .
       20-LEER1-F. EXIT.
       
       25-LEER2.
           READ DATOS2 NEXT RECORD
           AT END
           MOVE "F" TO  WS-STATUS2
           .
       25-LEER2-F. EXIT.

       30-CARGAR-DATOS1.
           ADD 1 TO WS-I     
           MOVE REG-NUM1 TO WS-NUM1(WS-I)
           
           PERFORM 20-LEER1
           THRU 20-LEER1-F
           .
       30-CARGAR-DATOS1-F. EXIT.
       35-CARGAR-DATOS2.
           ADD 1 TO WS-I     
           MOVE REG-NUM2 TO WS-NUM2(WS-I)
           
           PERFORM 25-LEER2
           THRU 25-LEER2-F
           .
       35-CARGAR-DATOS2-F. EXIT.
       40-SUMAR-VECTORES.
           MOVE 040 TO WS-X
           INITIALIZE WS-SUMA
           MOVE ZEROS TO WS-J

           PERFORM VARYING WS-J FROM 1
           BY 1 UNTIL WS-J > 40
               COMPUTE WS-SUMA = WS-NUM1(WS-J) + WS-NUM2(WS-X)
               MOVE WS-SUMA TO WS-NUM3(WS-J)
               COMPUTE WS-X = WS-X - 1
               MOVE ZEROS TO WS-SUMA
           END-PERFORM 

           .
       40-SUMAR-VECTORES-F. EXIT.
       50-MOSTRAR-VECTOR.
           MOVE 040 TO WS-X
           INITIALIZE WS-SUMA
           MOVE ZEROS TO WS-J

           PERFORM VARYING WS-J FROM 1
           BY 1 UNTIL WS-J > 40
               MOVE WS-J TO WS-I1
               MOVE WS-X TO WS-I2
               MOVE WS-NUM1(WS-J) TO WS-VALOR1
               MOVE WS-NUM2(WS-X) TO WS-VALOR2
               MOVE WS-NUM3(WS-J) TO WS-T-SUMA
               DISPLAY WS-TITULO
               COMPUTE WS-X = WS-X - 1
           END-PERFORM
           . 
       50-MOSTRAR-VECTOR-F. EXIT.
       END PROGRAM E23.