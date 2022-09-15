      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-15
      *Ingresar dos valores enteros 10=M y 15=N. 
      *A continuación ingresar un conjunto A de M
      *elementos y luego otro B de N elementos. Generar e imprimir:
      *a) Un conjunto C resultante de la anexión de A y B.
      *b) Un conjunto D resultante de la anexión de los 
      *elementos distintos de cero de A y B.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E22.
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
               02 WS-MAX                   PIC 9(02).
           01 WS-V1 OCCURS 10 TIMES.
               02 WS-NUM1                  PIC 9(02).                
           
           01 WS-V2 OCCURS 15 TIMES.
               02 WS-NUM2                   PIC 9(02).                
           
           01 WS-V3 OCCURS 25 TIMES.
               02 WS-NUM3                   PIC 9(02).                
           
           01 WS-V4 OCCURS 25 TIMES.
               02 WS-NUM4                   PIC 9(02).                
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
           
           PERFORM 40-UNIR-VECTORES
              THRU 40-UNIR-VECTORES-F
           
           PERFORM 50-UNIR-VECTORES-NO-CEROS
              THRU 50-UNIR-VECTORES-NO-CEROS-F   

           MOVE ZEROS TO WS-J
           PERFORM 60-MOSTRAR-VECTORES
              THRU 60-MOSTRAR-VECTORES-F 
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
       40-UNIR-VECTORES.
           MOVE ZEROS TO WS-J
           PERFORM VARYING WS-J FROM 1
           BY 1 UNTIL WS-J > 10
               MOVE WS-NUM1(WS-J) TO WS-NUM3(WS-J)
           END-PERFORM 

           ADD 1 TO WS-X
           PERFORM VARYING WS-J FROM 11
           BY 1 UNTIL WS-J > 25
               MOVE WS-NUM2(WS-X) TO WS-NUM3(WS-J)
               ADD 1 TO WS-X
           END-PERFORM 
           .
       40-UNIR-VECTORES-F. EXIT.
       50-UNIR-VECTORES-NO-CEROS.
           PERFORM VARYING WS-J FROM 1
               BY 1 UNTIL WS-J > 25
               IF WS-NUM3(WS-J) <> 0
                   MOVE WS-NUM3(WS-J) TO WS-NUM4(WS-J)
               END-IF    
           END-PERFORM
           .            
       50-UNIR-VECTORES-NO-CEROS-F. EXIT.
       60-MOSTRAR-VECTORES.
           DISPLAY "VECTOR A U VECTOR B"
           PERFORM VARYING WS-J FROM 1
           BY 1 UNTIL WS-J > 25
                DISPLAY WS-NUM3(WS-J)
           END-PERFORM

           DISPLAY " "
           DISPLAY "VECTOR A U VECTOR B, EXCEPT ZEROS"
           PERFORM VARYING WS-J FROM 1
           BY 1 UNTIL WS-J > 25
                DISPLAY WS-NUM4(WS-J)
           END-PERFORM
           . 
       60-MOSTRAR-VECTORES-F.
       END PROGRAM E22.