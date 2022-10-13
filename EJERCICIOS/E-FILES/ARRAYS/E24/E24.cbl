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
       PROGRAM-ID. E24.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT OPTIONAL DATOS1
       ASSIGN TO "DAT1.txt"
       FILE STATUS IS FS-STATUS1
       ORGANIZATION IS LINE SEQUENTIAL.
       
       SELECT OPTIONAL DATOS2
       ASSIGN TO "DAT2.txt"
       FILE STATUS IS FS-STATUS2
       ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD DATOS1.
           01 REG-DATOS1.
               05 REG-NUM1                 PIC 9(02).
       
       FD DATOS2.
           01 REG-DATOS2.
               05 REG-NUM2                 PIC 9(02).
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************         
       WORKING-STORAGE SECTION.
           01 FS-STATUS1                    PIC X(02) VALUE "00".
               88 FS-STATUS-OK                        VALUE "00".
               88 FS-STATUS-EOF                       VALUE "10".
           01 FS-STATUS2                    PIC X(02) VALUE "00".
               88 FS-STATUS2-OK                       VALUE "00".
               88 FS-STATUS2-EOF                      VALUE "10".
           01 WS-VAR.
               02 WS-Y                     PIC 9(03).
               02 WS-Z                     PIC 9(03).
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
      ******************************************************************
      *                         PROCEDURE DIVISION   
      ******************************************************************                           
       PROCEDURE DIVISION.
           
           PERFORM 10-INICIO
              THRU 10-INICIO-F
           
           PERFORM 20-PROCESO
              THRU 20-PROCESO-F
              UNTIL FS-STATUS-EOF AND FS-STATUS2-EOF           
           
           PERFORM 30-FIN
              THRU 30-FIN-F           
           .
            STOP RUN.
      ******************************************************************
      *                         10-INICIO   
      ******************************************************************      
       10-INICIO.
           
           PERFORM 10-ABRIR-DATOS1
              THRU 10-ABRIR-DATOS1-F

           PERFORM 10-ABRIR-DATOS2
              THRU 10-ABRIR-DATOS2-F
           .
       10-INICIO-F. EXIT.
      ******************************************************************
      *                         10-ABRIR-DATOS1   
      ******************************************************************     
       10-ABRIR-DATOS1.
           OPEN INPUT DATOS1
           IF NOT FS-STATUS-OK
               DISPLAY "ERROR AL ABRIR ARCHIVO " FS-STATUS1
           END-IF
           .
       10-ABRIR-DATOS1-F. EXIT.
      ******************************************************************
      *                         10-ABRIR-DATOS2   
      ******************************************************************      
       10-ABRIR-DATOS2.
           OPEN INPUT DATOS2
           IF NOT FS-STATUS2-OK
               DISPLAY "ERROR AL ABRIR ARCHIVO " FS-STATUS2
           END-IF
           .
       10-ABRIR-DATOS2-F. EXIT.
      ******************************************************************
      *                         20-PROCESO   
      ****************************************************************** 
       20-PROCESO.  

           PERFORM 20-LEER1
              THRU 20-LEER1-F

           PERFORM 20-LEER2
              THRU 20-LEER2-F
           .         
       20-PROCESO-F. EXIT.
      ******************************************************************
      *                         20-LEER1   
      ******************************************************************      
       20-LEER1.
           INITIALIZE REG-DATOS1
           READ DATOS1 INTO REG-DATOS1
           EVALUATE TRUE
               WHEN FS-STATUS-OK
                    ADD 1 TO WS-Y     
                    MOVE REG-NUM1 TO WS-NUM1(WS-Y)
               WHEN FS-STATUS-EOF
                    CONTINUE
           END-EVALUATE
           .
       20-LEER1-F. EXIT.
      ******************************************************************
      *                         20-LEER2   
      ****************************************************************** 
       20-LEER2.
           INITIALIZE REG-DATOS2
           READ DATOS2 INTO REG-DATOS2
           EVALUATE TRUE
               WHEN FS-STATUS2-OK
                    ADD 1 TO WS-Z     
                    MOVE REG-NUM2 TO WS-NUM2(WS-Z)
               WHEN FS-STATUS2-EOF
                    CONTINUE
           END-EVALUATE
           .
       20-LEER2-F. EXIT.
      ******************************************************************
      *                         30-FIN   
      ****************************************************************** 
       30-FIN.
           PERFORM 30-CERRAR-DATOS1
              THRU 30-CERRAR-DATOS1-F

           PERFORM 30-CERRAR-DATOS2
              THRU 30-CERRAR-DATOS2-F

           PERFORM 40-SUMAR-VECTORES
              THRU 40-SUMAR-VECTORES-F    
           
           PERFORM 50-MOSTRAR-VECTOR
              THRU 50-MOSTRAR-VECTOR-F    
           .    
       30-FIN-F. EXIT.
      ******************************************************************
      *                         30-CERRAR-DATOS1   
      ****************************************************************** 
       30-CERRAR-DATOS1.
           CLOSE DATOS1
           IF NOT FS-STATUS-OK
               DISPLAY "ERROR AL CERRAR ARCHIVO " FS-STATUS1
           END-IF
           .
       30-CERRAR-DATOS1-F. EXIT.
      ******************************************************************
      *                         30-CERRAR-DATOS2   
      ****************************************************************** 
       30-CERRAR-DATOS2.
           CLOSE DATOS2
           IF NOT FS-STATUS2-OK
               DISPLAY "ERROR AL CERRAR ARCHIVO " FS-STATUS2
           END-IF
           .
       30-CERRAR-DATOS2-F. EXIT.
      ******************************************************************
      *                         40-SUMAR-VECTORES   
      ****************************************************************** 
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
      ******************************************************************
      *                         50-MOSTRAR-VECTOR   
      ****************************************************************** 
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
       END PROGRAM E24.