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
       AUTHOR. MIGUEL MOYA.
       DATE-WRITTEN. OCTOBER 2022.
       DATE-COMPILED. OCTOBER 2022.
      ******************************************************************
      *                     ENVIRONMENT DIVISION
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      ******************************************************************
      *                            FILES   
      ******************************************************************
      *****************************  INPUT  ****************************
       SELECT DATOS1 ASSIGN TO "DAT1.txt"
                    FILE STATUS IS FS-STATUS-FILE
                    ORGANIZATION IS LINE SEQUENTIAL. 
       
       SELECT DATOS2 ASSIGN TO "DAT2.txt"
                     FILE STATUS IS FS-STATUS-FILE2
                     ORGANIZATION IS LINE SEQUENTIAL. 
       
      ****************************  OUTPUT  ***************************

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
      ************************  CONSTANTS  *****************************

      ************************** TABLES ********************************
       01 WS-V1 OCCURS 40 TIMES.
           02 WS-NUM1                  PIC 9(02).                
       
       01 WS-V2 OCCURS 40 TIMES.
           02 WS-NUM2                  PIC 9(02).                
       
       01 WS-V3 OCCURS 40 TIMES.
           02 WS-NUM3                  PIC 9(03).   

      **************************  SWITCHES  ****************************
       01 WS-SWITCHES.       
           05 FS-STATUS-FILE                PIC X(02) VALUE "00".
              88 FS-STATUS-FILE-OK                    VALUE "00".
              88 FS-STATUS-FILE-EOF                   VALUE "10".
         
           05 FS-STATUS-FILE2               PIC X(02) VALUE "00".
              88 FS-STATUS-FILE2-OK                   VALUE "00".
              88 FS-STATUS-FILE2-EOF                  VALUE "10".
           
           05 FS-LOAD-VECTOR1               PIC X(01) VALUE "N".
              88 FS-LOAD-VECTOR1-OK                   VALUE "Y".
              88 FS-LOAD-VECTOR1-NOK                  VALUE "N".
           
           05 FS-LOAD-VECTOR2               PIC X(01) VALUE "N".
              88 FS-LOAD-VECTOR2-OK                   VALUE "Y".
              88 FS-LOAD-VECTOR2-NOK                  VALUE "N".
           
      ************************** VARIABLES *****************************
       01 WS-VARIABLES.
           02 WS-MAX                   PIC 9(02).
              02 WS-I                     PIC 9(03).
              02 WS-J                     PIC 9(03).
              02 WS-X                     PIC 9(03).
              02 WS-SUMA                  PIC 9(03).
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
      *                       LINKAGE SECTION   
      ****************************************************************** 
       LINKAGE SECTION.        
      ******************************************************************
      *                      PROCEDURE DIVISION   
      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM 100000-START                      
              THRU 100000-START-F                    
                                                  
           PERFORM 200000-PROCESS                     
              THRU 200000-PROCESS-F                   
                                                  
           PERFORM 300000-END                         
              THRU 300000-END-F
           .   
      ******************************************************************
      *                         100000-START         
      ******************************************************************
       100000-START.
           PERFORM 110000-OPEN-DATOS1
              THRU 110000-OPEN-DATOS1-F                                 
           
           PERFORM 120000-OPEN-DATOS2
              THRU 120000-OPEN-DATOS2-F    
           
           PERFORM 210000-READ-DATOS1
              THRU 210000-READ-DATOS1-F    
           
           PERFORM 220000-READ-DATOS2
              THRU 220000-READ-DATOS2-F    

           .                                      
       100000-START-F. EXIT.
      ******************************************************************
      *                         110000-OPEN-DATOS1   
      ******************************************************************
       110000-OPEN-DATOS1.                        
           OPEN INPUT DATOS1                   
           IF NOT FS-STATUS-FILE-OK
               DISPLAY "ERROR AL ABRIR ARCHIVO DATOS1 " FS-STATUS-FILE
           END-IF
           .
       110000-OPEN-DATOS1-F. EXIT.        
      ******************************************************************
      *                         120000-OPEN-DATOS2   
      ******************************************************************
       120000-OPEN-DATOS2.                        
           OPEN INPUT DATOS2                   
           IF NOT FS-STATUS-FILE2-OK
               DISPLAY "ERROR AL ABRIR ARCHIVO DATOS2 " FS-STATUS-FILE2
           END-IF
           .
       120000-OPEN-DATOS2-F. EXIT.        
      ******************************************************************
      *                         200000-PROCESS   
      ****************************************************************** 
       200000-PROCESS.
           INITIALIZE WS-I 
           PERFORM 230000-LOAD-VECTOR1
              THRU 230000-LOAD-VECTOR1-F   
              UNTIL FS-LOAD-VECTOR1-OK 
           
           INITIALIZE WS-I
           PERFORM 240000-LOAD-VECTOR2
              THRU 240000-LOAD-VECTOR2-F   
              UNTIL FS-LOAD-VECTOR2-OK 
           .         
       200000-PROCESS-F. EXIT.    
      ******************************************************************
      *                         210000-READ-DATOS1   
      ******************************************************************      
       210000-READ-DATOS1.
           INITIALIZE REG-DATOS1
           READ DATOS1 INTO REG-DATOS1
           EVALUATE TRUE
               WHEN FS-STATUS-FILE-OK
                    CONTINUE   
               WHEN FS-STATUS-FILE-EOF
                    SET FS-LOAD-VECTOR1-OK TO TRUE
           END-EVALUATE
           .
       210000-READ-DATOS1-F. EXIT.  
      ******************************************************************
      *                         220000-READ-DATOS2   
      ******************************************************************      
       220000-READ-DATOS2.
           INITIALIZE REG-DATOS2
           READ DATOS2 INTO REG-DATOS2
           EVALUATE TRUE
               WHEN FS-STATUS-FILE2-OK
                    CONTINUE   
               WHEN FS-STATUS-FILE2-EOF
                    SET FS-LOAD-VECTOR2-OK TO TRUE
           END-EVALUATE
           .
       220000-READ-DATOS2-F. EXIT.  
      ******************************************************************
      *                         230000-LOAD-VECTOR1   
      ******************************************************************      
       230000-LOAD-VECTOR1.
           ADD 1 TO WS-I     
           MOVE REG-NUM1 TO WS-NUM1(WS-I)

           PERFORM 210000-READ-DATOS1
              THRU 210000-READ-DATOS1-F
           .
       230000-LOAD-VECTOR1-F. EXIT.  
      ******************************************************************
      *                         240000-LOAD-VECTOR2   
      ******************************************************************      
       240000-LOAD-VECTOR2.
           ADD 1 TO WS-I     
           MOVE REG-NUM2 TO WS-NUM2(WS-I)

           PERFORM 220000-READ-DATOS2
              THRU 220000-READ-DATOS2-F
           .
       240000-LOAD-VECTOR2-F. EXIT.   
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           PERFORM 310000-CLOSE-DATOS1
              THRU 310000-CLOSE-DATOS1-F
           
           PERFORM 320000-CLOSE-DATOS2
              THRU 320000-CLOSE-DATOS2-F
           
           PERFORM 330000-SUMAR-VECTORES
              THRU 330000-SUMAR-VECTORES-F
           
           PERFORM 340000-MOSTRAR-VECTORES
              THRU 340000-MOSTRAR-VECTORES-F
           
           STOP RUN 
           .    
       300000-END-F. EXIT.
      ******************************************************************
      *                         310000-CLOSE-DATOS1   
      ****************************************************************** 
       310000-CLOSE-DATOS1.
           CLOSE DATOS1
           IF NOT FS-STATUS-FILE-OK
               DISPLAY "ERROR AL CERRAR ARCHIVO DATOS1 " FS-STATUS-FILE
           END-IF
           .
       310000-CLOSE-DATOS1-F. EXIT. 
      ******************************************************************
      *                         320000-CLOSE-DATOS2   
      ****************************************************************** 
       320000-CLOSE-DATOS2.
           CLOSE DATOS2
           IF NOT FS-STATUS-FILE2-OK
               DISPLAY "ERROR AL CERRAR ARCHIVO DATOS2 " FS-STATUS-FILE2
           END-IF
           .
       320000-CLOSE-DATOS2-F. EXIT. 
      ******************************************************************
      *                         330000-SUMAR-VECTORES   
      ****************************************************************** 
       330000-SUMAR-VECTORES.
           MOVE 040 TO WS-X
           INITIALIZE WS-SUMA
           MOVE 1 TO WS-J

           PERFORM VARYING WS-J FROM 1
           BY 1 UNTIL WS-J > 40
               COMPUTE WS-SUMA = WS-NUM1(WS-J) + WS-NUM2(WS-X)
               MOVE WS-SUMA TO WS-NUM3(WS-J)
               COMPUTE WS-X = WS-X - 1
               MOVE ZEROS TO WS-SUMA
           END-PERFORM
           .
       330000-SUMAR-VECTORES-F. EXIT. 
      ******************************************************************
      *                         340000-MOSTRAR-VECTORES   
      ****************************************************************** 
       340000-MOSTRAR-VECTORES.
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
       340000-MOSTRAR-VECTORES-F. EXIT.   
       END PROGRAM E23.