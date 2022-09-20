      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-15
      * CORTE DE CONTROL POR SUCURSAL Y VENDEDOR
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E27.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT OPTIONAL DATOS1
       ASSIGN TO "SUC-VEN.txt"
       FILE STATUS IS FS-STATUS1
       ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD DATOS1.
           01 REG-DATOS1.
               05 REG-SUCURSAL              PIC 9(02).
               05 REG-VENDEDOR              PIC 9(02).
               05 REG-MONTO                 PIC 9(07)V99.
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************         
       WORKING-STORAGE SECTION.
           01 FS-STATUS1                    PIC X(02) VALUE "00".
               88 FS-STATUS-OK                        VALUE "00".
               88 FS-STATUS-EOF                       VALUE "10".
           01 WS-VAR.
               02 WS-VENDEDOR-ANT          PIC 9(02).
               02 WS-TOT-VENDEDOR          PIC 9(07)V99.
               02 WS-SUCURSAL-ANT          PIC 9(02).
               02 WS-TOT-SUC               PIC 9(07)V99.
               02 WS-TOT-EMPRESA           PIC 9(07)V99.
               02 WS-TOTE-MA               PIC ZZ,ZZZ,ZZZ.ZZ.
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
              UNTIL FS-STATUS-EOF           
           
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
           
           PERFORM 20-LEER1
              THRU 20-LEER1-F
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
      *                         20-PROCESO   
      ****************************************************************** 
       20-PROCESO.  
           MOVE REG-SUCURSAL TO WS-SUCURSAL-ANT
           INITIALIZE WS-TOT-SUC    
           
           PERFORM 20-CORTE-SUCURSAL
              THRU 20-CORTE-SUCURSAL-F
              UNTIL WS-SUCURSAL-ANT <> REG-SUCURSAL

           MOVE WS-TOT-SUC TO WS-TOTE-MA
           ADD WS-TOT-SUC TO WS-TOT-EMPRESA   
           DISPLAY "TOTAL " WS-SUCURSAL-ANT " : $" WS-TOTE-MA
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
                    CONTINUE
               WHEN FS-STATUS-EOF
                    CONTINUE
           END-EVALUATE
           .
       20-LEER1-F. EXIT.
      ******************************************************************
      *                         20-CORTE-SUCURSAL   
      ******************************************************************      
       20-CORTE-SUCURSAL.
           
           MOVE REG-VENDEDOR TO WS-VENDEDOR-ANT
           INITIALIZE WS-TOT-VENDEDOR

           PERFORM 20-CORTE-VENDEDOR
              THRU 20-CORTE-VENDEDOR-F
              UNTIL WS-VENDEDOR-ANT <> REG-VENDEDOR
           
           MOVE WS-TOT-VENDEDOR TO WS-TOTE-MA   
           DISPLAY "TOTAL VENDEDOR " WS-VENDEDOR-ANT " : $" WS-TOTE-MA   
           COMPUTE WS-TOT-SUC = WS-TOT-SUC + WS-TOT-VENDEDOR
           .
       20-CORTE-SUCURSAL-F. EXIT.
      ******************************************************************
      *                         20-CORTE-VENDEDOR   
      ******************************************************************      
       20-CORTE-VENDEDOR.
           ADD REG-MONTO TO WS-TOT-VENDEDOR
           PERFORM 20-LEER1
              THRU 20-LEER1-F
           .
       20-CORTE-VENDEDOR-F. EXIT.
      ******************************************************************
      *                         30-FIN   
      ****************************************************************** 
       30-FIN.
           PERFORM 30-CERRAR-DATOS1
              THRU 30-CERRAR-DATOS1-F

           PERFORM 30-VENTA-TOTAL
              THRU 30-VENTA-TOTAL-F
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
      *                         30-VENTA-TOTAL   
      ****************************************************************** 
       30-VENTA-TOTAL.
           MOVE WS-TOT-EMPRESA TO WS-TOTE-MA   
           DISPLAY "TOTAL EMPRESA " WS-TOTE-MA 
           .
       30-VENTA-TOTAL-F. EXIT.
       END PROGRAM E27.