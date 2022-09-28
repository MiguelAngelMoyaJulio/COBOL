      ******************************************************************
      *Una empresa tiene la siguiente información por 
      *cada uno de sus empleados: código empleado, código
      *sección, código de departamento, código de la sucursal 
      * salario del empleado. Hacer un algoritmo que
      *muestre el código y el salario de cada empleado
      *dando totales por sección, departamento y sucursal, y el
      *total del salario a pagar por parte de la empresa.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E33.
       AUTHOR. MIGUEL MOYA.
       DATE-WRITTEN. SEPTEMBER 2022.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT DATOS1 ASSIGN TO "LOTE.txt"
                     FILE STATUS IS FS-STATUS1
       ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD DATOS1.
          01 REG-DATOS1.
             05 REG-SUC                         PIC 9(02).
             05 REG-DEP                         PIC 9(02).
             05 REG-SEC                         PIC 9(02).
             05 REG-COD-EMP                     PIC 9(02).
             05 REG-SALARY                      PIC 9(04)V9(02).
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
          01 FS-STATUS1                    PIC X(02) VALUE "00".
             88 FS-STATUS-OK                         VALUE "00".
             88 FS-STATUS-EOF                        VALUE "10".
          01 WS-VAR.
             02 WS-SUC-ANT               PIC 9(02).
             02 WS-DEP-ANT               PIC 9(02).
             02 WS-SEC-ANT               PIC 9(02).
             02 WS-TOTAL-SEC             PIC 9(06)V9(02).
             02 WS-TOTAL-DEP             PIC 9(06)V9(02).
             02 WS-TOTAL-SUC             PIC 9(06)V9(02).
             02 WS-TOTAL-EMP             PIC 9(07)V9(02).
             02 WS-MONTO                 PIC ZZ.ZZZ.ZZZ,ZZ.
          01 WS-TITULO.
               02 FILLER                   PIC X(03). 
               02 T-VENDEDOR               PIC 9(02). 
               02 FILLER                   PIC X(10). 
               02 T-FACTURA                PIC 9(02). 
               02 FILLER                   PIC X(04). 
               02 T-MONTO                  PIC  ZZ.ZZZ.ZZZ,ZZ.
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
      *                         20-PROCESO   
      ****************************************************************** 
       20-PROCESO.
             
           MOVE REG-SUC TO WS-SUC-ANT
           MOVE ZEROS TO WS-TOTAL-SUC
           DISPLAY "COD.EMP" "                      " "SALARIO" 
           PERFORM 20-CUT-SUC 
              THRU 20-CUT-SUC-F
              UNTIL REG-SUC <> WS-SUC-ANT
           MOVE WS-TOTAL-SUC TO WS-MONTO   
           DISPLAY "TOTAL SUCURSAL " WS-SUC-ANT " :     " WS-MONTO  
           COMPUTE WS-TOTAL-EMP = WS-TOTAL-EMP + WS-TOTAL-SUC 
           DISPLAY " "          
           .         
       20-PROCESO-F. EXIT.
      ******************************************************************
      *                         20-CUT-SUC   
      ******************************************************************      
       20-CUT-SUC.
           MOVE REG-DEP TO WS-DEP-ANT
           MOVE ZEROS TO WS-TOTAL-DEP
           PERFORM 20-CUT-DEP
              THRU 20-CUT-DEP-F
              UNTIL REG-DEP <> WS-DEP-ANT
           MOVE WS-TOTAL-DEP TO WS-MONTO   
           DISPLAY "TOTAL DEPARTAMENTO " WS-DEP-ANT " : " WS-MONTO 
           COMPUTE WS-TOTAL-SUC = WS-TOTAL-SUC + WS-TOTAL-DEP
           .
       20-CUT-SUC-F. EXIT.
      ******************************************************************
      *                         20-CUT-DEP   
      ******************************************************************      
       20-CUT-DEP.
           MOVE REG-SEC TO WS-SEC-ANT
           MOVE ZEROS TO WS-TOTAL-SEC
           PERFORM 20-CUT-SEC
              THRU 20-CUT-SEC-F
              UNTIL REG-SEC <> WS-SEC-ANT
           MOVE WS-TOTAL-SEC TO WS-MONTO   
           DISPLAY "TOTAL SECCION " WS-SEC-ANT " :      " WS-MONTO 
           COMPUTE WS-TOTAL-DEP = WS-TOTAL-DEP + WS-TOTAL-SEC
           .
       20-CUT-DEP-F. EXIT.
      ******************************************************************
      *                         20-CUT-SEC   
      ******************************************************************      
       20-CUT-SEC.
           COMPUTE WS-TOTAL-SEC = WS-TOTAL-SEC + REG-SALARY
           MOVE REG-SALARY TO WS-MONTO
           DISPLAY "  " REG-COD-EMP "                    " WS-MONTO
           PERFORM 20-LEER1
              THRU 20-LEER1-F
           .
       20-CUT-SEC-F. EXIT.
      ******************************************************************
      *                         30-FIN   
      ****************************************************************** 
       30-FIN.
           PERFORM 30-CERRAR-DATOS1
              THRU 30-CERRAR-DATOS1-F

           PERFORM 30-MOSTRAR-TOTAL
              THRU 30-MOSTRAR-TOTAL-F   
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
      *                         30-MOSTRAR-TOTAL   
      ****************************************************************** 
       30-MOSTRAR-TOTAL.
           MOVE WS-TOTAL-EMP TO WS-MONTO
           DISPLAY "TOTAL A PAGAR :         " WS-MONTO
           .
       30-MOSTRAR-TOTAL-F. EXIT.
       END PROGRAM E33.