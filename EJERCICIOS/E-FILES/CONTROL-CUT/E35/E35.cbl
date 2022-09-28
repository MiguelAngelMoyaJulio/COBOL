      ******************************************************************
      *Se tiene la siguiente información por cada una de las materias 
      *que cursa cada estudiante:
      * Código del estudiante.
      * Código de la materia.
      * Nota definitiva.
      *No se conoce el número de estudiantes ni el número de materias 
      *cursadas por cada estudiante.
      *elaborar un algoritmo que muestre:
      * Código del estudiante.
      * Número de materias cursadas.
      * Nota mayor y en qué materia la obtuvo
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E35.
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
             05 REG-LEGAJO                         PIC 9(02).
             05 REG-MATERIA                        PIC 9(02).
             05 REG-NOTA                           PIC 9(02)V9(02).
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
          01 FS-STATUS1                    PIC X(02) VALUE "00".
             88 FS-STATUS-OK                         VALUE "00".
             88 FS-STATUS-EOF                        VALUE "10".
          01 WS-VAR.
             02 WS-LEG-ANT                 PIC 9(02).
             02 WS-CANTIDAD-MATERIAS       PIC 9(02).
             02 WS-NOTA-MAX                PIC 9(02)v9(02).
             02 WS-MATERIA-MAX             PIC 9(02).
             02 WS-I                       PIC 9(01).
             02 WS-MONTO                   PIC ZZZ,ZZ.
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
           MOVE REG-LEGAJO TO WS-LEG-ANT
           MOVE ZEROS TO WS-CANTIDAD-MATERIAS
           MOVE 1 TO WS-I
           MOVE ZEROS TO WS-NOTA-MAX
           MOVE ZEROS TO WS-MATERIA-MAX
           
           DISPLAY "LEGAJO ESTUDIANTE : " WS-LEG-ANT 
           DISPLAY "COD.MATERIA" "         " "NOTA"  
           
           PERFORM 20-CUT-LEG 
              THRU 20-CUT-LEG-F
              UNTIL REG-LEGAJO <> WS-LEG-ANT
           
           DISPLAY "MATERIAS CURSADAS : " WS-CANTIDAD-MATERIAS
           MOVE WS-NOTA-MAX TO WS-MONTO
           DISPLAY "NOTA MAXIMA : " WS-MONTO " MATERIA : " 
                   WS-MATERIA-MAX
           DISPLAY " "           
           .         
       20-PROCESO-F. EXIT.
      ******************************************************************
      *                         20-CUT-LEG   
      ******************************************************************      
       20-CUT-LEG.
          IF WS-I = 1 THEN
             MOVE REG-NOTA TO WS-NOTA-MAX
             MOVE REG-MATERIA TO WS-MATERIA-MAX
             ADD 1 TO WS-I
          END-IF
          IF REG-NOTA > WS-NOTA-MAX THEN
             MOVE REG-NOTA TO WS-NOTA-MAX
             MOVE REG-MATERIA TO WS-MATERIA-MAX
          END-IF
          COMPUTE WS-CANTIDAD-MATERIAS = WS-CANTIDAD-MATERIAS + 1
          MOVE REG-NOTA TO WS-MONTO
          DISPLAY REG-MATERIA "                 " WS-MONTO
          PERFORM 20-LEER1
             THRU 20-LEER1-F
           .
       20-CUT-LEG-F. EXIT.
      ******************************************************************
      *                         30-FIN   
      ****************************************************************** 
       30-FIN.
           PERFORM 30-CERRAR-DATOS1
              THRU 30-CERRAR-DATOS1-F
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
       END PROGRAM E35.