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
       SELECT DATOS1 ASSIGN TO "LOTE.txt"
                     FILE STATUS IS FS-STATUS-FILE
                     ORGANIZATION IS LINE SEQUENTIAL. 
       
      ****************************  OUTPUT  ****************************
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
      ************************  CONSTANTS  *****************************

      ************************** TABLES ********************************

      **************************  SWITCHES  **************************** 
       01 FS-STATUS-FILE                    PIC X(02) VALUE "00".
          88 FS-STATUS-FILE-OK                        VALUE "00".
          88 FS-STATUS-FILE-EOF                       VALUE "10".
      ************************** VARIABLES *****************************
       01 WS-VARIABLES.
          02 WS-LEG-ANT                 PIC 9(02).
          02 WS-CANTIDAD-MATERIAS       PIC 9(02).
          02 WS-NOTA-MAX                PIC 9(02)v9(02).
          02 WS-MATERIA-MAX             PIC 9(02).
          02 WS-I                       PIC 9(01).
          02 WS-MONTO                   PIC ZZZ,ZZ.
      ******************************************************************
      *                       LINKAGE SECTION   
      ****************************************************************** 
       LINKAGE SECTION.
      ******************************************************************
      *                         PROCEDURE DIVISION   
      ******************************************************************
       PROCEDURE DIVISION.
                                                                          
           PERFORM 100000-START                      
              THRU 100000-START-F                    
                                                  
           PERFORM 200000-PROCESS                     
              THRU 200000-PROCESS-F                   
              UNTIL FS-STATUS-FILE-EOF                 
                                                  
           PERFORM 300000-END                         
              THRU 300000-END-F                       
           .                                      
      ******************************************************************
      *                         100000-START         
      ******************************************************************
       100000-START.                                 
           PERFORM 110000-OPEN-DATOS1                
              THRU 110000-OPEN-DATOS1-F
                            
           PERFORM 210000-READ-DATOS1                       
              THRU 210000-READ-DATOS1-F                     
           .                                      
       100000-START-F. EXIT.                         
      ******************************************************************
      *                         110000-OPEN-DATOS1   
      ******************************************************************
       110000-OPEN-DATOS1.                        
           OPEN INPUT DATOS1                   
           IF NOT FS-STATUS-FILE-OK
               DISPLAY "ERROR AL ABRIR ARCHIVO " FS-STATUS-FILE
           END-IF
           .
       110000-OPEN-DATOS1-F. EXIT.
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
                    CONTINUE
           END-EVALUATE
           .
       210000-READ-DATOS1-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS   
      ****************************************************************** 
       200000-PROCESS.
           MOVE REG-LEGAJO TO WS-LEG-ANT
           MOVE ZEROS TO WS-CANTIDAD-MATERIAS
           MOVE 1 TO WS-I
           MOVE ZEROS TO WS-NOTA-MAX
           MOVE ZEROS TO WS-MATERIA-MAX
           
           DISPLAY "LEGAJO ESTUDIANTE : " WS-LEG-ANT 
           DISPLAY "COD.MATERIA" "         " "NOTA"  
           
           PERFORM UNTIL REG-LEGAJO <> WS-LEG-ANT
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
             PERFORM 210000-READ-DATOS1
                THRU 210000-READ-DATOS1-F   
           END-PERFORM
           DISPLAY "MATERIAS CURSADAS : " WS-CANTIDAD-MATERIAS
           MOVE WS-NOTA-MAX TO WS-MONTO
           DISPLAY "NOTA MAXIMA : " WS-MONTO " MATERIA : " 
                   WS-MATERIA-MAX
           DISPLAY " "           
           .         
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           PERFORM 30-CERRAR-DATOS1
              THRU 30-CERRAR-DATOS1-F
           STOP RUN   
           .    
       300000-END-F. EXIT.
      ******************************************************************
      *                         30-CERRAR-DATOS1   
      ****************************************************************** 
       30-CERRAR-DATOS1.
           CLOSE DATOS1
           IF NOT FS-STATUS-FILE-OK
               DISPLAY "ERROR AL CERRAR ARCHIVO " FS-STATUS-FILE
           END-IF
           .
       30-CERRAR-DATOS1-F. EXIT.
       END PROGRAM E35.