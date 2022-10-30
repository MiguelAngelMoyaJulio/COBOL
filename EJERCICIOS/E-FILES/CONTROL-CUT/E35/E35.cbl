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
       SELECT DATOS ASSIGN TO "LOTE.txt"
                     FILE STATUS IS FS-STATUS-FILE
                     ORGANIZATION IS LINE SEQUENTIAL. 
       
      ****************************  OUTPUT  ****************************
       DATA DIVISION.
       FILE SECTION.
       FD DATOS.
          01 REG-DATOS.
             05 REG-LEGAJO                         PIC 9(02).
             05 REG-MATERIA                        PIC 9(02).
             05 REG-NOTA                           PIC 9(02)V9(02).
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
      ************************  CONSTANTS  *****************************
       01 WS-CONSTANTES.
           02 CON-RUTINAS.
              05 CON-RUTINA01  PIC X(08) VALUE 'RUTINA01'.
           02 CON-PARRAFO.
              05 CON-110000-OPEN-DATOS      PIC X(30) VALUE 
              '110000-OPEN-DATOS           '.
              05 CON-210000-READ-DATOS      PIC X(30) VALUE 
              '210000-READ-DATOS           '.
              05 CON-310000-CLOSE-DATOS      PIC X(30) VALUE 
              '310000-CLOSE-DATOS          '.
           02 CON-OPERACIONES.
              05 CON-ABRIR     PIC X(15) VALUE 'ABRIR          '.
              05 CON-LEER      PIC X(15) VALUE 'LEER           '.
              05 CON-CERRAR    PIC X(15) VALUE 'CERRAR         '.
              05 CON-GRABAR    PIC X(15) VALUE 'GRABAR         '.
              05 CON-RUTINA    PIC X(15) VALUE 'LLAMAR RUTINA  '.
           02 CON-OBJETOS.
              05 CON-DATOS     PIC X(10) VALUE 'DATOS   '.
           02 CON-OTROS.
              05 CON-1         PIC 9(01) VALUE 1.
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
       01 WS-ERRORES.
           05 WS-ERR-PARRAFO            PIC X(30).
           05 WS-ERR-OBJETO             PIC X(10).
           05 WS-ERR-OPERACION          PIC X(15).
           05 WS-ERR-CODIGO             PIC 9(02).   
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
           PERFORM 110000-OPEN-DATOS                
              THRU 110000-OPEN-DATOS-F
                            
           PERFORM 210000-READ-DATOS                       
              THRU 210000-READ-DATOS-F                     
           .                                      
       100000-START-F. EXIT.                         
      ******************************************************************
      *                         110000-OPEN-DATOS   
      ******************************************************************
       110000-OPEN-DATOS.                        
           OPEN INPUT DATOS                   
           IF NOT FS-STATUS-FILE-OK
              MOVE CON-110000-OPEN-DATOS   TO WS-ERR-PARRAFO 
              MOVE CON-DATOS               TO WS-ERR-OBJETO 
              MOVE CON-ABRIR               TO WS-ERR-OPERACION 
              MOVE FS-STATUS-FILE          TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       110000-OPEN-DATOS-F. EXIT.
      ******************************************************************
      *                         210000-READ-DATOS   
      ******************************************************************      
       210000-READ-DATOS.
           INITIALIZE REG-DATOS
           READ DATOS INTO REG-DATOS
           EVALUATE TRUE
               WHEN FS-STATUS-FILE-OK
                    CONTINUE
               WHEN FS-STATUS-FILE-EOF
                    CONTINUE
               WHEN OTHER
                    MOVE CON-210000-READ-DATOS   TO WS-ERR-PARRAFO 
                    MOVE CON-DATOS               TO WS-ERR-OBJETO 
                    MOVE CON-LEER                TO WS-ERR-OPERACION 
                    MOVE FS-STATUS-FILE          TO WS-ERR-CODIGO
                    PERFORM 399999-END-PROGRAM
                       THRU 399999-END-PROGRAM-F     
           END-EVALUATE
           .
       210000-READ-DATOS-F. EXIT.
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
             PERFORM 210000-READ-DATOS
                THRU 210000-READ-DATOS-F   
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
           PERFORM 30-CERRAR-DATOS
              THRU 30-CERRAR-DATOS-F
           STOP RUN   
           .    
       300000-END-F. EXIT.
      ******************************************************************
      *                         30-CERRAR-DATOS   
      ****************************************************************** 
       30-CERRAR-DATOS.
           CLOSE DATOS
           IF NOT FS-STATUS-FILE-OK
              MOVE CON-310000-CLOSE-DATOS   TO WS-ERR-PARRAFO 
              MOVE CON-DATOS                TO WS-ERR-OBJETO 
              MOVE CON-CERRAR               TO WS-ERR-OPERACION 
              MOVE FS-STATUS-FILE           TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F 
           END-IF
           .
       30-CERRAR-DATOS-F. EXIT.
      ******************************************************************
      *                         399999-END-PROGRAM   
      ******************************************************************
       399999-END-PROGRAM.
           DISPLAY "***************************************************"
           DISPLAY "*              SE PRODUJO UN ERROR                *"
           DISPLAY "***************************************************"
           DISPLAY "PARRAFO : "   WS-ERR-PARRAFO
           DISPLAY "OBJETO : "    WS-ERR-OBJETO
           DISPLAY "OPERACION : " WS-ERR-OPERACION
           DISPLAY "CODIGO : "    WS-ERR-CODIGO
           STOP RUN
           .
       399999-END-PROGRAM-F. EXIT. 
       END PROGRAM E35.