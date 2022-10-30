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
             05 REG-SUC                         PIC 9(02).
             05 REG-DEP                         PIC 9(02).
             05 REG-SEC                         PIC 9(02).
             05 REG-COD-EMP                     PIC 9(02).
             05 REG-SALARY                      PIC 9(04)V9(02).
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
          88 FS-STATUS-FILE-OK                         VALUE "00".
          88 FS-STATUS-FILE-EOF                        VALUE "10".
      ************************** VARIABLES *****************************    
       01 WS-VARIABLES.
          02 WS-SUC-ANT               PIC 9(02).
          02 WS-DEP-ANT               PIC 9(02).
          02 WS-SEC-ANT               PIC 9(02).
          02 WS-TOTAL-SEC             PIC 9(06)V9(02).
          02 WS-TOTAL-DEP             PIC 9(06)V9(02).
          02 WS-TOTAL-SUC             PIC 9(06)V9(02).
          02 WS-TOTAL-EMP             PIC 9(07)V9(02).
          02 WS-MONTO                 PIC ZZ.ZZZ.ZZZ,ZZ.

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
             
           MOVE REG-SUC TO WS-SUC-ANT
           MOVE ZEROS TO WS-TOTAL-SUC
           DISPLAY "COD.EMP" "                      " "SALARIO" 
           PERFORM UNTIL REG-SUC <> WS-SUC-ANT
             MOVE REG-DEP TO WS-DEP-ANT
             MOVE ZEROS TO WS-TOTAL-DEP
             PERFORM UNTIL REG-DEP <> WS-DEP-ANT
                MOVE REG-SEC TO WS-SEC-ANT
                MOVE ZEROS TO WS-TOTAL-SEC
                PERFORM UNTIL REG-SEC <> WS-SEC-ANT
                   COMPUTE WS-TOTAL-SEC = WS-TOTAL-SEC + REG-SALARY
                   MOVE REG-SALARY TO WS-MONTO
                   DISPLAY "  " REG-COD-EMP "                    " 
                                WS-MONTO
                   PERFORM 210000-READ-DATOS
                      THRU 210000-READ-DATOS-F     
                END-PERFORM
                MOVE WS-TOTAL-SEC TO WS-MONTO   
                DISPLAY "TOTAL SECCION " WS-SEC-ANT " :      " WS-MONTO 
                COMPUTE WS-TOTAL-DEP = WS-TOTAL-DEP + WS-TOTAL-SEC  
             END-PERFORM
             MOVE WS-TOTAL-DEP TO WS-MONTO   
             DISPLAY "TOTAL DEPARTAMENTO " WS-DEP-ANT " : " WS-MONTO 
             COMPUTE WS-TOTAL-SUC = WS-TOTAL-SUC + WS-TOTAL-DEP
           END-PERFORM
           MOVE WS-TOTAL-SUC TO WS-MONTO   
           DISPLAY "TOTAL SUCURSAL " WS-SUC-ANT " :     " WS-MONTO  
           COMPUTE WS-TOTAL-EMP = WS-TOTAL-EMP + WS-TOTAL-SUC 
           DISPLAY " "          
           .         
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           PERFORM 310000-CLOSE-DATOS
              THRU 310000-CLOSE-DATOS-F

           PERFORM 320000-MOSTRAR-TOTAL
              THRU 320000-MOSTRAR-TOTAL-F
           STOP RUN      
           .    
       300000-END-F. EXIT.
      ******************************************************************
      *                         310000-CLOSE-DATOS   
      ****************************************************************** 
       310000-CLOSE-DATOS.
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
       310000-CLOSE-DATOS-F. EXIT.
      ******************************************************************
      *                         320000-MOSTRAR-TOTAL   
      ****************************************************************** 
       320000-MOSTRAR-TOTAL.
           MOVE WS-TOTAL-EMP TO WS-MONTO
           DISPLAY "TOTAL A PAGAR :         " WS-MONTO
           .
       320000-MOSTRAR-TOTAL-F. EXIT.
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
       END PROGRAM E33.