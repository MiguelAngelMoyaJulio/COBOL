      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-20
      * CORTE DE CONTROL POR SUCURSAL Y VENDEDOR
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E28.
       AUTHOR. MIGUEL MOYA.
       DATE-WRITTEN. OCTOBER 2022.
       DATE-COMPILED. OCTOBER 2022.
      ******************************************************************
      *                     ENVIRONMENT DIVISION
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      ******************************************************************
      *                            FILES   
      ******************************************************************
      *****************************  INPUT  ****************************
       SELECT DATOS ASSIGN TO "SUC-VEN.txt"
                     FILE STATUS IS FS-STATUS-FILE
                     ORGANIZATION IS LINE SEQUENTIAL. 
       
      ****************************  OUTPUT  ****************************
       
       DATA DIVISION.
       FILE SECTION.
       FD DATOS.
          01 REG-DATOS.
             05 REG-SUCURSAL              PIC 9(02).
             05 REG-VENDEDOR              PIC 9(02).
             05 REG-FACTURA               PIC 9(02).
             05 REG-MONTO                 PIC 9(07)V99.
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
       01 FS-STATUS-FILE              PIC X(02) VALUE "00".
          88 FS-STATUS-FILE-OK                  VALUE "00".
          88 FS-STATUS-FILE-EOF                 VALUE "10".
      
      ************************** VARIABLES *****************************
       01 WS-VAR.
          02 WS-VENDEDOR-ANT          PIC 9(02).
          02 WS-TOT-VENDEDOR          PIC 9(07)V99.
          02 WS-SUCURSAL-ANT          PIC 9(02).
          02 WS-TOT-SUC               PIC 9(07)V99.
          02 WS-TOT-EMPRESA           PIC 9(07)V99.
          02 WS-TOTE-MA               PIC ZZ,ZZZ,ZZZ.ZZ.
       01 WS-TITULO.
            02 FILLER                 PIC X(03). 
            02 T-VENDEDOR             PIC 9(02). 
            02 FILLER                 PIC X(10). 
            02 T-FACTURA              PIC 9(02). 
            02 FILLER                 PIC X(04). 
            02 T-MONTO                PIC  ZZ,ZZZ,ZZZ.ZZ.

       01 WS-ERRORES.
           05 WS-ERR-PARRAFO            PIC X(30).
           05 WS-ERR-OBJETO             PIC X(10).
           05 WS-ERR-OPERACION          PIC X(15).
           05 WS-ERR-CODIGO             PIC 9(02).     
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
      *                         200000-PROCESS   
      ****************************************************************** 
       200000-PROCESS.
           MOVE REG-SUCURSAL TO WS-SUCURSAL-ANT
           INITIALIZE WS-TOT-SUC    
           
           DISPLAY "VENTAS DEL MES SUCURSAL " WS-SUCURSAL-ANT
           DISPLAY "VENDEDOR  NRO.FACTURA  VALOR.VENTA "
           PERFORM UNTIL WS-SUCURSAL-ANT <> REG-SUCURSAL 
             INITIALIZE WS-TOT-VENDEDOR
             MOVE REG-VENDEDOR TO WS-VENDEDOR-ANT
             PERFORM UNTIL WS-VENDEDOR-ANT <> REG-VENDEDOR
                ADD REG-MONTO     TO WS-TOT-VENDEDOR
                MOVE REG-VENDEDOR TO T-VENDEDOR
                MOVE REG-FACTURA  TO T-FACTURA
                MOVE REG-MONTO    TO T-MONTO
                DISPLAY WS-TITULO
                PERFORM 210000-READ-DATOS
                   THRU 210000-READ-DATOS-F
             END-PERFORM

             MOVE WS-TOT-VENDEDOR TO WS-TOTE-MA   
             DISPLAY "TOTAL VENDEDOR :    $" WS-TOTE-MA   
             COMPUTE WS-TOT-SUC = WS-TOT-SUC + WS-TOT-VENDEDOR     
           END-PERFORM  

           MOVE WS-TOT-SUC TO WS-TOTE-MA
           ADD WS-TOT-SUC TO WS-TOT-EMPRESA   
           DISPLAY "TOTAL SUCURSAL " WS-SUCURSAL-ANT 
                   " : $" WS-TOTE-MA
           .         
       200000-PROCESS-F. EXIT.
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
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           PERFORM 310000-CLOSE-DATOS
              THRU 310000-CLOSE-DATOS-F

           PERFORM 320000-VENTA-TOTAL
              THRU 320000-VENTA-TOTAL-F
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
      *                         320000-VENTA-TOTAL   
      ****************************************************************** 
       320000-VENTA-TOTAL.
           MOVE WS-TOT-EMPRESA TO WS-TOTE-MA   
           DISPLAY "TOTAL EMPRESA " WS-TOTE-MA 
           .
       320000-VENTA-TOTAL-F. EXIT.
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
       END PROGRAM E28.