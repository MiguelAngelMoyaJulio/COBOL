      ******************************************************************
      * Una compañía aérea desea emitir un listado con los movimientos 
      *mensuales de sus m vuelos al exterior.
      *ara ello cuenta con la siguiente información:
      *	De cada vuelo realizado el número de vuelo, destino, y 
      *antidad de asientos.
      *	De cada pasajero el número de 
      *asaporte y el importe que abonó por el pasaje (en dólares). 
      *a información finaliza con un número de pasaporte igual a cero.
      *Se pide emitir el siguiente listado: 
      *Destino: xxxxxxxxxxxxxxxxx
      *Nro. de Pasaporte	Importe en u$s
      *9999999	               999.99
      *9999999	               999.99
      *9999999	               999.99
      *Total recaudado del vuelo: $99999.99
      * de Asientos Libres del vuelo: 999.99
      * de Asientos Ocupados del vuelo: 999.99
      *otal recaudado en el mes: $999999.99
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E29.
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
       SELECT DATOS ASSIGN TO "VUELOS.txt"
                     FILE STATUS IS FS-STATUS-FILE
                     ORGANIZATION IS LINE SEQUENTIAL. 
       
      ****************************  OUTPUT  ****************************
       DATA DIVISION.
       FILE SECTION.
       FD DATOS.
           01 REG-DATOS.
               05 REG-NUMERO-VUELO          PIC 9(03).
               05 REG-DESTINO               PIC X(14).
               05 REG-CANTIDAD-ASIENTOS     PIC 9(03).
               05 REG-PASAPORTE             PIC 9(06).
               05 REG-IMPORTE               PIC 9(04)V9(02).
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
       01 FS-STATUS-FILE                  PIC X(02) VALUE "00".
           88 FS-STATUS-FILE-OK                     VALUE "00".
           88 FS-STATUS-FILE-EOF                    VALUE "10".
      
      ************************** VARIABLES *****************************
       01 WS-VAR.
           02 WS-CANT-ASIENTO-OCUPADO     PIC 9(03).
           02 WS-CANT-ASIENTO-DISPONIBLE  PIC 9(03).
           02 WS-PORC-ASIENTO-LIBRE       PIC 9(03)V99.
           02 WS-PORC-ASIENTO-OCUPADO     PIC 9(03)V99.
           02 WS-DESTINO-ANT              PIC X(14).
           02 WS-TOT-DESTINO              PIC 9(07)V99.
           02 WS-TOT-EMPRESA              PIC 9(07)V99.
           02 WS-TOTE-MA                  PIC ZZ.ZZZ.ZZZ,ZZ.
       01 WS-TITULO.  
           02 FILLER                      PIC X(03). 
           02 T-PASAPORTE                 PIC 9(06). 
           02 FILLER                      PIC X(04). 
           02 T-MONTO                     PIC  ZZ.ZZZ.ZZZ,ZZ.

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
      *                         200000-PROCESS   
      ****************************************************************** 
       200000-PROCESS.  
           MOVE REG-DESTINO TO WS-DESTINO-ANT
           MOVE REG-CANTIDAD-ASIENTOS TO WS-CANT-ASIENTO-DISPONIBLE
           MOVE ZEROS TO WS-CANT-ASIENTO-OCUPADO
           MOVE ZEROS TO WS-PORC-ASIENTO-OCUPADO
           MOVE ZEROS TO WS-PORC-ASIENTO-LIBRE
           INITIALIZE WS-TOT-DESTINO    
           
           DISPLAY "DESTINO " WS-DESTINO-ANT
           DISPLAY "NRO.PASAPORTE  IMPORTE(U$S) "
           PERFORM UNTIL WS-DESTINO-ANT <> REG-DESTINO
               ADD 1 TO WS-CANT-ASIENTO-OCUPADO
               COMPUTE WS-TOT-DESTINO = WS-TOT-DESTINO + REG-IMPORTE
               MOVE REG-PASAPORTE TO T-PASAPORTE
               MOVE REG-IMPORTE TO T-MONTO
               DISPLAY WS-TITULO
               PERFORM 210000-READ-DATOS
                  THRU 210000-READ-DATOS-F
           END-PERFORM

           COMPUTE WS-PORC-ASIENTO-OCUPADO = (WS-CANT-ASIENTO-OCUPADO * 
                   100) / WS-CANT-ASIENTO-DISPONIBLE 
           
           COMPUTE WS-PORC-ASIENTO-LIBRE = 100,00 - 
                   WS-PORC-ASIENTO-OCUPADO

           PERFORM 220000-MOSTRAR-TOTAL-CORTE
              THRU 220000-MOSTRAR-TOTAL-CORTE-F        
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
      *                         220000-MOSTRAR-TOTAL-CORTE   
      ******************************************************************      
       220000-MOSTRAR-TOTAL-CORTE.
           MOVE WS-TOT-DESTINO TO WS-TOTE-MA
           ADD  WS-TOT-DESTINO TO WS-TOT-EMPRESA   
           DISPLAY "TOTAL RECAUDADO DEL VUELO " WS-DESTINO-ANT 
                   " : $" WS-TOTE-MA
           DISPLAY "% DE ASIENTOS LIBRES DEL VUELO : "
                   WS-PORC-ASIENTO-LIBRE
           
           DISPLAY "% DE ASIENTOS OCUPADOS DEL VUELO : "
                   WS-PORC-ASIENTO-OCUPADO
           .
        220000-MOSTRAR-TOTAL-CORTE-F. EXIT.
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
           DISPLAY "TOTAL RECAUDADO EN EL MES $" WS-TOTE-MA 
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
       END PROGRAM E29.