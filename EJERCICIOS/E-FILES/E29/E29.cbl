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
       SELECT DATOS1 ASSIGN TO "VUELOS.txt"
                     FILE STATUS IS FS-STATUS-FILE
                     ORGANIZATION IS LINE SEQUENTIAL. 
       
      ****************************  OUTPUT  ****************************
       DATA DIVISION.
       FILE SECTION.
       FD DATOS1.
           01 REG-DATOS1.
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
               PERFORM 210000-READ-DATOS1
                  THRU 210000-READ-DATOS1-F
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
           PERFORM 310000-CLOSE-DATOS1
              THRU 310000-CLOSE-DATOS1-F

           PERFORM 320000-VENTA-TOTAL
              THRU 320000-VENTA-TOTAL-F
           STOP RUN
           .    
       300000-END-F. EXIT.
      ******************************************************************
      *                         310000-CLOSE-DATOS1   
      ****************************************************************** 
       310000-CLOSE-DATOS1.
           CLOSE DATOS1
           IF NOT FS-STATUS-FILE-OK
               DISPLAY "ERROR AL CERRAR ARCHIVO " FS-STATUS-FILE
           END-IF
           .
       310000-CLOSE-DATOS1-F. EXIT.
      ******************************************************************
      *                         320000-VENTA-TOTAL   
      ****************************************************************** 
       320000-VENTA-TOTAL.
           MOVE WS-TOT-EMPRESA TO WS-TOTE-MA   
           DISPLAY "TOTAL RECAUDADO EN EL MES $" WS-TOTE-MA 
           .
       320000-VENTA-TOTAL-F. EXIT.
       END PROGRAM E29.