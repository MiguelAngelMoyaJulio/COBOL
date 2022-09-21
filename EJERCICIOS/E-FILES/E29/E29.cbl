      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-20
      * Una compañía aérea desea emitir un listado con los movimientos mensuales de sus m vuelos al exterior.
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
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT OPTIONAL DATOS1
       ASSIGN TO "VUELOS.txt"
       FILE STATUS IS FS-STATUS1
       ORGANIZATION IS LINE SEQUENTIAL.
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
           01 FS-STATUS1                    PIC X(02) VALUE "00".
               88 FS-STATUS-OK                        VALUE "00".
               88 FS-STATUS-EOF                       VALUE "10".
           01 WS-VAR.
               02 WS-CANT-ASIENTO-OCUPADO     PIC 9(03).
               02 WS-CANT-ASIENTO-DISPONIBLE  PIC 9(03).
               02 WS-PORC-ASIENTO-LIBRE       PIC 9(03)V99.
               02 WS-PORC-ASIENTO-OCUPADO  PIC 9(03)V99.
               02 WS-DESTINO-ANT           PIC X(14).
               02 WS-TOT-DESTINO           PIC 9(07)V99.
               02 WS-TOT-EMPRESA           PIC 9(07)V99.
               02 WS-TOTE-MA               PIC ZZ,ZZZ,ZZZ.ZZ.
           01 WS-TITULO.
               02 FILLER                   PIC X(03). 
               02 T-PASAPORTE              PIC 9(06). 
               02 FILLER                   PIC X(04). 
               02 T-MONTO                  PIC  ZZ,ZZZ,ZZZ.ZZ.
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
           MOVE REG-DESTINO TO WS-DESTINO-ANT
           MOVE REG-CANTIDAD-ASIENTOS TO WS-CANT-ASIENTO-DISPONIBLE
           MOVE ZEROS TO WS-CANT-ASIENTO-OCUPADO
           MOVE ZEROS TO WS-PORC-ASIENTO-OCUPADO
           MOVE ZEROS TO WS-PORC-ASIENTO-LIBRE
           INITIALIZE WS-TOT-DESTINO    
           
           DISPLAY "DESTINO " WS-DESTINO-ANT
           DISPLAY "NRO.PASAPORTE  IMPORTE(U$S) "
           
           PERFORM 20-CORTE-DESTINO
              THRU 20-CORTE-DESTINO-F
              UNTIL WS-DESTINO-ANT <> REG-DESTINO

           COMPUTE WS-PORC-ASIENTO-OCUPADO = (WS-CANT-ASIENTO-OCUPADO * 
                   100) / WS-CANT-ASIENTO-DISPONIBLE 
           
           COMPUTE WS-PORC-ASIENTO-LIBRE = 100.00 - 
                   WS-PORC-ASIENTO-OCUPADO

           PERFORM 20-MOSTRAR-TOTAL-CORTE
              THRU 20-MOSTRAR-TOTAL-CORTE-F        
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
      *                         20-CORTE-DESTINO   
      ******************************************************************      
       20-CORTE-DESTINO.
           ADD 1 TO WS-CANT-ASIENTO-OCUPADO
           COMPUTE WS-TOT-DESTINO = WS-TOT-DESTINO + REG-IMPORTE
           MOVE REG-PASAPORTE TO T-PASAPORTE
           MOVE REG-IMPORTE TO T-MONTO
           DISPLAY WS-TITULO
           PERFORM 20-LEER1
              THRU 20-LEER1-F
           .
        20-CORTE-DESTINO-F. EXIT.
      ******************************************************************
      *                         20-MOSTRAR-TOTAL-CORTE   
      ******************************************************************      
       20-MOSTRAR-TOTAL-CORTE.
           MOVE WS-TOT-DESTINO TO WS-TOTE-MA
           ADD WS-TOT-DESTINO TO WS-TOT-EMPRESA   
           DISPLAY "TOTAL RECAUDADO DEL VUELO " WS-DESTINO-ANT 
                   " : $" WS-TOTE-MA
           DISPLAY "% DE ASIENTOS LIBRES DEL VUELO : "
                   WS-PORC-ASIENTO-LIBRE
           
           DISPLAY "% DE ASIENTOS OCUPADOS DEL VUELO : "
                   WS-PORC-ASIENTO-OCUPADO
           .
        20-MOSTRAR-TOTAL-CORTE-F. EXIT.
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
           DISPLAY "TOTAL RECAUDADO EN EL MES $" WS-TOTE-MA 
           .
       30-VENTA-TOTAL-F. EXIT.
       END PROGRAM E29.