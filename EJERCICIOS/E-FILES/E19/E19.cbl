      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-07
      * Desarrollar una función que reciba un número natural de 4 
      *dígitos en formato de hhmm que corresponde a un tiempo 
      *determinado en horas y minutos, y retorne ese tiempo en minutos. 
      *El prototipo debe ser el siguiente:
      *long toMin(long t);
      *Desarrollar una función void que reciba el 
      *costo (expresado en pesos) de un abono telefónico, la cantidad 
      *de minutos libres que incluye el abono, 
      *el cargo (expresado en pesos) por minuto excedente y 
      *la cantidad de minutos utilizados por un abonado y 
      *retorne la cantidad de minutos excedidos y 
      *el monto (también expresado en pesos) que se debe 
      *abonar (costo del abono más minutos excedidos por el
      * costo de minutos excedidos) más el 21% del valor del IVA.
      *El prototipo y posterior desarrollo de esta función 
      *queda a cargo del alumno.
      *Utilizando lo anterior, se pide desarrollar un algoritmo 
      *que resuelva la siguiente situación problemática:
      *Todos los fines de mes, una empresa de telefonía 
      *celular debe confeccionar las facturas con los consumos de 
      *todos sus abonados, que se realizan en 
      *tres turnos de trabajo: Mañana, Tarde y Noche.
      **Para ello se ingresará por teclado la siguiente 
      *información de cada celular:
      *•	Número de celular 9 dígitos ( 0 indica cambio de turno).
      *•	Nombre del abonado 20 caracteres.
      *•	Dirección del abonado 25 caracteres.
      *•	Tiempo utilizado 4 dígitos en formato hhmm.
      *•	Tipo de abono (carácter, A, B, C, D o E).
      *Dependiendo del tipo de abono que se tenga, el usuario
      * tiene cierta cantidad de minutos libres, por los cuales 
      * no abona cargo extra, pero por cada minuto que se 
      * exceda debe abonar una suma extra según la siguiente tabla:
      *Plan	A	B	C	D	E
      *Costo	$70	$55	$40	$28	$19
      *Minutos Libres	300	200	100	60	40
      *Cargo Min Exced.	$0.09	$0.15	$0.21	$0.29	$0.37
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E19.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT OPTIONAL DATOS
       ASSIGN TO "DAT.txt"
       ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD DATOS.
           01 REG-DATOS.
               05 REG-MOBILE           PIC 9(09).
               05 REG-NAME             PIC X(06).
               05 REG-ADDRESS          PIC X(30).
               05 REG-CONSUMPTION      PIC 9(04).
               05 REG-TYPE             PIC X(01).
       WORKING-STORAGE SECTION.
           77 WS-STATUS                PIC X(01).
           01 WS-VAR.                
               02 WS-TOTAL-MINUTES     PIC 9(05).
               02 WS-MINUTES-REMAINING PIC S9(05).
               02 WS-TOTAL-BILLING     PIC 9(05)v99.
               02 WS-TOTAL-CONSUMPTION.
                   05 WS-HOUR          PIC 9(02).     
                   05 WS-MINUTES       PIC 9(02). 
           01 WS-TOTALS.
               02 T-NAME               PIC X(06).
               02 FILLER               PIC X(02).            
               02 T-ADDRESS            PIC X(30).
               02 FILLER               PIC X(02).            
               02 T-TYPE               PIC X(01).
               02 FILLER               PIC X(02).            
               02 T-FREE-MINUTES       PIC 9(03).
               02 FILLER               PIC X(02).            
               02 T-EX-MINUTES         PIC 9(05).
               02 FILLER               PIC X(02).            
               02 T-BILLING            PIC ZZZ.ZZ.
               02 FILLER               PIC X(02).            
              
       PROCEDURE DIVISION.
           OPEN INPUT DATOS
       
           PERFORM 20-LEER
           THRU 20-LEER-F
           MOVE "Y" TO WS-STATUS
           
           PERFORM 30-CALCULO
           THRU 30-CALCULO-F
           UNTIL WS-STATUS = "F"
       
           CLOSE DATOS

           PERFORM 40-TOTALES
              THRU 40-TOTALES-F
           . 
            STOP RUN.

       20-LEER.
           INITIALIZE REG-DATOS
           READ DATOS NEXT RECORD
           AT END
           MOVE "F" TO  WS-STATUS
           .
       20-LEER-F. EXIT.

       30-CALCULO.
           MOVE ZEROS TO WS-TOTAL-CONSUMPTION
           MOVE ZEROS TO WS-TOTAL-MINUTES
           MOVE ZEROS TO WS-TOTAL-BILLING

           PERFORM 31-TOTAL-MINUTES
              THRU 31-TOTAL-MINUTES-F
           
           PERFORM 32-BILLING
              THRU 32-BILLING-F

           PERFORM 20-LEER
           THRU 20-LEER-F
           .
       30-CALCULO-F. EXIT. 
       31-TOTAL-MINUTES.
           
           MOVE REG-CONSUMPTION TO WS-TOTAL-CONSUMPTION
           COMPUTE WS-TOTAL-MINUTES = WS-HOUR * 60 + WS-MINUTES
           .
       31-TOTAL-MINUTES-F.
       32-BILLING.
           EVALUATE TRUE
           WHEN REG-TYPE = "A"
               COMPUTE WS-MINUTES-REMAINING = WS-TOTAL-MINUTES - 300
               IF WS-MINUTES-REMAINING > 0
                   COMPUTE WS-TOTAL-BILLING = 70.00 + 
                           WS-MINUTES-REMAINING * 0.09
               ELSE
                   IF WS-MINUTES-REMAINING <= 0
                       COMPUTE WS-TOTAL-BILLING = 70.00
                   END-IF
               END-IF
               COMPUTE WS-TOTAL-BILLING = WS-TOTAL-BILLING +
                       WS-TOTAL-BILLING * 0.21    
           WHEN REG-TYPE = "B"
               COMPUTE WS-MINUTES-REMAINING = WS-TOTAL-MINUTES - 200
               IF WS-MINUTES-REMAINING > 0
                   COMPUTE WS-TOTAL-BILLING = 55.00 + 
                           WS-MINUTES-REMAINING * 0.15
               ELSE
                   COMPUTE WS-TOTAL-BILLING = 55.00
               END-IF
               COMPUTE WS-TOTAL-BILLING = WS-TOTAL-BILLING +
                       WS-TOTAL-BILLING * 0.21    
           WHEN REG-TYPE = "C"
               COMPUTE WS-MINUTES-REMAINING = WS-TOTAL-MINUTES - 100
               IF WS-MINUTES-REMAINING > 0
                   COMPUTE WS-TOTAL-BILLING = 40.00 + 
                           WS-MINUTES-REMAINING * 0.21
               ELSE
                   COMPUTE WS-TOTAL-BILLING = 40.00
               END-IF
               COMPUTE WS-TOTAL-BILLING = WS-TOTAL-BILLING +
                       WS-TOTAL-BILLING * 0.21    
           WHEN REG-TYPE = "D"
               COMPUTE WS-MINUTES-REMAINING = WS-TOTAL-MINUTES - 60
               IF WS-MINUTES-REMAINING > 0
                   COMPUTE WS-TOTAL-BILLING = 28.00 + 
                           WS-MINUTES-REMAINING * 0.29
               ELSE
                   COMPUTE WS-TOTAL-BILLING = 28.00
               END-IF
               COMPUTE WS-TOTAL-BILLING = WS-TOTAL-BILLING +
                       WS-TOTAL-BILLING * 0.21    
           WHEN REG-TYPE = "E"
               COMPUTE WS-MINUTES-REMAINING = WS-TOTAL-MINUTES - 40
               IF WS-MINUTES-REMAINING > 0
                   COMPUTE WS-TOTAL-BILLING = 19.00 + 
                           WS-MINUTES-REMAINING * 0.37
               ELSE
                   COMPUTE WS-TOTAL-BILLING = 19.00
               END-IF
               COMPUTE WS-TOTAL-BILLING = WS-TOTAL-BILLING +
                       WS-TOTAL-BILLING * 0.21    
           END-EVALUATE

           PERFORM 40-TOTALES
              THRU 40-TOTALES-F    
           DISPLAY WS-TOTALS
           .
       32-BILLING-F.
       40-TOTALES. 
           MOVE REG-NAME TO T-NAME
           MOVE REG-ADDRESS TO T-ADDRESS
           MOVE REG-TYPE TO T-TYPE
           EVALUATE TRUE
               WHEN REG-TYPE = "A"
                    MOVE 300 TO T-FREE-MINUTES   
               WHEN REG-TYPE = "B"
                    MOVE 200 TO T-FREE-MINUTES   
               WHEN REG-TYPE = "C"
                    MOVE 100 TO T-FREE-MINUTES   
               WHEN REG-TYPE = "D"
                    MOVE 60 TO T-FREE-MINUTES   
               WHEN REG-TYPE = "E"
                    MOVE 40 TO T-FREE-MINUTES   
           END-EVALUATE  

           IF WS-MINUTES-REMAINING > 0
               MOVE WS-MINUTES-REMAINING TO T-EX-MINUTES
           ELSE
               MOVE ZEROS TO T-EX-MINUTES
           END-IF               
           MOVE WS-TOTAL-BILLING TO T-BILLING
           .           
       40-TOTALES-F. EXIT.
       END PROGRAM E19.