      ******************************************************************
      *Desarrollar una función que reciba un número natural de 4 
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
      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E19.
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
       SELECT DATOS ASSIGN TO "DAT.txt"
                    FILE STATUS IS FS-STATUS-FILE
                    ORGANIZATION IS LINE SEQUENTIAL. 
       
      ****************************  OUTPUT  ****************************

       DATA DIVISION.
       FILE SECTION.
       FD DATOS.
          01 REG-DATOS.
              05 REG-MOBILE           PIC 9(09).
              05 REG-NAME             PIC X(06).
              05 REG-ADDRESS          PIC X(30).
              05 REG-CONSUMPTION      PIC 9(04).
              05 REG-TYPE             PIC X(01).
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
      ************************  CONSTANTS  *****************************

      ************************** TABLES ********************************

      **************************  SWITCHES  ****************************
       01 WS-SWITCHES.       
          05 FS-STATUS-FILE                PIC X(02) VALUE "00".
             88 FS-STATUS-FILE-OK                    VALUE "00".
             88 FS-STATUS-FILE-EOF                   VALUE "10".
      ************************** VARIABLES *****************************
       01 WS-VARIABLES.
           02 WS-STATUS                PIC X(01).
           02 WS-VAR.                
               05 WS-TOTAL-MINUTES     PIC 9(05).
               05 WS-MINUTES-REMAINING PIC S9(05).
               05 WS-TOTAL-BILLING     PIC 9(05)v99.
               05 WS-TOTAL-CONSUMPTION.
                   10 WS-HOUR          PIC 9(02).     
                   10 WS-MINUTES       PIC 9(02). 
           02 WS-TITLE.
               05 FILLER               PIC X(04) VALUE "NAME".
               05 FILLER               PIC X(04).            
               05 FILLER               PIC X(07) VALUE "ADDRESS".
               05 FILLER               PIC X(24).            
               05 FILLER               PIC X(04) VALUE "TYPE".
               05 FILLER               PIC X(02).            
               05 FILLER               PIC X(12) VALUE "FREE-MINUTES".
               05 FILLER               PIC X(02).            
               05 FILLER               PIC X(10) VALUE "EX-MINUTES".
               05 FILLER               PIC X(02).            
               05 FILLER               PIC X(07) VALUE "BILLING".
               05 FILLER               PIC X(02).            
           02 WS-TOTALS.
               05 T-NAME               PIC X(06).
               05 FILLER               PIC X(02).            
               05 T-ADDRESS            PIC X(30).
               05 FILLER               PIC X(02).            
               05 T-TYPE               PIC X(01).
               05 FILLER               PIC X(08).            
               05 T-FREE-MINUTES       PIC 9(03).
               05 FILLER               PIC X(10).            
               05 T-EX-MINUTES         PIC 9(05).
               05 FILLER               PIC X(03).            
               05 T-BILLING            PIC ZZZ.ZZ.
               05 FILLER               PIC X(02).            
      ******************************************************************
      *                       LINKAGE SECTION   
      ****************************************************************** 
       LINKAGE SECTION.        
      ******************************************************************
      *                      PROCEDURE DIVISION   
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

           DISPLAY WS-TITLE   
           .                                      
       100000-START-F. EXIT.
      ******************************************************************
      *                         110000-OPEN-DATOS   
      ******************************************************************
       110000-OPEN-DATOS.                        
           OPEN INPUT DATOS                   
           IF NOT FS-STATUS-FILE-OK
               DISPLAY "ERROR AL ABRIR ARCHIVO MAESTRO " FS-STATUS-FILE
           END-IF
           .
       110000-OPEN-DATOS-F. EXIT.                          
      ******************************************************************
      *                         200000-PROCESS   
      ****************************************************************** 
       200000-PROCESS.
           PERFORM 220000-TOTAL-MINUTES
              THRU 220000-TOTAL-MINUTES-F       
           
           PERFORM 230000-BILLING
              THRU 230000-BILLING-F       

           PERFORM 210000-READ-DATOS
              THRU 210000-READ-DATOS-F
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
           END-EVALUATE
           .
       210000-READ-DATOS-F. EXIT.    
      ******************************************************************
      *                         220000-TOTAL-MINUTES   
      ******************************************************************      
       220000-TOTAL-MINUTES.
           MOVE REG-CONSUMPTION TO WS-TOTAL-CONSUMPTION
           COMPUTE WS-TOTAL-MINUTES = WS-HOUR * 60 + WS-MINUTES
           .
       220000-TOTAL-MINUTES-F. EXIT.    
      ******************************************************************
      *                         230000-BILLING   
      ******************************************************************      
       230000-BILLING.
           EVALUATE TRUE
           WHEN REG-TYPE = "A"
               COMPUTE WS-MINUTES-REMAINING = WS-TOTAL-MINUTES - 300
               IF WS-MINUTES-REMAINING > 0
                   COMPUTE WS-TOTAL-BILLING = 70,00 + 
                           WS-MINUTES-REMAINING * 0,09
               ELSE
                   IF WS-MINUTES-REMAINING <= 0
                       COMPUTE WS-TOTAL-BILLING = 70,00
                   END-IF
               END-IF
               COMPUTE WS-TOTAL-BILLING = WS-TOTAL-BILLING +
                       WS-TOTAL-BILLING * 0,21    
           WHEN REG-TYPE = "B"
               COMPUTE WS-MINUTES-REMAINING = WS-TOTAL-MINUTES - 200
               IF WS-MINUTES-REMAINING > 0
                   COMPUTE WS-TOTAL-BILLING = 55,00 + 
                           WS-MINUTES-REMAINING * 0,15
               ELSE
                   COMPUTE WS-TOTAL-BILLING = 55,00
               END-IF
               COMPUTE WS-TOTAL-BILLING = WS-TOTAL-BILLING +
                       WS-TOTAL-BILLING * 0,21    
           WHEN REG-TYPE = "C"
               COMPUTE WS-MINUTES-REMAINING = WS-TOTAL-MINUTES - 100
               IF WS-MINUTES-REMAINING > 0
                   COMPUTE WS-TOTAL-BILLING = 40,00 + 
                           WS-MINUTES-REMAINING * 0,21
               ELSE
                   COMPUTE WS-TOTAL-BILLING = 40,00
               END-IF
               COMPUTE WS-TOTAL-BILLING = WS-TOTAL-BILLING +
                       WS-TOTAL-BILLING * 0,21    
           WHEN REG-TYPE = "D"
               COMPUTE WS-MINUTES-REMAINING = WS-TOTAL-MINUTES - 60
               IF WS-MINUTES-REMAINING > 0
                   COMPUTE WS-TOTAL-BILLING = 28,00 + 
                           WS-MINUTES-REMAINING * 0,29
               ELSE
                   COMPUTE WS-TOTAL-BILLING = 28,00
               END-IF
               COMPUTE WS-TOTAL-BILLING = WS-TOTAL-BILLING +
                       WS-TOTAL-BILLING * 0,21    
           WHEN REG-TYPE = "E"
               COMPUTE WS-MINUTES-REMAINING = WS-TOTAL-MINUTES - 40
               IF WS-MINUTES-REMAINING > 0
                   COMPUTE WS-TOTAL-BILLING = 19,00 + 
                           WS-MINUTES-REMAINING * 0,37
               ELSE
                   COMPUTE WS-TOTAL-BILLING = 19,00
               END-IF
               COMPUTE WS-TOTAL-BILLING = WS-TOTAL-BILLING +
                       WS-TOTAL-BILLING * 0,21    
           END-EVALUATE

           PERFORM 320000-TOTAL
              THRU 320000-TOTAL-F

           DISPLAY WS-TOTALS
           .
       230000-BILLING-F. EXIT.    
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           PERFORM 310000-CLOSE-DATOS
              THRU 310000-CLOSE-DATOS-F

           STOP RUN 
           .    
       300000-END-F. EXIT.
      ******************************************************************
      *                         310000-CLOSE-DATOS   
      ****************************************************************** 
       310000-CLOSE-DATOS.
           CLOSE DATOS
           IF NOT FS-STATUS-FILE-OK
               DISPLAY "ERROR AL CERRAR ARCHIVO DATOS " FS-STATUS-FILE
           END-IF
           .
       310000-CLOSE-DATOS-F. EXIT.  
      ******************************************************************
      *                         320000-TOTAL   
      ****************************************************************** 
       320000-TOTAL.
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
       320000-TOTAL-F. EXIT.  

       END PROGRAM E19.