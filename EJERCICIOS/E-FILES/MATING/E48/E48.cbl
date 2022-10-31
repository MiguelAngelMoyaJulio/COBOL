      ******************************************************************
      *Hacer un programa que realice un apareo entre el archivo CUENTAS 
      *SERVICIOS por numero de cliente. Para
      *cada cliente restar el monto total de sus servicios al saldo de 
      *u cuenta si y solo si dispone de un saldo mayor o
      *igual almonto total de sus servicios 
      *la suma total de los servicios para ese cliente),grabar el 
      *registro en el archivo
      *deCUENTAScon el saldo actualizado, en caso de queel total 
      demontos de serviciosupere al saldo deCUENTAS
      *para ese cliente, grabar el registro del cliente en el archivo
      *RECHAZOS con el siguiente formato:
      *RECHAZOS
      *Nro. Cliente | Nombre| Saldo actual de la cuenta | Imp. Deuda.
      *Si un nro. de cliente del archivo CUENTAS no se encuentra en 
      *el archivo SERVICIOS, grabar el registro en el
      *archivo INCIDENCIAS con el siguiente formato:
      *INCIDENCIAS
      *Nro. Cliente | Tabla Ausencia.
      *En el campo “Tabla Ausencia” se grabará el nombre de la tabla
      *en donde no se registra el dato.
      *En el caso de que un nro. de cliente de la tabla SERVICIOS 
      *no se encuentre en la tabla CUENTAS, se grabara
      *un registro en la tabla INCIDENCIAS con el formato indicado 
      *anteriormente.
      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E48.
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
       SELECT CUENTA ASSIGN TO "CUENTAS.txt"
                     ORGANIZATION IS LINE SEQUENTIAL
                     FILE STATUS IS WS-FS-STATUS1.
       
       SELECT SERVICIO ASSIGN TO "SERVICIOS.txt"
                       FILE STATUS IS WS-FS-STATUS2
                       ORGANIZATION IS LINE SEQUENTIAL.
      ****************************  OUTPUT  **************************** 
       SELECT RECHA  ASSIGN TO "RECHAZADOS.txt"
                     FILE STATUS IS WS-FS-STATUS3
                     ORGANIZATION IS LINE SEQUENTIAL.

       SELECT INCIDENCIA ASSIGN TO "INCIDENCIA.txt"
                     FILE STATUS IS WS-FS-STATUS4
                     ORGANIZATION IS LINE SEQUENTIAL.
       
       SELECT CUENTA-ACT ASSIGN TO "CUENTAS_ACTUALIZADAS.txt"
                     FILE STATUS IS WS-FS-STATUS5
                     ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD CUENTA.
          01 REG-CUENTA.
             05 REG-CLIENTE-M           PIC 9(08).
             05 REG-NOMBRE-M            PIC X(30).
             05 REG-SALDO-M             PIC 9(15)V9(02).
       
       FD SERVICIO.
          01 REG-SERVICIO.
             05 REG-CLIENTE-N           PIC 9(08).
             05 REG-SERVICIO-N          PIC X(03).
             05 REG-MONTO-N             PIC 9(15)V9(02).

       FD RECHA.
          01 REG-RECHA.              
             05 REG-NRO-CLI-R           PIC 9(08).
             05 REG-NOMBRE-R            PIC X(30).
             05 REG-SALDO-ACTUAL-R      PIC 9(15)V9(02).
             05 REG-IMPORTE-DEUDA-R     PIC 9(15)V9(02).
       
       FD INCIDENCIA.
          01 REG-INCIDENCIA.              
             05 REG-NRO-CLI-I           PIC 9(08).
             05 REG-NOMBRE-TABLA-I      PIC X(30).

       FD CUENTA-ACT.      
          01 REG-CUENTA-ACT.   
             05 REG-CLIENTE-A           PIC 9(08).
             05 REG-NOMBRE-A            PIC X(30).
             05 REG-SALDO-A             PIC 9(15)V9(02).   
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
      ************************  CONSTANTS  *****************************
       01 WS-CONSTANTES.
           02 CON-RUTINAS.
              05 CON-RUTINA01  PIC X(08) VALUE 'RUTINA01'.
           02 CON-PARRAFO.
              05 CON-110000-OPEN-CUENTA      PIC X(30) VALUE 
              '110000-OPEN-CUENTA          '.
              05 CON-120000-OPEN-SERVICIO      PIC X(30) VALUE 
              '120000-OPEN-SERVICIO        '.
              05 CON-130000-OPEN-RECHA      PIC X(30) VALUE 
              '130000-OPEN-RECHA           '.
              05 CON-140000-OPEN-INCIDENCIA      PIC X(30) VALUE 
              '140000-OPEN-INCIDENCIA      '.
              05 CON-150000-OPEN-CUENTA-ACT      PIC X(30) VALUE 
              '150000-OPEN-CUENTA-ACT      '.
              05 CON-210000-READ-CUENTA      PIC X(30) VALUE 
              '210000-READ-CUENTA          '.
              05 CON-220000-READ-SERVICIO      PIC X(30) VALUE 
              '220000-READ-SERVICIO        '.
              05 CON-230000-WRITE-RECHA      PIC X(30) VALUE 
              '230000-WRITE-RECHA          '.
              05 CON-240000-WRITE-INCIDENCIA      PIC X(30) VALUE 
              '240000-WRITE-INCIDENCIA     '.
              05 CON-250000-WRITE-CUENTA-ACT      PIC X(30) VALUE 
              '250000-WRITE-CUENTA-ACT     '.
              05 CON-310000-CLOSE-CUENTA      PIC X(30) VALUE 
              '310000-CLOSE-CUENTA         '.
              05 CON-320000-CLOSE-SERVICIO      PIC X(30) VALUE 
              '320000-CLOSE-SERVICIO       '.
              05 CON-330000-CLOSE-RECHA      PIC X(30) VALUE 
              '330000-CLOSE-RECHA          '.
              05 CON-330000-CLOSE-INCIDENCIA      PIC X(30) VALUE 
              '330000-CLOSE-INCIDENCIA     '.
              05 CON-340000-CLOSE-CUENTA-ACT      PIC X(30) VALUE 
              '340000-CLOSE-CUENTA-ACT     '.
           02 CON-OPERACIONES.
              05 CON-ABRIR     PIC X(15) VALUE 'ABRIR          '.
              05 CON-LEER      PIC X(15) VALUE 'LEER           '.
              05 CON-CERRAR    PIC X(15) VALUE 'CERRAR         '.
              05 CON-GRABAR    PIC X(15) VALUE 'GRABAR         '.
              05 CON-RUTINA    PIC X(15) VALUE 'LLAMAR RUTINA  '.
           02 CON-OBJETOS.
              05 CON-CUENTA     PIC X(10) VALUE 'CUENTA    '.
              05 CON-SERVICIO   PIC X(10) VALUE 'SERVICIO  '.
              05 CON-RECHAZADO  PIC X(10) VALUE 'RECHAZADO '.
              05 CON-INCIDENCIA PIC X(10) VALUE 'INCIDENCIA'.
              05 CON-CUENTA-ACT PIC X(10) VALUE 'CUENTA ACT'.
           02 CON-OTROS.
              05 CON-1         PIC 9(01) VALUE 1.
      ************************** TABLES ******************************** 

      **************************  SWITCHES  ****************************
       01 WS-SWITCHES.       
          05 WS-FS-STATUS1                    PIC X(02) VALUE "00".
             88 WS-FS-STATUS1-OK                        VALUE "00".
             88 WS-FS-STATUS1-EOF                       VALUE "10".
             
          05 WS-FS-STATUS2                    PIC X(02) VALUE "00".
             88 WS-FS-STATUS2-OK                        VALUE "00".
             88 WS-FS-STATUS2-EOF                       VALUE "10".
             
          05 WS-FS-STATUS3                    PIC X(02) VALUE "00".
             88 WS-FS-STATUS3-OK                        VALUE "00".
             88 WS-FS-STATUS3-EOF                       VALUE "10".

          05 WS-FS-STATUS4                    PIC X(02) VALUE "00".
             88 WS-FS-STATUS4-OK                        VALUE "00".
             88 WS-FS-STATUS4-EOF                       VALUE "10".

          05 WS-FS-STATUS5                    PIC X(02) VALUE "00".
             88 WS-FS-STATUS5-OK                        VALUE "00".
             88 WS-FS-STATUS5-EOF                       VALUE "10".
      ************************** VARIABLES *****************************
       01 WS-VARIABLES.
          02 WS-CODE-M                   PIC 9(08).
          02 WS-CODE-N                   PIC 9(08).
          02 WS-DEUDA                    PIC 9(15)V9(02).
          02 WS-SALDO-ACT                PIC 9(15)V9(02).
          02 WS-CANT-SERVICIOS           PIC 9(02).
          02 WS-TOTALES.
             05 WS-TOT-CUENTAS           PIC 9(02).
             05 WS-TOT-SERVICIOS         PIC 9(02).
             05 WS-TOT-RECHAZADOS        PIC 9(02).
             05 WS-TOT-CUENTAS-ACT       PIC 9(02).
             05 WS-TOT-INCIDENCIAS       PIC 9(02).
             
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
              UNTIL WS-FS-STATUS1-EOF AND WS-FS-STATUS2-EOF                 
                                                  
           PERFORM 300000-EXIT                         
              THRU 300000-EXIT-F                       
           .                                      
      ******************************************************************
      *                         100000-START         
      ******************************************************************
       100000-START.                                 
           PERFORM 110000-OPEN-CUENTA                
              THRU 110000-OPEN-CUENTA-F
           
           PERFORM 120000-OPEN-SERVICIO                
              THRU 120000-OPEN-SERVICIO-F
           
           PERFORM 130000-OPEN-RECHA                
              THRU 130000-OPEN-RECHA-F
           
           PERFORM 140000-OPEN-INCIDENCIA                
              THRU 140000-OPEN-INCIDENCIA-F
           
           PERFORM 150000-OPEN-CUENTA-ACT                
              THRU 150000-OPEN-CUENTA-ACT-F
                            
           PERFORM 210000-READ-CUENTA                       
              THRU 210000-READ-CUENTA-F                     
           
           PERFORM 220000-READ-SERVICIO                       
              THRU 220000-READ-SERVICIO-F  
           .                                      
       100000-START-F. EXIT.                         
      ******************************************************************
      *                         110000-OPEN-CUENTA   
      ******************************************************************
       110000-OPEN-CUENTA.                        
           OPEN INPUT CUENTA                   
           IF NOT WS-FS-STATUS1-OK
              MOVE CON-110000-OPEN-CUENTA  TO WS-ERR-PARRAFO 
              MOVE CON-CUENTA              TO WS-ERR-OBJETO 
              MOVE CON-ABRIR               TO WS-ERR-OPERACION 
              MOVE WS-FS-STATUS1          TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       110000-OPEN-CUENTA-F. EXIT.
      ******************************************************************
      *                         120000-OPEN-SERVICIO   
      ******************************************************************
       120000-OPEN-SERVICIO.                        
           OPEN INPUT SERVICIO                   
           IF NOT WS-FS-STATUS2-OK
              MOVE CON-120000-OPEN-SERVICIO  TO WS-ERR-PARRAFO 
              MOVE CON-SERVICIO              TO WS-ERR-OBJETO 
              MOVE CON-ABRIR                 TO WS-ERR-OPERACION 
              MOVE WS-FS-STATUS2             TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       120000-OPEN-SERVICIO-F. EXIT.
      ******************************************************************
      *                         130000-OPEN-RECHA   
      ******************************************************************
       130000-OPEN-RECHA.                        
           OPEN OUTPUT RECHA                   
           IF NOT WS-FS-STATUS3-OK
              MOVE CON-130000-OPEN-RECHA  TO WS-ERR-PARRAFO 
              MOVE CON-RECHAZADO          TO WS-ERR-OBJETO 
              MOVE CON-ABRIR              TO WS-ERR-OPERACION 
              MOVE WS-FS-STATUS3          TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       130000-OPEN-RECHA-F. EXIT.
      ******************************************************************
      *                         140000-OPEN-INCIDENCIA   
      ******************************************************************
       140000-OPEN-INCIDENCIA.                        
           OPEN OUTPUT INCIDENCIA                   
           IF NOT WS-FS-STATUS4-OK
              MOVE CON-140000-OPEN-INCIDENCIA TO WS-ERR-PARRAFO 
              MOVE CON-INCIDENCIA             TO WS-ERR-OBJETO 
              MOVE CON-ABRIR                  TO WS-ERR-OPERACION 
              MOVE WS-FS-STATUS4              TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       140000-OPEN-INCIDENCIA-F. EXIT.
      ******************************************************************
      *                         150000-OPEN-CUENTA-ACT   
      ******************************************************************
       150000-OPEN-CUENTA-ACT.                        
           OPEN OUTPUT CUENTA-ACT                   
           IF NOT WS-FS-STATUS5-OK
              MOVE CON-150000-OPEN-CUENTA-ACT TO WS-ERR-PARRAFO 
              MOVE CON-CUENTA-ACT             TO WS-ERR-OBJETO 
              MOVE CON-ABRIR                  TO WS-ERR-OPERACION 
              MOVE WS-FS-STATUS5              TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       150000-OPEN-CUENTA-ACT-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS   
      ****************************************************************** 
       200000-PROCESS.
           IF WS-CODE-M = WS-CODE-N
              ADD 1 TO WS-CANT-SERVICIOS
              COMPUTE WS-DEUDA = WS-DEUDA + REG-MONTO-N
              
              PERFORM 220000-READ-SERVICIO                       
                 THRU 220000-READ-SERVICIO-F
           ELSE
               IF WS-CODE-M > WS-CODE-N
                  INITIALIZE REG-INCIDENCIA 
                  MOVE REG-CLIENTE-N TO REG-NRO-CLI-I
                  MOVE "SERVICIOS"   TO REG-NOMBRE-TABLA-I
                  
                  PERFORM 240000-WRITE-INCIDENCIA
                     THRU 240000-WRITE-INCIDENCIA-F
                     
                  PERFORM 220000-READ-SERVICIO                       
                     THRU 220000-READ-SERVICIO-F 
               ELSE
                  IF WS-DEUDA <= REG-SALDO-M AND WS-CANT-SERVICIOS > 0
                     COMPUTE REG-SALDO-M = REG-SALDO-M - WS-DEUDA
                       
                     PERFORM 250000-WRITE-CUENTA-ACT
                        THRU 250000-WRITE-CUENTA-ACT-F   
                  ELSE
                     IF WS-DEUDA > REG-SALDO-M AND 
                        WS-CANT-SERVICIOS > 0
                        PERFORM 230000-WRITE-RECHA
                           THRU 230000-WRITE-RECHA-F
                     ELSE
                        INITIALIZE REG-INCIDENCIA 
                        MOVE REG-CLIENTE-M TO REG-NRO-CLI-I
                        MOVE "CUENTAS"     TO REG-NOMBRE-TABLA-I
                     
                        PERFORM 240000-WRITE-INCIDENCIA
                           THRU 240000-WRITE-INCIDENCIA-F
                     END-IF
                  END-IF

                  INITIALIZE WS-DEUDA
                  INITIALIZE WS-CANT-SERVICIOS
                  
                  PERFORM 210000-READ-CUENTA                       
                     THRU 210000-READ-CUENTA-F
               END-IF        
           END-IF
           .         
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         210000-READ-CUENTA   
      ******************************************************************      
       210000-READ-CUENTA.
           READ CUENTA INTO REG-CUENTA
           EVALUATE TRUE
               WHEN WS-FS-STATUS1-OK
                    ADD 1 TO WS-TOT-CUENTAS
                    MOVE REG-CLIENTE-M TO WS-CODE-M
               WHEN WS-FS-STATUS1-EOF
                    MOVE 99999999      TO WS-CODE-M
               WHEN OTHER
                    MOVE CON-210000-READ-CUENTA  TO WS-ERR-PARRAFO 
                    MOVE CON-CUENTA              TO WS-ERR-OBJETO 
                    MOVE CON-LEER                TO WS-ERR-OPERACION 
                    MOVE WS-FS-STATUS1           TO WS-ERR-CODIGO
                    PERFORM 399999-END-PROGRAM
                       THRU 399999-END-PROGRAM-F     
           END-EVALUATE
           .
       210000-READ-CUENTA-F. EXIT.
      ******************************************************************
      *                         220000-READ-SERVICIO   
      ******************************************************************      
       220000-READ-SERVICIO.
           INITIALIZE REG-SERVICIO
           READ SERVICIO INTO REG-SERVICIO
           EVALUATE TRUE
               WHEN WS-FS-STATUS2-OK
                    ADD 1 TO WS-TOT-SERVICIOS 
                    MOVE REG-CLIENTE-N TO WS-CODE-N
               WHEN WS-FS-STATUS2-EOF
                    MOVE 99999999      TO WS-CODE-N
               WHEN OTHER
                    MOVE CON-220000-READ-SERVICIO TO WS-ERR-PARRAFO 
                    MOVE CON-SERVICIO             TO WS-ERR-OBJETO 
                    MOVE CON-LEER                 TO WS-ERR-OPERACION 
                    MOVE WS-FS-STATUS2            TO WS-ERR-CODIGO
                    PERFORM 399999-END-PROGRAM
                       THRU 399999-END-PROGRAM-F     
           END-EVALUATE
           .
       220000-READ-SERVICIO-F. EXIT.
      ******************************************************************
      *                         230000-WRITE-RECHA   
      ******************************************************************      
       230000-WRITE-RECHA.
           ADD 1 TO WS-TOT-RECHAZADOS
           MOVE REG-CLIENTE-M TO REG-NRO-CLI-R              
           MOVE REG-NOMBRE-M  TO REG-NOMBRE-R               
           MOVE REG-SALDO-M   TO REG-SALDO-ACTUAL-R         
           MOVE WS-DEUDA     TO REG-IMPORTE-DEUDA-R

           WRITE REG-RECHA
           IF NOT WS-FS-STATUS3-OK
              MOVE CON-230000-WRITE-RECHA TO WS-ERR-PARRAFO 
              MOVE CON-RECHAZADO          TO WS-ERR-OBJETO 
              MOVE CON-GRABAR             TO WS-ERR-OPERACION 
              MOVE WS-FS-STATUS3          TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF 
           .
       230000-WRITE-RECHA-F. EXIT.
      ******************************************************************
      *                         240000-WRITE-INCIDENCIA   
      ******************************************************************      
       240000-WRITE-INCIDENCIA.
           ADD 1 TO WS-TOT-INCIDENCIAS
           WRITE REG-INCIDENCIA
           IF NOT WS-FS-STATUS4-OK
              MOVE CON-240000-WRITE-INCIDENCIA TO WS-ERR-PARRAFO 
              MOVE CON-INCIDENCIA              TO WS-ERR-OBJETO 
              MOVE CON-GRABAR                  TO WS-ERR-OPERACION 
              MOVE WS-FS-STATUS4               TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF 
           .
       240000-WRITE-INCIDENCIA-F. EXIT.
      ******************************************************************
      *                         250000-WRITE-CUENTA-ACT   
      ******************************************************************      
       250000-WRITE-CUENTA-ACT.
           ADD 1 TO WS-TOT-CUENTAS-ACT
           MOVE REG-CLIENTE-M TO REG-CLIENTE-A           
           MOVE REG-NOMBRE-M  TO REG-NOMBRE-A             
           MOVE REG-SALDO-M   TO REG-SALDO-A              

           WRITE REG-CUENTA-ACT
           IF NOT WS-FS-STATUS5-OK
              MOVE CON-250000-WRITE-CUENTA-ACT TO WS-ERR-PARRAFO 
              MOVE CON-CUENTA-ACT              TO WS-ERR-OBJETO 
              MOVE CON-GRABAR                  TO WS-ERR-OPERACION 
              MOVE WS-FS-STATUS5               TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF 
           .
       250000-WRITE-CUENTA-ACT-F. EXIT.
      ******************************************************************
      *                         300000-EXIT   
      ****************************************************************** 
       300000-EXIT.
           PERFORM 310000-CLOSE-CUENTA
              THRU 310000-CLOSE-CUENTA-F
           
           PERFORM 320000-CLOSE-SERVICIO
              THRU 320000-CLOSE-SERVICIO-F
              
           PERFORM 330000-CLOSE-RECHA
              THRU 330000-CLOSE-RECHA-F
           
           PERFORM 330000-CLOSE-INCIDENCIA
              THRU 330000-CLOSE-INCIDENCIA-F

           PERFORM 340000-CLOSE-CUENTA-ACT
              THRU 340000-CLOSE-CUENTA-ACT-F

           PERFORM 350000-TOTALES
              THRU 350000-TOTALES-F
           STOP RUN   
           .    
       300000-EXIT-F. EXIT.
      ******************************************************************
      *                         310000-CLOSE-CUENTA   
      ****************************************************************** 
       310000-CLOSE-CUENTA.
           CLOSE CUENTA
           IF NOT WS-FS-STATUS1-OK
              MOVE CON-310000-CLOSE-CUENTA TO WS-ERR-PARRAFO 
              MOVE CON-CUENTA              TO WS-ERR-OBJETO 
              MOVE CON-CERRAR              TO WS-ERR-OPERACION 
              MOVE WS-FS-STATUS1           TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       310000-CLOSE-CUENTA-F. EXIT.
      ******************************************************************
      *                         320000-CLOSE-SERVICIO   
      ****************************************************************** 
       320000-CLOSE-SERVICIO.
           CLOSE SERVICIO
           IF NOT WS-FS-STATUS2-OK
              MOVE CON-320000-CLOSE-SERVICIO TO WS-ERR-PARRAFO 
              MOVE CON-SERVICIO              TO WS-ERR-OBJETO 
              MOVE CON-CERRAR                TO WS-ERR-OPERACION 
              MOVE WS-FS-STATUS2             TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       320000-CLOSE-SERVICIO-F. EXIT.
      ******************************************************************
      *                         330000-CLOSE-RECHA   
      ****************************************************************** 
       330000-CLOSE-RECHA.
           CLOSE RECHA
           IF NOT WS-FS-STATUS3-OK
              MOVE CON-330000-CLOSE-RECHA    TO WS-ERR-PARRAFO 
              MOVE CON-RECHAZADO             TO WS-ERR-OBJETO 
              MOVE CON-CERRAR                TO WS-ERR-OPERACION 
              MOVE WS-FS-STATUS3             TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       330000-CLOSE-RECHA-F. EXIT.
      ******************************************************************
      *                         330000-CLOSE-INCIDENCIA   
      ****************************************************************** 
       330000-CLOSE-INCIDENCIA.
           CLOSE INCIDENCIA
           IF NOT WS-FS-STATUS4-OK
              MOVE CON-330000-CLOSE-INCIDENCIA TO WS-ERR-PARRAFO 
              MOVE CON-INCIDENCIA              TO WS-ERR-OBJETO 
              MOVE CON-CERRAR                  TO WS-ERR-OPERACION 
              MOVE WS-FS-STATUS4               TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       330000-CLOSE-INCIDENCIA-F. EXIT.
      ******************************************************************
      *                         340000-CLOSE-CUENTA-ACT   
      ****************************************************************** 
       340000-CLOSE-CUENTA-ACT.
           CLOSE CUENTA-ACT
           IF NOT WS-FS-STATUS5-OK
              MOVE CON-340000-CLOSE-CUENTA-ACT TO WS-ERR-PARRAFO 
              MOVE CON-CUENTA-ACT              TO WS-ERR-OBJETO 
              MOVE CON-CERRAR                  TO WS-ERR-OPERACION 
              MOVE WS-FS-STATUS5               TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       340000-CLOSE-CUENTA-ACT-F. EXIT.
      ******************************************************************
      *                         350000-TOTALES   
      ****************************************************************** 
       350000-TOTALES.
           DISPLAY "TOTALES DE CONTROL"  
           DISPLAY "CUENTAS LEIDAS : " WS-TOT-CUENTAS 
           DISPLAY "SERVICIOS LEIDOS : " WS-TOT-SERVICIOS 
           DISPLAY "CUENTAS ACTUALIZADAS : " WS-TOT-CUENTAS-ACT 
           DISPLAY "CUENTAS RECHAZADAS : " WS-TOT-RECHAZADOS 
           DISPLAY "INCIDENCIAS : " WS-TOT-INCIDENCIAS 
           .
       350000-TOTALES-F. EXIT.
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
       END PROGRAM E48.