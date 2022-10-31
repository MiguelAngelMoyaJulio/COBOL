      ******************************************************************
      *Realizar un programa que efectúe el 
      *ABMC (alta, baja, modificación y consulta) del archivo
      *CUENTASIX.DAT considerando los errores pertinentes en cada caso.
      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E47.
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

      ****************************  OUTPUT  ****************************
       SELECT DATOS  ASSIGN TO "CUENTASIX.txt"
                       ORGANIZATION IS INDEXED
                       ACCESS MODE  IS RANDOM
                       RECORD KEY   IS REG-NRO-CUENTA-IX
                       FILE STATUS  IS FS-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD DATOS.
          01 REG-CUENTASIX.
             05 REG-NRO-CUENTA-IX        PIC X(08).
             05 REG-COD-CLIENTE-IX       PIC 9(08).
             05 REG-MONTO-CUENTA-IX      PIC S9(15)V9(02).
      ******************************************************************
      *                     WORKING-STORAGE SECTION
      ******************************************************************
       WORKING-STORAGE SECTION.
      ************************  CONSTANTES  ****************************
       01 WS-CONSTANTES.
           02 CON-RUTINAS.
              05 CON-RUTINA01  PIC X(08) VALUE 'RUTINA01'.
           02 CON-PARRAFO.
              05 CON-110000-OPEN-DATOS      PIC X(30) VALUE 
              '110000-OPEN-DATOS           '.
              05 CON-210000-READ-DATOS      PIC X(30) VALUE 
              '210000-READ-DATOS           '.
              05 CON-220000-SELECT-ACCOUNT      PIC X(30) VALUE 
              '220000-SELECT-ACCOUNT       '.
              05 CON-230000-INSERT-ACCOUNT      PIC X(30) VALUE 
              '230000-INSERT-ACCOUNT       '.
              05 CON-240000-DELETE-ACCOUNT      PIC X(30) VALUE 
              '240000-DELETE-ACCOUNT       '.
              05 CON-250000-UPDATE-ACCOUNT      PIC X(30) VALUE 
              '250000-UPDATE-ACCOUNT       '.
              05 CON-310000-CLOSE-DATOS     PIC X(30) VALUE 
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
       01 WS-SWITCHES.
          05 FS-STATUS               PIC X(02) VALUE "00".
             88 FS-STATUS-OK                   VALUE "00".
             88 FS-STATUS-EOF                  VALUE "10".
             88 FS-STATUS-DUP                  VALUE "22".
             88 FS-STATUS-NOT-FOUND            VALUE "23".
      ************************** VARIABLES *****************************
       01 WSV-VARIABLES.
          05 WSV-OPCION      PIC 9(01) VALUE 9.
          05 WSV-CUENTA      PIC X(08).
          05 WSV-CLIENTE     PIC 9(08).
          05 WSV-MONTO       PIC S9(15)V9(02).
          05 WSV-RESPUESTA   PIC X(01).
          05 WSV-EDIT        PIC -ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.

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
             UNTIL WSV-OPCION = 0 

           PERFORM 300000-EXIT
              THRU 300000-EXIT-F
           .
      ******************************************************************
      *                         100000-START
      ******************************************************************
       100000-START.
           PERFORM 110000-OPEN-DATOS
              THRU 110000-OPEN-DATOS-F
           .
       100000-START-F. EXIT.
      ******************************************************************
      *                         110000-OPEN-DATOS
      ******************************************************************
       110000-OPEN-DATOS.
           OPEN I-O DATOS
           IF NOT FS-STATUS-OK
              MOVE CON-110000-OPEN-DATOS   TO WS-ERR-PARRAFO 
              MOVE CON-DATOS               TO WS-ERR-OBJETO 
              MOVE CON-ABRIR               TO WS-ERR-OPERACION 
              MOVE FS-STATUS               TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       110000-OPEN-DATOS-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS
      ******************************************************************
       200000-PROCESS.
           DISPLAY "1.CONSULTAR CUENTA"
           DISPLAY "2.ALTA DE CUENTA"
           DISPLAY "3.BAJA DE CUENTA"
           DISPLAY "4.MODIFICAR CUENTA"
           DISPLAY "0.SALIR"
           ACCEPT WSV-OPCION

           EVALUATE WSV-OPCION
               WHEN 1
                    PERFORM 220000-SELECT-ACCOUNT
                       THRU 220000-SELECT-ACCOUNT-F
               WHEN 2
                    PERFORM 230000-INSERT-ACCOUNT
                       THRU 230000-INSERT-ACCOUNT-F
               WHEN 3
                    PERFORM 240000-DELETE-ACCOUNT
                       THRU 240000-DELETE-ACCOUNT-F
               WHEN 4
                    PERFORM 250000-UPDATE-ACCOUNT
                       THRU 250000-UPDATE-ACCOUNT-F
           END-EVALUATE
           .
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         210000-READ-DATOS
      ******************************************************************
       210000-READ-DATOS.
           READ DATOS
           EVALUATE TRUE
               WHEN FS-STATUS-OK
                    CONTINUE   
               WHEN FS-STATUS-EOF
                    CONTINUE   
      *        WHEN OTHER
      *             MOVE CON-210000-READ-DATOS   TO WS-ERR-PARRAFO 
      *             MOVE CON-DATOS               TO WS-ERR-OBJETO 
      *             MOVE CON-LEER                TO WS-ERR-OPERACION 
      *             MOVE FS-STATUS               TO WS-ERR-CODIGO
      *             PERFORM 399999-END-PROGRAM
      *                THRU 399999-END-PROGRAM-F     
           END-EVALUATE
           .
       210000-READ-DATOS-F. EXIT.
      ******************************************************************
      *                         220000-SELECT-ACCOUNT
      ******************************************************************
       220000-SELECT-ACCOUNT.
           DISPLAY "INGRESE EL NUMERO DE CUENTA"
           ACCEPT WSV-CUENTA
           MOVE WSV-CUENTA TO REG-NRO-CUENTA-IX

           PERFORM 210000-READ-DATOS
              THRU 210000-READ-DATOS-F
           
           IF FS-STATUS-OK
              DISPLAY "CUENTA ENCONTRADA :)"   
              DISPLAY "CUENTA : " WSV-CUENTA   
              DISPLAY "CODIGO DE CLIENTE : " REG-COD-CLIENTE-IX   
              MOVE REG-MONTO-CUENTA-IX TO WSV-EDIT   
              DISPLAY "MONTO DISPONIBLE : " WSV-EDIT   
           ELSE
              IF FS-STATUS-NOT-FOUND
                 DISPLAY "NO ENCONTRADO"   
              ELSE
                 MOVE CON-220000-SELECT-ACCOUNT   TO WS-ERR-PARRAFO 
                 MOVE CON-DATOS                   TO WS-ERR-OBJETO 
                 MOVE CON-LEER                    TO WS-ERR-OPERACION 
                 MOVE FS-STATUS                   TO WS-ERR-CODIGO
                 PERFORM 399999-END-PROGRAM
                    THRU 399999-END-PROGRAM-F  
              END-IF  
           END-IF
           .
       220000-SELECT-ACCOUNT-F. EXIT.
      ******************************************************************
      *                         230000-INSERT-ACCOUNT
      ******************************************************************
       230000-INSERT-ACCOUNT.
           INITIALIZE WSV-CUENTA
           INITIALIZE WSV-CLIENTE
           INITIALIZE WSV-MONTO

           DISPLAY "INGRESE EL NUMERO DE CUENTA"
           ACCEPT WSV-CUENTA
           DISPLAY "INGRESE CODIGO DE CLIENTE"
           ACCEPT WSV-CLIENTE
           DISPLAY "INGRESE EL MONTO"
           ACCEPT WSV-MONTO

           MOVE WSV-CUENTA  TO REG-NRO-CUENTA-IX        
           MOVE WSV-CLIENTE TO REG-COD-CLIENTE-IX       
           MOVE WSV-MONTO       TO REG-MONTO-CUENTA-IX

           WRITE REG-CUENTASIX

           IF FS-STATUS-OK
              DISPLAY "ALTA EXITOSA!" 
           ELSE
             IF FS-STATUS-DUP
                DISPLAY "ESTA CUENTA YA ESTA REGISTRADA ;)"
             ELSE
                MOVE CON-230000-INSERT-ACCOUNT   TO WS-ERR-PARRAFO 
                MOVE CON-DATOS                   TO WS-ERR-OBJETO 
                MOVE CON-LEER                    TO WS-ERR-OPERACION 
                MOVE FS-STATUS                   TO WS-ERR-CODIGO
                PERFORM 399999-END-PROGRAM
                   THRU 399999-END-PROGRAM-F
             END-IF
           END-IF
           .
       230000-INSERT-ACCOUNT-F. EXIT.
      ******************************************************************
      *                         240000-DELETE-ACCOUNT
      ******************************************************************
       240000-DELETE-ACCOUNT.
           INITIALIZE WSV-CUENTA
           DISPLAY "INGRESE EL NUMERO DE CUENTA"
           ACCEPT WSV-CUENTA
           
           MOVE WSV-CUENTA TO REG-NRO-CUENTA-IX
           PERFORM 210000-READ-DATOS
              THRU 210000-READ-DATOS-F
           IF FS-STATUS-OK
              DISPLAY "CUENTA ENCONTRADA :)"   
              DISPLAY "CUENTA : " WSV-CUENTA   
              DISPLAY "CODIGO DE CLIENTE : " REG-COD-CLIENTE-IX   
              MOVE REG-MONTO-CUENTA-IX TO WSV-EDIT   
              DISPLAY "MONTO DISPONIBLE : " WSV-EDIT   

              DISPLAY "CONFIRMA ELIMINACION S/N"
              ACCEPT WSV-RESPUESTA

              IF WSV-RESPUESTA = "S"
                 INITIALIZE REG-CUENTASIX
                 MOVE WSV-CUENTA    TO REG-NRO-CUENTA-IX
                 
                 DELETE DATOS
                 IF FS-STATUS-OK
                    DISPLAY "CUENTA ELIMINADA! " 
                 ELSE
                    MOVE CON-240000-DELETE-ACCOUNT   TO WS-ERR-PARRAFO 
                    MOVE CON-DATOS                   TO WS-ERR-OBJETO 
                    MOVE CON-LEER                    TO WS-ERR-OPERACION 
                    MOVE FS-STATUS                   TO WS-ERR-CODIGO
                    PERFORM 399999-END-PROGRAM
                       THRU 399999-END-PROGRAM-F
                 END-IF
              END-IF
           ELSE
              IF FS-STATUS-NOT-FOUND
                 DISPLAY "CUENTA NO ENCONTRADA ;)"  
              END-IF
           END-IF   
           .
       240000-DELETE-ACCOUNT-F. EXIT.
      ******************************************************************
      *                         250000-UPDATE-ACCOUNT
      ******************************************************************
       250000-UPDATE-ACCOUNT.
           INITIALIZE WSV-CUENTA
           DISPLAY "INGRESE EL NUMERO DE CUENTA"
           ACCEPT WSV-CUENTA
           
           MOVE WSV-CUENTA TO REG-NRO-CUENTA-IX
           PERFORM 210000-READ-DATOS
              THRU 210000-READ-DATOS-F
           IF FS-STATUS-OK
              DISPLAY "CUENTA ENCONTRADA :)"   
              DISPLAY "CUENTA : " WSV-CUENTA   
              DISPLAY "CODIGO DE CLIENTE : " REG-COD-CLIENTE-IX   
              MOVE REG-MONTO-CUENTA-IX TO WSV-EDIT   
              DISPLAY "MONTO DISPONIBLE : " WSV-EDIT   

              INITIALIZE WSV-CLIENTE
              INITIALIZE WSV-MONTO

              DISPLAY "INGRESE EL NUEVO CODIGO DE CLIENTE"
              ACCEPT WSV-CLIENTE
              DISPLAY "INGRESE EL NUEVO MONTO"
              ACCEPT WSV-MONTO
              
              INITIALIZE REG-CUENTASIX
              MOVE WSV-CUENTA  TO REG-NRO-CUENTA-IX        
              MOVE WSV-CLIENTE TO REG-COD-CLIENTE-IX       
              MOVE WSV-MONTO       TO REG-MONTO-CUENTA-IX
           
              REWRITE REG-CUENTASIX
              IF FS-STATUS-OK
                 DISPLAY "ACTUALIZACION EXITOSA ;)! " 
              ELSE
                 MOVE CON-250000-UPDATE-ACCOUNT   TO WS-ERR-PARRAFO 
                 MOVE CON-DATOS                   TO WS-ERR-OBJETO 
                 MOVE CON-LEER                    TO WS-ERR-OPERACION 
                 MOVE FS-STATUS                   TO WS-ERR-CODIGO
                 PERFORM 399999-END-PROGRAM
                    THRU 399999-END-PROGRAM-F
              END-IF

           ELSE
              IF FS-STATUS-NOT-FOUND
                 DISPLAY "CUENTA NO ENCONTRADA :("  
              END-IF
           END-IF   
           .
       250000-UPDATE-ACCOUNT-F. EXIT.
      ******************************************************************
      *                         300000-EXIT
      ******************************************************************
       300000-EXIT.
           PERFORM 310000-CLOSE-DATOS
              THRU 310000-CLOSE-DATOS-F
           STOP RUN
           .
       300000-EXIT-F. EXIT.
      ******************************************************************
      *                         310000-CLOSE-DATOS
      ******************************************************************
       310000-CLOSE-DATOS.
           CLOSE DATOS
           IF NOT FS-STATUS-OK
              MOVE CON-310000-CLOSE-DATOS   TO WS-ERR-PARRAFO 
              MOVE CON-DATOS                TO WS-ERR-OBJETO 
              MOVE CON-CERRAR               TO WS-ERR-OPERACION 
              MOVE FS-STATUS                TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       310000-CLOSE-DATOS-F. EXIT.
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
       END PROGRAM E47.
