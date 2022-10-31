      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E45.
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
       SELECT CUENTAS ASSIGN TO "CUENTAS.txt"
                      ACCESS MODE  IS SEQUENTIAL 
                      FILE STATUS  IS FS-STATUS
                      ORGANIZATION IS LINE SEQUENTIAL.

      ****************************  OUTPUT  ****************************
       SELECT CUENTASIX ASSIGN TO "CUENTASIX.txt"
                        ORGANIZATION IS INDEXED
                        ACCESS MODE  IS RANDOM
                        RECORD KEY   IS REG-NRO-CUENTA-IX
                        FILE STATUS  IS FS-STATUS2.

       DATA DIVISION.
       FILE SECTION.
       FD CUENTAS.
          01 REG-CUENTAS.               
             05 REG-NRO-CUENTA           PIC X(08).
             05 REG-COD-CLIENTE          PIC 9(08).
             05 REG-MONTO-CUENTA         PIC S9(15)V9(02).

       FD CUENTASIX.
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
              05 CON-110000-OPEN-CUENTASIX      PIC X(30) VALUE 
              '110000-OPEN-CUENTASIX       '.
              05 CON-120000-OPEN-CUENTAS      PIC X(30) VALUE 
              '120000-OPEN-CUENTAS         '.
              05 CON-210000-READ-CUENTAS      PIC X(30) VALUE 
              '210000-READ-CUENTAS         '.
              05 CON-220000-WRITE-CUENTASIX      PIC X(30) VALUE 
              '220000-WRITE-CUENTASIX      '.
              05 CON-310000-CLOSE-CUENTAS      PIC X(30) VALUE 
              '310000-CLOSE-CUENTAS        '.
              05 CON-320000-CLOSE-CUENTASIX      PIC X(30) VALUE 
              '320000-CLOSE-CUENTASIX      '.
           02 CON-OPERACIONES.
              05 CON-ABRIR     PIC X(15) VALUE 'ABRIR          '.
              05 CON-LEER      PIC X(15) VALUE 'LEER           '.
              05 CON-CERRAR    PIC X(15) VALUE 'CERRAR         '.
              05 CON-GRABAR    PIC X(15) VALUE 'GRABAR         '.
              05 CON-RUTINA    PIC X(15) VALUE 'LLAMAR RUTINA  '.
           02 CON-OBJETOS.
              05 CON-CUENTAS   PIC X(10) VALUE 'CUENTAS   '.
              05 CON-CUENTASIX PIC X(10) VALUE 'CUENTASIX '.
           02 CON-OTROS.
              05 CON-1         PIC 9(01) VALUE 1.
      ************************** TABLES ********************************

      **************************  SWITCHES  ****************************
       01 WS-SWITCHES.
          05 FS-STATUS               PIC X(02) VALUE "00".
             88 FS-STATUS-OK                   VALUE "00".
             88 FS-STATUS-EOF                  VALUE "10".
             
          05 FS-STATUS2              PIC X(02) VALUE "00".
             88 FS-STATUS2-OK                  VALUE "00".
             88 FS-STATUS2-EOF                 VALUE "10".
      ************************** VARIABLES *****************************
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
             UNTIL FS-STATUS-EOF  

           PERFORM 300000-END
              THRU 300000-END-F
           .
      ******************************************************************
      *                         100000-START
      ******************************************************************
       100000-START.
           PERFORM 110000-OPEN-CUENTASIX
              THRU 110000-OPEN-CUENTASIX-F
           
           PERFORM 120000-OPEN-CUENTAS
              THRU 120000-OPEN-CUENTAS-F
           
           PERFORM 210000-READ-CUENTAS
              THRU 210000-READ-CUENTAS-F
           .
       100000-START-F. EXIT.
      ******************************************************************
      *                         110000-OPEN-CUENTASIX
      ******************************************************************
       110000-OPEN-CUENTASIX.
           OPEN OUTPUT CUENTASIX
           IF NOT FS-STATUS2-OK
              MOVE CON-110000-OPEN-CUENTASIX   TO WS-ERR-PARRAFO 
              MOVE CON-CUENTASIX               TO WS-ERR-OBJETO 
              MOVE CON-ABRIR                   TO WS-ERR-OPERACION 
              MOVE FS-STATUS2                  TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       110000-OPEN-CUENTASIX-F. EXIT.
      ******************************************************************
      *                         120000-OPEN-CUENTAS
      ******************************************************************
       120000-OPEN-CUENTAS.
           OPEN INPUT CUENTAS
           IF NOT FS-STATUS-OK
              MOVE CON-120000-OPEN-CUENTAS   TO WS-ERR-PARRAFO 
              MOVE CON-CUENTAS               TO WS-ERR-OBJETO 
              MOVE CON-ABRIR                 TO WS-ERR-OPERACION 
              MOVE FS-STATUS                 TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       120000-OPEN-CUENTAS-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS
      ******************************************************************
       200000-PROCESS.
           PERFORM 220000-WRITE-CUENTASIX
              THRU 220000-WRITE-CUENTASIX-F
           
           PERFORM 210000-READ-CUENTAS
              THRU 210000-READ-CUENTAS-F
           .
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         210000-READ-CUENTAS   
      ******************************************************************      
       210000-READ-CUENTAS.
           READ CUENTAS INTO REG-CUENTAS
           EVALUATE TRUE
               WHEN FS-STATUS-OK
                    CONTINUE
               WHEN FS-STATUS-EOF
                    CONTINUE
               WHEN OTHER
                    MOVE CON-210000-READ-CUENTAS TO WS-ERR-PARRAFO 
                    MOVE CON-CUENTAS             TO WS-ERR-OBJETO 
                    MOVE CON-LEER                TO WS-ERR-OPERACION 
                    MOVE FS-STATUS               TO WS-ERR-CODIGO
                    PERFORM 399999-END-PROGRAM
                       THRU 399999-END-PROGRAM-F     
           END-EVALUATE
           .
       210000-READ-CUENTAS-F. EXIT. 
      ******************************************************************
      *                         220000-WRITE-CUENTASIX
      ******************************************************************
       220000-WRITE-CUENTASIX.
           INITIALIZE REG-CUENTASIX
           MOVE REG-CUENTAS         TO REG-CUENTASIX
           WRITE REG-CUENTASIX
           IF NOT FS-STATUS2-OK
              MOVE CON-220000-WRITE-CUENTASIX TO WS-ERR-PARRAFO 
              MOVE CON-CUENTASIX              TO WS-ERR-OBJETO 
              MOVE CON-GRABAR                 TO WS-ERR-OPERACION 
              MOVE FS-STATUS2                 TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       220000-WRITE-CUENTASIX-F. EXIT.
      ******************************************************************
      *                         300000-END
      ******************************************************************
       300000-END.
           PERFORM 310000-CLOSE-CUENTAS
              THRU 310000-CLOSE-CUENTAS-F

           PERFORM 320000-CLOSE-CUENTASIX
              THRU 320000-CLOSE-CUENTASIX-F
           STOP RUN
           .
       300000-END-F. EXIT.
      ******************************************************************
      *                         310000-CLOSE-CUENTAS
      ******************************************************************
       310000-CLOSE-CUENTAS.
           CLOSE CUENTAS
           IF NOT FS-STATUS-OK
              MOVE CON-310000-CLOSE-CUENTAS TO WS-ERR-PARRAFO 
              MOVE CON-CUENTAS              TO WS-ERR-OBJETO 
              MOVE CON-CERRAR               TO WS-ERR-OPERACION 
              MOVE FS-STATUS                TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       310000-CLOSE-CUENTAS-F. EXIT.
      ******************************************************************
      *                         320000-CLOSE-CUENTASIX
      ******************************************************************
       320000-CLOSE-CUENTASIX.
           CLOSE CUENTASIX
           IF NOT FS-STATUS2-OK
              MOVE CON-320000-CLOSE-CUENTASIX TO WS-ERR-PARRAFO 
              MOVE CON-CUENTASIX              TO WS-ERR-OBJETO 
              MOVE CON-CERRAR                 TO WS-ERR-OPERACION 
              MOVE FS-STATUS2                 TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       320000-CLOSE-CUENTASIX-F. EXIT.
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
       END PROGRAM E45.
