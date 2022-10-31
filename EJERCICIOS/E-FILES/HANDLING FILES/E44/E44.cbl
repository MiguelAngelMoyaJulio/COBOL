      ******************************************************************
      *Realizar un programa que genere un listado por pantalla del 
      *archivo ‘CUENTAS.DAT’ con el siguiente formato:
      **-----------------*------------*---------------------------*
      *| NRO.CUENTA | COD.CLI.| DISPONIBLE CUENTA|
      **-----------------*------------*---------------------------*
      *| 99-99999-9|99999999|-999999999999999,99|
      *| 99-99999-9|99999999|-999999999999999,99|
      *| 99-99999-9|99999999|-999999999999999,99|
      *| 99-99999-9|99999999|-999999999999999,99|
      *| 99-99999-9|99999999|-999999999999999,99|
      **-----------------*------------*---------------------------*
      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E44.
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
       SELECT DATOS ASSIGN TO "CUENTAS.txt"
                     ACCESS MODE  IS SEQUENTIAL 
                     FILE STATUS  IS FS-STATUS
                     ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD DATOS.
          01 REG-DATOS.   
             05 REG-NRO-CUENTA           PIC X(08).
             05 REG-COD-CLIENTE          PIC 9(08).
             05 REG-MONTO-CUENTA         PIC S9(15)V9(02).    
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
       01 WS-SWITCHES.       
          05 FS-STATUS               PIC X(02) VALUE "00".
             88 FS-STATUS-OK                   VALUE "00".
             88 FS-STATUS-EOF                  VALUE "10".
      ************************** VARIABLES *****************************
       01 WSV-AST.   
           02 FILLER PIC X(01) VALUE "*".   
           02 FILLER PIC X(12) VALUE ALL "_".   
           02 FILLER PIC X(01) VALUE "*".   
           02 FILLER PIC X(09) VALUE ALL "_".   
           02 FILLER PIC X(01) VALUE "*".   
           02 FILLER PIC X(23) VALUE ALL "_".   
           02 FILLER PIC X(01) VALUE "*".   
       01 WSV-TITULO.   
           02 FILLER PIC X(01) VALUE "|".   
           02 FILLER PIC X(12) VALUE " NRO.CUENTA ".   
           02 FILLER PIC X(01) VALUE "|".   
           02 FILLER PIC X(09) VALUE " COD.CLI ".   
           02 FILLER PIC X(01) VALUE "|".   
           02 FILLER PIC X(23) VALUE " DISPONIBLE CUENTA     ".   
           02 FILLER PIC X(01) VALUE "|".   
       01 WSV-SUBTITULO.   
           02 FILLER      PIC X(03) VALUE "|  ".   
           02 SUB-CUENTA1 PIC X(02).   
           02 FILLER      PIC X(01) VALUE "-".   
           02 SUB-CUENTA2 PIC X(05).   
           02 FILLER      PIC X(01) VALUE "-".   
           02 SUB-CUENTA3 PIC X(01).   
           02 FILLER      PIC X(01) VALUE "|".   
           02 SUB-COD-CLI PIC X(08).   
           02 FILLER      PIC X(02) VALUE" |".   
           02 SUB-MONTO   PIC -ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.   
           02 FILLER      PIC X(01) VALUE "|".   

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
           DISPLAY WSV-AST                                                
           PERFORM 300000-EXIT                         
              THRU 300000-EXIT-F   
           .
           STOP RUN.                                      
      ******************************************************************
      *                         100000-START         
      ******************************************************************
       100000-START.                                 
           PERFORM 110000-OPEN-DATOS                
              THRU 110000-OPEN-DATOS-F
           
           PERFORM 210000-READ-DATOS                
              THRU 210000-READ-DATOS-F

           DISPLAY WSV-AST
           DISPLAY WSV-TITULO
           DISPLAY WSV-AST
           .                                      
       100000-START-F. EXIT.                         
      ******************************************************************
      *                         110000-OPEN-DATOS   
      ******************************************************************
       110000-OPEN-DATOS.                        
           OPEN INPUT DATOS                   
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
           INITIALIZE WSV-SUBTITULO
           MOVE REG-NRO-CUENTA(1:2) TO SUB-CUENTA1
           MOVE REG-NRO-CUENTA(3:5) TO SUB-CUENTA2
           MOVE REG-NRO-CUENTA(8:1) TO SUB-CUENTA3
           MOVE REG-COD-CLIENTE     TO SUB-COD-CLI
           MOVE REG-MONTO-CUENTA    TO SUB-MONTO
           DISPLAY WSV-SUBTITULO
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
               WHEN FS-STATUS-OK
                    CONTINUE
               WHEN FS-STATUS-EOF
                    CONTINUE
               WHEN OTHER
                    MOVE CON-210000-READ-DATOS   TO WS-ERR-PARRAFO 
                    MOVE CON-DATOS               TO WS-ERR-OBJETO 
                    MOVE CON-LEER                TO WS-ERR-OPERACION 
                    MOVE FS-STATUS               TO WS-ERR-CODIGO
                    PERFORM 399999-END-PROGRAM
                       THRU 399999-END-PROGRAM-F     
           END-EVALUATE
           .
       210000-READ-DATOS-F. EXIT.
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
       END PROGRAM E44.