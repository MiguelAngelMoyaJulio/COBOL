      ******************************************************************
      *Realizar un programa que pida al usuario que ingrese una frase, 
      *luego, que le pida que ingrese una palabra, en
      *este punto el programa debe informar si esa palabra se usa o no 
      *en la frase y cuantas veces es utilizada. Si la
      *palabra se usaba en la frase, pedir al usuario la palabra por 
      *la que debe ser reemplazada, y mostrar la frase con
      *el reemplazo realizado.
      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E49.
       AUTHOR. MIGUEL MOYA.
       DATE-WRITTEN. SEPTEMBER 2022.
       DATE-COMPILED. SEPTEMBER 2022.
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
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
      ************************  CONSTANTS  *****************************
       01 WS-CONSTANTES.
           02 CON-RUTINAS.
              05 CON-RUTSTRIN  PIC X(08) VALUE 'RUTSTRIN'.
           02 CON-PARRAFO.
              05 CON-110000-OPEN-SALIDA1      PIC X(30) VALUE 
              '110000-OPEN-SALIDA1           '.
              05 CON-200000-PROCESS     PIC X(30) VALUE 
              '200000-PROCESS                '.
              05 CON-210000-CALL-RUTSTRING     PIC X(30) VALUE 
              '210000-CALL-RUTSTRING         '.
              05 CON-220000-WRITE-SALIDA1     PIC X(30) VALUE 
              '220000-WRITE-SALIDA1          '.
              05 CON-310000-CLOSE-SALIDA1     PIC X(30) VALUE 
              '310000-CLOSE-SALIDA1          '.
           02 CON-OPERACIONES.
              05 CON-ABRIR     PIC X(15) VALUE 'ABRIR          '.
              05 CON-LEER      PIC X(15) VALUE 'LEER           '.
              05 CON-CERRAR    PIC X(15) VALUE 'CERRAR         '.
              05 CON-GRABAR    PIC X(15) VALUE 'GRABAR         '.
              05 CON-CALL      PIC X(15) VALUE 'LLAMAR RUTINA  '.
           02 CON-OBJETOS.
              05 CON-SALIDA1   PIC X(10) VALUE 'SALIDA1   '.
      ************************** TABLES ******************************** 
           
      **************************  SWITCHES  ****************************
      
      ************************** VARIABLES *****************************
       01 WS-VARIABLES.
           02 WS-FRASE           PIC X(250). 
           02 WS-PALABRA1. 
              05 WS-PAL          PIC X(23). 
              05 WS-PAL-LEN      PIC 9(03). 
              05 WS-PAL-POS1     PIC 9(03). 
              05 WS-PAL-POS2     PIC 9(03). 
           02 WS-PALABRA2. 
              05 WS-PAL2         PIC X(23). 
              05 WS-PAL2-LEN     PIC 9(03). 
              05 WS-PAL2-POS1    PIC 9(03). 
              05 WS-PAL2-POS2    PIC 9(03). 
           02 WS-INDEX           PIC 9(03). 
           02 WS-J               PIC 9(03). 

       01 WS-ERRORES.
           05 WS-ERR-PARRAFO     PIC X(30).
           05 WS-ERR-OBJETO      PIC X(10).
           05 WS-ERR-OPERACION   PIC X(15).
           05 WS-ERR-CODIGO      PIC 9(02).
      ************************** COPYS  ********************************
       01 WS-REG-RUTSTRIN.
       COPY STRCERUT.
      ******************************************************************
      *                       LINKAGE SECTION   
      ****************************************************************** 

      ******************************************************************
      *                         PROCEDURE DIVISION   
      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM 100000-START
              THRU 100000-START-F

           PERFORM 200000-PROCESS
              THRU 200000-PROCESS-F

           PERFORM 300000-EXIT
              THRU 300000-EXIT-F
           .
      ******************************************************************
      *                         100000-START   
      ******************************************************************
       100000-START.
           DISPLAY "INGRESE UNA FRASE"
           ACCEPT WS-FRASE  
              
           DISPLAY "INGRESE UNA PALABRA"
           ACCEPT WS-PAL  
           .   
       100000-START-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS   
      ******************************************************************
       200000-PROCESS.
           PERFORM 210000-CALL-RUTSTRING
              THRU 210000-CALL-RUTSTRING-F

           IF STRCERUT-CANT-OCURRENCIAS > 0
              DISPLAY "LA PALABRA APARECE : " STRCERUT-CANT-OCURRENCIAS 
              DISPLAY "INGRESE LA NUEVA PALABRA"
              ACCEPT WS-PAL2

              INITIALIZE WS-REG-RUTSTRIN
              MOVE WS-PAL2      TO STRCERUT-PALABRA
              MOVE 02           TO STRCERUT-OPCION
              CALL CON-RUTSTRIN USING WS-REG-RUTSTRIN

              IF STRCERUT-COD-RET = '00'
                 MOVE STRCERUT-POS-INI TO WS-PAL2-POS1
                 MOVE STRCERUT-POS-FIN TO WS-PAL2-POS2
                 MOVE STRCERUT-LEN     TO WS-PAL2-LEN

                 PERFORM VARYING WS-INDEX FROM 1 BY 1
                 UNTIL WS-INDEX > (LENGTH OF WS-FRASE - WS-PAL-LEN)
                   IF WS-FRASE(WS-INDEX:WS-PAL-LEN) = 
                      WS-PAL(WS-PAL-POS1:WS-PAL-LEN)
                      PERFORM VARYING WS-J FROM WS-PAL2-POS1
                      BY 1 UNTIL WS-J > WS-PAL2-LEN
                       DISPLAY WS-PAL2(WS-J:1) WITH NO ADVANCING 
                      END-PERFORM
                      COMPUTE WS-INDEX = WS-INDEX + WS-PAL-LEN - 1   
                   ELSE
                      DISPLAY WS-FRASE(WS-INDEX:1) WITH NO ADVANCING 
                   END-IF
                 END-PERFORM
              ELSE
                 MOVE CON-200000-PROCESS  TO WS-ERR-PARRAFO 
                 MOVE CON-RUTSTRIN        TO WS-ERR-OBJETO 
                 MOVE CON-CALL            TO WS-ERR-OPERACION 
                 MOVE STRCERUT-COD-RET    TO WS-ERR-CODIGO
                 PERFORM 399999-END-PROGRAM
                    THRU 399999-END-PROGRAM-F
              END-IF
           END-IF
           .           
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         210000-CALL-RUTSTRING   
      ******************************************************************
       210000-CALL-RUTSTRING.
           MOVE WS-FRASE    TO STRCERUT-FRASE
           MOVE WS-PAL      TO STRCERUT-PALABRA
           MOVE 01           TO STRCERUT-OPCION
           CALL CON-RUTSTRIN USING WS-REG-RUTSTRIN
           IF STRCERUT-COD-RET = '00'
              MOVE STRCERUT-POS-INI TO WS-PAL-POS1
              MOVE STRCERUT-POS-FIN TO WS-PAL-POS2
              MOVE STRCERUT-LEN     TO WS-PAL-LEN
           ELSE
              MOVE CON-210000-CALL-RUTSTRING   TO WS-ERR-PARRAFO 
              MOVE CON-RUTSTRIN                TO WS-ERR-OBJETO 
              MOVE CON-CALL                    TO WS-ERR-OPERACION 
              MOVE STRCERUT-COD-RET            TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .   
       210000-CALL-RUTSTRING-F. EXIT.
      ******************************************************************
      *                         300000-EXIT   
      ******************************************************************
       300000-EXIT.
           DISPLAY " "  
           DISPLAY "FIN PROGRAMA"  
           STOP RUN   
           .   
       300000-EXIT-F. EXIT.
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
       END PROGRAM E49.
