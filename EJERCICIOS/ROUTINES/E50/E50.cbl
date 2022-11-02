      ******************************************************************
      *Generar una rutina para calcular el determinante de
      *una matriz de 2 x 2. Utilizar la siguiente área de 
      *comunicación como guía:
      *01 EDET-AREA.
      *05 EDET-I-AREA.
      *10 EDET-MAT.
      *15 EDET-FIL OCCURS 2 TIMES.
      *20 EDET-COL OCCURS 2 TIMES.
      *25 EDET-NUM PIC 9(02).
      *05 EDET-O-AREA.
      *10 EDET-DET PIC 9(04).
      *05 EDET-S-AREA.
      *10 EDET-COD-RET PIC 9(01).
      *88 EDET-OK VALUE 0.
      *88 EDET-ERROR VALUE 9.
      *10 EDET-REF PIC X(60).
      *Donde se ingresará con el área cargada con la matriz, y 
      *la rutina devolverá el valor del determinante junto con
      *EDET-OK, ó EDET-ERROR con alguna explicación conveniente en 
      *EDET-REF.
      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E50.
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
              05 CON-RUTEDET  PIC X(08) VALUE 'RUTEDET '.
           02 CON-PARRAFO.
              05 CON-100000-START      PIC X(30) VALUE 
              '100000-START                  '.
              05 CON-200000-PROCESS      PIC X(30) VALUE 
              '200000-PROCESS                '.
              05 CON-210000-CALL-RUTDETER     PIC X(30) VALUE 
              '210000-CALL-RUTDETER          '.
              05 CON-300000-EXIT     PIC X(30) VALUE 
              '300000-EXIT                   '.
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
           02 WS-NUM1   PIC S9(02).
           02 WS-NUM2   PIC S9(02).
           02 WS-NUM3   PIC S9(02).
           02 WS-NUM4   PIC S9(02).

       01 WS-ERRORES.
           05 WS-ERR-PARRAFO            PIC X(30).
           05 WS-ERR-OBJETO             PIC X(10).
           05 WS-ERR-OPERACION          PIC X(15).
           05 WS-ERR-CODIGO             PIC 9(02).
      ************************** COPYS  ********************************
       01 WS-REG-RUTEDET.
       COPY EDECERUT.
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
           DISPLAY "INGRESAR LOS VALORES DE LA MATRIZ"
           ACCEPT WS-NUM1
           ACCEPT WS-NUM2
           ACCEPT WS-NUM3
           ACCEPT WS-NUM4

           DISPLAY " | " WS-NUM1 " | " WS-NUM2 " | "  
           DISPLAY " | " WS-NUM3 " | " WS-NUM4 " | "  
           .   
       100000-START-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS   
      ******************************************************************
       200000-PROCESS.
           PERFORM 210000-CALL-RUTDETER
              THRU 210000-CALL-RUTDETER-F
           .           
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         210000-CALL-RUTDETER   
      ******************************************************************
       210000-CALL-RUTDETER.
           MOVE WS-NUM1 TO EDET-NUM(1 , 1)
           MOVE WS-NUM2 TO EDET-NUM(1 , 2)
           MOVE WS-NUM3 TO EDET-NUM(2 , 1)
           MOVE WS-NUM4 TO EDET-NUM(2 , 2)
           CALL CON-RUTEDET USING WS-REG-RUTEDET
           IF EDET-COD-RET = '00'
              DISPLAY "DETERMINANTE : "EDET-DET
              DISPLAY "TIPO DE MATRIZ : " EDET-REF
           ELSE
              MOVE CON-210000-CALL-RUTDETER TO WS-ERR-PARRAFO 
              MOVE CON-RUTEDET              TO WS-ERR-OBJETO 
              MOVE CON-CALL                 TO WS-ERR-OPERACION 
              MOVE EDET-COD-RET             TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .           
       210000-CALL-RUTDETER-F. EXIT.
      ******************************************************************
      *                         300000-EXIT   
      ******************************************************************
       300000-EXIT.
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
       END PROGRAM E50.
