      ******************************************************************
      *ENUNCIADO 
      ******************************************************************
      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NAME-PGM.
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
       SELECT MASTER ASSIGN TO "DAT.txt"
                     FILE STATUS IS FS-STATUS-FILE
                     ORGANIZATION IS LINE SEQUENTIAL. 
       
      ****************************  OUTPUT  ****************************

       DATA DIVISION.
       FILE SECTION.
       FD MASTER.
          01 REG-MASTER.
             05 REG-FLIGHT           PIC X(10).
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
      ************************  CONSTANTS  *****************************
       01 WS-CONSTANTES.
           02 CON-RUTINAS.
              05 CON-RUTINA01  PIC X(08) VALUE 'RUTINA01'.
           02 CON-PARRAFO.
              05 CON-110000-OPEN-SALIDA1      PIC X(30) VALUE 
              '110000-OPEN-SALIDA1           '.
           02 CON-OPERACIONES.
              05 CON-ABRIR     PIC X(15) VALUE 'ABRIR          '.
              05 CON-LEER      PIC X(15) VALUE 'LEER           '.
              05 CON-CERRAR    PIC X(15) VALUE 'CERRAR         '.
              05 CON-GRABAR    PIC X(15) VALUE 'GRABAR         '.
              05 CON-RUTINA    PIC X(15) VALUE 'LLAMAR RUTINA  '.
           02 CON-OBJETOS.
              05 CON-SALIDA1   PIC X(10) VALUE 'SALIDA1   '.
           02 CON-OTROS.
              05 CON-1         PIC 9(01) VALUE 1.
      ************************** TABLES ********************************
      
      **************************  SWITCHES  ****************************
       01 WS-SWITCHES.       
          05 FS-STATUS-FILE                PIC X(02) VALUE "00".
             88 FS-STATUS-FILE-OK                    VALUE "00".
             88 FS-STATUS-FILE-EOF                   VALUE "10".
      ************************** VARIABLES *****************************
       01 WS-VARIABLES.
          02 WS-EJE PIC 9(01).

       01 WS-ERRORES.
           05 WS-ERR-PARRAFO            PIC X(30).
           05 WS-ERR-OBJETO             PIC X(10).
           05 WS-ERR-OPERACION          PIC X(15).
           05 WS-ERR-CODIGO             PIC 9(02).
      ************************** COPYS  ********************************     

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
                                                  
           PERFORM 300000-EXIT                         
              THRU 300000-EXIT-F   
           .                                      
      ******************************************************************
      *                         100000-START         
      ******************************************************************
       100000-START.
           PERFORM 110000-OPEN-MASTER
              THRU 110000-OPEN-MASTER-F                                 

           DISPLAY "INICIO"                      
           .                                      
       100000-START-F. EXIT.
      ******************************************************************
      *                         110000-OPEN-MASTER   
      ******************************************************************
       110000-OPEN-MASTER.                        
           OPEN INPUT MASTER                   
           IF NOT FS-STATUS-FILE-OK
              MOVE CON-110000-OPEN-E1DATOS TO WS-ERR-PARRAFO 
              MOVE CON-E1DATOS             TO WS-ERR-OBJETO 
              MOVE CON-ABRIR               TO WS-ERR-OPERACION 
              MOVE WSS-FILE-STATUS         TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       110000-OPEN-MASTER-F. EXIT.                          
      ******************************************************************
      *                         200000-PROCESS   
      ****************************************************************** 
       200000-PROCESS.
           DISPLAY "PROCESO"
           .         
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         210000-READ-MASTER   
      ******************************************************************      
       210000-READ-MASTER.
           INITIALIZE REG-MASTER
           READ MASTER INTO REG-MASTER
           EVALUATE TRUE
               WHEN FS-STATUS-FILE-OK
                    CONTINUE   
               WHEN FS-STATUS-FILE-EOF
                    CONTINUE
           END-EVALUATE
           .
       210000-READ-MASTER-F. EXIT. 
      ******************************************************************
      *                         300000-EXIT   
      ****************************************************************** 
       300000-EXIT.
           PERFORM 310000-CLOSE-MASTER
              THRU 310000-CLOSE-MASTER-F
           DISPLAY "FIN"
           STOP RUN
           .    
       300000-EXIT-F. EXIT.
      ******************************************************************
      *                         310000-CLOSE-MASTER   
      ****************************************************************** 
       310000-CLOSE-MASTER.
           CLOSE MASTER
           IF NOT FS-STATUS-FILE-OK
               DISPLAY "ERROR AL CERRAR ARCHIVO MASTER " FS-STATUS-FILE
           END-IF
           .
       310000-CLOSE-MASTER-F. EXIT. 
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
       END PROGRAM NAME-PGM.