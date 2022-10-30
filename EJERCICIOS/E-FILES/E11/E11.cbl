      ******************************************************************
      * Dados 100 números enteros, informar el promedio de los mayores 
      * que 100 y la suma de los que están entre que 30 y 60.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E11.
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
      *****************************  INPUT  ****************************
       SELECT DATOS ASSIGN TO "DAT.txt"
                    FILE STATUS IS FS-STATUS 
                    ORGANIZATION IS LINE SEQUENTIAL.
       
      ****************************  OUTPUT  ****************************
       
       DATA DIVISION.
       FILE SECTION.
       FD DATOS.
          01 REG-DATOS.
             05 REG-NUM PIC 9(03).
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
      ************************  CONSTANTES  ****************************
       01 WS-CONSTANTES.
           02 CON-RUTINAS.
              05 CON-RUTINA0  PIC X(08) VALUE 'RUTINA01'.
           02 CON-PARRAFO.
              05 CON-110000-OPEN-DATOS      PIC X(30) VALUE 
              '110000-OPEN-DATOS             '.
              05 CON-210000-READ-DATOS     PIC X(30) VALUE 
              '210000-READ-DATOS             '.
              05 CON-310000-CLOSE-DATOS     PIC X(30) VALUE 
              '310000-CLOSE-DATOS            '.
           02 CON-OPERACIONES.
              05 CON-ABRIR     PIC X(15) VALUE 'ABRIR          '.
              05 CON-LEER      PIC X(15) VALUE 'LEER           '.
              05 CON-CERRAR    PIC X(15) VALUE 'CERRAR         '.
              05 CON-GRABAR    PIC X(15) VALUE 'GRABAR         '.
              05 CON-RUTINA    PIC X(15) VALUE 'LLAMAR RUTINA  '.
           02 CON-OBJETOS.
              05 CON-DATOS   PIC X(10) VALUE 'DATOS     '.
           02 CON-OTROS.
              05 CON-1                         PIC 9(01) VALUE 1.
      **************************  SWITCHES  ****************************
       01 WS-SWITCHES.       
          05 FS-STATUS               PIC X(02) VALUE "00".
             88 FS-STATUS-OK                   VALUE "00".
             88 FS-STATUS-EOF                  VALUE "10".
      ************************** VARIABLES *****************************
       01 WS-VARIABLES.
           02 WS-PROM-100      PIC 9(05).
           02 WS-SUM-100       PIC 9(05).
           02 WS-TOT-100       PIC 9(05).
           02 WS-SUM-INT       PIC 9(05).
        
       01 WS-ERRORES.
           05 WS-ERR-PARRAFO   PIC X(30).
           05 WS-ERR-OBJETO    PIC X(10).
           05 WS-ERR-OPERACION PIC X(15).
           05 WS-ERR-CODIGO    PIC 9(02).    
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
           OPEN INPUT DATOS                   
           IF NOT FS-STATUS-OK
              MOVE CON-110000-OPEN-DATOS TO WS-ERR-PARRAFO 
              MOVE CON-DATOS             TO WS-ERR-OBJETO 
              MOVE CON-ABRIR             TO WS-ERR-OPERACION 
              MOVE FS-STATUS             TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F               
           END-IF
           .
       110000-OPEN-DATOS-F. EXIT.                        
      ******************************************************************
      *                         200000-PROCESS   
      ****************************************************************** 
       200000-PROCESS.
           IF REG-NUM > 100
               ADD 1 TO WS-TOT-100
               COMPUTE WS-SUM-100 = WS-SUM-100 + REG-NUM 
           END-IF
       
           IF REG-NUM > 30 AND REG-NUM < 60
              COMPUTE WS-SUM-INT = WS-SUM-INT + REG-NUM 
           END-IF

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
                    MOVE CON-210000-READ-DATOS TO WS-ERR-PARRAFO 
                    MOVE CON-DATOS             TO WS-ERR-OBJETO 
                    MOVE CON-LEER              TO WS-ERR-OPERACION 
                    MOVE FS-STATUS             TO WS-ERR-CODIGO
                    PERFORM 399999-END-PROGRAM
                       THRU 399999-END-PROGRAM-F
           END-EVALUATE
           .
       210000-READ-DATOS-F. EXIT. 
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-EXIT.
           PERFORM 310000-CLOSE-DATOS
              THRU 310000-CLOSE-DATOS-F
           
           PERFORM 320000-TOTAL
              THRU 320000-TOTAL-F

           STOP RUN 
           .    
       300000-EXIT-F. EXIT. 
      ******************************************************************
      *                         310000-CLOSE-DATOS   
      ****************************************************************** 
       310000-CLOSE-DATOS.
           CLOSE DATOS
           IF NOT FS-STATUS-OK
              MOVE CON-310000-CLOSE-DATOS TO WS-ERR-PARRAFO 
              MOVE CON-DATOS              TO WS-ERR-OBJETO 
              MOVE CON-CERRAR             TO WS-ERR-OPERACION 
              MOVE FS-STATUS              TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F            
           END-IF
           .
       310000-CLOSE-DATOS-F. EXIT.  
      ******************************************************************
      *                         320000-TOTAL   
      ****************************************************************** 
       320000-TOTAL.
           COMPUTE WS-PROM-100 = WS-SUM-100 / WS-TOT-100
           DISPLAY "AVERAGE OF NUMBERS GREATER THAN 100 " WS-PROM-100           
           DISPLAY "SUMMATION OF NUMBERS BETWEEN 30-60 " WS-SUM-INT
           .
       320000-TOTAL-F. EXIT.  
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
       END PROGRAM E11.
