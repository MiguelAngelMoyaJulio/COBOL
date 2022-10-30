      ******************************************************************
      *Crear un de VEC de 25 componentes. Si la
      *suma de las componentes resulta mayor que 900 imprimir 
      *las de índice par, sino las de índice impar.
      ******************************************************************
      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E20.
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
               05 REG-NUM              PIC 9(03).
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
      ************************  CONSTANTS  *****************************
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
       01 WS-VECTOR OCCURS 25 TIMES.
          02 WS-NUM               PIC 9(03).                

      **************************  SWITCHES  ****************************
       01 WS-SWITCHES.       
          05 FS-STATUS-FILE                PIC X(02) VALUE "00".
             88 FS-STATUS-FILE-OK                    VALUE "00".
             88 FS-STATUS-FILE-EOF                   VALUE "10".
      ************************** VARIABLES *****************************
       01 WS-VARIABLES.
           02 WS-I                     PIC 9(02).
           02 WS-SUM-TOTAL             PIC 9(06).
           02 WS-REMAINING             PIC 9(06)v9(02).
           02 WS-RESULT                PIC 9(06)V9(02).

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
           .                                      
       100000-START-F. EXIT.
      ******************************************************************
      *                         110000-OPEN-DATOS   
      ******************************************************************
       110000-OPEN-DATOS.                        
           OPEN INPUT DATOS                   
           IF NOT FS-STATUS-FILE-OK
              MOVE CON-110000-OPEN-DATOS   TO WS-ERR-PARRAFO 
              MOVE CON-DATOS               TO WS-ERR-OBJETO 
              MOVE CON-ABRIR               TO WS-ERR-OPERACION 
              MOVE FS-STATUS-FILE          TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       110000-OPEN-DATOS-F. EXIT.                          
      ******************************************************************
      *                         200000-PROCESS   
      ****************************************************************** 
       200000-PROCESS.
           ADD 1 TO WS-I     
           MOVE REG-NUM TO WS-NUM(WS-I)

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
               WHEN OTHER
                    MOVE CON-210000-READ-DATOS   TO WS-ERR-PARRAFO 
                    MOVE CON-DATOS               TO WS-ERR-OBJETO 
                    MOVE CON-LEER               TO WS-ERR-OPERACION 
                    MOVE FS-STATUS-FILE          TO WS-ERR-CODIGO
                    PERFORM 399999-END-PROGRAM
                       THRU 399999-END-PROGRAM-F 
           END-EVALUATE
           .
       210000-READ-DATOS-F. EXIT.     
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           PERFORM 310000-CLOSE-DATOS
              THRU 310000-CLOSE-DATOS-F
           
           PERFORM 320000-CALCULAR-SUMA
              THRU 320000-CALCULAR-SUMA-F
 
           DISPLAY "SUM OF ALL NUMBERS IN THE VECTOR " WS-SUM-TOTAL

           PERFORM 330000-MOSTRAR-PAR-IMPAR
              THRU 330000-MOSTRAR-PAR-IMPAR-F

           STOP RUN 
           .    
       300000-END-F. EXIT.
      ******************************************************************
      *                         310000-CLOSE-DATOS   
      ****************************************************************** 
       310000-CLOSE-DATOS.
           CLOSE DATOS
           IF NOT FS-STATUS-FILE-OK
              MOVE CON-310000-CLOSE-DATOS   TO WS-ERR-PARRAFO 
              MOVE CON-DATOS                TO WS-ERR-OBJETO 
              MOVE CON-CERRAR               TO WS-ERR-OPERACION 
              MOVE FS-STATUS-FILE           TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       310000-CLOSE-DATOS-F. EXIT. 
      ******************************************************************
      *                         320000-CALCULAR-SUMA   
      ****************************************************************** 
       320000-CALCULAR-SUMA.
           PERFORM VARYING WS-I FROM 1 
           BY 1 UNTIL WS-I > 25
               COMPUTE WS-SUM-TOTAL = WS-SUM-TOTAL + WS-NUM(WS-I) 
           END-PERFORM
           .
       320000-CALCULAR-SUMA-F. EXIT. 
      ******************************************************************
      *                         330000-MOSTRAR-PAR-IMPAR   
      ****************************************************************** 
       330000-MOSTRAR-PAR-IMPAR.
           IF WS-SUM-TOTAL > 900
               PERFORM VARYING WS-I FROM 1 
               BY 1 UNTIL WS-I > 25
                   INITIALIZE WS-REMAINING
                   INITIALIZE WS-RESULT
                   
                   DIVIDE WS-I BY 2 
                   GIVING WS-RESULT REMAINDER WS-REMAINING  
                   DISPLAY WS-RESULT
                   IF WS-REMAINING = 0
                       DISPLAY WS-NUM(WS-I)
                   END-IF    
               END-PERFORM     
           ELSE
               PERFORM VARYING WS-I FROM 1 
               BY 1 UNTIL WS-I > 25
                   DIVIDE WS-I BY 2 GIVING WS-RESULT REMAINDER
                          WS-REMAINING  
                   IF WS-REMAINING NOT EQUAL ZEROS
                       DISPLAY WS-NUM(WS-I)
                   END-IF    
               END-PERFORM
           END-IF
           .
       330000-MOSTRAR-PAR-IMPAR-F. EXIT. 
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
       END PROGRAM E20.