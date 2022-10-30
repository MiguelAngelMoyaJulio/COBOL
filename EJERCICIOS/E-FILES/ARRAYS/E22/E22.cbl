      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-15
      *Ingresar dos valores enteros 10=M y 15=N. 
      *A continuación ingresar un conjunto A de M
      *elementos y luego otro B de N elementos. Generar e imprimir:
      *a) Un conjunto C resultante de la anexión de A y B.
      *b) Un conjunto D resultante de la anexión de los 
      *elementos distintos de cero de A y B.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E22.
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
       SELECT DATOS ASSIGN TO "DAT1.txt"
                    FILE STATUS IS FS-STATUS-FILE
                    ORGANIZATION IS LINE SEQUENTIAL. 
       
       SELECT DATOS2 ASSIGN TO "DAT2.txt"
                     FILE STATUS IS FS-STATUS-FILE2
                     ORGANIZATION IS LINE SEQUENTIAL. 
       
      ****************************  OUTPUT  ***************************

       DATA DIVISION.
       FILE SECTION.
       FD DATOS.
           01 REG-DATOS.
               05 REG-NUM1                 PIC 9(02).
       
       FD DATOS2.
           01 REG-DATOS2.
               05 REG-NUM2                 PIC 9(02).
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
              05 CON-120000-OPEN-DATOS2      PIC X(30) VALUE 
              '120000-OPEN-DATOS2          '.
              05 CON-210000-READ-DATOS      PIC X(30) VALUE 
              '210000-READ-DATOS           '.
              05 CON-220000-READ-DATOS2      PIC X(30) VALUE 
              '220000-READ-DATOS2          '.
              05 CON-310000-CLOSE-DATOS      PIC X(30) VALUE 
              '310000-CLOSE-DATOS          '.
              05 CON-320000-CLOSE-DATOS2      PIC X(30) VALUE 
              '320000-CLOSE-DATOS2         '.
           02 CON-OPERACIONES.
              05 CON-ABRIR     PIC X(15) VALUE 'ABRIR          '.
              05 CON-LEER      PIC X(15) VALUE 'LEER           '.
              05 CON-CERRAR    PIC X(15) VALUE 'CERRAR         '.
              05 CON-GRABAR    PIC X(15) VALUE 'GRABAR         '.
              05 CON-RUTINA    PIC X(15) VALUE 'LLAMAR RUTINA  '.
           02 CON-OBJETOS.
              05 CON-DATOS     PIC X(10) VALUE 'DATOS   '.
              05 CON-DATOS2    PIC X(10) VALUE 'DATOS2  '.
           02 CON-OTROS.
              05 CON-1         PIC 9(01) VALUE 1.
      ************************** TABLES ********************************
       01 WS-V1 OCCURS 10 TIMES.
           02 WS-NUM1                  PIC 9(02).                
       
       01 WS-V2 OCCURS 15 TIMES.
           02 WS-NUM2                   PIC 9(02).                
       
       01 WS-V3 OCCURS 25 TIMES.
           02 WS-NUM3                   PIC 9(02).                
       
       01 WS-V4 OCCURS 25 TIMES.
           02 WS-NUM4                   PIC 9(02).                

      **************************  SWITCHES  ****************************
       01 WS-SWITCHES.       
           05 FS-STATUS-FILE                PIC X(02) VALUE "00".
              88 FS-STATUS-FILE-OK                    VALUE "00".
              88 FS-STATUS-FILE-EOF                   VALUE "10".
         
           05 FS-STATUS-FILE2               PIC X(02) VALUE "00".
              88 FS-STATUS-FILE2-OK                   VALUE "00".
              88 FS-STATUS-FILE2-EOF                  VALUE "10".
           
           05 FS-LOAD-VECTOR1               PIC X(01) VALUE "N".
              88 FS-LOAD-VECTOR1-OK                   VALUE "Y".
              88 FS-LOAD-VECTOR1-NOK                  VALUE "N".
           
           05 FS-LOAD-VECTOR2               PIC X(01) VALUE "N".
              88 FS-LOAD-VECTOR2-OK                   VALUE "Y".
              88 FS-LOAD-VECTOR2-NOK                  VALUE "N".
           
      ************************** VARIABLES *****************************
       01 WS-VARIABLES.
           02 WS-I                     PIC 9(03).
           02 WS-J                     PIC 9(03).
           02 WS-X                     PIC 9(03).
           02 WS-MAX                   PIC 9(02).

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
                                                  
           PERFORM 300000-END                         
              THRU 300000-END-F
           .
      ******************************************************************
      *                         100000-START         
      ******************************************************************
       100000-START.
           PERFORM 110000-OPEN-DATOS
              THRU 110000-OPEN-DATOS-F                                 
           
           PERFORM 120000-OPEN-DATOS2
              THRU 120000-OPEN-DATOS2-F
              
           PERFORM 210000-READ-DATOS
              THRU 210000-READ-DATOS-F    
           
           PERFORM 220000-READ-DATOS2
              THRU 220000-READ-DATOS2-F                                    
           .                                      
       100000-START-F. EXIT.
      ******************************************************************
      *                         110000-OPEN-DATOS   
      ******************************************************************
       110000-OPEN-DATOS.                        
           OPEN INPUT DATOS                   
           IF NOT FS-STATUS-FILE-OK
              MOVE CON-110000-OPEN-DATOS    TO WS-ERR-PARRAFO 
              MOVE CON-DATOS                TO WS-ERR-OBJETO 
              MOVE CON-ABRIR                TO WS-ERR-OPERACION 
              MOVE FS-STATUS-FILE           TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       110000-OPEN-DATOS-F. EXIT.        
      ******************************************************************
      *                         120000-OPEN-DATOS2   
      ******************************************************************
       120000-OPEN-DATOS2.                        
           OPEN INPUT DATOS2                   
           IF NOT FS-STATUS-FILE2-OK
              MOVE CON-120000-OPEN-DATOS2   TO WS-ERR-PARRAFO 
              MOVE CON-DATOS2               TO WS-ERR-OBJETO 
              MOVE CON-ABRIR                TO WS-ERR-OPERACION 
              MOVE FS-STATUS-FILE2          TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       120000-OPEN-DATOS2-F. EXIT.        
      ******************************************************************
      *                         200000-PROCESS   
      ****************************************************************** 
       200000-PROCESS.
           INITIALIZE WS-I 
           PERFORM 230000-LOAD-VECTOR1
              THRU 230000-LOAD-VECTOR1-F   
              UNTIL FS-LOAD-VECTOR1-OK 
           
           INITIALIZE WS-I
           PERFORM 240000-LOAD-VECTOR2
              THRU 240000-LOAD-VECTOR2-F   
              UNTIL FS-LOAD-VECTOR2-OK 
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
                    SET FS-LOAD-VECTOR1-OK TO TRUE
               WHEN OTHER
                    MOVE CON-210000-READ-DATOS  TO WS-ERR-PARRAFO 
                    MOVE CON-DATOS              TO WS-ERR-OBJETO 
                    MOVE CON-LEER               TO WS-ERR-OPERACION 
                    MOVE FS-STATUS-FILE         TO WS-ERR-CODIGO
                    PERFORM 399999-END-PROGRAM
                       THRU 399999-END-PROGRAM-F 
           END-EVALUATE
           .
       210000-READ-DATOS-F. EXIT.  
      ******************************************************************
      *                         220000-READ-DATOS2   
      ******************************************************************      
       220000-READ-DATOS2.
           INITIALIZE REG-DATOS2
           READ DATOS2 INTO REG-DATOS2
           EVALUATE TRUE
               WHEN FS-STATUS-FILE2-OK
                    CONTINUE   
               WHEN FS-STATUS-FILE2-EOF
                    SET FS-LOAD-VECTOR2-OK TO TRUE
               WHEN OTHER
                    MOVE CON-220000-READ-DATOS2  TO WS-ERR-PARRAFO 
                    MOVE CON-DATOS2              TO WS-ERR-OBJETO 
                    MOVE CON-LEER                TO WS-ERR-OPERACION 
                    MOVE FS-STATUS-FILE2         TO WS-ERR-CODIGO
                    PERFORM 399999-END-PROGRAM
                       THRU 399999-END-PROGRAM-F 
           END-EVALUATE
           .
       220000-READ-DATOS2-F. EXIT.  
      ******************************************************************
      *                         230000-LOAD-VECTOR1   
      ******************************************************************      
       230000-LOAD-VECTOR1.
           ADD 1 TO WS-I     
           MOVE REG-NUM1 TO WS-NUM1(WS-I)

           PERFORM 210000-READ-DATOS
              THRU 210000-READ-DATOS-F
           .
       230000-LOAD-VECTOR1-F. EXIT.  
      ******************************************************************
      *                         240000-LOAD-VECTOR2   
      ******************************************************************      
       240000-LOAD-VECTOR2.
           ADD 1 TO WS-I     
           MOVE REG-NUM2 TO WS-NUM2(WS-I)

           PERFORM 220000-READ-DATOS2
              THRU 220000-READ-DATOS2-F
           .
       240000-LOAD-VECTOR2-F. EXIT.  
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           PERFORM 310000-CLOSE-DATOS
              THRU 310000-CLOSE-DATOS-F
           
           PERFORM 320000-CLOSE-DATOS2
              THRU 320000-CLOSE-DATOS2-F
           
           PERFORM 330000-UNIR-VECTORES
              THRU 330000-UNIR-VECTORES-F
           
           PERFORM 340000-UNIR-VECTORES-NO-CEROS
              THRU 340000-UNIR-VECTORES-NO-CEROS-F
           
           PERFORM 350000-MOSTRAR-VECTORES
              THRU 350000-MOSTRAR-VECTORES-F
           
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
      *                         320000-CLOSE-DATOS2   
      ****************************************************************** 
       320000-CLOSE-DATOS2.
           CLOSE DATOS2
           IF NOT FS-STATUS-FILE2-OK
              MOVE CON-320000-CLOSE-DATOS2   TO WS-ERR-PARRAFO 
              MOVE CON-DATOS2                TO WS-ERR-OBJETO 
              MOVE CON-CERRAR                TO WS-ERR-OPERACION 
              MOVE FS-STATUS-FILE2           TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       320000-CLOSE-DATOS2-F. EXIT. 
      ******************************************************************
      *                         330000-UNIR-VECTORES   
      ****************************************************************** 
       330000-UNIR-VECTORES.
           MOVE ZEROS TO WS-J
           PERFORM VARYING WS-J FROM 1
           BY 1 UNTIL WS-J > 10
               MOVE WS-NUM1(WS-J) TO WS-NUM3(WS-J)
           END-PERFORM 

           ADD 1 TO WS-X
           PERFORM VARYING WS-J FROM 11
           BY 1 UNTIL WS-J > 25
               MOVE WS-NUM2(WS-X) TO WS-NUM3(WS-J)
               ADD 1 TO WS-X
           END-PERFORM     
           .
       330000-UNIR-VECTORES-F. EXIT. 
      ******************************************************************
      *                         340000-UNIR-VECTORES-NO-CEROS   
      ****************************************************************** 
       340000-UNIR-VECTORES-NO-CEROS.
           PERFORM VARYING WS-J FROM 1
               BY 1 UNTIL WS-J > 25
               IF WS-NUM3(WS-J) <> 0
                   MOVE WS-NUM3(WS-J) TO WS-NUM4(WS-J)
               END-IF    
           END-PERFORM
           .
       340000-UNIR-VECTORES-NO-CEROS-F. EXIT. 
      ******************************************************************
      *                         350000-MOSTRAR-VECTORES   
      ****************************************************************** 
       350000-MOSTRAR-VECTORES.
           DISPLAY "VECTOR A U VECTOR B"
           PERFORM VARYING WS-J FROM 1
           BY 1 UNTIL WS-J > 25
                DISPLAY WS-NUM3(WS-J)
           END-PERFORM

           DISPLAY " "
           DISPLAY "VECTOR A U VECTOR B, EXCEPT ZEROS"
           PERFORM VARYING WS-J FROM 1
           BY 1 UNTIL WS-J > 25
                DISPLAY WS-NUM4(WS-J)
           END-PERFORM
           .
       350000-MOSTRAR-VECTORES-F. EXIT.
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
       END PROGRAM E22.