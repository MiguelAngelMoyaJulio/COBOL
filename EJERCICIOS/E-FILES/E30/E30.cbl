      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-20
      * Se da un conjunto de 744 temperaturas que se tomaron en una 
      *ocalidad diariamente hora por hora
      *urante los 31 días de un mes (el conjunto esta ordenado por día 
      * hora). Desarrollar un programa
      *ue determine e imprima:
      *) Armar la matriz TEMPEDIA en que cada fila corresponda a 
      *n día y cada columna a una hora e
      *mprimirla por horas.
      *) En que día y hora se produjo la temperatura máxima del mes.
      *) En que día se produjo la menor temperatura media.
      *) A que hora se produjo la mayor temperatura media.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E30.
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
       SELECT DATOS ASSIGN TO "TEMP.txt"
                     FILE STATUS IS FS-STATUS-FILE
                     ORGANIZATION IS LINE SEQUENTIAL. 
       
      ****************************  OUTPUT  ****************************       
       DATA DIVISION.
       FILE SECTION.
       FD DATOS.
           01 REG-DATOS.
               05 REG-TEMP          PIC 9(03)V9(02).
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
       01 WS-TF OCCURS 31 TIMES.
           02 WS-TC OCCURS 24 TIMES. 
              05 WS-TEMP              PIC 9(03)V9(02).    

      **************************  SWITCHES  ****************************
       01 FS-STATUS-FILE                    PIC X(02) VALUE "00".
           88 FS-STATUS-FILE-OK                       VALUE "00".
           88 FS-STATUS-FILE-EOF                      VALUE "10".

      ************************** VARIABLES *****************************
       01 WS-VAR.
           02 WS-TEMP-MAX              PIC 9(03)V9(02).
           02 WS-TEMP-MIN-MEDIA        PIC 9(05)V9(02).
           02 WS-TEMP-MIN-MEDIA-REF    PIC 9(05)V9(02).
           02 WS-TEMP-MIN-MEDIA-POS    PIC 9(02).
           02 WS-TEMP-MAX-MEDIA        PIC 9(05)V9(02).
           02 WS-TEMP-MAX-MEDIA-REF    PIC 9(05)V9(02).
           02 WS-TEMP-MAX-MEDIA-POS    PIC 9(02).
           02 WS-I                     PIC 9(03).
           02 WS-J                     PIC 9(03).
           02 WS-X                     PIC 9(03).
           02 WS-TOTE-MA               PIC ZZ.ZZZ.ZZZ,ZZ.

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

           PERFORM 210000-READ-DATOS
              THRU 210000-READ-DATOS-F
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
           PERFORM 220000-CARGAR-MATRIZ
              THRU 220000-CARGAR-MATRIZ-F

           PERFORM 230000-TEMP-MAXIMA
              THRU 230000-TEMP-MAXIMA-F

           PERFORM 240000-TEMP-MIN-MEDIA
              THRU 240000-TEMP-MIN-MEDIA-F   

           PERFORM 250000-TEMP-MAX-MEDIA
              THRU 250000-TEMP-MAX-MEDIA-F
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
                    MOVE CON-LEER                TO WS-ERR-OPERACION 
                    MOVE FS-STATUS-FILE          TO WS-ERR-CODIGO
                    PERFORM 399999-END-PROGRAM
                       THRU 399999-END-PROGRAM-F
           END-EVALUATE
           .
       210000-READ-DATOS-F. EXIT. 
      ******************************************************************
      *                         220000-CARGAR-MATRIZ   
      ****************************************************************** 
       220000-CARGAR-MATRIZ.
           PERFORM VARYING WS-I FROM 1
           BY 1 UNTIL WS-I > 31
               PERFORM VARYING WS-J FROM 1
               BY 1 UNTIL WS-J > 24
                     MOVE REG-TEMP TO WS-TEMP (WS-I, WS-J)                                            
                     PERFORM 210000-READ-DATOS                                            
                        THRU 210000-READ-DATOS-F
               END-PERFORM 
           END-PERFORM                                               
           .         
       220000-CARGAR-MATRIZ-F. EXIT.
      ******************************************************************
      *                         230000-TEMP-MAXIMA   
      ****************************************************************** 
       230000-TEMP-MAXIMA.
           MOVE 1 TO WS-X
           PERFORM VARYING WS-I FROM 1
           BY 1 UNTIL WS-I > 31
               PERFORM VARYING WS-J FROM 1
               BY 1 UNTIL WS-J > 24
                   IF WS-X = 1 THEN
                       MOVE WS-TEMP (WS-I, WS-J) TO WS-TEMP-MAX
                       ADD 1 TO WS-X
                   END-IF
                   IF WS-TEMP (WS-I, WS-J) > WS-TEMP-MAX THEN
                      MOVE WS-TEMP (WS-I, WS-J) TO WS-TEMP-MAX 
                   END-IF
               END-PERFORM 
           END-PERFORM                                               
           .         
       230000-TEMP-MAXIMA-F. EXIT.
      ******************************************************************
      *                         240000-TEMP-MIN-MEDIA   
      ****************************************************************** 
       240000-TEMP-MIN-MEDIA.
           MOVE 1 TO WS-X
           PERFORM VARYING WS-I FROM 1
           BY 1 UNTIL WS-I > 31
               PERFORM VARYING WS-J FROM 1
               BY 1 UNTIL WS-J > 24
                   COMPUTE WS-TEMP-MIN-MEDIA-REF = 
                           WS-TEMP-MIN-MEDIA-REF + WS-TEMP (WS-I, WS-J)
               END-PERFORM 
                   COMPUTE WS-TEMP-MIN-MEDIA-REF = 
                           WS-TEMP-MIN-MEDIA-REF / 24  
                   IF WS-X = 1 THEN
                       MOVE WS-TEMP-MIN-MEDIA-REF TO WS-TEMP-MIN-MEDIA
                       ADD 1 TO WS-X
                   END-IF
                   IF WS-TEMP-MIN-MEDIA-REF < WS-TEMP-MIN-MEDIA THEN
                       MOVE WS-TEMP-MIN-MEDIA-REF TO WS-TEMP-MIN-MEDIA
                       MOVE WS-I TO WS-TEMP-MIN-MEDIA-POS
                   END-IF
           END-PERFORM                                               
           .         
       240000-TEMP-MIN-MEDIA-F. EXIT.
      ******************************************************************
      *                         250000-TEMP-MAX-MEDIA   
      ****************************************************************** 
       250000-TEMP-MAX-MEDIA.
           MOVE 1 TO WS-X
           PERFORM VARYING WS-J FROM 1
           BY 1 UNTIL WS-J > 24
               PERFORM VARYING WS-I FROM 1
               BY 1 UNTIL WS-I > 31
                   COMPUTE WS-TEMP-MAX-MEDIA-REF = 
                           WS-TEMP-MAX-MEDIA-REF + WS-TEMP (WS-I, WS-J)
               END-PERFORM 
                   COMPUTE WS-TEMP-MAX-MEDIA-REF = 
                           WS-TEMP-MAX-MEDIA-REF / 31
                   IF WS-X = 1 THEN
                       MOVE WS-TEMP-MAX-MEDIA-REF TO WS-TEMP-MAX-MEDIA
                       ADD 1 TO WS-X
                   END-IF
                   IF WS-TEMP-MAX-MEDIA-REF > WS-TEMP-MAX-MEDIA THEN
                       MOVE WS-TEMP-MAX-MEDIA-REF TO WS-TEMP-MAX-MEDIA
                       MOVE WS-J TO WS-TEMP-MAX-MEDIA-POS
                   END-IF
           END-PERFORM                                               
           .         
       250000-TEMP-MAX-MEDIA-F. EXIT.
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           PERFORM 310000-CLOSE-DATOS
              THRU 310000-CLOSE-DATOS-F
           
           PERFORM 330000-RESULTADOS
              THRU 330000-RESULTADOS-F
           
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
      *                         320000-MOSTRAR-MATRIZ   
      ****************************************************************** 
       320000-MOSTRAR-MATRIZ.
           PERFORM VARYING WS-I FROM 1
           BY 1 UNTIL WS-I > 31
                PERFORM VARYING WS-J FROM 1
                BY 1 UNTIL WS-J > 24
                     DISPLAY WS-TEMP (WS-I, WS-J)  
                END-PERFORM           
           END-PERFORM       
           .    
       320000-MOSTRAR-MATRIZ-F. EXIT.
      ******************************************************************
      *                         330000-RESULTADOS   
      ****************************************************************** 
       330000-RESULTADOS.
           DISPLAY "TEMPETARUTA MAXIMA " WS-TEMP-MAX
           DISPLAY "DIA TEMPETARUTA MEDIA MINIMA " 
                   WS-TEMP-MIN-MEDIA-POS
           DISPLAY "HORA TEMPETARUTA MEDIA MAXIMA " 
                   WS-TEMP-MAX-MEDIA-POS
           .    
       330000-RESULTADOS-F. EXIT.
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
       END PROGRAM E30.