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
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT DATOS1
       ASSIGN TO "TEMP.txt"
       FILE STATUS IS FS-STATUS1
       ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD DATOS1.
           01 REG-DATOS1.
               05 REG-TEMP          PIC 9(03)V9(02).
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************        
       WORKING-STORAGE SECTION.
           01 FS-STATUS1                    PIC X(02) VALUE "00".
               88 FS-STATUS-OK                        VALUE "00".
               88 FS-STATUS-EOF                       VALUE "10".
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
               02 WS-TOTE-MA               PIC ZZ,ZZZ,ZZZ.ZZ.
           01 WS-TF OCCURS 31 TIMES.
               02 WS-TC OCCURS 24 TIMES. 
                   05 WS-TEMP              PIC 9(03)V9(02).    
           01 WS-TITULO.
               02 FILLER                   PIC X(03). 
               02 T-PASAPORTE              PIC 9(06). 
               02 FILLER                   PIC X(04). 
               02 T-MONTO                  PIC  ZZ,ZZZ,ZZZ.ZZ.
      ******************************************************************
      *                         PROCEDURE DIVISION   
      ******************************************************************        
       PROCEDURE DIVISION.
           
           PERFORM 10-INICIO
              THRU 10-INICIO-F
           
           PERFORM 20-PROCESO
              THRU 20-PROCESO-F
              UNTIL FS-STATUS-EOF
           
           PERFORM 30-FIN
              THRU 30-FIN-F

           PERFORM 30-RESULTADOS
              THRU 30-RESULTADOS-F   
           .
            STOP RUN.
      ******************************************************************
      *                         10-INICIO   
      ******************************************************************      
       10-INICIO.
           PERFORM 10-ABRIR-DATOS1
              THRU 10-ABRIR-DATOS1-F

           PERFORM 20-LEER1
              THRU 20-LEER1-F
           .
       10-INICIO-F. EXIT.
      ******************************************************************
      *                         10-ABRIR-DATOS1   
      ******************************************************************     
       10-ABRIR-DATOS1.
           OPEN INPUT DATOS1
           IF NOT FS-STATUS-OK
               DISPLAY "ERROR AL ABRIR ARCHIVO " FS-STATUS1
           END-IF
           .
       10-ABRIR-DATOS1-F. EXIT.
      ******************************************************************
      *                         20-PROCESO   
      ****************************************************************** 
       20-PROCESO.
           PERFORM 20-CARGAR-MATRIZ
              THRU 20-CARGAR-MATRIZ-F

           PERFORM 20-TEMP-MAXIMA
              THRU 20-TEMP-MAXIMA-F

           PERFORM 20-TEMP-MIN-MEDIA
              THRU 20-TEMP-MIN-MEDIA-F   

           PERFORM 20-TEMP-MAX-MEDIA
              THRU 20-TEMP-MAX-MEDIA-F
           .         
       20-PROCESO-F. EXIT.
      ******************************************************************
      *                         20-CARGAR-MATRIZ   
      ****************************************************************** 
       20-CARGAR-MATRIZ.
           PERFORM VARYING WS-I FROM 1
           BY 1 UNTIL WS-I > 31
               PERFORM VARYING WS-J FROM 1
               BY 1 UNTIL WS-J > 24
                     MOVE REG-TEMP TO WS-TEMP (WS-I, WS-J)                                            
                     PERFORM 20-LEER1                                            
                        THRU 20-LEER1-F
               END-PERFORM 
           END-PERFORM                                               
           .         
       20-CARGAR-MATRIZ-F. EXIT.
      ******************************************************************
      *                         20-TEMP-MAXIMA   
      ****************************************************************** 
       20-TEMP-MAXIMA.
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
       20-TEMP-MAXIMA-F. EXIT.
      ******************************************************************
      *                         20-TEMP-MIN-MEDIA   
      ****************************************************************** 
       20-TEMP-MIN-MEDIA.
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
       20-TEMP-MIN-MEDIA-F. EXIT.
      ******************************************************************
      *                         20-TEMP-MAX-MEDIA   
      ****************************************************************** 
       20-TEMP-MAX-MEDIA.
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
       20-TEMP-MAX-MEDIA-F. EXIT.
      ******************************************************************
      *                         20-LEER1   
      ******************************************************************      
       20-LEER1.
           INITIALIZE REG-DATOS1
           READ DATOS1 INTO REG-DATOS1
           EVALUATE TRUE
               WHEN FS-STATUS-OK
                    CONTINUE
               WHEN FS-STATUS-EOF
                    CONTINUE
           END-EVALUATE
           .
       20-LEER1-F. EXIT.
      ******************************************************************
      *                         30-FIN   
      ****************************************************************** 
       30-FIN.
           PERFORM 30-CERRAR-DATOS1
              THRU 30-CERRAR-DATOS1-F
           .    
       30-FIN-F. EXIT.
      ******************************************************************
      *                         30-MOSTRAR-MATRIZ   
      ****************************************************************** 
       30-MOSTRAR-MATRIZ.
           PERFORM VARYING WS-I FROM 1
           BY 1 UNTIL WS-I > 31
                PERFORM VARYING WS-J FROM 1
                BY 1 UNTIL WS-J > 24
                     DISPLAY WS-TEMP (WS-I, WS-J)  
                END-PERFORM           
           END-PERFORM       
           .    
       30-MOSTRAR-MATRIZ-F. EXIT.
      ******************************************************************
      *                         30-RESULTADOS   
      ****************************************************************** 
       30-RESULTADOS.
           DISPLAY "TEMPETARUTA MAXIMA " WS-TEMP-MAX
           DISPLAY "DIA TEMPETARUTA MEDIA MINIMA " 
                   WS-TEMP-MIN-MEDIA-POS
           DISPLAY "HORA TEMPETARUTA MEDIA MAXIMA " 
                   WS-TEMP-MAX-MEDIA-POS
           .    
       30-RESULTADOS-F. EXIT.
      ******************************************************************
      *                         30-CERRAR-DATOS1   
      ****************************************************************** 
       30-CERRAR-DATOS1.
           CLOSE DATOS1
           IF NOT FS-STATUS-OK
               DISPLAY "ERROR AL CERRAR ARCHIVO " FS-STATUS1
           END-IF
           .
       30-CERRAR-DATOS1-F. EXIT.
       END PROGRAM E30.