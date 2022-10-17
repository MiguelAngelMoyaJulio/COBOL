      ******************************************************************
      *Hacer un programa que cargue en un vector 10 registros con la 
      *siguiente estructura:
      *01 Notas.
      *05 Alumnos Occurs 10 times.
      *10 Legajo Pic 9(02).
      *10 Nota Pic 9(02).
      *Repetir legajos en la carga, de modo que haya varios registros 
      *con un mismo legajo en el vector. Ej. 2, 1, 8, 2, 3,
      *3, 1, 4…. Una vez finalizada la carga, efectuar un ordenamiento 
      *del vector por la clave legajo, utilizando el
      *método de ordenamiento de Burbuja, mostrando al final de cada 
      *ciclo de ordenamiento el orden en que van
      *quedando los legajos en el vector.
      *Una vez ordenado los registros, leer el vector efectuando un 
      *corte de control por legajo para calcular el promedio
      *de notas de cada legajo, la máxima nota y la mínima nota para 
      *ese legajo y mostrarlo por pantalla
      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E41.
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
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      *                     WORKING-STORAGE SECTION
      ******************************************************************
       WORKING-STORAGE SECTION.
      ************************  CONSTANTS  *****************************
       01 WSC-CONSTANTS.       
          05 WSC-CON-ROWS                PIC 9(03) VALUE 2.
          05 WSC-CON-COLS                PIC 9(03) VALUE 2.
      ************************** TABLES ********************************
       01 WST-VECTOR.
          05 WST-LEN PIC 9(03).  
          05 WST-ALUMNOS OCCURS 1 TO 999 TIMES 
                         DEPENDING ON WST-LEN
                         INDEXED BY WSV-I.
             10 WST-LEGAJO               PIC 9(02).
             10 WST-NOTA                 PIC 9(02).
      **************************  SWITCHES  ****************************
       01 WSS-SWITCHES.
          05 WSS-CAMBIO PIC X(01).
             88 WSS-CAMBIO-S  VALUE '1'.
             88 WSS-CAMBIO-N  VALUE '0'.

      ************************** VARIABLES ***************************** 
       01 WSV-VARIABLES.
          05 WSV-J                      PIC 9(02).      
          05 WSV-X                      PIC 9(02).      
          05 WSV-AUXILIAR               PIC 9(02).      
          05 WSV-BUR-ORD                PIC 9(02).      
          05 WSV-OPCION                 PIC 9(01) VALUE 9.      
          05 WSV-EDIT                   PIC -9.      
          05 WSV-CORTE-CONTROL.      
             10 WSV-LEGAJO-ANT          PIC 9(02).      
             10 WSV-CORTE-SUMA          PIC 9(03).      
             10 WSV-CORTE-PROMEDIO      PIC 9(03)V9(02).      
             10 WSV-CORTE-CANT-NOTAS    PIC 9(02).      
             10 WSV-CORTE-EDIT          PIC ZZ9,99.      
             10 WSV-CORTE-INDEX         PIC 9(03) VALUE 1.      
             10 WSV-CORTE-NOTA-MAX      PIC 9(02).      
             10 WSV-CORTE-NOTA-MIN      PIC 9(02).      
             10 WSV-CORTE-NOTA-I        PIC 9(02).      
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
              WITH TEST AFTER UNTIL WSV-OPCION = 0

           PERFORM 300000-END
              THRU 300000-END-F
           .
      ******************************************************************
      *                         100000-START         
      ******************************************************************          
       100000-START.
           PERFORM 110000-CARGAR-VECTOR
              THRU 110000-CARGAR-VECTOR-F
           .     
       100000-START-F. EXIT.
      ******************************************************************
      *                         110000-CARGAR-VECTOR         
      ******************************************************************          
       110000-CARGAR-VECTOR.
           DISPLAY "CUANTOS DATOS VAS A CARGAR?"
           ACCEPT WST-LEN

           PERFORM VARYING WSV-I FROM 1
           BY 1 UNTIL WSV-I > WST-LEN
               DISPLAY "INGRESE LEGAJO Y NOTA"          
               ACCEPT WST-LEGAJO(WSV-I)
               ACCEPT WST-NOTA(WSV-I)
           END-PERFORM 
           .     
       110000-CARGAR-VECTOR-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS         
      ****************************************************************** 
       200000-PROCESS.
           DISPLAY "1. MOSTRAR VECTOR"  
           DISPLAY "2. ORDENAMIENTO"  
           DISPLAY "3. CORTE DE CONTROL POR LEGAJO"  
           DISPLAY "0. SALIR "  
           ACCEPT WSV-OPCION 

           EVALUATE WSV-OPCION
               WHEN 1
                    PERFORM 310000-MOSTRAR-VECTOR
                       THRU 310000-MOSTRAR-VECTOR-F
               WHEN 2
                    PERFORM 210000-ORDENAMIENTO-BURBUJA
                       THRU 210000-ORDENAMIENTO-BURBUJA-F
               WHEN 3
                    PERFORM 220000-CORTE-CONTROL
                       THRU 220000-CORTE-CONTROL-F
                       UNTIL WSV-CORTE-INDEX > WST-LEN
           END-EVALUATE
           .
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         210000-ORDENAMIENTO-BURBUJA         
      ****************************************************************** 
       210000-ORDENAMIENTO-BURBUJA.
           PERFORM VARYING WSV-J FROM 1
           BY 1 UNTIL WSV-J > WST-LEN OR WSS-CAMBIO-N
               SET WSS-CAMBIO-N TO TRUE
               PERFORM VARYING WSV-X FROM 1
               BY 1 UNTIL WSV-X > WST-LEN - WSV-J
                      IF WST-LEGAJO(WSV-X) > WST-LEGAJO(WSV-X + 1) 
                         SET WSS-CAMBIO-S TO TRUE
                         MOVE WST-LEGAJO(WSV-X)     TO WSV-AUXILIAR
                         MOVE WST-LEGAJO(WSV-X + 1) TO WST-LEGAJO(WSV-X)
                         MOVE WSV-AUXILIAR      TO WST-LEGAJO(WSV-X + 1)
                      END-IF           
               END-PERFORM 
               MOVE WSV-J TO WSV-BUR-ORD
               
               PERFORM 320000-MOSTRAR-VECTOR-BUR
                  THRU 320000-MOSTRAR-VECTOR-BUR-F
           END-PERFORM 
           .
       210000-ORDENAMIENTO-BURBUJA-F. EXIT. 
      ******************************************************************
      *                         220000-CORTE-CONTROL         
      ****************************************************************** 
       220000-CORTE-CONTROL.
           INITIALIZE WSV-CORTE-CANT-NOTAS
           INITIALIZE WSV-CORTE-SUMA
           INITIALIZE WSV-CORTE-PROMEDIO
           INITIALIZE WSV-CORTE-NOTA-MIN
           INITIALIZE WSV-CORTE-NOTA-MAX
           INITIALIZE WSV-CORTE-NOTA-I
          
           MOVE WST-LEGAJO(WSV-CORTE-INDEX) TO WSV-LEGAJO-ANT
           
           PERFORM UNTIL WST-LEGAJO(WSV-CORTE-INDEX) <> WSV-LEGAJO-ANT 
              IF WSV-CORTE-NOTA-I = 0
                 MOVE WST-NOTA(WSV-CORTE-INDEX) TO WSV-CORTE-NOTA-MAX
                 MOVE WST-NOTA(WSV-CORTE-INDEX) TO WSV-CORTE-NOTA-MIN
                 ADD 1 TO WSV-CORTE-NOTA-I  
              END-IF

              IF WST-NOTA(WSV-CORTE-INDEX) > WSV-CORTE-NOTA-MAX
                 MOVE WST-NOTA(WSV-CORTE-INDEX) TO WSV-CORTE-NOTA-MAX
              END-IF
              
              IF WST-NOTA(WSV-CORTE-INDEX) < WSV-CORTE-NOTA-MIN
                 MOVE WST-NOTA(WSV-CORTE-INDEX) TO WSV-CORTE-NOTA-MIN
              END-IF

              ADD 1 TO WSV-CORTE-CANT-NOTAS
              COMPUTE WSV-CORTE-SUMA = WSV-CORTE-SUMA + 
                      WST-NOTA(WSV-CORTE-INDEX)
              ADD 1 TO WSV-CORTE-INDEX       
           END-PERFORM
           COMPUTE WSV-CORTE-PROMEDIO = WSV-CORTE-SUMA /
                                        WSV-CORTE-CANT-NOTAS
           MOVE WSV-CORTE-PROMEDIO TO WSV-CORTE-EDIT
           DISPLAY "LEGAJO : " WSV-LEGAJO-ANT
           DISPLAY "PROMEDIO " WSV-CORTE-EDIT
           DISPLAY "NOTA MINIMA : " WSV-CORTE-NOTA-MIN
           DISPLAY "NOTA MAXIMA : " WSV-CORTE-NOTA-MAX
           DISPLAY " "
           .
       220000-CORTE-CONTROL-F. EXIT. 
      ******************************************************************
      *                         300000-END         
      ****************************************************************** 
       300000-END. 
           DISPLAY "FIN"
           STOP RUN
           .
       300000-END-F. EXIT.
      ******************************************************************
      *                         310000-MOSTRAR-VECTOR         
      ****************************************************************** 
       310000-MOSTRAR-VECTOR. 
           DISPLAY "[" WITH NO ADVANCING  
           PERFORM VARYING WSV-I FROM 1
           BY 1 UNTIL WSV-I > WST-LEN
                IF WSV-I = WST-LEN
                   DISPLAY "(" WST-LEGAJO(WSV-I)"," WST-NOTA(WSV-I)")"
                   WITH NO ADVANCING 
                ELSE
                   DISPLAY 
                   "(" WST-LEGAJO(WSV-I)"," WST-NOTA(WSV-I)")" ","
                   WITH NO ADVANCING 
                END-IF
           END-PERFORM
           DISPLAY "]" WITH NO ADVANCING
           DISPLAY " "
           .
       310000-MOSTRAR-VECTOR-F. EXIT.
      ******************************************************************
      *                         320000-MOSTRAR-VECTOR-BUR         
      ****************************************************************** 
       320000-MOSTRAR-VECTOR-BUR. 
           DISPLAY "ORDENAMIENTO " WSV-BUR-ORD " : "
                   "[" WITH NO ADVANCING  
           PERFORM VARYING WSV-I FROM 1
           BY 1 UNTIL WSV-I > WST-LEN
                IF WSV-I = WST-LEN
                   MOVE WST-LEGAJO(WSV-I) TO WSV-EDIT
                   DISPLAY WSV-EDIT WITH NO ADVANCING 
                ELSE
                   MOVE WST-LEGAJO(WSV-I) TO WSV-EDIT
                   DISPLAY WSV-EDIT "," WITH NO ADVANCING 
                END-IF
           END-PERFORM
           DISPLAY "]" WITH NO ADVANCING
           DISPLAY " "
           .
       320000-MOSTRAR-VECTOR-BUR-F. EXIT. 
       END PROGRAM E41.      