      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-18
      *matriz MATRIZA de 3 filas y 3 columnas. 
      *Desarrollar un programa que:
      *a) Imprima la matriz MATRIZA por columnas.
      *b) Calcule e imprima el valor promedio de los componentes 
      *de la matriz.
      *c) Genere e imprima un vector VECSUMCOL donde cada 
      *componente sea la suma de la columna
      *homóloga.
      *d) Genere e imprima un vector VECMAXFIL donde 
      *cada componente sea el valor máximo de cada
      *fila.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E25.
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
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
      ************************  CONSTANTS  *****************************

      ************************** TABLES ********************************
       01 WST-MATRIZ.
          05 WST-F OCCURS 3 TIMES.
             10 WST-C OCCURS 3 TIMES.
                15 WS-NUM1               PIC 9(02).
      **************************  SWITCHES  ****************************
      
      ************************** VARIABLES *****************************
       01 WS-VARIABLES.
           02 WS-I                     PIC 9(03).
           02 WS-J                     PIC 9(03).
           02 WS-SUMA                  PIC 9(03).
           02 WS-MAX-FIL               PIC 9(03).
           02 WS-PROMEDIO              PIC 9(03)V99.
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
           
           PERFORM 300000-END
              THRU 300000-END-F 
           .
      ******************************************************************
      *                         100000-START   
      ******************************************************************      
       100000-START.
           PERFORM 110000-CARGAR-MATRIZ
              THRU 110000-CARGAR-MATRIZ-F
           .
       100000-START-F. EXIT.
      ******************************************************************
      *                         110000-CARGAR-MATRIZ   
      ******************************************************************      
       110000-CARGAR-MATRIZ.
           PERFORM VARYING WS-I FROM 1 BY 1
           UNTIL WS-I > 3
               PERFORM VARYING WS-J FROM 1 BY 1
               UNTIL WS-J > 3
                   DISPLAY "ENTER A NUMBER IN ROW " WS-I " COL " WS-J
                   ACCEPT WS-NUM1(WS-I, WS-J)
               END-PERFORM 
           END-PERFORM
           .
       110000-CARGAR-MATRIZ-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS   
      ****************************************************************** 
       200000-PROCESS.  
           PERFORM 210000-PROMEDIO
              THRU 210000-PROMEDIO-F
           
           PERFORM 220000-SUMA-COL-HOMOLOGA
              THRU 220000-SUMA-COL-HOMOLOGA-F
           
           PERFORM 230000-MAX-FIL
              THRU 230000-MAX-FIL-F
           .         
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         210000-PROMEDIO   
      ****************************************************************** 
       210000-PROMEDIO.
           PERFORM VARYING WS-I FROM 1 BY 1
           UNTIL WS-I > 3
               PERFORM VARYING WS-J FROM 1 BY 1
               UNTIL WS-J > 3
                   COMPUTE WS-SUMA = WS-SUMA + WS-NUM1 (WS-I, WS-J)
               END-PERFORM
           END-PERFORM
           COMPUTE WS-PROMEDIO = WS-SUMA / 9
           DISPLAY "AVERAGE " WS-PROMEDIO
           .    
       210000-PROMEDIO-F. EXIT.
      ******************************************************************
      *                         220000-SUMA-COL-HOMOLOGA   
      ****************************************************************** 
       220000-SUMA-COL-HOMOLOGA.
           PERFORM VARYING WS-I FROM 1 BY 1
           UNTIL WS-I > 3
               INITIALIZE WS-SUMA
               PERFORM VARYING WS-J FROM 1 BY 1
               UNTIL WS-J > 3
                   COMPUTE WS-SUMA = WS-SUMA + WS-NUM1 (WS-J, WS-I)
               END-PERFORM
               DISPLAY "SUM COL " WS-I " " WS-SUMA
           END-PERFORM
           .    
       220000-SUMA-COL-HOMOLOGA-F. EXIT.
      ******************************************************************
      *                         230000-MAX-FIL   
      ****************************************************************** 
       230000-MAX-FIL.
           PERFORM VARYING WS-I FROM 1 BY 1
           UNTIL WS-I > 3
               PERFORM VARYING WS-J FROM 1 BY 1
               UNTIL WS-J > 3
                   IF WS-J = 1
                       MOVE WS-NUM1 (WS-I,WS-J) TO WS-MAX-FIL
                   END-IF
               
                   IF WS-NUM1 (WS-I,WS-J) > WS-MAX-FIL
                       MOVE WS-NUM1 (WS-I,WS-J) TO WS-MAX-FIL
                   END-IF
               END-PERFORM
               DISPLAY "MAX FIL " WS-I " " WS-MAX-FIL
           END-PERFORM
           .    
       230000-MAX-FIL-F. EXIT. 
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           PERFORM 310000-MOSTRAR-MATRIZ
              THRU 310000-MOSTRAR-MATRIZ-F

           STOP RUN
           .    
       300000-END-F. EXIT.
      ******************************************************************
      *                         310000-MOSTRAR-MATRIZ   
      ****************************************************************** 
       310000-MOSTRAR-MATRIZ.
           PERFORM VARYING WS-I FROM 1 BY 1
           UNTIL WS-I > 3
               PERFORM VARYING WS-J FROM 1 BY 1
               UNTIL WS-J > 3
                   DISPLAY " | " WS-NUM1(WS-I, WS-J) " | " 
                           WITH NO ADVANCING
               END-PERFORM
               DISPLAY " " 
           END-PERFORM
           .    
       310000-MOSTRAR-MATRIZ-F. EXIT.
       END PROGRAM E25.