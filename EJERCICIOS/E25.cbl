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
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************         
       WORKING-STORAGE SECTION.
           01 WS-VAR.
               02 WS-I                     PIC 9(03).
               02 WS-Z                     PIC 9(03).
               02 WS-J                     PIC 9(03).
               02 WS-X                     PIC 9(03).
               02 WS-SUMA                  PIC 9(03).
               02 WS-MAX-FIL               PIC 9(03).
               02 WS-PROMEDIO              PIC 9(03)V99.
           01 WS-V1 OCCURS 3 TIMES.
               02 WS-NUM1                  PIC 9(02) OCCURS 3 TIMES.                
           01 WS-TITULO.
               02 WS-T1                    PIC  X(04) VALUE "V1 [".                 
               02 WS-I1                    PIC  9(02).                  
               02 WS-F1                    PIC  X(05) VALUE "] -> ".                 
               02 WS-VALOR1                PIC  X(02).                 
               02 WS-T2                    PIC  X(07) VALUE " + V2 [".                 
               02 WS-I2                    PIC  9(02).                  
               02 WS-F2                    PIC  X(05) VALUE "] -> ".                 
               02 WS-VALOR2                PIC  9(02).                 
               02 WS-OP                    PIC  X(03) VALUE " = ".                 
               02 WS-T-SUMA                PIC  9(03).
      ******************************************************************
      *                         PROCEDURE DIVISION   
      ******************************************************************                           
       PROCEDURE DIVISION.
           
      *    PERFORM 10-INICIO
      *       THRU 10-INICIO-F
           
           PERFORM 20-PROCESO
              THRU 20-PROCESO-F
           
           PERFORM 30-FIN
              THRU 30-FIN-F 
           .
            STOP RUN.
      ******************************************************************
      *                         10-INICIO   
      ******************************************************************      
       10-INICIO.
           DISPLAY "ASDASD"
           .
       10-INICIO-F. EXIT.
      ******************************************************************
      *                         20-PROCESO   
      ****************************************************************** 
       20-PROCESO.  
           PERFORM 20-CARGAR-MATRIZ
              THRU 20-CARGAR-MATRIZ-F
           .         
       20-PROCESO-F. EXIT.
      ******************************************************************
      *                         20-CARGAR-MATRIZ   
      ******************************************************************      
       20-CARGAR-MATRIZ.
           PERFORM VARYING WS-I FROM 1 BY 1
           UNTIL WS-I > 3
               PERFORM VARYING WS-J FROM 1 BY 1
               UNTIL WS-J > 3
                   DISPLAY "ENTER A NUMBER IN ROW " WS-I " COL " WS-J
                   ACCEPT WS-NUM1(WS-I, WS-J)
               END-PERFORM 
           END-PERFORM
           .
       20-CARGAR-MATRIZ-F. EXIT.
      ******************************************************************
      *                         30-FIN   
      ****************************************************************** 
       30-FIN.
           PERFORM 30-MOSTRAR-MATRIZ
               THRU 30-MOSTRAR-MATRIZ-F
           
           PERFORM 30-PROMEDIO
              THRU 30-PROMEDIO-F
           
           PERFORM 30-SUMA-COL-HOMOLOGA
              THRU 30-SUMA-COL-HOMOLOGA-F
           
           PERFORM 30-MAX-FIL
              THRU 30-MAX-FIL-F
           .    
       30-FIN-F. EXIT.
      ******************************************************************
      *                         30-MOSTRAR-MATRIZ   
      ****************************************************************** 
       30-MOSTRAR-MATRIZ.
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
       30-MOSTRAR-MATRIZ-F. EXIT.
      ******************************************************************
      *                         30-PROMEDIO   
      ****************************************************************** 
       30-PROMEDIO.
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
       30-PROMEDIO-F. EXIT.
      ******************************************************************
      *                         30-SUMA-COL-HOMOLOGA   
      ****************************************************************** 
       30-SUMA-COL-HOMOLOGA.
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
       30-SUMA-COL-HOMOLOGA-F. EXIT.
      ******************************************************************
      *                         30-MAX-FIL   
      ****************************************************************** 
       30-MAX-FIL.
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
       30-MAX-FIL-F. EXIT.
       END PROGRAM E25.