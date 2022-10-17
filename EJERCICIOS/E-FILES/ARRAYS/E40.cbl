      ******************************************************************
      *Realizar un programa que cargue una matriz con números enteros 
      *en orden ascendente en forma de espiral
      *como se muestra en la figura. Compile y ejecute el
      *programa y verifique que el resultado sea el esperado.
      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     E40.
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
          05 WSC-CON-ROWS                PIC 9(03) VALUE 5.
          05 WSC-CON-COLS                PIC 9(03) VALUE 5.
      ************************** TABLES ********************************
       01 WST-MATRIZ.
          05 WST-F OCCURS 5 TIMES.
             10 WST-C OCCURS 5 TIMES.
                15 WS-NUM               PIC 9(02).
      **************************  SWITCHES  ****************************

      ************************** VARIABLES ***************************** 
       01 WSV-VARIABLES.
          05 WSV-I                      PIC 9(02).      
          05 WSV-CANT-CICLOS            PIC 9(03).      
          05 WSV-J                      PIC 9(02).      
          05 WSV-X                      PIC 9(02).      
          05 WSV-Y                      PIC 9(02).      
          05 WSV-NUMERO                 PIC 9(02) VALUE ZEROS.      
          05 WSV-ITER.                    
             10 WSV-ITERACION           PIC 9(02).      
             10 WSV-TOTAL-CICLOS        PIC 9(02).      
             10 WSV-ITE-INDEX           PIC 9(02) VALUE ZEROS.      
             10 WSV-COR-FIL             PIC 9(02).      
             10 WSV-COR-COL             PIC 9(02).      
             10 WSV-CAL-COR             PIC 9(02).      
          05 WSV-PAR-IMPAR.      
             10 WSV-RESULTADO           PIC 9(02).      
             10 WSV-RESTO               PIC 9(02).      
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

           STOP RUN
           .
      ******************************************************************
      *                         100000-START         
      ******************************************************************          
       100000-START.
           PERFORM 110000-CALCULO-CICLOS
              THRU 110000-CALCULO-CICLOS-F
           .     
       100000-START-F. EXIT.
      ******************************************************************
      *                         110000-CALCULO-CICLOS         
      ******************************************************************          
       110000-CALCULO-CICLOS.
           MOVE WSC-CON-ROWS TO WSV-CANT-CICLOS
           PERFORM UNTIL WSV-CANT-CICLOS = 0 OR 
                         WSV-CANT-CICLOS = 1
               ADD 1 TO WSV-TOTAL-CICLOS          
               COMPUTE WSV-CANT-CICLOS = WSV-CANT-CICLOS - 2
           END-PERFORM
           .     
       110000-CALCULO-CICLOS-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS         
      ****************************************************************** 
       200000-PROCESS.
           PERFORM 210000-ITERACIONES-RELLENO
              THRU 210000-ITERACIONES-RELLENO-F

           PERFORM 220000-CALCULO-PAR-IMPAR
              THRU 220000-CALCULO-PAR-IMPAR-F

           PERFORM 230000-ULTIMO-RELLENO
              THRU 230000-ULTIMO-RELLENO-F
           .   
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         210000-ITERACIONES-RELLENO         
      ****************************************************************** 
       210000-ITERACIONES-RELLENO.
           PERFORM VARYING WSV-ITERACION FROM 1
           BY 1 UNTIL WSV-ITERACION > WSV-TOTAL-CICLOS
                MOVE WSV-ITERACION TO WSV-X
                COMPUTE WSV-Y = WSC-CON-COLS - WSV-ITE-INDEX          
           
      *        CARGO PARTE SUPERIOR
                PERFORM VARYING WSV-I FROM WSV-X
                BY 1 UNTIL WSV-I > WSV-Y
                     ADD 1 TO WSV-NUMERO  
                     MOVE WSV-NUMERO TO WS-NUM(WSV-X , WSV-I)          
                END-PERFORM 
           
                COMPUTE WSV-X = WSV-X + 1
      *        CARGO PARTE LATERAL IZQUIERDO
                PERFORM VARYING WSV-I FROM WSV-X
                BY 1 UNTIL WSV-I > WSV-Y
                     ADD 1 TO WSV-NUMERO
                     MOVE WSV-NUMERO TO WS-NUM(WSV-I , WSV-Y)          
                END-PERFORM 

                COMPUTE WSV-I = WSV-Y - 1
      *        CARGO PARTE INFERIOR
                PERFORM UNTIL WSV-I = WSV-ITE-INDEX
                    ADD 1 TO WSV-NUMERO
                    MOVE WSV-NUMERO TO WS-NUM(WSV-Y , WSV-I)
                    COMPUTE WSV-I = WSV-I - 1
                END-PERFORM
           
                MOVE WSV-ITERACION TO WSV-X
                COMPUTE WSV-I = WSV-Y - 1
      *    CARGO PARTE LATERAL DERECHO
                PERFORM UNTIL WSV-I = WSV-X
                    ADD 1 TO WSV-NUMERO
                    MOVE WSV-NUMERO TO WS-NUM(WSV-I , WSV-X)
                    COMPUTE WSV-I = WSV-I - 1
                END-PERFORM  
                
                COMPUTE WSV-ITE-INDEX = WSV-ITE-INDEX + 1         
           END-PERFORM  
           .
       210000-ITERACIONES-RELLENO-F. EXIT.
      ******************************************************************
      *                         220000-CALCULO-PAR-IMPAR         
      ****************************************************************** 
       220000-CALCULO-PAR-IMPAR. 
           DIVIDE WSC-CON-COLS BY 2 GIVING WSV-RESULTADO 
                                    REMAINDER WSV-RESTO
           .
       220000-CALCULO-PAR-IMPAR-F. EXIT.
      ******************************************************************
      *                         230000-ULTIMO-RELLENO         
      ****************************************************************** 
       230000-ULTIMO-RELLENO. 
           IF WSV-RESTO NOT EQUAL ZEROS
               COMPUTE WSV-CAL-COR = ((WSC-CON-COLS - 1 ) / 2 ) + 1
               MOVE WSV-CAL-COR TO WSV-COR-FIL
               MOVE WSV-CAL-COR TO WSV-COR-COL
               ADD 1 TO WSV-NUMERO
               MOVE WSV-NUMERO TO WS-NUM(WSV-COR-FIL , WSV-COR-COL)
           END-IF                             
           .
       230000-ULTIMO-RELLENO-F. EXIT.
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
           DISPLAY "MATRIZ" 
           PERFORM VARYING WSV-I FROM 1
           BY 1 UNTIL WSV-I > WSC-CON-ROWS
                PERFORM VARYING WSV-J FROM 1
                BY 1 UNTIL WSV-J > WSC-CON-COLS
                    DISPLAY "| " WS-NUM(WSV-I,WSV-J) " " 
                    WITH NO ADVANCING          
                END-PERFORM          
                DISPLAY "|" WITH NO ADVANCING 
                DISPLAY " " 
           END-PERFORM
           .
       310000-MOSTRAR-MATRIZ-F. EXIT.
       END PROGRAM     E40.      