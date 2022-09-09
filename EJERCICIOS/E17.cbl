      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-06
      * Dado un valor entero positivo n que se ingresa 
      * por teclado, desarrollar un programa que 
      * muestre por consola los primeros n números primos.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E17.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-VAR. 
           02 WS-NUM                 PIC 9(03) VALUE ZEROS. 
           02 WS-VAL                 PIC 9(03) VALUE ZEROS. 
           02 WS-J                   PIC 9(03) VALUE ZEROS. 
           02 WS-RES                 PIC 9(03) VALUE ZEROS. 
           02 WS-MOD                 PIC 9(03) VALUE ZEROS. 
           02 WS-CANT                PIC 9(03) VALUE ZEROS. 
           02 WS-TOT-PRIMES          PIC 9(03) VALUE ZEROS. 
       PROCEDURE DIVISION.
           
           PERFORM 10-LOAD
              THRU 10-LOAD-F
           
           PERFORM 20-PROCESS
              THRU 20-PROCESS-F
               UNTIL WS-TOT-PRIMES = WS-NUM
           .
           STOP RUN.
       10-LOAD.
           DISPLAY "HOW MANY PRIME NUMBERS DO YOU WANNA SEE? "
           ACCEPT WS-NUM 
           MOVE "N" TO WS-PROCESS
           . 
       10-LOAD-F.

       20-PROCESS.
           ADD 1 TO WS-VAL

           PERFORM 40-CALCULO-PRIMO
              THRU 40-CALCULO-PRIMO-F
           
           PERFORM 30-MOSTRAR-PRIMO
              THRU 30-MOSTRAR-PRIMO-F
           .
       20-PROCESS-F.

       30-MOSTRAR-PRIMO.
           IF WS-CANT = 2
               ADD 1 TO WS-TOT-PRIMES
               DISPLAY WS-VAL
           END-IF

           MOVE ZEROS TO WS-CANT
           .           
       30-MOSTRAR-PRIMO-F.
       40-CALCULO-PRIMO.
           PERFORM VARYING WS-J FROM 1 BY 1
           UNTIL WS-J > WS-VAL
               DIVIDE WS-VAL BY WS-J GIVING WS-RES REMAINDER WS-MOD 
               IF WS-MOD = 0
                   ADD 1 TO WS-CANT
               END-IF
           END-PERFORM
           .
       40-CALCULO-PRIMO-F.
       END PROGRAM E17.
