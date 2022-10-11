      ******************************************************************
      * Dado un valor entero positivo n que se ingresa 
      * por teclado, desarrollar un programa que 
      * muestre por consola los primeros n números primos.
      ******************************************************************
      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E17.
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

      **************************  SWITCHES  ****************************

      ************************** VARIABLES *****************************
       01 WS-VARIABLES.
           02 WS-NUM                 PIC 9(03) VALUE ZEROS. 
           02 WS-VAL                 PIC 9(03) VALUE ZEROS. 
           02 WS-J                   PIC 9(03) VALUE ZEROS. 
           02 WS-RES                 PIC 9(03) VALUE ZEROS. 
           02 WS-MOD                 PIC 9(03) VALUE ZEROS. 
           02 WS-CANT                PIC 9(03) VALUE ZEROS. 
           02 WS-TOT-PRIMES          PIC 9(03) VALUE ZEROS. 
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
               UNTIL WS-TOT-PRIMES = WS-NUM

           PERFORM 300000-END
              THRU 300000-END-F    
           .
      ******************************************************************
      *                         100000-START         
      ******************************************************************     
       100000-START.
           DISPLAY "HOW MANY PRIME NUMBERS DO YOU WANNA SEE? "
           ACCEPT WS-NUM 
           . 
       100000-START-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS         
      ******************************************************************
       200000-PROCESS.
           ADD 1 TO WS-VAL

           PERFORM 210000-CALCULO-PRIMO
              THRU 210000-CALCULO-PRIMO-F
           
           PERFORM 220000-MOSTRAR-PRIMO
              THRU 220000-MOSTRAR-PRIMO-F
           .
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         210000-CALCULO-PRIMO         
      ******************************************************************
       210000-CALCULO-PRIMO.
           PERFORM VARYING WS-J FROM 1 BY 1
           UNTIL WS-J > WS-VAL
               DIVIDE WS-VAL BY WS-J GIVING WS-RES REMAINDER WS-MOD 
               IF WS-MOD = 0
                   ADD 1 TO WS-CANT
               END-IF
           END-PERFORM
           .
       210000-CALCULO-PRIMO-F. EXIT.
      ******************************************************************
      *                         220000-MOSTRAR-PRIMO         
      ******************************************************************
       220000-MOSTRAR-PRIMO.
           IF WS-CANT = 2
               ADD 1 TO WS-TOT-PRIMES
               DISPLAY WS-VAL
           END-IF

           MOVE ZEROS TO WS-CANT
           .
       220000-MOSTRAR-PRIMO-F. EXIT.
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           DISPLAY "FIN"
           STOP RUN 
           .    
       300000-END-F. EXIT. 

       END PROGRAM E17.
