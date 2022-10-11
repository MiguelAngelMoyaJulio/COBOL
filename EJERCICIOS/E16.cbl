      ******************************************************************
      * Desarrollar un programa tal que, dado un valor entero positivo 
      * que se ingresa por teclado indique 
      * si se trata de un número primo o no.
      ******************************************************************
      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E16.
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
           02 WS-J                   PIC 9(03) VALUE ZEROS. 
           02 WS-RES                 PIC 9(03) VALUE ZEROS. 
           02 WS-MOD                 PIC 9(03) VALUE ZEROS. 
           02 WS-CANT                PIC 9(03) VALUE ZEROS. 
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
           DISPLAY "ENTER A NUMER "
           ACCEPT WS-NUM 
           . 
       100000-START-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS         
      ******************************************************************
       200000-PROCESS.
           PERFORM VARYING WS-J FROM 1 BY 1
           UNTIL WS-J > WS-NUM
               DIVIDE WS-NUM BY WS-J GIVING WS-RES REMAINDER WS-MOD 
               IF WS-MOD = 0
                   ADD 1 TO WS-CANT
               END-IF
           END-PERFORM
           .
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         300000-END         
      ******************************************************************
       300000-END.
           PERFORM 310000-TOTAL
              THRU 310000-TOTAL-F
           STOP RUN   
           .
       300000-END-F. EXIT.
      ******************************************************************
      *                         310000-TOTAL         
      ******************************************************************
       310000-TOTAL.
           IF WS-CANT = 2
               DISPLAY "ES NUMERO PRIMO"
           ELSE
               DISPLAY "NO ES NUMERO PRIMO"
           END-IF
           .
       310000-TOTAL-F. EXIT.

       END PROGRAM E16.
