      ******************************************************************
      * Se ingresa un valor numérico entero n, se pide desarrollar un 
      * algoritmo que muestre por consola 
      * los primeros n números naturales.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E8.
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
      *****************************  INPUT  ****************************
       
      ****************************  OUTPUT  ****************************

       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
      ************************  CONSTANTES  ****************************

      **************************  SWITCHES  ****************************

      ************************** VARIABLES *****************************
       01 WS-VAR.
           02 WS-N PIC 9(04). 
           02 WS-I PIC 9(04). 
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
           DISPLAY "ENTER A NUMBER "
           ACCEPT WS-N
           .
       100000-START-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS   
      ******************************************************************
       200000-PROCESS.
           PERFORM VARYING WS-I FROM 1
           BY 1 UNTIL WS-I > WS-N
            DISPLAY "NUMBER " WS-I
           END-PERFORM
           .
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           DISPLAY "FIN"
           STOP RUN 
           .    
       300000-END-F. EXIT. 
       
       END PROGRAM E8.
