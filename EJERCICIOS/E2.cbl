      ******************************************************************
      * Dados dos valores numéricos enteros a y b calcular e informar 
      * el cosiente a/b. Considere que b puede ser cero. 
      * En ese caso mostrar el correspondiente mensaje de error.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E2.
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
       01 WS-CON.       
      **************************  SWITCHES  ****************************
       01 WS-SWITCHES.       
      ************************** VARIABLES *****************************
       01 WS-VAR.
          02 WS-A PIC 9(03).
          02 WS-B PIC 9(03).
          02 WS-DIV PIC 9(04).
          02 WS-REMAINDER PIC 9(04).
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
           DISPLAY "ENTER THE FIRST NUMBER "
           ACCEPT WS-A
           DISPLAY "ENTER THE SECOND NUMBER "
           ACCEPT WS-B
           .
       100000-START-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS         
      ******************************************************************
       200000-PROCESS.
           DIVIDE WS-A BY WS-B GIVING WS-DIV REMAINDER WS-REMAINDER
           .
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           DISPLAY "DIVISION " WS-A " / " WS-B " : " WS-DIV
           DISPLAY "REMAINDER " WS-REMAINDER
           STOP RUN 
           .    
       300000-END-F. EXIT. 
       
       END PROGRAM E2.
