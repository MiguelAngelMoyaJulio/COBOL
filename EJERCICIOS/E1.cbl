      ******************************************************************
      * Dados dos valores numéricos enteros a y b, calcular e informar
      * la suma: a+b, la diferencia: a-b, y el producto: a*b entre dichos 
      * valores.
      ******************************************************************
      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E1.
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
      *****************************  CONST  ****************************
      
      **************************  SWITCHES  ****************************
      
      *************************  VARIABLES  ****************************
       01 WS-VAR.
          02 WS-A PIC 9(03).
          02 WS-B PIC 9(03).
          02 WS-SUM PIC 9(04).
          02 WS-SUB PIC 9(04).
          02 WS-MUL PIC 9(06).
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
       100000-START-F.
      ******************************************************************
      *                         200000-PROCESS   
      ******************************************************************
       200000-PROCESS.
           COMPUTE WS-SUM = WS-A + WS-B
           COMPUTE WS-SUB = WS-A - WS-B
           COMPUTE WS-MUL = WS-A * WS-B
           DIVIDE WS-A BY WS-B GIVING WS-DIV REMAINDER WS-REMAINDER
           DISPLAY "SUM " WS-A " + " WS-B " : " WS-SUM
           DISPLAY "SUBSTRACTION " WS-A " - " WS-B " : " WS-SUB
           DISPLAY "MULTIPLICATION " WS-A " * " WS-B " : " WS-MUL
           DISPLAY "DIVISION " WS-A " / " WS-B " : " WS-DIV
           DISPLAY "REMAINDER " WS-REMAINDER
           .
       200000-PROCESS-F.
      ******************************************************************
      *                         300000-END   
      ******************************************************************
       300000-END.
           DISPLAY "SUM " WS-A " + " WS-B " : " WS-SUM
           DISPLAY "SUBSTRACTION " WS-A " - " WS-B " : " WS-SUB
           DISPLAY "MULTIPLICATION " WS-A " * " WS-B " : " WS-MUL
           DISPLAY "DIVISION " WS-A " / " WS-B " : " WS-DIV
           DISPLAY "REMAINDER " WS-REMAINDER
           STOP RUN
           .
       300000-END-F. EXIT.

       END PROGRAM E1.
