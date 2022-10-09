      ******************************************************************
      * Dado valor numérico entero que se ingresa por teclado, 
      * se pide informar:
      * •	La quinta parte de dicho valor,
      * •	el resto de la división por 5 y
      * •	la séptima parte de la quinta parte.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E3.
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
          02 WS-A         PIC 9(03).
          02 WS-B         PIC 9(03).
          02 WS-Q         PIC 9(04).
          02 WS-DIV       PIC 9(04).
          02 WS-BY-FIVE   PIC 9(04).
          02 WS-S         PIC 9(04)V9(02).
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
           .
       100000-START-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS   
      ******************************************************************
       200000-PROCESS.
           DIVIDE WS-A BY 5 GIVING WS-DIV REMAINDER WS-REMAINDER
           DIVIDE WS-DIV BY 7 GIVING WS-S
           COMPUTE WS-BY-FIVE = WS-REMAINDER * 5
           .
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           DISPLAY "DIVISION BY FIVE " WS-A " / 5 : " WS-DIV
           DISPLAY "RAMAINDER BY FIVE " WS-A " * 5 : " WS-BY-FIVE
           DISPLAY "DIVIDING THE FIRST DIVISION BY SEVEN " WS-S 
           STOP RUN 
           .    
       300000-END-F. EXIT.

       END PROGRAM E3.
