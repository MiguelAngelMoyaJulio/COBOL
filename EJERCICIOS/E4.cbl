      ******************************************************************
      * Dados dos valores numéricos, informar cual es el mayor y 
      * cual es el menor o, si ambos valores 
      * son iguales emitir un mensaje
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E4.
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
       LINKAGE SECTION.         
       ******************************************************************
      *                         PROCEDURE DIVISION   
      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM 100000-START                      
              THRU 100000-START-F                    
                                                  
           PERFORM 200000-PROCESS                     
              THRU 200000-PROCESS-F                   
              UNTIL CONDICION                 
                                                  
           PERFORM 300000-END                         
              THRU 300000-END-F   
           . 
       100-LOAD.
           DISPLAY "ENTER THE FIRST NUMBER "
           ACCEPT WS-A
           DISPLAY "ENTER THE SECOND NUMBER "
           ACCEPT WS-B
           .
       100-LOAD-F.

       200-PROCESS.
           IF WS-A > WS-B THEN
               DISPLAY WS-A " IS GREATER"
           ELSE
               IF WS-B > WS-A THEN
                   DISPLAY WS-B " IS GREATER"
               ELSE
                   DISPLAY " ITS THE SAME NUMBER"
               END-IF 
           END-IF
           .
       200-PROCESS-F. EXIT.
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           DISPLAY "FIN"
           STOP RUN 
           .    
       300000-END-F. EXIT. 
       END PROGRAM E4.
