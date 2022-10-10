      ******************************************************************
      * Dado un valor numérico entero m, determinar e imprimir un 
      * listado con los m primeros 
      * múltiplos de 3 que no sean múltiples de 5
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E13.
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
          02 WS-NUM        PIC 9(03). 
          02 WS-I          PIC 9(03). 
          02 WS-X          PIC 9(03). 
          02 WS-MOD-3      PIC 9(03). 
          02 WS-RES-3      PIC 9(03). 
          02 WS-RES-5      PIC 9(03). 
          02 WS-MOD-5      PIC 9(03). 
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
              UNTIL WS-I = WS-NUM               
                                                  
           PERFORM 300000-END                         
              THRU 300000-END-F
           .   
      ******************************************************************
      *                         100000-START         
      ******************************************************************     
       100000-START.
           DISPLAY "ENTER A NUMBER "
           ACCEPT WS-NUM
           .
       100000-START-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS         
      ******************************************************************
       200000-PROCESS.
           ADD 1 TO WS-X
           DIVIDE WS-X BY 3 GIVING WS-RES-3 REMAINDER WS-MOD-3
           DIVIDE WS-X BY 5 GIVING WS-RES-5 REMAINDER WS-MOD-5
               
           IF WS-MOD-3 = 0 AND WS-MOD-5 <> 0
               DISPLAY "NUMBER " WS-X 
               ADD 1 TO WS-I
           END-IF
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
       
       END PROGRAM E13.
