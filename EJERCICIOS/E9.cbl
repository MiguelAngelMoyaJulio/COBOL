      ******************************************************************
      * Dados dos valores numéricos enteros, calcular e 
      * informar su producto mediante sumas sucesivas
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E9.
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
           02 WS-A     PIC 9(04). 
           02 WS-B     PIC 9(04). 
           02 WS-I     PIC 9(04). 
           02 WS-TOTAL PIC 9(04). 
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
           DISPLAY "ENTER TWO NUMBERS "
           ACCEPT WS-A
           ACCEPT WS-B
           .
       100000-START-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS         
      ****************************************************************** 
       200000-PROCESS.
           PERFORM VARYING WS-I FROM 1
           BY 1 UNTIL WS-I > WS-A
            COMPUTE WS-TOTAL = WS-TOTAL + WS-B
           END-PERFORM
           .
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           DISPLAY "MULTIPLCATION BETWEEN " WS-A " AND " WS-B " IS "
                   WS-TOTAL  
           DISPLAY "FIN"
           STOP RUN 
           .    
       300000-END-F. EXIT.
       
       END PROGRAM E9.
