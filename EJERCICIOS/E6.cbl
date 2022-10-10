      ******************************************************************
      * Dado un triángulo representado por sus lados lado1, lado2 y 
      * lado3, determinar e indicar según corresponda: 
      * “equilátero”, “isósceles” o “escálenos”.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E6.
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
       01 WS-VAR.
           02 WS-L1     PIC 9(02). 
           02 WS-L2     PIC 9(02). 
           02 WS-L3     PIC 9(02). 
      ************************  CONSTANTES  ****************************

      **************************  SWITCHES  ****************************

      ************************** VARIABLES *****************************
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
           .
      ******************************************************************
      *                         100000-START         
      ******************************************************************     
       100000-START.
           DISPLAY "ENTER THE FIRST SIDE "
           ACCEPT WS-L1
           DISPLAY "ENTER THE SECOND SIDE "
           ACCEPT WS-L2
           DISPLAY "ENTER THE THIRD SIDE "
           ACCEPT WS-L3
           .
       100000-START-F.
      ******************************************************************
      *                         200000-PROCESS         
      ******************************************************************
       200000-PROCESS.
           IF WS-L1 = WS-L2 AND WS-L3 = WS-L2 
               DISPLAY "EQUILATERAL"
           ELSE
               IF WS-L1 = WS-L2 OR WS-L3 = WS-L2 OR WS-L1 = WS-L3
                   DISPLAY "ISOSCELES"
               ELSE
                   DISPLAY "SCALENE"
               END-IF
           END-IF  
           .
       200000-PROCESS-F.
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           DISPLAY "FIN"
           STOP RUN 
           .    
       300000-END-F. EXIT. 

       END PROGRAM E6.
