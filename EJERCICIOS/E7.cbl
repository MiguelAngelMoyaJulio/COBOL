      ******************************************************************
      * Dados un mes y el año al que corresponde, informar cuantos 
      * días tiene el mes, considerando la posibilidad del que 
      * el año sea bisiesto.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E7.
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
       01 WS-F1. 
          02 WS-Y           PIC 9(04). 
          02 FILLER         PIC X(01). 
          02 WS-M           PIC 9(02). 
          02 FILLER         PIC X(01). 
          02 WS-D           PIC 9(02).

       01 WS-OPE. 
          02 WS-MOD-4       PIC 9(04). 
          02 WS-MOD-100     PIC 9(04). 
          02 WS-MOD-400     PIC 9(04). 
          02 WS-RES1        PIC 9(04). 
          02 WS-RES2        PIC 9(04). 
          02 WS-RES3        PIC 9(04).
          02 WS-AMOUNT-DAYS PIC 9(04). 
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
           DISPLAY "DATE FORMAT YYYY-MM-DD"
           DISPLAY "ENTER THE FIRST SIDE "
           ACCEPT WS-F1
           .
       100000-START-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS   
      ******************************************************************
       200000-PROCESS.
           DIVIDE WS-Y BY 4 GIVING WS-RES1 REMAINDER WS-MOD-4
           DIVIDE WS-Y BY 100 GIVING WS-RES2 REMAINDER WS-MOD-100
           DIVIDE WS-Y BY 400 GIVING WS-RES3 REMAINDER WS-MOD-400
      
           IF WS-MOD-4 = 0 AND (WS-MOD-100 <> 0
              OR WS-MOD-400 = 0) 
               DISPLAY "LEAP YEAR"
               EVALUATE WS-M
                   WHEN  1
                        ADD WS-D TO WS-AMOUNT-DAYS
                   WHEN  2
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS + 31 
                                                 + WS-D
                   WHEN  3
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS + 59 
                                                 + WS-D
                   WHEN  4
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS + 90 
                                                 + WS-D
                   WHEN  5
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS  
                                                 + WS-D + 120
                   WHEN  6
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS + 151 
                                                 + WS-D
                   WHEN  7
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS + 181 
                                                 + WS-D
                   WHEN  8
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS + 212 
                                                 + WS-D
                   WHEN  9
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS + 243 
                                                 + WS-D
                   WHEN  10
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS + 273 
                                                 + WS-D
                   WHEN  11
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS + 304 
                                                 + WS-D
                   WHEN  12
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS + 334 
                                                 + WS-D
               END-EVALUATE
           ELSE
               DISPLAY "NO LEAP YEAR"
               EVALUATE WS-M
                   WHEN  1
                        ADD WS-D TO WS-AMOUNT-DAYS
                   WHEN  2
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS + 31 
                                                 + WS-D
                   WHEN  3
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS + 60 
                                                 + WS-D
                   WHEN  4
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS + 91 
                                                 + WS-D
                   WHEN  5
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS +  
                                                  WS-D + 121
                   WHEN  6
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS + 152 
                                                 + WS-D
                   WHEN  7
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS + 182 
                                                 + WS-D
                   WHEN  8
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS + 213 
                                                 + WS-D
                   WHEN  9
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS + 244 
                                                 + WS-D
                   WHEN  10
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS + 274 
                                                 + WS-D
                   WHEN  11
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS + 305 
                                                 + WS-D
                   WHEN  12
                        COMPUTE WS-AMOUNT-DAYS = WS-AMOUNT-DAYS + 335 
                                                 + WS-D
               END-EVALUATE
           END-IF
           .
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           DISPLAY "TOTAL DAYS " WS-AMOUNT-DAYS               
           STOP RUN 
           .    
       300000-END-F. EXIT. 

       END PROGRAM E7.
