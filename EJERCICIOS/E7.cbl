      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-06
      * Dados un mes y el año al que corresponde, informar cuantos 
      * días tiene el mes, considerando la posibilidad del que 
      * el año sea bisiesto.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E7.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-F1. 
          02 WS-Y        PIC 9(04). 
          02 FILLER      PIC X(01). 
          02 WS-M        PIC 9(02). 
          02 FILLER      PIC X(01). 
          02 WS-D        PIC 9(02). 
       77 WS-MOD-4       PIC 9(04). 
       77 WS-MOD-100     PIC 9(04). 
       77 WS-MOD-400     PIC 9(04). 
       77 WS-RES1        PIC 9(04). 
       77 WS-RES2        PIC 9(04). 
       77 WS-RES3        PIC 9(04). 
       77 WS-AMOUNT-DAYS PIC 9(04). 
       PROCEDURE DIVISION.
           PERFORM 100-LOAD
              THRU 100-LOAD-F
           
           PERFORM 200-PROCESS
              THRU 200-PROCESS-F
           .
           STOP RUN.
       100-LOAD.
           DISPLAY "DATE FORMAT YYYY-MM-DD"
           DISPLAY "ENTER THE FIRST SIDE "
           ACCEPT WS-F1
           .
       100-LOAD-F.

       200-PROCESS.
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
           DISPLAY "TOTAL DAYS " WS-AMOUNT-DAYS               
           .
       200-PROCESS-F.
       END PROGRAM E7.
