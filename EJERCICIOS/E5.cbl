      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-06
      * Dadas dos fechas, informar cual es la más reciente. 
      * Determinar cuales deben ser los datos de entrada 
      * y en que formato los debe ingresar el usuario.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E5.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-F1.
          02 WS-A     PIC X(04). 
          02 FILLER   PIC X(01). 
          02 WS-M     PIC X(02). 
          02 FILLER   PIC X(01). 
          02 WS-D     PIC X(02). 
       01 WS-F2.
          02 WS-AA     PIC X(04). 
          02 FILLER    PIC X(01). 
          02 WS-MM     PIC X(02). 
          02 FILLER    PIC X(01). 
          02 WS-DD     PIC X(02). 
       PROCEDURE DIVISION.
           PERFORM 100-LOAD
              THRU 100-LOAD-F
           
           PERFORM 200-PROCESS
              THRU 200-PROCESS-F
           .
           STOP RUN.
       100-LOAD.
           DISPLAY "FORMAT DATE AAAA-MM-DD"
           DISPLAY "ENTER THE FIRST DATE "
           ACCEPT WS-F1
           DISPLAY "ENTER THE SECOND DATE "
           ACCEPT WS-F2
           .
       100-LOAD-F.

       200-PROCESS.
           IF WS-A > WS-AA
               DISPLAY WS-F1 " IS MORE RECENT"
           ELSE
               IF WS-AA > WS-A
                   DISPLAY WS-F2 " IS MORE RECENT"
               ELSE
                   IF WS-M > WS-MM
                       DISPLAY WS-F1 " IS MORE RECENT"
                   ELSE
                       IF WS-MM > WS-M
                           DISPLAY WS-F2 " IS MORE RECENT"
                       ELSE
                           IF WS-D > WS-DD
                               DISPLAY WS-F1 " IS MORE RECENT"
                           ELSE
                               IF WS-DD > WS-D
                                   DISPLAY WS-F2 " IS MORE RECENT"
                               ELSE
                                   DISPLAY "DATES ARE EQUAL"
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF    
           END-IF
           .
       200-PROCESS-F.
       END PROGRAM E5.
