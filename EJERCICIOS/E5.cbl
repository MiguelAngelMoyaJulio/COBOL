      ******************************************************************
      * Dadas dos fechas, informar cual es la más reciente. 
      * Determinar cuales deben ser los datos de entrada 
      * y en que formato los debe ingresar el usuario.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E5.
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
                                                  
           PERFORM 300000-END                         
              THRU 300000-END-F
           .
      ******************************************************************
      *                         100000-START   
      ******************************************************************
       100000-START.
           DISPLAY "FORMAT DATE AAAA-MM-DD"
           DISPLAY "ENTER THE FIRST DATE "
           ACCEPT WS-F1
           DISPLAY "ENTER THE SECOND DATE "
           ACCEPT WS-F2
           .
       100000-START-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS   
      ******************************************************************
       200000-PROCESS.
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
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           DISPLAY "FIN"
           STOP RUN 
           .    
       300000-END-F. EXIT. 

       END PROGRAM E5.
