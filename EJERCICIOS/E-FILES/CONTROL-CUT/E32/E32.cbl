      ******************************************************************
      *Se cuenta con un archivo de Puntos WiFi públicos de la 
      *iudad de Buenos Aires, con el siguiente diseño de
      *egistro:COMUNA, SITIO, APS, NOMBRE, DIRECCIÓN NORMALIZADA,
      *LAT, LONG
      *sando corte de control a partir de este archivo, deben generar 
      *n listado de puntos wifi agrupando por
      *omuna y luego por sitio (Biblioteca, espacio público, etc).
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E32.
       AUTHOR. MIGUEL MOYA.
       DATE-WRITTEN. SEPTEMBER 2022.
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
      ******************************************************************
      *                            FILES   
      ******************************************************************
      *****************************  INPUT  ****************************
       SELECT DATOS1 ASSIGN TO "LOTE.txt"
                     FILE STATUS IS FS-STATUS-FILE
                     ORGANIZATION IS LINE SEQUENTIAL. 
       
      ****************************  OUTPUT  ****************************
       DATA DIVISION.
       FILE SECTION.
       FD DATOS1.
          01 REG-DATOS1.
             05 REG-NEIGHBORHOOD              PIC 9(02).
             05 REG-SITE                      PIC X(18).
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
      ************************  CONSTANTS  *****************************

      ************************** TABLES ********************************

      **************************  SWITCHES  **************************** 
       01 FS-STATUS-FILE               PIC X(02) VALUE "00".
          88 FS-STATUS-FILE-OK                   VALUE "00".
          88 FS-STATUS-FILE-EOF                  VALUE "10".
      ************************** VARIABLES *****************************
       01 WS-VARIABLES.
          02 WS-NEIGH-ANT              PIC 9(02).
          02 WS-SITE-ANT               PIC X(18).
          02 WS-TOTAL-NEIGH            PIC 9(02).
          02 WS-TOTAL-SITE             PIC 9(02).
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
              UNTIL FS-STATUS-FILE-EOF                 
                                                  
           PERFORM 300000-END                         
              THRU 300000-END-F                       
           .                                      
      ******************************************************************
      *                         100000-START         
      ******************************************************************
       100000-START.                                 
           PERFORM 110000-OPEN-DATOS1                
              THRU 110000-OPEN-DATOS1-F
                            
           PERFORM 210000-READ-DATOS1                       
              THRU 210000-READ-DATOS1-F                     
           .                                      
       100000-START-F. EXIT.                         
      ******************************************************************
      *                         110000-OPEN-DATOS1   
      ******************************************************************
       110000-OPEN-DATOS1.                        
           OPEN INPUT DATOS1                   
           IF NOT FS-STATUS-FILE-OK
               DISPLAY "ERROR AL ABRIR ARCHIVO " FS-STATUS-FILE
           END-IF
           .
       110000-OPEN-DATOS1-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS   
      ****************************************************************** 
       200000-PROCESS.
             
           MOVE REG-NEIGHBORHOOD TO WS-NEIGH-ANT
           MOVE ZEROS TO WS-TOTAL-NEIGH
           DISPLAY "PUNTO WIFI COMUNA : " WS-NEIGH-ANT
           PERFORM UNTIL REG-NEIGHBORHOOD <> WS-NEIGH-ANT
             MOVE REG-SITE TO WS-SITE-ANT
             MOVE ZEROS TO WS-TOTAL-SITE
             PERFORM UNTIL REG-SITE <> WS-SITE-ANT
                COMPUTE WS-TOTAL-SITE = WS-TOTAL-SITE + 1  
                PERFORM 210000-READ-DATOS1
                   THRU 210000-READ-DATOS1-F
             END-PERFORM

             DISPLAY WS-SITE-ANT " : " WS-TOTAL-SITE 
             COMPUTE WS-TOTAL-NEIGH = WS-TOTAL-NEIGH + WS-TOTAL-SITE
           END-PERFORM
           DISPLAY "TOTAL PUNTOS WIFI COMUNA " WS-NEIGH-ANT " : " 
                   WS-TOTAL-NEIGH   
           .         
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         210000-READ-DATOS1   
      ******************************************************************      
       210000-READ-DATOS1.
           INITIALIZE REG-DATOS1
           READ DATOS1 INTO REG-DATOS1
           EVALUATE TRUE
               WHEN FS-STATUS-FILE-OK
                    CONTINUE
               WHEN FS-STATUS-FILE-EOF
                    CONTINUE
           END-EVALUATE
           .
       210000-READ-DATOS1-F. EXIT.
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           PERFORM 310000-CLOSE-DATOS1
              THRU 310000-CLOSE-DATOS1-F
           .    
       300000-END-F. EXIT.
      ******************************************************************
      *                         310000-CLOSE-DATOS1   
      ****************************************************************** 
       310000-CLOSE-DATOS1.
           CLOSE DATOS1
           IF NOT FS-STATUS-FILE-OK
               DISPLAY "ERROR AL CERRAR ARCHIVO " FS-STATUS-FILE
           END-IF
           .
       310000-CLOSE-DATOS1-F. EXIT.
       END PROGRAM E32.