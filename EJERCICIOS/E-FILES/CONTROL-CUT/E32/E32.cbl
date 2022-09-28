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
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT DATOS1 ASSIGN TO "LOTE.txt"
                     FILE STATUS IS FS-STATUS1
       ORGANIZATION IS LINE SEQUENTIAL.
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
          01 FS-STATUS1                    PIC X(02) VALUE "00".
             88 FS-STATUS-OK                        VALUE "00".
             88 FS-STATUS-EOF                       VALUE "10".
          01 WS-VAR.
             02 WS-NEIGH-ANT           PIC 9(02).
             02 WS-SITE-ANT            PIC X(18).
             02 WS-TOTAL-NEIGH            PIC 9(02).
             02 WS-TOTAL-SITE            PIC 9(02).
          01 WS-TITULO.
               02 FILLER                   PIC X(03). 
               02 T-VENDEDOR               PIC 9(02). 
               02 FILLER                   PIC X(10). 
               02 T-FACTURA                PIC 9(02). 
               02 FILLER                   PIC X(04). 
               02 T-MONTO                  PIC  ZZ.ZZZ.ZZZ,ZZ.
      ******************************************************************
      *                         PROCEDURE DIVISION   
      ******************************************************************
       PROCEDURE DIVISION.
                                                                          
           PERFORM 10-INICIO                      
              THRU 10-INICIO-F                    
                                                  
           PERFORM 20-PROCESO                     
              THRU 20-PROCESO-F                   
              UNTIL FS-STATUS-EOF                 
                                                  
           PERFORM 30-FIN                         
              THRU 30-FIN-F                       
           .                                      
            STOP RUN.                             
      ******************************************************************
      *                         10-INICIO         
      ******************************************************************
       10-INICIO.                                 
           PERFORM 10-ABRIR-DATOS1                
              THRU 10-ABRIR-DATOS1-F
                            
           PERFORM 20-LEER1                       
              THRU 20-LEER1-F                     
           .                                      
       10-INICIO-F. EXIT.                         
      ******************************************************************
      *                         10-ABRIR-DATOS1   
      ******************************************************************
       10-ABRIR-DATOS1.                        
           OPEN INPUT DATOS1                   
           IF NOT FS-STATUS-OK
               DISPLAY "ERROR AL ABRIR ARCHIVO " FS-STATUS1
           END-IF
           .
       10-ABRIR-DATOS1-F. EXIT.
      ******************************************************************
      *                         20-PROCESO   
      ****************************************************************** 
       20-PROCESO.
             
           MOVE REG-NEIGHBORHOOD TO WS-NEIGH-ANT
           MOVE ZEROS TO WS-TOTAL-NEIGH
           DISPLAY "PUNTO WIFI COMUNA : " WS-NEIGH-ANT
           PERFORM 20-CUT-NEIGHBORHOOD 
              THRU 20-CUT-NEIGHBORHOOD-F
              UNTIL REG-NEIGHBORHOOD <> WS-NEIGH-ANT
           DISPLAY "TOTAL PUNTOS WIFI COMUNA " WS-NEIGH-ANT " : " 
                   WS-TOTAL-NEIGH   
           .         
       20-PROCESO-F. EXIT.
      ******************************************************************
      *                         20-LEER1   
      ******************************************************************      
       20-LEER1.
           INITIALIZE REG-DATOS1
           READ DATOS1 INTO REG-DATOS1
           EVALUATE TRUE
               WHEN FS-STATUS-OK
                    CONTINUE
               WHEN FS-STATUS-EOF
                    CONTINUE
           END-EVALUATE
           .
       20-LEER1-F. EXIT.
      ******************************************************************
      *                         20-CUT-NEIGHBORHOOD   
      ******************************************************************      
       20-CUT-NEIGHBORHOOD.
           MOVE REG-SITE TO WS-SITE-ANT
           MOVE ZEROS TO WS-TOTAL-SITE
           PERFORM 20-CUT-SITE
              THRU 20-CUT-SITE-F
              UNTIL REG-SITE <> WS-SITE-ANT
           DISPLAY WS-SITE-ANT " : " WS-TOTAL-SITE 
           COMPUTE WS-TOTAL-NEIGH = WS-TOTAL-NEIGH + WS-TOTAL-SITE
           .
       20-CUT-NEIGHBORHOOD-F. EXIT.
      ******************************************************************
      *                         20-CUT-SITE   
      ******************************************************************      
       20-CUT-SITE.
           COMPUTE WS-TOTAL-SITE = WS-TOTAL-SITE + 1  
           PERFORM 20-LEER1
              THRU 20-LEER1-F
           .
       20-CUT-SITE-F. EXIT.
      ******************************************************************
      *                         30-FIN   
      ****************************************************************** 
       30-FIN.
           PERFORM 30-CERRAR-DATOS1
              THRU 30-CERRAR-DATOS1-F
           .    
       30-FIN-F. EXIT.
      ******************************************************************
      *                         30-CERRAR-DATOS1   
      ****************************************************************** 
       30-CERRAR-DATOS1.
           CLOSE DATOS1
           IF NOT FS-STATUS-OK
               DISPLAY "ERROR AL CERRAR ARCHIVO " FS-STATUS1
           END-IF
           .
       30-CERRAR-DATOS1-F. EXIT.
       END PROGRAM E32.