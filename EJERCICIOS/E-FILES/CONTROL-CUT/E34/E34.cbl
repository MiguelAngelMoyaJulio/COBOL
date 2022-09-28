      ******************************************************************
      *Diseñar el algoritmo de resumen de tarjeta de 
      *crédito que liste por fecha los consumos del mes indicando
      *decha, detalle consumo e importe. 
      *efectuar corte por número de tarjeta y 
      *por último indicar el saldo total a
      *pagar y el pago mínimo que es el 20% del saldo total.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E34.
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
             05 REG-TAR                         PIC X(16).
             05 REG-FEC                         PIC X(10).
             05 REG-CON                         PIC X(10).
             05 REG-AMOUNT                      PIC 9(04)V9(02).
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
          01 FS-STATUS1                    PIC X(02) VALUE "00".
             88 FS-STATUS-OK                         VALUE "00".
             88 FS-STATUS-EOF                        VALUE "10".
          01 WS-VAR.
             02 WS-TAR-ANT                 PIC X(16).
             02 WS-TOTAL-TAR               PIC 9(06)V9(02).
             02 WS-TOTAL-TAR-MIN           PIC 9(07)V9(02).
             02 WS-MONTO                   PIC ZZ.ZZZ.ZZZ,ZZ.
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
      *                         20-PROCESO   
      ****************************************************************** 
       20-PROCESO.
           MOVE REG-TAR TO WS-TAR-ANT
           MOVE ZEROS TO WS-TOTAL-TAR
           
           DISPLAY "TARJETA : " WS-TAR-ANT 
           DISPLAY "FECHA" "           " "CONSUMO" "              " 
                   "IMPORTE"  
           
           PERFORM 20-CUT-TAR 
              THRU 20-CUT-TAR-F
              UNTIL REG-TAR <> WS-TAR-ANT
           
           MOVE WS-TOTAL-TAR TO WS-MONTO   
           DISPLAY "TOTAL " WS-TAR-ANT " :       " WS-MONTO

           COMPUTE WS-TOTAL-TAR-MIN = WS-TOTAL-TAR * 0,2  
           MOVE WS-TOTAL-TAR-MIN TO WS-MONTO   
           DISPLAY "TOTAL PAGO MINIMO :            " WS-MONTO
           
           DISPLAY " "           
           .         
       20-PROCESO-F. EXIT.
      ******************************************************************
      *                         20-CUT-TAR   
      ******************************************************************      
       20-CUT-TAR.
          COMPUTE WS-TOTAL-TAR = WS-TOTAL-TAR + REG-AMOUNT
          MOVE REG-AMOUNT TO WS-MONTO
          DISPLAY REG-FEC "     " REG-CON "      " WS-MONTO
          PERFORM 20-LEER1
             THRU 20-LEER1-F
           .
       20-CUT-TAR-F. EXIT.
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
       END PROGRAM E34.