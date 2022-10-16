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
             05 REG-TAR                         PIC X(16).
             05 REG-FEC                         PIC X(10).
             05 REG-CON                         PIC X(10).
             05 REG-AMOUNT                      PIC 9(04)V9(02).
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
      ************************  CONSTANTS  *****************************

      ************************** TABLES ********************************

      **************************  SWITCHES  ****************************
       01 FS-STATUS-FILE                    PIC X(02) VALUE "00".
          88 FS-STATUS-FILE-OK                        VALUE "00".
          88 FS-STATUS-FILE-EOF                       VALUE "10".

      ************************** VARIABLES ***************************** 
       01 WS-VARIABLES.
          02 WS-TAR-ANT                 PIC X(16).
          02 WS-TOTAL-TAR               PIC 9(06)V9(02).
          02 WS-TOTAL-TAR-MIN           PIC 9(07)V9(02).
          02 WS-MONTO                   PIC ZZ.ZZZ.ZZZ,ZZ.
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
      *                         200000-PROCESS   
      ****************************************************************** 
       200000-PROCESS.
           MOVE REG-TAR TO WS-TAR-ANT
           MOVE ZEROS TO WS-TOTAL-TAR
           
           DISPLAY "TARJETA : " WS-TAR-ANT 
           DISPLAY "FECHA" "           " "CONSUMO" "              " 
                   "IMPORTE"  
           
           PERFORM UNTIL REG-TAR <> WS-TAR-ANT
             COMPUTE WS-TOTAL-TAR = WS-TOTAL-TAR + REG-AMOUNT
             MOVE REG-AMOUNT TO WS-MONTO
             DISPLAY REG-FEC "     " REG-CON "      " WS-MONTO
             PERFORM 210000-READ-DATOS1
                THRU 210000-READ-DATOS1-F 
           END-PERFORM
           
           MOVE WS-TOTAL-TAR TO WS-MONTO   
           DISPLAY "TOTAL " WS-TAR-ANT " :       " WS-MONTO

           COMPUTE WS-TOTAL-TAR-MIN = WS-TOTAL-TAR * 0,2  
           MOVE WS-TOTAL-TAR-MIN TO WS-MONTO   
           DISPLAY "TOTAL PAGO MINIMO :            " WS-MONTO
           
           DISPLAY " "           
           .         
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           PERFORM 310000-CERRAR-DATOS1
              THRU 310000-CERRAR-DATOS1-F
           STOP RUN
           .    
       300000-END-F. EXIT.
      ******************************************************************
      *                         310000-CERRAR-DATOS1   
      ****************************************************************** 
       310000-CERRAR-DATOS1.
           CLOSE DATOS1
           IF NOT FS-STATUS-FILE-OK
               DISPLAY "ERROR AL CERRAR ARCHIVO " FS-STATUS-FILE
           END-IF
           .
       310000-CERRAR-DATOS1-F. EXIT.
       END PROGRAM E34.