      ******************************************************************
      *Se tiene un archivo maestro de facturas de gas de una localidad del Gran Buenos Aires y otro con las novedades
      *de pagos del mes de dichas facturas, ordenados en forma ascendente por Nº de medidor. Se desea imprimir un
      *listado con el detalle de los deudores, y los siguientes totales de Control: cant total de deudores y monto total
      *adeudado.
      *ENTRADA:
      *Archivo MAESTRO ( 1 registro por medidor – ordenado secuencial ascendente por NROMEDIDOR)
      *NROMEDIDOR NOMBRECLTE IMP-DEUDA
      *Archivo de COBRANZAS: (1, varios o ningún registro por medidor - ordenado secuencial ascendente por
      *NROMEDIDOR)
      *NROMEDIDOR IMP-PAGO
      *PROCESO:
      *Deberá aparear estos archivos a efectos de:
      *1. Actualizar el saldo para cada cliente para lo cuál deberá restar los importes de la
      *cobranza. 2. Contar la cantidad total de deudores.
      ******************************************************************
      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E37.
       AUTHOR. MIGUEL MOYA.
       DATE-WRITTEN. SEPTEMBER 2022.
       DATE-COMPILED. SEPTEMBER 2022.
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
       SELECT MASTER ASSIGN TO "MASTER.txt"
                     FILE STATUS IS FS-STATUS1
                     ORGANIZATION IS LINE SEQUENTIAL.
       
       SELECT NEWS ASSIGN TO "NEWS.txt"
                     FILE STATUS IS FS-STATUS2
                     ORGANIZATION IS LINE SEQUENTIAL.
      ****************************  OUTPUT  **************************** 
       SELECT MASTER-UPDATE ASSIGN TO "MASTER_UPDATE.txt"
                     FILE STATUS IS FS-STATUS3
                     ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD MASTER.
          01 REG-MASTER.
             05 REG-NRO-MEDIDOR                PIC 9(02).
             05 REG-NRO-CLIENTE                PIC 9(04).
             05 REG-NRO-DEUDA                  PIC 9(04)V9(02).
       
       FD NEWS.
          01 REG-NEWS.
             05 REG-NRO-MEDIDOR-N              PIC 9(02).
             05 REG-NRO-PAGO-N                 PIC 9(04)V9(02).
       
       FD MASTER-UPDATE.
          01 REG-MASTER-UPDATE.              
             05 REG-NRO-MEDIDOR-U              PIC 9(02).
             05 REG-NRO-CLIENTE-U              PIC 9(04).
             05 REG-NRO-DEUDA-U                PIC 9(04)V9(02).

      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
      ************************  CONSTANTS  *****************************
       01 WS-CONSTANTES.
           02 CON-RUTINAS.
              05 CON-RUTINA01  PIC X(08) VALUE 'RUTINA01'.
           02 CON-PARRAFO.
              05 CON-110000-OPEN-MASTER       PIC X(30) VALUE 
              '110000-OPEN-MASTER          '.
              05 CON-120000-OPEN-NEWS          PIC X(30) VALUE 
              '120000-OPEN-NEWS            '.
              05 CON-130000-OPEN-MASTER-UPDATE PIC X(30) VALUE 
              '130000-OPEN-MASTER-UPDATE   '.
              05 CON-210000-READ-MASTER      PIC X(30) VALUE 
              '210000-READ-MASTER          '.
              05 CON-220000-READ-NEWS      PIC X(30) VALUE 
              '220000-READ-NEWS            '.
              05 CON-230000-WRITE-MASTER-UPDATE  PIC X(30) VALUE 
              '230000-WRITE-MASTER-UPDATE  '.
              05 CON-310000-CLOSE-MASTER      PIC X(30) VALUE 
              '310000-CLOSE-MASTER         '.
              05 CON-320000-CLOSE-NEWS      PIC X(30) VALUE 
              '320000-CLOSE-NEWS           '.
              05 CON-330000-CLOSE-MASTER-UPDATE      PIC X(30) VALUE 
              '330000-CLOSE-MASTER-UPDATE  '.
           02 CON-OPERACIONES.
              05 CON-ABRIR     PIC X(15) VALUE 'ABRIR          '.
              05 CON-LEER      PIC X(15) VALUE 'LEER           '.
              05 CON-CERRAR    PIC X(15) VALUE 'CERRAR         '.
              05 CON-GRABAR    PIC X(15) VALUE 'GRABAR         '.
              05 CON-RUTINA    PIC X(15) VALUE 'LLAMAR RUTINA  '.
           02 CON-OBJETOS.
              05 CON-MASTER           PIC X(10) VALUE 'MASTER  '.
              05 CON-NEWS             PIC X(10) VALUE 'NEWS    '.
              05 CON-MASTER-UPDATE    PIC X(10) VALUE 'MASTERUP'.
           02 CON-OTROS.
              05 CON-1         PIC 9(01) VALUE 1.
      ************************** TABLES ********************************

      **************************  SWITCHES  ****************************
       01 WS-SWITCHES.       
          05 FS-STATUS1                    PIC X(02) VALUE "00".
             88 FS-STATUS1-OK                        VALUE "00".
             88 FS-STATUS1-EOF                       VALUE "10".
          05 FS-STATUS2                    PIC X(02) VALUE "00".
             88 FS-STATUS2-OK                        VALUE "00".
             88 FS-STATUS2-EOF                       VALUE "10".
          05 FS-STATUS3                    PIC X(02) VALUE "00".
             88 FS-STATUS3-OK                        VALUE "00".
             88 FS-STATUS3-EOF                       VALUE "10".
      ************************** VARIABLES *****************************
       01 WS-VARIABLES.
          02 WS-MEDIDOR-ANT                PIC 9(02).
          02 WS-MATING.
             05 WS-CODE-M                  PIC 9(02).
             05 WS-CODE-N                  PIC 9(02).
          02 WS-BILLING.   
             05 WS-BILLING-PARTIAL         PIC 9(04)V9(02).  
             05 WS-BILLING-FINAL           PIC 9(04)V9(02).  
          02 WS-ACCUM.
             05 WS-TOTAL-DEBTOR            PIC 9(03).
       01 WS-TITLE.
          02 FILLER                   PIC X(12) VALUE "NRO.MEDIDDOR". 
          02 FILLER                   PIC X(03) VALUE SPACES. 
          02 FILLER                   PIC X(11) VALUE "CLIENT NAME". 
          02 FILLER                   PIC X(03) VALUE SPACES. 
          02 FILLER                   PIC X(04) VALUE "DEBT". 
       
       01 WS-SUBTITLE.
          02 FILLER                   PIC X(05) VALUE SPACES. 
          02 SUB-MEDIDOR              PIC 9(02). 
          02 FILLER                   PIC X(12) VALUE SPACES. 
          02 SUB-CLIENT               PIC 9(04). 
          02 FILLER                   PIC X(05) VALUE SPACES. 
          02 SUB-DEUDA                PIC --.--9,99.

       01 WS-ERRORES.
          05 WS-ERR-PARRAFO            PIC X(30).
          05 WS-ERR-OBJETO             PIC X(10).
          05 WS-ERR-OPERACION          PIC X(15).
          05 WS-ERR-CODIGO             PIC 9(02).    
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
              UNTIL FS-STATUS1-EOF AND FS-STATUS2-EOF                 
                                                  
           PERFORM 300000-END                         
              THRU 300000-END-F   
           .                                      
      ******************************************************************
      *                         100000-START         
      ******************************************************************
       100000-START.                                 
           PERFORM 110000-OPEN-MASTER                
              THRU 110000-OPEN-MASTER-F
           
           PERFORM 120000-OPEN-NEWS                
              THRU 120000-OPEN-NEWS-F
           
           PERFORM 130000-OPEN-MASTER-UPDATE                
              THRU 130000-OPEN-MASTER-UPDATE-F
                            
           PERFORM 210000-READ-MASTER                       
              THRU 210000-READ-MASTER-F                     
           
           PERFORM 220000-READ-NEWS                       
              THRU 220000-READ-NEWS-F  
           
           DISPLAY WS-TITLE                      
           .                                      
       100000-START-F. EXIT.                         
      ******************************************************************
      *                         110000-OPEN-MASTER   
      ******************************************************************
       110000-OPEN-MASTER.                        
           OPEN INPUT MASTER                   
           IF NOT FS-STATUS1-OK
              MOVE CON-110000-OPEN-MASTER    TO WS-ERR-PARRAFO 
              MOVE CON-MASTER                TO WS-ERR-OBJETO 
              MOVE CON-ABRIR                 TO WS-ERR-OPERACION 
              MOVE FS-STATUS1                TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F 
           END-IF
           .
       110000-OPEN-MASTER-F. EXIT.
      ******************************************************************
      *                         120000-OPEN-NEWS   
      ******************************************************************
       120000-OPEN-NEWS.                        
           OPEN INPUT NEWS                   
           IF NOT FS-STATUS2-OK
              MOVE CON-120000-OPEN-NEWS    TO WS-ERR-PARRAFO 
              MOVE CON-NEWS                TO WS-ERR-OBJETO 
              MOVE CON-ABRIR               TO WS-ERR-OPERACION 
              MOVE FS-STATUS2              TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       120000-OPEN-NEWS-F. EXIT.
      ******************************************************************
      *                         130000-OPEN-MASTER-UPDATE   
      ******************************************************************
       130000-OPEN-MASTER-UPDATE.                        
           OPEN OUTPUT MASTER-UPDATE                   
           IF NOT FS-STATUS3-OK
              MOVE CON-130000-OPEN-MASTER-UPDATE TO WS-ERR-PARRAFO 
              MOVE CON-MASTER-UPDATE             TO WS-ERR-OBJETO 
              MOVE CON-ABRIR                     TO WS-ERR-OPERACION 
              MOVE FS-STATUS3                    TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       130000-OPEN-MASTER-UPDATE-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS   
      ****************************************************************** 
       200000-PROCESS.
           MOVE ZEROES TO WS-BILLING-PARTIAL
           MOVE ZEROES TO WS-BILLING-FINAL
           IF WS-CODE-M = WS-CODE-N
               PERFORM UNTIL WS-CODE-M <> WS-CODE-N
                       COMPUTE WS-BILLING-PARTIAL = WS-BILLING-PARTIAL +
                               REG-NRO-PAGO-N
                       PERFORM 220000-READ-NEWS                       
                          THRU 220000-READ-NEWS-F
               END-PERFORM
               
               COMPUTE WS-BILLING-FINAL = REG-NRO-DEUDA - 
                                          WS-BILLING-PARTIAL
                
               PERFORM 240000-CALCULATE-FINAL-BILL
                  THRU 240000-CALCULATE-FINAL-BILL-F

               PERFORM 250000-DISPLAY-RECORD
                  THRU 250000-DISPLAY-RECORD-F 

               PERFORM 230000-WRITE-MASTER-UPDATE
                  THRU 230000-WRITE-MASTER-UPDATE-F
               
               PERFORM 210000-READ-MASTER                       
                  THRU 210000-READ-MASTER-F                     
           ELSE
               IF WS-CODE-M > WS-CODE-N
                  MOVE WS-CODE-N TO WS-MEDIDOR-ANT
                  PERFORM UNTIL WS-MEDIDOR-ANT <> WS-CODE-N
                          PERFORM 220000-READ-NEWS                       
                             THRU 220000-READ-NEWS-F 
                  END-PERFORM
                  PERFORM 220000-READ-NEWS                       
                     THRU 220000-READ-NEWS-F 
               ELSE
                  MOVE REG-NRO-DEUDA TO WS-BILLING-FINAL 

                  PERFORM 230000-WRITE-MASTER-UPDATE
                     THRU 230000-WRITE-MASTER-UPDATE-F

                  PERFORM 250000-DISPLAY-RECORD
                     THRU 250000-DISPLAY-RECORD-F                   

                  PERFORM 240000-CALCULATE-FINAL-BILL
                     THRU 240000-CALCULATE-FINAL-BILL-F

                  PERFORM 210000-READ-MASTER                       
                     THRU 210000-READ-MASTER-F
               END-IF        
           END-IF
           .         
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         210000-READ-MASTER   
      ******************************************************************      
       210000-READ-MASTER.
           INITIALIZE REG-MASTER
           READ MASTER INTO REG-MASTER
           EVALUATE TRUE
               WHEN FS-STATUS1-OK
                    MOVE REG-NRO-MEDIDOR TO WS-CODE-M
               WHEN FS-STATUS1-EOF
                    MOVE 99 TO WS-CODE-M
               WHEN OTHER
                    MOVE CON-210000-READ-MASTER TO WS-ERR-PARRAFO 
                    MOVE CON-MASTER             TO WS-ERR-OBJETO 
                    MOVE CON-LEER               TO WS-ERR-OPERACION 
                    MOVE FS-STATUS1             TO WS-ERR-CODIGO
                    PERFORM 399999-END-PROGRAM
                       THRU 399999-END-PROGRAM-F 
           END-EVALUATE
           .
       210000-READ-MASTER-F. EXIT.
      ******************************************************************
      *                         220000-READ-NEWS   
      ******************************************************************      
       220000-READ-NEWS.
           INITIALIZE REG-NEWS
           READ NEWS INTO REG-NEWS
           EVALUATE TRUE
               WHEN FS-STATUS2-OK
                    MOVE REG-NRO-MEDIDOR-N TO WS-CODE-N
               WHEN FS-STATUS2-EOF
                    MOVE 99 TO WS-CODE-N
               WHEN OTHER
                    MOVE CON-220000-READ-NEWS TO WS-ERR-PARRAFO 
                    MOVE CON-NEWS             TO WS-ERR-OBJETO 
                    MOVE CON-LEER             TO WS-ERR-OPERACION 
                    MOVE FS-STATUS2           TO WS-ERR-CODIGO
                    PERFORM 399999-END-PROGRAM
                       THRU 399999-END-PROGRAM-F     
           END-EVALUATE
           .
       220000-READ-NEWS-F. EXIT.
      ******************************************************************
      *                         230000-WRITE-MASTER-UPDATE   
      ******************************************************************      
       230000-WRITE-MASTER-UPDATE.
           INITIALIZE REG-MASTER-UPDATE
           MOVE REG-NRO-MEDIDOR  TO REG-NRO-MEDIDOR-U
           MOVE REG-NRO-CLIENTE  TO REG-NRO-CLIENTE-U
           MOVE WS-BILLING-FINAL TO REG-NRO-DEUDA-U

           WRITE REG-MASTER-UPDATE
           IF NOT FS-STATUS3-OK
              MOVE CON-230000-WRITE-MASTER-UPDATE TO WS-ERR-PARRAFO 
              MOVE CON-MASTER-UPDATE              TO WS-ERR-OBJETO 
              MOVE CON-GRABAR                     TO WS-ERR-OPERACION 
              MOVE FS-STATUS3                     TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF 
           .
       230000-WRITE-MASTER-UPDATE-F. EXIT.
      ******************************************************************
      *                         240000-CALCULATE-FINAL-BILL   
      ******************************************************************      
       240000-CALCULATE-FINAL-BILL.
           IF WS-BILLING-FINAL > ZEROS
              ADD 1 TO WS-TOTAL-DEBTOR
           END-IF   
           .
       240000-CALCULATE-FINAL-BILL-F. EXIT.
      ******************************************************************
      *                         250000-DISPLAY-RECORD   
      ******************************************************************      
       250000-DISPLAY-RECORD.
           MOVE REG-NRO-MEDIDOR  TO SUB-MEDIDOR
           MOVE REG-NRO-CLIENTE  TO SUB-CLIENT
           MOVE WS-BILLING-FINAL TO SUB-DEUDA
           DISPLAY WS-SUBTITLE
           .
       250000-DISPLAY-RECORD-F. EXIT.
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           PERFORM 310000-CLOSE-MASTER
              THRU 310000-CLOSE-MASTER-F
           
           PERFORM 320000-CLOSE-NEWS
              THRU 320000-CLOSE-NEWS-F
              
           PERFORM 330000-CLOSE-MASTER-UPDATE
              THRU 330000-CLOSE-MASTER-UPDATE-F
           STOP RUN
           .    
       300000-END-F. EXIT.
      ******************************************************************
      *                         310000-CLOSE-MASTER   
      ****************************************************************** 
       310000-CLOSE-MASTER.
           CLOSE MASTER
           IF NOT FS-STATUS1-OK
              MOVE CON-310000-CLOSE-MASTER TO WS-ERR-PARRAFO 
              MOVE CON-MASTER              TO WS-ERR-OBJETO 
              MOVE CON-CERRAR              TO WS-ERR-OPERACION 
              MOVE FS-STATUS1              TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       310000-CLOSE-MASTER-F. EXIT.
      ******************************************************************
      *                         320000-CLOSE-NEWS   
      ****************************************************************** 
       320000-CLOSE-NEWS.
           CLOSE NEWS
           IF NOT FS-STATUS2-OK
              MOVE CON-320000-CLOSE-NEWS TO WS-ERR-PARRAFO 
              MOVE CON-NEWS              TO WS-ERR-OBJETO 
              MOVE CON-CERRAR            TO WS-ERR-OPERACION 
              MOVE FS-STATUS2            TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       320000-CLOSE-NEWS-F. EXIT.
      ******************************************************************
      *                         330000-CLOSE-MASTER-UPDATE   
      ****************************************************************** 
       330000-CLOSE-MASTER-UPDATE.
           CLOSE MASTER-UPDATE
           IF NOT FS-STATUS3-OK
              MOVE CON-330000-CLOSE-MASTER-UPDATE TO WS-ERR-PARRAFO 
              MOVE CON-MASTER-UPDATE              TO WS-ERR-OBJETO 
              MOVE CON-CERRAR                     TO WS-ERR-OPERACION 
              MOVE FS-STATUS3                     TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       330000-CLOSE-MASTER-UPDATE-F. EXIT.
      ******************************************************************
      *                         399999-END-PROGRAM   
      ******************************************************************
       399999-END-PROGRAM.
           DISPLAY "***************************************************"
           DISPLAY "*              SE PRODUJO UN ERROR                *"
           DISPLAY "***************************************************"
           DISPLAY "PARRAFO : "   WS-ERR-PARRAFO
           DISPLAY "OBJETO : "    WS-ERR-OBJETO
           DISPLAY "OPERACION : " WS-ERR-OPERACION
           DISPLAY "CODIGO : "    WS-ERR-CODIGO
           STOP RUN
           .
       399999-END-PROGRAM-F. EXIT. 
       END PROGRAM E37.