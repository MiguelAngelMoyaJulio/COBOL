      ******************************************************************
      *Se tiene un archivo maestro de cuentas de un Banco y otro con las
      *novedades de depósitos de dichas cuentas,
      *ordenados en forma ascendente por Nº de cuenta. Se desea obtener
      *un listado como el que se muestra en
      *salida, siguiendo las especificaciones del proceso.
      *ENTRADA:
      *Archivo maestro (1 registro por cuenta – ordenado secuencial 
      *scendente por nrocuenta)
      *NROCUENTA NOMBRECLTE SALDO
      *Archivo de novedades: (1, varios o ningún registro por cuenta)
      *NROCUENTA IMPORTE
      *2
      *PROCESO:
      *Deberá aparear estos archivos a efectos de:
      *1. Actualizar el saldo del maestro para lo cual deberá sumar 
      *os importes de la novedad.
      *2. Detectar la cuenta con menor saldo.
      *3. Realizar el siguiente informe, colocando un “*” en 
      *bservaciones, para las cuentas sin
      *movimientos.
      ******************************************************************
      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E38.
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
       SELECT MASTER ASSIGN TO "MASTER.txt"
                     FILE STATUS IS FS-STATUS1
                     ORGANIZATION IS LINE SEQUENTIAL.
       
       SELECT NEWS ASSIGN TO "NEWS.txt"
                     FILE STATUS IS FS-STATUS2
                     ORGANIZATION IS LINE SEQUENTIAL.
       
       SELECT MASTER-UPDATE ASSIGN TO "MASTER_UPDATE.txt"
                     FILE STATUS IS FS-STATUS3
                     ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD MASTER.
          01 REG-MASTER.
             05 REG-ACCOUNT           PIC 9(02).
             05 REG-NAME-CLIENT       PIC X(06).
             05 REG-BALANCE           PIC 9(04)V9(02).
       
       FD NEWS.
          01 REG-NEWS.
             05 REG-ACCOUNT-N         PIC 9(02).
             05 REG-AMOUNT-N          PIC 9(04)V9(02).
       
       FD MASTER-UPDATE.
          01 REG-MASTER-UPDATE.              
             05 REG-ACCOUNT-U         PIC 9(02).
             05 REG-NAME-CLIENT-U     PIC X(06).
             05 REG-BALANCE-U         PIC 9(07)V9(02).

      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
      **************************  SWITCHES  ****************************
       01 WS-SWITCHES.       
          05 FS-STATUS1               PIC X(02) VALUE "00".
             88 FS-STATUS1-OK                   VALUE "00".
             88 FS-STATUS1-EOF                  VALUE "10".
          05 FS-STATUS2               PIC X(02) VALUE "00".
             88 FS-STATUS2-OK                   VALUE "00".
             88 FS-STATUS2-EOF                  VALUE "10".
          05 FS-STATUS3               PIC X(02) VALUE "00".
             88 FS-STATUS3-OK                   VALUE "00".
             88 FS-STATUS3-EOF                  VALUE "10".
      ************************** VARIABLES *****************************
       01 WS-VAR.
          02 WS-MEDIDOR-ANT           PIC 9(02).
          02 WS-MATING.
             05 WS-CODE-M             PIC 9(02).
             05 WS-CODE-N             PIC 9(02).
          02 WS-BILLING.   
             05 WS-BALANCE-UPDATE     PIC 9(07)V9(02).  
          02 WS-MIN.
             05 WS-MIN-BALANCE        PIC 9(04)V9(02).
             05 WS-MIN-ACCOUNT        PIC 9(02).
             05 WS-MIN-I              PIC 9(02).
       01 WS-TITLE.
          02 FILLER                   PIC X(07) VALUE "ACCOUNT". 
          02 FILLER                   PIC X(03) VALUE SPACES. 
          02 FILLER                   PIC X(11) VALUE "CLIENT NAME". 
          02 FILLER                   PIC X(04) VALUE SPACES. 
          02 FILLER                   PIC X(07) VALUE "BALANCE". 
          02 FILLER                   PIC X(03) VALUE SPACES. 
          02 FILLER                   PIC X(12) VALUE "OBSERVATIONS". 
       
       01 WS-SUBTITLE.
          02 FILLER                   PIC X(02) VALUE SPACES. 
          02 SUB-ACCOUNT              PIC 9(02). 
          02 FILLER                   PIC X(09) VALUE SPACES. 
          02 SUB-CLIENT               PIC X(06). 
          02 FILLER                   PIC X(02) VALUE SPACES. 
          02 SUB-BALANCE              PIC -.---.--9,99. 
          02 FILLER                   PIC X(05) VALUE SPACES. 
          02 SUB-OBSERVATIONS         PIC X(01). 
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
            STOP RUN.                             
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
               DISPLAY "ERROR AL ABRIR ARCHIVO MAESTRO " FS-STATUS1
           END-IF
           .
       110000-OPEN-MASTER-F. EXIT.
      ******************************************************************
      *                         120000-OPEN-NEWS   
      ******************************************************************
       120000-OPEN-NEWS.                        
           OPEN INPUT NEWS                   
           IF NOT FS-STATUS2-OK
               DISPLAY "ERROR AL ABRIR ARCHIVO MAESTRO " FS-STATUS2
           END-IF
           .
       120000-OPEN-NEWS-F. EXIT.
      ******************************************************************
      *                         130000-OPEN-MASTER-UPDATE   
      ******************************************************************
       130000-OPEN-MASTER-UPDATE.                        
           OPEN OUTPUT MASTER-UPDATE                   
           IF NOT FS-STATUS3-OK
               DISPLAY "ERROR AL ABRIR ARCHIVO MASTER UPDATE " 
                       FS-STATUS3
           END-IF
           .
       130000-OPEN-MASTER-UPDATE-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS   
      ****************************************************************** 
       200000-PROCESS.
           MOVE ZEROES                 TO WS-BALANCE-UPDATE
           IF WS-CODE-M = WS-CODE-N
               MOVE REG-BALANCE        TO WS-BALANCE-UPDATE
               PERFORM UNTIL WS-CODE-M <> WS-CODE-N
                       COMPUTE WS-BALANCE-UPDATE = WS-BALANCE-UPDATE +
                               REG-AMOUNT-N
                       PERFORM 220000-READ-NEWS                       
                          THRU 220000-READ-NEWS-F
               END-PERFORM
               
               PERFORM 240000-FINDING-MIN
                  THRU 240000-FINDING-MIN-F 

               MOVE REG-ACCOUNT        TO SUB-ACCOUNT
               MOVE REG-NAME-CLIENT    TO SUB-CLIENT
               MOVE REG-BALANCE        TO SUB-BALANCE
               MOVE SPACES             TO SUB-OBSERVATIONS
               DISPLAY WS-SUBTITLE

               PERFORM 230000-WRITE-MASTER-UPDATE
                  THRU 230000-WRITE-MASTER-UPDATE-F
               
               PERFORM 210000-READ-MASTER                       
                  THRU 210000-READ-MASTER-F                     
           ELSE
               IF WS-CODE-M > WS-CODE-N
                  MOVE WS-CODE-N       TO WS-MEDIDOR-ANT
                  PERFORM UNTIL WS-MEDIDOR-ANT <> WS-CODE-N
                          PERFORM 220000-READ-NEWS                       
                             THRU 220000-READ-NEWS-F 
                  END-PERFORM
                  PERFORM 220000-READ-NEWS                       
                     THRU 220000-READ-NEWS-F 
               ELSE
                  MOVE REG-BALANCE     TO WS-BALANCE-UPDATE
                  
                  PERFORM 240000-FINDING-MIN
                     THRU 240000-FINDING-MIN-F                   

                  MOVE REG-ACCOUNT     TO SUB-ACCOUNT
                  MOVE REG-NAME-CLIENT TO SUB-CLIENT
                  MOVE REG-BALANCE     TO SUB-BALANCE
                  MOVE "*"             TO SUB-OBSERVATIONS
                  DISPLAY WS-SUBTITLE

                  PERFORM 230000-WRITE-MASTER-UPDATE
                     THRU 230000-WRITE-MASTER-UPDATE-F

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
                    MOVE REG-ACCOUNT TO WS-CODE-M
               WHEN FS-STATUS1-EOF
                    MOVE 99          TO WS-CODE-M
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
                    MOVE REG-ACCOUNT-N TO WS-CODE-N
               WHEN FS-STATUS2-EOF
                    MOVE 99            TO WS-CODE-N
           END-EVALUATE
           .
       220000-READ-NEWS-F. EXIT.
      ******************************************************************
      *                         230000-WRITE-MASTER-UPDATE   
      ******************************************************************      
       230000-WRITE-MASTER-UPDATE.
           INITIALIZE REG-MASTER-UPDATE
           MOVE REG-ACCOUNT        TO REG-ACCOUNT-U
           MOVE REG-NAME-CLIENT    TO REG-NAME-CLIENT-U
           MOVE WS-BALANCE-UPDATE  TO REG-BALANCE-U
           
           WRITE REG-MASTER-UPDATE
           IF NOT FS-STATUS3-OK
               DISPLAY "ERROR AL GRABAR MAESTRO-UPDATE" FS-STATUS3
           END-IF 
           .
       230000-WRITE-MASTER-UPDATE-F. EXIT.
      ******************************************************************
      *                         240000-FINDING-MIN   
      ******************************************************************      
       240000-FINDING-MIN.
           IF WS-MIN-I = 0
              ADD  1                 TO WS-MIN-I
              MOVE REG-ACCOUNT       TO WS-MIN-ACCOUNT
              MOVE WS-BALANCE-UPDATE TO WS-MIN-BALANCE
           END-IF  

           IF WS-BALANCE-UPDATE < WS-MIN-BALANCE 
              MOVE REG-ACCOUNT       TO WS-MIN-ACCOUNT
              MOVE WS-BALANCE-UPDATE TO WS-MIN-BALANCE
           END-IF
           .
       240000-FINDING-MIN-F. EXIT.
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

           PERFORM 340000-TOTALS
              THRU 340000-TOTALS-F   
           .    
       300000-END-F. EXIT.
      ******************************************************************
      *                         310000-CLOSE-MASTER   
      ****************************************************************** 
       310000-CLOSE-MASTER.
           CLOSE MASTER
           IF NOT FS-STATUS1-OK
               DISPLAY "ERROR AL CERRAR ARCHIVO MASTER " FS-STATUS1
           END-IF
           .
       310000-CLOSE-MASTER-F. EXIT.
      ******************************************************************
      *                         320000-CLOSE-NEWS   
      ****************************************************************** 
       320000-CLOSE-NEWS.
           CLOSE NEWS
           IF NOT FS-STATUS2-OK
               DISPLAY "ERROR AL CERRAR ARCHIVO NEWS " FS-STATUS2
           END-IF
           .
       320000-CLOSE-NEWS-F. EXIT.
      ******************************************************************
      *                         330000-CLOSE-MASTER-UPDATE   
      ****************************************************************** 
       330000-CLOSE-MASTER-UPDATE.
           CLOSE MASTER-UPDATE
           IF NOT FS-STATUS3-OK
               DISPLAY "ERROR AL CERRAR ARCHIVO MASTER-UPDATE " 
                       FS-STATUS3
           END-IF
           .
       330000-CLOSE-MASTER-UPDATE-F. EXIT.
      ******************************************************************
      *                         340000-TOTALS   
      ****************************************************************** 
       340000-TOTALS.
           MOVE WS-MIN-BALANCE TO SUB-BALANCE
           DISPLAY "MIN BALANCE : "SUB-BALANCE
           DISPLAY "ACCOUNT : " WS-MIN-ACCOUNT
           .
       340000-TOTALS-F. EXIT.
       END PROGRAM E38.