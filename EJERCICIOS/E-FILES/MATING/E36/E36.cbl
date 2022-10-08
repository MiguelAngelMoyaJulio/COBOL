      ******************************************************************
      * En las últimas elecciones, se desea conocer  
      *los totales para cada candidato por provincia.
      ******************************************************************
      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E36.
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
       SELECT MASTER ASSIGN TO "MASTER.txt"
                     FILE STATUS IS FS-STATUS1
                     ORGANIZATION IS LINE SEQUENTIAL.
       
       SELECT NEWS ASSIGN TO "NEWS.txt"
                     FILE STATUS IS FS-STATUS2
                     ORGANIZATION IS LINE SEQUENTIAL.
       
       SELECT MASTER-UPDATE ASSIGN TO "MASTER_UPDATE.txt"
                     FILE STATUS IS FS-STATUS3
                     ORGANIZATION IS LINE SEQUENTIAL.
       
       SELECT ERRORS ASSIGN TO "ERRORS.txt"
                     FILE STATUS IS FS-STATUS4
                     ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD MASTER.
          01 REG-MASTER.
             05 REG-CODE-PRO                PIC 9(02).
             05 REG-NAME-PRO                PIC X(03).
             05 REG-AMOUNT                  PIC 9(02).
       
       FD NEWS.
          01 REG-NEWS.
             05 REG-CODE-PRO-N              PIC 9(02).
             05 REG-NAME-PRO-N              PIC X(03).
             05 REG-AMOUNT-N                PIC 9(02).
       
       FD MASTER-UPDATE.
          01 REG-MASTER-UPDATE.              
             05 REG-CODE-PRO-U              PIC 9(02).
             05 REG-NAME-PRO-U              PIC X(03).
             05 REG-AMOUNT-U                PIC 9(03).

       FD ERRORS.
          01 REG-ERRORS                     PIC X(07).              
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
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
          05 FS-STATUS4                    PIC X(02) VALUE "00".
             88 FS-STATUS4-OK                        VALUE "00".
             88 FS-STATUS4-EOF                       VALUE "10".
      ************************** VARIABLES *****************************
       01 WS-VAR.
          02 WS-CODE-M                  PIC 9(02).
          02 WS-CODE-N                  PIC 9(02).
          02 WS-CAN-UPDATE              PIC 9(03).

       01 WS-TITLE.
          02 FILLER                   PIC X(11) VALUE "COD.PRODUCT". 
          02 FILLER                   PIC X(03) VALUE SPACES. 
          02 FILLER                   PIC X(12) VALUE "PRODUCT NAME". 
          02 FILLER                   PIC X(03) VALUE SPACES. 
          02 FILLER                   PIC X(05) VALUE "STOCK". 
       
       01 WS-SUBTITLE.
          02 FILLER                   PIC X(05) VALUE SPACES. 
          02 SUB-CODE                 PIC X(02). 
          02 FILLER                   PIC X(12) VALUE SPACES. 
          02 SUB-NAME                 PIC X(03). 
          02 FILLER                   PIC X(09) VALUE SPACES. 
          02 SUB-STOCK                PIC X(03). 
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
           
           PERFORM 140000-OPEN-ERRORS                
              THRU 140000-OPEN-ERRORS-F
                            
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
      *                         140000-OPEN-ERRORS   
      ******************************************************************
       140000-OPEN-ERRORS.
           OPEN OUTPUT ERRORS                   
           IF NOT FS-STATUS4-OK
               DISPLAY "ERROR AL ABRIR ARCHIVO ERRORS " FS-STATUS4
           END-IF                        
           .
       140000-OPEN-ERRORS-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS   
      ****************************************************************** 
       200000-PROCESS.
           IF WS-CODE-M = WS-CODE-N
               COMPUTE WS-CAN-UPDATE = REG-AMOUNT-N + REG-AMOUNT

               MOVE REG-CODE-PRO  TO SUB-CODE
               MOVE REG-NAME-PRO  TO SUB-NAME
               MOVE WS-CAN-UPDATE TO SUB-STOCK
               DISPLAY WS-SUBTITLE

               INITIALIZE REG-MASTER-UPDATE
               MOVE REG-MASTER(1:5) TO REG-MASTER-UPDATE(1:5)
               MOVE WS-CAN-UPDATE   TO REG-AMOUNT-U

               PERFORM 230000-WRITE-MASTER-UPDATE
                  THRU 230000-WRITE-MASTER-UPDATE-F
               
               PERFORM 210000-READ-MASTER                       
                  THRU 210000-READ-MASTER-F                     
           
               PERFORM 220000-READ-NEWS                       
                  THRU 220000-READ-NEWS-F
           ELSE
               IF WS-CODE-M > WS-CODE-N

                  PERFORM 240000-WRITE-ERRORS
                     THRU 240000-WRITE-ERRORS-F                  

                  PERFORM 220000-READ-NEWS                       
                     THRU 220000-READ-NEWS-F 
               ELSE
                  INITIALIZE REG-MASTER-UPDATE
                  MOVE REG-MASTER(1:5) TO REG-MASTER-UPDATE(1:5)
                  MOVE REG-MASTER(6:2) TO REG-AMOUNT-U
                  
                  PERFORM 230000-WRITE-MASTER-UPDATE
                     THRU 230000-WRITE-MASTER-UPDATE-F

                  MOVE REG-CODE-PRO  TO SUB-CODE
                  MOVE REG-NAME-PRO  TO SUB-NAME
                  MOVE REG-AMOUNT    TO SUB-STOCK
                  DISPLAY WS-SUBTITLE 
                  
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
                    MOVE REG-CODE-PRO TO WS-CODE-M
               WHEN FS-STATUS1-EOF
                    MOVE 99 TO WS-CODE-M
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
                    MOVE REG-CODE-PRO-N TO WS-CODE-N
               WHEN FS-STATUS2-EOF
                    MOVE 99 TO WS-CODE-N
           END-EVALUATE
           .
       220000-READ-NEWS-F. EXIT.
      ******************************************************************
      *                         230000-WRITE-MASTER-UPDATE   
      ******************************************************************      
       230000-WRITE-MASTER-UPDATE.
           WRITE REG-MASTER-UPDATE
           IF NOT FS-STATUS3-OK
               DISPLAY "ERROR AL GRABAR MAESTRO-UPDATE" FS-STATUS3
           END-IF 
           .
       230000-WRITE-MASTER-UPDATE-F. EXIT.
      ******************************************************************
      *                         240000-WRITE-ERRORS   
      ******************************************************************      
       240000-WRITE-ERRORS.
           INITIALIZE REG-ERRORS
           MOVE REG-NEWS TO REG-ERRORS
           WRITE REG-ERRORS
           IF NOT FS-STATUS4-OK
               DISPLAY "ERROR AL GRABAR ERROR" FS-STATUS4
           END-IF 
           .
       240000-WRITE-ERRORS-F. EXIT.
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

           PERFORM 340000-CLOSE-ERRORS
              THRU 340000-CLOSE-ERRORS-F
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
      *                         340000-CLOSE-ERRORS   
      ****************************************************************** 
       340000-CLOSE-ERRORS.
           CLOSE ERRORS
           IF NOT FS-STATUS4-OK
               DISPLAY "ERROR AL CERRAR ARCHIVO ERROR " FS-STATUS4
           END-IF
           .
       340000-CLOSE-ERRORS-F. EXIT.
       END PROGRAM E36.