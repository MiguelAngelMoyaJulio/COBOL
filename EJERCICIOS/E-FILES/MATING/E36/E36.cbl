      ******************************************************************
      *En una farmacia, se desea actualizar el stock de 
      *medicamentos con la llegada de un lote reciente de un
      *laboratorio determinado. Ambos archivos se 
      *encuentran ordenados en forma ascendente por Código de Producto.
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
              05 CON-140000-OPEN-ERRORS      PIC X(30) VALUE 
              '140000-OPEN-ERRORS          '.
              05 CON-210000-READ-MASTER      PIC X(30) VALUE 
              '210000-READ-MASTER          '.
              05 CON-220000-READ-NEWS      PIC X(30) VALUE 
              '220000-READ-NEWS            '.
              05 CON-230000-WRITE-MASTER-UPDATE  PIC X(30) VALUE 
              '230000-WRITE-MASTER-UPDATE  '.
              05 CON-240000-WRITE-ERRORS  PIC X(30) VALUE 
              '240000-WRITE-ERRORS         '.
              05 CON-310000-CLOSE-MASTER      PIC X(30) VALUE 
              '310000-CLOSE-MASTER         '.
              05 CON-320000-CLOSE-NEWS      PIC X(30) VALUE 
              '320000-CLOSE-NEWS           '.
              05 CON-330000-CLOSE-MASTER-UPDATE      PIC X(30) VALUE 
              '330000-CLOSE-MASTER-UPDATE  '.
              05 CON-340000-CLOSE-ERRORS      PIC X(30) VALUE 
              '340000-CLOSE-ERRORS         '.
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
              05 CON-ERRORS           PIC X(10) VALUE 'ERRORS  '.
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
      *                         140000-OPEN-ERRORS   
      ******************************************************************
       140000-OPEN-ERRORS.
           OPEN OUTPUT ERRORS                   
           IF NOT FS-STATUS4-OK
              MOVE CON-140000-OPEN-ERRORS TO WS-ERR-PARRAFO 
              MOVE CON-ERRORS             TO WS-ERR-OBJETO 
              MOVE CON-ABRIR              TO WS-ERR-OPERACION 
              MOVE FS-STATUS4             TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
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
                    MOVE REG-CODE-PRO-N TO WS-CODE-N
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
      *                         240000-WRITE-ERRORS   
      ******************************************************************      
       240000-WRITE-ERRORS.
           INITIALIZE REG-ERRORS
           MOVE REG-NEWS TO REG-ERRORS
           WRITE REG-ERRORS
           IF NOT FS-STATUS4-OK
              MOVE CON-240000-WRITE-ERRORS TO WS-ERR-PARRAFO 
              MOVE CON-ERRORS              TO WS-ERR-OBJETO 
              MOVE CON-GRABAR              TO WS-ERR-OPERACION 
              MOVE FS-STATUS4              TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
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
      *                         340000-CLOSE-ERRORS   
      ****************************************************************** 
       340000-CLOSE-ERRORS.
           CLOSE ERRORS
           IF NOT FS-STATUS4-OK
              MOVE CON-340000-CLOSE-ERRORS TO WS-ERR-PARRAFO 
              MOVE CON-ERRORS              TO WS-ERR-OBJETO 
              MOVE CON-CERRAR              TO WS-ERR-OPERACION 
              MOVE FS-STATUS4              TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F 
           END-IF
           .
       340000-CLOSE-ERRORS-F. EXIT.
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
       END PROGRAM E36.