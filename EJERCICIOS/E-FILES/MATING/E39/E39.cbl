      ******************************************************************
      *En una compañía aérea , se desea actualizar el cupo de asientos 
      *libres del vuelo AR1627 a Ushuaia del
      *Boeing 767-400 con capacidad de 318 asientos para pasajeros. 
      *Se recibe la lista de boletos vendidos para un
      *día determinado . Ambos archivos están ordenados por nro de 
      *vuelo y nro de asiento en orden ascendente. Se
      *pide listar la disposición completa del avión asignando 
      *nro de asiento y nombre de cada pasajero y poniendo la
      *palabra “disponible” en los que no se hayan vendido aún.
      *Al finalizar , mostrar los totales de control de Cantidad
      *de asientos totales, vendidos y disponibles.
      *Entrada
      *Archivo VUELO_AR1627 contiene el detalle del mapa de 
      *asientos del vuelo(ordenado secuencial
      *ascendente por NROVUELO, ASIENTO) - En el campo VENDIDO ,
      *el valor default es 0 y es 1 si el asiento
      *está asignado.
      *NROVUELO ASIENTO VENDIDO
      *Archivo VENTAS contiene el detalle de los boletos 
      *emitidos (ordenado secuencial ascendente por
      *NROVUELO, ASIENTO)
      *NROVUELO ASIENTO NROBOLETO NOMBRE PASAJERO
      ******************************************************************
      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E39.
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
             05 REG-FLIGHT           PIC X(04).
             05 REG-SEAT             PIC X(02).
             05 REG-SEAT-SOLD        PIC 9(01).
       
       FD NEWS.
          01 REG-NEWS.
             05 REG-FLIGHT-N         PIC X(04).
             05 REG-SEAT-N           PIC X(02).
             05 REG-TICKET-N         PIC 9(04).
             05 REG-NAME-N           PIC X(06).

       FD MASTER-UPDATE.
          01 REG-MASTER-UPDATE.               
             05 REG-FLIGHT-U         PIC X(04).
             05 REG-SEAT-U           PIC X(02).
             05 REG-SEAT-SOLD-U      PIC 9(01).
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
      ************************  CONSTANTES  ****************************
       01 WS-CON.       
          05 WS-CON-1                 PIC 9(01) VALUE 1.
          05 WS-CON-AVALIABLE         PIC X(09) VALUE "AVALIABLE".
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
          02 WS-MATING.
             05 WS-CODE-M             PIC X(06).
             05 WS-CODE-N             PIC X(06).
          02 WS-TOTALS.
             05 WS-TOTAL-SEAT         PIC 9(03).
             05 WS-SEAT-AVALIABLE     PIC 9(03).
             05 WS-SEAT-NOT-AVALIABLE PIC 9(03).
          02 WS-STATUS-SEAT           PIC 9(01).
       01 WS-TITLE.
          02 FILLER                   PIC X(04) VALUE "SEAT". 
          02 FILLER                   PIC X(03) VALUE SPACES. 
          02 FILLER                   PIC X(14) VALUE "PASSENGER NAME". 
       
       01 WS-SUBTITLE.
          02 FILLER                   PIC X(01) VALUE SPACES. 
          02 SUB-SEAT                 PIC X(02). 
          02 FILLER                   PIC X(07) VALUE SPACES. 
          02 SUB-NAME                 PIC X(09). 
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
           IF WS-CODE-M = WS-CODE-N
               ADD 1 TO WS-SEAT-NOT-AVALIABLE
               ADD 1 TO WS-TOTAL-SEAT

               MOVE WS-CON-1 TO WS-STATUS-SEAT

               PERFORM 240000-DISPLAY-DATA
                  THRU 240000-DISPLAY-DATA-F

               PERFORM 230000-WRITE-MASTER-UPDATE
                  THRU 230000-WRITE-MASTER-UPDATE-F
               
               PERFORM 210000-READ-MASTER                       
                  THRU 210000-READ-MASTER-F                     
               
               PERFORM 220000-READ-NEWS                       
                  THRU 220000-READ-NEWS-F
           ELSE
               IF WS-CODE-M > WS-CODE-N

                  PERFORM 220000-READ-NEWS                       
                     THRU 220000-READ-NEWS-F 
               ELSE
                  ADD 1 TO WS-TOTAL-SEAT
                  MOVE REG-SEAT-SOLD TO WS-STATUS-SEAT
                  
                  PERFORM 240000-DISPLAY-DATA
                     THRU 240000-DISPLAY-DATA-F

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
                    MOVE REG-FLIGHT  TO WS-CODE-M(1:4)
                    MOVE REG-SEAT    TO WS-CODE-M(5:2)
               WHEN FS-STATUS1-EOF
                    MOVE "9999Z9"    TO WS-CODE-M
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
                    MOVE REG-FLIGHT-N TO WS-CODE-N(1:4)
                    MOVE REG-SEAT-N   TO WS-CODE-N(5:2)
               WHEN FS-STATUS2-EOF
                    MOVE "9999Z9"     TO WS-CODE-N
           END-EVALUATE
           .
       220000-READ-NEWS-F. EXIT.
      ******************************************************************
      *                         230000-WRITE-MASTER-UPDATE   
      ******************************************************************      
       230000-WRITE-MASTER-UPDATE.
           INITIALIZE REG-MASTER-UPDATE
           MOVE REG-FLIGHT         TO REG-FLIGHT-U
           MOVE REG-SEAT           TO REG-SEAT-U
           MOVE WS-STATUS-SEAT     TO REG-SEAT-SOLD-U
           
           WRITE REG-MASTER-UPDATE
           IF NOT FS-STATUS3-OK
               DISPLAY "ERROR AL GRABAR MAESTRO-UPDATE" FS-STATUS3
           END-IF 
           .
       230000-WRITE-MASTER-UPDATE-F. EXIT.
      ******************************************************************
      *                         240000-DISPLAY-DATA   
      ******************************************************************      
       240000-DISPLAY-DATA.
           INITIALIZE WS-SUBTITLE
           IF WS-STATUS-SEAT = 1
              MOVE REG-SEAT          TO SUB-SEAT
              MOVE REG-NAME-N        TO SUB-NAME
           ELSE   
              MOVE REG-SEAT          TO SUB-SEAT
              MOVE WS-CON-AVALIABLE  TO SUB-NAME
           END-IF     
           DISPLAY WS-SUBTITLE
           .
       240000-DISPLAY-DATA-F. EXIT.
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
           COMPUTE WS-SEAT-AVALIABLE = WS-TOTAL-SEAT - 
                                       WS-SEAT-NOT-AVALIABLE
           DISPLAY "TOTAL SEATS : " WS-TOTAL-SEAT
           DISPLAY "SEATS SOLD : " WS-SEAT-NOT-AVALIABLE
           DISPLAY "SEATS AVALIABLE : " WS-SEAT-AVALIABLE
           .
       340000-TOTALS-F. EXIT.
       END PROGRAM E39.