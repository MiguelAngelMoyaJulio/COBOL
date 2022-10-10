      ******************************************************************
      * Dados n valores numéricos, informar el mayor, el menor y en
      * que posición del conjunto fueron ingresados cada uno de ellos.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E12.
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
       SELECT DATOS ASSIGN TO "DAT.txt"
                    FILE STATUS IS FS-STATUS 
                    ORGANIZATION IS LINE SEQUENTIAL.
       
      ****************************  OUTPUT  ****************************
       
       DATA DIVISION.
       FILE SECTION.
       FD DATOS.
          01 REG-DATOS.
             05 REG-NUM PIC 9(03).
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
      ************************  CONSTANTES  ****************************

      **************************  SWITCHES  ****************************
       01 WS-SWITCHES.       
          05 FS-STATUS               PIC X(02) VALUE "00".
             88 FS-STATUS-OK                   VALUE "00".
             88 FS-STATUS-EOF                  VALUE "10".
      ************************** VARIABLES *****************************
       01 WS-VAR.
          02 WS-MIN         PIC 9(03). 
          02 WS-MAX         PIC 9(03). 
          02 WS-POS-MAX     PIC 9(03). 
          02 WS-POS-MIN     PIC 9(03).
          02 WS-POS         PIC 9(03).
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
              UNTIL FS-STATUS-EOF                   
                                                  
           PERFORM 300000-END                         
              THRU 300000-END-F
           .
      ******************************************************************
      *                         100000-START         
      ******************************************************************
       100000-START.            
           PERFORM 110000-OPEN-DATOS
              THRU 110000-OPEN-DATOS-F
              
           PERFORM 210000-READ-DATOS
              THRU 210000-READ-DATOS-F

           MOVE REG-NUM   TO WS-MAX
           MOVE REG-NUM   TO WS-MIN                        
           .                                      
       100000-START-F. EXIT.
      ******************************************************************
      *                         110000-OPEN-DATOS   
      ******************************************************************
       110000-OPEN-DATOS.                        
           OPEN INPUT DATOS                   
           IF NOT FS-STATUS-OK
               DISPLAY "ERROR AL ABRIR ARCHIVO : " FS-STATUS
           END-IF
           .
       110000-OPEN-DATOS-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS   
      ****************************************************************** 
       200000-PROCESS.
           IF REG-NUM > WS-MAX
              MOVE REG-NUM TO WS-MAX
              MOVE WS-POS  TO WS-POS-MAX 
           END-IF

           IF REG-NUM < WS-MIN
              MOVE REG-NUM TO WS-MIN 
              MOVE WS-POS  TO WS-POS-MIN 
           END-IF
           
           PERFORM 210000-READ-DATOS
              THRU 210000-READ-DATOS-F
           .         
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         210000-READ-DATOS   
      ******************************************************************      
       210000-READ-DATOS.
           INITIALIZE REG-DATOS
           READ DATOS INTO REG-DATOS
           EVALUATE TRUE
               WHEN FS-STATUS-OK
                    ADD 1 TO WS-POS   
               WHEN FS-STATUS-EOF
                    CONTINUE
           END-EVALUATE
           .
       210000-READ-DATOS-F. EXIT.
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.

           PERFORM 310000-CLOSE-DATOS
              THRU 310000-CLOSE-DATOS-F
           
           PERFORM 320000-TOTAL
              THRU 320000-TOTAL-F

           STOP RUN 
           .    
       300000-END-F. EXIT. 
      ******************************************************************
      *                         310000-CLOSE-DATOS   
      ****************************************************************** 
       310000-CLOSE-DATOS.
           CLOSE DATOS
           IF NOT FS-STATUS-OK
               DISPLAY "ERROR AL CERRAR ARCHIVO DATOS " FS-STATUS
           END-IF
           .
       310000-CLOSE-DATOS-F. EXIT.  
      ******************************************************************
      *                         320000-TOTAL   
      ****************************************************************** 
       320000-TOTAL.
           DISPLAY "MAX NUMBER " WS-MAX           
                   "  POSITION " WS-POS-MAX           
           DISPLAY "MIN NUMBER " WS-MIN           
                   "  POSITION " WS-POS-MIN           
           .
       320000-TOTAL-F. EXIT.               

       END PROGRAM E12.
