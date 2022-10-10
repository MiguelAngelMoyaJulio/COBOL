      ******************************************************************
      * Dados 100 números enteros, informar el promedio de los mayores 
      * que 100 y la suma de los que están entre que 30 y 60.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E11.
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
           02 WS-PROM-100  PIC 9(05).
           02 WS-SUM-100   PIC 9(05).
           02 WS-TOT-100   PIC 9(05).
           02 WS-SUM-INT   PIC 9(05).
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
           IF REG-NUM > 100
               ADD 1 TO WS-TOT-100
               COMPUTE WS-SUM-100 = WS-SUM-100 + REG-NUM 
           END-IF
       
           IF REG-NUM > 30 AND REG-NUM < 60
              COMPUTE WS-SUM-INT = WS-SUM-INT + REG-NUM 
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
                    CONTINUE   
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
           COMPUTE WS-PROM-100 = WS-SUM-100 / WS-TOT-100
           DISPLAY "AVERAGE OF NUMBERS GREATER THAN 100 " WS-PROM-100           
           DISPLAY "SUMMATION OF NUMBERS BETWEEN 30-60 " WS-SUM-INT
           .
       320000-TOTAL-F. EXIT.  

       END PROGRAM E11.
