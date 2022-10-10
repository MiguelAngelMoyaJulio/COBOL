      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-07
      * Un buque de carga traslada 100 contenedores a 3 diferentes puer-
      * tos del paísidentificados con los números 1, 2 y 3.
      * Por cada uno de los contenedores trasladados por el buque se 
      * registran los siguientes datos:
      *•	Identificación del contenedor: idCont.
      *•	Peso del contenedor en (en kilos): peso.
      *•	Puerto de arribo (un valor de 1 a 3): idPuerto. 
      * Se pide calcular e informar:
      *•	El peso total que el buque debe trasladar.
      *•	El contenedor de mayor peso.
      *•	La cantidad de contenedores que se trasladarán a cada puerto
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E14.
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
              05 REG-ID-CONT  PIC 9(05).
              05 REG-ID-PORT  PIC 9(01).
              05 REG-WEIGHT   PIC 9(02)V99.
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
           02 WS-AMOUNT-P1              PIC 9(05).
           02 WS-AMOUNT-P2              PIC 9(05).
           02 WS-AMOUNT-P3              PIC 9(05).
           02 WS-ID-MAX-WEIGHT          PIC 9(05).
           02 WS-PA-MAX-WEIGHT          PIC 9(02)V99.
           02 WS-TOTAL-WEIGHT           PIC 9(04)V99.
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

           MOVE REG-WEIGHT  TO  WS-PA-MAX-WEIGHT
           MOVE REG-ID-CONT TO WS-ID-MAX-WEIGHT
           .                                      
       100000-START-F. EXIT.
      ******************************************************************
      *                         110000-OPEN-DATOS   
      ******************************************************************
       110000-OPEN-DATOS.                        
           OPEN INPUT DATOS                   
           IF NOT FS-STATUS-OK
               DISPLAY "ERROR AL ABRIR ARCHIVO MAESTRO " FS-STATUS
           END-IF
           .
       110000-OPEN-DATOS-F. EXIT.     
      ******************************************************************
      *                         200000-PROCESS   
      ****************************************************************** 
       200000-PROCESS.
           COMPUTE WS-TOTAL-WEIGHT = WS-TOTAL-WEIGHT + REG-WEIGHT
   
           IF REG-WEIGHT > WS-PA-MAX-WEIGHT
               MOVE REG-WEIGHT  TO WS-PA-MAX-WEIGHT
               MOVE REG-ID-CONT TO WS-ID-MAX-WEIGHT
           END-IF
   
           EVALUATE TRUE
           WHEN REG-ID-PORT = 1
                ADD 1 TO WS-AMOUNT-P1
           WHEN REG-ID-PORT = 2
                ADD 1 TO WS-AMOUNT-P2
           WHEN REG-ID-PORT = 3
                ADD 1 TO WS-AMOUNT-P3
           END-EVALUATE

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
           DISPLAY "TOTAL WEIGHT - TON "        WS-TOTAL-WEIGHT           
           DISPLAY "MAX WEIGHT - ID CONTAINER " WS-ID-MAX-WEIGHT           
           DISPLAY "AMOUNT OF CONTS TO PORT 1 " WS-AMOUNT-P1           
           DISPLAY "AMOUNT OF CONTS TO PORT 2 " WS-AMOUNT-P2           
           DISPLAY "AMOUNT OF CONTS TO PORT 3 " WS-AMOUNT-P3           
           .
       320000-TOTAL-F. EXIT.

       END PROGRAM E14.