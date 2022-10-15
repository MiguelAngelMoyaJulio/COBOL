      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-15
      * CORTE DE CONTROL POR SUCURSAL Y VENDEDOR
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E27.
       AUTHOR. MIGUEL MOYA.
       DATE-WRITTEN. OCTOBER 2022.
       DATE-COMPILED. OCTOBER 2022.
      ******************************************************************
      *                     ENVIRONMENT DIVISION
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      ******************************************************************
      *                            FILES   
      ******************************************************************
      *****************************  INPUT  **************************** 
       SELECT DATOS1 ASSIGN TO "SUC-VEN.txt"
                     FILE STATUS IS FS-STATUS-FILE
                     ORGANIZATION IS LINE SEQUENTIAL.
      ****************************  OUTPUT  **************************** 

       DATA DIVISION.
       FILE SECTION.
       FD DATOS1.
           01 REG-DATOS1.
               05 REG-SUCURSAL              PIC 9(02).
               05 REG-VENDEDOR              PIC 9(02).
               05 REG-MONTO                 PIC 9(07)V99.
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************         
       WORKING-STORAGE SECTION.
      ************************  CONSTANTS  *****************************

      ************************** TABLES ********************************

      **************************  SWITCHES  **************************** 
       01 FS-STATUS-FILE               PIC X(02) VALUE "00".
           88 FS-STATUS-FILE-OK                  VALUE "00".
           88 FS-STATUS-FILE-EOF                 VALUE "10".

      ************************** VARIABLES *****************************
       01 WS-VAR.
           02 WS-VENDEDOR-ANT          PIC 9(02).
           02 WS-TOT-VENDEDOR          PIC 9(07)V99.
           02 WS-SUCURSAL-ANT          PIC 9(02).
           02 WS-TOT-SUC               PIC 9(07)V99.
           02 WS-TOT-EMPRESA           PIC 9(07)V99.
           02 WS-TOTE-MA               PIC ZZ,ZZZ,ZZZ.ZZ.
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
      *                         200000-PROCESS   
      ****************************************************************** 
       200000-PROCESS.  
           MOVE REG-SUCURSAL TO WS-SUCURSAL-ANT
           INITIALIZE WS-TOT-SUC    
           
           PERFORM UNTIL REG-SUCURSAL <> WS-SUCURSAL-ANT
           
               MOVE REG-VENDEDOR TO WS-VENDEDOR-ANT
               INITIALIZE WS-TOT-VENDEDOR
               
               PERFORM UNTIL WS-VENDEDOR-ANT <> REG-VENDEDOR

                   ADD REG-MONTO TO WS-TOT-VENDEDOR
                   PERFORM 210000-READ-DATOS1
                      THRU 210000-READ-DATOS1-F
                      
               END-PERFORM
               
               MOVE WS-TOT-VENDEDOR TO WS-TOTE-MA   
               DISPLAY "TOTAL VENDEDOR " WS-VENDEDOR-ANT " : $" 
                                                              WS-TOTE-MA   
               COMPUTE WS-TOT-SUC = WS-TOT-SUC + WS-TOT-VENDEDOR
           END-PERFORM  

           MOVE WS-TOT-SUC TO WS-TOTE-MA
           ADD  WS-TOT-SUC TO WS-TOT-EMPRESA   
           DISPLAY "TOTAL " WS-SUCURSAL-ANT " : $" WS-TOTE-MA
           .         
       200000-PROCESS-F. EXIT.
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
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           PERFORM 310000-CLOSE-DATOS1
              THRU 310000-CLOSE-DATOS1-F

           PERFORM 30-VENTA-TOTAL
              THRU 30-VENTA-TOTAL-F
           STOP RUN
           .    
       300000-END-F. EXIT.
      ******************************************************************
      *                         310000-CLOSE-DATOS1   
      ****************************************************************** 
       310000-CLOSE-DATOS1.
           CLOSE DATOS1
           IF NOT FS-STATUS-FILE-OK
               DISPLAY "ERROR AL CERRAR ARCHIVO " FS-STATUS-FILE
           END-IF
           .
       310000-CLOSE-DATOS1-F. EXIT.
      ******************************************************************
      *                         30-VENTA-TOTAL   
      ****************************************************************** 
       30-VENTA-TOTAL.
           MOVE WS-TOT-EMPRESA TO WS-TOTE-MA   
           DISPLAY "TOTAL EMPRESA " WS-TOTE-MA 
           .
       30-VENTA-TOTAL-F. EXIT.
       END PROGRAM E27.