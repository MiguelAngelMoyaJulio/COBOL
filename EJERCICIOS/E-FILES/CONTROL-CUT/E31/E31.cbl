      ******************************************************************
      * En las últimas elecciones, se desea conocer  
      *los totales para cada candidato por provincia.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E31.
       AUTHOR. MIGUEL MOYA.
       DATE-WRITTEN. SEPTEMBER 2022.
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
             05 REG-CANDIDATE              PIC 9(02).
             05 REG-PROVINCE               PIC X(38).
             05 REG-VOTES                  PIC 9(05).
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
       01 WS-VAR.
          02 WS-CANDIDATE-ANT           PIC 9(02).
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
             
           MOVE REG-CANDIDATE TO WS-CANDIDATE-ANT
           
           DISPLAY "CANDIDATE : " WS-CANDIDATE-ANT
           DISPLAY "PROVINCE" "                                   " 
                   "VOTES" 
           PERFORM UNTIL WS-CANDIDATE-ANT <> REG-CANDIDATE
             DISPLAY REG-PROVINCE "     " REG-VOTES 
             PERFORM 210000-READ-DATOS1
                THRU 210000-READ-DATOS1-F
           END-PERFORM
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
       END PROGRAM E31.