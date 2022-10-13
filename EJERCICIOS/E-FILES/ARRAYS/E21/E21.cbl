      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-14
      *Ingresar un conjunto VEC de 40 elementos.
      *Determinar e imprimir el valor máximo y la posición del 
      *mismo dentro del conjunto. Si el máximo no es
      *único, imprimir todas las posiciones en que se encuentra.
      ******************************************************************
      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E21.
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
       SELECT DATOS ASSIGN TO "DAT.txt"
                     FILE STATUS IS FS-STATUS-FILE
                     ORGANIZATION IS LINE SEQUENTIAL. 
       
      ****************************  OUTPUT  ****************************

       DATA DIVISION.
       FILE SECTION.
       FD DATOS.
           01 REG-DATOS.
               05 REG-NUM              PIC 9(02).
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
      ************************  CONSTANTS  *****************************
       01 WS-CONSTANTS.       
          05 CON-1                         PIC 9(01) VALUE 1.
      ************************** TABLES ********************************
       01 WS-VECTOR OCCURS 40 TIMES.
          02 WS-NUM               PIC 9(02).                
      **************************  SWITCHES  ****************************
       01 WS-SWITCHES.       
          05 FS-STATUS-FILE                PIC X(02) VALUE "00".
             88 FS-STATUS-FILE-OK                    VALUE "00".
             88 FS-STATUS-FILE-EOF                   VALUE "10".
      ************************** VARIABLES *****************************
       01 WS-VARIABLES.
          02 WS-I                     PIC 9(03).
          02 WS-MAX                   PIC 9(02).
      ******************************************************************
      *                       LINKAGE SECTION   
      ****************************************************************** 
       LINKAGE SECTION.        
      ******************************************************************
      *                      PROCEDURE DIVISION   
      ****************************************************************** 
       PROCEDURE DIVISION.
           PERFORM 100000-START                      
              THRU 100000-START-F                    
                                                  
           PERFORM 200000-PROCESS                     
              THRU 200000-PROCESS-F                   
                                                  
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
           .                                      
       100000-START-F. EXIT.
      ******************************************************************
      *                         110000-OPEN-DATOS   
      ******************************************************************
       110000-OPEN-DATOS.                        
           OPEN INPUT DATOS                   
           IF NOT FS-STATUS-FILE-OK
               DISPLAY "ERROR AL ABRIR ARCHIVO MAESTRO " FS-STATUS-FILE
           END-IF
           .
       110000-OPEN-DATOS-F. EXIT.        
      ******************************************************************
      *                         200000-PROCESS   
      ****************************************************************** 
       200000-PROCESS.
           ADD 1 TO WS-I     
           MOVE REG-NUM TO WS-NUM(WS-I)

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
               WHEN FS-STATUS-FILE-OK
                    CONTINUE   
               WHEN FS-STATUS-FILE-EOF
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

           PERFORM 320000-CALCULAR-MAX
              THRU 320000-CALCULAR-MAX-F

           PERFORM 330000-POSICION-MAXIMO
              THRU 330000-POSICION-MAXIMO-F
           STOP RUN 
           .    
       300000-END-F. EXIT.
      ******************************************************************
      *                         310000-CLOSE-DATOS   
      ****************************************************************** 
       310000-CLOSE-DATOS.
           CLOSE DATOS
           IF NOT FS-STATUS-FILE-OK
               DISPLAY "ERROR AL CERRAR ARCHIVO DATOS " FS-STATUS-FILE
           END-IF
           .
       310000-CLOSE-DATOS-F. EXIT.  
      ******************************************************************
      *                         320000-CALCULAR-MAX   
      ****************************************************************** 
       320000-CALCULAR-MAX.
           MOVE ZEROS TO WS-MAX
           PERFORM VARYING WS-I FROM 1 
           BY 1 UNTIL WS-I > 40

               IF WS-NUM(WS-I) > WS-MAX
                   MOVE WS-NUM(WS-I) TO WS-MAX
               END-IF 
           
           END-PERFORM

           DISPLAY "MAX'S NUMBER : " WS-MAX
           .
       320000-CALCULAR-MAX-F. EXIT.  
      ******************************************************************
      *                         330000-POSICION-MAXIMO   
      ****************************************************************** 
       330000-POSICION-MAXIMO.
           DISPLAY "MAX'S POSITION/S "

           PERFORM VARYING WS-I FROM 1 
           BY 1 UNTIL WS-I > 40
               IF WS-MAX = WS-NUM(WS-I)
                   DISPLAY WS-I
               END-IF    
           END-PERFORM
           .
       330000-POSICION-MAXIMO-F. EXIT.  

       END PROGRAM E21.