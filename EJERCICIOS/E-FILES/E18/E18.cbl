      ******************************************************************
      * De un censo realizado en una población se conocen los siguientes
      * datos:
      *1.	Día de nacimiento (2 dígitos)
      *2.	Mes (2 dígitos)
       *3.	Año (4 dígitos)
      *4.	Sexo ('M'=masculino, 'F'=femenino)
      *Con estos datos de cada habitante se forma un lote, 
      *finalizado su ingreso con un día igual a 0.
      * Se pide desarrollar el programa que determine e imprima:
      *1.	Cuántos nacimientos hubo en el mes de octubre de 
      *todos los años.
      *2.	Cuántos nacimientos hubo antes del 9 de julio de 1990.
      *3.	Cuántos nacimientos de mujeres hubo en la primavera del 1982
      *4.	Sexo de la persona más vieja (solo existe una).
      ******************************************************************
      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E18.
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
               05 REG-DIA       PIC 9(02).
               05 REG-MES       PIC 9(02).
               05 REG-ANIO      PIC 9(04).
               05 REG-SEXO      PIC X(01).
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
      ************************  CONSTANTS  *****************************
       01 WS-CONSTANTES.
           02 CON-RUTINAS.
              05 CON-RUTINA01  PIC X(08) VALUE 'RUTINA01'.
           02 CON-PARRAFO.
              05 CON-110000-OPEN-DATOS      PIC X(30) VALUE 
              '110000-OPEN-DATOS           '.
              05 CON-210000-READ-DATOS      PIC X(30) VALUE 
              '210000-READ-DATOS           '.
              05 CON-310000-CLOSE-DATOS      PIC X(30) VALUE 
              '310000-CLOSE-DATOS          '.
           02 CON-OPERACIONES.
              05 CON-ABRIR     PIC X(15) VALUE 'ABRIR          '.
              05 CON-LEER      PIC X(15) VALUE 'LEER           '.
              05 CON-CERRAR    PIC X(15) VALUE 'CERRAR         '.
              05 CON-GRABAR    PIC X(15) VALUE 'GRABAR         '.
              05 CON-RUTINA    PIC X(15) VALUE 'LLAMAR RUTINA  '.
           02 CON-OBJETOS.
              05 CON-DATOS     PIC X(10) VALUE 'DATOS   '.
           02 CON-OTROS.
              05 CON-1         PIC 9(01) VALUE 1.
      ************************** TABLES ********************************

      **************************  SWITCHES  ****************************
       01 WS-SWITCHES.       
          05 FS-STATUS-FILE            PIC X(02) VALUE "00".
             88 FS-STATUS-FILE-OK                VALUE "00".
             88 FS-STATUS-FILE-EOF               VALUE "10".
      ************************** VARIABLES *****************************
       01 WS-VARIABLES.
           05 WS-FECHA.
               10 WS-D                 PIC 9(02).
               10 WS-M                 PIC 9(02).
               10 WS-A                 PIC 9(04).
           05 WS-OLDER-PERSON          PIC X(01).
           05 WS-AMOUNT-OCTOBER        PIC 9(05).
           05 WS-AMOUNT-SPECIAL        PIC 9(05).
           05 WS-AMOUNT-SPRING         PIC 9(05).
           05 WS-ID-MAX-WEIGHT         PIC 9(05).
           05 WS-PA-MAX-WEIGHT         PIC 9(02)V9.
           05 WS-TOTAL-WEIGHT          PIC 9(04)V9.

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
      *                      PROCEDURE DIVISION   
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
           PERFORM 110000-OPEN-DATOS
              THRU 110000-OPEN-DATOS-F                                 

           PERFORM 210000-READ-DATOS
              THRU 210000-READ-DATOS-F

           MOVE REG-DIA  TO WS-D
           MOVE REG-MES  TO WS-M
           MOVE REG-ANIO TO WS-A
           MOVE REG-SEXO TO WS-OLDER-PERSON   
           .                                      
       100000-START-F. EXIT.
      ******************************************************************
      *                         110000-OPEN-DATOS   
      ******************************************************************
       110000-OPEN-DATOS.                        
           OPEN INPUT DATOS                   
           IF NOT FS-STATUS-FILE-OK
              MOVE CON-110000-OPEN-DATOS   TO WS-ERR-PARRAFO 
              MOVE CON-DATOS               TO WS-ERR-OBJETO 
              MOVE CON-ABRIR               TO WS-ERR-OPERACION 
              MOVE FS-STATUS-FILE          TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       110000-OPEN-DATOS-F. EXIT.        
      ******************************************************************
      *                         200000-PROCESS   
      ****************************************************************** 
       200000-PROCESS.
           IF REG-MES = 10
               ADD 1 TO WS-AMOUNT-OCTOBER
           END-IF
           
           IF REG-ANIO < 1990
               ADD 1 TO WS-AMOUNT-SPECIAL
           ELSE        
               IF REG-ANIO = 1990 AND (REG-MES >= 1 
                   AND REG-MES <= 6) 
                   ADD 1 TO WS-AMOUNT-SPECIAL
               ELSE
                   IF REG-ANIO = 1990 AND REG-MES = 7 AND
                      REG-DIA >= 1 AND REG-DIA <= 9                    
                       ADD 1 TO WS-AMOUNT-SPECIAL
                   END-IF
               END-IF
           END-IF
           
           IF REG-ANIO = 1982 AND REG-SEXO = "F"
               IF REG-MES = 9 AND REG-DIA >= 21 AND REG-DIA <= 30
                   ADD 1 TO WS-AMOUNT-SPRING
               ELSE
                   IF REG-MES >= 10 AND REG-MES <= 11
                       ADD 1 TO WS-AMOUNT-SPRING
                   ELSE
                       IF REG-MES = 12 AND REG-DIA >= 1 
                           AND REG-DIA <= 21
                               ADD 1 TO WS-AMOUNT-SPRING
                       END-IF        
                   END-IF
               END-IF
           END-IF

           IF REG-ANIO < WS-A
               MOVE REG-DIA TO WS-D
               MOVE REG-MES TO WS-M
               MOVE REG-ANIO TO WS-A
               MOVE REG-SEXO TO WS-OLDER-PERSON
           ELSE
               IF REG-ANIO = WS-A AND REG-MES < WS-M
                   MOVE REG-DIA TO WS-D
                   MOVE REG-MES TO WS-M
                   MOVE REG-ANIO TO WS-A
                   MOVE REG-SEXO TO WS-OLDER-PERSON
               ELSE
                   IF REG-ANIO = WS-A AND REG-MES = WS-M 
                      AND REG-DIA < WS-D
                       MOVE REG-DIA TO WS-D
                       MOVE REG-MES TO WS-M
                       MOVE REG-ANIO TO WS-A
                       MOVE REG-SEXO TO WS-OLDER-PERSON
                   END-IF
               END-IF
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
               WHEN FS-STATUS-FILE-OK
                    CONTINUE   
               WHEN FS-STATUS-FILE-EOF
                    CONTINUE
               WHEN OTHER
                    MOVE CON-210000-READ-DATOS   TO WS-ERR-PARRAFO 
                    MOVE CON-DATOS               TO WS-ERR-OBJETO 
                    MOVE CON-LEER                TO WS-ERR-OPERACION 
                    MOVE FS-STATUS-FILE          TO WS-ERR-CODIGO
                    PERFORM 399999-END-PROGRAM
                       THRU 399999-END-PROGRAM-F
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
           IF NOT FS-STATUS-FILE-OK
              MOVE CON-310000-CLOSE-DATOS   TO WS-ERR-PARRAFO 
              MOVE CON-DATOS                TO WS-ERR-OBJETO 
              MOVE CON-CERRAR               TO WS-ERR-OPERACION 
              MOVE FS-STATUS-FILE           TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F
           END-IF
           .
       310000-CLOSE-DATOS-F. EXIT. 
      ******************************************************************
      *                         320000-TOTAL   
      ****************************************************************** 
       320000-TOTAL.
           DISPLAY "TOTAL OF BIRTHS ON OCTOBER " WS-AMOUNT-OCTOBER                  
           DISPLAY "TOTAL SUBSECTION 2 " WS-AMOUNT-SPECIAL                  
           DISPLAY "TOTAL OF WOMEN's BIRTHS ON SPRING " WS-AMOUNT-SPRING                  
           DISPLAY "SEX'S OLDEST PERSON " WS-OLDER-PERSON                  
           .
       320000-TOTAL-F. EXIT. 
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
       END PROGRAM E18.