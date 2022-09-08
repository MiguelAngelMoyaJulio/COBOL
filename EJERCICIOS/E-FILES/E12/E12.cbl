      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-07
      * Dados n valores numéricos, informar el mayor, el menor y en
      * que posición del conjunto fueron ingresados cada uno de ellos.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E12.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT OPTIONAL DATOS
       ASSIGN TO "DAT.txt"
       ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD DATOS.
           01 REG-DATOS.
               05 REG-NUM PIC 9(03).
           
       WORKING-STORAGE SECTION.
       01 WS-VAR.
          02 WS-MIN         PIC 9(03). 
          02 WS-MAX         PIC 9(03). 
          02 WS-POS-MAX     PIC 9(03). 
          02 WS-POS-MIN     PIC 9(03).
          02 WS-POS         PIC 9(03).
       77 WS-STATUS PIC X(01).

           
       PROCEDURE DIVISION.
       OPEN INPUT DATOS
       
       PERFORM 20-LEER
       THRU 20-LEER-F
       
       MOVE REG-NUM   TO WS-MAX
       MOVE REG-NUM   TO WS-MIN

       PERFORM 30-CALCULO
       THRU 30-CALCULO-F
       UNTIL WS-STATUS = "F"
       
       CLOSE DATOS

       PERFORM 40-TOTALES
          THRU 40-TOTALES-F
       . 
       STOP RUN.

       20-LEER. 
       ADD 1 TO WS-POS

       READ DATOS NEXT RECORD
       AT END
       MOVE "F" TO  WS-STATUS
       .            
       20-LEER-F.

       30-CALCULO. 
       
       IF REG-NUM > WS-MAX
          MOVE REG-NUM TO WS-MAX
          MOVE WS-POS TO WS-POS-MAX 
       END-IF

       IF REG-NUM < WS-MIN
          MOVE REG-NUM TO WS-MIN 
          MOVE WS-POS TO WS-POS-MIN 
       END-IF

       PERFORM 20-LEER
       THRU 20-LEER-F
       .
       30-CALCULO-F.  

       40-TOTALES. 
           DISPLAY "MAX NUMBER " WS-MAX           
                   "  POSITION " WS-POS-MAX           
           DISPLAY "MIN NUMBER " WS-MIN           
                   "  POSITION " WS-POS-MIN           
           .           
       40-TOTALES-F.            
       END PROGRAM E12.
