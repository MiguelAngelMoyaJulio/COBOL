      ******************************************************************
      * Dada una serie de caracteres que conforman una oración
      * , donde cada palabra está separada de la 
      * siguiente por un carácter blanco y 
      * la oración finaliza con un punto. Se pide informar:
      *•	Cantidad de veces que apareció cada vocal.
      *•	Cantidad de palabras que contiene la oración.
      *•	Cantidad de letras que posee la palabra más larga.

      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E15.
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
       
      ****************************  OUTPUT  ****************************
 
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************
       WORKING-STORAGE SECTION.
      ************************  CONSTANTES  ****************************
       01 WS-CONS.
           02 CON-BLANK             PIC X(01) VALUE " ".     
           02 CON-POINT             PIC X(01) VALUE ".".     
      **************************  SWITCHES  ****************************

      ************************** VARIABLES *****************************
       01 WS-VAR. 
           02 WS-WORD               PIC X(250) VALUE SPACES. 
           02 WS-A                  PIC 9(04)  VALUE ZEROS. 
           02 WS-E                  PIC 9(04)  VALUE ZEROS. 
           02 WS-I                  PIC 9(04)  VALUE ZEROS. 
           02 WS-J                  PIC 9(04)  VALUE ZEROS. 
           02 WS-O                  PIC 9(04)  VALUE ZEROS. 
           02 WS-U                  PIC 9(04)  VALUE ZEROS. 
           02 WS-INDEX              PIC 9(04)  VALUE ZEROS. 
           02 WS-AMOUNT-WORDS       PIC 9(04)  VALUE ZEROS.
           02 WS-AMOUNT-SPACES      PIC 9(04)  VALUE ZEROS.
           02 WS-AMOUNT-MAX-LETTERS PIC 9(04)  VALUE ZEROS.
           02 WS-POINT-FOUND        PIC X(01)  VALUE SPACES.
           02 WS-CANT-SPACES        PIC 9(03)  VALUE ZEROS.
           02 WS-POS-1              PIC 9(03)  VALUE 1.
           02 WS-POS-2              PIC 9(03)  VALUE ZEROS.
           02 WS-POS-3              PIC 9(03)  VALUE ZEROS.
           02 WS-MAX-LEN-WORD       PIC 9(03)  VALUE ZEROS.
           02 WS-PARTIAL-LEN-WORD   PIC 9(03)  VALUE ZEROS.
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
             UNTIL WS-POINT-FOUND = "Y"   
                                                  
           PERFORM 300000-END                         
              THRU 300000-END-F
           .
      ******************************************************************
      *                         100000-START         
      ******************************************************************     
       100000-START.
           DISPLAY "ENTER A SENTENCE "
           ACCEPT WS-WORD       
           
           PERFORM VARYING WS-J FROM 1 BY 1
           UNTIL WS-J > 250
               IF WS-WORD(WS-J:1) EQUAL SPACES
                   ADD 1 TO WS-AMOUNT-SPACES
               END-IF
           END-PERFORM           
           . 
       100000-START-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS         
      ******************************************************************
       200000-PROCESS.
           MOVE "N" TO WS-POINT-FOUND
           PERFORM VARYING WS-INDEX FROM 1
           BY 1 UNTIL WS-POINT-FOUND = "Y"
               
               IF WS-WORD(WS-INDEX:1) = CON-POINT
                   MOVE WS-INDEX TO WS-POS-3
                   MOVE "Y" TO WS-POINT-FOUND
               ELSE
                   IF WS-WORD(WS-INDEX:1) = CON-BLANK
                      COMPUTE WS-POS-2 = WS-INDEX - WS-POS-1
      *               DISPLAY WS-WORD(WS-POS-1:WS-POS-2)
                      COMPUTE WS-PARTIAL-LEN-WORD = WS-INDEX - WS-POS-1
                      MOVE WS-INDEX TO WS-POS-1
                      ADD 1 TO WS-POS-1  

                      IF WS-POS-2 > WS-MAX-LEN-WORD
                         MOVE WS-POS-2 TO WS-MAX-LEN-WORD
                      END-IF 

                      ADD 1 TO WS-CANT-SPACES
                   END-IF
                   
                   EVALUATE TRUE
                       WHEN WS-WORD(WS-INDEX:1) = "A" 
                            ADD 1 TO WS-A 
                       WHEN WS-WORD(WS-INDEX:1) = "E"
                            ADD 1 TO WS-E 
                       WHEN WS-WORD(WS-INDEX:1) = "I"
                            ADD 1 TO WS-I 
                       WHEN WS-WORD(WS-INDEX:1) = "O"
                            ADD 1 TO WS-O 
                       WHEN WS-WORD(WS-INDEX:1) = "U"
                            ADD 1 TO WS-U 
                   END-EVALUATE
               END-IF
               
           END-PERFORM
           COMPUTE WS-POS-3 = WS-POS-3 - WS-POS-1

           IF WS-POS-3 > WS-MAX-LEN-WORD
               MOVE WS-POS-3 TO WS-MAX-LEN-WORD
           END-IF

           COMPUTE WS-CANT-SPACES= WS-CANT-SPACES + 1

           .
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           PERFORM 310000-TOTAL
              THRU 310000-TOTAL-F

           STOP RUN 
           .    
       300000-END-F. EXIT. 
      ******************************************************************
      *                         310000-TOTAL   
      ****************************************************************** 
       310000-TOTAL.
           IF WS-AMOUNT-SPACES <> 250
               DISPLAY "TOTALS "
               DISPLAY "VOWEL A " WS-A 
               DISPLAY "VOWEL E " WS-E 
               DISPLAY "VOWEL I " WS-I 
               DISPLAY "VOWEL O " WS-O 
               DISPLAY "VOWEL U " WS-U 
               DISPLAY "TOTAL WORDS " WS-CANT-SPACES 
               DISPLAY "LONGEST WORD " WS-MAX-LEN-WORD 
           ELSE
               DISPLAY "NO DATA TO BE PROCESSED"
           END-IF
           DISPLAY "END"
           .    
       310000-TOTAL-F. EXIT. 

       END PROGRAM E15.
