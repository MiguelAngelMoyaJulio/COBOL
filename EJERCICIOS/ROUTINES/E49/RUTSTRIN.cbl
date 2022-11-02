      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUTSTRIN.
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
      ****************************  OUTPUT  **************************** 
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      *                     WORKING-STORAGE SECTION
      ******************************************************************
       WORKING-STORAGE SECTION.
       
      ************************  CONSTANTS  *****************************

      ************************** TABLES ******************************** 

      **************************  SWITCHES  ****************************

      ************************** COPYS *********************************
      
      ************************** VARIABLES *****************************    
       01 WSV-VARIABLES. 
          02 WSV-PROCESS         PIC X(01). 
          02 WSV-FRASE           PIC X(250). 
          02 WSV-NPALABRA        PIC X(02).
          02 WSV-INDEX           PIC 9(03). 
          02 WSV-INDEX-GENERAL. 
             05 WSV-I            PIC 9(03). 
             05 WSV-J            PIC 9(03). 
          02 WSV-INDEX-PALABRA. 
             05 WSV-LEN-ORI      PIC 9(03). 
             05 WSV-CONT-I       PIC 9(03). 
             05 WSV-OCURR        PIC 9(03).
      ******************************************************************
      *                       LINKAGE SECTION   
      ****************************************************************** 
       LINKAGE SECTION.
       01 LK-REG-STRING.
       COPY STRCERUT.
      ******************************************************************
      *                         PROCEDURE DIVISION
      ******************************************************************
       PROCEDURE DIVISION USING LK-REG-STRING.
           
           PERFORM 100000-START
              THRU 100000-START-F

           PERFORM 200000-PROCESS
              THRU 200000-PROCESS-F

           PERFORM 300000-EXIT
              THRU 300000-EXIT-F
           .   
      ******************************************************************
      *                         100000-START   
      ******************************************************************
       100000-START.
           IF LENGTH OF STRCERUT-PALABRA > 23
              MOVE 01 TO STRCERUT-COD-RET
              MOVE 'N' TO WSV-PROCESS
           ELSE
              MOVE 'S' TO WSV-PROCESS
           END-IF
           .   
       100000-START-F. EXIT.     
      ******************************************************************
      *                         200000-PROCESS   
      ******************************************************************
       200000-PROCESS.
           IF WSV-PROCESS = 'S'
              EVALUATE STRCERUT-OPCION
                  WHEN 1
                       PERFORM 210000-EXTREMOS-PALABRA
                          THRU 210000-EXTREMOS-PALABRA-F
                       
                       PERFORM 220000-CANT-PALABRA-OCU
                          THRU 220000-CANT-PALABRA-OCU-F

                       MOVE 00 TO STRCERUT-COD-RET    
                  WHEN 2
                       PERFORM 210000-EXTREMOS-PALABRA
                          THRU 210000-EXTREMOS-PALABRA-F

                       MOVE 00 TO STRCERUT-COD-RET    
              END-EVALUATE
           END-IF
           .   
       200000-PROCESS-F. EXIT.     
      ******************************************************************
      *                         210000-EXTREMOS-PALABRA   
      ******************************************************************
       210000-EXTREMOS-PALABRA.
           INITIALIZE WSV-CONT-I
           PERFORM VARYING WSV-I FROM 1
           BY 1 UNTIL WSV-I > LENGTH OF STRCERUT-PALABRA
                 IF STRCERUT-PALABRA(WSV-I:1) <> SPACES AND
                    WSV-CONT-I = 0
                    ADD 1 TO WSV-CONT-I
                    MOVE WSV-I TO STRCERUT-POS-INI
                 END-IF

                 IF STRCERUT-PALABRA(WSV-I:1) <> SPACES AND
                    WSV-I > STRCERUT-POS-FIN
                       MOVE WSV-I TO STRCERUT-POS-FIN
                 END-IF         
           END-PERFORM
           COMPUTE STRCERUT-LEN =
                   STRCERUT-POS-FIN -  STRCERUT-POS-INI + 1
           .   
       210000-EXTREMOS-PALABRA-F. EXIT.     
      ******************************************************************
      *                         220000-CANT-PALABRA-OCU   
      ******************************************************************
       220000-CANT-PALABRA-OCU.
           PERFORM VARYING WSV-I FROM 1 BY 1
           UNTIL WSV-I > (LENGTH OF STRCERUT-FRASE - STRCERUT-LEN + 1)
             IF STRCERUT-FRASE(WSV-I:STRCERUT-LEN) = 
                STRCERUT-PALABRA(STRCERUT-POS-INI:STRCERUT-POS-FIN)
                ADD 1 TO STRCERUT-CANT-OCURRENCIAS 
             END-IF
           END-PERFORM
           .   
       220000-CANT-PALABRA-OCU-F. EXIT.     
      ******************************************************************
      *                         300000-EXIT   
      ******************************************************************
       300000-EXIT.
           GOBACK
           .   
       300000-EXIT-F. EXIT.     
       END PROGRAM RUTSTRIN.      