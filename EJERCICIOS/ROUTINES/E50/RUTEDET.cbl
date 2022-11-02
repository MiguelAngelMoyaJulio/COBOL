      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUTEDET.
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

      ******************************************************************
      *                       LINKAGE SECTION   
      ****************************************************************** 
       LINKAGE SECTION.
       01 LK-REG-EDE.
       COPY EDECERUT.
      ******************************************************************
      *                         PROCEDURE DIVISION
      ******************************************************************
       PROCEDURE DIVISION USING LK-REG-EDE.
           PERFORM 200000-PROCESS
              THRU 200000-PROCESS-F

           PERFORM 300000-EXIT
              THRU 300000-EXIT-F
           .   
      ******************************************************************
      *                         100000-START   
      ******************************************************************
       100000-START.
           DISPLAY " "  
           .   
       100000-START-F. EXIT.     
      ******************************************************************
      *                         200000-PROCESS   
      ******************************************************************
       200000-PROCESS.
           COMPUTE EDET-DET = EDET-NUM(1 , 1) * EDET-NUM(2 , 2) - 
                              EDET-NUM(2 , 1) * EDET-NUM(1 , 2)
           IF EDET-DET <> 0 
              MOVE "INVERSIBLE"    TO EDET-REF
           ELSE
              MOVE "NO INVERSIBLE" TO EDET-REF
           END-IF
           MOVE '00' TO EDET-COD-RET                   
           .   
       200000-PROCESS-F. EXIT.     
      ******************************************************************
      *                         300000-EXIT   
      ******************************************************************
       300000-EXIT.
           GOBACK
           .   
       300000-EXIT-F. EXIT.     
       END PROGRAM RUTEDET.      