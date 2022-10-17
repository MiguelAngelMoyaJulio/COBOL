      ******************************************************************
      *                     IDENTIFICATION DIVISION
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E42.
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
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      *                     WORKING-STORAGE SECTION
      ******************************************************************
       WORKING-STORAGE SECTION.
      ************************  CONSTANTS  *****************************
       01 WSC-CONSTANTS.       
          05 WSC-CON-ROWS                PIC 9(03) VALUE 13.
          05 WSC-CON-COLS                PIC 9(03) VALUE 5.
      ************************** TABLES ******************************** 
       01 WSC-MESES.
          10 FILLER PIC X(10) VALUE "ENERO     ".
          10 FILLER PIC X(10) VALUE "FEBRERO   ".
          10 FILLER PIC X(10) VALUE "MARZO     ".
          10 FILLER PIC X(10) VALUE "ABRIL     ".
          10 FILLER PIC X(10) VALUE "MAYO      ".
          10 FILLER PIC X(10) VALUE "JUNIO     ".
          10 FILLER PIC X(10) VALUE "JULIO     ".
          10 FILLER PIC X(10) VALUE "AGOSTO    ".
          10 FILLER PIC X(10) VALUE "SEPTIEMBRE".
          10 FILLER PIC X(10) VALUE "OCTUBRE   ".
          10 FILLER PIC X(10) VALUE "NOVIEMBRE ".
          10 FILLER PIC X(10) VALUE "DICIEMBRE ".
       01 WST-MESES REDEFINES WSC-MESES.
          05 WST-MES PIC X(10) OCCURS 12 TIMES.   
       
       01 WSC-CUENTAS.
          10 FILLER PIC X(08) VALUE "GAS     ".
          10 FILLER PIC X(08) VALUE "LUZ     ".
          10 FILLER PIC X(08) VALUE "TELEFONO".
          10 FILLER PIC X(08) VALUE "AGUA    ".
       01 WST-CUENTAS REDEFINES WSC-CUENTAS.
          05 WST-CUENTA PIC X(08) OCCURS 4 TIMES.   
       
       01 WSC-GASTOS.
          05 FILLER PIC X(30) VALUE '00345-00500-00445-00090-00000-'.
          05 FILLER PIC X(30) VALUE '00360-00455-00440-00095-00000-'.
          05 FILLER PIC X(30) VALUE '00333-00521-00446-00094-00000-'.
          05 FILLER PIC X(30) VALUE '00300-00654-00443-00100-00000-'.
          05 FILLER PIC X(30) VALUE '00345-00590-00454-00089-00000-'.
          05 FILLER PIC X(30) VALUE '00380-00566-00490-00101-00000-'.
          05 FILLER PIC X(30) VALUE '00323-00600-00435-00092-00000-'.
          05 FILLER PIC X(30) VALUE '00299-00532-00390-00085-00000-'.
          05 FILLER PIC X(30) VALUE '00346-00534-00449-00090-00000-'.
          05 FILLER PIC X(30) VALUE '00321-00536-00545-00095-00000-'.
          05 FILLER PIC X(30) VALUE '00344-00569-00345-00093-00000-'.
          05 FILLER PIC X(30) VALUE '00380-00566-00390-00075-00000-'.
          05 FILLER PIC X(30) VALUE '00000-00000-00000-00000-00000-'.
       01 WST-MATRIZ REDEFINES WSC-GASTOS.
          03 WST-MESES-DET OCCURS 13 TIMES.
             10 WST-GASTOS-DET OCCURS 5 TIMES.
                15 WST-GASTOS           PIC 9(05).
                15 FILLER               PIC X(01).
      **************************  SWITCHES  ****************************

      ************************** VARIABLES ***************************** 
       01 WSV-VARIABLES.
          05 WSV-I                      PIC 9(02).      
          05 WSV-J                      PIC 9(02).      
          05 WSV-SUMA-COLUMNAS          PIC 9(05).      
          05 WSV-SUMA-FILAS             PIC 9(05).      
          05 WSV-TOTAL-FILA-13          PIC 9(10).      
          05 WSV-TOTAL-COLUMNA-5        PIC 9(10).      
          05 WSV-OPCION                 PIC 9(01).      
          05 WSV-OPCION-MES             PIC 9(02).      
          05 WSV-OPCION-CUENTA          PIC 9(02).      
          05 WSV-EDIT                   PIC $ZZ.ZZ9,99.      
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
              WITH TEST AFTER UNTIL WSV-OPCION = 0
           
           PERFORM 300000-END
              THRU 300000-END-F
           .
      ******************************************************************
      *                         100000-START         
      ******************************************************************          
       100000-START.
           PERFORM 210000-SUMAR-COLUMNAS
              THRU 210000-SUMAR-COLUMNAS-F
           
           PERFORM 220000-SUMAR-FILAS
              THRU 220000-SUMAR-FILAS-F
 
           PERFORM 230000-SUMAR-COLUMNA-5
              THRU 230000-SUMAR-COLUMNA-5-F
 
           PERFORM 240000-SUMAR-FILA-13
              THRU 240000-SUMAR-FILA-13-F
          
           PERFORM 310000-VALIDAR-SUMAS
              THRU 310000-VALIDAR-SUMAS-F
           .     
       100000-START-F. EXIT.
      ******************************************************************
      *                         200000-PROCESS         
      ****************************************************************** 
       200000-PROCESS.
           DISPLAY "1.VER MATRIZ-GASTOS"  
           DISPLAY "2.VER GASTOS POR MES"  
           DISPLAY "3.VER GASTOS POR CUENTA"  
           DISPLAY "0.SALIR"  
           ACCEPT WSV-OPCION
           EVALUATE WSV-OPCION
               WHEN 1
                    PERFORM 310000-MOSTRAR-MATRIZ
                       THRU 310000-MOSTRAR-MATRIZ-F
               WHEN 2
                    DISPLAY "INGRESE EL NUMERO DEL MES"
                    ACCEPT WSV-OPCION-MES  
                    IF WSV-OPCION-MES >= 1 AND WSV-OPCION-MES <= 12 
                       PERFORM 250000-GASTO-POR-MES
                          THRU 250000-GASTO-POR-MES-F  
                    ELSE 
                       DISPLAY "MES INVALIDO"
                    END-IF
               WHEN 3
                    DISPLAY "SELECCION UNA CUENTA"
                    DISPLAY "1.GAS"  
                    DISPLAY "2.LUZ"  
                    DISPLAY "3.TELEFONO"  
                    DISPLAY "4.AGUA"
                    ACCEPT WSV-OPCION-CUENTA
                    
                    IF WSV-OPCION-CUENTA >= 1 AND WSV-OPCION-CUENTA <= 4 
                       PERFORM 260000-GASTO-POR-CUENTA
                          THRU 260000-GASTO-POR-CUENTA-F  
                    ELSE
                       DISPLAY "CUENTA INVALIDA"
                    END-IF
           END-EVALUATE
           DISPLAY " "
           .     
       200000-PROCESS-F. EXIT.
      ******************************************************************
      *                         210000-SUMAR-COLUMNAS         
      ****************************************************************** 
       210000-SUMAR-COLUMNAS.
           PERFORM VARYING WSV-J FROM 1
           BY 1 UNTIL WSV-J > 4
                
                INITIALIZE WSV-SUMA-COLUMNAS
                PERFORM VARYING WSV-I FROM 1
                BY 1 UNTIL WSV-I > 12
                    COMPUTE WSV-SUMA-COLUMNAS = WSV-SUMA-COLUMNAS + 
                            WST-GASTOS (WSV-I , WSV-J)          
                END-PERFORM    
                MOVE WSV-SUMA-COLUMNAS TO WST-GASTOS (13 , WSV-J)
      *         DISPLAY "SUMA COLUMNA " WSV-J " : " WSV-SUMA-COLUMNAS 
           END-PERFORM
           .
       210000-SUMAR-COLUMNAS-F. EXIT.
      ******************************************************************
      *                         220000-SUMAR-FILAS         
      ****************************************************************** 
       220000-SUMAR-FILAS. 
           PERFORM VARYING WSV-I FROM 1
           BY 1 UNTIL WSV-I > WSC-CON-ROWS
           
                INITIALIZE WSV-SUMA-FILAS
                PERFORM VARYING WSV-J FROM 1
                BY 1 UNTIL WSV-J > 4
                    COMPUTE WSV-SUMA-FILAS = WSV-SUMA-FILAS + 
                            WST-GASTOS (WSV-I , WSV-J)          
                END-PERFORM   
                
                MOVE WSV-SUMA-FILAS TO WST-GASTOS (WSV-I , 5)   
      *         DISPLAY "SUMAR FILA " WSV-I " : " WSV-SUMA-FILAS 
           END-PERFORM     
           .
       220000-SUMAR-FILAS-F. EXIT.
      ******************************************************************
      *                         230000-SUMAR-COLUMNA-5         
      ****************************************************************** 
       230000-SUMAR-COLUMNA-5. 
           PERFORM VARYING WSV-I FROM 1
           BY 1 UNTIL WSV-I > 12
                COMPUTE WSV-TOTAL-COLUMNA-5 = WSV-TOTAL-COLUMNA-5 + 
                        WST-GASTOS (WSV-I , 5)          
           END-PERFORM  
           .
       230000-SUMAR-COLUMNA-5-F. EXIT.
      ******************************************************************
      *                         240000-SUMAR-FILA-13         
      ****************************************************************** 
       240000-SUMAR-FILA-13. 
           PERFORM VARYING WSV-I FROM 1
           BY 1 UNTIL WSV-I > 4
                COMPUTE WSV-TOTAL-FILA-13 = WSV-TOTAL-FILA-13 + 
                        WST-GASTOS (13 , WSV-I)          
           END-PERFORM     
           .
       240000-SUMAR-FILA-13-F. EXIT.
      ******************************************************************
      *                         250000-GASTO-POR-MES         
      ****************************************************************** 
       250000-GASTO-POR-MES. 
           DISPLAY "MES : " WST-MES (WSV-OPCION-MES)
           PERFORM VARYING WSV-I FROM 1
           BY 1 UNTIL WSV-I > 4
                INITIALIZE WSV-EDIT
                MOVE WST-GASTOS(WSV-OPCION-MES , WSV-I) TO WSV-EDIT
                IF WSV-I = 1
                   DISPLAY "GASTOS  " WST-CUENTA(WSV-I) "       = " 
                                      WSV-EDIT
                ELSE
                   DISPLAY "        " WST-CUENTA(WSV-I)"       = "
                                      WSV-EDIT
                END-IF
           END-PERFORM
           INITIALIZE WSV-EDIT
           MOVE WST-GASTOS(WSV-OPCION-MES , 5) TO WSV-EDIT
           DISPLAY "        TOTAL          = " WSV-EDIT
           .
       250000-GASTO-POR-MES-F. EXIT.
      ******************************************************************
      *                         260000-GASTO-POR-CUENTA         
      ****************************************************************** 
       260000-GASTO-POR-CUENTA. 
           DISPLAY "CUENTA : " WST-CUENTA(WSV-OPCION-CUENTA)
           PERFORM VARYING WSV-I FROM 1
           BY 1 UNTIL WSV-I > 12
                INITIALIZE WSV-EDIT
                MOVE WST-GASTOS(WSV-I , WSV-OPCION-CUENTA) TO WSV-EDIT
                IF WSV-I = 1
                   DISPLAY "MES " WST-MES(WSV-I) "   = " 
                                  WSV-EDIT
                ELSE
                   DISPLAY "    " WST-MES(WSV-I) "   = "
                                  WSV-EDIT
                END-IF
           END-PERFORM 
           INITIALIZE WSV-EDIT
           MOVE WST-GASTOS(13 , WSV-OPCION-CUENTA) TO WSV-EDIT
           DISPLAY "       TOTAL     = " WSV-EDIT
           .
       260000-GASTO-POR-CUENTA-F. EXIT.
      ******************************************************************
      *                         300000-END         
      ****************************************************************** 
       300000-END. 
           DISPLAY "FIN"
           STOP RUN
           .
       300000-END-F. EXIT.
      ******************************************************************
      *                         310000-MOSTRAR-MATRIZ         
      ****************************************************************** 
       310000-MOSTRAR-MATRIZ. 
           DISPLAY "MATRIZ" 
           PERFORM VARYING WSV-I FROM 1
           BY 1 UNTIL WSV-I > WSC-CON-ROWS
                PERFORM VARYING WSV-J FROM 1
                BY 1 UNTIL WSV-J > WSC-CON-COLS
                    DISPLAY "| " WST-GASTOS(WSV-I,WSV-J) " " 
                    WITH NO ADVANCING          
                END-PERFORM          
                DISPLAY "|" WITH NO ADVANCING 
                DISPLAY " " 
           END-PERFORM
           .
       310000-MOSTRAR-MATRIZ-F. EXIT.
      ******************************************************************
      *                         310000-VALIDAR-SUMAS         
      ****************************************************************** 
       310000-VALIDAR-SUMAS.
           IF WSV-TOTAL-COLUMNA-5 = WSV-TOTAL-FILA-13
      *       DISPLAY "LOS TOTALES COINCIDEN" 
      *       DISPLAY "TOTAL COLUMNA 5 : " WSV-TOTAL-COLUMNA-5 
      *       DISPLAY "TOTAL FILA 13 : " WSV-TOTAL-FILA-13
              MOVE WSV-TOTAL-COLUMNA-5 TO WST-GASTOS(13 , 5)
           ELSE
           DISPLAY " "
      *       DISPLAY "LOS TOTALES NO COINCIDEN"
      *       DISPLAY "TOTAL COLUMNA 5 : " WSV-TOTAL-COLUMNA-5 
      *       DISPLAY "TOTAL FILA 13 : " WSV-TOTAL-FILA-13 
           END-IF
           .
       310000-VALIDAR-SUMAS-F. EXIT.
       END PROGRAM E42.      