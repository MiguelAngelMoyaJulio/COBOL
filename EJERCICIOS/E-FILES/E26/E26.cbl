      ******************************************************************
      * Author: MIGUEL MOYA
      * Date: 2022-09-15
      *Ingresar un valor entero N (< 40). A continuación ingresar 
      *un conjunto A y luego otro conjunto B
      *ambos de N elementos. Generar un arreglo C donde 
      *cada elemento se forme de la siguiente forma:
      *C[1] ← A[1]+B[N] C[2] ← A[2]+B[N-1] ..........................
      *C[N] ← A[N]+B[1]
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E26.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT DATOS1
       ASSIGN TO "DAT1.txt"
       FILE STATUS IS FS-STATUS1
       ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD DATOS1.
           01 REG-DATOS1.
               05 REG-DOMICILIO                 PIC X(30).
               05 REG-VIVIENDA                  PIC X(01).
               05 REG-INTEGRANTES               PIC 9(01).
               05 REG-NOMBRE                    PIC X(06).
               05 REG-EDAD                      PIC 9(02).
               05 REG-SEXO                      PIC X(01).
               05 REG-ESTUDIO                   PIC X(01).
               05 REG-ESTADO                    PIC X(01).
      ******************************************************************
      *                     WORKING-STORAGE SECTION   
      ******************************************************************         
       WORKING-STORAGE SECTION.
           01 FS-STATUS1                     PIC X(02) VALUE "00".
               88 FS-STATUS-OK                         VALUE "00".
               88 FS-STATUS-EOF                        VALUE "10".
           01 WS-VAR.
               02 WS-CORTE-DOMICILIO       PIC X(30).
               02 WS-CANT-INTE             PIC 9(02).
               02 WS-CANT-REC              PIC 9(03).
               02 WS-CANT-ANALFABETO       PIC 9(03).
               02 WS-PORC-ANALFABETO       PIC 9(03)V99.
               02 WS-DOMICILIO-AUX         PIC X(30).
               02 WS-INTE-MAX              PIC 9(02).
               02 WS-EDAD-TOTAL-CIUDAD     PIC 9(05).
               02 WS-EDAD-PORC-CIUDAD      PIC 9(05)V99.
               02 WS-EDAD-TOTAL            PIC 9(05).
               02 WS-EDAD-FAM-PROM         PIC 9(05)V99.
               02 WS-CANT-FEM              PIC 9(02).
               02 WS-CANT-MAS              PIC 9(02).
               02 WS-PORC-FEM              PIC 9(03)V99.
               02 WS-PORC-MAS              PIC 9(03)V99.
               02 WS-CANT-PRIMARIO         PIC 9(03).
               02 WS-CANT-SECUNDARIO       PIC 9(03).
               02 WS-CANT-TERCIARIO        PIC 9(03).
               02 WS-CANT-UNIVERSITARIO    PIC 9(03).
               02 WS-Y                     PIC 9(03).
               02 WS-Z                     PIC 9(03).
               02 WS-J                     PIC 9(03).
               02 WS-X                     PIC 9(03).
               02 WS-SUMA                  PIC 9(03).
           01 WS-V1 OCCURS 40 TIMES.
               02 WS-NUM1                  PIC 9(02).                
           01 WS-TITULO.
               02 WS-T1                    PIC  X(04) VALUE "V1 [".                 
               02 WS-I1                    PIC  9(02).                  
               02 WS-F1                    PIC  X(05) VALUE "] -> ".                 
               02 WS-VALOR1                PIC  X(02).                 
               02 WS-T2                    PIC  X(07) VALUE " + V2 [".                 
               02 WS-I2                    PIC  9(02).                  
               02 WS-F2                    PIC  X(05) VALUE "] -> ".                 
               02 WS-VALOR2                PIC  9(02).                 
               02 WS-OP                    PIC  X(03) VALUE " = ".                 
               02 WS-T-SUMA                PIC  9(03).
      ******************************************************************
      *                         PROCEDURE DIVISION   
      ******************************************************************                           
       PROCEDURE DIVISION.
           
           PERFORM 10-INICIO
              THRU 10-INICIO-F
           
           PERFORM 20-PROCESO
              THRU 20-PROCESO-F
              UNTIL FS-STATUS-EOF  

           PERFORM 30-FIN
              THRU 30-FIN-F           
           .
            STOP RUN.
      ******************************************************************
      *                         10-INICIO   
      ******************************************************************      
       10-INICIO.
           
           PERFORM 10-ABRIR-DATOS1
              THRU 10-ABRIR-DATOS1-F

           PERFORM 20-LEER1
               THRU 20-LEER1-F

           MOVE REG-DOMICILIO TO WS-CORTE-DOMICILIO
           MOVE REG-DOMICILIO TO WS-DOMICILIO-AUX
           MOVE REG-INTEGRANTES TO WS-INTE-MAX
           .
       10-INICIO-F. EXIT.
      ******************************************************************
      *                         10-ABRIR-DATOS1   
      ******************************************************************     
       10-ABRIR-DATOS1.
           OPEN INPUT DATOS1
           IF NOT FS-STATUS-OK
               DISPLAY "ERROR AL ABRIR ARCHIVO " FS-STATUS1
           END-IF
           .
       10-ABRIR-DATOS1-F. EXIT.
      ******************************************************************
      *                         20-PROCESO   
      ****************************************************************** 
       20-PROCESO.  

           IF REG-DOMICILIO = WS-CORTE-DOMICILIO
              COMPUTE WS-EDAD-TOTAL-CIUDAD = WS-EDAD-TOTAL-CIUDAD +
                                             REG-EDAD         

              PERFORM 20-ESTUDIO-PRIMARIO
                 THRU 20-ESTUDIO-PRIMARIO-F

              PERFORM 20-ANALFABETO
                 THRU 20-ANALFABETO-F

              PERFORM 20-MAX-DEPARTAMENTO
                 THRU 20-MAX-DEPARTAMENTO-F

              PERFORM 20-FEM-MAS
                 THRU 20-FEM-MAS-F

              PERFORM 20-ESTUDIO-INCOMPLETO
                 THRU 20-ESTUDIO-INCOMPLETO-F


              PERFORM 20-EDAD-PROMEDIO-FAMILIA
                 THRU 20-EDAD-PROMEDIO-FAMILIA-F
               
              PERFORM 20-LEER1
                 THRU 20-LEER1-F
           ELSE
              COMPUTE WS-EDAD-FAM-PROM = WS-EDAD-TOTAL / WS-CANT-INTE
              DISPLAY WS-CORTE-DOMICILIO " : " WS-EDAD-FAM-PROM 

              INITIALIZE WS-EDAD-FAM-PROM  
              INITIALIZE WS-EDAD-TOTAL  
              INITIALIZE WS-CANT-INTE  
              MOVE REG-DOMICILIO TO WS-CORTE-DOMICILIO
           END-IF
           .         
       20-PROCESO-F. EXIT.
      ******************************************************************
      *                         20-LEER1   
      ******************************************************************      
       20-LEER1.
           INITIALIZE REG-DATOS1
           READ DATOS1 INTO REG-DATOS1
           EVALUATE TRUE
               WHEN FS-STATUS-OK
                    ADD 1 TO WS-CANT-REC
               WHEN FS-STATUS-EOF
                    CONTINUE
           END-EVALUATE
           .
       20-LEER1-F. EXIT.
      ******************************************************************
      *                         20-ESTUDIO-PRIMARIO   
      ******************************************************************      
       20-ESTUDIO-PRIMARIO.
           IF REG-ESTUDIO = "P" AND REG-ESTADO = "C"
               DISPLAY REG-DATOS1
           END-IF
           .
       20-ESTUDIO-PRIMARIO-F. EXIT.
      ******************************************************************
      *                         20-ANALFABETO   
      ******************************************************************      
       20-ANALFABETO.
           IF REG-ESTUDIO = "N" AND REG-EDAD > 10
               ADD 1 TO WS-CANT-ANALFABETO
           END-IF
           .
       20-ANALFABETO-F. EXIT.
      ******************************************************************
      *                         20-EDAD-PROMEDIO-FAMILIA   
      ******************************************************************      
       20-EDAD-PROMEDIO-FAMILIA.
           ADD 1 TO WS-CANT-INTE
           COMPUTE WS-EDAD-TOTAL = WS-EDAD-TOTAL + REG-EDAD
           .
       20-EDAD-PROMEDIO-FAMILIA-F. EXIT.
      ******************************************************************
      *                         20-MAX-DEPARTAMENTO   
      ******************************************************************      
       20-MAX-DEPARTAMENTO.
           IF REG-VIVIENDA = "D" AND REG-INTEGRANTES > WS-INTE-MAX
               MOVE REG-INTEGRANTES TO WS-INTE-MAX
               MOVE REG-DOMICILIO TO WS-DOMICILIO-AUX
           END-IF
           .
       20-MAX-DEPARTAMENTO-F. EXIT.
      ******************************************************************
      *                         20-FEM-MAS   
      ******************************************************************      
       20-FEM-MAS.
           
           EVALUATE REG-SEXO
               WHEN "F"
                    ADD 1 TO WS-CANT-FEM
               WHEN "M"
                    ADD 1 TO WS-CANT-MAS
           END-EVALUATE     
           .
       20-FEM-MAS-F. EXIT.
      ******************************************************************
      *                         20-ESTUDIO-INCOMPLETO   
      ******************************************************************      
       20-ESTUDIO-INCOMPLETO.
           IF REG-ESTUDIO = "P" AND REG-ESTADO = "I"
              ADD 1 TO WS-CANT-PRIMARIO 
           END-IF
           IF REG-ESTUDIO = "S" AND REG-ESTADO = "I"
              ADD 1 TO WS-CANT-SECUNDARIO 
           END-IF
           IF REG-ESTUDIO = "T" AND REG-ESTADO = "I"
              ADD 1 TO WS-CANT-TERCIARIO 
           END-IF
           IF REG-ESTUDIO = "U" AND REG-ESTADO = "I"
              ADD 1 TO WS-CANT-UNIVERSITARIO 
           END-IF
           .
       20-ESTUDIO-INCOMPLETO-F. EXIT.
      ******************************************************************
      *                         30-FIN   
      ****************************************************************** 
       30-FIN.
           PERFORM 30-CERRAR-DATOS1
              THRU 30-CERRAR-DATOS1-F
              
           PERFORM 30-ULTIMO-CORTE
              THRU 30-ULTIMO-CORTE-F

           PERFORM 30-RESULTADOS
              THRU 30-RESULTADOS-F
           .    
       30-FIN-F. EXIT.
      ******************************************************************
      *                         30-RESULTADOS   
      ****************************************************************** 
       30-RESULTADOS.
           COMPUTE WS-PORC-ANALFABETO = (WS-CANT-ANALFABETO *100) / 
                                         WS-CANT-REC   
           DISPLAY "PORCENTAJE DE ANALFABETISMO : " WS-PORC-ANALFABETO
           DISPLAY " "
           DISPLAY "DOMICILIO CON MAYOR CANTIDAD DE GENTE DEPA : "
                   WS-DOMICILIO-AUX
           DISPLAY " "                   
           COMPUTE WS-EDAD-PORC-CIUDAD = WS-EDAD-TOTAL-CIUDAD 
                                           / WS-CANT-REC 
           DISPLAY "EDAD PROMEDIO DE LA CIUDAD : " WS-EDAD-PORC-CIUDAD
           DISPLAY " "
           DISPLAY "ESTUDIOS INCOMPLETOS"
           DISPLAY "PRIMARIA : " WS-CANT-PRIMARIO      
           DISPLAY "SECUNDARIA : " WS-CANT-SECUNDARIO      
           DISPLAY "TERCIARIO : " WS-CANT-TERCIARIO      
           DISPLAY "UNIVERSIDAD : " WS-CANT-UNIVERSITARIO      
           DISPLAY " "
           DISPLAY "PORCENTAJE MASCULINO Y FEMENINO"
           COMPUTE WS-PORC-FEM = WS-CANT-FEM *100 / WS-CANT-REC
           COMPUTE WS-PORC-MAS = WS-CANT-MAS *100 / WS-CANT-REC
           DISPLAY "PORCENTAJE MASCULINO : " WS-PORC-MAS
           DISPLAY "PORCENTAJE FEMENINO : " WS-PORC-FEM
           .    
       30-RESULTADOS-F. EXIT.
      ******************************************************************
      *                         30-ULTIMO-CORTE   
      ****************************************************************** 
       30-ULTIMO-CORTE.
           COMPUTE WS-EDAD-FAM-PROM = WS-EDAD-TOTAL / WS-CANT-INTE
           DISPLAY WS-CORTE-DOMICILIO " : " WS-EDAD-FAM-PROM 
           .    
       30-ULTIMO-CORTE-F. EXIT.
      ******************************************************************
      *                         30-CERRAR-DATOS1   
      ****************************************************************** 
       30-CERRAR-DATOS1.
           CLOSE DATOS1
           IF NOT FS-STATUS-OK 
               DISPLAY "ERROR AL CERRAR ARCHIVO " FS-STATUS1
           END-IF
           .
       30-CERRAR-DATOS1-F. EXIT.
       END PROGRAM E26.