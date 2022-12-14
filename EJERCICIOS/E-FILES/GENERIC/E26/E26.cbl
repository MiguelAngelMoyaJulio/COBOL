      ******************************************************************
      * El gobierno de la Ciudad de Buenos Aires realiza una encuesta 
      *en casas de familia. De cada familia se conoce: domicilio, 
      *tipo de vivienda (‘C’: casa, ‘D’: departamento), 
      *y cantidad de integrantes.
      *De cada integrante de la familia se conoce: nombre, apellido,
      *edad, sexo (‘F’, ‘M’), nivel de estudios
      * alcanzados (‘N’: no posee, ‘P’: primario, ‘S’: secundario
      * , ‘T’: terciario, ‘U’: universitario), y 
      * un indicador (‘I’: incompleto, ‘C’: completo) que 
      * se refiere al ítem anterior.
      *Los datos finalizan cuando la cantidad de integrantes 
      *sea igual a cero. Se pide emitir un listado con los resultados:
      *•	Los datos de los encuestados que hayan completado 
      *los estudios primarios.
      *•	El porcentaje de analfabetismo en la 
      *ciudad (se considera analfabetos a los mayores de 
      *10 años que no posean estudios).
      *•	El domicilio de la familia con mayor cantidad de integrantes
      *que viven en departamento.
      *•	La edad promedio de cada familia y de la ciudad.
      *•	La cantidad de encuestados en cada tipo de nivel de estudios
      *alcanzados incompletos.
      *•	El porcentaje de encuestados de sexo femenino y masculino.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E26.
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
       SELECT DATOS ASSIGN TO "DAT1.txt"
                     FILE STATUS IS FS-STATUS-FILE
                     ORGANIZATION IS LINE SEQUENTIAL. 
       
      ****************************  OUTPUT  ****************************

       DATA DIVISION.
       FILE SECTION.
       FD DATOS.
          01 REG-DATOS.
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
              05 CON-DATOS   PIC X(10) VALUE 'DATOS   '.
           02 CON-OTROS.
              05 CON-1         PIC 9(01) VALUE 1.
      ************************** TABLES ********************************

      **************************  SWITCHES  ****************************
       01 WS-SWITCHES.       
           05 FS-STATUS-FILE                PIC X(02) VALUE "00".
              88 FS-STATUS-FILE-OK                    VALUE "00".
              88 FS-STATUS-FILE-EOF                   VALUE "10".
      ************************** VARIABLES *****************************
       01 WS-VARIABLES.
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

           MOVE REG-DOMICILIO TO WS-CORTE-DOMICILIO
           MOVE REG-DOMICILIO TO WS-DOMICILIO-AUX
           MOVE REG-INTEGRANTES TO WS-INTE-MAX
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

           IF REG-DOMICILIO = WS-CORTE-DOMICILIO
              COMPUTE WS-EDAD-TOTAL-CIUDAD = WS-EDAD-TOTAL-CIUDAD +
                                             REG-EDAD         

              PERFORM 220000-ESTUDIO-PRIMARIO
                 THRU 220000-ESTUDIO-PRIMARIO-F

              PERFORM 230000-ANALFABETO
                 THRU 230000-ANALFABETO-F

              PERFORM 250000-MAX-DEPARTAMENTO
                 THRU 250000-MAX-DEPARTAMENTO-F

              PERFORM 260000-FEM-MAS
                 THRU 260000-FEM-MAS-F

              PERFORM 270000-ESTUDIO-INCOMPLETO
                 THRU 270000-ESTUDIO-INCOMPLETO-F

              PERFORM 240000-EDAD-PROMEDIO-FAMILIA
                 THRU 240000-EDAD-PROMEDIO-FAMILIA-F
               
              PERFORM 210000-READ-DATOS
                 THRU 210000-READ-DATOS-F
           ELSE
              COMPUTE WS-EDAD-FAM-PROM = WS-EDAD-TOTAL / WS-CANT-INTE
              DISPLAY WS-CORTE-DOMICILIO " : " WS-EDAD-FAM-PROM 

              INITIALIZE WS-EDAD-FAM-PROM  
              INITIALIZE WS-EDAD-TOTAL  
              INITIALIZE WS-CANT-INTE  
              MOVE REG-DOMICILIO TO WS-CORTE-DOMICILIO
           END-IF
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
                    ADD 1 TO WS-CANT-REC
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
      *                         220000-ESTUDIO-PRIMARIO   
      ******************************************************************      
       220000-ESTUDIO-PRIMARIO.
           IF REG-ESTUDIO = "P" AND REG-ESTADO = "C"
               DISPLAY REG-DATOS
           END-IF
           .
       220000-ESTUDIO-PRIMARIO-F. EXIT.
      ******************************************************************
      *                         230000-ANALFABETO   
      ******************************************************************      
       230000-ANALFABETO.
           IF REG-ESTUDIO = "N" AND REG-EDAD > 10
               ADD 1 TO WS-CANT-ANALFABETO
           END-IF
           .
       230000-ANALFABETO-F. EXIT.
      ******************************************************************
      *                         240000-EDAD-PROMEDIO-FAMILIA   
      ******************************************************************      
       240000-EDAD-PROMEDIO-FAMILIA.
           ADD 1 TO WS-CANT-INTE
           COMPUTE WS-EDAD-TOTAL = WS-EDAD-TOTAL + REG-EDAD
           .
       240000-EDAD-PROMEDIO-FAMILIA-F. EXIT.
      ******************************************************************
      *                         250000-MAX-DEPARTAMENTO   
      ******************************************************************      
       250000-MAX-DEPARTAMENTO.
           IF REG-VIVIENDA = "D" AND REG-INTEGRANTES > WS-INTE-MAX
               MOVE REG-INTEGRANTES TO WS-INTE-MAX
               MOVE REG-DOMICILIO TO WS-DOMICILIO-AUX
           END-IF
           .
       250000-MAX-DEPARTAMENTO-F. EXIT.
      ******************************************************************
      *                         260000-FEM-MAS   
      ******************************************************************      
       260000-FEM-MAS.
           EVALUATE REG-SEXO
               WHEN "F"
                    ADD 1 TO WS-CANT-FEM
               WHEN "M"
                    ADD 1 TO WS-CANT-MAS
           END-EVALUATE     
           .
       260000-FEM-MAS-F. EXIT.
      ******************************************************************
      *                         270000-ESTUDIO-INCOMPLETO   
      ******************************************************************      
       270000-ESTUDIO-INCOMPLETO.
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
       270000-ESTUDIO-INCOMPLETO-F. EXIT.
      ******************************************************************
      *                         300000-END   
      ****************************************************************** 
       300000-END.
           PERFORM 310000-CLOSE-DATOS
              THRU 310000-CLOSE-DATOS-F
              
           PERFORM 320000-ULTIMO-CORTE
              THRU 320000-ULTIMO-CORTE-F

           PERFORM 330000-RESULTADOS
              THRU 330000-RESULTADOS-F
           STOP RUN   
           .    
       300000-END-F. EXIT.
      ******************************************************************
      *                         310000-CLOSE-DATOS   
      ****************************************************************** 
       310000-CLOSE-DATOS.
           CLOSE DATOS
           IF NOT FS-STATUS-FILE-OK
              MOVE CON-310000-CLOSE-DATOS  TO WS-ERR-PARRAFO 
              MOVE CON-DATOS               TO WS-ERR-OBJETO 
              MOVE CON-CERRAR              TO WS-ERR-OPERACION 
              MOVE FS-STATUS-FILE          TO WS-ERR-CODIGO
              PERFORM 399999-END-PROGRAM
                 THRU 399999-END-PROGRAM-F 
           END-IF
           .
       310000-CLOSE-DATOS-F. EXIT.
      ******************************************************************
      *                         320000-ULTIMO-CORTE   
      ****************************************************************** 
       320000-ULTIMO-CORTE.
           COMPUTE WS-EDAD-FAM-PROM = WS-EDAD-TOTAL / WS-CANT-INTE
           DISPLAY WS-CORTE-DOMICILIO " : " WS-EDAD-FAM-PROM 
           .    
       320000-ULTIMO-CORTE-F. EXIT.
      ******************************************************************
      *                         330000-RESULTADOS   
      ****************************************************************** 
       330000-RESULTADOS.
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
       330000-RESULTADOS-F. EXIT.
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
       END PROGRAM E26.