      ******************************************************************
      *    COPY DE LA RUTINA RUTEDET
      ******************************************************************
      ***** OPCIONES.
      ***** 01 CANTIDAD DE VECES QUE APARECE UNA PALABRA EN UNA FRASE     
      ***** 02 RETORNA LA LONGITUD ORIGINAL DE LA PALABRA, INICIO Y FIN     
      ***** CODIGOS DE RETORNO.     
      *****    - '00' -> OK     
      *****    - '01' -> ERR EN EL FORMATO DE LA CUENTA      
      ******************************************************************
           02 EDECERUT.
              05 EDECERUT-ENTRADA.
                 10 EDET-MAT.
                    15 EDET-FIL    OCCURS 2 TIMES.
                       20 EDET-COL OCCURS 2 TIMES.
                          25 EDET-NUM PIC S9(02).
              05 EDECERUT-RETORNO.
                 10 EDET-DET          PIC S9(04).
                 10 EDET-COD-RET      PIC X(02).
                 10 EDET-REF          PIC X(60).