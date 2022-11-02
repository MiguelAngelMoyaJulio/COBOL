      ******************************************************************
      *    COPY DE LA RUTINA RUTSTRIN
      ******************************************************************
      ***** OPCIONES.
      ***** 01 CANTIDAD DE VECES QUE APARECE UNA PALABRA EN UNA FRASE     
      ***** 02 RETORNA LA LONGITUD ORIGINAL DE LA PALABRA, INICIO Y FIN     
      ***** CODIGOS DE RETORNO.     
      *****    - '00' -> OK     
      *****    - '01' -> ERR PALABRA DE MAS DE 23 POSICIONES      
      ******************************************************************
           02 STRCERUT.
              05 STRCERUT-ENTRADA.
                 10 STRCERUT-OPCION            PIC 9(02).
                 10 STRCERUT-FRASE             PIC X(250).
                 10 STRCERUT-PALABRA           PIC X(23).
              05 STRCERUT-RETORNO.
                 10 STRCERUT-POS-INI           PIC 9(03).   
                 10 STRCERUT-POS-FIN           PIC 9(03).   
                 10 STRCERUT-LEN               PIC 9(03).   
                 10 STRCERUT-CANT-OCURRENCIAS  PIC 9(03).   
                 10 STRCERUT-COD-RET           PIC X(02).   