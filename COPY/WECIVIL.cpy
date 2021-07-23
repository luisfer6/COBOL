      ******************************************************************
      *Purpose:COPY ESTRUCTURA DEL ARCHIVO DE REGISTRO CIVIL
      *LONGITUD DE REGISTRO 9(16)
      ******************************************************************
       01  WS-REGISTRO-PERSONA.
           02 WS-GENERO.
              88 88-MASCULINO               VALUE 1.
              88 88-FEMENINO                VALUE 2.
              88 88-GENERO-OK               VALUES 1,2.
              88 88-GENERO-SALE             VALUE '99'.
           02 WS-ESTADO-CIVIL.
              88 88-SOLTERX                   VALUE 1.
              88 88-CASADX                    VALUE 2.
              88 88-DIVORCIADX                VALUE 3.
              88 88-VIUDX                     VALUE 4.
              88 88-ESTADO-CIVIL-OK           VALUES 1,2 ,3 ,4.
              88 88-ESTADO-CIVIL-SALE         VALUE '99'.
