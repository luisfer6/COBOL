      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       77  OPCION                          PIC X VALUE "S".
           88 88-OPCION-SI                 VALUE "S".
           88 88-OPCION-NO                 VALUE "N".

       01  WS-REGISTRO-PERS.
           03 WS-EDUCACION                 PIC 99 VALUE 00.
              88 88-BACHILLERATO           VALUE 12.
              88 88-UNIVERSIDAD            VALUE 16.
              88 88-MAESTRIA               VALUE 17.
              88 88-DOCTORADO              VALUE 20.
           03 WS-EXPERIENCIA               PIC 99 VALUE 00.
           03 WS-SEXO                      PIC 99 VALUE 00.
              88 88-MASCULINO              VALUE 1.
              88 88-FEMENINO               VALUE 2.
           03 WS-PREFERENCIA-GEOGRAFICA    PIC 9  VALUE 0.
              88 88-ESTE                   VALUE 1.
              88 88-CENTRO                 VALUE 2.
              88 88-OESTE                  VALUE 3.
              88 88-SUR                    VALUE 4.
              88 88-DESEA-VIAJAR           VALUE 5.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM 1000-PROCESO UNTIL 88-OPCION-NO.

           PERFORM 2000-FINALIZA.

       1000-PROCESO.

           PERFORM 1200-INGRESAR-DATOS.

           PERFORM 1500-CORROBORAR.

       1200-INGRESAR-DATOS.

           DISPLAY " ".
           DISPLAY "BACHILLERATO= 12, UNIVERSIDAD= 16, MAESTRIA= 17,"
           DISPLAY "DOCTORADO= 20."
           DISPLAY "INGRESE EDUCACION:"
           ACCEPT WS-EDUCACION.

           DISPLAY " ".
           DISPLAY "FEMENINO= 2, MASCULINO= 1"
           DISPLAY "INGRESE SEXO:"
           ACCEPT WS-SEXO.

           DISPLAY " ".
           DISPLAY "INGRESE EXPERIENCIA:"
           ACCEPT WS-EXPERIENCIA.

           DISPLAY "ESTE: 1, CENTRO: 2, OESTE: 3, SUR: 4 , VIAJAR: 5".
           DISPLAY "INGRESE PREFERENCIA GEOGRAFICA:"
           ACCEPT WS-PREFERENCIA-GEOGRAFICA.

       1500-CORROBORAR.

           DISPLAY " ".
           EVALUATE  WS-EXPERIENCIA
                   WHEN 0
                        IF NOT 88-DESEA-VIAJAR
                           DISPLAY "COINCIDE SON LOS REQUISTOS DEL: D"
                        ELSE
                           DISPLAY "NO COINCIDE CON NINGUN REQUISISTO"
                        END-IF
                   WHEN 1
                        IF  88-MASCULINO
                        AND 88-MAESTRIA
                        AND (88-OESTE OR 88-SUR)
                           DISPLAY "COINCIDE SON LOS REQUISTOS DEL: B"
                        ELSE
                           DISPLAY "NO COINCIDE CON NINGUN REQUISISTO"
                        END-IF
                   WHEN 3
                        IF  88-FEMENINO
                        AND 88-DOCTORADO
                        AND 88-ESTE
                           DISPLAY "COINCIDE SON LOS REQUISTOS DEL: C"
                        ELSE
                           DISPLAY "NO COINCIDE CON NINGUN REQUISISTO"
                        END-IF
                   WHEN 5
                        IF  88-MASCULINO
                        AND 88-BACHILLERATO
                        AND 88-DESEA-VIAJAR
                           DISPLAY "COINCIDE SON LOS REQUISTOS DEL: A"
                        ELSE
                           DISPLAY "NO COINCIDE CON NINGUN REQUISISTO"
                        END-IF
                   WHEN >10
                        IF NOT 88-BACHILLERATO
                        AND 88-CENTRO
                           DISPLAY "COINCIDE SON LOS REQUISTOS DEL: E"
                        ELSE
                           DISPLAY "NO COINCIDE CON NINGUN REQUISISTO"
                        END-IF
                   WHEN OTHER
                        DISPLAY "NO COINCIDE CON LO QUE SE REQUIERE"
           END-EVALUATE.

           DISPLAY " ".
           DISPLAY "DESEA SEGUIR CARGANDO DATOS? (S - N)"
           ACCEPT OPCION.

       2000-FINALIZA.

           STOP RUN.

       END PROGRAM YOUR-PROGRAM-NAME.
