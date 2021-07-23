      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
      *11)Considere los siguientes enunciados de la DATA DIVISION
      * relacionados con un registro de personal:
      *02  EDUCACION PIC 99.
      *    88 BACHILLERATO VALUE 12.
      *    88 UNIVERSIDAD VALUE 16.
      *    88 MAESTRIA VALUE 17.
      *    88 DOCTORADO VALUE 20.
      *02  EXPERIENCIA PIC 99.
      *02  SEXO PIC 99.
      *    88  MASCULINO  VALUE 1.
      *    88 FEMENINO  VALUE 2.
      *02  PREFERENCIA-GEOGRAFICA PIC 9.
      *    88 ESTE VALUE 1.
      *    88 CENTRO VALUE 2.
      *    88 OESTE VALUE 3.
      *    88 SUR VALUE 4.
      *    88 DESEA-VIAJAR VALUE 5.
      *Suponga que deseamos encontrar individuos que reúnan uno de estos
      *tres requisitos:
      *  a.    Cinco años de experiencia, sexo masculino, bachiller,
      *     desea viajar.
      *  b.    Hombre con un año de experiencia, con maestría, que
      *   prefiere el oeste o el sur.
      *  c.    Con tres años de experiencia, mujer, doctorada, que
      *    prefiere el este.
      *  d.    Sexo indistinto, sin experiencia que no desea viajar.
      *  e.    Título universitario o superior, con mas de 10 años de
      *     experiencia que prefiere el centro.
      * Escriba una oración condicional por medio de la cual podamos
      * comprobar si un registro cumple con algún requisito.
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *------------------------
       PROGRAM-ID. EJERCICIO-11.
       DATA DIVISION.
      *--------------
       FILE SECTION.
      *-------------
       WORKING-STORAGE SECTION.
      *------------------------
       77  OPCION                        PIC X  VALUE 'S'.
           88 88-OPCION-SI                      VALUE 'S'.
           88 88-OPCION-NO                      VALUE 'N'.
       01  WS-REG-POSTULANTE.
           02 WS-EDUCACION.
              03 WS-EDUCACION-N          PIC 99 VALUE 00 .
                 88 88-BACHILLERATO             VALUE 12.
                 88 88-UNIVERSIDAD              VALUE 16.
                 88 88-MAESTRIA                 VALUE 17.
                 88 88-DOCTORADO                VALUE 20.
                 88 88-EDUCACION-OK             VALUES 12, 16, 17, 20.
                 88 88-EDUCACION-SALE           VALUE 99.
           02  WS-EXPERIENCIA.
               03 WS-EXPERIENCIA-N       PIC 99 VALUE 00.
                  88 88-EXPERIENCIA-OK             VALUE 1, 5, 3, 0, 10.
                  88 88-EXPERIENCIA-SALE           VALUE 99.
           02  WS-SEXO.
               03 WS-SEXO-N              PIC 99 VALUE 00.
                   88 88-MASCULINO              VALUE 1.
                   88 88-FEMENINO               VALUE 2.
                   88 88-SEXO-OK                VALUES 1, 2.
                   88 88-SEXO-SALE              VALUE '99'.
           02  WS-ZONA.
               03 WS-ZONA-N               PIC 9 VALUE 0.
               88 88-ESTE                       VALUE 1.
               88 88-CENTRO                     VALUE 2.
               88 88-OESTE                      VALUE 3.
               88 88-SUR                        VALUE 4.
               88 88-DESEA-VIAJAR               VALUE 5.
               88 88-ZONA-OK                    VALUES  1, 2 ,3 ,4, 5.
               88 88-ZONA-SALE                  VALUE '99'.

       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       0000-MAIN-PROCEDURE.
           PERFORM 1000-INICIO.
           PERFORM 2000-PROCESO UNTIL 88-OPCION-NO.
           PERFORM 3000-FINALIZA.
      *-----------------------------------------------------------------
       1000-INICIO.
      *    Detalles del Postulante

           INITIALIZE WS-REG-POSTULANTE.



           PERFORM 1210-INGRESO-EDUCACION
                UNTIL 88-EDUCACION-OK
                   OR 88-OPCION-NO.
                   PERFORM 1220-INGRESO-SEXO
                UNTIL 88-EDUCACION-OK
                   OR 88-OPCION-NO.
                   PERFORM 1220-INGRESO-SEXO
                UNTIL 88-EDUCACION-OK
                   OR 88-OPCION-NO.
                   PERFORM 1220-INGRESO-SEXO
                UNTIL 88-SEXO-OK
                   OR 88-OPCION-NO.

           PERFORM 1220-INGRESO-SEXO
                UNTIL 88-SEXO-OK
                   OR 88-OPCION-NO.
                   PERFORM 1220-INGRESO-SEXO
                UNTIL 88-SEXO-OK
                   OR 88-OPCION-NO.
                   PERFORM 1220-INGRESO-SEXO
                UNTIL 88-SEXO-OK
                   OR 88-OPCION-NO.
                   PERFORM 1220-INGRESO-SEXO
                UNTIL 88-SEXO-OK
                   OR 88-OPCION-NO.

           PERFORM 1230-INGRESO-EXPERIENCIA
                UNTIL 88-EXPERIENCIA-OK
                   OR 88-OPCION-NO.
                   PERFORM 1230-INGRESO-EXPERIENCIA
                UNTIL 88-EXPERIENCIA-OK
                   OR 88-OPCION-NO.
                   PERFORM 1230-INGRESO-EXPERIENCIA
                UNTIL 88-EXPERIENCIA-OK
                   OR 88-OPCION-NO.
                   PERFORM 1230-INGRESO-EXPERIENCIA
                UNTIL 88-EXPERIENCIA-OK
                   OR 88-OPCION-NO.

              PERFORM 1240-INGRESO-ZONA
                UNTIL 88-ZONA-OK
                   OR 88-OPCION-NO.
                   PERFORM 1240-INGRESO-ZONA
                UNTIL 88-ZONA-OK
                   OR 88-OPCION-NO.
                   PERFORM 1240-INGRESO-ZONA
                UNTIL 88-ZONA-OK
                   OR 88-OPCION-NO.
                   PERFORM 1240-INGRESO-ZONA
                UNTIL 88-ZONA-OK
                   OR 88-OPCION-NO.

      *-----------------------------------------------------------------
       1210-INGRESO-EDUCACION.
           DISPLAY ' '.
           DISPLAY 'BACHILLERATO=12, UNIVERSIDAD=16, MAESTRIA=17,'
           DISPLAY 'DOCTORADO=20.'
           DISPLAY 'Ingrese Educacion:'.
           ACCEPT WS-EDUCACION.
           IF WS-EDUCACION-N IS NUMERIC
              IF 88-EDUCACION-SALE
                 DISPLAY ' ESTA ACTIVO EDUCACION SALE'
                 SET 88-OPCION-NO TO TRUE
              END-IF
           ELSE
             DISPLAY 'VALOR PARA EDUCACION INVALIDO'
           END-IF.
       1220-INGRESO-SEXO.
           DISPLAY ' '.
           DISPLAY 'Ingrese Sexo: '.
           DISPLAY 'FEMENINO=2, MASCULINO= 1'
           DISPLAY 'Ingrese Sexo:'
           ACCEPT WS-SEXO.
           IF WS-SEXO-N IS NUMERIC
            IF 88-SEXO-SALE
               DISPLAY ' ESTA ACTIVO SEXO SALE'
               SET 88-OPCION-NO TO TRUE
              END-IF
           ELSE
               DISPLAY 'VALOR PARA SEXO INVALIDO'
           END-IF.
       1230-INGRESO-EXPERIENCIA.
           DISPLAY ' '.
           DISPLAY 'Ingrese Experiencia:'.
           ACCEPT WS-EXPERIENCIA.
           IF WS-EXPERIENCIA-N IS NUMERIC
            IF 88-EXPERIENCIA-SALE
               DISPLAY ' ESTA ACTIVO EXPERIENCIA SALE'
               SET 88-OPCION-NO TO TRUE
              END-IF
           ELSE
               DISPLAY 'VALOR PARA EXPERIENCIA INVALIDO'
           END-IF.
       1240-INGRESO-ZONA.
           DISPLAY ' '.
           DISPLAY 'ESTE:1, CENTRO: 2, OESTE:3, SUR :4, VIAJAR: 5'
           DISPLAY 'Ingrese Preferencia geográfica:'.
           ACCEPT WS-ZONA.
           IF WS-ZONA-N IS NUMERIC
            IF 88-ZONA-SALE
               DISPLAY ' ESTA ACTIVO ZONA SALE'
               SET 88-OPCION-NO TO TRUE
              END-IF
           ELSE
               DISPLAY 'VALOR PARA ZONA INVALIDO'
           END-IF.



       2000-PROCESO.

           DISPLAY ' '.
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
           DISPLAY "¿DESEA SEGUIR CARGANDO DATOS? (S - N)"
           ACCEPT OPCION.

       3000-FINALIZA.

           STOP RUN.

       END PROGRAM EJERCICIO-11.
