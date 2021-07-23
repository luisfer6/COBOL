      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *------------------------
       PROGRAM-ID. EJER12.
      *-----------------------------------------------------------------
      *EJERCICIO 12
      *12)Usando el indicador de numero de nivel 88 en la DATA DIVISION
      * y cláusulas adecuadas de nombres de condición, los
      *  identificadores siguientes se han definido:
      *     a. MASCULINO         FEMENINO
      *     b. SOLTERO-M        CASADO-M        DIVORCIADO-M    VIUDO-M
      *     c. SOLTERO-F        CASADO-F        DIVORCIADO-F    VIUDO-F
      * Suponga que deseamos tabular el numero de individuos que se
      *quedan en las ultimas ocho clases, como por ejemplo, el numero de
      *hombres solteros (SOLTERO-M).
      *Deseamos así probar el campo que tenga el código de
      *identificación  sumarle 1 al contador correspondiente.Suponga
      *también que se deben emplear los siguiente campos como
      *contadores: SM, CM, DM, VM, SF, CF, DF, VF, donde SM significa
      *soltero masculino.
      *a. Dibuje un diagrama para representar la lógica de su
      * programa.
      *b. Escriba una expresión de condicionales que cumpla la prueba
      *y la tabulación requerida.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REGISTRO  ASSIGN TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\REGISTRO
      -    '-GE.TXT'
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS  IS FS-REGISTRO.

           SELECT LISTADO  ASSIGN TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\LISTADO-
      -    'GE.TXT'

                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS  IS FS-LISTADO.

       DATA DIVISION.
       FILE SECTION.

       FD  REGISTRO.
           01  REG-REGISTRO-FD         PIC X(02).

       FD  LISTADO.
           01  REG-LISTADO-ED          PIC X(20) .

       WORKING-STORAGE SECTION.

       77  FS-REGISTRO                 PIC X(02) VALUE ' '.
              88 88-REGISTRO-FIN                 VALUE '10'.
              88 88-REGISTRO-OK                  VALUE '00'.
       77  WS-REGISTRO-OPEN            PIC X(01) VALUE 'N'.
              88  88-REGISTRO-OPEN-SI            VALUE 'S'.
              88  88-REGISTRO-OPEN-NO            VALUE 'N'.
       77  WS-CONT-REG-LEIDOS          PIC 9(03) VALUE 0.
       77  FS-LISTADO                  PIC X(02) VALUE ' '.
       77  WS-LISTADO-OPEN             PIC X(01) VALUE 'N'.
           88  88-LISTADO-SI                     VALUE 'S'.
           88  88-LISTADO-NO                     VALUE 'N'.
       77  WS-VARIABLE-PRINT-AUX       PIC X(20) VALUE ' '.
      *-----------------------------------------------------------------
      *******************REGISTRO-SALIDA********************************
      *-----------------------------------------------------------------
       01  WS-REG-SALIDA.
           05  WS-VARIABLE-PRINT       PIC X(20)  VALUE ' '.
      ***********INICIO ESTRUCTURA DEL ARCHIVO REGISTRO*****************
       01  WS-REG-REGISTRO.
           03 GENERO                   PIC A(01).
           03 ESTADO-CIVIL             PIC A(01).
      *----------FIN ESTRUCTURA DEL ARCHIVO REGISTRO********************

      *-----------------------------------------------------------------
      ******************ENCABEZADO-1-BOCETO*****************************
      *-----------------------------------------------------------------
       01  WS-ENCABEZADO-1.
           03 WS-E-1-GENERO            PIC A(06) VALUE 'GENERO'.
           03 FILLER                   PIC X(02) VALUE ' '.
           03 WS-E-1-ESTADO-CIVIL      PIC X(12) VALUE 'ESTADO-CIVIL'.
      *-----------------------------------------------------------------
      *****************LINEA-1-REGISTRO-CIVILES*************************
      *-----------------------------------------------------------------
       01  WS-REGISTRO-CIVILES.
           03 FILLER                   PIC X(01) VALUE ' '.
           03 WS-GENERO                PIC A(01) VALUE ' '.
           03 FILLER                   PIC X(08) VALUE ' '.
           03 WS-ESTADO-CIVIL          PIC A(01) VALUE ' '.
           03 FILLER                   PIC X(09) VALUE ' '.
      *-----------------------------------------------------------------
      *************CONTADOR**DE**8**INDIVIUDOS************************
      *-----------------------------------------------------------------
       01  WS-CONTADOR.
           05  WS-MS                   PIC 9(02) VALUE 0.
           05  WS-MC                   PIC 9(02) VALUE 0.
           05  WS-MD                   PIC 9(02) VALUE 0.
           05  WS-MV                   PIC 9(02) VALUE 0.
           05  WS-FS                   PIC 9(02) VALUE 0.
           05  WS-FC                   PIC 9(02) VALUE 0.
           05  WS-FD                   PIC 9(02) VALUE 0.
           05  WS-FV                   PIC 9(02) VALUE 0.
      *-----------------------------------------------------------------
      *************INDIVIDUO********************************************
      *-----------------------------------------------------------------
       01  WS-INDIVIDUO.
           03 WS-GENERO-X              PIC X     VALUE ' '.
              88 88-MASCULINO                    VALUE 'M'.
              88 88-FEMENINO                     VALUE 'F'.
           03 WS-ESTADO-CIVIL-X        PIC X     VALUE ' '.
              88 88-SOLTERO                      VALUE 'S'.
              88 88-CASADO                       VALUE 'C'.
              88 88-DIVORCIADO                   VALUE 'D'.
              88 88-VIUDO                        VALUE 'V'.

       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       0000-MAIN-PROCEDURE.

           PERFORM 1000-INICIO.

           PERFORM 2000-PROCESO
               UNTIL 88-REGISTRO-FIN.

           PERFORM 3000-FINALIZO.
           STOP RUN.

      *-----------------------------------------------------------------
       1000-INICIO.

           PERFORM 1100-OPEN-REGISTRO.

           PERFORM 1200-OPEN-LISTADO.

           PERFORM 2200-LEO-REGISTRO.

           IF 88-REGISTRO-FIN
               DISPLAY 'ARCHIVO DE REGISTRO VACIO'
           END-IF.
      *-----------------------------------------------------------------
       1100-OPEN-REGISTRO.

           OPEN INPUT REGISTRO.

           IF FS-REGISTRO EQUALS '00'
              MOVE 'S' TO WS-REGISTRO-OPEN
           ELSE
              DISPLAY 'ERROR OPEN EN REGISTRO'
              DISPLAY 'ERROR CODIGO: ' FS-REGISTRO
              PERFORM 9000-CANCELO
           END-IF.
      *-----------------------------------------------------------------
       1200-OPEN-LISTADO.

           OPEN OUTPUT LISTADO.

           IF FS-LISTADO EQUALS '00'
              MOVE 'S' TO WS-LISTADO-OPEN
           ELSE
              DISPLAY 'ERROR OPEN EN LISTADO'
              DISPLAY 'ERROR CODIGO: ' FS-LISTADO
              PERFORM 9000-CANCELO
           END-IF.
      *-----------------------------------------------------------------
       1300-PRINT-TITLES.

           MOVE WS-ENCABEZADO-1            TO WS-VARIABLE-PRINT.
           PERFORM 2920-WRITE-LISTADO.

           MOVE WS-REGISTRO-CIVILES        TO WS-VARIABLE-PRINT.
           PERFORM 2920-WRITE-LISTADO.
      *-----------------------------------------------------------------
       2000-PROCESO.

           MOVE GENERO                     TO WS-GENERO
           MOVE ESTADO-CIVIL               TO WS-ESTADO-CIVIL
           PERFORM 2905-DIVISION-DATOS.

           MOVE WS-REG-REGISTRO            TO WS-VARIABLE-PRINT

           PERFORM 2200-LEO-REGISTRO.
           PERFORM 2900-DATA-SAVE.

      *-----------------------------------------------------------------
       2200-LEO-REGISTRO.

           INITIALIZE      WS-REG-REGISTRO.

           READ REGISTRO INTO WS-REG-REGISTRO

           EVALUATE TRUE
              WHEN 88-REGISTRO-OK
                   ADD 1                   TO WS-CONT-REG-LEIDOS
              WHEN 88-REGISTRO-FIN
                   CONTINUE
              WHEN OTHER
                   DISPLAY 'ERROR EN READ DE REGISTRO'
                   DISPLAY 'ERROR CODE: ' FS-REGISTRO
                   PERFORM 9000-CANCELO
           END-EVALUATE.

      *-----------------------------------------------------------------
       2900-DATA-SAVE.

                MOVE WS-VARIABLE-PRINT      TO WS-VARIABLE-PRINT-AUX
                PERFORM 1300-PRINT-TITLES

                MOVE WS-VARIABLE-PRINT-AUX  TO WS-VARIABLE-PRINT
                PERFORM 2920-WRITE-LISTADO.

      *-----------------------------------------------------------------
       2905-DIVISION-DATOS.

           MOVE GENERO                      TO WS-GENERO-X.
           MOVE ESTADO-CIVIL                TO WS-ESTADO-CIVIL-X.

           EVALUATE TRUE
                    WHEN 88-MASCULINO
                         PERFORM 2910-MASCULINO
                    WHEN 88-FEMENINO
                         PERFORM 2915-FEMENINO
                    WHEN OTHER
                       CONTINUE
           END-EVALUATE.

      *-----------------------------------------------------------------
       2910-MASCULINO.

             EVALUATE TRUE
                    WHEN 88-SOLTERO
                       ADD 1               TO WS-MS
                    WHEN 88-CASADO
                       ADD 1               TO WS-MC
                    WHEN 88-DIVORCIADO
                       ADD 1               TO WS-MD
                    WHEN 88-VIUDO
                       ADD 1               TO WS-MV
                    WHEN OTHER
                       CONTINUE
           END-EVALUATE.

      *-----------------------------------------------------------------
       2915-FEMENINO.

           EVALUATE TRUE
                    WHEN 88-SOLTERO
                       ADD 1               TO WS-FS
                    WHEN 88-CASADO
                       ADD 1               TO WS-FC
                    WHEN 88-DIVORCIADO
                       ADD 1               TO WS-FD
                    WHEN 88-VIUDO
                       ADD 1               TO WS-FV
                    WHEN OTHER
                       CONTINUE
           END-EVALUATE.

      *-----------------------------------------------------------------
       2920-WRITE-LISTADO.

           WRITE REG-LISTADO-ED FROM WS-VARIABLE-PRINT-AUX.

           EVALUATE FS-LISTADO
               WHEN '00'
                  WRITE REG-LISTADO-ED
                    CONTINUE
               WHEN OTHER
                    PERFORM 9000-CANCELO
           END-EVALUATE.

      *-----------------------------------------------------------------
       3000-FINALIZO.

           PERFORM 9100-CIERRO-ARCHIVOS.
           PERFORM 9400-PRINT-DATOS.

      *-----------------------------------------------------------------
       9000-CANCELO.

           DISPLAY 'PROGRAMA CANCELADO'

           PERFORM 3000-FINALIZO.
      *-----------------------------------------------------------------
       9100-CIERRO-ARCHIVOS.

           EVALUATE TRUE
              WHEN 88-LISTADO-SI
                   PERFORM 9200-CIERRO-LISTADO
           END-EVALUATE

           EVALUATE TRUE
              WHEN 88-REGISTRO-OPEN-SI
                   PERFORM 9300-CIERRO-REGISTRO
           END-EVALUATE.
      *-----------------------------------------------------------------
       9200-CIERRO-LISTADO.

           DISPLAY 'INICIANDO CIERRE LISTADO '

           MOVE 'N' TO WS-LISTADO-OPEN

           CLOSE LISTADO.

           IF FS-LISTADO EQUAL '00'
               CONTINUE
           ELSE
               DISPLAY 'ERROR EN CIERRE'
               DISPLAY 'FS-LISTADO: ' FS-LISTADO
               PERFORM 9000-CANCELO
           END-IF

           DISPLAY 'CIERRE EXITOSO'
           DISPLAY ' '.
      *-----------------------------------------------------------------
       9300-CIERRO-REGISTRO.

           DISPLAY 'INICIANDO CIERRE REGISTRO'

           MOVE 'N' TO WS-REGISTRO-OPEN

           CLOSE REGISTRO.

           IF FS-REGISTRO EQUAL '00'
               CONTINUE
           ELSE
               DISPLAY 'ERROR EN CIERRE'
               DISPLAY 'FS-REGISTRO: ' FS-REGISTRO
               PERFORM 9000-CANCELO
           END-IF

           DISPLAY 'CIERRE EXITOSO'
           DISPLAY ' '
           DISPLAY 'REGISTROS LEIDOS: ' WS-CONT-REG-LEIDOS
           DISPLAY ' '.
      *-----------------------------------------------------------------
       9400-PRINT-DATOS.

           DISPLAY ' MASCULINO  -  FEMENINO'
           DISPLAY ' '
           DISPLAY 'SOLTERO:' WS-MS
                   '  -  '
                   'SOLTERA:' WS-FS
           DISPLAY ' '
           DISPLAY 'CASADO:' WS-MC
                   '  -  '
                   'CASADA:' WS-FC
           DISPLAY ' '
           DISPLAY 'DIVORCIADO:' WS-MD
                   '  -  '
                   'DIVORCIADA:' WS-FD
           DISPLAY ' '
           DISPLAY 'VIUDO:' WS-MV
                   '  -  '
                   'VIUDA:' WS-FV
           DISPLAY ' '

           DISPLAY 'REGISTROS LEIDOS:' WS-CONT-REG-LEIDOS.


       END PROGRAM EJER12.
