      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJER-5.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHEQUE              ASSIGN TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\CHEQUE.t
      -    'xt'

                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS IS FS-CHEQUE.

           SELECT LISTADO              ASSIGN TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\LISTADO.
      -    'txt'
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS IS FS-LISTADO.

       DATA DIVISION.

       FILE SECTION.

       FD  CHEQUE.
           01 REG-CHEQUE-FD          PIC X(54)  VALUE ' '.

       FD  LISTADO.
           01  REG-LISTADO-ED         PIC X(133) VALUE ' '.
      *         03 REG-LISTADO-CC      PIC X(01)  VALUE ' '.
      *         03 REG-LISTADO-DET     PIC X(133) VALUE ' '.

       WORKING-STORAGE SECTION.
       77  FS-CHEQUE              PIC X(02)  VALUE ' '.
           88  88-CHEQUE-FIN                 VALUE '10'.
           88  88-CHEQUE-OKEY                VALUE '00'.
       77  WS-CHEQUE-OPEN         PIC X(01)  VALUE 'N'.
           88  88-CHEQUE-OPEN-SI             VALUE 'S'.
           88  88-CHEQUE-OPEN-NO             VALUE 'N'.
       77  WS-CONT-REG-LEIDOS     PIC 9(05)  VALUE 0.
       77  WS-SALTO-DE-HOJA       PIC X(01)  VALUE 'N'.
           88  88-SALTO-DE-HOJA-SI           VALUE 'S'.
           88  88-SALTO-DE-HOJA-NO           VALUE 'N'.
       77  FS-LISTADO             PIC X(02)  VALUE ' '.
       77  WS-LISTADO-OPEN        PIC X(01)  VALUE 'N'.
           88  88-LISTADO-SI                 VALUE 'S'.
           88  88-LISTADO-NO                 VALUE 'N'.
       77  WS-HOJA                PIC 9(05)  VALUE 0.
       77  WS-LINEA               PIC 99     VALUE 90.
       77  WS-VARIABLE-PRINT-AUX  PIC X(132) VALUE ' '.
       01  WS-REG-SALIDA.
           05  WS-CC                  PIC X(01)  VALUE ' '.
           05  WS-VARIABLE-PRINT      PIC X(132) VALUE ' '.
      *-----------------------------------------------------------------
       COPY WCHEQUE.
      *----------FIN ESTRUCTURA DEL ARCHIVO CHEQUE *********************
      *----------ENCABEZADOS********************************************
       01  WS-ENCABEZADO-1.
           03  FILLER              PIC X(07) VALUE 'FECHA: '.
           03  WS-E-1-FECHA-DD     PIC 9(02) VALUE 0.
           03  FILLER              PIC X(01) VALUE '/'.
           03  WS-E-1-FECHA-MM     PIC 9(02) VALUE 0.
           03  FILLER              PIC X(01) VALUE '/'.
           03  WS-E-1-FECHA-AAAA   PIC 9(04) VALUE 0.
           03  FILLER              PIC X(44) VALUE ' '.
           03  FILLER              PIC X(19)
                                   VALUE 'LISTADO DE CLIENTES'.
           03  FILLER              PIC X(40) VALUE ' '.
           03  FILLER              PIC X(06) VALUE 'HOJA:'.
           03  WS-E-1-HOJA         PIC ZZ.ZZ9.

       01  WS-ENCABEZADO-2.
           03  FILLER              PIC X(07) VALUE 'HORA:'.
           03  WS-E-2-HORA-HS      PIC 9(02) VALUE 0.
           03  FILLER              PIC X(01) VALUE ':'.
           03  WS-E-2-HORA-MS      PIC 9(02) VALUE 0.
           03  FILLER              PIC X(49) VALUE ' '.
           03  FILLER              PIC X(19)
                                   VALUE ALL '*'.

       01  WS-CURRENT-DATE.
           03 WS-CURRENT-DATE-DATE.
              05 WS-CURRENT-DATE-YYYY    PIC 9(04) VALUE 0.
              05 WS-CURRENT-DATE-MM      PIC 9(02) VALUE 0.
              05 WS-CURRENT-DATE-DD      PIC 9(02) VALUE 0.
           03 WS-CURRENT-DATE-TIME.
              05 WS-CURRENT-DATE-HS      PIC 9(02) VALUE 0.
              05 WS-CURRENT-DATE-MS      PIC 9(02) VALUE 0.
              05 WS-CURRENT-DATE-SS      PIC 9(02) VALUE 0.

       01  WS-TITULOS-1.
           05  FILLER             PIC X(15)  VALUE 'NOMBRE DEL'.
           05  FILLER             PIC X(11)  VALUE 'NUMERO DEL'.
           05  FILLER             PIC X(11)  VALUE 'NUMERO DEL'.
           05  FILLER             PIC X(11)  VALUE 'FECHA'.
           05  FILLER             PIC X(12)  VALUE 'DEBITO'.
           05  FILLER             PIC X(15)  VALUE 'DESCUENTO'.
           05  FILLER             PIC X(10)  VALUE 'EFECTIVO'.
       01  WS-TITULOS-2.
           05  FILLER             PIC X(15)  VALUE 'VENDEDOR'.
           05  FILLER             PIC X(11)  VALUE 'VENDEDOR'.
           05  FILLER             PIC X(10)  VALUE 'CHEQUE'.
       01  WS-TITULOS-3           PIC X(132) VALUE ' '.
       01  WS-REGISTRO-CHEQUES.
           05 WS-NOM-V            PIC X(15)  VALUE ' '.
           05 WS-NUM-V            PIC 9999   VALUE 0.
           05 FILLER              PIC X(7)   VALUE ' '.
           05 WS-NUM-CHEQUE       PIC 99999  VALUE 0.
           05 FILLER              PIC X(6)   VALUE ' '.
           05 WS-FECHA.
              10 WS-DIA           PIC 99     VALUE 0.
              10 FILLER           PIC X      VALUE '/'.
              10 WS-MES           PIC 99     VALUE 0.
              10 FILLER           PIC X      VALUE '/'.
              10 WS-AÑO           PIC 9999   VALUE 0.
           05 FILLER              PIC X(1)   VALUE ' '.
           05 WS-DEBITO           PIC $$$.$$$9,99 VALUE '0'.
           05 FILLER              PIC X(1)        VALUE ' '.
           05 WS-DESCUENTO        PIC $$.$$9,99   VALUE '0'.
           05 FILLER              PIC X(2)        VALUE ' '.
           05 WS-EFECTIVO         PIC $$$.$$$9,99 VALUE '0'.
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       0000-MAIN-PROCEDURE.

           PERFORM 1000-INICIO.

           PERFORM 2000-PROCESO
               UNTIL 88-CHEQUE-FIN.

           PERFORM 3000-FINALIZO.

      *-----------------------------------------------------------------
       1000-INICIO.

           PERFORM 1100-OPEN-CHEQUE.

           PERFORM 1200-OPEN-LISTADO.

           PERFORM 2100-LEO-CHEQUE.

           IF 88-CHEQUE-FIN
               DISPLAY "ARCHIVO DE CHEQUE VACIO"
           END-IF.

      *-----------------------------------------------------------------
       2000-PROCESO.

           MOVE NOMBRE        TO WS-NOM-V
           MOVE NUMERO        TO WS-NUM-V
           MOVE CHEQUE-NUMERO TO WS-NUM-CHEQUE
           MOVE DIA           TO WS-DIA
           MOVE MES           TO WS-MES
           MOVE ANIO          TO WS-AÑO
           MOVE DEBITO        TO WS-DEBITO
           MOVE DESCUENTO     TO WS-DESCUENTO
           MOVE EFECTIVO      TO WS-EFECTIVO

           MOVE WS-REGISTRO-CHEQUES     TO WS-VARIABLE-PRINT

      *     PERFORM 2900-DATA-SAVE

           PERFORM 2100-LEO-CHEQUE.
      *-----------------------------------------------------------------
       2100-LEO-CHEQUE.

           READ CHEQUE INTO REG-CHEQUE
      *     READ CHEQUE
      *    MOVE REG-CHEQUE-FD TO REG-CHEQUE
           EVALUATE TRUE
              WHEN 88-CHEQUE-OKEY
                   ADD 1                        TO WS-CONT-REG-LEIDOS
              WHEN 88-CHEQUE-FIN
                   CONTINUE
              WHEN OTHER
                   DISPLAY 'ERROR EN READ DE CHEQUE'
                   DISPLAY 'ERROR CODE: ' FS-CHEQUE
                   PERFORM 9000-CANCELO
           END-EVALUATE.
      *-----------------------------------------------------------------
       3000-FINALIZO.

           PERFORM 9100-CIERRO-ARCHIVOS.
           STOP RUN.
      *-----------------------------------------------------------------
       1100-OPEN-CHEQUE.

           OPEN INPUT CHEQUE.

           IF FS-CHEQUE EQUALS '00'
              MOVE 'S' TO WS-CHEQUE-OPEN
           ELSE
              DISPLAY 'ERROR OPEN EN CHEQUE'
              DISPLAY 'ERROR CODE: ' FS-CHEQUE
              PERFORM 9000-CANCELO
           END-IF.
      *-----------------------------------------------------------------
       1200-OPEN-LISTADO.

           OPEN OUTPUT LISTADO.

           IF FS-LISTADO EQUALS '00'
              MOVE 'S' TO WS-LISTADO-OPEN
           ELSE
              DISPLAY 'ERROR OPEN EN LISTADO'
              DISPLAY 'ERROR CODE: ' FS-LISTADO
              PERFORM 9000-CANCELO
           END-IF.
      *-----------------------------------------------------------------
       1300-PRINT-TITLES.

           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE.
           MOVE WS-CURRENT-DATE-DD         TO WS-E-1-FECHA-DD.
           MOVE WS-CURRENT-DATE-MM         TO WS-E-1-FECHA-MM.
           MOVE WS-CURRENT-DATE-YYYY       TO WS-E-1-FECHA-AAAA.
           ADD 1                           TO WS-HOJA.
           MOVE WS-HOJA                    TO WS-E-1-HOJA.
           MOVE WS-ENCABEZADO-1            TO WS-VARIABLE-PRINT.
           PERFORM 2910-WRITE-LISTADO.

           MOVE WS-CURRENT-DATE-HS         TO WS-E-2-HORA-HS.
           MOVE WS-CURRENT-DATE-MS         TO WS-E-2-HORA-MS.
           MOVE WS-ENCABEZADO-2            TO WS-VARIABLE-PRINT.
           PERFORM 2910-WRITE-LISTADO.

           MOVE ALL SPACE                  TO WS-VARIABLE-PRINT.
           PERFORM 2910-WRITE-LISTADO.

           MOVE WS-TITULOS-1               TO WS-VARIABLE-PRINT.
           PERFORM 2910-WRITE-LISTADO.

           MOVE WS-TITULOS-2               TO WS-VARIABLE-PRINT.
           PERFORM 2910-WRITE-LISTADO.

           MOVE WS-TITULOS-3               TO WS-VARIABLE-PRINT.
           PERFORM 2910-WRITE-LISTADO.
      *-----------------------------------------------------------------
      *  2900-DATA-SAVE.

      *     IF WS-LINEA < 66
      *         PERFORM 2910-WRITE-LISTADO
      *        ADD 1                            TO WS-LINEA
      *     ELSE
      *         MOVE WS-VARIABLE-PRINT TO WS-VARIABLE-PRINT-AUX
      *         SET 88-SALTO-DE-HOJA-SI TO TRUE
      *         PERFORM 1300-PRINT-TITLES
      *         MOVE WS-VARIABLE-PRINT-AUX TO WS-VARIABLE-PRINT
      *        PERFORM 2910-WRITE-LISTADO
      *         MOVE 7                  TO WS-LINEA
      *     END-IF.

       2910-WRITE-LISTADO.

           EVALUATE TRUE
               WHEN 88-SALTO-DE-HOJA-SI
                   MOVE X'0a'               TO REG-LISTADO-ED
                   WRITE REG-LISTADO-ED
                   WRITE REG-LISTADO-ED     FROM WS-REG-SALIDA
                                            AFTER 1
      *                                      AFTER PAGE
                   SET 88-SALTO-DE-HOJA-NO      TO TRUE
               WHEN OTHER
                   MOVE " "                 TO WS-CC
                   WRITE REG-LISTADO-ED     FROM WS-REG-SALIDA
                                            AFTER 1
           END-EVALUATE

           EVALUATE FS-LISTADO
               WHEN '00'
                    CONTINUE
               WHEN OTHER
                    PERFORM 9000-CANCELO
           END-EVALUATE.

           EVALUATE FS-CHEQUE
               WHEN '00'
                    CONTINUE
               WHEN OTHER
                    PERFORM 9000-CANCELO
           END-EVALUATE.
      *-----------------------------------------------------------------
       9000-CANCELO.

           DISPLAY "PROGRAMA CANCELADO"

           PERFORM 3000-FINALIZO.
      *-----------------------------------------------------------------
       9100-CIERRO-ARCHIVOS.

           EVALUATE TRUE
              WHEN 88-LISTADO-SI
                   PERFORM 9200-CIERRO-LISTADO-1
           END-EVALUATE

           EVALUATE TRUE
              WHEN 88-CHEQUE-OPEN-SI
                   PERFORM 9300-CIERRO-CHEQUE
           END-EVALUATE.
      *-----------------------------------------------------------------
       9200-CIERRO-LISTADO-1.

           DISPLAY "INICIANDO CIERRE LISTADO "

           MOVE 'N' TO WS-LISTADO-OPEN

           CLOSE LISTADO.

           IF FS-LISTADO EQUAL '00'
               CONTINUE
           ELSE
               DISPLAY "ERROR EN CIERRE"
               DISPLAY "FS-LISTADO: " FS-LISTADO
               PERFORM 9000-CANCELO
           END-IF

           DISPLAY "CIERRE EXITOSO".
      *-----------------------------------------------------------------
       9300-CIERRO-CHEQUE.

           DISPLAY "INICIANDO CIERRE CHEQUE"

           MOVE 'N' TO WS-CHEQUE-OPEN

           CLOSE CHEQUE.

           IF FS-CHEQUE EQUAL '00'
               CONTINUE
           ELSE
               DISPLAY "ERROR EN CIERRE"
               DISPLAY "FS-CHEQUE: " FS-CHEQUE
               PERFORM 9000-CANCELO
           END-IF

           DISPLAY "CIERRE EXITOSO"
           DISPLAY "REGISTROS LEIDOS: " WS-CONT-REG-LEIDOS.

       END PROGRAM EJER-5.
