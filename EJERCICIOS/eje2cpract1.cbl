      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJER2C.
      *------------------------------
        ENVIRONMENT DIVISION.
      *------------------------------

       CONFIGURATION SECTION.
      *------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *------------------------------
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *------------------------------
       01  REGISTROS-DATOS-DE-VENTA.
           05 TIPO.
              10 NUM.
                 15 LONGITUD               PIC 9(05).
                 15 NOMBRE                 PIC X(12) VALUE 'NUMERO-VENTA
      -          ''.
                 15 POSICION-IMPRESION     PIC 9(05).
                 15 EDICION                PIC ZZZZZ.
              10 FILLER.
                 15 LONGITUD               PIC X     VALUE ' '.
                 15 NOMBRE                 PIC X     VALUE ' '.
                 15 POSICION-IMPRESION     PIC X(02).
                 15 EDICION                PIC X     VALUE SPACE.
              10 CARAC .
                 15 LONGITUD               PIC 9(25).
                 15 NOMBRE                 PIC X(06) VALUE 'NOMBRE'.
                 15 POSICION-IMPRESION     PIC X(24).
                 15 EDICION                PIC X     VALUE ' '.
              10 NUM-DEC-CON-SIGNO .
                 15 LONGITUD               PIC 9999,99.
                 15 NOMBRE                 PIC A(05) VALUE 'PESOS'.
                 15 POSICION-IMPRESION     PIC X(08).
                 15 EDICION                PIC $$$0.V99-.
              10 FILLER.
                 15 LONGITUD               PIC X     VALUE ' '.
                 15 NOMBRE                 PIC X     VALUE ' '.
                 15 POSICION-IMPRESION     PIC X(02).
                 15 EDICION                PIC X(02) VALUE '  '.
              10 NUM-DEC-CON-SIGNO .
                 15 LONGITUD               PIC S9(03)V9999.
                 15 NOMBRE                 PIC X(08) VALUE 'GANANCIA'.
                 15 POSICION-DE-IMPRESION  PIC X.
                 15 EDICION                PIC ----9.9999.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "REGISTROS DATOS DE VENTA: " 'REGISTROS-DATOS-DE-VEN
      -    'TA'.
            STOP RUN.
       END PROGRAM EJER2C.
