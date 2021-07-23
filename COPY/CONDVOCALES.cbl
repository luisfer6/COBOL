      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COND1.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  CHAR              PIC X.
           88 VOCAL          VALUE 'a', 'e', 'i', 'o', 'u'.
           88 CONSONANTE     VALUE 'b', 'c', 'd', 'f', 'g', 'h'
                                   'j' THRU 'n', 'p' THRU 't',
                                   'v' THRU 'z'.
           88 DIGITO         VALUE '0' THRU '9'.
           88 CHAR-VALIDO    VALUE 'a' THRU 'z', '0' THRU '9'.
       PROCEDURE DIVISION.
       BEGIN.
           DISPLAY 'Ingrese un caracter o digito en minuscula: '.
           ACCEPT CHAR.
           PERFORM UNTIL NOT CHAR-VALIDO
              EVALUATE TRUE
                WHEN VOCAL
                DISPLAY 'La letra 'CHAR' es una vocal.'
                WHEN CONSONANTE
                DISPLAY 'La letra 'CHAR' es una consonante.'
                WHEN DIGITO
                DISPLAY 'La letra 'CHAR' es un digito.'
                WHEN OTHER
                DISPLAY 'Problemas encontrados '
              END-EVALUATE
           DISPLAY 'Ingrese un caracter o digito en minuscula: '
           ACCEPT CHAR
           IF NOT CHAR-VALIDO THEN
            DISPLAY 'Problemas encontrados en el caracter o digito 'CHAR
           END-IF
           END-PERFORM.
            STOP RUN.
       END PROGRAM COND1.
