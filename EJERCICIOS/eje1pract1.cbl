      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
      * 1)    Declare las siguientes variables en la WORKING-STORAGE SECTION:
      *a.    Campo alfab�tico de 13 bytes
      *b.    Campo num�rico entero de 3 bytes
      *c.    Campo alfanum�rico de 3 bytes
      *d.    Campo num�rico editado de 4 bytes entero,
      *con supresi�n de 3 ceros a izquierda
      *e.    Campo num�rico editado de 7 bytes con 2 decimales,
      *con supresi�n de 2 ceros a izquierda
      *f.    Campo num�rico de 7 bytes con 2 decimales

       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJERCICIO1.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *--------------------------
       77  CAMPO-ALFABETICO      PIC A(13).
       77  CAMPO-NUMERICO        PIC 9(03).
       77  CAMPO-ALFANUMERICO    PIC X(03).
       77  CAMPO-NUMERO-3        PIC ZZZ9.
       77  CAMPO-NUMERO-2        PIC ZZ999V99.
       77  CAMPO-NUMERO-7        PIC 9(05)V99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            STOP RUN.
       END PROGRAM EJERCICIO1.
