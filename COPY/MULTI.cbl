      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULTIPL1.
      *Programa de ejemplo que usa ACCEPT, DISPLAY y MULTIPLY para
      *obtener dos n�meros de un solo d�gito del usuario y
      *multiplicarlos juntos
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  NUM1                   PIC 9  VALUE 0.
       01  NUM2                   PIC 9  VALUE 0.
       01  RESULTADO              PIC 99 VALUE 0.
       PROCEDURE DIVISION.
       BEGIN.
            DISPLAY 'Ingrese el primer numero: '.
            ACCEPT NUM1.
            DISPLAY 'Ingrese el segundo numero: '.
            ACCEPT NUM2.
            MULTIPLY NUM1 BY NUM2 GIVING RESULTADO.
            DISPLAY 'RESULTADO ES = ', RESULTADO.
            STOP RUN.
       END PROGRAM MULTIPL1.