      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
      * 6) Supongamos que es necesario cambiar un programa COBOL
      *    existente. La versión original de las entradas pertinentes de
      *    la DATA DIVISION es como sigue:

      *02  CAMPO-A.
      *    03 CAMPO-B
      *    03 CAMPO-C
      *    03 CAMPO-D
      *02  CAMPO-E.
      * En la versión revisada se necesita que los campos sean
      * reestructurados, para que a) se pueda hacer referencia a todos
      * los campos como una unidad; b) se pueda hacer referencia a los
      * campos B y C como una unidad; que C) se pueda hacer referencia
      * a los campos D y E como una entidad. Mostrar como se puede hacer
      * esto.
      *--------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJ6.
      *--------------------------------------
        ENVIRONMENT DIVISION.
      *--------------------------------------

       CONFIGURATION SECTION.
      *--------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *--------------------------------------
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *--------------------------------------
       01  WS-UNIDAD.
           02  CAMPO-A.
               03 CAMPO-B PIC X(10) VALUE 'CAMPO-B'.
               03 CAMPO-C PIC X(10) VALUE 'CAMPO-C'.
               03 CAMPO-D PIC X(10) VALUE 'CAMPO-D'.
           02  CAMPO-E    PIC X(10) VALUE 'CAMPO-E'.
           66  CAMPO-B-C RENAMES CAMPO-B THRU CAMPO-C.
           66  CAMPO-D-E RENAMES CAMPO-D THRU CAMPO-E.

       01  FILLER  REDEFINES WS-UNIDAD.
           02 RCAMPO-B-C  PIC X(20).
           02 RCAMPO-D-E  PIC X(20).



       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            DISPLAY 'RESUELTO CON RENAME..........'.
            DISPLAY "WS-UNIDAD : " WS-UNIDAD.
            DISPLAY "CAMPO-A   : " CAMPO-A.
            DISPLAY "CAMPO-B-C : " CAMPO-B-C.
            DISPLAY "CAMPO-D-E : " CAMPO-D-E.


            DISPLAY ' '.
            DISPLAY 'RESUELTO CON REDEFINES.......'.
            DISPLAY "WS-UNIDAD  : " WS-UNIDAD.
            DISPLAY "RCAMPO-B-C : " RCAMPO-B-C.
            DISPLAY "RCAMPO-D-E : " RCAMPO-D-E.

            MOVE ALL 'A' TO CAMPO-A.
            MOVE ALL 'B' TO CAMPO-B.
            MOVE ALL 'C' TO CAMPO-C.
            MOVE ALL 'D' TO CAMPO-D.
            MOVE ALL 'E' TO CAMPO-E.

            DISPLAY ' '.
            DISPLAY 'LUEGO DE MODIFICAR VALORES'.

            DISPLAY 'RESUELTO CON RENAME..........'.
            DISPLAY "WS-UNIDAD : " WS-UNIDAD.
            DISPLAY "CAMPO-A   : " CAMPO-A.
            DISPLAY "CAMPO-B-C : " CAMPO-B-C.
            DISPLAY "CAMPO-D-E : " CAMPO-D-E.


            DISPLAY ' '.
            DISPLAY 'RESUELTO CON REDEFINES.......'.
            DISPLAY "WS-UNIDAD  : " WS-UNIDAD.
            DISPLAY "RCAMPO-B-C : " RCAMPO-B-C.
            DISPLAY "RCAMPO-D-E : " RCAMPO-D-E.


            MOVE ALL 'BC' TO CAMPO-B-C.
            MOVE ALL 'DE' TO RCAMPO-D-E.


            DISPLAY ' '.
            DISPLAY 'LUEGO DE MODIFICAR VALORES A LOS NUEVOS GRUPOS'.

            DISPLAY 'RESUELTO CON RENAME..........'.
            DISPLAY "WS-UNIDAD : " WS-UNIDAD.
            DISPLAY "CAMPO-A   : " CAMPO-A.
            DISPLAY "CAMPO-B-C : " CAMPO-B-C.
            DISPLAY "CAMPO-D-E : " CAMPO-D-E.


            DISPLAY ' '.
            DISPLAY 'RESUELTO CON REDEFINES.......'.
            DISPLAY "WS-UNIDAD  : " WS-UNIDAD.
            DISPLAY "RCAMPO-B-C : " RCAMPO-B-C.
            DISPLAY "RCAMPO-D-E : " RCAMPO-D-E.

            STOP RUN.
       END PROGRAM EJ6.
