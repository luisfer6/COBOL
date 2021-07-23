      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALL-PRINCIPAL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01  NOMBRE       PIC X(10) VALUE ' ' .
       01  APELLIDO     PIC X(10) VALUE ' '.

       PROCEDURE DIVISION USING NOMBRE,APELLIDO.
       MAIN-PROCEDURE.

           DISPLAY 'NOMBRE: 'NOMBRE
           DISPLAY 'APELLIDO: 'APELLIDO
      *     MOVE '*' TO NOMBRE APELLIDO
           GOBACK.
       END PROGRAM CALL-PRINCIPAL.
