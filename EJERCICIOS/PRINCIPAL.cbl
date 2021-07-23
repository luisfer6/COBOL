      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRINCIPAL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  NOMBRE                   PIC X(10) VALUE 'DANIEL' .
       01  APELLIDO                 PIC X(10) VALUE 'SANCHEZ'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           CALL "CALL-PRINCIPAL" USING  APELLIDO,NOMBRE.
           CALL "CALL-PRINCIPAL" USING  NOMBRE,APELLIDO.
           DISPLAY ' '.
           CALL "CALL-PRINCIPAL" USING BY CONTENT APELLIDO,NOMBRE.
           CALL "CALL-PRINCIPAL" USING BY CONTENT NOMBRE,APELLIDO.
           STOP RUN.
       END PROGRAM PRINCIPAL.
