      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALL-PRINCIPAL-D.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       77  NOMBRE-PROG            PIC X(8).
           01 REGISTRO-2          PIC X.
           01 REGISTRO-1.
              05 PAGO             PIC S9(5)V99.
              05 VALOR-HORA       PIC S9V99.
              05 HORAS            PIC S99V9.

       PROCEDURE DIVISION USING PAGO,VALOR-HORA,HORAS.
       MAIN-PROCEDURE.

           DISPLAY 'PAGO: ' PAGO.
           DISPLAY 'VALOR-HORA: ' VALOR-HORA.
           DISPLAY 'HORAS: ' HORAS.
      *     MOVE '*' TO NOMBRE APELLIDO
           GOBACK.
       END PROGRAM CALL-PRINCIPAL-D.
