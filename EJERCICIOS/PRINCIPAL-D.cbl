      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRINCIPAL-D.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       77  NOMBRE-PROG            PIC X(08).
           01 REGISTRO-2          PIC X.
           01 REGISTRO-1.
              05 PAGO             PIC S9(05)V999   VALUE 10249.992.
              05 VALOR-HORA       PIC S999V999    VALUE 427.083.
              05 HORAS            PIC S99V99      VALUE 13.30.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE 'SUBPROG' TO NOMBRE-PROG.
           CALL NOMBRE-PROG USING PAGO.
           CANCEL NOMBRE-PROG.
           MOVE 'PAGOS' TO NOMBRE-PROG.
           CALL NOMBRE-PROG USING REGISTRO-1 REGISTRO-2
           STOP RUN.
       END PROGRAM PRINCIPAL-D.
