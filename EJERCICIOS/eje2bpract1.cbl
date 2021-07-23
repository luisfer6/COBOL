      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJERCICIO2B.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *------------------------------
       01  REGISTROS-PAGOS.
           05 EMPLEADO.
              10 DEPART         PIC A(02)      VALUE ' '.
              10 NOMBRE         PIC 9(05)      VALUE 0.
           05 TASA              PIC 9(04)V99   VALUE 0.
           05 CARGO             PIC A(01)      VALUE ' '.
           05 PAGO-A-LA-FECHA.
              10 BRUTO.
                 15 REGULAR     PIC 9(07)V99   VALUE 0.
                 15 EXTRAS      PIC 9(06)V99   VALUE 0.
              10 TASA-SS        PIC 9(05)V99   VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            STOP RUN.
       END PROGRAM EJERCICIO2B.
