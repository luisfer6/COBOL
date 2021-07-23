      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJ7.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  TABLA-TERRITORIO.
       77  WS-SEXO        PIC X   VALUE ' '.
           88 88-ES-MASCULINO     VALUE 'M'.
           88 88-ES-FEMENINO     VALUE 'F'.
     *
       77  WS-CALIFIC    PIC 9 VALUE 0.
           88 88-ES-APROBADO   VALUE  4 THRU 10.
           88 88-ES-REPROBADO  VALUE  0 THRU 3.
           88 88-ES-FINAL      VALUE  0 THRU 3.
      *
       01  REG-CALIFICACION.
           03 COD-ALUMNO     PIC 9(03).
           03 FECHA-EXAMEN   PIC 9(10).
           03 NOTA           PIC 9(02).
           03 COD-MATERIA    PIC 9(03).
      *
       77  WS-NOTA      PIC 9 VALUE 0.
           88 88-RECURSA      VALUE  0 THRU 3.
           88 88-RINDE-FINAL  VALUE  4 THRU 6.
           88 88-PROMOCION    VALUE  7 THRU 10.
           02 FILLER                        PIC X(03) VALUE '001'.
           02 FILLER                        PIC X(01) VALUE ' '.
           02 FILLER                        PIC X(03) VALUE '230'.
           02 FILLER                        PIC X(01) VALUE ' '.
           02 FILLER                        PIC X(03) VALUE '198'.
           03  TABLA-CODIGO.
           03 FILLER                        PIC X(03) VALUE '001'.
           03 FILLER                        PIC X(01) VALUE ' '.
           03 FILLER                        PIC X(03) VALUE '230'.
           03 FILLER                        PIC X(01) VALUE ' '.
           03 FILLER                        PIC X(03) VALUE '198'.

       01  TABLA REDEFINES TABLA-INICIAL.
           03 CAMPO-TABLA-V      OCCURS 6 TIMES.
              05 CAMPO-TABLA              PIC X(03).
              05 FILLER                   PIC X(01).


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Hello world"

           DISPLAY 'TABLA-INICIAL: ' TABLA-INICIAL.

           SET 88-HUBO-CAMBIO-SI    TO TRUE.
           PERFORM UNTIL 88-HUBO-CAMBIO-NO

               SET 88-HUBO-CAMBIO-NO    TO TRUE
               PERFORM VARYING WS-I FROM 1 BY 1
                 UNTIL WS-I > 5
                 IF CAMPO-TABLA(WS-I) > CAMPO-TABLA(WS-I + 1)
                    MOVE CAMPO-TABLA(WS-I + 1) TO WS-AUX
                    MOVE CAMPO-TABLA(WS-I)     TO CAMPO-TABLA(WS-I + 1)
                    MOVE WS-AUX                TO CAMPO-TABLA(WS-I)
                    SET 88-HUBO-CAMBIO-SI      TO TRUE
                    DISPLAY 'TABLA: ' TABLA
                  END-IF
                END-PERFORM
                DISPLAY ' '
           END-PERFORM.
           DISPLAY 'TABLA-FINAL: ' TABLA.


           STOP RUN.
       END PROGRAM EJ7.
