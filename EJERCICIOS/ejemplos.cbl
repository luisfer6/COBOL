      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GENERO.
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
       77  WS-SEXO        PIC X   VALUE ' '.
           88 88-ES-MASCULINO     VALUE 'M'.
           88 88-ES-FEMENINO     VALUE 'F'.
     **    IF 88-ES-HOMBRE
      *       SET 88-ES-MASCULINO TO TRUE
      *    END-IF
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
       77  WS-NOTA      PIC 9(02) VALUE 7.
           88 88-RECURSA      VALUE  0 THRU 3.
           88 88-RINDE-FINAL  VALUE  4 THRU 6.
           88 88-PROMOCION    VALUE  7 THRU 10.

      *    MOVE 5 TO WS-NOTA.
      *    IF 88-ACOMADOS
      *       SET 88-PROMOCION TO TRUE
      *    END-IF
           0102021-05-0507420
           7652021-05-0503420
             EVALUATE NOTA
              WHEN 0                       *MOVE NOTA TO WS-NOTA.
                                         0000-TRONCO-PRINCIPAL
              WHEN 1                      * EVALUATE TRUE
              WHEN 2                          WHEN 88-RECURSA
              WHEN 3                               PERFORM 1000-RECURSA
                   PERFORM 1000-RECURSA     *  WHEN 88-RINDE-FINAL
              WHEN 4                               PERFORM 2000-RINDE-FINAL
              WHEN 5                         * WHEN 88-RINDE-FINAL
              WHEN 6                               PERFORM 3000-PROMOCION
                   PERFORM 2000-RINDE-FINAL  * WHEN OTHER
              WHEN 7                            DISPLAY 'NOTA INVALIDA'
              WHEN 8                          * END-EVALUATE.
              WHEN 9                            STOP RUN.
                   PERFORM 3000-RECURSA        *IF 88-RECURSA
              WHEN OTHER                            PERFORM 1000-RECURSA
                   DISPLAY 'NOTA INVALIDA'.    *ELSE
                                               *    IF 88-RINDE-FINAL
       PROCEDURE DIVISION.                       *     PERFOM 2000-RINDE-FINAL
       MAIN-PROCEDURE.                            * ELSE
                                                       IF 88-PROMOCION
                                                          PERFORM 3000-PROMOCION
      *                                                ELSE
           EVALUATE TRUE                                  DISPLAY 'NOTA INVALIDA'
              WHEN 88-ES-MASCULINO                        END-IF
                   PERFORM 30000-PROCESO-MASC    END-IF
              WHEN 88-ES-FEMENINO              0000-CONTINUA.
                   PERFORM 30000-PROCESO-MASC      INSTRUCCION-YYYY.
              WHEN 88-ES-OTHER                     STOP RUN.
                   DISPLAY 'NO ES MASC NI FEM'. 1000-RECURSA.
           END-EVALUATE.                            INSTRUCCION-XXX
                                                    INSTRUCCION-XXXXXX
                                                2000-RINDE-FINAL
                                                    INSTRUCCION-XXX
                                                    INSTRUCCION-XXXXXX
                                                3000-PROMOCION
                                                    INSTRUCCION-XXX
                                                    INSTRUCCION-XXXXXX



            STOP RUN.
       END PROGRAM GENERO.
