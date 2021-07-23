      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EjerPatente.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT AUTOS ASSIGN TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\vehiculo
      -    '.dat'
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE  IS RANDOM
                                   RECORD KEY   IS ID-KEY-FD
                                   ALTERNATE KEY   PATENTE-KEY-FD
                                   ALTERNATE KEY   MARCA-KEY-FD
                                                   WITH DUPLICATES
                                   FILE STATUS  IS FS-AUTOS.

       DATA DIVISION.
       FILE SECTION.

       FD  AUTOS
           RECORDING MODE IS F
           BLOCK 0.
       01  REG-AUTOS-FD.
           03 ID-KEY-FD         PIC X(05).
           03 PATENTE-KEY-FD    PIC X(09).
           03 MARCA-KEY-FD      PIC X(08).
           03 FILLER            PIC X(17).

       WORKING-STORAGE SECTION.

       77  FS-AUTOS                PIC XX VALUE '  '.
           88 88-AUTOS-OKEY               VALUE '00'.

       77  WS-AUTOS-OPEN           PIC X  VALUE 'N'.
           88 88-AUTOS-OPEN-SI            VALUE 'S'.
           88 88-AUTOS-OPEN-NO            VALUE 'N'.

       77  WS-PATENTE              PIC X(09).
           88 88-PATENTE-FIN       VALUE 'FIN      '.

       COPY WAUTOS.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM 1000-INICIO.

           PERFORM 2000-PROCESO UNTIL 88-PATENTE-FIN.

           PERFORM 3000-FINALIZO.

           STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIO.

           PERFORM 1100-OPEN-AUTOS.

       1100-OPEN-AUTOS.

           OPEN INPUT AUTOS.

           IF FS-AUTOS EQUALS '00'
              SET 88-AUTOS-OPEN-SI TO TRUE
           ELSE
              DISPLAY 'ERROR OPEN EN AUTOS'
              DISPLAY 'ERROR CODE: ' FS-AUTOS
              PERFORM 3000-FINALIZO
           END-IF.
      *----------------------------------------------------------------*
       2000-PROCESO.

           DISPLAY "INGRESE PATENTE. FINALIZAR = FIN"
           ACCEPT WS-PATENTE.

           IF NOT 88-PATENTE-FIN
               PERFORM 2100-LECTURA
           END-IF.

       2100-LECTURA.

           MOVE WS-PATENTE TO PATENTE-KEY-FD

           READ AUTOS RECORD INTO  VEH-REGISTRO KEY IS PATENTE-KEY-FD

           EVALUATE FS-AUTOS
               WHEN '00'
                    CONTINUE
               WHEN '23'
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR EN LECTURA DE AUTOS: ' FS-AUTOS
                    DISPLAY 'CLAVE PATENTE: ' VEH-PATENTE
           END-EVALUATE.

           PERFORM 2200-MOSTRAR.

       2200-MOSTRAR.

           DISPLAY " ".
           DISPLAY "ID AUTO         : " VEH-ID
           DISPLAY "PATENTE DEL AUTO: " VEH-PATENTE.
           DISPLAY "MARCA DEL AUTO  : " VEH-MARCA.
           DISPLAY "COLOR DEL AUTO  : " VEH-COLOR.
           DISPLAY "MODELO DE AUTO  : " VEH-MODELO.

       3000-FINALIZO.

           PERFORM 3100-CERRAR-ARCHIVOS.

       3100-CERRAR-ARCHIVOS.

           IF 88-AUTOS-OPEN-SI
               CLOSE AUTOS

               EVALUATE TRUE
                   WHEN 88-AUTOS-OKEY
                       CONTINUE
                   WHEN OTHER
                       DISPLAY 'ERROR CLOSE: ' FS-AUTOS
                       STOP RUN
               END-EVALUATE
           END-IF.
