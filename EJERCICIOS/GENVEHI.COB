       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID. GENVEHI.
      *AUTHOR. EDUARDO PALMEYRO.

       ENVIRONMENT DIVISION.
      *--------------------

       CONFIGURATION SECTION.
      *---------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.


       INPUT-OUTPUT SECTION.
      *--------------------

       FILE-CONTROL.
      *------------

           SELECT VEHICULOI        ASSIGN       TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\vehiculo
      -    '.txt'
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS  IS FS-VEHICULOI.

           SELECT VEHICULOO         ASSIGN       TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\vehiculo
      -    '.dat'
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE  IS SEQUENTIAL
                                   RECORD KEY   IS VEH-CLAVE-FD
                                   ALTERNATE KEY   VEH-CLAVE-2-FD
                                   ALTERNATE KEY   VEH-CLAVE-3-FD
                                                   WITH DUPLICATES
                                   FILE STATUS  IS FS-VEHICULOO.


       DATA DIVISION.
      *-------------

       FILE SECTION.
      *------------

       FD  VEHICULOI
           RECORDING MODE IS F
           BLOCK 0.
       01  REGI-VEH-REGISTRO-FD               PIC X(39).

       FD  VEHICULOO
           RECORDING MODE IS F
           BLOCK 0.
       01  REGO-VEH-REGISTRO-FD.
           03 VEH-CLAVE-FD               PIC X(05).
           03 VEH-CLAVE-2-FD             PIC X(09).
           03 VEH-CLAVE-3-FD             PIC X(08).
           03 FILLER                     PIC X(17).


       WORKING-STORAGE SECTION.
      *-----------------------
       77  FS-VEHICULOI                  PIC X(02) VALUE ' '.
           88 88-FS-VEHICULOI-OK                   VALUE '00' '97'.
           88 88-FS-VEHICULOI-EOF                  VALUE '10'.
       77  WS-OPEN-VEHICULOI             PIC X     VALUE 'N'.
           88 88-OPEN-VEHICULOI-SI                 VALUE 'S'.
       77  WS-LEIDOS-VEHICULOI           PIC 9(09) VALUE 0.
       77  WS-LEIDOS-VEHICULOI-ED        PIC ZZZ.ZZZ.ZZ9.

       77  FS-VEHICULOO                  PIC X(02) VALUE ' '.
           88 88-FS-VEHICULOO-OK                   VALUE '00'.
           88 88-FS-VEHICULOO-DUPK                 VALUE '22'.
       77  WS-OPEN-VEHICULOO             PIC X     VALUE 'N'.
           88 88-OPEN-VEHICULOO-SI                 VALUE 'S'.
       77  WS-GRABADOS-VEHICULOO         PIC 9(09) VALUE 0.
       77  WS-GRABADOS-VEHICULOO-ED      PIC ZZZ.ZZZ.ZZ9.

       COPY WAUTOS.

       PROCEDURE DIVISION.
      *------------------

       00000-CUERPO-PRINCIPAL.
      *----------------------

           PERFORM 10000-INICIO

           PERFORM 20000-PROCESO
             UNTIL 88-FS-VEHICULOI-EOF

           PERFORM 30000-FINALIZO

           STOP RUN.

       10000-INICIO.
      *-------------

           PERFORM 10100-ABRO-ARCHIVOS.

           PERFORM 11000-1ERA-LECTURA.


       FIN-10000.
           EXIT.

       10100-ABRO-ARCHIVOS.
      *-------------------

           OPEN INPUT VEHICULOI.


           EVALUATE FS-VEHICULOI
               WHEN '00'
                    SET 88-OPEN-VEHICULOI-SI TO TRUE

               WHEN OTHER
                    DISPLAY 'ERROR OPEN VEHICULOI FS: ' FS-VEHICULOI
                    STOP RUN
           END-EVALUATE.

           OPEN OUTPUT  VEHICULOO.

           EVALUATE FS-VEHICULOO
               WHEN '00'
                    SET 88-OPEN-VEHICULOO-SI TO TRUE

               WHEN OTHER
                    DISPLAY 'ERROR OPEN VEHICULOO FS: ' FS-VEHICULOO
                    STOP RUN

           END-EVALUATE.

       FIN-10100.
           EXIT.

       11000-1ERA-LECTURA.
      *------------------

           PERFORM 11100-READ-VEHICULOI.

           IF 88-FS-VEHICULOI-EOF
              DISPLAY ' '
              DISPLAY 'ARCHIVO VEHICULOI VACIO'
           END-IF.

       FIN-11000.
           EXIT.

       11100-READ-VEHICULOI.
      *-------------------

           INITIALIZE VEH-REGISTRO.

           READ VEHICULOI INTO VEH-REGISTRO.

           EVALUATE TRUE
               WHEN 88-FS-VEHICULOI-OK
                    ADD 1              TO WS-LEIDOS-VEHICULOI

               WHEN 88-FS-VEHICULOI-EOF
                    CONTINUE

               WHEN OTHER
                    DISPLAY 'ERROR EN READ VEHICULOI FS: ' FS-VEHICULOI
                    STOP RUN
           END-EVALUATE.

       FIN-11000.
           EXIT.

       20000-PROCESO.
      *-------------

           PERFORM 21000-GRABO-VEHICULOO.

           PERFORM 11100-READ-VEHICULOI.

       FIN-20000.
           EXIT.

       21000-GRABO-VEHICULOO.
      *-------------------

           PERFORM 21100-WRITE-VEHICULOO.

       FIN-21000.
           EXIT.

       21100-WRITE-VEHICULOO.
      *-------------------

           DISPLAY 'VEH-CLAVE LEIDO: ' VEH-CLAVE.

           MOVE  VEH-CLAVE          TO VEH-CLAVE-FD.
           WRITE REGO-VEH-REGISTRO-FD FROM VEH-REGISTRO.

           EVALUATE FS-VEHICULOO
               WHEN '00'
                    ADD 1           TO WS-GRABADOS-VEHICULOO

               WHEN OTHER
                    DISPLAY 'ERROR WRITE VEHICULOO FS: ' FS-VEHICULOO
                    DISPLAY 'CLAVE VEH-CLAVE : ' VEH-CLAVE

           END-EVALUATE.

       FIN-21100.
           EXIT.

       30000-FINALIZO.
      *--------------

           PERFORM 30100-TOTALES-CONTROL.

           PERFORM 31000-CIERRO-ARCHIVOS.

           STOP RUN.

       FIN-30000.
           EXIT.

       30100-TOTALES-CONTROL.
      *---------------------

           MOVE WS-LEIDOS-VEHICULOI         TO WS-LEIDOS-VEHICULOI-ED.
           MOVE WS-GRABADOS-VEHICULOO       TO WS-GRABADOS-VEHICULOO-ED.

           DISPLAY ' '.
           DISPLAY '****************************************'.
           DISPLAY '    TOTALES DE CONTROL PGM: GENVEHI     '.
           DISPLAY '****************************************'.
           DISPLAY '*                                      *'.
           DISPLAY '* CANT. REGISTROS LEIDOS VEHICULOI  : '
                                               WS-LEIDOS-VEHICULOI-ED.
           DISPLAY '* CANT. REGISTROS GRABADOS VEHICULOO : '
                                               WS-GRABADOS-VEHICULOO-ED.
           DISPLAY '*                                      *'.
           DISPLAY '****************************************'.
           DISPLAY ' '.

       FIN-30100.
           EXIT.

       31000-CIERRO-ARCHIVOS.
      *---------------------

           CLOSE VEHICULOI.

           EVALUATE TRUE
               WHEN 88-FS-VEHICULOI-OK
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR CLOSE VEHICULOI FS: ' FS-VEHICULOI
                    STOP RUN

           END-EVALUATE.

           CLOSE VEHICULOO.

           EVALUATE TRUE
               WHEN 88-FS-VEHICULOO-OK
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR CLOSE VEHICULOO FS: ' FS-VEHICULOO
                    STOP RUN

           END-EVALUATE.

           STOP RUN.

       FIN-31000.
           EXIT.
