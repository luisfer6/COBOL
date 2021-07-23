       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID. GenVsam.
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

           SELECT CUSTOMER         ASSIGN       TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\Customer
      -    '.dat'
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS  IS FS-CUSTOMER.

           SELECT CLIENTES         ASSIGN       TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\Clientes
      -    'GNU.dat'
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE  IS SEQUENTIAL
                                   RECORD KEY   IS REG-CLIENTES-KEY-FD
                                   ALTERNATE KEY   REG-CLIENTES-KEY2-FD
                                                   WITH DUPLICATES
                                   FILE STATUS  IS FS-CLIENTES.


       DATA DIVISION.
      *-------------

       FILE SECTION.
      *------------

       FD  CUSTOMER
           RECORDING MODE IS F
           BLOCK 0.
       01  REG-CUSTOMER-FD               PIC X(650).

       FD  CLIENTES
           RECORDING MODE IS F
           BLOCK 0.
       01  REG-CLIENTES-FD.
           03 REG-CLIENTES-KEY-FD        PIC X(010).
           03 FILLER                     PIC X(190).
           03 REG-CLIENTES-KEY2-FD       PIC X(050).
           03 FILLER                     PIC X(400).



       WORKING-STORAGE SECTION.
      *-----------------------
       77  FS-CUSTOMER                   PIC X(02) VALUE ' '.
           88 88-FS-CUSTOMER-OK                    VALUE '00' '97'.
           88 88-FS-CUSTOMER-EOF                   VALUE '10'.
       77  WS-OPEN-CUSTOMER              PIC X     VALUE 'N'.
           88 88-OPEN-CUSTOMER-SI                  VALUE 'S'.
       77  WS-LEIDOS-CUSTOMER            PIC 9(09) VALUE 0.
       77  WS-LEIDOS-CUSTOMER-ED         PIC ZZZ.ZZZ.ZZ9.

       77  FS-CLIENTES                   PIC X(02) VALUE ' '.
           88 88-FS-CLIENTES-OK                    VALUE '00'.
           88 88-FS-CLIENTES-DUPK                  VALUE '22'.
       77  WS-OPEN-CLIENTES              PIC X     VALUE 'N'.
           88 88-OPEN-CLIENTES-SI                  VALUE 'S'.
       77  WS-GRABADOS-CLIENTES          PIC 9(09) VALUE 0.
       77  WS-GRABADOS-CLIENTES-ED       PIC ZZZ.ZZZ.ZZ9.
       77  WS-A                          PIC 9     VALUE 5.

       COPY WCLIENTE.

       PROCEDURE DIVISION.
      *------------------

       00000-CUERPO-PRINCIPAL.
      *----------------------

           PERFORM 10000-INICIO

           PERFORM 20000-PROCESO
             UNTIL 88-FS-CUSTOMER-EOF

           PERFORM 30000-FINALIZO

           STOP RUN.

       10000-INICIO.
      *-------------

      *>      add 1 to tally.

      *>      COMPUTE TALLY = TALLY + 1.

      *>      DISPLAY 'TALLY :' TALLY.

      *>      DISPLAY 'WS-A: ' WS-A.
      *>      IF 1 < WS-A
      *>         DISPLAY '1 ES MENOR QUE WS-A'
      *>      ELSE
      *>         DISPLAY '1 ES MAYOR O IGUAL A WS-A'
      *>      END-IF.


           PERFORM 10100-ABRO-ARCHIVOS.

           PERFORM 11000-1ERA-LECTURA.


       FIN-10000.
           EXIT.

       10100-ABRO-ARCHIVOS.
      *-------------------

           OPEN INPUT CUSTOMER.


           EVALUATE FS-CUSTOMER
               WHEN '00'
                    SET 88-OPEN-CUSTOMER-SI TO TRUE

               WHEN OTHER
                    DISPLAY 'ERROR OPEN CUSTOMER FS: ' FS-CUSTOMER
                    STOP RUN
           END-EVALUATE.

           OPEN OUTPUT  CLIENTES.

           EVALUATE FS-CLIENTES
               WHEN '00'
                    SET 88-OPEN-CLIENTES-SI TO TRUE

               WHEN OTHER
                    DISPLAY 'ERROR OPEN CLIENTES FS: ' FS-CLIENTES
                    STOP RUN

           END-EVALUATE.

       FIN-10100.
           EXIT.

       11000-1ERA-LECTURA.
      *------------------

           PERFORM 11100-READ-CUSTOMER.

           IF 88-FS-CUSTOMER-EOF
              DISPLAY ' '
              DISPLAY 'ARCHIVO CUSTOMER VACIO'
           END-IF.

       FIN-11000.
           EXIT.

       11100-READ-CUSTOMER.
      *-------------------

           INITIALIZE         REG-CLIENTES.

           READ CUSTOMER INTO REG-CLIENTES.

           EVALUATE TRUE
               WHEN 88-FS-CUSTOMER-OK
                    ADD 1              TO WS-LEIDOS-CUSTOMER

               WHEN 88-FS-CUSTOMER-EOF
                    CONTINUE

               WHEN OTHER
                    DISPLAY 'ERROR EN READ CUSTOMER FS: ' FS-CUSTOMER
                    STOP RUN
           END-EVALUATE.

       FIN-11000.
           EXIT.

       20000-PROCESO.
      *-------------

           PERFORM 21000-GRABO-CLIENTES.

           PERFORM 11100-READ-CUSTOMER.

       FIN-20000.
           EXIT.

       21000-GRABO-CLIENTES.
      *-------------------

           PERFORM 21100-WRITE-CLIENTES.

       FIN-21000.
           EXIT.

       21100-WRITE-CLIENTES.
      *-------------------

      *    DISPLAY 'CLI-ID LEIDO :'CLI-ID 'REG: ' WS-LEIDOS-CUSTOMER.

           MOVE  CLI-CLAVE          TO REG-CLIENTES-KEY-FD.
           WRITE REG-CLIENTES-FD  FROM REG-CLIENTES.

           EVALUATE FS-CLIENTES
               WHEN '00'
                    ADD 1           TO WS-GRABADOS-CLIENTES

               WHEN OTHER
                    DISPLAY 'ERROR WRITE CLIENTES FS: ' FS-CLIENTES
                    DISPLAY 'CLAVE CLI-CLAVE : ' CLI-CLAVE

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

           MOVE WS-LEIDOS-CUSTOMER          TO WS-LEIDOS-CUSTOMER-ED.
           MOVE WS-GRABADOS-CLIENTES        TO WS-GRABADOS-CLIENTES-ED.

           DISPLAY ' '.
           DISPLAY '****************************************'.
           DISPLAY '    TOTALES DE CONTROL PGM: GENVSAM     '.
           DISPLAY '****************************************'.
           DISPLAY '*                                      *'.
           DISPLAY '* CANT. REGISTROS LEIDOS CUSTOMER   : '
                                               WS-LEIDOS-CUSTOMER-ED.
           DISPLAY '* CANT. REGISTROS GRABADOS CLIENTES : '
                                               WS-GRABADOS-CLIENTES-ED.
           DISPLAY '*                                      *'.
           DISPLAY '****************************************'.
           DISPLAY ' '.

       FIN-30100.
           EXIT.

       31000-CIERRO-ARCHIVOS.
      *---------------------

           CLOSE CUSTOMER.

           EVALUATE TRUE
               WHEN 88-FS-CUSTOMER-OK
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR CLOSE CUSTOMER FS: ' FS-CUSTOMER
                    STOP RUN

           END-EVALUATE.

           CLOSE CLIENTES.

           EVALUATE TRUE
               WHEN 88-FS-CLIENTES-OK
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR CLOSE CLIENTES FS: ' FS-CLIENTES
                    STOP RUN

           END-EVALUATE.

           STOP RUN.

       FIN-31000.
           EXIT.
