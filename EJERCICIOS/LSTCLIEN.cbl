       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID. LSTCLIEN.
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

           SELECT CLIENTES         ASSIGN       TO
           'C:\Users\epalmeyro\Documents\Eduardo\Carpeta Profesional\Cob
      -    'OL\FILES\CLIENTESGNU.DAT'
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE  IS SEQUENTIAL
                                   RECORD KEY   IS REG-CLIENTES-KEY-FD
                                   ALTERNATE KEY   REG-CLIENTES-KEY2-FD
                                                   WITH DUPLICATES
                                   FILE STATUS  IS FS-CLIENTES.



           SELECT LISTADO          ASSIGN       TO
           'C:\Users\epalmeyro\Documents\Eduardo\Carpeta Profesional\Cob
      -    'OL\FILES\LSTCLIEN.DAT'
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS  IS FS-LISTADO.

       DATA DIVISION.
      *-------------

       FILE SECTION.
      *------------

       FD  CLIENTES
           RECORDING MODE IS F
           BLOCK 0.
       01  REG-CLIENTES-FD.
           03 REG-CLIENTES-KEY-FD        PIC X(010).
           03 FILLER                     PIC X(190).
           03 REG-CLIENTES-KEY2-FD       PIC X(050).
           03 FILLER                     PIC X(400).

       FD  LISTADO
           RECORDING MODE IS F
           BLOCK 0.
       01  REG-LISTADO-FD                PIC X(133).



       WORKING-STORAGE SECTION.
      *-----------------------
       77  CTE-PROGRAMA                  PIC X(20) VALUE 'LSTCLIEN'.

       77  FS-LISTADO                    PIC X(02) VALUE ' '.
           88 88-FS-LISTADO-OK                     VALUE '00'.

       77  WS-OPEN-LISTADO               PIC X     VALUE 'N'.
           88 88-OPEN-LISTADO-SI                   VALUE 'S'.
           88 88-OPEN-LISTADO-NO                   VALUE 'N'.

       77  WS-GRABADOS-LISTADO           PIC 9(09) VALUE 0.
       77  WS-GRABADOS-LISTADO-ED        PIC ZZZ.ZZZ.ZZ9.

       77  FS-CLIENTES                   PIC X(02) VALUE ' '.
           88 88-FS-CLIENTES-OK                    VALUE '00'.
           88 88-FS-CLIENTES-EOF                   VALUE '10'.

       77  WS-OPEN-CLIENTES              PIC X     VALUE 'N'.
           88 88-OPEN-CLIENTES-SI                  VALUE 'S'.
           88 88-OPEN-CLIENTES-NO                  VALUE 'N'.

       77  WS-LEIDOS-CLIENTES            PIC 9(09) VALUE 0.
       77  WS-LEIDOS-CLIENTES-ED         PIC ZZZ.ZZZ.ZZ9.
       77  WS-CLI-ID-ED                  PIC  ZZZZZZZZZ9.

       77  WS-HOJA                       PIC 9(05).
       77  WS-HOJA-ED                    PIC ZZ.ZZZ.
       77  WS-LINEA                      PIC 9(02) VALUE 80.
       77  WCN-LINEAS-MAX                PIC 9(02) VALUE 66.


       01  WS-CURRENT-DATE.
           03 WS-CURRENT-DATE-DATE.
              05 WS-CURRENT-DATE-YYYY    PIC 9(04) VALUE 0.
              05 WS-CURRENT-DATE-MM      PIC 9(02) VALUE 0.
              05 WS-CURRENT-DATE-DD      PIC 9(02) VALUE 0.
           03 WS-CURRENT-DATE-TIME.
              05 WS-CURRENT-DATE-HS      PIC 9(02) VALUE 0.
              05 WS-CURRENT-DATE-MS      PIC 9(02) VALUE 0.
              05 WS-CURRENT-DATE-SS      PIC 9(02) VALUE 0.


      *------------------------------------
      * DEFINICION DEL ARCHIVO DE CLIENTES
      *------------------------------------
       COPY WCLIENTE.


      *------------------------------------
      * DEFINICION DE LINEA DE IMPRESION
      *------------------------------------
       COPY WLINEA.


      *---------------------------------------------------
      * DEFINICION DE COPY WORKING RUTINA DE CANCELACION
      *---------------------------------------------------
       COPY WCANCELA.


       PROCEDURE DIVISION.
      *------------------

       00000-CUERPO-PRINCIPAL.
      *-----------------------

           PERFORM 10000-INICIO.

           PERFORM 20000-PROCESO
             UNTIL 88-FS-CLIENTES-EOF.

           PERFORM 30000-FINALIZO.

           STOP RUN.

       10000-INICIO.
      *-------------

           INITIALIZE WCANCELA.
           MOVE CTE-PROGRAMA          TO WCANCELA-PROGRAMA.


           PERFORM 10100-ABRO-ARCHIVOS.

           PERFORM 11000-1ERA-LECTURA.


       FIN-10000.
           EXIT.

       10100-ABRO-ARCHIVOS.
      *-------------------

           MOVE '10100-ABRO-ARCHIVOS'      TO WCANCELA-PARRAFO.

           OPEN OUTPUT LISTADO.

           EVALUATE FS-LISTADO
               WHEN '00'
                    SET 88-OPEN-LISTADO-SI TO TRUE

               WHEN OTHER
                    MOVE '10100-ABRO-ARCHIVOS'  TO WCANCELA-PARRAFO
                    MOVE 'LISTADO'         TO WCANCELA-RECURSO
                    MOVE 'OPEN OUTPUT'     TO WCANCELA-OPERACION
                    MOVE FS-LISTADO        TO WCANCELA-CODRET
                    MOVE 'ERROR EN OPEN'   TO WCANCELA-MENSAJE
                    PERFORM 99999-CANCELO

           END-EVALUATE.

           OPEN INPUT   CLIENTES.

           EVALUATE FS-CLIENTES
               WHEN '00'
                    SET 88-OPEN-CLIENTES-SI TO TRUE

               WHEN OTHER
                    MOVE '10100-ABRO-ARCHIVOS' TO WCANCELA-PARRAFO
                    MOVE 'CLIENTES'        TO WCANCELA-RECURSO
                    MOVE 'OPEN INPUT'      TO WCANCELA-OPERACION
                    MOVE FS-CLIENTES       TO WCANCELA-CODRET
                    MOVE 'ERROR EN OPEN'   TO WCANCELA-MENSAJE
                    PERFORM 99999-CANCELO

           END-EVALUATE.

       FIN-10100.
           EXIT.

       11000-1ERA-LECTURA.
      *------------------

           PERFORM 11100-READ-CLIENTES.

           IF 88-FS-CLIENTES-EOF
              DISPLAY ' '
              DISPLAY '*** ARCHIVO CLIENTES VACIO ***'
           END-IF.

       FIN-11000.
           EXIT.

       11100-READ-CLIENTES.
      *-------------------

           INITIALIZE         REG-CLIENTES.

           READ CLIENTES INTO REG-CLIENTES.

           EVALUATE TRUE
               WHEN 88-FS-CLIENTES-OK
                    ADD 1              TO WS-LEIDOS-CLIENTES

               WHEN 88-FS-CLIENTES-EOF
                    CONTINUE

               WHEN OTHER
                    MOVE '11100-READ-CLIENTES'  TO WCANCELA-PARRAFO
                    MOVE 'CLIENTES'        TO WCANCELA-RECURSO
                    MOVE 'READ'            TO WCANCELA-OPERACION
                    MOVE FS-CLIENTES       TO WCANCELA-CODRET
                    MOVE 'ERROR EN READ'   TO WCANCELA-MENSAJE
                    PERFORM 99999-CANCELO

           END-EVALUATE.

       FIN-11000.
           EXIT.

       20000-PROCESO.
      *-------------

           PERFORM 21100-IMPRIMO-DETALLE.

           PERFORM 11100-READ-CLIENTES.

       FIN-20000.
           EXIT.


       21100-IMPRIMO-DETALLE.
      *---------------------

           ADD 1                    TO WS-LINEA.
           PERFORM 21200-CONTROL-LINEA.

           PERFORM 21300-ARMO-DETALLE.
           PERFORM 21400-WRITE-LISTADO.

       FIN-21100.
           EXIT.


       21200-CONTROL-LINEA.
      *-------------------

           IF WS-LINEA          > WCN-LINEAS-MAX
              PERFORM 21210-IMPRIMO-TITULOS
           END-IF.

       FIN-21200.
           EXIT.

       21210-IMPRIMO-TITULOS.
      *----------------------

           ADD 1                           TO WS-HOJA.

      * TITULO-LINEA-1
      *---------------

           MOVE ' '                        TO WLINEA.
           MOVE '-'                        TO PCC.
           MOVE 'FECHA:'                   TO P1.
           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE.
           MOVE WS-CURRENT-DATE-DD         TO P9.
           MOVE '/'                        TO P11.
           MOVE WS-CURRENT-DATE-MM         TO P12.
           MOVE '/'                        TO P14.
           MOVE WS-CURRENT-DATE-YYYY       TO P15.
           MOVE 'LISTADO DE CLIENTES'      TO P62.
           MOVE 'HOJA:'                    TO P121.
           MOVE WS-HOJA                    TO WS-HOJA-ED.
           MOVE WS-HOJA-ED                 TO P127.
           PERFORM 21400-WRITE-LISTADO.

      * TITULO-LINEA-2
      *---------------

           MOVE ' '                        TO WLINEA.
           MOVE 'HORA:'                    TO P1.
           MOVE WS-CURRENT-DATE-HS         TO P9.
           MOVE ':'                        TO P11.
           MOVE WS-CURRENT-DATE-MS         TO P12.
           MOVE ':'                        TO P14.
           MOVE WS-CURRENT-DATE-SS         TO P15.
           MOVE '-------------------'      TO P62.
           PERFORM 21400-WRITE-LISTADO.

      * TITULO-LINEA-3
      *---------------

           MOVE ' '                        TO WLINEA.
           PERFORM 21400-WRITE-LISTADO.

      * TITULO-LINEA-4
      *---------------

           MOVE ' '                        TO WLINEA.
           MOVE 'ID CLIENTE'               TO P1.
           MOVE 'APELLIDO Y NOMBRES'       TO P23.
           MOVE 'SEXO'                     TO P53.
           MOVE 'F.NACIMIENTO'             TO P58.
           MOVE 'DOMICILIO'                TO P91.
           MOVE 'E'                        TO P132.
           PERFORM 21400-WRITE-LISTADO.

      * TITULO-LINEA-5
      *---------------

           MOVE ' '                        TO WLINEA.
           MOVE '----------'               TO P1.
           MOVE ALL '-'                    TO WLINEA(13:40).
           MOVE '----'                     TO P53.
           MOVE '------------'             TO P58.
           MOVE ALL '-'                    TO WLINEA(72:62).
           MOVE ' -'                       TO P131.
           PERFORM 21400-WRITE-LISTADO.

           MOVE 5                          TO WS-LINEA.

       FIN-21200.
           EXIT.

       21300-ARMO-DETALLE.
      *-------------------

           MOVE ' '                        TO WLINEA.
           MOVE CLI-ID                     TO WS-CLI-ID-ED.
           MOVE WS-CLI-ID-ED               TO P1.
           STRING
                 CLI-LAST-NAME        DELIMITED BY '  '
                 ', '                 DELIMITED BY SIZE
                 CLI-FIRST-NAME       DELIMITED BY '  '
             INTO
                 P12
           END-STRING.

           MOVE CLI-SEX                    TO P55.
           MOVE CLI-BIRTH(9:2)             TO P59.
           MOVE '/'                        TO P61.
           MOVE CLI-BIRTH(6:2)             TO P62.
           MOVE '/'                        TO P64.
           MOVE CLI-BIRTH(1:4)             TO P65.

           STRING
                 CLI-HOME-ADDY           DELIMITED BY '  '
                 ' '                     DELIMITED BY SIZE
                 CLI-HOME-ZIP-CODE       DELIMITED BY '  '
                 ' '                     DELIMITED BY SIZE
                 CLI-HOME-CITY           DELIMITED BY '  '
                 ' '                     DELIMITED BY SIZE
                 CLI-HOME-PROVINCE-NAME  DELIMITED BY '  '
             INTO
                 P71
           END-STRING.
           MOVE CLI-STATUS                 TO P132.

       FIN-21300.
           EXIT.

       21400-WRITE-LISTADO.
      *-------------------

           WRITE REG-LISTADO-FD     FROM WLINEA.

           EVALUATE FS-LISTADO
               WHEN '00'
                    ADD 1             TO WS-GRABADOS-LISTADO

               WHEN OTHER
                    MOVE '21400-WRITE-LISTADO' TO WCANCELA-PARRAFO
                    MOVE 'LISTADO'         TO WCANCELA-RECURSO
                    MOVE 'WRITE'           TO WCANCELA-OPERACION
                    MOVE FS-LISTADO        TO WCANCELA-CODRET
                    MOVE WLINEA            TO WCANCELA-MENSAJE
                    PERFORM 99999-CANCELO

           END-EVALUATE.

       FIN-21400.
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

           MOVE WS-LEIDOS-CLIENTES          TO WS-LEIDOS-CLIENTES-ED.
           MOVE WS-GRABADOS-LISTADO         TO WS-GRABADOS-LISTADO-ED.

           DISPLAY ' '.
           DISPLAY '****************************************'.
           DISPLAY 'TOTALES DE CONTROL PGM: LSTCLIEN        '.
           DISPLAY '****************************************'.
           DISPLAY '*                                      *'.
           DISPLAY '* CANT. REGISTROS LEIDOS CLIENTES   : '
                                               WS-LEIDOS-CLIENTES-ED.
           DISPLAY '* CANT. REGISTROS GRABADOS LISTADO  : '
                                               WS-GRABADOS-LISTADO-ED.
           DISPLAY '*                                      *'.
           DISPLAY '****************************************'.
           DISPLAY ' '.

       FIN-30100.
           EXIT.

       31000-CIERRO-ARCHIVOS.
      *---------------------

           IF 88-OPEN-CLIENTES-SI
              SET 88-OPEN-CLIENTES-NO          TO TRUE
              CLOSE CLIENTES
              EVALUATE TRUE
                  WHEN 88-FS-CLIENTES-OK
                       CONTINUE
                  WHEN OTHER
                       MOVE '31000-CIERRO-ARCHIVOS' TO WCANCELA-PARRAFO
                       MOVE 'CLIENTES'         TO WCANCELA-RECURSO
                       MOVE 'CLOSE'            TO WCANCELA-OPERACION
                       MOVE FS-CLIENTES        TO WCANCELA-CODRET
                       MOVE 'CIERRA CLIENTES'  TO WCANCELA-MENSAJE
                       PERFORM 99999-CANCELO
              END-EVALUATE
           END-IF.

           IF 88-OPEN-LISTADO-SI
              SET 88-OPEN-LISTADO-NO           TO TRUE
              CLOSE LISTADO
              EVALUATE TRUE
                  WHEN 88-FS-LISTADO-OK
                       CONTINUE
                  WHEN OTHER
                       MOVE '31000-CIERRO-ARCHIVOS' TO WCANCELA-PARRAFO
                       MOVE 'LISTADO'          TO WCANCELA-RECURSO
                       MOVE 'CLOSE'            TO WCANCELA-OPERACION
                       MOVE FS-LISTADO         TO WCANCELA-CODRET
                       MOVE 'CIERRA LISTADO'   TO WCANCELA-MENSAJE
                       PERFORM 99999-CANCELO
              END-EVALUATE
           END-IF.

       FIN-31000.
           EXIT.

       99999-CANCELO.

           PERFORM 31000-CIERRO-ARCHIVOS.

           CALL 'CANCELA' USING WCANCELA.

           STOP RUN.

       FIN-99999.
           EXIT.
