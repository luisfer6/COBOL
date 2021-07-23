       IDENTIFICATION DIVISION.

       PROGRAM-ID. GENTARJETAS.
      * AUTHOR LUIS FERNANDEZ GARCIA.
      *
      *-----------------------------------------------------------------
      *Se pide obtener el resumen de las tarjetas de cada cliente, en
      *el mismo se reflejaran los gastos mensual de cada una de sus tar-
      *jetas.
      * Para ello se tienen 2 archivos de entrada:
      * - Gastos mensuales de tarjetas (una tarjeta puede tener de 0 a N
      *gastos) (Id-gasto, num tarjeta, monto, fecha, descrip-
      *ción) Archivo VSAM clave primaria Id-gasto, clave secundaria num
      *tarjeta.
      * - Maestro de tarjetas (un registro por cada tarjeta) (num tarje-
      *ta, tipo (1 visa, 2 MasterCard), límite, num cliente, fec-alta,
      *fec-venc) Archivo secuencial.
      * El archivo de salida deberá contener un registro por cada tarje-
      *ta con el monto gastado en el mes. (Num tarjeta, monto gastado
      *del mes)
      *-----------------------------------------------------------------

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



           SELECT TARJETAS       ASSIGN       TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\MaestroT
      -    'arjetas.txt'
                                 ORGANIZATION IS SEQUENTIAL
                                 FILE STATUS  IS FS-TARJETAS.

           SELECT GASTOS         ASSIGN       TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\GastosTa
      -    'rjetas.dat'
                                 ORGANIZATION IS INDEXED
                                 ACCESS MODE  IS DYNAMIC
                                 RECORD KEY   IS REG-ID-GASTO-KEY-FD
                                 ALTERNATE KEY   REG-NUM-TARJETA-KEY2-FD
                                                   WITH DUPLICATES
                                 FILE STATUS  IS FS-GASTOS.
           SELECT LISTADO        ASSIGN       TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\ListadoT
      -    'arjetas.txt'
                                  ORGANIZATION IS SEQUENTIAL
                                 FILE STATUS  IS FS-LISTADO.

       DATA DIVISION.


       FILE SECTION.

       FD  TARJETAS
           RECORDING MODE IS F
           BLOCK 0.
       01  REG-MAESTRO-TARJETAS-FD       PIC  9(38).

       FD  GASTOS
           RECORDING MODE IS F
           BLOCK 0.
       01  REG-GASTOS-FD.
           03 REG-ID-GASTO-KEY-FD        PIC  9(04).
           03 REG-NUM-TARJETA-KEY2-FD    PIC  X(19).

       FD  LISTADO
           RECORDING MODE IS F
           BLOCK 0.
       01  REG-LISTADO-FD                PIC X(133).

       WORKING-STORAGE SECTION.

       77  FS-TARJETAS                   PIC  X(02).
           88 88-FS-TARJETAS-OK                     VALUE '00'.
           88 88-FS-TARJETAS-EOF                    VALUE '10'.

       77  WS-LEIDOS-TARJETAS            PIC  9(09) VALUE 0.
       77  WS-LEIDOS-TARJETAS-ED         PIC  ZZZ.ZZZ.ZZ9.

       77  WS-TARJETAS-ED                PIC  9(019) VALUE 0.

       77  FS-GASTOS                     PIC  X(02).
           88 88-FS-GASTOS-OK                       VALUE '00'.
           88 88-FS-GASTOS-EOF                      VALUE '10'.
           88 88-FS-GASTOS-INVALIDKEY               VALUE '21'.
           88 88-FS-GASTOS-DUPKEY                   VALUE '22'.
           88 88-FS-GASTOS-NOKEY                    VALUE '23'.

       77  WS-OPEN-GASTOS                PIC  X     VALUE 'N'.
           88 88-OPEN-GASTOS-SI                     VALUE 'S'.
           88 88-OPEN-GASTOS-NO                     VALUE 'N'.

       77  WS-LEIDOS-GASTOS              PIC  9(09) VALUE 0.
       77  WS-LEIDOS-GASTOS-ED           PIC  ZZZ.ZZZ.ZZ9.
       77  WS-GASTOS-ED                  PIC  $9999V99.

       77  FS-LISTADO                    PIC  X(02) VALUE ' '.
           88 88-FS-LISTADO-OK                      VALUE '00'.

       77  WS-OPEN-LISTADO               PIC  X     VALUE 'N'.
           88 88-OPEN-LISTADO-SI                    VALUE 'S'.
           88 88-OPEN-LISTADO-NO                    VALUE 'N'.

       77  WS-GRABADOS-LISTADO           PIC  9(09) VALUE 0.
       77  WS-GRABADOS-LISTADO-ED        PIC  ZZZ.ZZZ.ZZ9.


       01  WS-GASTOS-TARJETAS-PROCESAR.
           03  FILLER                    PIC 9(04) VALUE 4517.
           03  FILLER                    PIC 9(04) VALUE 4105.
           03  FILLER                    PIC 9(04) VALUE 0000.
           03  FILLER                    PIC 9(04) VALUE 777.

       01  FILLER REDEFINES WS-GASTOS-TARJETAS-PROCESAR.
           03 WT-KEY-BUSQUEDA-1          PIC 9(04) OCCURS 4 TIMES
                                         INDEXED BY IDX-GTO.

         01  WS-ACUMULADO.
           03 ACUM                      PIC 9(19) VALUE 0.

       COPY WTARJETAS.

       COPY WGASTOS.

       PROCEDURE DIVISION.
       0000-MAIN-PROCEDURE.

           PERFORM 1000-INICIO.

           PERFORM 2000-PROCESO
                UNTIL 88-FS-TARJETAS-EOF.

           PERFORM 3000-FINALIZO.

           STOP RUN.

       1000-INICIO.
      *-------------
           MOVE 1 TO IDX-GTO
           MOVE 0 TO ACUM
           PERFORM 1010-ABRO-ARCHIVOS
           PERFORM 1015-PRIMERA-LECTURA.

       FIN-1000.
           EXIT.

       1010-ABRO-ARCHIVOS.
      *-------------------


           OPEN INPUT TARJETAS

           EVALUATE TRUE
               WHEN 88-FS-TARJETAS-OK
                    CONTINUE
               WHEN 88-FS-TARJETAS-EOF
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR EN OPEN TARJETAS'
                    DISPLAY 'FILE STATUS' FS-TARJETAS
           END-EVALUATE

           OPEN INPUT GASTOS

           EVALUATE TRUE
               WHEN 88-FS-GASTOS-OK
                    CONTINUE
               WHEN 88-FS-TARJETAS-EOF
                    CONTINUE
               WHEN 88-FS-GASTOS-INVALIDKEY
                    CONTINUE
               WHEN 88-FS-GASTOS-DUPKEY
                    CONTINUE
               WHEN 88-FS-GASTOS-NOKEY
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR EN OPEN GASTOS'
                    DISPLAY 'FILE STATUS' FS-GASTOS
            END-EVALUATE.

           OPEN OUTPUT LISTADO.

           EVALUATE FS-LISTADO
               WHEN '00'
                    SET 88-OPEN-LISTADO-SI      TO TRUE

               WHEN OTHER
                    DISPLAY 'ERROR EN OPEN LISTADO'
                    DISPLAY 'FILE STATUS' FS-LISTADO
           END-EVALUATE.
       FIN-1010.
           EXIT.
       2000-PROCESO.

           PERFORM 1110-START-GASTOS

           PERFORM 1220-START-TARJETAS
           PERFORM 2110-GRABADO-LISTADO.

       FIN-2000.
           EXIT.

       1015-PRIMERA-LECTURA.
      *------------------

           PERFORM 1111-READ-GASTOS.

           IF 88-FS-GASTOS-EOF
              DISPLAY ' '
              DISPLAY 'ARCHIVO GASTOS VACIO'
           END-IF.

           PERFORM 1221-READ-TARJETAS.

           IF 88-FS-TARJETAS-EOF
              DISPLAY ' '
              DISPLAY 'ARCHIVO TARJETAS VACIO'
           END-IF.
       FIN-1015.
           EXIT.

       1110-START-GASTOS.
      *---------------------

           INITIALIZE REG-GASTOS.

           MOVE WT-KEY-BUSQUEDA-1(IDX-GTO)   TO GTO-MONTO
                                                REG-ID-GASTO-KEY-FD.
           DISPLAY ' '.
           DISPLAY 'START'.
           DISPLAY 'IDX-KEY                  :' IDX-GTO.
           DISPLAY 'WT-KEY-BUSQUEDA-1(IDX-KEY) :'
                                             WT-KEY-BUSQUEDA-1(IDX-GTO)

           DISPLAY 'GTO-MONTO-GASTOS         :' GTO-MONTO.
           DISPLAY 'REG-ID-GASTO-KEY-FD      :' REG-ID-GASTO-KEY-FD.

           START GASTOS
             KEY IS NOT < REG-ID-GASTO-KEY-FD
           MOVE GTO-MONTO TO WS-GASTOS-ED.
           DISPLAY 'FS-GASTOS : ' FS-GASTOS.

           EVALUATE TRUE
               WHEN 88-FS-GASTOS-OK
                    ADD 1              TO WS-LEIDOS-GASTOS
                    CONTINUE

               WHEN 88-FS-GASTOS-NOKEY
                    MOVE ALL '*'       TO REG-GASTOS

               WHEN OTHER
                    DISPLAY 'ERROR EN STAR GASTOS FS: ' FS-GASTOS
                    PERFORM 3000-FINALIZO
           END-EVALUATE.
       FIN-1110.
           EXIT.
       1111-READ-GASTOS.
      *------------------------
           INITIALIZE REG-GASTOS

           READ GASTOS   INTO  REG-GASTOS


           EVALUATE TRUE
               WHEN 88-FS-GASTOS-OK
                    CONTINUE
               WHEN 88-FS-GASTOS-EOF
                    CONTINUE
               WHEN 88-FS-GASTOS-INVALIDKEY
                    CONTINUE
               WHEN 88-FS-GASTOS-DUPKEY
                    CONTINUE
               WHEN 88-FS-GASTOS-NOKEY
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR EN READ GASTOS FS: ' FS-TARJETAS
                    PERFORM 3000-FINALIZO
           END-EVALUATE.
       FIN-1111.
           EXIT.

       1220-START-TARJETAS.
      *------------------------

           INITIALIZE REG-MAESTRO-TARJETAS.

           DISPLAY 'TAR-NRO-TARJETA          :'  TAR-NRO-TARJETA


           EVALUATE TRUE
               WHEN 88-FS-TARJETAS-OK
                    ADD 1              TO WS-LEIDOS-TARJETAS
                    CONTINUE

               WHEN 88-FS-TARJETAS-EOF
                    MOVE ALL '*'       TO REG-MAESTRO-TARJETAS

               WHEN OTHER
                    DISPLAY 'ERROR EN STAR TARJETAS FS: ' FS-TARJETAS
                    PERFORM 3000-FINALIZO

           END-EVALUATE.
       FIN-1220.
           EXIT.

       1221-READ-TARJETAS.
      *------------------------
           INITIALIZE REG-MAESTRO-TARJETAS.

           READ TARJETAS    INTO REG-MAESTRO-TARJETAS
           MOVE TAR-NRO-TARJETA TO WS-TARJETAS-ED

           EVALUATE TRUE
               WHEN 88-FS-TARJETAS-OK
                    CONTINUE
               WHEN 88-FS-TARJETAS-EOF
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR EN READ TARJETAS FS: ' FS-TARJETAS
                    PERFORM 3000-FINALIZO
           END-EVALUATE.
       FIN-1221.
           EXIT.


       2110-GRABADO-LISTADO.
      *---------------------

           PERFORM 3300-PRINT-DATA-ENCONTRADO.
           PERFORM 3400-PRINT-DATA-NO-ENCONTRADO.
       FIN-2110.
           EXIT.
       3000-FINALIZO.
      *--------------

           PERFORM 3100-CIERRO-ARCHIVOS.
           STOP RUN.
       FIN-3000.
           EXIT.

       3100-CIERRO-ARCHIVOS.
      *---------------------
              CLOSE GASTOS
              EVALUATE TRUE
                  WHEN 88-FS-GASTOS-OK
                       CONTINUE
                  WHEN OTHER
                       DISPLAY 'ERROR CLOSE URL FS: ' FS-GASTOS
              END-EVALUATE.

              CLOSE TARJETAS
              EVALUATE TRUE
                  WHEN 88-FS-TARJETAS-OK
                       CONTINUE
                  WHEN OTHER
                       DISPLAY 'ERROR CLOSE URL FS: ' FS-TARJETAS
              END-EVALUATE.
       FIN-3100.
           EXIT.
       3300-PRINT-DATA-ENCONTRADO.
           DISPLAY ':::::::::::::::::::::::::::::::::::::::::::::::::::'
                   ':::::::::::'.
           DISPLAY ' TAR-NRO-TARJETA      : ' TAR-NRO-TARJETA.
           DISPLAY ' GTO-MONTO-GASTADO    : ' GTO-MONTO.
       FIN-3300.
           EXIT.

       3400-PRINT-DATA-NO-ENCONTRADO.
           DISPLAY ':::::::::::::::::::::::::::::::::::::::::::::::::::'
                   ':::::::::::'.
           DISPLAY 'NUMERO TARJETA      : ' TAR-NRO-TARJETA.
           DISPLAY ' MONTO GASTADO      : ' GTO-MONTO.
       FIN-3400.
           EXIT.

       END PROGRAM GENTARJETAS.
