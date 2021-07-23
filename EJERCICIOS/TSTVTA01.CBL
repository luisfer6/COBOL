       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID. TSTVTA01.
      *AUTHOR. EDUARDO PALMEYRO.
      *
      *-----------------------------------------------------------
      * ESTE PROGRAMA REALIZA LA CARGA DE LAS VENTAS DEL DIA
      * Y GENERA ESTADISTICA POR VENDEDOR
      *-----------------------------------------------------------
      * POR CADA REGISTRO DE VENTAS SE CARGA UNA TABLA EN
      * MEMORIA CON LOS DATOS DEL VENDEDOR Y ACUMULA EL
      * IMPORTE DE LAS VENTAS DEL DIA
      * TAMBIEN ACUMULA POR SUCURSAL Y POR CODIGO DE CLIENTE
      *
      * POR FIN DE ARCHIVO GENERA REPORTE DE VENTAS
      *
      *-----------------------------------------------------------

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

           SELECT VENTAS           ASSIGN       TO
           'C:\Users\Usuario\Documents\COBOL CDA\COBOL\Ejemplos\VENTAS.D
      -    'AT'
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS  IS FS-VENTAS.

           SELECT CLIENTES         ASSIGN       TO
           'C:\Users\Usuario\Documents\COBOL CDA\COBOL\Ejemplos\Clientes
      -    'GNU.dat'
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE  IS RANDOM
                                   RECORD KEY   IS REG-CLIENTES-KEY-FD
                                   ALTERNATE KEY   REG-CLIENTES-KEY2-FD
                                                   WITH DUPLICATES
                                   FILE STATUS  IS FS-CLIENTES.

           SELECT LISTADO          ASSIGN       TO
           'C:\Users\Usuario\Documents\COBOL CDA\COBOL\Ejemplos\TSTVTA01
      -    '.DAT'
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS  IS FS-LISTADO.


       DATA DIVISION.
      *-------------

       FILE SECTION.
      *------------

       FD  VENTAS
           RECORDING MODE IS F
           BLOCK 0.
       01  REG-VENTAS-FD                 PIC  X(100).


       FD  CLIENTES
           RECORDING MODE IS F
           BLOCK 0.
       01  REG-CLIENTES-FD.
           03 REG-CLIENTES-KEY-FD        PIC  9(010).
           03 FILLER                     PIC  X(190).
           03 REG-CLIENTES-KEY2-FD       PIC  X(050).
           03 FILLER                     PIC  X(400).


       FD  LISTADO
           RECORDING MODE IS F
           BLOCK 0.
       01  REG-LISTADO-FD                PIC  X(199).



       WORKING-STORAGE SECTION.
      *-----------------------
       77  CTE-PROGRAMA                  PIC  X(20) VALUE 'TSTVTA01'.
       77  WS-TITULO-LISTADO             PIC  X(20) VALUE ' '.
       77  WS-IMPORTE-ED                 PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9,99.

       77  FS-VENTAS                     PIC  X(02) VALUE ' '.
           88 88-FS-VENTAS-OK                       VALUE '00'.
           88 88-FS-VENTAS-EOF                      VALUE '10'.

       77  WS-OPEN-VENTAS                PIC  X     VALUE 'N'.
           88 88-OPEN-VENTAS-SI                     VALUE 'S'.
           88 88-OPEN-VENTAS-NO                     VALUE 'N'.

       77  WS-LEIDOS-VENTAS              PIC  9(09) VALUE 0.
       77  WS-LEIDOS-VENTAS-ED           PIC  ZZZ.ZZZ.ZZ9.
       77  WS-VENTAS-ERROR               PIC  X VALUE 'N'.
           88 88-VENTAS-ERROR-SI                VALUE 'S'.
           88 88-VENTAS-ERROR-NO                VALUE 'N'.

       77  FS-LISTADO                    PIC  X(02) VALUE ' '.
           88 88-FS-LISTADO-OK                      VALUE '00'.

       77  WS-OPEN-LISTADO               PIC  X     VALUE 'N'.
           88 88-OPEN-LISTADO-SI                    VALUE 'S'.
           88 88-OPEN-LISTADO-NO                    VALUE 'N'.

       77  WS-GRABADOS-LISTADO           PIC  9(09) VALUE 0.
       77  WS-GRABADOS-LISTADO-ED        PIC  ZZZ.ZZZ.ZZ9.

       77  FS-CLIENTES                   PIC  X(02) VALUE ' '.
           88 88-FS-CLIENTES-OK                     VALUE '00'.
           88 88-FS-CLIENTES-EOF                    VALUE '10'.
           88 88-FS-CLIENTES-INVALIDKEY             VALUE '21'.
           88 88-FS-CLIENTES-DUPKEY                 VALUE '22'.
           88 88-FS-CLIENTES-NOKEY                  VALUE '23'.

       77  WS-OPEN-CLIENTES              PIC  X     VALUE 'N'.
           88 88-OPEN-CLIENTES-SI                   VALUE 'S'.
           88 88-OPEN-CLIENTES-NO                   VALUE 'N'.

       77  WS-LEIDOS-CLIENTES            PIC  9(09) VALUE 0.
       77  WS-LEIDOS-CLIENTES-ED         PIC  ZZZ.ZZZ.ZZ9.
       77  WS-GRABADOS-CLIENTES          PIC  9(09) VALUE 0.
       77  WS-GRABADOS-CLIENTES-ED       PIC  ZZZ.ZZZ.ZZ9.
       77  WS-REGRABADOS-CLIENTES        PIC  9(09) VALUE 0.
       77  WS-REGRABADOS-CLIENTES-ED     PIC  ZZZ.ZZZ.ZZ9.
       77  WS-CLI-ID-ED                  PIC   ZZZZZZZZZ9.

       77  WS-CANT-ALTAS                 PIC  9(09) VALUE 0.
       77  WS-CANT-BAJAS-LOGICAS         PIC  9(09) VALUE 0.
       77  WS-CANT-BAJAS-FISICAS         PIC  9(09) VALUE 0.
       77  WS-CANT-MODIFICACIONES        PIC  9(09) VALUE 0.
       77  WS-CANT-ERRONEOS              PIC  9(09) VALUE 0.
       77  WS-CANT-ED                    PIC  ZZZ.ZZZ.ZZ9.


       77  WS-HOJA                       PIC  9(05).
       77  WS-HOJA-ED                    PIC  ZZ.ZZZ.
       77  WS-LINEA                      PIC  9(02) VALUE 80.
       77  WCN-LINEAS-MAX                PIC  9(02) VALUE 66.
       77  WCN-MAX-KEYS                  PIC  9(02) VALUE 21.
       77  IDX-KEY                       INDEX      VALUE +0.

       77  WS-PARRAFO                    PIC  X(50) VALUE ' '.
       77  WS-MJE-ERROR                  PIC  X(80) VALUE ' '.

       01  WS-CURRENT-DATE.
           03 WS-CURRENT-DATE-DATE.
              05 WS-CURRENT-DATE-YYYY    PIC 9(04) VALUE 0.
              05 WS-CURRENT-DATE-MM      PIC 9(02) VALUE 0.
              05 WS-CURRENT-DATE-DD      PIC 9(02) VALUE 0.
           03 WS-CURRENT-DATE-TIME.
              05 WS-CURRENT-DATE-HS      PIC 9(02) VALUE 0.
              05 WS-CURRENT-DATE-MS      PIC 9(02) VALUE 0.
              05 WS-CURRENT-DATE-SS      PIC 9(02) VALUE 0.

      *-----------------------------------------------
      * DEFINICION DE TABLA DE VENTAS X VENDEDOR
      * LA TABLA TIENE PREVISTAS OCURRENCIAS PARA 100
      * VENDEDORES.
      * LA OCURRENCIA 101 ES PARA EL ULTIMO ELEMENTO HV
      *-----------------------------------------------
       01   WT-VENTASXVENDEDOR.
            03 WT-VXV-TABLA          OCCURS 101 TIMES
                         ASCENDING KEY IS WT-VXV-ID-VENDEDOR
                         INDEXED BY       IDX-VXV.
               05 WT-VXV-ID-VENDEDOR     PIC  9(10).
               05 WT-VXV-IMPORTE         PIC S9(13)V99 COMP-3.

       01   WS-VXV-CONTROL.
            03 WS-VXV-MAX-OCCURS         PIC S9(009) COMP VALUE 100.
            03 WS-VXV-HUBO-CAMBIO        PIC  X(001) VALUE 'S'.
               88 88-VXV-HUBO-CAMBIO-SI              VALUE 'S'.
               88 88-VXV-HUBO-CAMBIO-NO              VALUE 'N'.
            03 WS-VXV-VECTOR-TEMP.
               05 WT-VXV-VT-VENDEDOR     PIC  9(10).
               05 WT-VXV-VT-IMPORTE      PIC S9(13)V99 COMP-3.
            03 WS-VXV-ULTIMO-CARGADO     PIC S9(009) COMP VALUE +0.

      *-----------------------------------------------
      * DEFINICION DE TABLA DE VENTAS X CLIENTE
      * LA TABLA TIENE PREVISTAS OCURRENCIAS PARA 2000
      * CLIENTES.
      * LA OCURRENCIA 2001 ES PARA EL ULTIMO ELEMENTO HV
      *-----------------------------------------------

       01   WT-VENTASXCLIENTE.
            03 WT-VXC-TABLA         OCCURS 2001 TIMES
                         ASCENDING KEY IS WT-VXC-ID-CLIENTE
                         INDEXED BY       IDX-VXC.
               05 WT-VXC-ID-CLIENTE      PIC  9(10).
               05 WT-VXC-IMPORTE         PIC  9(13)V99.

       01   WS-VXC-CONTROL.
            03 WS-VXC-MAX-OCCURS         PIC S9(009) COMP VALUE 2000.
            03 WS-VXC-HUBO-CAMBIO        PIC  X(001) VALUE 'S'.
               88 88-VXC-HUBO-CAMBIO-SI              VALUE 'S'.
               88 88-VXC-HUBO-CAMBIO-NO              VALUE 'N'.
            03 WS-VXC-VECTOR-TEMP.
               05 WT-VXC-VT-CLIENTE      PIC  9(10).
               05 WT-VXC-VT-IMPORTE      PIC S9(13)V99 COMP-3.
            03 WS-VXC-ULTIMO-CARGADO     PIC S9(009) COMP VALUE +0.

      *-----------------------------------------------
      * DEFINICION DE TABLA DE VENTAS X FECHA DE VENTA
      * LA TABLA TIENE PREVISTAS OCURRENCIAS PARA FECHAS
      * POR 2 AÑOS DE VENTAS DIARIAS.
      * LA OCURRENCIA 721 ES PARA EL ULTIMO ELEMENTO HV
      *-----------------------------------------------
       01   WT-VENTASXFECHA.
            03 WT-VXF-TABLA         OCCURS  721 TIMES
                         ASCENDING KEY IS WT-VXF-FECHA
                         INDEXED BY       IDX-VXF.
               05 WT-VXF-FECHA           PIC  X(10).
               05 WT-VXF-IMPORTE         PIC  9(13)V99.

       01   WS-VXF-CONTROL.
            03 WS-VXF-MAX-OCCURS         PIC S9(009) COMP VALUE 720.
            03 WS-VXF-HUBO-CAMBIO        PIC  X(001) VALUE 'S'.
               88 88-VXF-HUBO-CAMBIO-SI              VALUE 'S'.
               88 88-VXF-HUBO-CAMBIO-NO              VALUE 'N'.
            03 WS-VXF-VECTOR-TEMP.
               05 WT-VXF-VT-DATE         PIC  X(10).
               05 WT-VXF-VT-IMPORTE      PIC S9(13)V99 COMP-3.
            03 WS-VXF-ULTIMO-CARGADO     PIC S9(009) COMP VALUE +0.


      *-----------------------------------------------
      * DEFINICION DEL ARCHIVO DE NOVEDADES DE CLIENTE
      *-----------------------------------------------
       COPY WVENTAS.

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

       INCLUDE SQLCA.

       PROCEDURE DIVISION.
      *------------------

       00000-CUERPO-PRINCIPAL.
      *-----------------------

           PERFORM 10000-INICIO.

           PERFORM 20000-PROCESO.

           PERFORM 30000-FINALIZO.

           STOP RUN.

       10000-INICIO.
      *-------------

           INITIALIZE WCANCELA.
           MOVE CTE-PROGRAMA               TO WCANCELA-PROGRAMA.

           PERFORM 10100-ABRO-ARCHIVOS.

           PERFORM 10200-1RA-LECTURA-VENTAS.

       FIN-10000.
           EXIT.

       10100-ABRO-ARCHIVOS.
      *-------------------

           MOVE '10100-ABRO-ARCHIVOS'      TO WS-PARRAFO.

           OPEN INPUT   VENTAS.

           EVALUATE FS-VENTAS
               WHEN '00'
                    SET 88-OPEN-VENTAS-SI  TO TRUE

               WHEN OTHER
                    MOVE WS-PARRAFO        TO WCANCELA-PARRAFO
                    MOVE 'VENTAS  '        TO WCANCELA-RECURSO
                    MOVE 'OPEN INPUT'      TO WCANCELA-OPERACION
                    MOVE FS-VENTAS         TO WCANCELA-CODRET
                    MOVE 'ERROR EN OPEN'   TO WCANCELA-MENSAJE
                    PERFORM 99999-CANCELO

           END-EVALUATE.


           OPEN INPUT CLIENTES.

           EVALUATE FS-CLIENTES
               WHEN '00'
                    SET 88-OPEN-CLIENTES-SI TO TRUE

               WHEN OTHER
                    MOVE WS-PARRAFO        TO WCANCELA-PARRAFO
                    MOVE 'CLIENTES'        TO WCANCELA-RECURSO
                    MOVE 'OPEN INPUT'      TO WCANCELA-OPERACION
                    MOVE FS-CLIENTES       TO WCANCELA-CODRET
                    MOVE 'ERROR EN OPEN'   TO WCANCELA-MENSAJE
                    PERFORM 99999-CANCELO

           END-EVALUATE.


           OPEN OUTPUT LISTADO.

           EVALUATE FS-LISTADO
               WHEN '00'
                    SET 88-OPEN-LISTADO-SI TO TRUE

               WHEN OTHER
                    MOVE WS-PARRAFO        TO WCANCELA-PARRAFO
                    MOVE 'LISTADO'         TO WCANCELA-RECURSO
                    MOVE 'OPEN OUTPUT'     TO WCANCELA-OPERACION
                    MOVE FS-LISTADO        TO WCANCELA-CODRET
                    MOVE 'ERROR EN OPEN'   TO WCANCELA-MENSAJE
                    PERFORM 99999-CANCELO

           END-EVALUATE.

       FIN-10100.
           EXIT.

       10200-1RA-LECTURA-VENTAS.
      *-------------------------

           PERFORM 11000-READ-VENTAS.

           IF 88-FS-VENTAS-EOF
              DISPLAY '************************************'
              DISPLAY '***    ARCHIVO VENTAS VACIO      ***'
              DISPLAY '************************************'
           END-IF.

       FIN-10200.
           EXIT.

       11000-READ-VENTAS.
      *-------------------
                                                                                                                                C
           MOVE '11000-READ-VENTAS  '      TO WS-PARRAFO.

           INITIALIZE REG-VENTAS.

           READ VENTAS   INTO REG-VENTAS.                                                                                       R

           EVALUATE TRUE
               WHEN 88-FS-VENTAS-OK
                    ADD 1                  TO WS-LEIDOS-VENTAS
SACAR               DISPLAY 'VENTAS LEIDO' REG-VENTAS

               WHEN 88-FS-VENTAS-EOF
                    MOVE ALL '*'           TO REG-VENTAS

               WHEN OTHER
                    MOVE WS-PARRAFO        TO WCANCELA-PARRAFO
                    MOVE 'VENTAS'          TO WCANCELA-RECURSO
                    MOVE 'READ'            TO WCANCELA-OPERACION
                    MOVE FS-VENTAS         TO WCANCELA-CODRET
                    MOVE 'ERROR EN READ'   TO WCANCELA-MENSAJE
                    PERFORM 99999-CANCELO

           END-EVALUATE.
                                                                                                                                C

       FIN-11000.
           EXIT.

       11100-READ-CLIENTES.
      *-------------------
                                                                                                                                C
           MOVE '11100-READ-CLIENTES'      TO WS-PARRAFO.

           INITIALIZE REG-CLIENTES
                      REG-CLIENTES-FD.

           MOVE ID-CLIENTE                 TO REG-CLIENTES-KEY-FD.

           DISPLAY ' '.
           DISPLAY 'READ CLIENTES VENTAS-ID  :' CLI-ID IN REG-CLIENTES.
           DISPLAY 'REG-CLIENTES-KEY-FD      :' REG-CLIENTES-KEY-FD.

           READ CLIENTES INTO REG-CLIENTES


           EVALUATE TRUE
               WHEN 88-FS-CLIENTES-OK
                    ADD 1                  TO WS-LEIDOS-CLIENTES

               WHEN 88-FS-CLIENTES-NOKEY
                    MOVE ALL '*'           TO REG-CLIENTES

               WHEN OTHER
                    MOVE WS-PARRAFO        TO WCANCELA-PARRAFO
                    MOVE 'CLIENTES'        TO WCANCELA-RECURSO
                    MOVE 'READ'            TO WCANCELA-OPERACION
                    MOVE FS-CLIENTES       TO WCANCELA-CODRET
                    MOVE 'ERROR EN READ'   TO WCANCELA-MENSAJE
                    PERFORM 99999-CANCELO

           END-EVALUATE.

       FIN-11100.
           EXIT.

       20000-PROCESO.
      *-------------

           MOVE HIGH-VALUES           TO WT-VENTASXVENDEDOR
                                         WT-VENTASXCLIENTE
                                         WT-VENTASXFECHA.

SACAR      DISPLAY 'PROCESO..........'.
           PERFORM 20100-CARGO-TABLAS
             UNTIL 88-FS-VENTAS-EOF.

           PERFORM 21000-IMPRIMO-VXV.

           PERFORM 21010-IMPRIMO-VXC.

           PERFORM 21020-IMPRIMO-VXF.

       FIN-20000.
           EXIT.


       20100-CARGO-TABLAS.
      *---------------------

           PERFORM 20200-CARGO-TABLA-VXV.

           PERFORM 20300-CARGO-TABLA-VXC.

           PERFORM 20400-CARGO-TABLA-VXF.

           PERFORM 11000-READ-VENTAS.

       FIN-20100.
           EXIT.


       20200-CARGO-TABLA-VXV.
      *------------------------
SACAR      DISPLAY '20200-CARGO-TABLA-VXV'.
      * ***************************************************************
      * BUSCO EN TABLA DE VENTAS POR VENDEDOR SI EL VENDEDOR YA EXISTE
      * LA TABLA DEBE ESTAR ORDENADA POR VENDEDOR.
      * SI EL VENDEDOR NO EXISTE YA EN LA TABLA SE LO CARGA Y LUEGO SE
      * RE-ORDENA LA TABLA PARA LA PROXIMA BÚSQUEDA BINARIA.
      * ***************************************************************
           SEARCH ALL WT-VXV-TABLA
               AT END
                      PERFORM 20210-INCORPORO-VENDEDOR
                 WHEN
                      WT-VXV-ID-VENDEDOR(IDX-VXV) = ID-VENDEDOR
                      ADD IMP-TOTAL    TO WT-VXV-IMPORTE (IDX-VXV)
                      DISPLAY 'ENCONTRO VENDEDOR  TABLA ' ID-VENDEDOR

           END-SEARCH.

       FIN-20200.
           EXIT.



       20210-INCORPORO-VENDEDOR.
      *-------------------------

      * ***************************************************************
      * LA BUSQUEDA BINARIA EN LA TABLA ORDENADA NO FUE EXITOSA
      * SE CONTROLA QUE LA TABLA TENGA ELEMENTOS LIBRES PARA AGREGAR
      * UN NUEVO VENDEDOR Y EL IMPORTE INICIAL Y LUEGO ORDENAR LA
      * TABLA PARA DEJARLA LISTA PARA EL NUEVO REGISTRO A LEER
      * ***************************************************************

           MOVE '20210-INCORPORO-VENDEDOR'       TO WS-PARRAFO.


           ADD 1                          TO WS-VXV-ULTIMO-CARGADO.

           DISPLAY 'INCOPORA VENDEDOR : ' ID-VENDEDOR.
           DISPLAY 'EN POSICION       : ' WS-VXV-ULTIMO-CARGADO.

           IF WS-VXV-ULTIMO-CARGADO > WS-VXV-MAX-OCCURS
              MOVE WS-PARRAFO             TO WCANCELA-PARRAFO
              MOVE 'WT-VXV-TABLA '        TO WCANCELA-RECURSO
              MOVE 'ALTA VENDEDOR'        TO WCANCELA-OPERACION
              MOVE WS-VXV-ULTIMO-CARGADO  TO WCANCELA-CODRET
              MOVE 'TABLA EXCEDIDA - REVISAR DIMENSION'
                                          TO WCANCELA-MENSAJE
              PERFORM 99999-CANCELO
           END-IF.

           MOVE ID-VENDEDOR                TO WT-VXV-ID-VENDEDOR
                                             (WS-VXV-ULTIMO-CARGADO).

           MOVE IMP-TOTAL                  TO WT-VXV-IMPORTE
                                             (WS-VXV-ULTIMO-CARGADO).

      * ***************************************************************
      * SE RE-ORDENA POR EL METODO DEL BURBUJEO LA TABLA PARA DEJARLA
      * ORDENADA PARA LA PROXIMA BUSQUEDA.
      * ***************************************************************

           SET 88-VXV-HUBO-CAMBIO-SI                TO TRUE.

           PERFORM 20220-ORDENO-TABLA-VXV
             UNTIL 88-VXV-HUBO-CAMBIO-NO.


       FIN-20210.
           EXIT.


       20220-ORDENO-TABLA-VXV.
      *-----------------------

           SET 88-VXV-HUBO-CAMBIO-NO                TO TRUE.

           PERFORM VARYING IDX-VXV FROM 1 BY 1
             UNTIL IDX-VXV > WS-VXV-ULTIMO-CARGADO

             IF WT-VXV-ID-VENDEDOR (IDX-VXV)
              > WT-VXV-ID-VENDEDOR (IDX-VXV + 1)
                MOVE WT-VXV-ID-VENDEDOR (IDX-VXV + 1)
                  TO WS-VXV-VECTOR-TEMP
                MOVE WT-VXV-ID-VENDEDOR (IDX-VXV)
                  TO WT-VXV-ID-VENDEDOR (IDX-VXV + 1)
                MOVE WS-VXV-VECTOR-TEMP
                  TO WT-VXV-ID-VENDEDOR (IDX-VXV)
                SET 88-VXV-HUBO-CAMBIO-SI         TO TRUE
             END-IF

           END-PERFORM.

       FIN-20220.
           EXIT.


       20300-CARGO-TABLA-VXC.
      *------------------------

      * ***************************************************************
      * BUSCO EN TABLA DE VENTAS POR CLIENTE  SI EL CLIENTE  YA EXISTE
      * LA TABLA DEBE ESTAR ORDENADA POR CLIENTE.
      * SI EL CLIENTE  NO EXISTE YA EN LA TABLA SE LO CARGA Y LUEGO SE
      * RE-ORDENA LA TABLA PARA LA PROXIMA BÚSQUEDA BINARIA.
      * ***************************************************************
           SEARCH ALL WT-VXC-TABLA
               AT END
                      PERFORM 20310-INCORPORO-CLIENTE
                 WHEN
                      WT-VXC-ID-CLIENTE (IDX-VXC) = ID-CLIENTE
                      ADD IMP-TOTAL    TO WT-VXC-IMPORTE (IDX-VXC)
                      DISPLAY 'ENCONTRO CLIENTE   TABLA ' ID-CLIENTE

           END-SEARCH.

       FIN-20300.
           EXIT.


       20310-INCORPORO-CLIENTE.
      *-------------------------

      * ***************************************************************
      * SI LA BUSQUEDA BINARIA EN LA TABLA ORDENADA NO FUE EXITOSA
      * SE CONTROLA QUE LA TABLA TENGA ELEMENTOS LIBRES PARA AGREGAR
      * UN NUEVO CLIENTE  Y EL IMPORTE INICIAL Y LUEGO ORDENAR LA
      * TABLA PARA DEJARLA LISTA PARA EL NUEVO REGISTRO A LEER
      * ***************************************************************

           MOVE '20310-INCORPORO-CLIENTE '       TO WS-PARRAFO.

           ADD 1                          TO WS-VXC-ULTIMO-CARGADO.

           DISPLAY 'INCOPORA CLIENTE  : ' ID-CLIENTE.
           DISPLAY 'EN POSICION       : ' WS-VXC-ULTIMO-CARGADO.

           IF WS-VXC-ULTIMO-CARGADO > WS-VXC-MAX-OCCURS
              MOVE WS-PARRAFO             TO WCANCELA-PARRAFO
              MOVE 'WT-VXC-TABLA '        TO WCANCELA-RECURSO
              MOVE 'ALTA CLIENTE '        TO WCANCELA-OPERACION
              MOVE WS-VXC-ULTIMO-CARGADO  TO WCANCELA-CODRET
              MOVE 'TABLA EXCEDIDA - REVISAR DIMENSION'
                                          TO WCANCELA-MENSAJE
              PERFORM 99999-CANCELO
           END-IF.

           MOVE ID-CLIENTE                 TO WT-VXC-ID-CLIENTE
                                             (WS-VXC-ULTIMO-CARGADO).

           MOVE IMP-TOTAL                  TO WT-VXC-IMPORTE
                                             (WS-VXC-ULTIMO-CARGADO).

      * ***************************************************************
      * SE RE-ORDENA POR EL METODO DEL BURBUJEO LA TABLA PARA DEJARLA
      * ORDENADA PARA LA PROXIMA BUSQUEDA.
      * ***************************************************************

           SET 88-VXC-HUBO-CAMBIO-SI                TO TRUE.

           PERFORM 20320-ORDENO-TABLA-VXC
             UNTIL 88-VXC-HUBO-CAMBIO-NO.


       FIN-20310.
           EXIT.


       20320-ORDENO-TABLA-VXC.
      *-----------------------

           SET 88-VXC-HUBO-CAMBIO-NO                TO TRUE.

           PERFORM VARYING IDX-VXC FROM 1 BY 1
             UNTIL IDX-VXC > WS-VXC-ULTIMO-CARGADO

             IF WT-VXC-ID-CLIENTE  (IDX-VXC)
              > WT-VXC-ID-CLIENTE  (IDX-VXC + 1)
                MOVE WT-VXC-ID-CLIENTE  (IDX-VXC + 1)
                  TO WS-VXC-VECTOR-TEMP
                MOVE WT-VXC-ID-CLIENTE  (IDX-VXC)
                  TO WT-VXC-ID-CLIENTE  (IDX-VXC + 1)
                MOVE WS-VXC-VECTOR-TEMP
                  TO WT-VXC-ID-CLIENTE  (IDX-VXC)
                SET 88-VXC-HUBO-CAMBIO-SI         TO TRUE
             END-IF

           END-PERFORM.

       FIN-20320.
           EXIT.


       20400-CARGO-TABLA-VXF.
      *------------------------

      * ***************************************************************
      * BUSCO EN TABLA DE VENTAS POR FECHA SI LA FECHA YA EXISTE
      * LA TABLA DEBE ESTAR ORDENADA POR FECHA.
      * SI LA FECHA    NO EXISTE YA EN LA TABLA SE LA CARGA Y LUEGO SE
      * RE-ORDENA LA TABLA PARA LA PROXIMA BÚSQUEDA BINARIA.
      * ***************************************************************
           SEARCH ALL WT-VXF-TABLA
               AT END
                      PERFORM 20410-INCORPORO-FECHA
                 WHEN
                      WT-VXF-FECHA      (IDX-VXF) = FECHA-VENTA
                      ADD IMP-TOTAL    TO WT-VXF-IMPORTE(IDX-VXF)
                      DISPLAY 'ENCONTRO FECHA     TABLA ' FECHA-VENTA

           END-SEARCH.

       FIN-20400.
           EXIT.


       20410-INCORPORO-FECHA.
      *-------------------------

      * ***************************************************************
      * SI LA BUSQUEDA BINARIA EN LA TABLA ORDENADA NO FUE EXITOSA
      * SE CONTROLA QUE LA TABLA TENGA ELEMENTOS LIBRES PARA AGREGAR
      * UNA NUEVA FECHA   Y EL IMPORTE INICIAL Y LUEGO ORDENAR LA
      * TABLA PARA DEJARLA LISTA PARA EL NUEVO REGISTRO A LEER
      * ***************************************************************

           MOVE '20410-INCORPORO-FECHA   '       TO WS-PARRAFO.

           ADD 1                          TO WS-VXF-ULTIMO-CARGADO.

           DISPLAY 'INCOPORA FECHA    : ' FECHA-VENTA.
           DISPLAY 'EN POSICION       : ' WS-VXF-ULTIMO-CARGADO.

           IF WS-VXF-ULTIMO-CARGADO > WS-VXF-MAX-OCCURS
              MOVE WS-PARRAFO             TO WCANCELA-PARRAFO
              MOVE 'WT-VXF-TABLA '        TO WCANCELA-RECURSO
              MOVE 'ALTA FECHA   '        TO WCANCELA-OPERACION
              MOVE WS-VXF-ULTIMO-CARGADO  TO WCANCELA-CODRET
              MOVE 'TABLA EXCEDIDA - REVISAR DIMENSION'
                                          TO WCANCELA-MENSAJE
              PERFORM 99999-CANCELO
           END-IF.

           MOVE FECHA-VENTA                TO WT-VXF-FECHA
                                             (WS-VXF-ULTIMO-CARGADO).

           MOVE IMP-TOTAL                  TO WT-VXF-IMPORTE
                                             (WS-VXF-ULTIMO-CARGADO).

      * ***************************************************************
      * SE RE-ORDENA POR EL METODO DEL BURBUJEO LA TABLA PARA DEJARLA
      * ORDENADA PARA LA PROXIMA BUSQUEDA.
      * ***************************************************************

           SET 88-VXF-HUBO-CAMBIO-SI                TO TRUE.

           PERFORM 20420-ORDENO-TABLA-VXF
             UNTIL 88-VXF-HUBO-CAMBIO-NO.


       FIN-20410.
           EXIT.


       20420-ORDENO-TABLA-VXF.
      *-----------------------

           SET 88-VXF-HUBO-CAMBIO-NO                TO TRUE.

           PERFORM VARYING IDX-VXF FROM 1 BY 1
             UNTIL IDX-VXF > WS-VXF-ULTIMO-CARGADO

             IF WT-VXF-FECHA       (IDX-VXF)
              > WT-VXF-FECHA       (IDX-VXF + 1)
                MOVE WT-VXF-FECHA       (IDX-VXF + 1)
                  TO WS-VXF-VECTOR-TEMP
                MOVE WT-VXF-FECHA       (IDX-VXF)
                  TO WT-VXF-FECHA       (IDX-VXF + 1)
                MOVE WS-VXF-VECTOR-TEMP
                  TO WT-VXF-FECHA       (IDX-VXF)
                SET 88-VXF-HUBO-CAMBIO-SI         TO TRUE
             END-IF

           END-PERFORM.

       FIN-20420.
           EXIT.


       21000-IMPRIMO-VXV.
      *---------------------

           MOVE 'VENDEDOR'          TO WS-TITULO-LISTADO.
           MOVE 90                  TO WS-LINEA.

           PERFORM VARYING IDX-VXV FROM 1 BY 1
                     UNTIL IDX-VXV > WS-VXV-ULTIMO-CARGADO

                ADD 1               TO WS-LINEA
                PERFORM 21200-CONTROL-LINEA
                PERFORM 21300-ARMO-DETALLE-VXV
                PERFORM 21400-WRITE-LISTADO

           END-PERFORM.

       FIN-21000.
           EXIT.


       21010-IMPRIMO-VXC.
      *---------------------

           MOVE 'CLIENTE '          TO WS-TITULO-LISTADO.
           MOVE 90                  TO WS-LINEA.

           PERFORM VARYING IDX-VXC FROM 1 BY 1
                     UNTIL IDX-VXC > WS-VXC-ULTIMO-CARGADO

                ADD 1               TO WS-LINEA
                PERFORM 21200-CONTROL-LINEA
                PERFORM 21310-ARMO-DETALLE-VXC
                PERFORM 21400-WRITE-LISTADO

           END-PERFORM.

       FIN-21010.
           EXIT.


       21020-IMPRIMO-VXF.
      *---------------------

           MOVE 'FECHA VENTA'       TO WS-TITULO-LISTADO.
           MOVE 90                  TO WS-LINEA.

           PERFORM VARYING IDX-VXF FROM 1 BY 1
                     UNTIL IDX-VXF > WS-VXF-ULTIMO-CARGADO

                ADD 1               TO WS-LINEA
                PERFORM 21200-CONTROL-LINEA
                PERFORM 21320-ARMO-DETALLE-VXF
                PERFORM 21400-WRITE-LISTADO

           END-PERFORM.

       FIN-21020.
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
           MOVE 'LISTADO DE VENTAS POR:'   TO P56.
           MOVE WS-TITULO-LISTADO          TO P79.
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
           MOVE ALL '-'                    TO WLINEA(57:43).
           PERFORM 21400-WRITE-LISTADO.

      * TITULO-LINEA-3
      *---------------

           MOVE ' '                        TO WLINEA.
           PERFORM 21400-WRITE-LISTADO.

      * TITULO-LINEA-4
      *---------------

           MOVE ' '                        TO WLINEA.
           MOVE 'CLAVE'                    TO P1.
           MOVE 'IMPORTE'                  TO P12.
           PERFORM 21400-WRITE-LISTADO.

      * TITULO-LINEA-5
      *---------------

           MOVE ' '                        TO WLINEA.
           MOVE '----------'               TO P1.
           MOVE '--------------------'     TO P12.
           PERFORM 21400-WRITE-LISTADO.

           MOVE 5                          TO WS-LINEA.

       FIN-21200.
           EXIT.

       21300-ARMO-DETALLE-VXV.
      *-----------------------

           MOVE ' '                          TO WLINEA.

           MOVE WT-VXV-ID-VENDEDOR (IDX-VXV) TO P1.
           MOVE WT-VXV-IMPORTE     (IDX-VXV) TO WS-IMPORTE-ED.
           MOVE WS-IMPORTE-ED                TO P12.

       FIN-21300.
           EXIT.

       21310-ARMO-DETALLE-VXC.
      *-----------------------

           MOVE ' '                          TO WLINEA.

           MOVE WT-VXC-ID-CLIENTE  (IDX-VXC) TO P1.
           MOVE WT-VXC-IMPORTE     (IDX-VXC) TO WS-IMPORTE-ED.
           MOVE WS-IMPORTE-ED                TO P12.

       FIN-21310.
           EXIT.

       21320-ARMO-DETALLE-VXF.
      *-----------------------

           MOVE ' '                          TO WLINEA.

           MOVE WT-VXF-FECHA       (IDX-VXF) TO P1.
           MOVE WT-VXF-IMPORTE     (IDX-VXF) TO WS-IMPORTE-ED.
           MOVE WS-IMPORTE-ED                TO P12.

       FIN-21310.
           EXIT.

       21400-WRITE-LISTADO.
      *-------------------

           MOVE '21400-WRITE-LISTADO' TO WS-PARRAFO.

           WRITE REG-LISTADO-FD     FROM WLINEA.

           EVALUATE FS-LISTADO
               WHEN '00'
                    ADD 1             TO WS-GRABADOS-LISTADO

               WHEN OTHER
                    MOVE WS-PARRAFO        TO WCANCELA-PARRAFO
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

           MOVE WS-LEIDOS-VENTAS          TO WS-LEIDOS-VENTAS-ED.

           DISPLAY ' '.
           DISPLAY '****************************************'.
           DISPLAY 'TOTALES DE CONTROL PGM: TSTVTA01        '.
           DISPLAY '****************************************'.
           DISPLAY '*                                      *'.
           DISPLAY '* CANT. REG. LEIDOS VENTAS          : '
                                               WS-LEIDOS-VENTAS-ED.

           DISPLAY '*                                      *'.
           DISPLAY '****************************************'.
           DISPLAY ' '.

       FIN-30100.
           EXIT.

       31000-CIERRO-ARCHIVOS.
      *---------------------

           MOVE '31000-CIERRO-ARCHIVOS'        TO WS-PARRAFO.

           IF 88-OPEN-VENTAS-SI
              SET 88-OPEN-VENTAS-NO            TO TRUE
              CLOSE VENTAS
              EVALUATE TRUE
                  WHEN 88-FS-VENTAS-OK
                       CONTINUE
                  WHEN OTHER
                       MOVE WS-PARRAFO         TO WCANCELA-PARRAFO
                       MOVE 'VENTAS  '         TO WCANCELA-RECURSO
                       MOVE 'CLOSE'            TO WCANCELA-OPERACION
                       MOVE FS-VENTAS          TO WCANCELA-CODRET
                       MOVE 'CIERRA VENTAS  '  TO WCANCELA-MENSAJE
                       PERFORM 99999-CANCELO
              END-EVALUATE
           END-IF.

           IF 88-OPEN-CLIENTES-SI
              SET 88-OPEN-CLIENTES-NO          TO TRUE
              CLOSE CLIENTES
              EVALUATE TRUE
                  WHEN 88-FS-CLIENTES-OK
                       CONTINUE
                  WHEN OTHER
                       MOVE WS-PARRAFO         TO WCANCELA-PARRAFO
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
                       MOVE WS-PARRAFO         TO WCANCELA-PARRAFO
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
