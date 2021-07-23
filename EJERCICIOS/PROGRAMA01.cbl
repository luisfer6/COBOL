       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID. PROGRAMA01.

      *----------------------------------------------------------------*
      *OBJETIVO: VISUALIZAR DEFINICIONES DE CAMPOS DE WORKING
      *
      *----------------------------------------------------------------*

       ENVIRONMENT DIVISION.
      *--------------------

       CONFIGURATION SECTION.
      *--------------------
       SOURCE-COMPUTER. Z15.
       OBJECT-COMPUTER. Z15.
       SPECIAL-NAMES.
          DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
      *--------------------
       FILE-CONTROL.
           SELECT ARCHIVO-P           ASSIGN TO
              "C:\Users\epalmeyro\Documents\Eduardo\Carpeta Profesional\
      -"Cobol\Files\ARCHIVO-J.TXT"
                                LINE SEQUENTIAL
                                FILE STATUS IS FS-ARCHIVO-P.

       DATA DIVISION.
      *--------------------

       FILE SECTION.
      *--------------------
       FD  ARCHIVO-P
           RECORDING MODE IS F
           BLOCK CONTAINS 0 CHARACTERS.

       01  REG-ARCHIVO-P-FD                PIC X(400).

       WORKING-STORAGE SECTION.
      *-----------------------

       77  VTA-IMPORTE-VTA                 PIC 9(15)V99.
       77  IND-ANIO                                     INDEX.
       77  IND-MES                                      INDEX.


       01  WS-ACUMULADORES.
           05 WS-TOT-VENTAS                PIC S9(15)V99 VALUE +0.
           05 WS-TOT-COMPRAS               PIC S9(15)V99 VALUE +0.



       01  WS-CONTADORES.
           05  WS-CANT-REG-LEIDOS-CLI      PIC 9(9) VALUE 0.
           05  WS-CANT-REG-GRABADOS-CLI    PIC 9(9) VALUE 0.


       01  WS-SIN-DEPENDIENTES.
           05 WS-AHORA-TIENE-UN-DEP        PIC S9(15)V9999.

       01  WS-FILE-STATUS.
           05 FS-ARCHIVO-P                 PIC X(02) VALUE ' '.
              88 88-FS-ARCHIVO-P-OK                  VALUE '00'.
              88 88-FS-ARCHIVO-P-FIN                 VALUE '10'.

           05 WS-ARCHIVO-P-OPEN            PIC X(02) VALUE '  '.
              88 88-ARCHIVO-P-OPEN-SI                VALUE 'SI'.
              88 88-ARCHIVO-P-OPEN-NO                VALUE 'NO'.

       01  WS-FECNAC.
           05 WS-FECNAC-AAAA                  PIC 9(04) VALUE 0.
           05 WS-FECNAC-MM                    PIC 9(02) VALUE 0.
           05 WS-FECNAC-DD                    PIC 9(02) VALUE 0.

       01  WS-FECNAC-N REDEFINES WS-FECNAC    PIC 9(08).



       01  REG-ARCHIVO-P.
           05 NRO-REGISTRO         PIC  9(05) VALUE 99999.
           05 WS-BINARIO           PIC  9(04) BINARY VALUE 0.
           05 N-DISPLAY-UNSIGNED   PIC  9(10) VALUE 8888888888.
           05 N-DISPLAY-POSITIVE   PIC S9(10)V9 VALUE +7777777777,8.
           05 N-DISPLAY-NEGATIVE   PIC S9(10)V99 VALUE -6666666666,99.
           05 N-COMP-UNSIGNED      PIC  9(10) COMP VALUE  5555555555.
           05 N-COMP-POSITIVE      PIC S9(10) COMP VALUE +4444444444.
           05 N-COMP-NEGATIVE      PIC S9(10) COMP VALUE -3333333333.
           05 N-COMP-3-UNSIGNED    PIC  9(10) COMP-3 VALUE  2222222222.
           05 N-COMP-3-POSITIVE    PIC S9(11) COMP-3 VALUE +1111111111.
           05 N-COMP-3-NEGATIVE    PIC S9(10) COMP-3 VALUE -9191919191.
           05 N-PACKED-UNSIGNED    PIC  9(10) PACKED-DECIMAL VALUE  0.
           05 N-PACKED-POSITIVE    PIC S9(10) PACKED-DECIMAL VALUE +51.
           05 N-PACKED-NEGATIVE    PIC S9(10) PACKED-DECIMAL VALUE -52.
           05 X-ALFA               PIC  X(10) VALUE SPACES.
           05 X-ALFA-LOW-VALUE     PIC  X(10) VALUE LOW-VALUES.
           05 X-ALFA-HIGH-VALUE    PIC  X(10) VALUE HIGH-VALUES.
           05 N-POINTER                       POINTER.
           05 N-INDEX                         INDEX.
           05 FILLER               PIC  X(10).


       01  WS-TABLAS.
           05 WS-MES-TABLA-R.
              07 FILLER               PIC  X(12) VALUE 'ENERO'.
              07 FILLER               PIC  X(12) VALUE 'FEBRERO'.
              07 FILLER               PIC  X(12) VALUE 'MARZO'.
              07 FILLER               PIC  X(12) VALUE 'ABRIL'.
              07 FILLER               PIC  X(12) VALUE 'MAYO'.
              07 FILLER               PIC  X(12) VALUE 'JUNIO'.
              07 FILLER               PIC  X(12) VALUE 'JULIO'.
              07 FILLER               PIC  X(12) VALUE 'AGOSTO'.
              07 FILLER               PIC  X(12) VALUE 'SEPTIEMBRE'.
              07 FILLER               PIC  X(12) VALUE 'OCTUBRE'.
              07 FILLER               PIC  X(12) VALUE 'NOVIEMBRE'.
              07 FILLER               PIC  X(12) VALUE 'DICIEMBRE'.
           05 WS-MES-TABLA REDEFINES WS-MES-TABLA-R.
              07 WS-MES               PIC  X(12) OCCURS 12 TIMES.


       01  WS-ACUMULACION-PERIODOS.
           05 WS-ACUM-CARGA.
              07 FILLER.
                 09 FILLER            PIC 9(04)    VALUE 2015.
                 09 FILLER            PIC 9(15)V99 VALUE 0.
                 09 FILLER.
                    10 FILLER         PIC X(12)    VALUE 'ENERO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'FEBRERO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'MARZO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'ABRIL'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'MAYO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'JUNIO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'JULIO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'AGOSTO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'SEPTIEMBRE'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'OCTUBRE'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'NOVIEMBRE'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'DICIEMBRE'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                 09 FILLER            PIC 9(04) VALUE 2016.
                 09 FILLER            PIC 9(15)V99 VALUE 0.
                 09 FILLER.
                    10 FILLER         PIC X(12)    VALUE 'ENERO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'FEBRERO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'MARZO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'ABRIL'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'MAYO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'JUNIO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'JULIO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'AGOSTO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'SEPTIEMBRE'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'OCTUBRE'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'NOVIEMBRE'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'DICIEMBRE'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                 09 FILLER            PIC 9(04) VALUE 2017.
                 09 FILLER            PIC 9(15)V99 VALUE 0.
                 09 FILLER.
                    10 FILLER         PIC X(12)    VALUE 'ENERO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'FEBRERO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'MARZO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'ABRIL'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'MAYO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'JUNIO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'JULIO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'AGOSTO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'SEPTIEMBRE'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'OCTUBRE'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'NOVIEMBRE'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'DICIEMBRE'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                 09 FILLER            PIC 9(04) VALUE 2018.
                 09 FILLER            PIC 9(15)V99 VALUE 0.
                 09 FILLER.
                    10 FILLER         PIC X(12)    VALUE 'ENERO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'FEBRERO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'MARZO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'ABRIL'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'MAYO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'JUNIO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'JULIO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'AGOSTO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'SEPTIEMBRE'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'OCTUBRE'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'NOVIEMBRE'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'DICIEMBRE'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                 09 FILLER            PIC 9(04) VALUE 2019.
                 09 FILLER            PIC 9(15)V99 VALUE 0.
                 09 FILLER.
                    10 FILLER         PIC X(12)    VALUE 'ENERO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'FEBRERO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'MARZO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'ABRIL'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'MAYO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'JUNIO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'JULIO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'AGOSTO'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'SEPTIEMBRE'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'OCTUBRE'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'NOVIEMBRE'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.
                    10 FILLER         PIC X(12)    VALUE 'DICIEMBRE'.
                    10 FILLER         PIC 9(15)V99 VALUE 0.

           05 WS-VALORES-ANUALES  REDEFINES  WS-ACUM-CARGA.
              07 WS-TOT-VTAS-AA           OCCURS 5 TIMES.
                 09 WS-TOT-VTAS-AAAA        PIC 9(04).
                 09 WS-TOT-VTAS-AA-IMP      PIC 9(15)V99.
                 09 WS-TOT-VTAS-MM        OCCURS 12 TIMES.
                    11 WS-TOT-VTAS-MES      PIC X(12).
                    11 WS-TOT-VTAS-MM-IMP   PIC 9(15)V99.


       01  WS-CAMPOS-WORKING.
           05 WS-IMPORTE-ED1       PIC Z.ZZZ.ZZZ.ZZ9.
           05 WS-IMPORTE-ED2       PIC Z.ZZZ.ZZZ.ZZ9-.

       01  WS-FECHA-INICIO.
           05 WS-FECHA-INI-AAAA           PIC 9(04) VALUE 2020.
           05 WS-INTERMEDIO01             PIC X(03) VALUE 'I01'.
           05 WS-INTERMEDIO02             PIC X(03) VALUE 'I02'.
           05 WS-INTERMEDIO03             PIC X(03) VALUE 'I03'.
           05 WS-INTERMEDIO04             PIC X(03) VALUE 'I04'.
           05 WS-INTERMEDIO05             PIC X(03) VALUE 'I05'.
           05 WS-FECHA-INI-MM             PIC 9(02) VALUE 11.
           05 WS-FECHA-INI-DD             PIC 9(02) VALUE 05.

       66  WS-FECHA-INI-AAAA-MM
              RENAMES WS-FECHA-INI-AAAA
                 THRU WS-FECHA-INI-MM.


       01  WS-FECHA-FIN.
           05 WS-FECHA-FIN-AAAA-MM.
              07 WS-FECHA-FIN-AAAA        PIC 9(04) VALUE 2020.
              07 WS-FECHA-FIN-MM          PIC 9(02) VALUE 11.
           05 WS-FECHA-FIN-DD             PIC 9(02) VALUE 30.

       01  WS-FECHA-ORIG.
           03 WS-DIA                      PIC 9(02) VALUE 08.
           03 WS-MESS                     PIC 9(02) VALUE 10.
           03 WS-ANIO                     PIC 9(04) VALUE 2019.

       01  WS-FECHA-DEST.
           03 WS-DIA                      PIC 9(02) VALUE 30.
           03 WS-MESS                     PIC 9(02) VALUE 06.
           03 WS-ANIO                     PIC 9(04) VALUE 2020.

       77  WS-STRING                      PIC X(50) VALUE
      *     '         1         2         3         4         5'
      *     '12345678901234567890123456789012345678901234567890'
            'ABCDEFGCASACAMPOHIPOLITOYRIGOYENJUANBJUSTOCALLEUNO'.
       77  WS-STRING-SELECCIONADO         PIC X(20) VALUE ' '.


       PROCEDURE DIVISION.
      *------------------

       0000-MAIN-PROCEDURE.
      *--------------------

           DISPLAY "Hello world".

           DISPLAY 'ANTES'
           DISPLAY 'WS-STRING              :' WS-STRING.
           DISPLAY 'WS-STRING-SELECCIONADO :' WS-STRING-SELECCIONADO.

           MOVE  WS-STRING(17:8)           TO WS-STRING-SELECCIONADO.

           DISPLAY 'DESPUES'
           DISPLAY 'WS-STRING              :' WS-STRING.
           DISPLAY 'WS-STRING-SELECCIONADO :' WS-STRING-SELECCIONADO.

           DISPLAY '-----------------'.


           DISPLAY 'ANTES'
           DISPLAY 'WS-STRING              :' WS-STRING.
           DISPLAY 'WS-STRING-SELECCIONADO :' WS-STRING-SELECCIONADO.

           MOVE  ALL '*'                   TO WS-STRING(17:8).

           DISPLAY 'DESPUES'
           DISPLAY 'WS-STRING              :' WS-STRING.
           DISPLAY 'WS-STRING-SELECCIONADO :' WS-STRING-SELECCIONADO.

           DISPLAY '-----------------'.



           DISPLAY 'WS-ANIO OF WS-FECHA-DEST:' WS-ANIO OF WS-FECHA-DEST.
           DISPLAY 'WS-ANIO IN WS-FECHA-ORIG:' WS-ANIO IN WS-FECHA-ORIG.




           DISPLAY  'WS-FECHA-INICIO      :' WS-FECHA-INICIO.
           DISPLAY  'WS-FECHA-INI-AAAA-MM :' WS-FECHA-INI-AAAA-MM.
           DISPLAY '------------------------------------------------'.
           DISPLAY ' '.

           DISPLAY  'WS-FECHA-FIN         :' WS-FECHA-FIN.
           DISPLAY  'WS-FECHA-FIN-AAAA-MM :' WS-FECHA-FIN-AAAA-MM.
           DISPLAY '------------------------------------------------'.
           DISPLAY ' '.


           DISPLAY 9999,55

      *     MOVE 'ENERO'                  TO WS-MES(01) .
      *     MOVE 'OCTUBRE'                TO WS-MES(10).


           DISPLAY 'LEN DE WS-MES-TABLA:   ' LENGTH OF WS-MES-TABLA.
           DISPLAY 'LEN DE WS-MES-TABLA-R: ' LENGTH OF WS-MES-TABLA-R.
           DISPLAY 'LEN DE WS-TABLAS:      ' LENGTH OF WS-TABLAS.

           DISPLAY ' '.
           DISPLAY 'WS-TABLAS     : '  WS-TABLAS.
           DISPLAY 'WS-MES-TABLA-R: '  WS-MES-TABLA-R.
           DISPLAY 'WS-MES-TABLA  : '  WS-MES-TABLA.

           DISPLAY ' '.
           DISPLAY 'WS-MES(01)  : '  WS-MES(01).
           DISPLAY 'WS-MES(02)  : '  WS-MES(02).
           DISPLAY 'WS-MES(03)  : '  WS-MES(03).
           DISPLAY 'WS-MES(04)  : '  WS-MES(04).
           DISPLAY 'WS-MES(05)  : '  WS-MES(05).
           DISPLAY 'WS-MES(06)  : '  WS-MES(06).
           DISPLAY 'WS-MES(07)  : '  WS-MES(07).
           DISPLAY 'WS-MES(08)  : '  WS-MES(08).
           DISPLAY 'WS-MES(09)  : '  WS-MES(09).
           DISPLAY 'WS-MES(10)  : '  WS-MES(10).
           DISPLAY 'WS-MES(11)  : '  WS-MES(11).
           DISPLAY 'WS-MES(12)  : '  WS-MES(12).

           MOVE 4            TO IND-ANIO.
           MOVE 5            TO IND-MES.

           MOVE 2540,60      TO WS-TOT-VTAS-MM-IMP(IND-ANIO,IND-MES).


           PERFORM VARYING IND-ANIO
              FROM 1 BY 1 UNTIL IND-ANIO > 5
             DISPLAY ' '
             DISPLAY ' AÑO NUEVO '
             DISPLAY 'IND-ANIO          : ' IND-ANIO
             DISPLAY 'WS-TOT-VTAS-AAAA  : ' WS-TOT-VTAS-AAAA(IND-ANIO)
             DISPLAY 'WS-TOT-VTAS-AA-IMP: ' WS-TOT-VTAS-AA-IMP(IND-ANIO)
             PERFORM VARYING IND-MES
                FROM 1 BY 1 UNTIL IND-MES > 12
               DISPLAY ' '
               DISPLAY 'IND-MES        : ' IND-MES
               DISPLAY 'WS-TOT-VTAS-MES: '
                                   WS-TOT-VTAS-MES(IND-ANIO,IND-MES)
               DISPLAY 'WS-TOT-VTAS-MM-IMP: '
                                    WS-TOT-VTAS-MM-IMP(IND-ANIO,IND-MES)
               ADD WS-TOT-VTAS-MM-IMP(IND-ANIO,IND-MES)
                TO WS-TOT-VTAS-AA-IMP(IND-ANIO)
             END-PERFORM
           END-PERFORM.

           PERFORM VARYING IND-ANIO
              FROM 1 BY 1 UNTIL IND-ANIO > 5
             DISPLAY ' '
             DISPLAY ' AÑO NUEVO TOTALES'
             DISPLAY 'IND-ANIO          : ' IND-ANIO
             DISPLAY 'WS-TOT-VTAS-AAAA  : ' WS-TOT-VTAS-AAAA(IND-ANIO)
             DISPLAY 'WS-TOT-VTAS-AA-IMP: ' WS-TOT-VTAS-AA-IMP(IND-ANIO)
           END-PERFORM.


           PERFORM 10000-INICIO           THRU FIN-10000.

           PERFORM 20000-PROCESO          THRU FIN-20000.

           PERFORM 30000-FIN              THRU FIN-30000.



       10000-INICIO.
      *-------------

           DISPLAY '10000-INICIO'.

           PERFORM 10100-OPEN-ARCHIVOS    THRU FIN-10100.

       FIN-10000.
           EXIT.

       10100-OPEN-ARCHIVOS.
      *--------------------

           DISPLAY '10100-OPEN-ARCHIVOS'.

           OPEN OUTPUT ARCHIVO-P.
           DISPLAY 'FS-ARCHIVO-P: ' FS-ARCHIVO-P

           EVALUATE TRUE
               WHEN 88-FS-ARCHIVO-P-OK
                    SET 88-ARCHIVO-P-OPEN-SI      TO TRUE
                    DISPLAY 'OPEN ARCHIVO-P OK'

               WHEN OTHER
                    DISPLAY 'OPEN ARCHIVO-P NOT OK'
                    PERFORM 30000-FIN      THRU FIN-30000

           END-EVALUATE.

       FIN-10100.
           EXIT.

       20000-PROCESO.
      *--------------------

           DISPLAY '20000-PROCESO'.



           DISPLAY ' '.
           DISPLAY 'GRABO REGISTRO 99999 DEFAULT'
           PERFORM 20100-GRABO-ARCHIVO-P   THRU FIN-20100.


           DISPLAY ' '.
           DISPLAY 'GRABO REGISTRO 1 INITIALIZE'.
           INITIALIZE REG-ARCHIVO-P.
           MOVE 1                           TO NRO-REGISTRO.
           PERFORM 20100-GRABO-ARCHIVO-P   THRU FIN-20100.

           DISPLAY ' '.
           DISPLAY 'GRABO REGISTRO 2  CON MOVE DE VALORES'.

           MOVE 2                           TO NRO-REGISTRO.
           MOVE 5                           TO WS-BINARIO.
           MOVE 5000                        TO N-DISPLAY-UNSIGNED.
           MOVE +5000                       TO N-DISPLAY-POSITIVE.
           MOVE -5000                       TO N-DISPLAY-NEGATIVE.

           MOVE 5000                        TO N-COMP-UNSIGNED.
           MOVE +5000                       TO N-COMP-POSITIVE.
           MOVE -5000                       TO N-COMP-NEGATIVE.

           MOVE 5000                        TO N-COMP-3-UNSIGNED.
           MOVE +5000                       TO N-COMP-3-POSITIVE.
           MOVE -5000                       TO N-COMP-3-NEGATIVE.

           MOVE 5000                        TO N-PACKED-UNSIGNED.
           MOVE +5000                       TO N-PACKED-POSITIVE.
           MOVE -5000                       TO N-PACKED-NEGATIVE.
           SET N-POINTER         TO ADDRESS OF X-ALFA-HIGH-VALUE.

           MOVE ALL '*'                     TO X-ALFA.
           MOVE LOW-VALUE                   TO X-ALFA-LOW-VALUE.
           MOVE HIGH-VALUE                  TO X-ALFA-HIGH-VALUE.

           PERFORM 20100-GRABO-ARCHIVO-P   THRU FIN-20100.

           DISPLAY ' '.
           DISPLAY 'IMPORTES EDITADOS'.

           DISPLAY ' '.
           MOVE N-DISPLAY-UNSIGNED          TO  WS-IMPORTE-ED1
                                                WS-IMPORTE-ED2.
           DISPLAY 'N-DISPLAY-UNSIGNED: ' N-DISPLAY-UNSIGNED.
           DISPLAY 'WS-IMPORTE-ED1: ' WS-IMPORTE-ED1.
           DISPLAY 'WS-IMPORTE-ED2: ' WS-IMPORTE-ED2.

           DISPLAY ' '.
           MOVE N-DISPLAY-POSITIVE          TO  WS-IMPORTE-ED1
                                                WS-IMPORTE-ED2.
           DISPLAY 'N-DISPLAY-POSITIVE: ' N-DISPLAY-POSITIVE.
           DISPLAY 'WS-IMPORTE-ED1: ' WS-IMPORTE-ED1.
           DISPLAY 'WS-IMPORTE-ED2: ' WS-IMPORTE-ED2.

           DISPLAY ' '.
           MOVE N-DISPLAY-NEGATIVE          TO  WS-IMPORTE-ED1
                                                WS-IMPORTE-ED2.
           DISPLAY 'N-DISPLAY-NEGATIVE: ' N-DISPLAY-NEGATIVE.
           DISPLAY 'WS-IMPORTE-ED1: ' WS-IMPORTE-ED1.
           DISPLAY 'WS-IMPORTE-ED2: ' WS-IMPORTE-ED2.

       FIN-20000.
           EXIT.


       20100-GRABO-ARCHIVO-P.
      *-----------------------

           DISPLAY ' '.
           DISPLAY '20100-GRABO-ARCHIVO-P'.
           DISPLAY 'LENGTH OF REG-ARCHIVO-P:' LENGTH OF REG-ARCHIVO-P.
           DISPLAY 'REG-ARCHIVO-P :' REG-ARCHIVO-P.

           DISPLAY 'NRO-REGISTRO       ' NRO-REGISTRO.
           DISPLAY 'WS-BINARIO         ' WS-BINARIO.
           DISPLAY 'N-DISPLAY-UNSIGNED ' N-DISPLAY-UNSIGNED.
           DISPLAY 'N-DISPLAY-POSITIVE ' N-DISPLAY-POSITIVE.
           DISPLAY 'N-DISPLAY-NEGATIVE ' N-DISPLAY-NEGATIVE.

           DISPLAY 'N-COMP-UNSIGNED    ' N-COMP-UNSIGNED.
           DISPLAY 'N-COMP-POSITIVE    ' N-COMP-POSITIVE.
           DISPLAY 'N-COMP-NEGATIVE    ' N-COMP-NEGATIVE.

           DISPLAY 'N-COMP-3-UNSIGNED  ' N-COMP-3-UNSIGNED.
           DISPLAY 'N-COMP-3-POSITIVE  ' N-COMP-3-POSITIVE.
           DISPLAY 'N-COMP-3-NEGATIVE  ' N-COMP-3-NEGATIVE.

           DISPLAY 'N-PACKED-UNSIGNED  ' N-PACKED-UNSIGNED.
           DISPLAY 'N-PACKED-POSITIVE  ' N-PACKED-POSITIVE.
           DISPLAY 'N-PACKED-NEGATIVE  ' N-PACKED-NEGATIVE.

           DISPLAY 'X-ALFA             ' X-ALFA.
           DISPLAY 'X-ALFA-LOW-VALUE   ' X-ALFA-LOW-VALUE.
           DISPLAY 'X-ALFA-HIGH-VALUE  ' X-ALFA-HIGH-VALUE.
           DISPLAY 'N-POINTER          ' N-POINTER.


           WRITE REG-ARCHIVO-P-FD        FROM REG-ARCHIVO-P.
           DISPLAY 'FS-ARCHIVO-P: ' FS-ARCHIVO-P.

           EVALUATE TRUE
               WHEN 88-FS-ARCHIVO-P-OK
                    DISPLAY 'WRITE ARCHIVO-P OK'

               WHEN OTHER
                    DISPLAY 'WRITE ARCHIVO-P NOT OK'
                    PERFORM 30000-FIN      THRU FIN-30000

           END-EVALUATE.

       FIN-20100.
           EXIT.


       30000-FIN.
      *----------

           DISPLAY '30000-FIN'.

           PERFORM 30100-CIERRO-ARCHIVOS     THRU FIN-30100.

           STOP RUN.

       FIN-30000.
           EXIT.

       30100-CIERRO-ARCHIVOS.
      *----------------------

           DISPLAY '30100-CIERRO-ARCHIVOS'.

           IF 88-ARCHIVO-P-OPEN-SI
              CLOSE ARCHIVO-P
              DISPLAY 'FS-ARCHIVO-P: ' FS-ARCHIVO-P
              EVALUATE TRUE
                  WHEN 88-FS-ARCHIVO-P-OK
                       DISPLAY 'CLOSE ARCHIVO-P OK'
                  WHEN OTHER
                       DISPLAY 'CLOSE ARCHIVO-P NOT OK'
              END-EVALUATE
           END-IF.

       FIN-30100.
           EXIT.

       END PROGRAM PROGRAMA01.
