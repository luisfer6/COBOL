      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
      * 7)    Un distribuidor mayorista emplea varios vendedores en los
      *cinco territorios designados de un país, se usa un método de
      * codificacióN  para denotar territorio y vendedor, el primer
      * digito indica el territorio y el segundo digito el vendedor
      *individual, como se muestra a continuación:
      * TERRITORIO      CODIGO
      *     Suroeste    11-16
      *     Noroeste    21-25
      *     Central     31-38
      *     Nordeste    41-49
      *     Sudeste     51-54
      *a)    Escriba entradas de nombres-de-condición de tal manera
      * que para cada valor CODIGO-VENTAS podamos determinar el
      * territorio e identificar errores en el código. Por ejemplo
      * los códigos 08 y 19 serian códigos errados.
      *b) Para ciertos propósitos estamos interesados en probar si un
      * valor de CODIGO-VENTAS se refiere al territorio central o a los
      * otros territorios. Forme nombres-de-condición que nos permitan
      * probar si es territorio central u otros territorios
      *-----------------------------------------------------------------

       IDENTIFICATION DIVISION.
      *------------------------
       PROGRAM-ID. ejercicio7.
       DATA DIVISION.
      *--------------
       FILE SECTION.
      *-------------
       WORKING-STORAGE SECTION.
      *------------------------
       77  WS-BLANCO                     PIC X(132).
       77  WS-COD-CONDICION-A            PIC 9(02).
       77  WS-COD-CONDICION-B            PIC 9(02).

       01  WS-COD-X.
           03  WS-COD                     PIC 9(02).
               88  88-COD-SUROESTE                  VALUE 11 THRU 16.
               88  88-COD-NOROESTE                  VALUE 21 THRU 25.
               88  88-COD-CENTRAL                   VALUE 31 THRU 38.
               88  88-COD-NORDESTE                  VALUE 41 THRU 49.
               88  88-COD-SUDESTE                   VALUE 51 THRU 54.

       01  FILLER REDEFINES WS-COD-X.
           03 WS-COD-REGION               PIC X(01).
               88 88-COD-SUROESTE-RED               VALUE '1'.
               88 88-COD-NOROESTE-RED               VALUE '2'.
               88 88-COD-CENTRAL-RED                VALUE '3'.
               88 88-COD-NORDESTE-RED               VALUE '4'.
               88 88-COD-SUDESTE-RED                VALUE '5'.
           03 FILLER                      PIC X(01).




       01  WS-MAY-LINEA1.
           05 FILLER                   PIC X(10)  VALUE 'TERRITORIO'.
           05 FILLER                   PIC X(06)  VALUE SPACES.
           05 FILLER                   PIC X(12)  VALUE 'CODIGO'.
           05 FILLER                   PIC X(104) VALUE SPACES.

       01  WS-MAY-LINEA2.
           05 FILLER                   PIC X(08)  VALUE 'Suroeste'.
           05 FILLER                   PIC X(08)  VALUE SPACES.
           05 WS-COD2-A                PIC 9(02).
           05 FILLER                   PIC X      VALUE '-'.
           05 WS-COD2-B                PIC 9(02).
           05 FILLER                   PIC X(111) VALUE SPACES.

       01 WS-MAY-LINEA3.
           05 FILLER                   PIC X(08)  VALUE 'Noroeste'.
           05 FILLER                   PIC X(08)  VALUE SPACES.
           05 WS-COD3-A                PIC 9(02).
           05 FILLER                   PIC X      VALUE '-'.
           05 WS-COD3-B                PIC 9(02).
           05 FILLER                   PIC X(111) VALUE SPACES.

       01  WS-MAY-LINEA4.
           05 FILLER                   PIC X(07)  VALUE 'Central'.
           05 FILLER                   PIC X(09)  VALUE SPACES.
           05 WS-COD4-A                PIC 9(02).
           05 FILLER                   PIC X      VALUE '-'.
           05 WS-COD4-B                PIC 9(02).
           05 FILLER                   PIC X(111) VALUE SPACES.

       01  WS-MAY-LINEA5.
           05 FILLER                   PIC X(08)  VALUE 'Nordeste'.
           05 FILLER                   PIC X(08)  VALUE SPACES.
           05 WS-COD5-A                PIC 9(02).
           05 FILLER                   PIC X      VALUE '-'.
           05 WS-COD5-B                PIC 9(02).
           05 FILLER                   PIC X(111) VALUE SPACES.

       01  WS-MAY-LINEA6.
           05 FILLER                   PIC X(07) VALUE 'Sudeste'.
           05 FILLER                   PIC X(09) VALUE SPACES.
           05 WS-COD6-A                PIC 9(02).
           05 FILLER                   PIC X VALUE '-'.
           05 WS-COD6-B                PIC 9(02).
           05 FILLER                   PIC X(111) VALUE SPACES.


           PROCEDURE DIVISION.
      *-----------------------
           MAIN-PROCEDURE.

           MOVE 11 TO WS-COD2-A.
           MOVE 16 TO WS-COD2-B.
           MOVE 21 TO WS-COD3-A.
           MOVE 25 TO WS-COD3-B.
           MOVE 31 TO WS-COD4-A.
           MOVE 38 TO WS-COD4-B.
           MOVE 41 TO WS-COD5-A.
           MOVE 49 TO WS-COD5-B.
           MOVE 51 TO WS-COD6-A.
           MOVE 54 TO WS-COD6-B.


           DISPLAY WS-MAY-LINEA1.
           DISPLAY WS-MAY-LINEA2.
           DISPLAY WS-MAY-LINEA3.
           DISPLAY WS-MAY-LINEA4.
           DISPLAY WS-MAY-LINEA5.
           DISPLAY WS-MAY-LINEA6.
           DISPLAY WS-BLANCO.

           DISPLAY 'Ingrese un codigo de Territorio: '.
           ACCEPT WS-COD-X(1:1).

           DISPLAY 'Ingrese un codigo de vendedor: '.
           ACCEPT WS-COD-X(2:1).

           DISPLAY WS-BLANCO.

           DISPLAY "Su codigo es: " WS-COD-X.

           EVALUATE TRUE
               WHEN 88-COD-SUROESTE-RED
                   DISPLAY 'Su region es Suroeste.'
               WHEN 88-COD-NOROESTE-RED
                   DISPLAY 'Su region es Noroeste.'
               WHEN 88-COD-CENTRAL-RED
                   DISPLAY 'Su region es Central.'
               WHEN 88-COD-NORDESTE-RED
                   DISPLAY 'Su region es Nordeste.'
               WHEN 88-COD-SUDESTE-RED
                   DISPLAY 'Su region es Sudeste.'
               WHEN OTHER
                   DISPLAY 'Region invalida.'
           END-EVALUATE.

           EVALUATE TRUE
            WHEN 88-COD-SUROESTE
            WHEN 88-COD-NOROESTE
            WHEN 88-COD-CENTRAL
            WHEN 88-COD-NORDESTE
            WHEN 88-COD-SUDESTE
             DISPLAY 'Su codigo de territorio y de vendedor son correcto
      -    's.'
            WHEN OTHER
             DISPLAY 'Codigo de vendedor invalido'
           END-EVALUATE.

           STOP RUN.
       END PROGRAM ejercicio7.
