      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           77  TOTAL               PIC S9(15)V99   VALUE 0.
           77  SUBTOTAL-1          PIC S9(15)V99   VALUE 0.
           77  SUBTOTAL-2          PIC S9(15)V99   VALUE 0.
           77  TASA                PIC 9(03)V9999  VALUE 0.
           77  PRIMA               PIC 9(15)V99    VALUE 0.
           77  PAGO                PIC S9(15)V99   VALUE 0.
           77  COMISION            PIC S9(15)V99   VALUE 0.
           77  VENTAS              PIC S9(15)V99   VALUE 0.
           77  ACCIONES            PIC 9(15)V99    VALUE 0.
           77  PESOS               PIC 9(15)V99    VALUE 0.
           77  PRECIO              PIC 9(15)V99    VALUE 0.
           77  MONTO               PIC 9(15)V99    VALUE 0.
           77  CUOTAS              PIC 9(13)V9999  VALUE 0.
           77  IMPORTE-A-PAGAR     PIC 9(15)V99    VALUE 0.
      *AQUI VAN MIS VARIABLES DE EDICION********
           77  IMPORTE-ED          PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99-.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 1000-CASO-1.
           PERFORM 2000-CASO-2.
           PERFORM 3000-CASO-3.
           PERFORM 4000-CASO-4.
           PERFORM 5000-CASO-5.
           STOP RUN.

      *SUMAR SUBTOTAL 1 Y SUBTOTAL 2 A TOTAL

       1000-CASO-1.
           DISPLAY "CALCULO DE TOTAL".
           DISPLAY "INGRESE SUBTOTAL 1".
           ACCEPT  SUBTOTAL-1.
           DISPLAY "INGRESE SUBTOTAL 2".
           ACCEPT  SUBTOTAL-2.
           ADD     SUBTOTAL-1, SUBTOTAL-2 TO TOTAL.
           MOVE    TOTAL                  TO IMPORTE-ED.
           DISPLAY "EL TOTAL ES: " IMPORTE-ED.

      *SUMAR EL VALOR PRIMA A PAGO

       2000-CASO-2.
           DISPLAY "SUMANDO PRIMA A PAGO".
           DISPLAY "INGRESE MONTO DE LA PRIMA".
           ACCEPT  PRIMA.
           DISPLAY "INGRESE MONTO DEL PAGO".
           ACCEPT  PAGO.
           ADD     PRIMA           TO PAGO.
           MOVE    PAGO            TO IMPORTE-ED.
           DISPLAY "EL MONTO DE PAGO ES: " IMPORTE-ED.

      *CALCULAR COMISION COMO PRODUCTO DE TASA Y VENTAS***

       3000-CASO-3.
           DISPLAY  "PRODUCTO TASA POR VENTAS".
           DISPLAY  "INGRESE VALOR DE TASA".
           ACCEPT   TASA.
           DISPLAY  "INGRESE VALOR DE VENTAS".
           ACCEPT   VENTAS.
           MULTIPLY TASA BY VENTAS GIVING COMISION.
           MOVE     COMISION       TO IMPORTE-ED.
           DISPLAY  "LA COMISION ES: " IMPORTE-ED.

      *CALCULAR NUMERO DE ACCIONES QUE SE PUEDEN COMPRAR***

       4000-CASO-4.
           DISPLAY "CALCULO DE CANTIDAD DE ACCIONES A COMPRAR".
           DISPLAY "INGRESE PRECIO DE LA ACCION".
           ACCEPT  PRECIO.
           DISPLAY "INGRESE PESOS PARA COMPRAR ACCIONES".
           ACCEPT  PESOS.
           DIVIDE  PESOS BY PRECIO GIVING ACCIONES.
           MOVE    ACCIONES        TO IMPORTE-ED.
           DISPLAY "SE PUEDEN COMPRAR " IMPORTE-ED " ACCIONES".

      *DIVIDIMOS MONTO POR CUOTAS CON REDONDEO EN IMPORTE A PAGAR

       5000-CASO-5.
           DISPLAY "DIVISION DE MONTO POR CUOTAS".
           DISPLAY "INGRESE MONTO".
           ACCEPT  MONTO.
           DISPLAY "INGRESE CUOTAS".
           ACCEPT  CUOTAS.
           DIVIDE  CUOTAS INTO MONTO GIVING IMPORTE-A-PAGAR ROUNDED.
           MOVE    IMPORTE-A-PAGAR  TO IMPORTE-ED.
           DISPLAY "EL MONTO DE LA CUOTA ES: " IMPORTE-ED.

       END PROGRAM YOUR-PROGRAM-NAME.
