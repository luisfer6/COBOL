      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
      *9)Escriba las instrucciones COBOL para los siguientes trabajos de
      * cálculo declarando cada variable utilizada en la
      * WORKING-STORAGE SECTION:
      *i.  Haga TOTAL igual a la suma de SUBTOTAL-1 y SUBTOTAL-2
      *ii. Sume el valor PRIMA a PAGO
      *iii.Calcule COMISION como producto TASA y VENTAS
      *iv. Haga que el valor ACCIONES sea igual al numero de acciones
      *     que se pueden comprar por PESOS al PRECIO por acción
      *v.Divida el valor de MONTO por CUOTAS con redondeo de 2
      * decimales dejando el resultado en IMPORTE-A-PAGAR

       IDENTIFICATION DIVISION.
      *------------------------
       PROGRAM-ID. EJERCICIO-9.
       ENVIRONMENT DIVISION.
      *---------------------
       CONFIGURATION SECTION.
      *----------------------
      *     SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
      *--------------
       FILE SECTION.
      *-------------
       WORKING-STORAGE SECTION.
      *------------------------
       01  SUB-TOTAL-1         PIC 9(08)V99.
       01  SUB-TOTAL-2         PIC 9(08)V99.
       01  TOTAL               PIC $9(08).99.
       01  PRIMA               PIC 9(08)V99.
       01  PAGO                PIC 9(08)V99.
       01  SUMA                PIC $9(08).99.
       01  TASA                PIC 9(08)V99.
       01  VENTAS              PIC 9(08)V99.
       01  COMISION            PIC $9(08).99.
       01  PESOS               PIC 9(08)V99.
       01  PRECIO-POR-ACCION   PIC 9(08)V99.
       01  ACCION              PIC $9(08).99.
       01  MONTO               PIC 9(08) .
       01  CUOTAS              PIC 9(08)V9 .
       01  IMPORTE-A-PAGAR     PIC 9(08)V99.
       01  FILLER              PIC 9(03)V9.
       PROCEDURE DIVISION.
      *-------------------
       MAIN-PROCEDURE.
      *-----------------------------------------------------------------
      *    i.TOTAL IGUAL A LA SUMA  SUB-TOTAL-1 Y SUB-TOTAL-2
           DISPLAY 'Introduzca el primer Sub-total-1: '.
           ACCEPT SUB-TOTAL-1.
           DISPLAY 'Introduzca el primer Sub-total-2: '.
           ACCEPT SUB-TOTAL-2.
           ADD SUB-TOTAL-1 TO SUB-TOTAL-2 GIVING TOTAL.
           DISPLAY 'Total: ' TOTAL.
           DISPLAY ' '
      *-----------------------------------------------------------------
      *    ii.Suma el valor PRIMA a PAGO
           DISPLAY 'Introduzca el valor Prima: '.
           ACCEPT PRIMA.
           DISPLAY 'Introduzca el valor Pago: '.
           ACCEPT PAGO.
           ADD PRIMA TO PAGO GIVING SUMA.
           DISPLAY 'Suma: ' SUMA.
           DISPLAY ' '
      *-----------------------------------------------------------------
      *    iii.Calcula la Comisión como producto Tasa y Ventas
           DISPLAY 'Introduzca el valor Tasa: '.
           ACCEPT TASA.
           DISPLAY 'Introduzca el numero de Ventas: '.
           ACCEPT VENTAS.
           MULTIPLY TASA BY VENTAS GIVING COMISION.
           DISPLAY 'Comision: ' COMISION.
           DISPLAY ' '
      *-----------------------------------------------------------------
      *    iv.valor ACCIONES sea igual al numero de acciones que se
      *    pueden comprar por PESOS al PRECIO por acción
           DISPLAY 'Introduzca pesos disponibles: '.
           ACCEPT PESOS.
           DISPLAY 'Introduzca el Precio por Accion: '.
           ACCEPT PRECIO-POR-ACCION.
           DIVIDE PESOS BY PRECIO-POR-ACCION GIVING ACCION.
           DISPLAY 'Accion: ' ACCION.
           DISPLAY ' '
      *------------------------------------------------------
      *    v.Divida el valor de MONTO por CUOTAS con redondeo de 2
      *     decimales dejando el resultado en IMPORTE-A-PAGAR
           DISPLAY 'Introduzca el valor de Monto: '.
           ACCEPT MONTO.
           DISPLAY 'Introduzca numero de Cuotas: '.
           ACCEPT CUOTAS.
           COMPUTE IMPORTE-A-PAGAR ROUNDED = MONTO/CUOTAS.
           DISPLAY 'Importe a pagar: ' IMPORTE-A-PAGAR.
           DISPLAY ' '.
            STOP RUN.
       END PROGRAM EJERCICIO-9.
