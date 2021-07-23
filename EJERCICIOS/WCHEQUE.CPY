      ******************************************************************
      *
      *Purpose:COPY ESTRUCUTURA DEL ARCHIVO DE CHEQUES
      *LONGITUD DE REGISTRO  X(54)
      *
      ******************************************************************

       01  REG-CHEQUE.
           03  CHEQUE-NUMERO      PIC 9(05).
           03  DEBITO             PIC 9(06)V99.
           03  DESCUENTO          PIC 9(04)V99.
           03  EFECTIVO           PIC 9(06)V99.
           03  NOMBRE             PIC X(15).
           03  NUMERO             PIC X(04).
           03  DIA                PIC 99.
           03  MES                PIC 99.
           03  ANIO               PIC 9999.
