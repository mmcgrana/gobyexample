IMPLEMENTATION MODULE Sorting;

(* J. Andrea, Dec.16/91 *)
(* This code may be freely used and distributed, it may not be sold. *)

(* Adapted to ISO Module-2 by Frank Schoonjans  Feb 2004 *)

FROM Storage IMPORT ALLOCATE;

CONST
   max_stack = 20;
   n_small   = 6; (* use a simple sort for this size and smaller *)

VAR
  rtemp :REAL;
  ctemp :CARDINAL;

  L, R, n               :INTEGER;
  top, bottom, lastflip :INTEGER;

  tos            :CARDINAL;
  Lstack, Rstack :ARRAY [1..max_stack] OF INTEGER;

      (* --------------------------------------------------- *)
      PROCEDURE CardQSortIndex( x :ARRAY OF CARDINAL; array_len :CARDINAL;
                                VAR index :ARRAY OF CARDINAL );

      VAR
        median : CARDINAL;
        i,j    : INTEGER;
      BEGIN

        n := VAL(INTEGER,array_len) - 1; (* back to zero offset *)

        (* initialize the index *)
        FOR i := 0 TO n DO
          index[i] := VAL(CARDINAL,i);
        END;

        tos := 0;

        L := 0;  R := n;

        (* PUSH very first set *)
        tos := tos + 1;  Lstack[tos] := L;  Rstack[tos] := R;

        REPEAT

          (* POP *)
          L := Lstack[tos];  R := Rstack[tos];  tos := tos - 1;

          IF R - L + 1 > n_small THEN

            REPEAT
              i := L;  j := R;    median := x[index[( L + R ) DIV 2]];

              REPEAT
                WHILE x[index[i]] < median DO
                  i := i + 1;
                END;
                WHILE median < x[index[j]] DO
                  j := j - 1;
                END;

                IF i <= j THEN (* swap *)
                  ctemp := index[i];  index[i] := index[j];  index[j] := ctemp;
                  i := i + 1;  j := j - 1;
                END;
              UNTIL i > j;

              IF j - L < R - i THEN
                IF i < R THEN (* PUSH *)
                  tos := tos + 1;  Lstack[tos] := i;  Rstack[tos] := R;
                END;
                R := j;
              ELSE
                IF L < j THEN (* push *)
                  tos := tos + 1;  Lstack[tos] := L;  Rstack[tos] := j;
                END;
                L := i;
              END;

            UNTIL L >= R;

         ELSE

           (* small sort for small number of values *)
           FOR i := L TO R - 1 DO
             FOR j := i TO R DO
               IF x[index[i]] > x[index[j]] THEN
                  ctemp    := index[i];
                  index[i] := index[j];
                  index[j] := ctemp
               END;
             END;
           END;

         END; (* check for small *)

       UNTIL tos = 0;

      END CardQSortIndex;

      (* --------------------------------------------------- *)
      PROCEDURE RealQSortIndex( x :ARRAY OF REAL; array_len :CARDINAL;
                                VAR index :ARRAY OF CARDINAL );

      VAR
        median :REAL;
        i,j    :INTEGER;
      BEGIN

        n := VAL(INTEGER,array_len) - 1; (* back to zero offset *)

        (* initialize the index *)
        FOR i := 0 TO n DO
          index[i] := VAL(CARDINAL,i);
        END;

        tos := 0;

        L := 0;  R := n;

        (* PUSH very first set *)
        tos := tos + 1;  Lstack[tos] := L;  Rstack[tos] := R;

        REPEAT

          (* POP *)
          L := Lstack[tos];  R := Rstack[tos];  tos := tos - 1;

          IF R - L + 1 > n_small THEN

            REPEAT
              i := L;  j := R;    median := x[index[( L + R ) DIV 2]];

              REPEAT
                WHILE x[index[i]] < median DO
                  i := i + 1;
                END;
                WHILE median < x[index[j]] DO
                  j := j - 1;
                END;

                IF i <= j THEN (* swap *)
                  ctemp := index[i];  index[i] := index[j];  index[j] := ctemp;
                  i := i + 1;  j := j - 1;
                END;
              UNTIL i > j;

              IF j - L < R - i THEN
                IF i < R THEN (* PUSH *)
                  tos := tos + 1;  Lstack[tos] := i;  Rstack[tos] := R;
                END;
                R := j;
              ELSE
                IF L < j THEN (* push *)
                  tos := tos + 1;  Lstack[tos] := L;  Rstack[tos] := j;
                END;
                L := i;
              END;

            UNTIL L >= R;

         ELSE

           (* small sort for small number of values *)
           FOR i := L TO R - 1 DO
             FOR j := i TO R DO
               IF x[index[i]] > x[index[j]] THEN
                  ctemp    := index[i];
                  index[i] := index[j];
                  index[j] := ctemp
               END;
             END;
           END;

         END; (* check for small *)

       UNTIL tos = 0;

      END RealQSortIndex;

      (* --------------------------------------------------- *)
      PROCEDURE CardQSort( VAR x :ARRAY OF CARDINAL; array_len :CARDINAL );

      VAR
        median : CARDINAL;
        n,i,j  : INTEGER;
      BEGIN

        n := VAL(INTEGER,array_len) - 1; (* back to zero offset *)

        tos := 0;

        L := 0;  R := n;

        (* PUSH very first set *)
        tos := tos + 1;  Lstack[tos] := L;  Rstack[tos] := R;

        REPEAT

          (* POP *)
          L := Lstack[tos];  R := Rstack[tos];  tos := tos - 1;

          IF R - L + 1 > n_small THEN

            REPEAT
              i := L;  j := R;    median := x[( L + R ) DIV 2];

              REPEAT
                WHILE x[i] < median DO
                  i := i + 1;
                END;
                WHILE median < x[j] DO
                  j := j - 1;
                END;

                IF i <= j THEN (* swap *)
                  ctemp := x[i];  x[i] := x[j];  x[j] := ctemp;
                  i := i + 1;  j := j - 1;
                END;
              UNTIL i > j;

              IF j - L < R - i THEN
                IF i < R THEN (* PUSH *)
                  tos := tos + 1;  Lstack[tos] := i;  Rstack[tos] := R;
                END;
                R := j;
              ELSE
                IF L < j THEN (* push *)
                  tos := tos + 1;  Lstack[tos] := L;  Rstack[tos] := j;
                END;
                L := i;
              END;

            UNTIL L >= R;

         ELSE

           (* small sort for small number of values *)
           FOR i := L TO R - 1 DO
             FOR j := i TO R DO
               IF x[i] > x[j] THEN
                  ctemp := x[i];
                  x[i]  := x[j];
                  x[j]  := ctemp
               END;
             END;
           END;

         END; (* check for small *)

       UNTIL tos = 0;

      END CardQSort;

      (* ----------------------------------------------------- *)
      PROCEDURE CardBSort( VAR x :ARRAY OF CARDINAL; array_len :CARDINAL );
      VAR i,j : INTEGER;
      BEGIN
        top    := 0;      (* open arrays are zero offset *)
        bottom := VAL(INTEGER,array_len) - 1;

        WHILE top < bottom DO

          lastflip := top;

          FOR i := top TO bottom-1 DO
             IF x[i] > x[i+1] THEN    (* flip *)
               ctemp  := x[i];
               x[i]   := x[i+1];
               x[i+1] := ctemp;
               lastflip := i;
             END;
          END;

          bottom := lastflip;

          IF bottom > top THEN

             i := bottom - 1;
             FOR j := top TO bottom-1 DO
               IF x[i] > x[i+1] THEN    (* flip *)
                 ctemp  := x[i];
                 x[i]   := x[i+1];
                 x[i+1] := ctemp;
                 lastflip := i;
               END;
               i := i - 1;
             END;

             top := lastflip + 1;

          ELSE
             (* force a loop failure *)
             top := bottom + 1;
          END;

       END;

      END CardBSort;


      (* ----------------------------------------------------- *)
      PROCEDURE RealBSort( VAR x :ARRAY OF REAL; array_len :CARDINAL );
      VAR bottom,top : INTEGER;
          i,j        : INTEGER;
      BEGIN
        top    := 0;      (* open arrays are zero offset *)
        bottom := VAL(INTEGER,array_len) - 1;

        WHILE top < bottom DO

          lastflip := top;

          FOR i := top TO bottom-1 DO
             IF x[i] > x[i+1] THEN    (* flip *)
               rtemp  := x[i];
               x[i]   := x[i+1];
               x[i+1] := rtemp;
               lastflip := i;
             END;
          END;

          bottom := lastflip;

          IF bottom > top THEN

             i := bottom - 1;
             FOR j := top TO bottom-1 DO
               IF x[i] > x[i+1] THEN    (* flip *)
                 rtemp  := x[i];
                 x[i]   := x[i+1];
                 x[i+1] := rtemp;
                 lastflip := i;
               END;
               i := i - 1;
             END;

             top := lastflip + 1;

          ELSE
             (* force a loop failure *)
             top := bottom + 1;
          END;

       END;

      END RealBSort;


    (* ----------------------------------------------------- *)
    PROCEDURE TopoSort( x, y :ARRAY OF CARDINAL; n_pairs :CARDINAL;
                       VAR solution :ARRAY OF CARDINAL; VAR n_solution :CARDINAL;
                       VAR error, sorted :BOOLEAN );
    (*
     This procedure needs some garbage collection added, i've tried but
     will little success. J. Andrea, Dec.18/91
    *)

    TYPE
      LPtr = POINTER TO Leader;
      TPtr = POINTER TO Trailer;

      Leader = RECORD
                  key   :CARDINAL;
                  count :INTEGER;
                  trail :TPtr;
                  next  :LPtr;
               END;

      Trailer = RECORD
                  id   :LPtr;
                  next :TPtr;
                END;

    VAR
      p, q, head, tail :LPtr;
      t                :TPtr;
      i, max_solutions :CARDINAL;

      (* -------------------------------------------- *)
      PROCEDURE Find( w :CARDINAL ) :LPtr;
      VAR h :LPtr;
      BEGIN
        h := head;   tail^.key := w;  (* sentinel *)
        WHILE h^.key # w DO
           h := h^.next;
        END;
        IF h = tail THEN
          NEW( tail );
          n := n + 1;
          h^.count := 0;
          h^.trail := NIL;
          h^.next  := tail;
        END;
        RETURN h;
      END Find;

    BEGIN

        error      := FALSE;
        n_solution := 0;

        IF n_pairs < 2 THEN
          error := TRUE;
        ELSE

          max_solutions := HIGH( solution ) + 1;

          NEW( head );  tail := head;  n := 0;

          (* add all of the given pairs *)

          FOR i := 0 TO n_pairs - 1 DO
            p := Find( x[i] );   q := Find( y[i] );
            NEW(t);
            t^.id    := q;
            t^.next  := p^.trail;
            p^.trail := t;
            q^.count := q^.count + 1;
          END;

          (* search for leaders without predecessors *)

           p := head;  head := NIL;
           WHILE p # tail DO
            q := p;  p := q^.next;
            IF q^.count = 0 THEN
              (* insert q^ in new chain *)
              q^.next := head;   head := q;
            END;
          END;

          (* output phase *)

          q := head;
          WHILE ( NOT error ) & ( q # NIL ) DO
            n_solution := n_solution + 1;
            IF n_solution > max_solutions THEN
              error := TRUE;
            ELSE

              solution[n_solution-1] := q^.key;
              n := n - 1;
              t := q^.trail;  q := q^.next;
              WHILE t # NIL DO
                p := t^.id;  p^.count := p^.count - 1;
                IF p^.count = 0 THEN
                  (* insert p^ in leader list *)
                  p^.next := q;  q := p;
                END;
                t := t^.next;
              END;
            END;
          END;

          IF n # 0 THEN
            sorted := FALSE;
          ELSE
            sorted := TRUE;
          END;

       END;

    END TopoSort;

BEGIN
END Sorting.
