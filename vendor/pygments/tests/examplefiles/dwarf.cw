;redcode
;name          Dwarf
;author        A. K. Dewdney
;version       94.1
;date          April 29, 1993
;strategy      Bombs every fourth instruction.
        ORG     start              ; Indicates the instruction with
                                   ; the label "start" should be the
                                   ; first to execute.
step    EQU      4                 ; Replaces all occurrences of "step"
                                   ; with the character "4".
target  DAT.F   #0,     #0         ; Pointer to target instruction.
start   ADD.AB  #step,   target    ; Increments pointer by step.
        MOV.AB  #0,     @target    ; Bombs target instruction.
        JMP.A    start             ; Same as JMP.A -2.  Loops back to
                                   ; the instruction labelled "start".
        END
