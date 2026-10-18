; 24 char text kernel by AtariAge member c-dw
; https://atariage.com/forums/blogs/entry/4523-13-lines/
; Taken unchanged from 1942 bB (TextKernel, TextPosition).

  ALIGN 256
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;#region rem "Text Screen Kernel"

TextKernel
  ; Alternate each frame
  lda CYCLE
  lsr
  bcs Kernel1A
  jmp Kernel2A
Kernel1A
  sta WSYNC                     ; [0]
  sta HMOVE                     ; [0] + 3       P0A P1A P0B P1B
  lda BUFF1+6                   ; [3] + 3
  sta GRP0                      ; [6] + 3       - - 0 -
  lda BUFF1+13                  ; [9] + 3
  sta GRP1                      ; [12] + 3      0 - - 3   < 36
  lda BUFF1+20                  ; [15] + 3
  sta GRP0                      ; [18] + 3      0 3 5 -   < 42
  SLEEP 6                       ; [21] + 6
  lda #0                        ; [27] + 2
  sta HMP0                      ; [29] + 3
  sta HMP1                      ; [32] + 3
  lda BUFF1+27                  ; [35] + 3
  sta GRP1                      ; [38] + 3      5 3 - 7   > 38 < 47
  lda BUFF1+34                  ; [41] + 3
  sta GRP0                      ; [44] + 3      5 7 9 -   > 44 < 52 
  lda BUFF1+41                  ; [47] + 3
  sta GRP1                      ; [50] + 3      9 7 - 11  > 49 < 58
  sta GRP0                      ; [53] + 3      9 11 - -  > 54 < 63
  SLEEP 15                      ; [56] + 15
  sta HMOVE                     ; [71] + 3      = 74!
  lda BUFF1+2                   ; [74] + 3      P0A P1A P0B P1B 
  sta GRP0                      ; [1] + 3       - - 0 -
  lda BUFF1+9                   ; [4] + 3
  sta GRP1                      ; [7] + 3       0 - - 2   < 34
  lda BUFF1+16                  ; [10] + 3
  sta GRP0                      ; [13] + 3      0 2 4 -   < 39
  SLEEP 9                       ; [16] + 9
  lda #%10000000                ; [25] + 2
  sta HMP0                      ; [27] + 3
  sta HMP1                      ; [30] + 3
  lda BUFF1+23                  ; [33] + 3      
  sta GRP1                      ; [36] + 3      4 2 - 6   > 36 < 44
  lda BUFF1+30                  ; [39] + 3
  sta GRP0                      ; [42] + 3      4 6 8 -   > 41 < 50 
  lda BUFF1+37                  ; [45] + 3
  sta GRP1                      ; [48] + 3      8 6 - 10  > 47 < 55
  sta GRP0                      ; [51] + 3      8 11 - -  > 52 < 60
  ; SPARE 22 CYCLES
  sta WSYNC                     ; [0]
  sta HMOVE                     ; [0] + 3       P0A P1A P0B P1B
  lda BUFF1+5                   ; [3] + 3
  sta GRP0                      ; [6] + 3       - - 0 -
  lda BUFF1+12                  ; [9] + 3
  sta GRP1                      ; [12] + 3      0 - - 3   < 36
  lda BUFF1+19                  ; [15] + 3
  sta GRP0                      ; [18] + 3      0 3 5 -   < 42
  SLEEP 6                       ; [21] + 6
  lda #0                        ; [27] + 2
  sta HMP0                      ; [29] + 3
  sta HMP1                      ; [32] + 3
  lda BUFF1+26                  ; [35] + 3
  sta GRP1                      ; [38] + 3      5 3 - 7   > 38 < 47
  lda BUFF1+33                  ; [41] + 3
  sta GRP0                      ; [44] + 3      5 7 9 -   > 44 < 52 
  lda BUFF1+40                  ; [47] + 3
  sta GRP1                      ; [50] + 3      9 7 - 11  > 49 < 58
  sta GRP0                      ; [53] + 3      9 11 - -  > 54 < 63
  SLEEP 15                      ; [56] + 15
  sta HMOVE                     ; [71] + 3      = 74!
  lda BUFF1+1                   ; [74] + 3      P0A P1A P0B P1B 
  sta GRP0                      ; [1] + 3       - - 0 -
  lda BUFF1+8                   ; [4] + 3
  sta GRP1                      ; [7] + 3       0 - - 2   < 34
  lda BUFF1+15                  ; [10] + 3
  sta GRP0                      ; [13] + 3      0 2 4 -   < 39
  SLEEP 9                       ; [16] + 9
  lda #%10000000                ; [25] + 2
  sta HMP0                      ; [27] + 3
  sta HMP1                      ; [30] + 3
  lda BUFF1+22                  ; [33] + 3      
  sta GRP1                      ; [36] + 3      4 2 - 6   > 36 < 44
  lda BUFF1+29                  ; [39] + 3
  sta GRP0                      ; [42] + 3      4 6 8 -   > 41 < 50 
  lda BUFF1+36                  ; [45] + 3
  sta GRP1                      ; [48] + 3      8 6 - 10  > 47 < 55
  sta GRP0                      ; [51] + 3      8 11 - -  > 52 < 60
  sta WSYNC                     ; [0]
  sta HMOVE                     ; [0] + 3       P0A P1A P0B P1B
  lda BUFF1+4                   ; [3] + 3
  sta GRP0                      ; [6] + 3       - - 0 -
  lda BUFF1+11                  ; [9] + 3
  sta GRP1                      ; [12] + 3      0 - - 3   < 36
  lda BUFF1+18                  ; [15] + 3
  sta GRP0                      ; [18] + 3      0 3 5 -   < 42
  SLEEP 6                       ; [21] + 6
  lda #0                        ; [27] + 2
  sta HMP0                      ; [29] + 3
  sta HMP1                      ; [32] + 3
  lda BUFF1+25                  ; [35] + 3
  sta GRP1                      ; [38] + 3      5 3 - 7   > 38 < 47
  lda BUFF1+32                  ; [41] + 3
  sta GRP0                      ; [44] + 3      5 7 9 -   > 44 < 52 
  lda BUFF1+39                  ; [47] + 3
  sta GRP1                      ; [50] + 3      9 7 - 11  > 49 < 58
  sta GRP0                      ; [53] + 3      9 11 - -  > 54 < 63
  SLEEP 15                      ; [56] + 15
  sta HMOVE                     ; [71] + 3      = 74!
  lda BUFF1+0                   ; [74] + 3      P0A P1A P0B P1B 
  sta GRP0                      ; [1] + 3       - - 0 -
  lda BUFF1+7                   ; [4] + 3
  sta GRP1                      ; [7] + 3       0 - - 2   < 34
  lda BUFF1+14                  ; [10] + 3
  sta GRP0                      ; [13] + 3      0 2 4 -   < 39
  SLEEP 6                       ; [16] + 6
  jmp Kernel1B                  ; [22] + 3
EndKernel1A

  if (>TextKernel != >EndKernel1A)
    echo "WARNING: Kernel1A Crosses Page Boundary!"
  endif

  ALIGN 256
  
Kernel1B
  lda #%10000000                ; [25] + 2
  sta HMP0                      ; [27] + 3
  sta HMP1                      ; [30] + 3
  lda BUFF1+21                  ; [33] + 3      
  sta GRP1                      ; [36] + 3      4 2 - 6   > 36 < 44
  lda BUFF1+28                  ; [39] + 3
  sta GRP0                      ; [42] + 3      4 6 8 -   > 41 < 50 
  lda BUFF1+35                  ; [45] + 3
  sta GRP1                      ; [48] + 3      8 6 - 10  > 47 < 55
  sta GRP0                      ; [51] + 3      8 11 - -  > 52 < 60
  ; SPARE 22 CYCLES
  sta WSYNC                     ; [0]
  sta HMOVE                     ; [0] + 3       P0A P1A P0B P1B
  lda BUFF1+3                   ; [3] + 3
  sta GRP0                      ; [6] + 3       - - 0 -
  lda BUFF1+10                  ; [9] + 3
  sta GRP1                      ; [12] + 3      0 - - 3   < 36
  lda BUFF1+17                  ; [15] + 3
  sta GRP0                      ; [18] + 3      0 3 5 -   < 42
  SLEEP 6                       ; [21] + 6
  lda #0                        ; [27] + 2
  sta HMP0                      ; [29] + 3
  sta HMP1                      ; [32] + 3
  lda BUFF1+24                  ; [35] + 3
  sta GRP1                      ; [38] + 3      5 3 - 7   > 38 < 47
  lda BUFF1+31                  ; [41] + 3
  sta GRP0                      ; [44] + 3      5 7 9 -   > 44 < 52 
  lda BUFF1+38                  ; [47] + 3
  sta GRP1                      ; [50] + 3      9 7 - 11  > 49 < 58
  sta GRP0                      ; [53] + 3      9 11 - -  > 54 < 63
EndKernel1B
  rts
  
Kernel2A
  sta WSYNC                     ; [0]
  lda BUFF1+3                   ; [0] + 3      P0A P1A P0B P1B 
  sta GRP0                      ; [3] + 3       - - 0 -
  lda BUFF1+10                  ; [6] + 3
  sta GRP1                      ; [9] + 3       0 - - 2   < 34
  lda BUFF1+17                  ; [12] + 3
  sta GRP0                      ; [15] + 3      0 2 4 -   < 39
  SLEEP 7                       ; [18] + 7
  lda #%10000000                ; [25] + 2
  sta HMP0                      ; [27] + 3
  sta HMP1                      ; [30] + 3
  lda BUFF1+24                  ; [33] + 3
  sta GRP1                      ; [36] + 3      4 2 - 6   > 36 < 44
  lda BUFF1+31                  ; [39] + 3
  sta GRP0                      ; [42] + 3      4 6 8 -   > 41 < 50 
  lda BUFF1+38                  ; [45] + 3
  sta GRP1                      ; [48] + 3      8 6 - 10  > 47 < 55
  sta GRP0                      ; [51] + 3      8 11 - -  > 52 < 60
  sta WSYNC                     ; [0]
  sta HMOVE                     ; [0] + 3       P0A P1A P0B P1B
  lda BUFF1+6                   ; [3] + 3
  sta GRP0                      ; [6] + 3       - - 0 -
  lda BUFF1+13                  ; [9] + 3
  sta GRP1                      ; [12] + 3      0 - - 3   < 36
  lda BUFF1+20                  ; [15] + 3
  sta GRP0                      ; [18] + 3      0 3 5 -   < 42
  SLEEP 15                      ; [21] + 15
  lda BUFF1+27                  ; [36] + 3
  sta GRP1                      ; [39] + 3      5 3 - 7   > 38 < 47
  lda BUFF1+34                  ; [42] + 3
  sta GRP0                      ; [45] + 3      5 7 9 -   > 44 < 52
  lda BUFF1+41                  ; [48] + 3
  sta GRP1                      ; [51] + 3      9 7 - 11  > 49 < 58
  sta GRP0                      ; [54] + 3      9 11 - -  > 54 < 63
  lda #0                        ; [57] + 2
  sta HMP0                      ; [59] + 3
  sta HMP1                      ; [62] + 3
  SLEEP 6                       ; [65] + 6
  sta HMOVE                     ; [71] + 3      = 74!   
  lda BUFF1+2                   ; [74] + 3      P0A P1A P0B P1B 
  sta GRP0                      ; [1] + 3       - - 0 -
  lda BUFF1+9                   ; [4] + 3
  sta GRP1                      ; [7] + 3       0 - - 2   < 34
  lda BUFF1+16                  ; [10] + 3
  sta GRP0                      ; [13] + 3      0 2 4 -   < 39
  SLEEP 9                       ; [16] + 9
  lda #%10000000                ; [25] + 2
  sta HMP0                      ; [27] + 3
  sta HMP1                      ; [30] + 3
  lda BUFF1+23                  ; [33] + 3
  sta GRP1                      ; [36] + 3      4 2 - 6   > 36 < 44
  lda BUFF1+30                  ; [39] + 3
  sta GRP0                      ; [42] + 3      4 6 8 -   > 41 < 50 
  lda BUFF1+37                  ; [45] + 3
  sta GRP1                      ; [48] + 3      8 6 - 10  > 47 < 55
  sta GRP0                      ; [51] + 3      8 11 - -  > 52 < 60
  sta WSYNC                     ; [0]
  sta HMOVE                     ; [0] + 3       P0A P1A P0B P1B
  lda BUFF1+5                   ; [3] + 3
  sta GRP0                      ; [6] + 3       - - 0 -
  lda BUFF1+12                  ; [9] + 3
  sta GRP1                      ; [12] + 3      0 - - 3   < 36
  lda BUFF1+19                  ; [15] + 3
  sta GRP0                      ; [18] + 3      0 3 5 -   < 42
  SLEEP 15                      ; [21] + 15
  lda BUFF1+26                  ; [36] + 3
  sta GRP1                      ; [39] + 3      5 3 - 7   > 38 < 47
  lda BUFF1+33                  ; [42] + 3
  sta GRP0                      ; [45] + 3      5 7 9 -   > 44 < 52
  lda BUFF1+40                  ; [48] + 3
  sta GRP1                      ; [51] + 3      9 7 - 11  > 49 < 58
  sta GRP0                      ; [54] + 3      9 11 - -  > 54 < 63
  lda #0                        ; [57] + 2
  sta HMP0                      ; [59] + 3
  sta HMP1                      ; [62] + 3
  SLEEP 3                       ; [65] + 3
  jmp Kernel2B                  ; [68] + 3
EndKernel2A
  
  if (>Kernel1B != >EndKernel2A)
    echo "WARNING: Kernel1B/2A Crosses Page Boundary!"
  endif

  ALIGN 256

Kernel2B
  sta HMOVE                     ; [71] + 3      = 74!   
  lda BUFF1+1                   ; [74] + 3      P0A P1A P0B P1B 
  sta GRP0                      ; [1] + 3       - - 0 -
  lda BUFF1+8                   ; [4] + 3
  sta GRP1                      ; [7] + 3       0 - - 2   < 34
  lda BUFF1+15                  ; [10] + 3
  sta GRP0                      ; [13] + 3      0 2 4 -   < 39
  SLEEP 9                       ; [16] + 9
  lda #%10000000                ; [25] + 2
  sta HMP0                      ; [27] + 3
  sta HMP1                      ; [30] + 3
  lda BUFF1+22                  ; [33] + 3
  sta GRP1                      ; [36] + 3      4 2 - 6   > 36 < 44
  lda BUFF1+29                  ; [39] + 3
  sta GRP0                      ; [42] + 3      4 6 8 -   > 41 < 50 
  lda BUFF1+36                  ; [45] + 3
  sta GRP1                      ; [48] + 3      8 6 - 10  > 47 < 55
  sta GRP0                      ; [51] + 3      8 11 - -  > 52 < 60
  sta WSYNC                     ; [0]
  sta HMOVE                     ; [0] + 3       P0A P1A P0B P1B
  lda BUFF1+4                   ; [3] + 3
  sta GRP0                      ; [6] + 3       - - 0 -
  lda BUFF1+11                  ; [9] + 3
  sta GRP1                      ; [12] + 3      0 - - 3   < 36
  lda BUFF1+18                  ; [15] + 3
  sta GRP0                      ; [18] + 3      0 3 5 -   < 42
  SLEEP 15                      ; [21] + 15
  lda BUFF1+25                  ; [36] + 3
  sta GRP1                      ; [39] + 3      5 3 - 7   > 38 < 47
  lda BUFF1+32                  ; [42] + 3
  sta GRP0                      ; [45] + 3      5 7 9 -   > 44 < 52
  lda BUFF1+39                  ; [48] + 3
  sta GRP1                      ; [51] + 3      9 7 - 11  > 49 < 58
  sta GRP0                      ; [54] + 3      9 11 - -  > 54 < 63
  lda #0                        ; [57] + 2
  sta HMP0                      ; [59] + 3
  sta HMP1                      ; [62] + 3
  SLEEP 6                       ; [65] + 6
  sta HMOVE                     ; [71] + 3      = 74!   
  lda BUFF1+0                   ; [74] + 3      P0A P1A P0B P1B 
  sta GRP0                      ; [1] + 3       - - 0 -
  lda BUFF1+7                   ; [4] + 3
  sta GRP1                      ; [7] + 3       0 - - 2   < 34
  lda BUFF1+14                  ; [10] + 3
  sta GRP0                      ; [13] + 3      0 2 4 -   < 39
  SLEEP 9                       ; [16] + 9
  lda #%10000000                ; [25] + 2
  sta HMP0                      ; [27] + 3
  sta HMP1                      ; [30] + 3
  lda BUFF1+21                  ; [33] + 3
  sta GRP1                      ; [36] + 3      4 2 - 6   > 36 < 44
  lda BUFF1+28                  ; [39] + 3
  sta GRP0                      ; [42] + 3      4 6 8 -   > 41 < 50 
  lda BUFF1+35                  ; [45] + 3
  sta GRP1                      ; [48] + 3      8 6 - 10  > 47 < 55
  sta GRP0                      ; [51] + 3      8 11 - -  > 52 < 60
EndKernel2B
  rts
  
  ; Set Initial Sprite Positions
TextPosition
  lda CYCLE
  lsr
  bcs Position1
  jmp Position2
Position1
  sta WSYNC
  lda #%10010000                ; [0] + 2
  sta HMP0                      ; [2] + 3
  lda #%10000000                ; [5] + 2
  sta HMP1                      ; [7] + 3
  SLEEP 19                      ; [10] + 19
  sta RESP0                     ; [29] + 3 = 32
  nop                           ; [32] + 2
  sta RESP1                     ; [34] + 3 = 37
  rts
Position2
  sta WSYNC
  SLEEP 29                      ; [0] + 29
  sta RESP0                     ; [29] + 3 = 32
  nop                           ; [32] + 2
  sta RESP1                     ; [34] + 3 = 37
  lda #%10010000                ; [37] + 2
  sta HMP0                      ; [39] + 3
  lda #%10000000                ; [42] + 2
  sta HMP1                      ; [44] + 3 
  SLEEP 24                      ; [47] + 21
  sta HMOVE                     ; [71] + 3
  rts
EndPosition

  if (>Kernel2B != >EndPosition)
    echo "WARNING: Kernel2B Crosses Page Boundary!"
  endif
