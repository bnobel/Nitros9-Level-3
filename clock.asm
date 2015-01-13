* Disassembled 1915/01/06 22:24:51 by Disasm v1.5 (C) 1988 by RML

         ifp1
         use   defsfile
         endc

tylg     set   Systm+Objct
atrv     set   ReEnt+rev
rev      equ   4

         mod   eom,name,tylg,atrv,start,size

size     equ   65360

name     fcs   /Clock/
         fcb   $0A

l0013    fcb   F$Time
         fdb   $01fc
         fcb   F$VIRQ
         fdb   $0170
         fcb   F$Alarm
         fdb   $01bf
         fcb   F$STime
         fdb   $0207
         fcb   $80

L0020    lda   >$FF92              get GIME IRQ status
         ora   <D.IRQS
         sta   <D.IRQS
         bita  #$08                clock interrupt?
         beq   L0035
         anda  #$F7                drop clock bit
         sta   <D.IRQS
         ldx   <D.VIRQ             set VIRQ
         clr   <D.QIRQ
         bra   L003C

L0035    leax  >L0121,pc           if not clock IRQ, just poll IRQ source
         oim   #$FF,<D.QIRQ        set flag to non clock IRQ
L003C    stx   <D.SvcIRQ
         jmp   [>D.XIRQ]           chain through kernel to continue IRQ

L0042    ldb   >$0643
         clra  
         pshs  b,a
         ldy   <D.CLTb
         bra   L0077

L004D    ldd   $02,y
         orcc  #$50
         sta   >$0643
         stb   >$0645
         std   >$FFA1
         andcc #$AF
         ldd   ,x
         decd  
         bne   L0073
         lda   $04,x
         bmi   L0069
         lbsr  L01CD
L0069    ora   #$01
         sta   $04,x
         lda   #$80
         sta   ,s
         ldd   $02,x
L0073    std   ,x
         leay  $04,y
L0077    ldx   ,y
         bne   L004D
         puls  b,a
         orcc  #$50
         stb   >$0643
         stb   >$FFA1
         incb  
         stb   >$0645
         stb   >$FFA1
         andcc #$AF
         ora   <D.IRQS
         bita  #$B7
         beq   L0099
         lbsr  L0121
         bra   L009C

L0099    lbsr  L013E
L009C    lda   >$0643
         ldb   >$0645
         pshs  b,a
         orcc  #$50
         lda   >$0660
         sta   >$0643
         sta   >$FFA1
         inca  
         sta   >$0645
         sta   >$FFA2
         jsr   [>D.AltIRQ]
         puls  b,a
         orcc  #$50
         sta   >$0643
         stb   >$0645
         std   >$FFA1
         andcc #$AF
         dec   <D.Tick
         bne   L011D
         lda   #$32
         sta   <D.Tick
         inc   <D.Sec
         lda   <D.Sec
         cmpa  #$3C
         bcs   L011D
         clr   <D.Sec
         lbsr  L0154
         ldd   >$1015
         ble   L011D
         ldd   >$1012
         cmpd  <D.Hour
         bne   L011D
         ldd   >$1010
         cmpd  <D.Month
         bne   L011D
         ldb   >$100F
         cmpb  <D.Year
         bne   L011D
         ldd   >$1015
         cmpd  #$0001
         beq   L0108
         os9   F$Send   
         bra   L010E

L0108    ldb   <D.Sec
         andb  #$F0
         beq   L0116
L010E    ldd   #$FFFF
         std   >$1015
         bra   L011D
L0116    ldx   >$1017
         beq   L011D
         jsr   ,x
L011D    jmp   [>D.Clock]

L0121    lda   >$0643
         ldb   >$0645
         pshs  b,a
L0129    jsr   [>D.Poll]
         bcc   L0129
         puls  b,a
         orcc  #$50
         sta   >$0643
         stb   >$0645
         std   >$FFA1
         andcc #$AF
L013E    lda   #$FE
         anda  <D.IRQS
         sta   <D.IRQS
         lda   <D.IRQER
         tfr   a,b
         anda  #$FE
         orb   #$01
         sta   >$FF92
         stb   >$FF92
         clrb  
         rts   

L0154    ldx   >$000B,pcr
         ldy   #$0028
         ldb   #$0B
         bsr   L016D
         bsr   L016D
         bsr   L016D
         lda   #$03
         bsr   L016F
         bsr   L016D
         bsr   L016D
         rts   

L016D    lda   #$0F
L016F    stb   $01,x
         decb  
         anda  ,x
         pshs  b
         ldb   #$0A
         mul   
         stb   ,y
         puls  b
         stb   $01,x
         decb  
         lda   ,x
         anda  #$0F
         adda  ,y
         sta   ,y+
         rts   

F.VIRQ   pshs  cc
         orcc  #IntMasks
         ldy   <D.CLTb
         ldx   <D.Init
         ldb   PollCnt,x
         ldx   R$X,u
         beq   L01BF
         bra   L019C

         leay  $04,y
L019C    ldx   ,y
         beq   L01A9
         decb  
         bne   L019C
         puls  cc
         comb  
         ldb   #$CA
         rts   

L01A9    ldx   R$Y,u
         stx   ,y
         lda   >$0643
         ldb   >$0645
         std   $02,y
         ldy   R$D,u
         sty   ,x
         bra   L01C9
         leay  $04,y
L01BF    ldx   ,y
         beq   L01C9
         cmpx  R$Y,u
         bne   L01BF
         bsr   L01CD
L01C9    puls  cc
         clrb  
         rts   

L01CD    pshs  y,x
L01CF    ldq   ,y++
         leay  $02,y
         stq   -$08,y
         bne   L01CF
         puls  pc,y,x

F.Alarm  ldx   #$100F
         ldd   R$D,u
         bne   L01E5
         std   $06,x
         rts   

L01E5    tsta  
         bne   L01EE
         cmpd  #$0001
         bne   L0202
L01EE    std   $06,x
         ldy   <D.Proc
         lda   P$Task,y
         ldb   <D.SysTsk
         ldx   R$X,u
         ldu   #$100F
L01FC    ldy   #$0005
         bra   L0222
L0202    cmpd  #$0002
         bne   L020E
         ldd   $06,x
         std   u0001,u
         bra   L0215
L020E    comb  
         ldb   #$BB
         rts   

F.Time   ldx   #D.Time
L0215    ldy   <D.Proc
         ldb   P$Task,y
         lda   <D.SysTsk
         ldu   R$X,u
L021E    ldy   #$0006
L0222    os9   F$Move   
         rts   

F.STime  ldx   <D.Proc
         lda   P$Task,x
         ldx   R$X,u
         ldu   #D.Time
         ldb   <D.SysTsk
         bsr   L021E
         lda   #TkPerSec
         sta   <D.Tick
         pshs  cc
         orcc  #IntMasks
         ldy   #$002E
         ldx   >$000B,pc
         clrb  
         bsr   L0254
         bsr   L0254
         bsr   L0254
         bsr   L0254
         bsr   L0254
         bsr   L0254
         puls  cc
         clrb  
         rts   

L0254    clr   ,-s
         lda   ,-y
L0258    suba  #$0A
         bcs   L0260
         inc   ,s
         bra   L0258

L0260    adda  #$0A
         stb   $01,x
         incb  
         sta   ,x
         stb   $01,x
         incb  
         puls  a
         sta   ,x
         rts   

start    ldx   #$FF00         point to PIA0
         clra                 no error for return
         pshs  cc             save IRQ enable status
         orcc  #IntMasks      stop interupts
         ldb   <D.VIDMD       get video mode register copy
         orb   #$08           set 50hz VSYNC bit
         stb   <D.VIDMD
         stb   >$FF98
         sta   $01,x          enable DDRA
         sta   ,x             set port A to all inputs
         sta   $03,x          enable DDRB
         coma                 set all ports to outputs
         sta   $02,x
         ldd   #$343C         [A]=PIA0 CRA contents, [B]=PIA0 CRB contents
         sta   $01,x          CA2 (MUX0) out low, port A, disable HBORD high to low IRQs
         stb   $03,x          CB2 (MUX1) out low, port B, disable VBORD low to high IRQs
         lda   ,x             clear possible pending PIA0 HBORD IRQ
         lda   $02,x          clear possible pending PIA0 VBORD IRQ
         ldd   #$3B02
         std   <D.Sec
         stb   <D.TSlice
         stb   <D.Slice
         leax  >L0020,pc
         stx   <D.IRQ
         leax  >L0042,pc
         stx   <D.VIRQ
         leay  >L0013,pc
         os9   F$SSvc   
         lda   <D.IRQER
         ora   #$08
         sta   <D.IRQER
         sta   >$FF92
         ldx   >$000B,pc
         ldd   #$010F
         stb   $01,x
         sta   ,x
         ldd   #$0504
         sta   ,x
         stb   ,x
         puls  pc,cc

         emod

eom      equ   *

         end
