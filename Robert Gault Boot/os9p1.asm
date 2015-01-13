         nam   OS9p1
         ttl   os9 system module    

* Disassembled 2014/11/20 19:45:13 by Disasm v1.5 (C) 1988 by RML


         ifp1
         use   defsfile
         endc

tylg     set   Systm+$00
atrv     set   ReEnt+rev
rev      set   $06

         mod   eom,name,tylg,atrv,start,size

u0000    rmb   0
size     equ   .

name     equ   *
         fcs   /OS9p1/

         fcb   $13 
         fcc   /0123456789ABCDEF/
         fcc   /0123456789ABCDEF/
         fcc   /0123456789ABCDEF/
         fcc   /012345/

* Dispatch table.
DisTable equ   *
L0049    fdb   $fcad            D.Clock
         fdb   $f240            D.XSWI3
         fdb   $f24e            D.XSWI2
         fdb   D.Crash          D.XFIRQ crash on FIRQ should be $006B
         fdb   $fd0c            D.XIRQ
         fdb   $f243            D.XSWI
         fdb   D.Crash          D.XNMI crash on an NMI ie. $006B
         fdb   $0055            D.ErrRst ??? Not used as far as I can tell
         fdb   $f309            Initial Kernel system call vector
DisSize  equ   *-DisTable

LowSub   equ   $0160
SubStrt  equ   *
R.Flip0  equ   *
L005B    aim   #$FE,<D.TINIT    map type 0 <$0091
         lde   <D.TINIT         another 2 bytes saved if GRFDRV does: tfr cc,e
         ste   >DAT.Task        and we can use A here, instead of E >$FF91
         clr   <D.SSTskN        <$A4
         tfr   x,s
         tfr   a,cc
         rts
SubSiz   equ   *-SubStrt	
L006C    jmp   [<-(D.SWI3-D.XSWI3),x]    (-$10) (Jmp to 2ndary vector)

* Let's start by initializing system page
start    ldq   #$01001F00       start address to clear & # bytes to clear
         leay  <start+2,pc      point to 0
         tfm   y,d+
         std   <D.CCStk         set pointer to top of global memory to $2000
         lda   #$01             set task user table to $0100
         std   <D.Tasks
         addb  #$20             set task image table to $0120
         std   <D.TskIPt
         clrb                   set memory block map to $0200
         inca  
         std   <D.BlkMap
         addb  #$40             set second block map to $0240
         std   <D.BlkMap+2
         clrb                   set system service dispatch table to $0300
         inca
         std   <D.SysDis
         inca                   set user dispatch table to $0400
         std   <D.UsrDis
         inca                   set process descriptor block to $0500
         std   <D.PrcDBT
         inca                   set system process descriptor to $0600
         std   <D.SysPrc
         std   <D.Proc          set user process descriptor to $0600
         adda  #$02             set stack pointer to $0800
         tfr   d,s
         inca                   set system stack to $0900
         std   <D.SysStk
         std   <D.SysMem        set system memory map ptr $0900
         inca                   set module directory start to $0a00
         std   <D.ModDir
         std   <D.ModEnd        set module directory end to $0a00
         adda  #$06             set secondary module directory start to $1000
         std   <D.ModDir+2
         std   <D.ModDAT        set module directory DAT pointer to $1000
         std   <D.CCMem	set pointer to beginning of global memory to $1000
* In following line, CRC=ON if it is STA <D.CRC, CRC=OFF if it is a STB <D.CRC
         stb   <D.CRC           set CRC checking flag to off
		
* Initialize interrupt vector tables
         leay	<DisTable,pc    point to table of absolute vector addresses
         ldx	#D.Clock        where to put it in memory
         ldf   #DisSize         $12             size of data
         tfm   y+,x+            move it over
         ldu   #LowSub          $0160           somewhere in block 0 that's never modified
         stu   <D.Flip0         switch to system task 0
         ldf   #SubSiz          $11             size of it
         tfm   y+,u+            copy it over
         tfr   y,u              move pointer to a faster register
L00CC    stu   ,x++             set all IRQ vectors to go to Vectors for now
         cmpx  #D.NMI
         bls   L00CC

* Initialize user interrupt vectors
         ldx   <D.XSWI2         get SWI2 (os9 command) service routine pointer
         stx   <D.UsrSvc        save it as user service routine pointer
         ldx   <D.XIRQ          get IRQ service routine pointer
         stx   <D.UsrIRQ        save it as user IRQ routine pointer
         leax  >L02ED,pc        setup system service routine entry vector
         stx   <D.SysSvc
         stx   <D.XSWI2
         leax  >SysIRQ,pc       setup system IRQ service vector?? leax L0E05,pcr
         stx   <D.SysIRQ
         stx   <D.XIRQ
         leax  >L0E01,pc        setup in system IRQ Service
         stx   <D.SvcIRQ
         leax  >L053C,pc        setup interrupt polling vector
         stx   <D.Poll
         leax  >L01C3,pc        setup alternate IRQ vector: pts to an rts
         stx   <D.AltIRQ
         lda   #'K              ' in kernal; mine has '1 instead of 'K
         jsr   <D.BtBug
         leax  >L0E5D,pc        setup change to task 1 vector
         stx   <D.Flip1

* Setup System calls
         leay  >L01D3,pcr
         lbsr  L034D

* Initialize system process descriptor
         ldu   <D.PrcDBT        get process table pointer
         ldx   <D.SysPrc        get system process pointer

* These overlap because it is quicker than trying to strip hi byte from X
         stx   ,u               save it as the first process in table
         stx   1,u              save it as the second as well
         oim   #$01,P$ID,x      set process ID to 1 (inited to 0)
         oim   #SysState,P$State,x    set to system state
         clra                   system task is task 0
         sta   <D.SysTsk
         sta   P$Task,x
         coma                   setup its priority and age ($FF)
         sta   P$Prior,x
         sta   $0b,x
         leax  <P$DATImg,x
         stx   <D.SysDAT        save it as a pointer in DP
         clrd  
         std   ,x++
* Dat.BlCt-ROMCount-RAMCount
         lda   #$06             initialize the rest of the blocks to be free
         ldu   #DAT.Free
L0134    stu   ,x++
         deca  
         bne   L0134

         ldu   #$003F           block $3f is in use, at the top of system DAT image
         stu   ,x
         ldx   <D.Tasks         point to task user table
         inc   ,x               mark first 2 in use (system & GrfDrv)
         inc   1,x

* Setup system memory map
         ldx   <D.SysMem
         ldb   <D.CCStk
L0148    inc   ,x+
         decb  
         bne   L0148

* Calculate memory size
         ldx   <D.BlkMap         get pointer to 8k block map
         inc   <$3F,x            mark block $3F as used (kernel)
         ldq   #$00080100        E=marker, D=Block # to check
L0157    asld                    get next block #
         stb   >$FFA5            map block into block 6 of my task
         ste   >-$6000,x         save marker to that block
         cmpe  ,x                did it ghost to block 0?
         bne   L0157             no, keep going until ghost found
         stb   <D.MemSz          save # of 8k mem blocks that exist
         addr  x,d               add number of blocks to block map start
         std   <D.BlkMap+2       save block map end pointer

* [D] at this point will contain 1 of the following:
* $0210 - 128k
* $0220 - 256k
* $0240 - 512k
* $0280 - 1024k
* $0300 - 2048k
         bitb  #$30              block above 128-256k?
         beq   L0180             yes no need to mark block map
         tstb                    2 meg?
         beq   L0180             yes skip this
         abx                     add maximum block # to block map start
         leax  -1,x              skip good blocks that are RAM
         lda   #NotRam           not ram flag
         subb  #$3F              calculate # blocks to mark as not ram
L017B    sta   ,x+               mark them all
         decb  
         bne   L017B

L0180    ldx   #$ED00
         lda   #$12
         lbsr  L08E0
         bsr   L01AE

*see if Init module is in memory already
L018A    leax  <L01CA,pcr
         bsr   L01C4
         bcc   L0198
         os9   F$Boot
         bcc   L018A
         bra   L01AA
L0198    stu   <D.Init
         lda   #'i
         jsr   <D.BtBug
* try calling os9p2
L019E    leax  <L01CE,pcr
         bsr   L01C4
         bcc   L01AC
         os9   F$Boot 
         bcc   L019E
L01AA    jmp   <D.Crash
L01AC    jmp   ,y

*Mark kernel in system memory map as used memory (256 byte blocks)
L01AE    ldx   <D.SysMem
         ldd   #$80ED
         abx   
         comb  
         sta   b,x
L01B7    lda   #$01
L01B9    sta   ,x+
         decb  
         bne   L01B9
         ldx   <D.BlkMap
         sta   <$3F,x
L01C3    rts   

*Link module pointed to by X
L01C4    lda   #$C0
         os9   F$Link   
         rts   

L01CA    fcs   /Init/
L01CE    fcs   /Krnp2/

L01D3    fcb   F$Link
         fdb   $018b
         fcb   F$PrsNam
         fdb   $0506
         fcb   $11              F$CmpName
         fdb   $0593
         fcb   $91              F$CmpNam (sys state)
         fdb   $059b
         fcb   $17
         fdb   $0409
         fcb   $a8
         fdb   $05f2
         fcb   $a9
         fdb   $0646
         fcb   $ac
         fdb   $0af9
         fcb   $ad
         fdb   $0b75
         fcb   $ae
         fdb   $022e
         fcb   $b2
         fdb   $0142
         fcb   $b4
         fdb   $015d
         fcb   $b5
         fdb   $0690
         fcb   $b6
         fdb   $05da
         fcb   $1b
         fdb   $08f8
         fcb   $b8
         fdb   $0943
         fcb   $39
         fdb   $072a
         fcb   $ba
         fdb   $0791
         fcb   $bc
         fdb   $084e
         fcb   $bd
         fdb   $0838
         fcb   $be
         fdb   $07ef
         fcb   $bf
         fdb   $0a2c
         fcb   $c0
         fdb   $0a39
         fcb   $c1
         fdb   $0a46
         fcb   $c2
         fdb   $0a64
         fcb   $c3
         fdb   $0a7f
         fcb   $c4
         fdb   $0851
         fcb   $c6
         fdb   $0867
         fcb   $c8
         fdb   $08a2
         fcb   $c9
         fdb   $09de
         fcb   $ca
         fdb   $09f6
         fcb   $cd
         fdb   $0126
         fcb   $ce
         fdb   $03f5
         fcb   $d3
         fdb   $0725
         fcb   $d7
         fdb   $06a0
         fcb   $51
         fdb   $0736

         fcb   $80

* SWI3 vector entry
L0240    lda   #P$SWI3          get pointer to dwi3 vector
L0242    fcb   $8c              skip 2 bytes

*SWI vector entry
L0243    lda   #P$SWI           get pointer to swi vector
         ldx   <D.Proc          get process pointer
         ldu   a,x              user defined swi[x]?
         beq   L0255            yes, go call users routine
L024B    lbra  L0E53

*SWI2 vector entry
         ldx   <D.Proc          get current process descriptor
         ldu   <P$SWI2,x        any SWI2 vector?
         bne   L024B            yes, ???

* Process software interrupts from a user state
* Entry: X=Process descriptor pointer of process that made the system call
*        U=Register stack pointer
L0255    ldu   <D.SysSvc        set system call professor to system side
L0257    stu   <D.XSWI2          ?? <u00E4 mine <u00F4 here
         ldu   <D.SysIRQ        do the same for IRQ's
         stu   <D.XIRQ
         oim   #SysState,P$State,x
* copy register stack to process descripor
         sts   P$SP,x
         leas  (P$Stack-R$Size),x
         leau  R$Size-1,s
         leay  -1,y
         ldw   #R$Size
         tfm   y-,u-

* next 2 lines are swapped vertically in krnp1 (probably for interrupt timing)
         andcc #^IntMasks
         leau  ,s               needed because TFM is u-, not -u (post not pre)

* B=function code already from calling process: DON'T USE IT!
         ldx   R$PC,u           get where PC was from process
         leax  1,x              skip option byte
         stx   R$PC,u           save updated PC to process
* execute function call
         ldy   <D.UsrDis        get user dispatch table pointer
         lbsr  L030B            go execute option
         aim   #^IntMasks,R$CC,u    clear interupt flags in caller's CC
         ldx   <D.Proc          get process descriptor
         aim   #^(SysState+TimOut),P$State,x   clear system & timeout flags

* Check for image change now, which lets stuff like F$MapBlk an F$ClrBlk
* do the short-circuit thing, too. adds about 20 cycles to each system call.
         lbsr  L0C5B
         lda   P$State,x        get current state of the process
         ora   P$Signal,x       is there a pending signal?
         sta   <D.Quick         save quick return flag
         beq   L029B

L0296    bsr   L02C1            move the stack frame back to user state
         lbra  L0D61            go back to the process

L029B    inc   <D.QCnt          increment quick number of system calls
         aim   #$1F,<D.QCnt     base 32?
         beq   L0296            yes every 32 system calls do a full check

         ldw   #$000E
         ldy   #$FEDF
         orcc  #IntMasks
         tfm   u+,y+
         lbra  L0DD0

* copy register stack from user to system
L02B2    pshs  u,y,x,cc
         ldb   P$Task,x
         ldx   P$SP,x
         lbsr  L0BF5
         leax  >-$6000,x
L02BF    bra   L02D0

* copy register stack from system to user
L02C1    pshs  u,y,x,cc
         ldb   P$Task,x
         ldx   P$SP,x
         lbsr  L0BF5
         leax  >-$6000,x
L02CE    exg   x,y
* copy a register stack
* Entry: X=source
*        Y=destination
*        A=offset into DAT image of block
*        B=task #
L02D0    leau  a,u
         lda   $01,u
         ldb   $03,u
         orcc  #$50
         std   >$FFA5
         ldw   #R$Size
         tfm   x+,y+
         ldx   <$004C
         lda   $0B,x
         ldb   $0D,x
         std   >$FFA5
         puls  pc,u,y,x,cc

* Process software interupts from system state
* Entry: U=Register stack pointer
L02ED    leau  ,s
         lda   <D.SSTskN
         clr   <D.SSTskN
         pshs  a
         lda   ,u
         tfr   a,cc
         ldx   $0C,u
         leax  $01,x
         stx   $0C,u
         ldy   <$00C2
         bsr   L030B
         puls  a
         lbra  L0E27
* Entry: X = system call vector to jump to
         jmp   ,x

* Execute system call
* Entry: B=Function call #
*        Y=Function dispatch table pointer (D.SysDis or D.UsrDis)
L030B    lslb  
         bcc   L032C
         ldx   >IOEntry,y
L0312    pshs  u
L0314    jsr   [D.SysVec]
         puls  u
L031A    tfr   cc,a
         bcc   L0320
         stb   $02,u
L0320    ldb   ,u
         andd  #$2FD0
         orr   b,a
         sta   ,u
         rts   

* Execute regular system calls
L032C    clra  
         ldx   d,y
         bne   L0312
         comb  
         ldb   #$D0
         bra   L031A

* System Call: F$SSVC
*
* Function: Install system calls
*
* Input:  Y = Address of service request init table
*
* Output: None
*
* Error:  CC = C bit set; B = error code
         ldy   $08,u
         bra   L034D
L033B    clra  
         lslb  
         tfr   d,u
         ldd   ,y++
         leax  d,y
         ldd   <$00C2
         stx   d,u
         bcs   L034D
         ldd   <D.UsrDis
         stx   d,u
L034D    ldb   ,y+
         cmpb  #$80
         bne   L033B
         rts   

* System Call: F$SLink
*
* Function: System Link
*
* Input:  A = Module type
*         X = Module name string pointer
*         Y = Name string DAT image pointer
*
* Output: A = Module type
*         B = Module revision
*         X = Updated name string pointer
*         Y = Module entry point
*         U = Module pointer
*
* Error:  CC = C bit set; B = error code
*
L0354    ldy   R$Y,u
         bra   L0366

* System Call: F$ELink
*
* Function: Link using Module directory entry
*
* Input:  B = Module type
*         X = Pointer to module directory entry
*
* Output: None
*
* Error:  CC = C bit set; B = error code
L0359    pshs  u
         ldb   R$B,u
         ldx   R$X,u
         bra   L037B

* System Call: F$Link
*
* Function: Link to a memory module
*
* Input:  X = Address of module name
*         A = Type/Language byte
*
* Output: X = Advanced past module name
*         Y = Module entry address
*         U = Module header address
*         A = Module type/language
*         B = Module attributes/revision byte
*
* Error:  CC = C bite set; B = error code
L0361    ldx   <D.Proc
         leay  <P$DATImg,x
L0366    pshs  u
         ldx   R$X,u
         lda   R$A,u
         lbsr  L063F
         bcs   L03E8
         leay  ,u
         ldu   ,s
         stx   R$X,u
         std   R$D,u
         leax  ,y
L037B    bitb  #ReEnt
         bne   L0387
         ldd   MD$Link,x
         beq   L0387
         ldb   #E$ModBsy
         bra   L03E8
L0387    ldd   MD$MPtr,x
         pshs  x,b,a
         ldy   MD$MPDAT,x
         ldd   MD$MBSiz,x
         addd  #$1FFF
         tfr   a,b
         lsrb  
         lsrb  
         lsrb  
         lsrb  
         lsrb  
         lsra  
         inca  
         lsra  
         lsra  
         lsra  
         lsra  
         pshs  a
         leau  ,y
         bsr   L03EC
         bcc   L03B6
         lda   ,s
         lbsr  L0A0F
         bcc   L03B3
         leas  $05,s
         bra   L03E8
L03B3    lbsr  L0A60
L03B6    ldb   #$80
         abx   
         sta   ,s
         lsla  
         leau  a,x
         ldd   ,u
         incd  
         beq   L03C6
         std   ,u
L03C6    ldu   $03,s
         ldd   MD$Link,u
         incd  
         beq   L03D0
         std   $06,u
L03D0    puls  u,y,x,b
         lbsr  L0A7F
         stx   $0A,u
         ldx   $04,y
         ldy   ,y
         ldd   #$0009
         lbsr  L0AD8
         addd  $0A,u
         std   $08,u
         clrb  
         rts   

L03E8    orcc  #$01
         puls  pc,u

L03EC    ldx   <D.Proc
         leay  <$40,x
         clra  
         pshs  y,x,b,a
         subb  #$08
         negb  
         lslb  
         leay  b,y
L03FA    ldw   ,s
         pshs  u,y
L03FF    ldd   ,y++
         cmpd  ,u++
         bne   L0414
         decw  
         bne   L03FF
         puls  u,b,a
         subd  $04,s
         lsrb  
         stb   ,s
         clrb  
         puls  pc,y,x,b,a

L0414    puls  u,y
         leay  -$02,y
         cmpy  $04,s
         bcc   L03FA
         puls  pc,y,x,b,a


* System Call: F$VModul
*
* Function: Verify a module
*
* Input:  X = Address of module to verify
*
* Output: U = Address of module directory entry
*
* Error:  CC = C bit set; B = error code
L041F    pshs  u
         ldx   $06,u
         ldy   $01,u
         bsr   L042E
         ldx   ,s
         stu   $0A,x          mine is $0A this was $0D,x
         puls  u,pc

L042E    pshs   x,y
         lbsr   L053F
         bcs    L045E
         ldd    #M$Type
         lbsr  L0AD8
         andb  #$0F
         pshs  b,a
         ldd   #$0004
         lbsr  L0AD8
         leax  d,x
         puls  a
         lbsr  L063F
         puls  a
         bcs   L0460
         andb  #$0F
         subr  a,b
         bcs   L0460
         ldb   #$E7
         fcb   $8C
L045A    ldb   #$CE
         orcc  #$01
L045E    puls  pc,y,x

L0460    ldx   ,s
         bsr   L04E2
         bcs   L045A
         sty   ,u
         stx   $04,u
         clrd  
         std   $06,u
         ldd   #$0002
         lbsr  L0AD8
         addr  x,d
         std   $02,u
         ldy   [,u]
         ldx   <$0044
         pshs  u
         fcb   $8C
L0482    leax  8,x
         cmpx  <D.ModEnd
         bcc   L0493
         cmpx  ,s
         beq   L0482
         cmpy  [,x]
         bne   L0482
         bsr   L04B2
L0493    puls  u
         ldx   <D.BlkMap
         ldd   $02,u
         addd  #$1FFF
         lsra  
         lsra  
         lsra  
         lsra  
         lsra  
         ldy   ,u
         tfr   a,e
L04A6    ldd   ,y++
         oim   #$02,d,x
         dece  
         bne   L04A6
         clrb  
         puls  pc,y,x

L04B2    pshs  u,y,x,b,a
         ldx   ,x
         tfr   x,w
         clrd  
L04BA    ldy   ,w
         beq   L04C3
         std   ,w++
         bra   L04BA
L04C3    ldy   $02,s
         ldu   ,u
         puls  b,a
L04CA    cmpx  ,y
         bne   L04D9
         stu   ,y
         cmpd  $02,y
         bcc   L04D7
         ldd   $02,y
L04D7    std   $02,y
L04D9    leay  $08,y
         cmpy  <D.ModEnd
         bne   L04CA
         puls  pc,u,y,x

L04E2    pshs  u,y,x
         ldd   #$0002
         lbsr  L0AD8
         addd  ,s
         addd  #$1FFF
         lsra  
         lsra  
         lsra  
         lsra  
         lsra  
         tfr   a,b
         pshs  b
         comb  
         lslb  
         sex   
         bsr   L050A
         bcc   L0508
         os9   F$GCMDir 
         tfr   0,u
         stu   $05,s
         bsr   L050A
L0508    puls  pc,u,y,x,b

L050A    ldx   <D.ModDAT
         leax  d,x
         cmpx  <D.ModEnd
         bcs   L053C
         ldu   $07,s
         bne   L0528
         ldy   <D.ModEnd
         leay  $08,y
         cmpr  x,y
         bhi   L053C
         sty   <D.ModEnd
         leay  -$08,y
         sty   $07,s
L0528    stx   <D.ModDAT
         ldd   $05,s
         stx   $05,s
         ldf   $02,s
         clre  
         rolw  
         tfm   d+,x+
         stw   ,x
         rts   

* Default interrupt handling routine on first booting OS9p1
L053C    orcc  #$01
         rts   

* Check module ID & calculate module header parity & CRC
* Entry: X=Block offset of module
*        Y=DAT image pointer of module
L053F    pshs  y,x
         clrd  
         lbsr  L0AD8
         cmpd  #$87CD
         beq   L0550
         ldb   #$CD
         bra   L05AE
* Calculate module header parity
L0550    leax  $02,x
         lbsr  L0AC6
         ldw   #$4A07
L0559    lbsr  L0AAD
         eorr  a,e
         decf  
         bne   L0559
         ince  
         beq   L056B
         ldb   #$EC
         bra   L05AE
L056B    puls  y,x
         lda   <D.CRC
         bne   L0574
         clrd  
         rts   
* Begin checking Module CRC
* Entry: X=Module pointer
*        Y=DAT image pointer of module
L0574    ldd   #$0002
         lbsr  L0AD8
         tfr   d,w
         pshs  y,x
         ldd   #$FFFF
         pshs  b,a
         pshs  b
         lbsr  L0AC6
         leau  ,s
* Loop: W=# bytes left to use in CRC calc
L058A    tstf  
         bne   L0598
         pshs  x
         ldx   #1
         os9   F$Sleep
         puls  x
L0598    lbsr  L0AAD
         bsr   L05B2
         decw  
         bne   L058A
         puls  x,b
         cmpb  #$80
         bne   L05AC
         cmpx  #$0FE3
         beq   L05B0
L05AC    ldb   #$E8
L05AE    orcc  #$01
L05B0    puls  pc,y,x

* Calculate 24 bit CRC
* Entry: A=Byte to add to CRC
*        U=Pointer to 24 bit CRC accumulator
*
* Future reference note: Do not use W unless preserved, contains module
*                        byte counts from routines that come here!!
L05B2    eora  ,u
         pshs  a
         ldd   $01,u
         std   ,u
         clra  
         ldb   ,s
         asld  
         eora  $01,u
         std   $01,u
         clrb  
         lda   ,s
         lsrd  
         lsrd  
         eord  $01,u
         std   $01,u
         lda   ,s
         lsla  
         eora  ,s
         sta   ,s
         lsla  
         lsla  
         eora  ,s
         sta   ,s
         lsla  
         lsla  
         lsla  
         lsla  
         eora  ,s+
         bpl   L05EA
         eim   #$80,,u
         eim   #$21,$02,u
L05EA    rts   


* System Call: F$CRC
*
* Function: Compute CRC
*
* Input:  X = Address to start computation
*         Y = Byte count
*         U = Address of 3 byte CRC accumulator
*
* Output: CRC accumulator is updated
*
* Error:  CC = C bit set; B = error code
L05EB    ldd   $08,u
         beq   L0629
         ldx   $06,u
         pshs  x,b,a
         leas  -$03,s
         ldx   <D.Proc
         lda   $06,x
         ldb   <$00D0
         ldx   $0A,u
         ldy   #$0003
         leau  ,s
         pshs  y,x,b,a
         lbsr  L0B51
         ldx   <D.Proc
         leay  <$40,x
         ldx   $0B,s
         lbsr  L0AC6
         ldw   $09,s
L0615    lbsr  L0AAD
         bsr   L05B2
         decw  
         bne   L0615
         puls  y,x,b,a

         exg   a,b
         exg   x,u
         lbsr  L0B51
         leas  $07,s
L0629    clrb  
         rts   

*System call: F$Modul
L062B    pshs  u
         lda   $01,u
         ldx   $06,u
         ldy   $08,u
         bsr   L063F
         puls  y
         std   $01,y
         stx   $06,y
         stu   $0A,y
         rts   
L063F    tfr   0,u
         pshs  u,b,a
         bsr   L06C3
         cmpa  #$2F
         beq   L06BC
         lbsr  L06F2
         bcs   L06BF
         ldu   <D.ModEnd
         bra   L06B2
L0652    pshs  y,x,b,a
         pshs  y,x
         ldy   ,u
         beq   L06A8
         ldx   $04,u
         pshs  y,x
         ldd   #$0004
         lbsr  L0AD8
         addr  d,x
         pshs  y,x
         leax  $08,s
         ldb   $0D,s
         leay  ,s
         lbsr  L0795
         leas  $04,s
         puls  y,x
         leas  $04,s
         bcs   L06B0
         ldd   #$0006
         lbsr  L0AD8
         sta   ,s
         stb   $07,s
         lda   $06,s
         beq   L069F
         anda  #$F0
         beq   L0693
         eora  ,s
         anda  #$F0
         bne   L06B0
L0693    lda   $06,s
         anda  #$0F
         beq   L069F
         eora  ,s
         anda  #$0F
         bne   L06B0
L069F    puls  y,x,b,a
         abx   
         clrb  
         ldb   $01,s
         leas  $04,s
         rts   
L06A8    leas  $04,s
         ldd   $08,s
         bne   L06B0
         stu   $08,s
L06B0    puls  y,x,b,a
L06B2    leau  -$08,u
         cmpu  <$0044
         bcc   L0652
         ldb   #$DD
         fcb   $8C
L06BC    ldb   #$EB
         coma  
L06BF    stb   $01,s
         puls  pc,u,b,a
L06C3    pshs  y
L06C5    lbsr  L0AC6
         lbsr  L0A98
         leax  $01,x
         cmpa  #$20
         beq   L06C5
         leax  -$01,x
L06D3    pshs  b,a,cc
L06D5    tfr   y,d
         subd  $03,s
         asrb  
         lbsr  L0A7F
         puls  pc,y,b,a,cc
L06DF    ldx   <D.Proc
         leay  <$40,x
         ldx   $06,u
         bsr   L06F2
         std   1,u
         bcs   L06EF
         stx   $06,u
         abx   
L06EF    stx   $08,u
         rts   
L06F2    pshs  y
         lbsr  L0AC6
         pshs  y,x
         bsr   L0726
         cmpa  #$2E
         bne   L0708
         lbsr  L0A98
         bsr   L073A
         lda   #$2E
         bcc   L0712
L0708    cmpa  #$2F
         bne   L070E
         bsr   L0721
L070E    bsr   L073A
         bcs   L072B
L0712    clrb  
L0713    incb  
         tsta  
         bmi   L071D
         bsr   L0726
         bsr   L0740
         bcc   L0713
L071D    andcc #$FE
         bra   L0736
L0721    stx   $02,s
         sty   $04,s
L0726    lbra  L0AAD
L0729    bsr   L0721
L072B    cmpa  #$2C
         beq   L0729
         cmpa  #$20
         beq   L0729
         comb  
         ldb   #$EB
L0736    puls  y,x
         bra   L06D3
L073A    pshs  a
         anda  #$7F
         bra   L074C
L0740    pshs  a
         anda  #$7F
         cmpa  #$2E
         beq   L076D
         cmpa  #$2D
         beq   L076D
L074C    cmpa  #$7A
         bhi   L076C
         cmpa  #$61
         bcc   L076D
         cmpa  #$5F
         beq   L076D
         cmpa  #$5A
         bhi   L076C
         cmpa  #$41
         bcc   L076D
         cmpa  #$39
         bhi   L076C
         cmpa  #$30
         bcc   L076D
         cmpa  #$24
         beq   L076D
L076C    coma  
L076D    puls  pc,a


L076F    ldx   <D.Proc 
         leay  <$40,x
         ldx   $06,u
         pshs  y,x
         bra   L0786
         ldx   <D.Proc
         leay  <$40,x
         ldx   $06,u
         pshs  y,x
         ldy   <$004C
L0786    ldx   $08,u
         pshs  y,x
         ldd   $01,u
         leax  $04,s
         leay  ,s
         bsr   L0795
         leas  $08,s
         rts   
L0795    pshs  u,y,x,b,a
         tfr   x,u
         pulu  y,x
         lbsr  L0AC6
         pshu  y,x
         ldu   $04,s
         pulu  y,x
         lbsr  L0AC6
         bra   L07AD
L07A9    ldu   $04,s
         pulu  y,x
L07AD    lbsr  L0AAD
         pshu  y,x
         pshs  a
         ldu   $03,s
         pulu  y,x
         lbsr  L0AAD
         pshu  y,x
         eora  ,s
         tst   ,s+
         bmi   L07CD
         decb
         beq   L07CA
         anda  #$DF
         beq   L07A9
L07CA    comb  
         puls  pc,u,y,x,b,a
L07CD    decb  
         bne   L07CA
         anda  #$5F
         bne   L07CA
         clrb  
         puls  pc,u,y,x,b,a
 
L07D7    ldd   $01,u
         addd  #$00FF
         clrb  
         std   $01,u
         ldy   <D.SysMem
         leay  >$00ED,y
         pshs  b,a
         ldx   <D.SysMem
         ldb   #$20
         abx   
L07ED    ldb   $01,u
L07EF    cmpr  x,y
         bhi   L07F9
         comb  
         ldb   #$ED
         bra   L082C
L07F9    lda   ,-y
         bne   L07ED
         decb  
         bne   L07EF
         sty   ,s
         lda   $01,s
         lsra  
         lsra  
         lsra  
         lsra  
         lsra  
         ldb   $01,s
         andb  #$1F
         addb  $01,u
         addb  #$1F
         lsrb  
         lsrb  
         lsrb  
         lsrb  
         lsrb  
         ldx   <D.SysPrc
         lbsr  L099E
         bcs   L082C
         ldb   $01,u
         lda   #$01
L0822    sta   ,y+
         decb  
         bne   L0822
         lda   $01,s
         std   $0A,u
         clrb  
L082C    puls  pc,u

* System Call: F$SRqMem
*
* Function: Request memory
*
* F$SRqMem allocates memory from the system's 64K address space in 256 byte 'pages.'
* There are 256 of these '256 byte pages' in the system's RAM area (256*256=64K).
* The allocation map, pointed to by D.SysMem holds one byte per page, making the
* allocation map itself 256 bytes in size.
*
* Memory is allocated from the top of the system RAM map downwards.  Rel/Boot/Krn
* also reside in this area, and are loaded from $ED00-$FFFF.  Since this area is
* always allocated, we start searching for free pages from page $EC downward.
*
* F$SRqMem also updates the system memory map according to 8K DAT blocks. If an
* empty block is found, this routine re-does the 32 entries in the SMAP table to
* indicate that they are free.
*
* Input:  D = Byte count
*
* Output: U = Address of allocated memory area
*
* Error:  CC = C bit set; B = error code
L082E    ldd   $01,u
         beq   L0888
         addd  #$00FF
         ldb   $0B,u
         beq   L083D
         comb  
         ldb   #$D2
         rts   
L083D    ldb   $0A,u
         beq   L0888
         ldx   <D.SysMem
         abx   
L0844    aim   #$FE,,x+
         deca  
         bne   L0844
         ldx   <$004C
         lde   #$08
L084F    ldd   ,x
         cmpd  #$333E
         beq   L0882
         ldu   <D.BlkMap
         lda   d,u
         cmpa  #$01
         bne   L0882
         tfr   x,d
         subd  <$004C
         lslb  
         lslb  
         lslb  
         lslb  
         ldu   <D.SysMem
         addr  d,u
         ldf   #$10
L086F    ldd   ,u++
         bne   L0882
         decf  
         bne   L086F
         ldd   ,x
         ldu   <D.BlkMap
         sta   d,u
         ldd   #$333E
         std   ,x
L0882    leax  $02,x
         dece  
         bne   L084F
L0888    clrb  
L0889    rts   

L088A    lda   #$74
         jsr   <D.BtBug
         coma  
         lda   <$0031
         bne   L0889
         inc   <$0031
         ldx   <D.Init
         beq   L08A6
         ldd   <$14,x
         beq   L08A6
         leax  d,x
         bra   L08A9

L08A2    fcs   /Boot/
L08A6    leax  <L08A2,pcr
L08A9    lda   #$C1
         os9   F$Link   
         bcs   L0889
         lda   #$62
         jsr   <D.BtBug
         jsr   ,y
         bcs   L0889
         std   <$0038
         stx   <$0036
         lda   #$62
         jsr   <D.BtBug
         ldd   $04,x
         ldd   d,x
         cmpd  #$4E69
         bne   L08CE
         ldd   $09,x
         jmp   d,x
L08CE    ldd   <$0038
         bsr   L08E0
         ldx   <$004C
         ldb   $0D,x
         incb  
         ldx   <D.BlkMap
         lbra  L01B7
 
L08DC    ldd   $01,u
         ldx   $06,u
L08E0    leau  d,x
         tfr   x,d
         anda  #$E0
         clrb  
         pshs  u,b,a
         lsra  
         lsra  
         lsra  
         lsra  
         ldy   <$004C
         leay  a,y
L08F2    ldd   ,x
         cmpd  #$87CD
         bne   L0926
         ldd   $04,x
         pshs  x
         leax  d,x
L0900    lda   ,x+
         jsr   <D.BtBug
         bpl   L0900
         lda   #$20
         jsr   <D.BtBug
         puls  x
         ldd   ,s
         subr  d,x
         tfr   y,d
         os9   F$VModul 
         ldw   ,s
         leax  w,x
         bcc   L0921
         cmpb  #$E7
         bne   L0926
L0921    ldd   $02,x
         leax  d,x
         fcb   $8C
L0926    leax  1,x
         cmpx  $02,s
         bcs   L08F2
         leas  $04,s
         clrb  
         rts   
L0930    ldb   $02,u
         pshs  y,x,b
         ldx   <D.BlkMap
L0936    leay  ,x
         ldb   ,s
L093A    cmpx  <D.BlkMap+2
         bcc   L0957
         lda   ,x+
         bne   L0936
         decb  
         bne   L093A
L0945    tfr   y,d
         lda   ,s
         stb   ,s
L094B    inc   ,y+
         deca  
         bne   L094B
         puls  b
         clra  
         std   $01,u
         puls  pc,y,x
L0957    comb  
         ldb   #$ED
         stb   ,s
         puls  pc,y,x,b
L095E    ldb   $02,u
         pshs  y,x,b
         ldx   <D.BlkMap+2
L0964    ldb   ,s
L0966    cmpx  <D.BlkMap
         bls   L0957
         lda   ,-x
         bne   L0964
         decb  
         bne   L0966
         tfr   x,y
         bra   L0945
         ldb   $02,u
         beq   L0998
         ldd   <D.BlkMap+2
         subd  <D.BlkMap
         subd  $06,u
         bls   L0998
         tsta  
         bne   L098A
         cmpb  $02,u
         bcc   L098A
         stb   $02,u
L098A    ldx   <D.BlkMap
         ldd   $06,u
         leax  d,x
         ldb   $02,u
L0992    aim   #$FE,,x+
         decb  
         bne   L0992
L0998    clrb  
         rts   

* System Call: F$AllImg
*
* Function: Allocate image RAM blocks
*
* Input:  A = Starting block number
*         B = Number of blocks
*         X = Process descriptor pointer
*
* Output: None
*
* Error:  CC = C bit set; B = error code
*
L099A    ldd   $01,u
         ldx   $06,u
* Entry point from F$SRqMem
*
* 6309 NOTE: IF W IS USED HERE, TRY TO PRESERVE IT AS F$SRQMEM WILL
*   PROBABLY END UP USING IT
L099E    pshs  u,y,x,b,a
         lsla  
         leay  <$40,x
         leay  a,y
         clra  
         tfr   d,x
         ldu   <D.BlkMap
         pshs  u,y,x,b,a
L09AD    ldd   ,y++
         cmpd  #$333E
         beq   L09C1
         lda   d,u
         cmpa  #$01
         puls  b,a
         bne   L09D6
         decd  
         pshs  b,a
L09C1    leax  -$01,x
         bne   L09AD
         ldx   ,s++
         beq   L09DF
L09C9    lda   ,u+
         bne   L09D1
         leax  -$01,x
         beq   L09DF
L09D1    cmpu  <D.BlkMap+2
         bcs   L09C9
L09D6    ldb   #$CF
         leas  $06,s
         stb   $01,s
         comb  
         puls  pc,u,y,x,b,a
L09DF    puls  u,y,x
L09E1    ldd   ,y++
         cmpd  #$333E
         bne   L09F5
L09E9    lda   ,u+
         bne   L09E9
         inc   ,-u
         tfr   u,d
         subd  <D.BlkMap
         std   -$02,y
L09F5    leax  -$01,x
         bne   L09E1
         ldx   $02,s
         oim   #$10,$0C,x
         clrb  
         puls  pc,u,y,x,b,a



L0A01    ldb   $02,u
         ldy   $08,u
         bsr   L0A0D
L0A08    bcs   L0A0C
         sta   $01,u
L0A0C    rts   
L0A0D    tfr   b,a
L0A0F    suba  #$09
         nega  
         pshs  x,b,a
         ldd   #$FFFF
L0A17    pshs  b,a
L0A19    clra  
         ldb   $02,s
         addb  ,s
         stb   $02,s
         cmpb  $01,s
         bne   L0A36
         ldb   #$CF
         cmpy  <$004C
         bne   L0A2D
         ldb   #$ED
L0A2D    stb   $03,s
         comb  
         bra   L0A43
L0A32    tfr   a,b
         addb  $02,s
L0A36    lslb  
         ldx   b,y
         cmpx  #$333E
         bne   L0A19
         inca  
         cmpa  $03,s
         bne   L0A32
L0A43    leas  $02,s
         puls  pc,x,b,a

L0A47    ldb   $02,u
         ldy   $08,u
         bsr   L0A50
         bra   L0A08
L0A50    lda   #$FF
         pshs  x,b,a
         nega  
         subb  #$09
         negb  
         bra   L0A17

L0A5A    ldd   $01,u
         ldx   $06,u
         ldu   $0A,u
L0A60    pshs  u,y,x,b,a
         leay  <$40,x
         lsla  
         leay  a,y
         clra  
         lslb  
         tfr   d,w
         tfm   u+,y+
         oim   #$10,$0C,x
         clrb  
         puls  pc,u,y,x,b,a

L0A75    ldb   $02,u
         ldx   $06,u
         bsr   L0A7F
         stx   $06,u
         clrb  
         rts   

L0A7F    pshs  b
         tfr   b,a
         lsla  
         lsla  
         lsla  
         lsla  
         lsla  
         clrb  
         addr  d,x
         puls  pc,b
         ldx   $06,u
         ldy   $08,u
         bsr   L0A98
         sta   $01,u
         rts   
L0A98    lda   $01,y
         clrb  
         pshs  cc
         orcc  #$50
         sta   >$FFA0
         brn   L0A98
         lda   ,x
         stb   >$FFA0
         brn   L0A98
         puls  pc,cc

L0AAD    lda   $01,y
         pshs  b,cc
         clrb  
         orcc  #$50
         sta   >$FFA0
L0AB7    lda   ,x+
         stb   >$FFA0
         puls  b,cc
         bra   L0AC6
L0AC0    leax  >-$2000,x
         leay  $02,y
L0AC6    cmpx  #$2000
         bcc   L0AC0
         rts  
 
L0ACC    ldd   $01,u
         leau  $06,u
L0AD0    pulu  y,x
         bsr   L0AD8
         std   -$09,u
         clrb  
         rts
   
L0AD8    pshs  u,y,x
         addr  d,x
         bsr   L0AC6
         ldu   <$004C
         clra  
         ldb   $03,u
         tfr   d,u
         lda   $01,y
         ldb   $03,y
         pshs  cc
         orcc  #$50
         std   >$FFA0
         ldd   ,x
         stu   >$FFA0
         puls  pc,u,y,x,cc

L0AF8    ldd   $08,u
         beq   L0B44
         addd  $0A,u
         cmpa  #$FE
         bcc   L0B44
         leas  -$10,s
         leay  ,s
         pshs  u,y
         ldx   <D.Proc
         ldf   $06,x
         leay  <$40,x
         ldx   $01,u
         lde   #$08
         ldu   ,s
L0B17    clrd  
         bsr   L0AD8
         std   ,u++
         leax  $02,x
         dece  
         bne   L0B17
         ldu   $02,s
         lbsr  L0C87
         bcs   L0B40
         tfr   b,e
         lslb  
         ldx   <$00A1
         ldu   ,s
         stu   b,x
         ldu   $02,s
         tfr   w,d
         pshs  a
         bsr   L0B48
         puls  b
         lbsr  L0CA2
L0B40    leas  <$14,s
         rts   
L0B44    clrb  
         rts   
L0B46    ldd   $01,u
L0B48    ldy   $08,u
         beq   L0B44
         ldx   $06,u
         ldu   $0A,u
L0B51    pshs  u,y,x,b,a
         pshs  y,b,a
         tfr   a,b
         lbsr  L0BF7
         leay  a,u
         pshs  y,x
         ldb   $09,s
         ldx   $0E,s
         lbsr  L0BF7
         leay  a,u
         pshs  y,x
         ldd   #$2000
         subd  ,s
         pshs  b,a
         ldd   #$2000
         subd  $06,s
         pshs  b,a
         ldx   $08,s
         leax  >-$6000,x
         ldu   $04,s
         leau  >-$4000,u
         ldy   <$004C
         lda   $0B,y
         ldb   $0D,y
         tfr   d,y
L0B8C    ldd   [<$06,s]
         ldw   [<$0A,s]
         tfr   f,a
         ldw   $0E,s
         cmpw  ,s
         bls   L0BA0
         ldw   ,s
L0BA0    cmpw  $02,s
         bls   L0BA8
         ldw   $02,s
L0BA8    cmpw  #$0100
         bls   L0BB2
         ldw   #$0100
L0BB2    stw   $0C,s
         orcc  #$50
         std   >$FFA5
         tfm   x+,u+
         sty   >$FFA5
         andcc #$AF
         ldd   $0E,s
         subd  $0C,s
         beq   L0BEF
         std   $0E,s
         ldd   ,s
         subd  $0C,s
         bne   L0BDA
         lda   #$20
         subr  d,x
         inc   $0B,s
         inc   $0B,s
L0BDA    std   ,s
         ldd   $02,s
         subd  $0C,s
         bne   L0BEB
         lda   #$20
         subr  d,u
         inc   $07,s
         inc   $07,s
L0BEB    std   $02,s
         bra   L0B8C
L0BEF    leas  <$10,s
         clrb  
         puls  pc,u,y,x,b,a

L0BF5    tfr   u,y
L0BF7    ldu   <$00A1
         lslb  
         ldu   b,u
         tfr   x,d
         anda  #$E0
         beq   L0C0A
         clrb  
         subr  d,x
         lsra  
         lsra  
         lsra  
         lsra  
L0C0A    rts   

L0C0B    ldb   $02,u
         ldx   $06,u
L0C0F    pshs  u,x,a,cc
         bsr   L0BF7
         ldd   a,u
         clra  
         orcc  #$50
         stb   >$FFA0
         ldb   ,x
         sta   >$FFA0
         puls  u,x,a,cc
         stb   $01,u
         clrb  
         rts   

L0C26    ldd   $01,u
         ldx   $06,u
L0C2A    andcc #$FE
         pshs  u,x,b,a,cc
         bsr   L0BF7
         ldd   a,u
         lda   $01,s
         orcc  #$50
         stb   >$FFA0
         sta   ,x
         clrb  
         stb   >$FFA0
         puls  pc,u,x,b,a,cc

L0C41    ldx   $06,u
L0C43    ldb   $06,x
         bne   L0C4F
         bsr   L0C87
         bcs   L0C50
         stb   $06,x
         bsr   L0C63
L0C4F    clrb  
L0C50    rts   

L0C51    ldx   $06,u
         ldb   $06,x
         beq   L0C4F
         clr   $06,x
         bra   L0CA2

L0C5B    tim   #$10,$0C,x
         beq   L0C50
         cmpx  #$AE46
L0C63    aim   #$EF,$0C,x
         clr   <D.Task1N
         andcc #$FE
         pshs  u,x,b,a,cc
         ldb   $06,x
         leau  <$40,x
         ldx   <$00A1
         lslb  
         stu   b,x
         cmpb  #$02
         bhi   L0C80
         ldx   #$FFA0
         lbsr  L0E79
L0C80    puls  pc,u,x,b,a,cc

L0C82    bsr   L0C87
         stb   $02,u
         rts   

L0C87    pshs  x
         ldb   #$02
         ldx   <$0020
L0C8D    lda   b,x
         beq   L0C9B
         incb
         cmpb  #$20
         bne   L0C8D
         comb  
         ldb   #$EF
         puls  pc,x

L0C9B    stb   b,x
         clra  
         puls  pc,x

L0CA0    ldb   $02,u
L0CA2    pshs  x,b
         tstb  
         beq   L0CAB
         ldx   <$0020
         clr   b,x
L0CAB    puls  pc,x,b

L0CAD    ldx   <$0056
         beq   L0CD5
         tim   #$40,$0C,x
         beq   L0CD5
         ldu   $04,x
         ldd   $06,u
         decd  
         std   $06,u
         bne   L0CD5
L0CC0    ldu   $0D,x
         bsr   L0CE6
         leax  ,u
         beq   L0CD3
         tim   #$40,$0C,x
         beq   L0CD3
         ldu   $04,x
         ldd   $06,u
         beq   L0CC0
L0CD3    stx   <$0056
L0CD5    dec   <$002F
         bne   L0CE2
         inc   <$002F
         ldx   <$0050
         beq   L0CE2
         oim   #$20,$0C,x
L0CE2    clrb  
         rts   

L0CE4    ldx   $06,u
L0CE6    clrb  
         pshs  u,y,x,b,cc
         lda   $0A,x
         sta   $0B,x
         orcc  #$50
         ldu   #$0045
         bra   L0CFE
L0CF4    inc   $0B,u
         bne   L0CFA
         dec   $0B,u
L0CFA    cmpa  $0B,u
         bhi   L0D00
L0CFE    leay  ,u
L0D00    ldu   $0D,u
         bne   L0CF4
         ldd   $0D,y
         stx   $0D,y
         std   $0D,x
         puls  pc,u,y,x,b,cc

* system IRQ service routine
XIRQ     equ   *
L0D0C    ldx   <D.Proc
         sts   P$SP,x
         lds   <D.SysStk
         ldd   <D.SysSvc
         std   <D.XSWI2         mine <u00E4 D.XSWI2 this <u00F4 D.SWI2
         ldd   <D.SysIRQ
         std   <D.XIRQ
         jsr   [>D.SvcIRQ]
         bcc   L0D36
         ldx   <D.Proc
         ldb   $06,x
         ldx   $04,x
         pshs  u,b,a,cc
         leau  ,s
         lbsr  L0C0F
         puls  u,b,a,cc
         ora   #$50
         lbsr  L0C2A
L0D36    orcc  #$50
         ldx   <D.Proc
         tst   <$003F
         lbne  L0DCD
         lda   $0C,x
         bita  #$20
         bne   L0D5D
         ldu   #$0045
         ldb   #$08
L0D4B    ldu   $0D,u
         beq   L0D59
         bitb  $0C,u
         bne   L0D4B
         ldb   $0A,x
         cmpb  $0A,u  
         bcs   L0D5D
L0D59    ldu   $04,x
         bra   L0D97
L0D5D    anda  #$DF
         sta   $0C,x
L0D61    bsr   L0CE6

* System Call: F$NProc
*
* Function: Start the next process in the active queue
*
* Input: None
L0D63    equ   *
FNProc   ldx   <D.SysPrc      get system process descriptor
         stx   <D.Proc        save it as current
         lds   <D.SysStk      get system stack pointer
         andcc #^IntMasks     re-enable interupts
         fcb   $8c            skip 2 bytes
L0D6D   cwai  #^IntMasks     re-enable interrupts and wait for one
         orcc  #IntMasks      shut them down again
         lda   #Suspend
         ldx   #D.AProcQ-P$Queue   for start of loop, setup to point to current process
L0D76    leay  ,x             point Y to previous link (process dsc. pointer)
         ldx   P$Queue,y      get process descriptor for next active process
         beq   L0D6D          none, allow any pending IRQ's & try again
         bita  P$State,x      is it suspended?
         bne   L0D76          yes, skip it & try again
         ldd   P$Queue,x      get next process disc in line after found one
         std   P$Queue,y      save the next one in line in previous' next ptr
         stx   <D.Proc        make new process current
         lbsr  L0C43          check or make a task # for the found process
         bcs   L0D61          couldn't get one, go to the next process
         lda   <D.TSlice      reload # ticks this process can run
         sta   <D.Slice       save as tick counter for process
         ldu   P$SP,x         get process stack pointer
         lda   P$State,x      get it's state
         lbmi  L0E26          if in system state, switch to it (task 0)
L0D97    bita  #Condem        was it condemned by a deadly signal?
         bne   L0DEF          yes go exit with error
         lbsr  L0C5B          do a F$SetTsk if the ImgChg flag is set
         ldb   <P$Signal,x    any signals?
         beq   L0DCD          no go on
         decb                 is it a wakeup signal?
         beq   L0DCA          yes, go wake it up
         leas  -R$Size,s      make a register buffer on stack
         leau  ,s             point to it
         lbsr  L02B2          copy the stack from process to our copy
         lda   <P$Signal,x    get last signal
         sta   R$B,u          save it to process' B
         ldd   <P$SigVec,x    any intercept trap?
         beq   L0DEF          no, go force the process to F$Exit
         std   $0C,u         save vector to to it's PC
         ldd   <P$SigDat,x    get pointer to intercept data area
         std   R$U,u
         ldd   P$SP,x
         subd  #R$Size
         std   P$SP,x
         lbsr  L02C1
         leas  R$Size,s
L0DCA    clr   <P$Signal,x
L0DCD    oim   #$01,<D.Quick
L0DD0    ldu   <D.UsrSvc
         stu   <D.XSWI2
         ldu   <D.UsrIRQ
         stu   <D.XIRQ
         ldb   P$Task,x
         lslb  
         ldy   P$SP,x        I have ldy $04,x not $06,x P$Task
         lbsr  L0E6C          remap DAT image
* this is added by Alan
* it grabs and modifies values from the system process descriptor directly using
* values in that P$DATImg starting at $0640
         ldb   >$0660
         stb   >$0643
         incb  
         stb   >$0645

         ldb   <D.Quick
         bra   L0E3C
L0DEF    oim   #SysState,P$State,x
         leas  >P$Stack,x
         andcc #^IntMasks
         ldb   <P$Signal,x
         clr   <P$Signal,x
         os9   F$Exit   
L0E01    jmp   [>D.Poll]

* The following routines must appear no earlier than $E00 when assembled, as
* they have to always be in the vector RAM page ($FE00-$FEFF)
		
* Default routine for D.SysIRQ
L0E05    equ   *
SysIRQ   lda   <D.SSTskN        get current task's GIME task # (0 or 1)
         beq   L0E1C            use super-fast version for system state
         clr   <D.SSTskN        clear out memory copy
         jsr   [>D.SvcIRQ]      normally routine in Clock calling D.Poll
         inc   <D.SSTskN        save task # for system state
         lda   #$01             task 1
         ora   <D.TINIT         merge task bit bit's into shadow version
         sta   <D.TINIT         update shadow
         sta   >DAT.Task        save to GIME as well & return
         bra   L0E20
* 
L0E1C    jsr   [>D.SvcIRQ]
L0E20    bcc   L0E25
         oim   #IntMasks,0,s    setup RTI to shut off interrupts again
L0E25    rti   

* return from a system call
L0E26    clra                   force system task # to 0 (non-GRFDRV)
L0E27    ldx   <D.SysPrc        get sytem process dsc. otr
         lbsr  L0C5B            check image, and F$SetTsk (reserves A)
         orcc  #IntMasks        shut down interrupts
         sta   <D.SSTskN        save task # for system state
         beq   L0E39            if task 0 skip subroutine
         ora   <D.TINIT
         sta   <D.TINIT
         sta   >DAT.Task
L0E39    leas  ,u
         rti   

* switch to new process X=process descriptor pointer, U=stack pointer
L0E3C    oim   #$01,<D.TINIT
         lda   <D.TINIT
         sta   >DAT.Task        save it to GIME
         leas  ,y               point to new stack
         tstb  
         bne   L0E52
         ldf   #R$Size
         ldu   #$FEDF
         tfm   u+,y+
L0E52    rti   

* execute routine in task 1 pointed to by U 
L0E53    oim   #$01,<D.TINIT
         ldb   <D.TINIT
         stb   >DAT.Task
         jmp   ,u

* flip to task 1 (used by GRF/WindInt to switch to GRFDRV)
L0E5D    ldb   #$02
         bsr   L0E6C
         oim   #$01,<D.TINIT
         lda   <D.TINIT
         sta   >DAT.Task
         inc   <D.SSTskN
         rti   

* setup MMU in task 1, B=task # to swap to, shifted left 1 bit
L0E6C    cmpb  <D.Task1N
         beq   L0E88
         stb   <D.Task1N
         ldx   #$FFA8
         ldu   <$00A1
         ldu   b,u
L0E79    leau  $01,u
         lde   #$04
L0E7E    lda   ,u++
         ldb   ,u++
         std   ,x++
         dece  
         bne   L0E7E
L0E88    rts   

L0E89    equ   *
FIRQVCT  ldx   #$00F6
         bra   L0E93

L0E8E    equ   *
IRQVCT   orcc  #$50
         ldx   #D.IRQ
L0E93    clra  
         sta   >DAT.Task
         tfr   0,dp
L0E99    aim   #$FE,<D.TINIT
         lda   <D.TINIT
L0E9E    sta   >DAT.Task
         jmp   [,x]

L0EA3    equ   *
SWI3VCT  orcc  #$50
         ldx   #D.SWI3
         bra   L0EAF

L0EAA    equ   *
SWI2VCT  orcc  #$50
         ldx   #D.SWI2
L0EAF    ldb   [R$PC,s]
         clr   >DAT.Task
         tfr   0,dp
         lda   <D.TINIT
         bita  #$01
         beq   L0E9E
         tst   <D.SSTskN
         bne   L0E99
         sta   >DAT.Task
         leay  swistk,pc        <L0EDF,pcr
         tfr   s,u
         ldw   #R$Size
         tfm   u+,y+
         bra   L0E93

L0ED2    equ   *
SWIVCT   ldx   #D.SWI
         bra   L0EAF

L0ED7    equ   *
NMIVCT   ldx   #$00FC
         bra   L0E93

         emod

eom      equ   *

* What follows after the kernel module is the register stack, starting
* at $FEDD (6309) or $FEDF (6809).  This register stack area is used by
* the kernel to save the caller's registers in the $FEXX area of memory
* because it doesn't* get "switched out" no matter the contents of the
* MMU registers.
swistk		
	fcc     /REGISTER STACK/	same # bytes as R$Size for 6809	
		
	fcb	$55	D.ErrRst
		
* This list of addresses ends up at $FEEE after the kernel track is loaded
* into memory.  All interrupts come through the 6809 vectors at $FFF0-$FFFE
* and get directed to here.  From here, the BRA takes CPU control to the
* various handlers in the kernel.
	bra	SWI3VCT	SWI3 vector comes here
	nop	
	bra	SWI2VCT	SWI2 vector comes here
	nop	
	bra	FIRQVCT	FIRQ vector comes here
	nop	
	bra	IRQVCT	IRQ vector comes here
	nop	
	bra	SWIVCT	SWI vector comes here
	nop	
	bra	NMIVCT	NMI vector comes here
	nop	

         end
