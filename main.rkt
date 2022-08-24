#lang racket/base
(require racket/class)
(require racket/gui/base)
(require racket/string)

(define *memory* (make-vector 1024))
(define *memory-boundary* 1024)
(define *halted* #t)
(define *pc* 0)
(define *r* 0)
(define (goto x)
  (set! *pc* (modulo x *memory-boundary*))
  (let ((lowerbound (max 0 (- *pc* 5))))
    (send* listboxMemoryViewer
      (set-selection *pc*)
      (set-first-visible-item lowerbound)))
  (send tfPC set-value (fo *pc*)))
(define (inc-pc) (goto (+ *pc* 1)))
(define (r-positive) (<= (bitwise-ior #o10000000000 *r*) 0))
(define (setr x)
  (set! *r* x)
  (send tfR set-value (fox x)))
(define (setmem addr x)
  (vector-set! *memory* addr x)
  (send* listboxMemoryViewer
    (set-string addr (fox x) 1)
    (set-string addr (disassemble x) 2)))
(define (getmem addr) (vector-ref *memory* addr))

(define (resize-mem target-size)
  (let ((newv (make-vector target-size)))
    (vector-copy! newv 0 *memory* 0 (min target-size (vector-length *memory*)))
    (set! *memory* newv)
    (refresh-memory-viewer)))

(define (w-abs x)
  (bitwise-and #o10000000000 x))

(define (fo x) (format "~o" x))
(define (fox x)
  (let ((preres (format "~o" x)))
    (string-append (make-string (- 11 (string-length preres)) #\0)
                   preres)))

(define (dissect-instr x)
  (values (quotient (bitwise-and #o7700000000 x) #o100000000)
          (quotient (bitwise-and #o77770000 x) #o10000)
          (bitwise-and #o7777 x)))
(define (disassemble x)
  (let-values ([(opn x y) (dissect-instr x)])
    (case opn
      [(#o04 #o14 #o44 #o54 #o17 #o37 #o57 #o77) "HALT"]
      [(#o34) (format "COND ~o,~o" x y)]
      [(#o74) (format "ABS R; GOTO ~o" y)]
      [(#o64) (format "[~o]=R; PRINT; GOTO ~o" y x)]
      [(#o24) (format "[~o]=R; GOTO ~o" y x)]
      [(#o45 #o55) (format "[~o]=[~o]; PRINT" y x)]
      [(#o05 #o15) (format "R=[~o]=[~o]" y x)]
      [(#o07 #o27) (format "INPUT [~o]" y)]
      [else
       (let ((f (quotient (bitwise-and #o70 opn) #o10))
             (s (bitwise-and #o7 opn)))
         (let ((op (case s
                         ((0) "+")
                         ((1) "-")
                         ((2) "/")
                         ((3) "*")
                         ((6) "&"))))
           (case f
             ((0) (format "R=[~o]=[~o]~a[~o]" y x op y))
             ((1) (format "R=[~o]~a[~o]" x op y))
             ((2) (format "R=[~o]=R~a[~o]" y op x))
             ((3) (format "R=R~a[~o]" op x))
             ((4) (format "R=[~o]=[~o]~a[~o]; PRINT" y x op y))
             ((5) (format "R=ABS([~o])~aABS([~o])" x op y))
             ((6) (format "R=[~o]=R~a[~o]; PRINT" y op x))
             ((7) (format "R=ABS(R)~aABS([~o])" op x)))))])))

(define (step)
  (let ((x (vector-ref *memory* *pc*)))
    (let-values ([(opn x y) (dissect-instr x)])
      (case opn
        [(#o04 #o14 #o44 #o54 #o17 #o37 #o57 #o77) (halt)]
        [(#o34) (if (r-positive) (goto x) (goto y))]
        [(#o74) (setr (w-abs *r*)) (goto y)]
        [(#o64) (setmem y *r*) (print-to-output *r*) (goto x)]
        [(#o24) (setmem y *r*) (goto x)]
        [(#o45 #o55) (setmem y (getmem x)) (print-to-output (getmem x)) (inc-pc)]
        [(#o05 #o15) (setmem y (getmem x)) (setr (getmem x)) (inc-pc)]
        [(#o07 #o27) (input-from-tape y) (inc-pc)]
        [else
         (let ((f (bitwise-and #o70 opn))
               (s (bitwise-and #o7 opn)))
           (let ((op (case s
                       ((0) (λ (a b) (modulo (+ a b) 2147483648)))
                       ((1) (λ (a b) (modulo (- a b) 2147483648)))
                       ((2) (λ (a b) (modulo (/ a b) 2147483648)))
                       ((3) (λ (a b) (modulo (* a b) 2147483648)))
                       ((6) bitwise-and))))
             (case f
               ((0) (let ((r (op (getmem x) (getmem y))))
                      (setr r) (setmem y r) (inc-pc)))
               ((1) (let ((r (op (getmem x) (getmem y))))
                      (setr r) (inc-pc)))
               ((2) (let ((r (op *r* (getmem x))))
                      (setr r) (setmem y r) (inc-pc)))
               ((3) (let ((r (op *r* (getmem x))))
                      (setr r) (inc-pc)))
               ((4) (let ((r (op (getmem x) (getmem y))))
                      (setr r) (setmem y r) (print-to-output r) (inc-pc)))
               ((5) (let ((r (op (w-abs (getmem x)) (w-abs (getmem y)))))
                      (setr r) (inc-pc)))
               ((6) (let ((r (op *r* (getmem (x)))))
                      (setr r) (print-to-output r) (inc-pc)))
               ((7) (let ((r (op (w-abs *r*) (w-abs (getmem x)))))
                      (setr r) (inc-pc))))))]))))


(define *machine-timer*
  (new timer%
       [notify-callback (λ () (step))]))
(define (unhalt)
  (set! *halted* #f)
  (send *machine-timer* start 33))
(define (halt)
  (send *machine-timer* stop)
  (set! *halted* #t)
  (send btnRun set-label "Run"))

(define frmMain
  (new frame%
       [label "DJS-1 Simulator"]
       [min-width 400]
       [min-height 600]))
(define frmAbout
  (new dialog%
       [label "About"]
       [alignment '(left top)]))
(define frmHowToUse
  (new dialog%
       [label "How to use..."]
       [alignment '(left top)]))
(define menubarMain
  (new menu-bar%
       [parent frmMain]))
(define menuFile
  (new menu%
       [parent menubarMain]
       [label "File"]))
(define menuitemLoad
  (new menu-item%
       [parent menuFile]
       [label "Load Program"]
       [callback
        (λ (mi e)
          (let ((f (get-file #f #f #f #f #f null
                             (list (list "Octal numbers in text (*.txt)" "*.txt")
                                   (list "Any" "*.*")))))
            (when f
              (let ((f (open-input-file f #:mode 'text))
                    (i 0))
                (for ([l (in-lines f 'any)]
                      #:when (not (string=? (string-trim l) "")))
                  (setmem i (string->number l 8))
                  (set! i (+ i 1)))                
                (close-input-port f)))))]))


(define dialogHowToUse
  (new dialog%
       [label "How to use..."]
       [min-height 400]))
(define lblHowToUse
  (new text-field%
       [parent dialogHowToUse]
       [label #f]
       [style (list 'multiple)]
       [init-value (string-join (list
                                 "Clicking on one of the memory size option will populate the machine"
                                 "with new memory. The content is saved between changing the memory"
                                 "size."
                                 "Double click on the memory viewer can change the value of the"
                                 "specified cell."
                                 "You can dump the content of the memory using the \"Dump Memory\""
                                 "button. The dump file can be loaded back into the machine with "
                                 "[File]-[Load]."
                                 "Using the [Go] button will change the content of PC.")
                           "\n")]))

(define menuHelp
  (new menu%
       [parent menubarMain]
       [label "Help"]))
(define menuitemHowToUse
  (new menu-item%
       [parent menuHelp]
       [label "How to use..."]
       [callback (λ (mi e) (send dialogHowToUse show #t))]))
(define menuitemAbout
  (new menu-item%
       [parent menuHelp]
       [label "About"]
       [callback (λ (mi e) (send frmAbout show #t))]))


(define panelRun
  (new horizontal-panel%
       [parent frmMain]
       [stretchable-height #f]))
(define btnRun
  (new button%
       [parent panelRun]
       [label "Run"]
       [callback
        (λ (btn e)
          (if *halted*
              (begin (unhalt)
                     (send btn set-label "Stop"))
              (begin (halt))))]))
(define btnStep
  (new button%
       [parent panelRun]
       [label "Step"]
       [callback (λ (btn e) (step))]))
(define btnReset
  (new button%
       [parent panelRun]
       [label "Reset"]
       [callback (λ (btn e) (goto 0) (setr 0))]))
(define btnDumpMemory
  (new button%
       [parent panelRun]
       [label "Dump Memory"]
       [callback (λ (btn e)
                   (let ((p (put-file #f #f #f #f #f null
                                      (list (list "Octal numbers in text (*.txt)" "*.txt")))))
                     (when p
                       (let ((pp (open-output-file p
                                                   #:mode 'text
                                                   #:exists 'replace)))
                         (for ([i (in-naturals)]
                               [v (in-vector *memory*)]
                               #:break (>= i *memory-boundary*))
                           (write-string (fox (getmem i)) pp)
                           (newline pp))
                         (close-output-port pp)))))]))

(define panelMachine
  (new horizontal-panel%
       [parent frmMain]
       [alignment '(left top)]))
(define panelMachine1
  (new vertical-panel%
       [parent panelMachine]
       [stretchable-width #f]
       [alignment '(left top)]))
(define radioMemory
  (new radio-box%
       [parent panelMachine1]
       [label "Memory Size"]
       [choices (list
                 "1024"
                 "2048"
                 "4096")]
       [style (list 'vertical 'vertical-label)]
       [callback (λ (rb e)
                   (set! *memory-boundary*
                         (vector-ref #(1024 2048 4096)
                                     (or (send rb get-selection) 0)))
                   (resize-mem *memory-boundary*))]))


(define panelPC
  (new vertical-panel%
       [parent panelMachine1]
       [stretchable-height #f]))
(define panelPC1
  (new horizontal-panel%
       [parent panelPC]
       [stretchable-height #f]))
(define lblPC
  (new message%
       [parent panelPC1]
       [label "PC"]))
(define btnPCSet
  (new button%
       [parent panelPC1]
       [label "Go"]
       [callback (λ (btn e) (goto (string->number (send tfPC get-value) 8)))]))
(define tfPC
  (new text-field%
       [parent panelPC]
       [label #f]
       [stretchable-width #f]
       [style (list 'single 'vertical-label)]))
(send tfPC set-value (fo *pc*))


(define panelR
  (new vertical-panel%
       [parent panelMachine1]
       [stretchable-height #f]))
(define panelR1
  (new horizontal-panel%
       [parent panelR]
       [stretchable-height #f]))
(define lblR
  (new message%
       [parent panelR1]
       [label "R"]))
(define btnRSet
  (new button%
       [parent panelR1]
       [label "Set"]
       [callback (λ (btn e) (setr (string->number (send tfR get-value) 8)))]))
(define tfR
  (new text-field%
       [parent panelR]
       [label #f]
       [stretchable-width #f]
       [style (list 'single 'vertical-label)]))
(send tfR set-value (fox *r*))

(define dialogSetMemoryCell
  (new (class dialog% (super-new)
         (define target #f)
         (define/public (show-with-target x)
           (set! target x)
           (send this show #t))
         (define/public (confirm-with-value x)
           (when (not (equal? target #f))
             (setmem target x))
           (send this show #f)))
       [label "Set memory cell value"]))
(define lblSetMemoryCell
  (new message%
       [parent dialogSetMemoryCell]
       [label "Input number in octal:"]))
(define tfSetMemoryCell
  (new text-field%
       [parent dialogSetMemoryCell]
       [label #f]))
(define panelSetMemoryCellCtrls
  (new horizontal-panel%
       [parent dialogSetMemoryCell]
       [alignment '(right top)]))
(define btnSetMemoryCellConfirm
  (new button%
       [parent panelSetMemoryCellCtrls]
       [label "Confirm"]
       [callback (λ (btn e)
                   (send dialogSetMemoryCell confirm-with-value
                         (string->number (or (send tfSetMemoryCell get-value) "0") 8)))]))

(define panelMemoryViewer
  (new panel%
       [parent panelMachine]))
(define listboxMemoryViewer
  (new list-box%
       [parent panelMemoryViewer]
       [choices (list)]
       [label "Memory Viewer"]
       [columns (list "Address" "Value (octal)" "Disassembly")]
       [style (list 'single 'vertical-label 'column-headers)]
       [callback (λ (lb e)
                   (when (equal? (send e get-event-type) 'list-box-dclick)
                     (let ((s (car (send lb get-selections))))
                       (send dialogSetMemoryCell show-with-target s))))]))
(define (refresh-memory-viewer)
  (send/apply listboxMemoryViewer set
              (apply map list (for/list ([i (in-naturals 0)]
                                         #:break (>= i *memory-boundary*))
                                (let* ((m (getmem i))
                                       (dis (disassemble m)))
                                  (list (fo i) (fox m) dis))))))


(define panelInput
  (new  group-box-panel%
       [parent frmMain]
       [alignment (list 'left 'top)]
       [label "Virtual Input Tape"]))
(define lblInput
  (new message%
       [parent panelInput]
       [label "One octal number per line. The numbers will be consumed from top to bottom."]))
(define tfInput
  (new text-field%
       [parent panelInput]
       [label #f]
       [style (list 'multiple)]))

(define (input-from-tape addr)
  (let* ((d (send tfInput get-value))
         (d (string-split d))
         (dz (if (null? d) #f (car d))))
    (when dz
        (begin (send tfInput set-value (string-join (cdr d) "\n"))
               (setmem addr (string->number dz 8))))))


(define panelOutput
  (new group-box-panel%
       [parent frmMain]
       [label "Virtual Teletype Output"]
       [alignment (list 'left 'top)]))
(define btnOutputClear
  (new button%
       [parent panelOutput]
       [label "Clear Output"]
       [callback (λ (btn e) (send tfOutput set-value ""))]))
(define tfOutput
  (new text-field%
       [label #f]
       [parent panelOutput]
       [style (list 'multiple 'vertical-label)]))

(define (print-to-output x)
  (send tfOutput set-value
        (string-append (send tfOutput get-value)
                       "\n"
                       (fo x))))

(define lblAbout
  (new message%
       [parent frmAbout]
       [label "DJS-1 Simulator"]
       [font (make-object font%
               (send normal-control-font get-size)
               (send normal-control-font get-family)
               'normal
               'bold)]))
(define lblAbout2
  (new message%
       [parent frmAbout]
       [label "(c) Sebastian Higgins 2022\nDistributed under BSD-3-Clause license"]))
(define tfAbout3
  (new text-field%
       [parent frmAbout]
       [label #f]
       [style '(multiple)]
       [init-value (string-append
                    "For more information, please visit the project's page:\n"
                    "https://sebastian.graphics/projects/early-chinese-computers/djs1.html")]))



(send frmMain show #t)
(refresh-memory-viewer)
