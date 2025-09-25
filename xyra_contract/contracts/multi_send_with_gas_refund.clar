;; Multi-Send with Gas Refund Contract
;; Allows batch sending of STX with automatic gas refund for unused gas

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-invalid-recipient (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-empty-batch (err u104))
(define-constant err-transfer-failed (err u105))
(define-constant err-gas-calculation-failed (err u106))

;; Gas cost constants (in microSTX)
(define-constant base-gas-cost u1000)       ;; Base cost for contract execution
(define-constant per-transfer-gas-cost u500) ;; Cost per individual transfer
(define-constant gas-refund-overhead u200)   ;; Overhead for gas refund calculation

;; Data structures
(define-map user-gas-deposits principal uint)
(define-map batch-gas-usage uint uint)

;; Data variables
(define-data-var next-batch-id uint u1)
(define-data-var total-gas-refunded uint u0)

;; Transfer recipient structure
(define-map transfer-recipients 
  { batch-id: uint, index: uint }
  { recipient: principal, amount: uint }
)

;; Events
(define-map batch-events
  uint
  {
    sender: principal,
    total-recipients: uint,
    total-amount: uint,
    gas-deposited: uint,
    gas-used: uint,
    gas-refunded: uint,
    block-height: uint
  }
)

;; Private functions

;; Helper function to check if a response is ok
(define-private (is-response-ok (response-val (response bool uint)))
  (is-ok response-val)
)

;; Calculate estimated gas cost for a batch
(define-private (calculate-estimated-gas (recipient-count uint))
  (+ base-gas-cost 
     (* recipient-count per-transfer-gas-cost)
     gas-refund-overhead)
)

;; Execute a single transfer
(define-private (execute-transfer (transfer-data { recipient: principal, amount: uint }))
  (let ((recipient (get recipient transfer-data))
        (amount (get amount transfer-data)))
    (if (and (> amount u0) (not (is-eq recipient tx-sender)))
      (stx-transfer? amount tx-sender recipient)
      (err u102) ;; err-invalid-recipient
    )
  )
)

;; Calculate actual gas used based on successful transfers
(define-private (calculate-actual-gas-used (successful-transfers uint))
  (+ base-gas-cost 
     (* successful-transfers per-transfer-gas-cost)
     gas-refund-overhead)
)

;; Store batch event data
(define-private (store-batch-event 
  (batch-id uint)
  (sender principal)
  (recipient-count uint)
  (total-amount uint)
  (gas-deposited uint)
  (gas-used uint)
  (gas-refunded uint))
  (map-set batch-events batch-id
    {
      sender: sender,
      total-recipients: recipient-count,
      total-amount: total-amount,
      gas-deposited: gas-deposited,
      gas-used: gas-used,
      gas-refunded: gas-refunded,
      block-height: block-height
    }
  )
)

;; Public functions

;; Multi-send with automatic gas refund
(define-public (multi-send-with-gas-refund (recipients (list 100 { recipient: principal, amount: uint })))
  (let ((sender tx-sender)
        (batch-id (var-get next-batch-id))
        (recipient-count (len recipients))
        (estimated-gas (calculate-estimated-gas recipient-count)))
    
    ;; Validate inputs
    (asserts! (> recipient-count u0) err-empty-batch)
    
    ;; Calculate total amount to send
    (let ((total-amount (fold + (map get-amount recipients) u0))
          (total-with-gas (+ total-amount estimated-gas)))
      
      ;; Check sender has sufficient balance
      (asserts! (>= (stx-get-balance sender) total-with-gas) err-insufficient-balance)
      
      ;; Store gas deposit
      (map-set user-gas-deposits sender estimated-gas)
      
      ;; Execute transfers and count successful ones
      (let ((transfer-results (map execute-transfer recipients))
            (successful-transfers (len (filter is-response-ok transfer-results))))
        
        ;; Calculate actual gas used and refund
        (let ((actual-gas-used (calculate-actual-gas-used successful-transfers))
              (gas-to-refund (if (> estimated-gas actual-gas-used)
                               (- estimated-gas actual-gas-used)
                               u0)))
          
          ;; Refund unused gas if any
          (if (> gas-to-refund u0)
            (begin
              (try! (stx-transfer? gas-to-refund (as-contract tx-sender) sender))
              (var-set total-gas-refunded (+ (var-get total-gas-refunded) gas-to-refund))
            )
            true
          )
          
          ;; Store batch information
          (store-batch-event 
            batch-id 
            sender 
            recipient-count 
            total-amount 
            estimated-gas 
            actual-gas-used 
            gas-to-refund)
          
          ;; Update batch ID
          (var-set next-batch-id (+ batch-id u1))
          
          ;; Store gas usage for this batch
          (map-set batch-gas-usage batch-id actual-gas-used)
          
          (ok {
            batch-id: batch-id,
            successful-transfers: successful-transfers,
            total-recipients: recipient-count,
            total-amount-sent: total-amount,
            gas-deposited: estimated-gas,
            gas-used: actual-gas-used,
            gas-refunded: gas-to-refund
          })
        )
      )
    )
  )
)

;; Helper function to extract amount from recipient data
(define-private (get-amount (recipient-data { recipient: principal, amount: uint }))
  (get amount recipient-data)
)

;; Optimized multi-send for known gas usage (advanced users)
(define-public (multi-send-exact-gas 
  (recipients (list 100 { recipient: principal, amount: uint }))
  (gas-deposit uint))
  (let ((sender tx-sender)
        (batch-id (var-get next-batch-id))
        (recipient-count (len recipients)))
    
    ;; Validate inputs
    (asserts! (> recipient-count u0) err-empty-batch)
    (asserts! (> gas-deposit u0) err-invalid-amount)
    
    ;; Calculate total amount
    (let ((total-amount (fold + (map get-amount recipients) u0))
          (total-with-gas (+ total-amount gas-deposit)))
      
      ;; Check balance
      (asserts! (>= (stx-get-balance sender) total-with-gas) err-insufficient-balance)
      
      ;; Execute transfers
      (let ((transfer-results (map execute-transfer recipients))
            (successful-transfers (len (filter is-response-ok transfer-results))))
        
        ;; Calculate actual gas and refund
        (let ((actual-gas-used (calculate-actual-gas-used successful-transfers))
              (gas-to-refund (if (> gas-deposit actual-gas-used)
                               (- gas-deposit actual-gas-used)
                               u0)))
          
          ;; Refund if needed
          (if (> gas-to-refund u0)
            (try! (stx-transfer? gas-to-refund (as-contract tx-sender) sender))
            true
          )
          
          ;; Store event data
          (store-batch-event 
            batch-id 
            sender 
            recipient-count 
            total-amount 
            gas-deposit 
            actual-gas-used 
            gas-to-refund)
          
          (var-set next-batch-id (+ batch-id u1))
          (map-set batch-gas-usage batch-id actual-gas-used)
          
          (ok {
            batch-id: batch-id,
            successful-transfers: successful-transfers,
            gas-refunded: gas-to-refund
          })
        )
      )
    )
  )
)

;; Read-only functions

;; Get estimated gas cost for a given number of recipients
(define-read-only (get-estimated-gas-cost (recipient-count uint))
  (calculate-estimated-gas recipient-count)
)

;; Get batch information
(define-read-only (get-batch-info (batch-id uint))
  (map-get? batch-events batch-id)
)

;; Get gas usage for a specific batch
(define-read-only (get-batch-gas-usage (batch-id uint))
  (map-get? batch-gas-usage batch-id)
)

;; Get total gas refunded by the contract
(define-read-only (get-total-gas-refunded)
  (var-get total-gas-refunded)
)

;; Get current batch ID
(define-read-only (get-current-batch-id)
  (var-get next-batch-id)
)

;; Get user's current gas deposit
(define-read-only (get-user-gas-deposit (user principal))
  (default-to u0 (map-get? user-gas-deposits user))
)

;; Admin functions (for contract maintenance)

;; Emergency function to withdraw stuck funds (owner only)
(define-public (emergency-withdraw (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (as-contract (stx-transfer? amount tx-sender contract-owner)))
    (ok true)
  )
)

;; Get contract balance
(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)