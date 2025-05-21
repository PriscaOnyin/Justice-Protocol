;; Justice Protocol: Decentralized Dispute Resolution System
;; A smart contract built with Clarity

;; Error codes
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-CASE-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-DEPOSIT (err u102))
(define-constant ERR-DUPLICATE-JUDGMENT (err u103))
(define-constant ERR-CASE-CLOSED (err u104))
(define-constant ERR-INVALID-JUDGMENT (err u105))
(define-constant ERR-INVALID-INPUT (err u106))
(define-constant ERR-ZERO-COST (err u107))
(define-constant ERR-INVALID-SUMMARY (err u108))
(define-constant ERR-INVALID-DOCUMENT (err u109))

;; Define fungible token for deposits and incentives
(define-fungible-token justice-token)

;; Data structures
(define-map cases
    { case-id: uint }
    {
        plaintiff: principal,
        summary: (string-utf8 500),
        state: (string-utf8 20),
        judgments-positive: uint,
        judgments-negative: uint,
        total-deposits: uint,
        resolution-cost: uint
    }
)

(define-map judge-judgments
    { case-id: uint, judge: principal }
    { judgment: (string-utf8 10), deposit: uint }
)

(define-map case-documents
    { case-id: uint, document-id: uint }
    { provider: principal, document-hash: (buff 32) }
)

;; Variables
(define-data-var case-counter uint u0)
(define-data-var document-counter uint u0)
(define-data-var min-deposit uint u100) ;; Minimum deposit required to judge
(define-data-var protocol-admin principal tx-sender)
(define-data-var certified-judges (list 100 principal) (list tx-sender))

;; Private functions
(define-private (is-certified-judge (caller principal))
    (is-some (index-of (var-get certified-judges) caller))
)

;; Validation functions
(define-private (validate-case-id (case-id uint))
    (and 
        (> case-id u0)
        (<= case-id (var-get case-counter))
    )
)

(define-private (validate-summary (summary (string-utf8 500)))
    (> (len summary) u0)
)

(define-private (validate-resolution-cost (cost uint))
    (> cost u0)
)

(define-private (validate-document-hash (hash (buff 32)))
    ;; For a buffer, we check if it's the correct length
    (is-eq (len hash) u32)
)

(define-private (validate-judgment (judgment (string-utf8 10)))
    (or (is-eq judgment u"positive") (is-eq judgment u"negative"))
)

(define-private (validate-deposit (deposit uint))
    (>= deposit (var-get min-deposit))
)

(define-private (validate-judge (judge principal))
    ;; A principal is never none, so we just check it's not the sender
    (not (is-eq judge tx-sender))
)

;; Public functions

;; Create a new case
(define-public (create-case (summary (string-utf8 500)) (resolution-cost uint))
    (begin
        ;; Validate inputs
        (asserts! (validate-summary summary) ERR-INVALID-SUMMARY)
        (asserts! (validate-resolution-cost resolution-cost) ERR-ZERO-COST)
        
        (let
            ((case-id (+ (var-get case-counter) u1))
             (validated-summary summary)
             (validated-cost resolution-cost))
            (map-set cases
                { case-id: case-id }
                {
                    plaintiff: tx-sender,
                    summary: validated-summary,
                    state: u"active",
                    judgments-positive: u0,
                    judgments-negative: u0,
                    total-deposits: u0,
                    resolution-cost: validated-cost
                }
            )
            (var-set case-counter case-id)
            (ok case-id)
        )
    )
)

;; Submit document for a case
(define-public (submit-document (case-id uint) (document-hash (buff 32)))
    (begin
        ;; Validate inputs
        (asserts! (validate-case-id case-id) ERR-CASE-NOT-FOUND)
        (asserts! (validate-document-hash document-hash) ERR-INVALID-DOCUMENT)
        
        (let
            ((document-id (+ (var-get document-counter) u1))
             (case-data (unwrap! (map-get? cases { case-id: case-id }) ERR-CASE-NOT-FOUND))
             (validated-case-id case-id)
             (validated-hash document-hash))
            (asserts! (is-eq (get state case-data) u"active") ERR-CASE-CLOSED)
            (map-set case-documents
                { case-id: validated-case-id, document-id: document-id }
                { provider: tx-sender, document-hash: validated-hash }
            )
            (var-set document-counter document-id)
            (ok document-id)
        )
    )
)

;; Judge a case
(define-public (judge-case (case-id uint) (judgment (string-utf8 10)) (deposit uint))
    (begin
        ;; Validate inputs
        (asserts! (validate-case-id case-id) ERR-CASE-NOT-FOUND)
        (asserts! (validate-judgment judgment) ERR-INVALID-JUDGMENT)
        (asserts! (validate-deposit deposit) ERR-INSUFFICIENT-DEPOSIT)
        
        (let
            ((case-data (unwrap! (map-get? cases { case-id: case-id }) ERR-CASE-NOT-FOUND))
             (existing-judgment (map-get? judge-judgments { case-id: case-id, judge: tx-sender }))
             (validated-case-id case-id)
             (validated-judgment judgment)
             (validated-deposit deposit))
            (asserts! (is-certified-judge tx-sender) ERR-UNAUTHORIZED)
            (asserts! (is-eq (get state case-data) u"active") ERR-CASE-CLOSED)
            (asserts! (is-none existing-judgment) ERR-DUPLICATE-JUDGMENT)
            
            (try! (ft-transfer? justice-token validated-deposit tx-sender (as-contract tx-sender)))
            
            (map-set judge-judgments
                { case-id: validated-case-id, judge: tx-sender }
                { judgment: validated-judgment, deposit: validated-deposit }
            )
            
            (map-set cases
                { case-id: validated-case-id }
                (merge case-data {
                    judgments-positive: (if (is-eq validated-judgment u"positive")
                        (+ (get judgments-positive case-data) u1)
                        (get judgments-positive case-data)),
                    judgments-negative: (if (is-eq validated-judgment u"negative")
                        (+ (get judgments-negative case-data) u1)
                        (get judgments-negative case-data)),
                    total-deposits: (+ (get total-deposits case-data) validated-deposit)
                })
            )
            
            (ok true)
        )
    )
)

;; Close a case
(define-public (close-case (case-id uint))
    (begin
        ;; Validate input
        (asserts! (validate-case-id case-id) ERR-CASE-NOT-FOUND)
        
        (let
            ((case-data (unwrap! (map-get? cases { case-id: case-id }) ERR-CASE-NOT-FOUND))
             (validated-case-id case-id))
            (asserts! (is-certified-judge tx-sender) ERR-UNAUTHORIZED)
            (asserts! (is-eq (get state case-data) u"active") ERR-CASE-CLOSED)
            
            (let
                ((total-judgments (+ (get judgments-positive case-data) (get judgments-negative case-data)))
                 (outcome (if (> (get judgments-positive case-data) (get judgments-negative case-data)) 
                              u"resolved-positive" 
                              u"resolved-negative"))
                 (winning-judgments (if (is-eq outcome u"resolved-positive")
                                       (get judgments-positive case-data)
                                       (get judgments-negative case-data)))
                 (incentive-per-deposit (if (> winning-judgments u0)
                                           (/ (get resolution-cost case-data) winning-judgments)
                                           u0)))
                
                ;; Update case status
                (map-set cases
                    { case-id: validated-case-id }
                    (merge case-data { state: outcome })
                )
                
                ;; Distribute incentives and return outcome
                (if (> incentive-per-deposit u0)
                    (begin
                        (try! (distribute-incentives validated-case-id outcome incentive-per-deposit))
                        (ok outcome)
                    )
                    (ok outcome)
                )
            )
        )
    )
)

;; Private function to distribute incentives
(define-private (distribute-incentives (case-id uint) (outcome (string-utf8 20)) (incentive-per-deposit uint))
    (let
        ((judges (unwrap! (get-judges-for-case case-id) ERR-CASE-NOT-FOUND)))
        ;; Use fold to distribute incentives to each judge
        (fold distribute-incentive-to-judge
            judges
            { total-distributed: u0, case-id: case-id, outcome: outcome, incentive-per-deposit: incentive-per-deposit }
        )
        (ok true)
    )
)

;; Helper function to distribute incentive to a single judge
(define-private (distribute-incentive-to-judge 
    (judge principal)
    (acc { total-distributed: uint, case-id: uint, outcome: (string-utf8 20), incentive-per-deposit: uint })
)
    (let
        ((judgment-data (unwrap-panic (map-get? judge-judgments 
                                      { case-id: (get case-id acc), judge: judge }))))
        (if (is-eq (get judgment judgment-data) (get outcome acc))
            (let
                ((incentive (* (get deposit judgment-data) (get incentive-per-deposit acc))))
                (match (as-contract (ft-transfer? justice-token
                    incentive
                    tx-sender
                    judge))
                    success (merge acc { total-distributed: (+ (get total-distributed acc) incentive) })
                    error acc)
            )
            acc
        )
    )
)

;; Helper function for checking and adding judges to the list
(define-private (check-and-add-judge
    (judge principal)
    (acc { judges: (list 100 principal), case-id: uint })
)
    (let 
        (
            (judgment (map-get? judge-judgments { case-id: (get case-id acc), judge: judge }))
            (current-judges (get judges acc))
        )
        (if (is-some judgment)
            (merge acc { 
                judges: (match (as-max-len? (append current-judges judge) u100)
                    success-result success-result
                    current-judges)
            })
            acc
        )
    )
)

;; Read-only functions

;; Get case details
(define-read-only (get-case-details (case-id uint))
    (if (validate-case-id case-id)
        (map-get? cases { case-id: case-id })
        none
    )
)

;; Get document for a case
(define-read-only (get-document (case-id uint) (document-id uint))
    (if (and (validate-case-id case-id) (> document-id u0))
        (map-get? case-documents { case-id: case-id, document-id: document-id })
        none
    )
)

;; Get judge's judgment for a case
(define-read-only (get-judge-judgment (case-id uint) (judge principal))
    (if (validate-case-id case-id)
        (map-get? judge-judgments { case-id: case-id, judge: judge })
        none
    )
)

;; Get all judges who judged a case
(define-read-only (get-judges-for-case (case-id uint))
    (if (validate-case-id case-id)
        (ok (get judges (fold check-and-add-judge
            (var-get certified-judges)
            { judges: (list), case-id: case-id }
        )))
        (err ERR-CASE-NOT-FOUND)
    )
)

;; Helper function for filtering judges
(define-private (not-equal-to (current principal)) 
    (not (is-eq current tx-sender))
)

;; Add a certified judge
(define-public (add-certified-judge (judge principal))
    (begin
        ;; Validate input
        (asserts! (validate-judge judge) ERR-INVALID-INPUT)
        
        (let
            ((current-judges (var-get certified-judges))
             (validated-judge judge))
            (asserts! (is-eq tx-sender (var-get protocol-admin)) ERR-UNAUTHORIZED)
            (asserts! (< (len current-judges) u100) ERR-UNAUTHORIZED)
            (asserts! (is-none (index-of current-judges validated-judge)) ERR-INVALID-INPUT)
            
            (let ((new-judges (unwrap! (as-max-len? (append current-judges validated-judge) u100) ERR-UNAUTHORIZED)))
                (var-set certified-judges new-judges)
                (ok true)
            )
        )
    )
)

;; Remove a certified judge
(define-public (remove-certified-judge (judge principal))
    (begin
        (let
            ((current-judges (var-get certified-judges)))
            (asserts! (is-eq tx-sender (var-get protocol-admin)) ERR-UNAUTHORIZED)
            (asserts! (is-some (index-of current-judges judge)) ERR-INVALID-INPUT)
            
            (var-set certified-judges 
                (filter not-equal-to current-judges))
            (ok true)
        )
    )
)

;; Mint initial tokens to contract deployer
(define-private (mint-initial-supply)
    (ft-mint? justice-token u1000000000 tx-sender)
)

;; Initialize contract
(begin
    (try! (mint-initial-supply))
)
