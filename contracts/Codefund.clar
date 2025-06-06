(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-funds (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-bounty-closed (err u106))
(define-constant err-bounty-open (err u107))
(define-constant err-invalid-status (err u108))

(define-data-var next-bounty-id uint u1)
(define-data-var platform-fee-rate uint u250)

(define-map bounties
  { bounty-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    reward: uint,
    status: (string-ascii 20),
    assignee: (optional principal),
    created-at: uint,
    deadline: uint
  }
)

(define-map bounty-funders
  { bounty-id: uint, funder: principal }
  { amount: uint }
)

(define-map bounty-total-funds
  { bounty-id: uint }
  { total: uint }
)

(define-map user-submissions
  { bounty-id: uint, submitter: principal }
  {
    submission-url: (string-ascii 200),
    submitted-at: uint,
    approved: bool
  }
)

(define-map platform-earnings
  { period: uint }
  { amount: uint }
)

(define-public (create-bounty (title (string-ascii 100)) (description (string-ascii 500)) (deadline uint) (initial-reward uint))
  (let
    (
      (bounty-id (var-get next-bounty-id))
      (current-block stacks-block-height)
    )
    (asserts! (> initial-reward u0) err-invalid-amount)
    (asserts! (> deadline current-block) err-invalid-amount)
    (try! (stx-transfer? initial-reward tx-sender (as-contract tx-sender)))
    (map-set bounties
      { bounty-id: bounty-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        reward: initial-reward,
        status: "open",
        assignee: none,
        created-at: current-block,
        deadline: deadline
      }
    )
    (map-set bounty-funders
      { bounty-id: bounty-id, funder: tx-sender }
      { amount: initial-reward }
    )
    (map-set bounty-total-funds
      { bounty-id: bounty-id }
      { total: initial-reward }
    )
    (var-set next-bounty-id (+ bounty-id u1))
    (ok bounty-id)
  )
)

(define-public (fund-bounty (bounty-id uint) (amount uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
      (current-funding (default-to { amount: u0 } (map-get? bounty-funders { bounty-id: bounty-id, funder: tx-sender })))
      (total-funds (unwrap! (map-get? bounty-total-funds { bounty-id: bounty-id }) err-not-found))
    )
    (asserts! (is-eq (get status bounty) "open") err-bounty-closed)
    (asserts! (> amount u0) err-invalid-amount)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set bounty-funders
      { bounty-id: bounty-id, funder: tx-sender }
      { amount: (+ (get amount current-funding) amount) }
    )
    (map-set bounty-total-funds
      { bounty-id: bounty-id }
      { total: (+ (get total total-funds) amount) }
    )
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { reward: (+ (get reward bounty) amount) })
    )
    (ok true)
  )
)

(define-public (assign-bounty (bounty-id uint) (assignee principal))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator bounty)) err-unauthorized)
    (asserts! (is-eq (get status bounty) "open") err-bounty-closed)
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { 
        status: "assigned",
        assignee: (some assignee)
      })
    )
    (ok true)
  )
)

(define-public (submit-work (bounty-id uint) (submission-url (string-ascii 200)))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
    )
    (asserts! (is-eq (get status bounty) "assigned") err-invalid-status)
    (asserts! (is-eq (some tx-sender) (get assignee bounty)) err-unauthorized)
    (map-set user-submissions
      { bounty-id: bounty-id, submitter: tx-sender }
      {
        submission-url: submission-url,
        submitted-at: stacks-block-height,
        approved: false
      }
    )
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { status: "submitted" })
    )
    (ok true)
  )
)

(define-public (approve-submission (bounty-id uint) (submitter principal))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
      (submission (unwrap! (map-get? user-submissions { bounty-id: bounty-id, submitter: submitter }) err-not-found))
      (platform-fee (/ (* (get reward bounty) (var-get platform-fee-rate)) u10000))
      (payout (- (get reward bounty) platform-fee))
    )
    (asserts! (is-eq tx-sender (get creator bounty)) err-unauthorized)
    (asserts! (is-eq (get status bounty) "submitted") err-invalid-status)
    (try! (as-contract (stx-transfer? payout tx-sender submitter)))
    (try! (as-contract (stx-transfer? platform-fee tx-sender contract-owner)))
    (map-set user-submissions
      { bounty-id: bounty-id, submitter: submitter }
      (merge submission { approved: true })
    )
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { status: "completed" })
    )
    (ok true)
  )
)

(define-public (reject-submission (bounty-id uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator bounty)) err-unauthorized)
    (asserts! (is-eq (get status bounty) "submitted") err-invalid-status)
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { status: "assigned" })
    )
    (ok true)
  )
)

(define-public (cancel-bounty (bounty-id uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator bounty)) err-unauthorized)
    (asserts! (or (is-eq (get status bounty) "open") (is-eq (get status bounty) "assigned")) err-invalid-status)
    (try! (as-contract (stx-transfer? (get reward bounty) tx-sender (get creator bounty))))
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { status: "cancelled" })
    )
    (ok true)
  )
)

(define-public (set-platform-fee (new-fee-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee-rate u1000) err-invalid-amount)
    (var-set platform-fee-rate new-fee-rate)
    (ok true)
  )
)

(define-read-only (get-bounty (bounty-id uint))
  (map-get? bounties { bounty-id: bounty-id })
)

(define-read-only (get-bounty-funds (bounty-id uint))
  (map-get? bounty-total-funds { bounty-id: bounty-id })
)

(define-read-only (get-user-funding (bounty-id uint) (funder principal))
  (map-get? bounty-funders { bounty-id: bounty-id, funder: funder })
)

(define-read-only (get-submission (bounty-id uint) (submitter principal))
  (map-get? user-submissions { bounty-id: bounty-id, submitter: submitter })
)

(define-read-only (get-next-bounty-id)
  (var-get next-bounty-id)
)

(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate)
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)