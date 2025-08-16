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
(define-constant err-arbitration-exists (err u109))
(define-constant err-arbitration-not-found (err u110))
(define-constant err-not-arbitrator (err u111))
(define-constant err-arbitration-resolved (err u112))
(define-constant err-insufficient-reputation (err u113))
(define-constant err-arbitrator-not-available (err u114))
(define-constant err-invalid-rating (err u115))
(define-constant err-already-endorsed (err u116))
(define-constant err-self-endorsement (err u117))
(define-constant err-portfolio-not-found (err u118))
(define-constant err-invalid-skill-level (err u119))

(define-data-var next-bounty-id uint u1)
(define-data-var next-portfolio-id uint u1)
(define-data-var next-arbitration-id uint u1)
(define-data-var arbitration-fee uint u50)
(define-data-var min-arbitrator-reputation uint u500)
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

(define-map user-reputation
  { user: principal }
  {
    score: uint,
    completed-bounties: uint,
    total-earned: uint,
    disputes-lost: uint,
    arbitrations-won: uint,
    last-activity: uint
  }
)

(define-map arbitrators
  { arbitrator: principal }
  {
    reputation: uint,
    total-cases: uint,
    successful-resolutions: uint,
    available: bool,
    registration-block: uint
  }
)

(define-map arbitrations
  { arbitration-id: uint }
  {
    bounty-id: uint,
    complainant: principal,
    respondent: principal,
    arbitrator: principal,
    dispute-reason: (string-ascii 300),
    evidence-url: (string-ascii 200),
    status: (string-ascii 20),
    decision: (string-ascii 20),
    created-at: uint,
    resolved-at: (optional uint),
    arbitration-fee-paid: uint
  }
)

(define-map dispute-votes
  { arbitration-id: uint }
  {
    votes-for-complainant: uint,
    votes-for-respondent: uint,
    total-stake: uint,
    voting-deadline: uint
  }
)

(define-map voter-stakes
  { arbitration-id: uint, voter: principal }
  {
    stake-amount: uint,
    vote-side: (string-ascii 20),
    reward-claimed: bool
  }
)

;; Developer portfolio and skill verification system
(define-map developer-portfolios
  { developer: principal }
  {
    bio: (string-ascii 300),
    github-username: (string-ascii 50),
    experience-years: uint,
    hourly-rate: uint,
    availability-status: (string-ascii 20),
    total-ratings: uint,
    average-rating: uint,
    portfolio-created-at: uint
  }
)

(define-map developer-skills
  { developer: principal, skill-name: (string-ascii 50) }
  {
    skill-level: uint,
    endorsed-by-count: uint,
    verified: bool,
    last-updated: uint
  }
)

(define-map skill-endorsements
  { developer: principal, skill-name: (string-ascii 50), endorser: principal }
  {
    endorsement-strength: uint,
    endorsement-comment: (string-ascii 200),
    endorsed-at: uint
  }
)

(define-map work-samples
  { portfolio-id: uint }
  {
    developer: principal,
    title: (string-ascii 100),
    description: (string-ascii 400),
    technology-stack: (string-ascii 200),
    project-url: (string-ascii 200),
    completion-time: uint,
    bounty-id: (optional uint),
    created-at: uint
  }
)

(define-map work-ratings
  { portfolio-id: uint, rater: principal }
  {
    code-quality: uint,
    communication: uint,
    timeliness: uint,
    overall-rating: uint,
    feedback-comment: (string-ascii 300),
    rated-at: uint
  }
)

(define-map developer-specializations
  { developer: principal }
  {
    primary-specialization: (string-ascii 50),
    secondary-specializations: (string-ascii 200),
    preferred-project-size: (string-ascii 20),
    min-bounty-amount: uint,
    max-concurrent-bounties: uint
  }
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
      (current-reputation (default-to { score: u100, completed-bounties: u0, total-earned: u0, disputes-lost: u0, arbitrations-won: u0, last-activity: u0 } 
                          (map-get? user-reputation { user: submitter })))
      (reputation-bonus (if (< (/ (get reward bounty) u1000) u50) (/ (get reward bounty) u1000) u50))
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
    (map-set user-reputation
      { user: submitter }
      {
        score: (+ (get score current-reputation) reputation-bonus),
        completed-bounties: (+ (get completed-bounties current-reputation) u1),
        total-earned: (+ (get total-earned current-reputation) payout),
        disputes-lost: (get disputes-lost current-reputation),
        arbitrations-won: (get arbitrations-won current-reputation),
        last-activity: stacks-block-height
      }
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

(define-public (register-arbitrator)
  (let
    (
      (current-reputation (default-to { score: u100, completed-bounties: u0, total-earned: u0, disputes-lost: u0, arbitrations-won: u0, last-activity: u0 } 
                          (map-get? user-reputation { user: tx-sender })))
    )
    (asserts! (>= (get score current-reputation) (var-get min-arbitrator-reputation)) err-insufficient-reputation)
    (map-set arbitrators
      { arbitrator: tx-sender }
      {
        reputation: (get score current-reputation),
        total-cases: u0,
        successful-resolutions: u0,
        available: true,
        registration-block: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (set-arbitrator-availability (available bool))
  (let
    (
      (arbitrator-data (unwrap! (map-get? arbitrators { arbitrator: tx-sender }) err-not-found))
    )
    (map-set arbitrators
      { arbitrator: tx-sender }
      (merge arbitrator-data { available: available })
    )
    (ok true)
  )
)

(define-public (initiate-arbitration (bounty-id uint) (respondent principal) (dispute-reason (string-ascii 300)) (evidence-url (string-ascii 200)))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
      (arbitration-id (var-get next-arbitration-id))
      (arbitration-cost (/ (* (get reward bounty) (var-get arbitration-fee)) u10000))
    )
    (asserts! (is-none (map-get? arbitrations { arbitration-id: arbitration-id })) err-arbitration-exists)
    (asserts! (or (is-eq tx-sender (get creator bounty)) 
                  (is-eq (some tx-sender) (get assignee bounty))) err-unauthorized)
    (try! (stx-transfer? arbitration-cost tx-sender (as-contract tx-sender)))
    (map-set arbitrations
      { arbitration-id: arbitration-id }
      {
        bounty-id: bounty-id,
        complainant: tx-sender,
        respondent: respondent,
        arbitrator: contract-owner,
        dispute-reason: dispute-reason,
        evidence-url: evidence-url,
        status: "pending",
        decision: "none",
        created-at: stacks-block-height,
        resolved-at: none,
        arbitration-fee-paid: arbitration-cost
      }
    )
    (map-set dispute-votes
      { arbitration-id: arbitration-id }
      {
        votes-for-complainant: u0,
        votes-for-respondent: u0,
        total-stake: u0,
        voting-deadline: (+ stacks-block-height u144)
      }
    )
    (var-set next-arbitration-id (+ arbitration-id u1))
    (ok arbitration-id)
  )
)

(define-public (assign-arbitrator (arbitration-id uint) (arbitrator principal))
  (let
    (
      (arbitration (unwrap! (map-get? arbitrations { arbitration-id: arbitration-id }) err-arbitration-not-found))
      (arbitrator-data (unwrap! (map-get? arbitrators { arbitrator: arbitrator }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status arbitration) "pending") err-arbitration-resolved)
    (asserts! (get available arbitrator-data) err-arbitrator-not-available)
    (map-set arbitrations
      { arbitration-id: arbitration-id }
      (merge arbitration { 
        arbitrator: arbitrator,
        status: "assigned"
      })
    )
    (ok true)
  )
)

(define-public (vote-on-dispute (arbitration-id uint) (vote-side (string-ascii 20)) (stake-amount uint))
  (let
    (
      (arbitration (unwrap! (map-get? arbitrations { arbitration-id: arbitration-id }) err-arbitration-not-found))
      (votes (unwrap! (map-get? dispute-votes { arbitration-id: arbitration-id }) err-not-found))
      (voter-reputation (default-to { score: u100, completed-bounties: u0, total-earned: u0, disputes-lost: u0, arbitrations-won: u0, last-activity: u0 } 
                        (map-get? user-reputation { user: tx-sender })))
    )
    (asserts! (is-eq (get status arbitration) "assigned") err-invalid-status)
    (asserts! (< stacks-block-height (get voting-deadline votes)) err-invalid-status)
    (asserts! (>= (get score voter-reputation) u250) err-insufficient-reputation)
    (asserts! (> stake-amount u0) err-invalid-amount)
    (asserts! (or (is-eq vote-side "complainant") (is-eq vote-side "respondent")) err-invalid-status)
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    (map-set voter-stakes
      { arbitration-id: arbitration-id, voter: tx-sender }
      {
        stake-amount: stake-amount,
        vote-side: vote-side,
        reward-claimed: false
      }
    )
    (if (is-eq vote-side "complainant")
      (map-set dispute-votes
        { arbitration-id: arbitration-id }
        (merge votes {
          votes-for-complainant: (+ (get votes-for-complainant votes) stake-amount),
          total-stake: (+ (get total-stake votes) stake-amount)
        })
      )
      (map-set dispute-votes
        { arbitration-id: arbitration-id }
        (merge votes {
          votes-for-respondent: (+ (get votes-for-respondent votes) stake-amount),
          total-stake: (+ (get total-stake votes) stake-amount)
        })
      )
    )
    (ok true)
  )
)

(define-public (resolve-arbitration (arbitration-id uint) (decision (string-ascii 20)))
  (let
    (
      (arbitration (unwrap! (map-get? arbitrations { arbitration-id: arbitration-id }) err-arbitration-not-found))
      (votes (unwrap! (map-get? dispute-votes { arbitration-id: arbitration-id }) err-not-found))
      (arbitrator-data (unwrap! (map-get? arbitrators { arbitrator: (get arbitrator arbitration) }) err-not-found))
      (winning-side (if (> (get votes-for-complainant votes) (get votes-for-respondent votes)) "complainant" "respondent"))
    )
    (asserts! (is-eq tx-sender (get arbitrator arbitration)) err-not-arbitrator)
    (asserts! (is-eq (get status arbitration) "assigned") err-arbitration-resolved)
    (asserts! (>= stacks-block-height (get voting-deadline votes)) err-invalid-status)
    (asserts! (or (is-eq decision "complainant") (is-eq decision "respondent")) err-invalid-status)
    (map-set arbitrations
      { arbitration-id: arbitration-id }
      (merge arbitration {
        status: "resolved",
        decision: decision,
        resolved-at: (some stacks-block-height)
      })
    )
    (map-set arbitrators
      { arbitrator: (get arbitrator arbitration) }
      (merge arbitrator-data {
        total-cases: (+ (get total-cases arbitrator-data) u1),
        successful-resolutions: (if (is-eq decision winning-side) 
                                   (+ (get successful-resolutions arbitrator-data) u1)
                                   (get successful-resolutions arbitrator-data))
      })
    )
    (try! (as-contract (stx-transfer? (get arbitration-fee-paid arbitration) tx-sender (get arbitrator arbitration))))
    (ok true)
  )
)

(define-public (claim-voting-reward (arbitration-id uint))
  (let
    (
      (arbitration (unwrap! (map-get? arbitrations { arbitration-id: arbitration-id }) err-arbitration-not-found))
      (voter-stake (unwrap! (map-get? voter-stakes { arbitration-id: arbitration-id, voter: tx-sender }) err-not-found))
      (votes (unwrap! (map-get? dispute-votes { arbitration-id: arbitration-id }) err-not-found))
      (current-reputation (default-to { score: u100, completed-bounties: u0, total-earned: u0, disputes-lost: u0, arbitrations-won: u0, last-activity: u0 } 
                          (map-get? user-reputation { user: tx-sender })))
      (won-vote (is-eq (get vote-side voter-stake) (get decision arbitration)))
      (reward-amount (if won-vote 
                        (+ (get stake-amount voter-stake) (/ (* (get stake-amount voter-stake) u200) u10000))
                        u0))
    )
    (asserts! (is-eq (get status arbitration) "resolved") err-invalid-status)
    (asserts! (not (get reward-claimed voter-stake)) err-invalid-status)
    (map-set voter-stakes
      { arbitration-id: arbitration-id, voter: tx-sender }
      (merge voter-stake { reward-claimed: true })
    )
    (if won-vote
      (begin
        (try! (as-contract (stx-transfer? reward-amount tx-sender tx-sender)))
        (map-set user-reputation
          { user: tx-sender }
          (merge current-reputation {
            score: (+ (get score current-reputation) u10),
            arbitrations-won: (+ (get arbitrations-won current-reputation) u1),
            last-activity: stacks-block-height
          })
        )
      )
      (map-set user-reputation
        { user: tx-sender }
        (merge current-reputation {
          disputes-lost: (+ (get disputes-lost current-reputation) u1),
          last-activity: stacks-block-height
        })
      )
    )
    (ok won-vote)
  )
)

(define-read-only (get-user-reputation (user principal))
  (map-get? user-reputation { user: user })
)

(define-read-only (get-arbitrator-info (arbitrator principal))
  (map-get? arbitrators { arbitrator: arbitrator })
)

(define-read-only (get-arbitration (arbitration-id uint))
  (map-get? arbitrations { arbitration-id: arbitration-id })
)

(define-read-only (get-dispute-votes (arbitration-id uint))
  (map-get? dispute-votes { arbitration-id: arbitration-id })
)

(define-read-only (get-voter-stake (arbitration-id uint) (voter principal))
  (map-get? voter-stakes { arbitration-id: arbitration-id, voter: voter })
)

(define-read-only (get-next-arbitration-id)
  (var-get next-arbitration-id)
)

(define-public (set-arbitration-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u500) err-invalid-amount)
    (var-set arbitration-fee new-fee)
    (ok true)
  )
)

(define-public (set-min-arbitrator-reputation (new-min uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (>= new-min u100) err-invalid-amount)
    (var-set min-arbitrator-reputation new-min)
    (ok true)
  )
)

;; Developer Portfolio Functions
(define-public (create-developer-portfolio (bio (string-ascii 300)) (github-username (string-ascii 50)) (experience-years uint) (hourly-rate uint))
  (begin
    (map-set developer-portfolios
      { developer: tx-sender }
      {
        bio: bio,
        github-username: github-username,
        experience-years: experience-years,
        hourly-rate: hourly-rate,
        availability-status: "available",
        total-ratings: u0,
        average-rating: u0,
        portfolio-created-at: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (update-availability-status (status (string-ascii 20)))
  (let
    (
      (portfolio (unwrap! (map-get? developer-portfolios { developer: tx-sender }) err-portfolio-not-found))
    )
    (map-set developer-portfolios
      { developer: tx-sender }
      (merge portfolio { availability-status: status })
    )
    (ok true)
  )
)

(define-public (add-developer-skill (skill-name (string-ascii 50)) (skill-level uint))
  (begin
    (asserts! (and (>= skill-level u1) (<= skill-level u10)) err-invalid-skill-level)
    (map-set developer-skills
      { developer: tx-sender, skill-name: skill-name }
      {
        skill-level: skill-level,
        endorsed-by-count: u0,
        verified: false,
        last-updated: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (endorse-skill (developer principal) (skill-name (string-ascii 50)) (endorsement-strength uint) (endorsement-comment (string-ascii 200)))
  (let
    (
      (skill (unwrap! (map-get? developer-skills { developer: developer, skill-name: skill-name }) err-not-found))
      (existing-endorsement (map-get? skill-endorsements { developer: developer, skill-name: skill-name, endorser: tx-sender }))
    )
    (asserts! (not (is-eq tx-sender developer)) err-self-endorsement)
    (asserts! (is-none existing-endorsement) err-already-endorsed)
    (asserts! (and (>= endorsement-strength u1) (<= endorsement-strength u5)) err-invalid-rating)
    (map-set skill-endorsements
      { developer: developer, skill-name: skill-name, endorser: tx-sender }
      {
        endorsement-strength: endorsement-strength,
        endorsement-comment: endorsement-comment,
        endorsed-at: stacks-block-height
      }
    )
    (map-set developer-skills
      { developer: developer, skill-name: skill-name }
      (merge skill { endorsed-by-count: (+ (get endorsed-by-count skill) u1) })
    )
    (ok true)
  )
)

(define-public (add-work-sample (title (string-ascii 100)) (description (string-ascii 400)) (technology-stack (string-ascii 200)) (project-url (string-ascii 200)) (completion-time uint) (bounty-id-ref (optional uint)))
  (let
    (
      (portfolio-id (var-get next-portfolio-id))
    )
    (unwrap! (map-get? developer-portfolios { developer: tx-sender }) err-portfolio-not-found)
    (map-set work-samples
      { portfolio-id: portfolio-id }
      {
        developer: tx-sender,
        title: title,
        description: description,
        technology-stack: technology-stack,
        project-url: project-url,
        completion-time: completion-time,
        bounty-id: bounty-id-ref,
        created-at: stacks-block-height
      }
    )
    (var-set next-portfolio-id (+ portfolio-id u1))
    (ok portfolio-id)
  )
)

(define-public (rate-work-sample (portfolio-id uint) (code-quality uint) (communication uint) (timeliness uint) (feedback-comment (string-ascii 300)))
  (let
    (
      (work-sample (unwrap! (map-get? work-samples { portfolio-id: portfolio-id }) err-portfolio-not-found))
      (developer (get developer work-sample))
      (portfolio (unwrap! (map-get? developer-portfolios { developer: developer }) err-portfolio-not-found))
      (overall-rating (/ (+ code-quality communication timeliness) u3))
      (existing-rating (map-get? work-ratings { portfolio-id: portfolio-id, rater: tx-sender }))
    )
    (asserts! (not (is-eq tx-sender developer)) err-self-endorsement)
    (asserts! (is-none existing-rating) err-already-endorsed)
    (asserts! (and (>= code-quality u1) (<= code-quality u5)) err-invalid-rating)
    (asserts! (and (>= communication u1) (<= communication u5)) err-invalid-rating)
    (asserts! (and (>= timeliness u1) (<= timeliness u5)) err-invalid-rating)
    (map-set work-ratings
      { portfolio-id: portfolio-id, rater: tx-sender }
      {
        code-quality: code-quality,
        communication: communication,
        timeliness: timeliness,
        overall-rating: overall-rating,
        feedback-comment: feedback-comment,
        rated-at: stacks-block-height
      }
    )
    (let
      (
        (new-total-ratings (+ (get total-ratings portfolio) u1))
        (current-average (get average-rating portfolio))
        (new-average (if (is-eq current-average u0) 
                        overall-rating 
                        (/ (+ (* current-average (get total-ratings portfolio)) overall-rating) new-total-ratings)))
      )
      (map-set developer-portfolios
        { developer: developer }
        (merge portfolio {
          total-ratings: new-total-ratings,
          average-rating: new-average
        })
      )
    )
    (ok true)
  )
)

(define-public (set-specializations (primary-specialization (string-ascii 50)) (secondary-specializations (string-ascii 200)) (preferred-project-size (string-ascii 20)) (min-bounty-amount uint) (max-concurrent-bounties uint))
  (begin
    (unwrap! (map-get? developer-portfolios { developer: tx-sender }) err-portfolio-not-found)
    (map-set developer-specializations
      { developer: tx-sender }
      {
        primary-specialization: primary-specialization,
        secondary-specializations: secondary-specializations,
        preferred-project-size: preferred-project-size,
        min-bounty-amount: min-bounty-amount,
        max-concurrent-bounties: max-concurrent-bounties
      }
    )
    (ok true)
  )
)

(define-public (verify-skill (developer principal) (skill-name (string-ascii 50)))
  (let
    (
      (skill (unwrap! (map-get? developer-skills { developer: developer, skill-name: skill-name }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (>= (get endorsed-by-count skill) u3) err-insufficient-reputation)
    (map-set developer-skills
      { developer: developer, skill-name: skill-name }
      (merge skill { verified: true })
    )
    (ok true)
  )
)

;; Portfolio read-only functions
(define-read-only (get-developer-portfolio (developer principal))
  (map-get? developer-portfolios { developer: developer })
)

(define-read-only (get-developer-skill (developer principal) (skill-name (string-ascii 50)))
  (map-get? developer-skills { developer: developer, skill-name: skill-name })
)

(define-read-only (get-skill-endorsement (developer principal) (skill-name (string-ascii 50)) (endorser principal))
  (map-get? skill-endorsements { developer: developer, skill-name: skill-name, endorser: endorser })
)

(define-read-only (get-work-sample (portfolio-id uint))
  (map-get? work-samples { portfolio-id: portfolio-id })
)

(define-read-only (get-work-rating (portfolio-id uint) (rater principal))
  (map-get? work-ratings { portfolio-id: portfolio-id, rater: rater })
)

(define-read-only (get-developer-specializations (developer principal))
  (map-get? developer-specializations { developer: developer })
)

(define-read-only (get-next-portfolio-id)
  (var-get next-portfolio-id)
)


